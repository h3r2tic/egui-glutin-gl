#![allow(clippy::collapsible_else_if)]
#![allow(unsafe_code)]
#![allow(dead_code)]

use std::{collections::HashMap, ffi::CString};

use egui::{
    emath::Rect,
    epaint::{Color32, Mesh, PaintCallbackInfo, Primitive, Vertex},
};
use gl::types::*;
use memoffset::offset_of;

use super::vao;

const VERT_SRC: &str = include_str!("shader/vertex.glsl");
const FRAG_SRC: &str = include_str!("shader/fragment.glsl");

pub type TextureFilter = egui::TextureFilter;

trait TextureFilterExt {
    fn glow_code(&self) -> u32;
}

impl TextureFilterExt for TextureFilter {
    fn glow_code(&self) -> u32 {
        match self {
            TextureFilter::Linear => gl::LINEAR,
            TextureFilter::Nearest => gl::NEAREST,
        }
    }
}

type Program = GLuint;
type Buffer = GLuint;
type Texture = GLuint;

type UniformLocation = GLint;

/// An OpenGL painter using [`glow`].
///
/// This is responsible for painting egui and managing egui textures.
/// You can access the underlying [`gl::Context`] with [`Self::gl`].
///
/// This struct must be destroyed with [`Painter::destroy`] before dropping, to ensure OpenGL
/// objects have been properly deleted and are not leaked.
pub struct Painter {
    max_texture_side: usize,

    program: Program,
    u_screen_size: UniformLocation,
    u_sampler: UniformLocation,
    vao: super::vao::VertexArrayObject,
    vbo: Buffer,
    element_array_buffer: Buffer,

    textures: HashMap<egui::TextureId, Texture>,

    next_native_tex_id: u64,

    /// Stores outdated OpenGL textures that are yet to be deleted
    textures_to_destroy: Vec<Texture>,

    /// Used to make sure we are destroyed correctly.
    destroyed: bool,
}

/// A callback function that can be used to compose an [`egui::PaintCallback`] for custom rendering
/// with [`glow`].
///
/// The callback is passed, the [`egui::PaintCallbackInfo`] and the [`Painter`] which can be used to
/// access the OpenGL context.
///
/// # Example
///
/// See the [`custom3d_glow`](https://github.com/emilk/egui/blob/master/crates/egui_demo_app/src/apps/custom3d_wgpu.rs) demo source for a detailed usage example.
pub struct CallbackFn {
    #[allow(clippy::type_complexity)]
    f: Box<dyn Fn(PaintCallbackInfo, &Painter) + Sync + Send>,
}

impl CallbackFn {
    pub fn new<F: Fn(PaintCallbackInfo, &Painter) + Sync + Send + 'static>(callback: F) -> Self {
        let f = Box::new(callback);
        CallbackFn { f }
    }
}

fn gl_create_buffer() -> Buffer {
    let mut id = 0;
    unsafe {
        gl::GenBuffers(1, &mut id);
    }
    id
}

fn gl_get_uniform_location(program: Program, name: &str) -> UniformLocation {
    let c_str = CString::new(name.as_bytes())
        .unwrap_or_else(|_| panic!("Couldn't create a CString from {}", name));
    unsafe { gl::GetUniformLocation(program, c_str.as_ptr()) }
}

fn gl_get_attrib_location(program: Program, name: &str) -> GLint {
    let c_str = CString::new(name.as_bytes())
        .unwrap_or_else(|_| panic!("Couldn't create a CString from {}", name));
    unsafe { gl::GetAttribLocation(program, c_str.as_ptr()) }
}

fn gl_buffer_data(target: GLenum, data: &[u8], usage: GLenum) {
    unsafe {
        gl::BufferData(target, data.len() as isize, data.as_ptr().cast(), usage);
    }
}

impl Painter {
    /// Create painter.
    ///
    /// Set `shader_prefix` if you want to turn on shader workaround e.g. `"#define APPLY_BRIGHTENING_GAMMA\n"`
    /// (see <https://github.com/emilk/egui/issues/794>).
    ///
    /// # Errors
    /// will return `Err` below cases
    /// * failed to compile shader
    /// * failed to create postprocess on webgl with `sRGB` support
    /// * failed to create buffer
    pub fn new() -> Painter {
        puffin::profile_function!();

        let max_texture_side = 4096;

        unsafe {
            let vert = compile_shader(VERT_SRC, gl::VERTEX_SHADER);
            let frag = compile_shader(FRAG_SRC, gl::FRAGMENT_SHADER);
            let program = link_program(vert, frag);
            gl::DetachShader(program, vert);
            gl::DetachShader(program, frag);
            gl::DeleteShader(vert);
            gl::DeleteShader(frag);
            let u_screen_size = gl_get_uniform_location(program, "u_screen_size");
            let u_sampler = gl_get_uniform_location(program, "u_sampler");

            let vbo = gl_create_buffer();

            let a_pos_loc = gl_get_attrib_location(program, "a_pos");
            let a_tc_loc = gl_get_attrib_location(program, "a_tc");
            let a_srgba_loc = gl_get_attrib_location(program, "a_srgba");

            let stride = std::mem::size_of::<Vertex>() as i32;
            let buffer_infos = vec![
                vao::BufferInfo {
                    location: a_pos_loc,
                    vector_size: 2,
                    data_type: gl::FLOAT,
                    normalized: false,
                    stride,
                    offset: offset_of!(Vertex, pos) as u32,
                },
                vao::BufferInfo {
                    location: a_tc_loc,
                    vector_size: 2,
                    data_type: gl::FLOAT,
                    normalized: false,
                    stride,
                    offset: offset_of!(Vertex, uv) as u32,
                },
                vao::BufferInfo {
                    location: a_srgba_loc,
                    vector_size: 4,
                    data_type: gl::UNSIGNED_BYTE,
                    normalized: false,
                    stride,
                    offset: offset_of!(Vertex, color) as u32,
                },
            ];
            let vao = super::vao::VertexArrayObject::new(vbo, buffer_infos);

            let element_array_buffer = gl_create_buffer();

            Painter {
                max_texture_side,
                program,
                u_screen_size,
                u_sampler,
                vao,
                vbo,
                element_array_buffer,
                textures: Default::default(),
                next_native_tex_id: 1 << 32,
                textures_to_destroy: Vec::new(),
                destroyed: false,
            }
        }
    }

    pub fn max_texture_side(&self) -> usize {
        self.max_texture_side
    }

    unsafe fn prepare_painting(
        &mut self,
        [width_in_pixels, height_in_pixels]: [u32; 2],
        pixels_per_point: f32,
    ) -> (u32, u32) {
        gl::Enable(gl::SCISSOR_TEST);
        // egui outputs mesh in both winding orders
        gl::Disable(gl::CULL_FACE);
        gl::Disable(gl::DEPTH_TEST);

        gl::ColorMask(gl::TRUE, gl::TRUE, gl::TRUE, gl::TRUE);

        gl::Enable(gl::BLEND);
        gl::BlendEquationSeparate(gl::FUNC_ADD, gl::FUNC_ADD);
        gl::BlendFuncSeparate(
            // egui outputs colors with premultiplied alpha:
            gl::ONE,
            gl::ONE_MINUS_SRC_ALPHA,
            // Less important, but this is technically the correct alpha blend function
            // when you want to make use of the framebuffer alpha (for screenshots, compositing, etc).
            gl::ONE_MINUS_DST_ALPHA,
            gl::ONE,
        );

        let width_in_points = width_in_pixels as f32 / pixels_per_point;
        let height_in_points = height_in_pixels as f32 / pixels_per_point;

        gl::Viewport(0, 0, width_in_pixels as i32, height_in_pixels as i32);
        gl::UseProgram(self.program);

        gl::Uniform2f(self.u_screen_size, width_in_points, height_in_points);
        gl::Uniform1i(self.u_sampler, 0);
        gl::ActiveTexture(gl::TEXTURE0);

        self.vao.bind();
        gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, self.element_array_buffer);

        (width_in_pixels, height_in_pixels)
    }

    /// You are expected to have cleared the color buffer before calling this.
    pub fn paint_and_update_textures(
        &mut self,
        screen_size_px: [u32; 2],
        pixels_per_point: f32,
        clipped_primitives: &[egui::ClippedPrimitive],
        textures_delta: &egui::TexturesDelta,
    ) {
        puffin::profile_function!();
        for (id, image_delta) in &textures_delta.set {
            self.set_texture(*id, image_delta);
        }

        self.paint_primitives(screen_size_px, pixels_per_point, clipped_primitives);

        for &id in &textures_delta.free {
            self.free_texture(id);
        }
    }

    /// Main entry-point for painting a frame.
    ///
    /// You should call `target.clear_color(..)` before
    /// and `target.finish()` after this.
    ///
    /// The following OpenGL features will be set:
    /// - Scissor test will be enabled
    /// - Cull face will be disabled
    /// - Blend will be enabled
    /// - Depth test will be disabled
    ///
    /// The scissor area and blend parameters will be changed.
    ///
    /// As well as this, the following objects will be unset:
    /// - Vertex Buffer
    /// - Element Buffer
    /// - Texture (and active texture will be set to 0)
    /// - Program
    ///
    /// Please be mindful of these effects when integrating into your program, and also be mindful
    /// of the effects your program might have on this code. Look at the source if in doubt.
    pub fn paint_primitives(
        &mut self,
        screen_size_px: [u32; 2],
        pixels_per_point: f32,
        clipped_primitives: &[egui::ClippedPrimitive],
    ) {
        puffin::profile_function!();
        self.assert_not_destroyed();

        let size_in_pixels = unsafe { self.prepare_painting(screen_size_px, pixels_per_point) };

        for egui::ClippedPrimitive {
            clip_rect,
            primitive,
        } in clipped_primitives
        {
            set_clip_rect(size_in_pixels, pixels_per_point, *clip_rect);

            match primitive {
                Primitive::Mesh(mesh) => {
                    self.paint_mesh(mesh);
                }
                Primitive::Callback(callback) => {
                    if callback.rect.is_positive() {
                        puffin::profile_scope!("callback");
                        // Transform callback rect to physical pixels:
                        let rect_min_x = pixels_per_point * callback.rect.min.x;
                        let rect_min_y = pixels_per_point * callback.rect.min.y;
                        let rect_max_x = pixels_per_point * callback.rect.max.x;
                        let rect_max_y = pixels_per_point * callback.rect.max.y;

                        let rect_min_x = rect_min_x.round() as i32;
                        let rect_min_y = rect_min_y.round() as i32;
                        let rect_max_x = rect_max_x.round() as i32;
                        let rect_max_y = rect_max_y.round() as i32;

                        unsafe {
                            gl::Viewport(
                                rect_min_x,
                                size_in_pixels.1 as i32 - rect_max_y,
                                rect_max_x - rect_min_x,
                                rect_max_y - rect_min_y,
                            );
                        }

                        let info = egui::PaintCallbackInfo {
                            viewport: callback.rect,
                            clip_rect: *clip_rect,
                            pixels_per_point,
                            screen_size_px,
                        };

                        if let Some(callback) = callback.callback.downcast_ref::<CallbackFn>() {
                            (callback.f)(info, self);
                        } else {
                            log::warn!("Warning: Unsupported render callback. Expected egui_gl::CallbackFn");
                        }

                        // Restore state:
                        unsafe { self.prepare_painting(screen_size_px, pixels_per_point) };
                    }
                }
            }
        }

        unsafe {
            self.vao.unbind();
            gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, 0);
            gl::Disable(gl::SCISSOR_TEST);
        }
    }

    #[inline(never)] // Easier profiling
    fn paint_mesh(&mut self, mesh: &Mesh) {
        debug_assert!(mesh.is_valid());
        if let Some(texture) = self.texture(mesh.texture_id) {
            unsafe {
                gl::BindBuffer(gl::ARRAY_BUFFER, self.vbo);
                gl_buffer_data(
                    gl::ARRAY_BUFFER,
                    bytemuck::cast_slice(&mesh.vertices),
                    gl::STREAM_DRAW,
                );

                gl::BindBuffer(gl::ELEMENT_ARRAY_BUFFER, self.element_array_buffer);
                gl_buffer_data(
                    gl::ELEMENT_ARRAY_BUFFER,
                    bytemuck::cast_slice(&mesh.indices),
                    gl::STREAM_DRAW,
                );

                gl::BindTexture(gl::TEXTURE_2D, texture);
            }

            unsafe {
                gl::DrawElements(
                    gl::TRIANGLES,
                    mesh.indices.len() as i32,
                    gl::UNSIGNED_INT,
                    std::ptr::null(),
                );
            }
        } else {
            log::warn!("Failed to find texture {:?}", mesh.texture_id);
        }
    }

    // ------------------------------------------------------------------------

    pub fn set_texture(&mut self, tex_id: egui::TextureId, delta: &egui::epaint::ImageDelta) {
        puffin::profile_function!();

        self.assert_not_destroyed();

        let glow_texture = *self.textures.entry(tex_id).or_insert_with(|| unsafe {
            let mut handle = 0;
            gl::GenTextures(1, &mut handle);
            handle
        });
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, glow_texture);
        }

        match &delta.image {
            egui::ImageData::Color(image) => {
                assert_eq!(
                    image.width() * image.height(),
                    image.pixels.len(),
                    "Mismatch between texture size and texel count"
                );

                let data: &[u8] = bytemuck::cast_slice(image.pixels.as_ref());

                self.upload_texture_srgb(delta.pos, image.size, delta.filter, data);
            }
            egui::ImageData::Font(image) => {
                assert_eq!(
                    image.width() * image.height(),
                    image.pixels.len(),
                    "Mismatch between texture size and texel count"
                );

                let gamma = 1.0;
                let data: Vec<u8> = image
                    .srgba_pixels(gamma)
                    .flat_map(|a| a.to_array())
                    .collect();

                self.upload_texture_srgb(delta.pos, image.size, delta.filter, &data);
            }
        };
    }

    fn upload_texture_srgb(
        &mut self,
        pos: Option<[usize; 2]>,
        [w, h]: [usize; 2],
        texture_filter: TextureFilter,
        data: &[u8],
    ) {
        assert_eq!(data.len(), w * h * 4);
        assert!(
            w <= self.max_texture_side && h <= self.max_texture_side,
            "Got a texture image of size {}x{}, but the maximum supported texture side is only {}",
            w,
            h,
            self.max_texture_side
        );

        unsafe {
            gl::TexParameteri(
                gl::TEXTURE_2D,
                gl::TEXTURE_MAG_FILTER,
                texture_filter.glow_code() as i32,
            );
            gl::TexParameteri(
                gl::TEXTURE_2D,
                gl::TEXTURE_MIN_FILTER,
                texture_filter.glow_code() as i32,
            );

            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);

            let (internal_format, src_format) = (gl::SRGB8_ALPHA8, gl::RGBA);

            gl::PixelStorei(gl::UNPACK_ALIGNMENT, 1);

            let level = 0;
            if let Some([x, y]) = pos {
                gl::TexSubImage2D(
                    gl::TEXTURE_2D,
                    level,
                    x as _,
                    y as _,
                    w as _,
                    h as _,
                    src_format,
                    gl::UNSIGNED_BYTE,
                    data.as_ptr().cast(),
                );
            } else {
                let border = 0;
                gl::TexImage2D(
                    gl::TEXTURE_2D,
                    level,
                    internal_format as _,
                    w as _,
                    h as _,
                    border,
                    src_format,
                    gl::UNSIGNED_BYTE,
                    data.as_ptr().cast(),
                );
            }
        }
    }

    pub fn free_texture(&mut self, tex_id: egui::TextureId) {
        if let Some(old_tex) = self.textures.remove(&tex_id) {
            unsafe { gl::DeleteTextures(1, &old_tex) };
        }
    }

    /// Get the [`Texture`] bound to a [`egui::TextureId`].
    pub fn texture(&self, texture_id: egui::TextureId) -> Option<Texture> {
        self.textures.get(&texture_id).copied()
    }

    #[allow(clippy::needless_pass_by_value)] // False positive
    pub fn register_native_texture(&mut self, native: Texture) -> egui::TextureId {
        self.assert_not_destroyed();
        let id = egui::TextureId::User(self.next_native_tex_id);
        self.next_native_tex_id += 1;
        self.textures.insert(id, native);
        id
    }

    #[allow(clippy::needless_pass_by_value)] // False positive
    pub fn replace_native_texture(&mut self, id: egui::TextureId, replacing: Texture) {
        if let Some(old_tex) = self.textures.insert(id, replacing) {
            self.textures_to_destroy.push(old_tex);
        }
    }

    unsafe fn destroy_gl(&self) {
        gl::DeleteProgram(self.program);
        for tex in self.textures.values() {
            gl::DeleteTextures(1, tex);
        }
        gl::DeleteBuffers(1, &self.vbo);
        gl::DeleteBuffers(1, &self.element_array_buffer);
        for t in &self.textures_to_destroy {
            gl::DeleteTextures(1, t);
        }
    }

    /// This function must be called before [`Painter`] is dropped, as [`Painter`] has some OpenGL objects
    /// that should be deleted.
    pub fn destroy(&mut self) {
        if !self.destroyed {
            unsafe {
                self.destroy_gl();
            }
            self.destroyed = true;
        }
    }

    fn assert_not_destroyed(&self) {
        assert!(!self.destroyed, "the egui glow has already been destroyed!");
    }
}

impl Default for Painter {
    fn default() -> Self {
        Self::new()
    }
}

pub fn clear(screen_size_in_pixels: [u32; 2], clear_color: egui::Rgba) {
    puffin::profile_function!();
    unsafe {
        gl::Disable(gl::SCISSOR_TEST);

        gl::Viewport(
            0,
            0,
            screen_size_in_pixels[0] as i32,
            screen_size_in_pixels[1] as i32,
        );

        if true {
            // verified to be correct on eframe native (on Mac).
            gl::ClearColor(
                clear_color[0],
                clear_color[1],
                clear_color[2],
                clear_color[3],
            );
        } else {
            let clear_color: Color32 = clear_color.into();
            gl::ClearColor(
                clear_color[0] as f32 / 255.0,
                clear_color[1] as f32 / 255.0,
                clear_color[2] as f32 / 255.0,
                clear_color[3] as f32 / 255.0,
            );
        }
        gl::Clear(gl::COLOR_BUFFER_BIT);
    }
}

impl Drop for Painter {
    fn drop(&mut self) {
        if !self.destroyed {
            log::warn!(
                "You forgot to call destroy() on the egui glow painter. Resources will leak!"
            );
        }
    }
}

fn set_clip_rect(size_in_pixels: (u32, u32), pixels_per_point: f32, clip_rect: Rect) {
    // Transform clip rect to physical pixels:
    let clip_min_x = pixels_per_point * clip_rect.min.x;
    let clip_min_y = pixels_per_point * clip_rect.min.y;
    let clip_max_x = pixels_per_point * clip_rect.max.x;
    let clip_max_y = pixels_per_point * clip_rect.max.y;

    // Round to integer:
    let clip_min_x = clip_min_x.round() as i32;
    let clip_min_y = clip_min_y.round() as i32;
    let clip_max_x = clip_max_x.round() as i32;
    let clip_max_y = clip_max_y.round() as i32;

    // Clamp:
    let clip_min_x = clip_min_x.clamp(0, size_in_pixels.0 as i32);
    let clip_min_y = clip_min_y.clamp(0, size_in_pixels.1 as i32);
    let clip_max_x = clip_max_x.clamp(clip_min_x, size_in_pixels.0 as i32);
    let clip_max_y = clip_max_y.clamp(clip_min_y, size_in_pixels.1 as i32);

    unsafe {
        gl::Scissor(
            clip_min_x,
            size_in_pixels.1 as i32 - clip_max_y,
            clip_max_x - clip_min_x,
            clip_max_y - clip_min_y,
        );
    }
}

pub fn compile_shader(src: &str, ty: GLenum) -> GLuint {
    let shader;
    unsafe {
        shader = gl::CreateShader(ty);
        // Attempt to compile the shader
        let c_str = CString::new(src.as_bytes()).unwrap();
        gl::ShaderSource(shader, 1, &c_str.as_ptr(), std::ptr::null());
        gl::CompileShader(shader);
        // Get the compile status
        let mut status = gl::FALSE as GLint;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);

        // Fail on error
        if status != (gl::TRUE as GLint) {
            let mut log_len: gl::types::GLint = 0;
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut log_len);

            let log_str = CString::from_vec_unchecked(vec![b'\0'; (log_len + 1) as usize]);
            gl::GetShaderInfoLog(
                shader,
                log_len,
                std::ptr::null_mut(),
                log_str.as_ptr() as *mut gl::types::GLchar,
            );

            panic!("{}", log_str.to_string_lossy());
        }
    }
    shader
}

pub fn link_program(vs: GLuint, fs: GLuint) -> GLuint {
    unsafe {
        let program = gl::CreateProgram();
        gl::AttachShader(program, vs);
        gl::AttachShader(program, fs);
        gl::LinkProgram(program);
        // Get the link status
        let mut status = gl::FALSE as GLint;
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut status);

        // Fail on error
        if status != (gl::TRUE as GLint) {
            let mut log_len: gl::types::GLint = 0;
            gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut log_len);

            let log_str = CString::from_vec_unchecked(vec![b'\0'; (log_len + 1) as usize]);
            gl::GetProgramInfoLog(
                program,
                log_len,
                std::ptr::null_mut(),
                log_str.as_ptr() as *mut gl::types::GLchar,
            );

            panic!("{}", log_str.to_string_lossy());
        }
        program
    }
}
