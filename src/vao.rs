#![allow(unsafe_code)]

// ----------------------------------------------------------------------------

use gl::types::*;

#[derive(Debug)]
pub(crate) struct BufferInfo {
    pub location: i32,
    pub vector_size: i32,
    pub data_type: u32, //GL_FLOAT,GL_UNSIGNED_BYTE
    pub normalized: bool,
    pub stride: i32,
    pub offset: u32,
}

// ----------------------------------------------------------------------------

/// Wrapper around either Emulated VAO or GL's VAO.
pub(crate) struct VertexArrayObject {
    // If `None`, we emulate VAO:s.
    vao: GLuint,
}

impl VertexArrayObject {
    #[allow(clippy::needless_pass_by_value)] // false positive
    pub(crate) unsafe fn new(vbo: GLuint, buffer_infos: Vec<BufferInfo>) -> Self {
        let vao = {
            let mut vao = 0;
            gl::CreateVertexArrays(1, &mut vao);
            vao
        };

        // Store state in the VAO:
        gl::BindVertexArray(vao);
        gl::BindBuffer(gl::ARRAY_BUFFER, vbo);

        for attribute in &buffer_infos {
            gl::VertexAttribPointer(
                attribute.location as _,
                attribute.vector_size,
                attribute.data_type,
                if attribute.normalized {
                    gl::TRUE
                } else {
                    gl::FALSE
                },
                attribute.stride,
                std::ptr::null::<u8>().add(attribute.offset as usize).cast(),
            );
            gl::EnableVertexAttribArray(attribute.location as _);
        }

        gl::BindVertexArray(0);

        Self { vao }
    }

    pub(crate) unsafe fn bind(&self) {
        gl::BindVertexArray(self.vao);
    }

    #[allow(clippy::unused_self)]
    pub(crate) unsafe fn unbind(&self) {
        gl::BindVertexArray(0);
    }
}
