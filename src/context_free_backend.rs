pub use egui_winit;

use egui_winit::winit::event_loop::EventLoopWindowTarget;
use glutin::event::WindowEvent;

use super::painter::Painter;

// ----------------------------------------------------------------------------

/// Convenience wrapper for using [`egui`] from a [`glutin`] app.
///
/// Use this if you'd rather keep the [`egui::Context`] separate from the backend.
pub struct EguiContextFreeBackend {
    pub egui_winit: egui_winit::State,
    pub painter: Painter,

    shapes: Vec<egui::epaint::ClippedShape>,
    textures_delta: egui::TexturesDelta,
}

type Display = glutin::ContextWrapper<glutin::PossiblyCurrent, glutin::window::Window>;

impl EguiContextFreeBackend {
    pub fn new<E>(display: &Display, event_loop: &EventLoopWindowTarget<E>) -> Self {
        let painter = Painter::new();

        let mut egui_winit = egui_winit::State::new(event_loop);
        egui_winit.set_max_texture_side(2048);

        let pixels_per_point = display.window().scale_factor() as f32;
        egui_winit.set_pixels_per_point(pixels_per_point);

        Self {
            egui_winit,
            painter,
            shapes: Default::default(),
            textures_delta: Default::default(),
        }
    }

    pub fn on_event(&mut self, egui_ctx: &egui::Context, event: &WindowEvent<'_>) -> bool {
        self.egui_winit.on_event(egui_ctx, event)
    }

    pub fn take_input(&mut self, display: &Display) -> egui::RawInput {
        self.egui_winit.take_egui_input(display.window())
    }

    pub fn handle_output(
        &mut self,
        egui_ctx: &egui::Context,
        full_output: egui::FullOutput,
        display: &Display,
    ) -> std::time::Duration {
        let egui::FullOutput {
            platform_output,
            repaint_after,
            textures_delta,
            shapes,
        } = full_output;

        self.egui_winit
            .handle_platform_output(display.window(), egui_ctx, platform_output);

        self.shapes = shapes;
        self.textures_delta.append(textures_delta);

        repaint_after
    }

    pub fn paint(&mut self, egui_ctx: &egui::Context, display: &Display) {
        let shapes = std::mem::take(&mut self.shapes);
        let textures_delta = std::mem::take(&mut self.textures_delta);
        let clipped_primitives = egui_ctx.tessellate(shapes);

        let pixels_per_point = egui_ctx.pixels_per_point();
        let screen_size_px = display.window().inner_size().into();

        self.painter.paint_and_update_textures(
            screen_size_px,
            pixels_per_point,
            &clipped_primitives,
            &textures_delta,
        );
    }

    /// Call to release the allocated graphics resources.
    pub fn destroy(&mut self) {
        self.painter.destroy();
    }
}
