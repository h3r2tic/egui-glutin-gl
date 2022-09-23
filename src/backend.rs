//! [`egui`] bindings for [`glium`](https://github.com/glium/glium).
//!
//! The main type you want to use is [`EguiGlium`].
//!
//! If you are writing an app, you may want to look at [`eframe`](https://docs.rs/eframe) instead.
//!
//! ## Feature flags
#![cfg_attr(feature = "document-features", doc = document_features::document_features!())]
//!

#![allow(clippy::float_cmp)]
#![allow(clippy::manual_range_contains)]

pub use egui_winit;

use egui_winit::winit::event_loop::EventLoopWindowTarget;
use glutin::event::WindowEvent;

use super::painter::Painter;

// ----------------------------------------------------------------------------

/// Convenience wrapper for using [`egui`] from a [`glium`] app.
pub struct EguiBackend {
    pub egui_ctx: egui::Context,
    pub egui_winit: egui_winit::State,
    pub painter: Painter,

    shapes: Vec<egui::epaint::ClippedShape>,
    textures_delta: egui::TexturesDelta,
}

type Display = glutin::ContextWrapper<glutin::PossiblyCurrent, glutin::window::Window>;

impl EguiBackend {
    pub fn new<E>(display: &Display, event_loop: &EventLoopWindowTarget<E>) -> Self {
        let painter = Painter::new();

        let mut egui_winit = egui_winit::State::new(event_loop);
        egui_winit.set_max_texture_side(2048);

        let pixels_per_point = display.window().scale_factor() as f32;
        egui_winit.set_pixels_per_point(pixels_per_point);

        Self {
            egui_ctx: Default::default(),
            egui_winit,
            painter,
            shapes: Default::default(),
            textures_delta: Default::default(),
        }
    }

    pub fn on_event(&mut self, event: &WindowEvent<'_>) -> bool {
        self.egui_winit.on_event(&self.egui_ctx, event)
    }

    /// Returns `true` if egui requests a repaint.
    ///
    /// Call [`Self::paint`] later to paint.
    pub fn run(
        &mut self,
        display: &Display,
        run_ui: impl FnMut(&egui::Context),
    ) -> std::time::Duration {
        let raw_input = self.egui_winit.take_egui_input(display.window());
        let egui::FullOutput {
            platform_output,
            repaint_after,
            textures_delta,
            shapes,
        } = self.egui_ctx.run(raw_input, run_ui);

        self.egui_winit
            .handle_platform_output(display.window(), &self.egui_ctx, platform_output);

        self.shapes = shapes;
        self.textures_delta.append(textures_delta);

        repaint_after
    }

    /// Paint the results of the last call to [`Self::run`].
    pub fn paint(&mut self, display: &Display) {
        let shapes = std::mem::take(&mut self.shapes);
        let textures_delta = std::mem::take(&mut self.textures_delta);
        let clipped_primitives = self.egui_ctx.tessellate(shapes);

        let pixels_per_point = self.egui_ctx.pixels_per_point();
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
