//! [`egui`] bindings for [`glium`](https://crates.io/crates/glutin).
//!
//! The main type you want to use is [`EguiBackend`].
//!
//! If you are writing an app, you may want to look at [`eframe`](https://docs.rs/eframe) instead.
//!
//! ## Feature flags
#![cfg_attr(feature = "document-features", doc = document_features::document_features!())]
//!

#![allow(clippy::float_cmp)]
#![allow(clippy::manual_range_contains)]

mod backend;
mod context_free_backend;
mod painter;
mod vao;

pub use backend::EguiBackend;
pub use context_free_backend::EguiContextFreeBackend;
