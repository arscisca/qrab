mod canvas;
mod data;
mod meta;
pub mod qrstandard;

pub use canvas::{Canvas, Module};
pub use data::{Mode, Segment};
pub use meta::{Ecl, Mask, MaskTable, Meta, Version};

/// QR code.
pub struct QrCode {
    canvas: Canvas,
    meta: Meta,
}

impl QrCode {
    /// Construct a new [QrCode]. Returns `None` if the canvas is incompatible with `meta` (e.g.: the [Version] and,
    /// thus, the canvas size).
    pub fn new(canvas: Canvas, meta: Meta) -> Option<Self> {
        if canvas.size() == meta.canvas_size() {
            Some(Self { canvas, meta })
        } else {
            None
        }
    }

    /// Get the underlying canvas.
    pub fn canvas(&self) -> &Canvas {
        &self.canvas
    }

    /// Get the metadata.
    pub fn meta(&self) -> &Meta {
        &self.meta
    }
}

impl AsRef<Canvas> for QrCode {
    fn as_ref(&self) -> &Canvas {
        self.canvas()
    }
}

impl From<QrCode> for Canvas {
    fn from(value: QrCode) -> Self {
        value.canvas
    }
}
