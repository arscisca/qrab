//! Library for QR code generation and scanning.

pub mod encode;
mod info;
mod qrcode;

pub use qrcode::{Matrix, Module, QrCode};

use info::Segment;
pub use info::{Ecl, InvalidVersionNumber, Mask, Mode, QrInfo, Version};

// Encoding re-exports
pub use encode::{Encoder, EncodingConstraints, EncodingError};
