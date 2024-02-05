//! Library for QR code generation and scanning.

pub mod encode;
mod info;
mod qrcode;

pub use qrcode::{Matrix, Module, QrCode};

pub use info::{Ecl, InvalidVersionNumber, Mask, QrInfo, Version};

// Encoding re-exports
pub use encode::{Encoder, EncodingConstraints, EncodingError};
