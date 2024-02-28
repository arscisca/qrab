//! Generate and scan QR codes.
mod encode;
mod info;
mod qrcode;

pub use encode::{Encoder, EncodingConstraints, EncodingError};
pub use info::{Ecl, InvalidVersionNumber, Mask, Mode, QrInfo, Version};
pub use qrcode::{Matrix, Module, QrCode};
