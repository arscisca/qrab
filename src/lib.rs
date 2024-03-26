//! Generate and scan QR codes.
mod encode;
mod info;
pub mod qrcode;

pub use encode::{Encoder, EncodingConstraints, EncodingError};
pub use info::{Ecl, InvalidVersionNumber, Mask, MaskTable, Mode, QrInfo, Version};
pub use qrcode::{Matrix, Module, QrCode};
