pub(crate) mod builder;
pub(crate) mod ecc;
pub(crate) mod encoder;
pub(crate) mod segment;

use crate::{Ecl, Version};
pub(crate) use builder::Builder;

pub use encoder::{Constraint, Encoder};

/// Encoding error.
#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    /// Data is too large for the selected QR code version.
    #[error("data is too long for a QR version {version}-{ecl}")]
    DataTooLarge { version: Version, ecl: Ecl },
    #[error("building error")]
    BuildingError(#[from] builder::BuildingError),
    // TODO: Remove this
    /// A feature is not supported.
    #[error("feature not supported: {0}")]
    NotSupported(String),
}
