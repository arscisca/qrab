pub(crate) mod builder;
pub(crate) mod ecc;
pub(crate) mod encoder;
pub(crate) mod segment;

use crate::*;
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

/// Settings for an encoder.
#[derive(Clone)]
pub(crate) struct Settings {
    pub version: Version,
    pub ecl: Ecl,
}

impl Settings {
    /// Initialize with the specified `version` and `ecl`.
    pub fn new(version: Version, ecl: Ecl) -> Self {
        Self { version, ecl }
    }
}

impl std::fmt::Debug for Settings {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}-{}", self.version, self.ecl)
    }
}