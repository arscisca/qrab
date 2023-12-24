/// Version of the QR code.
/// Versions determine various attributes of the QR code such as its size. Valid version numbers
/// range from 1 to 40 (included).
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version {
    number: u8,
}

impl Version {
    /// Create a new `Version` from its `number`, which must be inside the range `1..=40`.
    pub const fn new(number: u8) -> Result<Self, InvalidVersionNumber> {
        match number {
            number @ 1..=40 => Ok(Self { number }),
            number => Err(InvalidVersionNumber(number)),
        }
    }

    /// Get the version number.
    pub fn number(&self) -> u8 {
        self.number
    }
}

impl TryFrom<u8> for Version {
    type Error = InvalidVersionNumber;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<Version> for u8 {
    fn from(value: Version) -> Self {
        value.number
    }
}

impl std::fmt::Debug for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.number)
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.number)
    }
}

/// Error type for invalid `Version`s.
#[derive(thiserror::Error)]
#[error("invalid version number: {0}")]
pub struct InvalidVersionNumber(u8);

impl std::fmt::Debug for InvalidVersionNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
