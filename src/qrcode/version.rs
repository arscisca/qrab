/// Version of a QR code symbol. It determines various attributes such as its size and data capacity. Micro QR versions
/// are not supported.
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[rustfmt::skip]
pub enum Version {
     V1 =  1,  V2 =  2,  V3 =  3,  V4 =  4,  V5 =  5,  V6 =  6,  V7 =  7,  V8 =  8,  V9 =  9, V10 = 10,
    V11 = 11, V12 = 12, V13 = 13, V14 = 14, V15 = 15, V16 = 16, V17 = 17, V18 = 18, V19 = 19, V20 = 20,
    V21 = 21, V22 = 22, V23 = 23, V24 = 24, V25 = 25, V26 = 26, V27 = 27, V28 = 28, V29 = 29, V30 = 30,
    V31 = 31, V32 = 32, V33 = 33, V34 = 34, V35 = 35, V36 = 36, V37 = 37, V38 = 38, V39 = 39, V40 = 40,
}

impl Version {
    /// Get the version number as a `u8`.
    pub const fn number(&self) -> u8 {
        *self as u8
    }
}

impl From<Version> for u8 {
    fn from(value: Version) -> Self {
        value.number()
    }
}

impl TryFrom<u8> for Version {
    type Error = InvalidVersionNumber;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::V1),
            2 => Ok(Self::V2),
            3 => Ok(Self::V3),
            4 => Ok(Self::V4),
            5 => Ok(Self::V5),
            6 => Ok(Self::V6),
            7 => Ok(Self::V7),
            8 => Ok(Self::V8),
            9 => Ok(Self::V9),
            10 => Ok(Self::V10),
            11 => Ok(Self::V11),
            12 => Ok(Self::V12),
            13 => Ok(Self::V13),
            14 => Ok(Self::V14),
            15 => Ok(Self::V15),
            16 => Ok(Self::V16),
            17 => Ok(Self::V17),
            18 => Ok(Self::V18),
            19 => Ok(Self::V19),
            20 => Ok(Self::V20),
            21 => Ok(Self::V21),
            22 => Ok(Self::V22),
            23 => Ok(Self::V23),
            24 => Ok(Self::V24),
            25 => Ok(Self::V25),
            26 => Ok(Self::V26),
            27 => Ok(Self::V27),
            28 => Ok(Self::V28),
            29 => Ok(Self::V29),
            30 => Ok(Self::V30),
            31 => Ok(Self::V31),
            32 => Ok(Self::V32),
            33 => Ok(Self::V33),
            34 => Ok(Self::V34),
            35 => Ok(Self::V35),
            36 => Ok(Self::V36),
            37 => Ok(Self::V37),
            38 => Ok(Self::V38),
            39 => Ok(Self::V39),
            40 => Ok(Self::V40),
            n => Err(InvalidVersionNumber(n)),
        }
    }
}

impl std::fmt::Debug for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.number())
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.number())
    }
}

/// Error type for invalid `Version`s.
#[derive(thiserror::Error)]
#[error("invalid version number: {0}")]
pub struct InvalidVersionNumber(pub u8);

impl std::fmt::Debug for InvalidVersionNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn comparison() {
        assert_eq!(Version::V5, Version::V5);
        assert!(Version::V3 < Version::V8);
        assert!(Version::V29 >= Version::V17);
    }
}
