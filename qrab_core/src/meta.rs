use crate::qrstandard;

/// Collection of metadata about a QR code.
#[derive(Debug, Clone)]
pub struct Meta {
    pub version: Version,
    pub ecl: Ecl,
    pub mask: Mask,
}

impl Meta {
    pub fn canvas_size(&self) -> usize {
        qrstandard::canvas_size(self.version)
    }
}

/// Version of a QR code, which determines its size.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
#[rustfmt::skip]
pub enum Version {
    V01 =  1, V02 =  2, V03 =  3, V04 =  4, V05 =  5, V06 =  6, V07 =  7, V08 =  8, V09 =  9, V10 = 10,
    V11 = 11, V12 = 12, V13 = 13, V14 = 14, V15 = 15, V16 = 16, V17 = 17, V18 = 18, V19 = 19, V20 = 20,
    V21 = 21, V22 = 22, V23 = 23, V24 = 24, V25 = 25, V26 = 26, V27 = 27, V28 = 28, V29 = 29, V30 = 30,
    V31 = 31, V32 = 32, V33 = 33, V34 = 34, V35 = 35, V36 = 36, V37 = 37, V38 = 38, V39 = 39, V40 = 40,
}

impl Version {
    /// Construct a new version given its number. Valid version numbers are in the range 1..=40.
    /// # Example
    /// ```
    /// use qrab_core::Version;
    /// assert!(Version::new(1).is_some());
    /// assert!(Version::new(50).is_none());
    /// ```
    pub const fn new(number: u8) -> Option<Self> {
        let version = match number {
             1 => Self::V01,  2 => Self::V02,  3 => Self::V03,  4 => Self::V04,  5 => Self::V05,  6 => Self::V06,  7 => Self::V07,  8 => Self::V08,  9 => Self::V09, 10 => Self::V10,
            11 => Self::V11, 12 => Self::V12, 13 => Self::V13, 14 => Self::V14, 15 => Self::V15, 16 => Self::V16, 17 => Self::V17, 18 => Self::V18, 19 => Self::V19, 20 => Self::V20,
            21 => Self::V21, 22 => Self::V22, 23 => Self::V23, 24 => Self::V24, 25 => Self::V25, 26 => Self::V26, 27 => Self::V27, 28 => Self::V28, 29 => Self::V29, 30 => Self::V30,
            31 => Self::V31, 32 => Self::V32, 33 => Self::V33, 34 => Self::V34, 35 => Self::V35, 36 => Self::V36, 37 => Self::V37, 38 => Self::V38, 39 => Self::V39, 40 => Self::V40,
            _ => return None,
        };
        Some(version)
    }

    /// Get the version number.
    /// # Example
    /// ```
    /// use qrab_core::Version;
    /// assert_eq!(Version::V12.number(), 12);
    /// ```
    pub fn number(self) -> u8 {
        self as u8
    }

    /// Get the next higher version, if it exists.
    /// # Example
    /// ```
    /// use qrab_core::Version;
    /// assert_eq!(Version::V10.incr(), Some(Version::V11));
    /// assert_eq!(Version::V40.incr(), None);
    /// ```
    pub fn incr(self) -> Option<Self> {
        Self::new(self.number() + 1)
    }

    /// Get the next lower version, if it exists.
    /// # Example
    /// ```
    /// use qrab_core::Version;
    /// assert_eq!(Version::V10.decr(), Some(Version::V09));
    /// assert_eq!(Version::V01.decr(), None);
    /// ```
    pub fn decr(self) -> Option<Self> {
        Self::new(self.number() - 1)
    }
}

impl From<Version> for u8 {
    fn from(value: Version) -> Self {
        value.number()
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "V{}", self.number())
    }
}

/// Error correction level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Ecl {
    /// Low: 7% recovery rate.
    L,
    /// Medium: 15% recovery rate.
    M,
    /// Quartile: 25% recovery rate.
    Q,
    /// High: 30% recovery rate.
    H,
}

impl Ecl {
    /// Get the next higher ECL, if it exists.
    /// # Example
    /// ```
    /// use qrab_core::Ecl;
    /// assert_eq!(Ecl::L.incr(), Some(Ecl::M));
    /// assert_eq!(Ecl::H.incr(), None)
    /// ```
    pub fn incr(self) -> Option<Self> {
        match self {
            Self::L => Some(Self::M),
            Self::M => Some(Self::Q),
            Self::Q => Some(Self::H),
            Self::H => None,
        }
    }

    /// Get the next lower ECL, if it exists.
    /// # Example
    /// ```
    /// use qrab_core::Ecl;
    /// assert_eq!(Ecl::M.decr(), Some(Ecl::L));
    /// assert_eq!(Ecl::L.decr(), None)
    /// ```
    pub fn decr(self) -> Option<Self> {
        match self {
            Self::L => None,
            Self::M => Some(Self::L),
            Self::Q => Some(Self::M),
            Self::H => Some(Self::Q),
        }
    }
}

/// Mask used in the QR code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Mask {
    M000 = 0b000,
    M001 = 0b001,
    M010 = 0b010,
    M011 = 0b011,
    M100 = 0b100,
    M101 = 0b101,
    M110 = 0b110,
    M111 = 0b111,
}

impl Mask {
    /// Construct a new mask given its `code`. Valid codes are in the range 0..=7.
    pub const fn new(code: u8) -> Option<Self> {
        match code {
            0 => Some(Self::M000),
            1 => Some(Self::M001),
            2 => Some(Self::M010),
            3 => Some(Self::M011),
            4 => Some(Self::M100),
            5 => Some(Self::M101),
            6 => Some(Self::M110),
            7 => Some(Self::M111),
            _ => None,
        }
    }

    /// Get the code associated to the mask.
    pub fn code(self) -> u8 {
        self as u8
    }

    /// Get the function of position `(i, j)` in a canvas associated to this mask. If `f(i, j) == true`, then the
    /// module at position `(i, j)` needs to be inverted.
    pub fn function(&self) -> fn(usize, usize) -> bool {
        match self {
            Self::M000 => |i, j| (i + j) % 2 == 0,
            Self::M001 => |i, _| i % 2 == 0,
            Self::M010 => |_, j| j % 3 == 0,
            Self::M011 => |i, j| (i + j) % 3 == 0,
            Self::M100 => |i, j| (i / 2 + j / 3) % 2 == 0,
            Self::M101 => |i, j| (i * j) % 2 + (i * j) % 3 == 0,
            Self::M110 => |i, j| ((i * j) % 2 + (i * j) % 3) % 2 == 0,
            Self::M111 => |i, j| ((i + j) % 2 + (i * j) % 3) % 2 == 0,
        }
    }
}

const NUM_MASKS: usize = 8;

/// Table mapping each possible [Mask] to an arbitrary value of generic type `T`.
#[derive(Default, Clone, PartialEq, Eq)]
pub struct MaskTable<T> {
    data: [T; NUM_MASKS],
}

impl<T: Clone> MaskTable<T> {
    /// Fill the table with `value`.
    pub fn fill(&mut self, value: T) {
        self.data.fill(value)
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for MaskTable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for (mask, value) in self.data.iter().enumerate() {
            writeln!(f, "    M{:03b} => {:?},", mask, value)?;
        }
        writeln!(f, "}}")
    }
}

impl<T> From<[T; NUM_MASKS]> for MaskTable<T> {
    fn from(value: [T; NUM_MASKS]) -> Self {
        Self { data: value }
    }
}

impl<T> std::ops::Index<Mask> for MaskTable<T> {
    type Output = T;

    fn index(&self, index: Mask) -> &Self::Output {
        &self.data[index.code() as usize]
    }
}

impl<T> std::ops::IndexMut<Mask> for MaskTable<T> {
    fn index_mut(&mut self, index: Mask) -> &mut Self::Output {
        &mut self.data[index.code() as usize]
    }
}

