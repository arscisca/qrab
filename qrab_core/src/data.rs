/// Encoding mode of a data segment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    /// Encoding for digits 0-9.
    Num,
    /// Encoding for digita 0-9 and capital characters A-Z.
    Alnum,
    /// Encoding for arbitrary text or byte string.
    Bytes,
}

impl Mode {
    /// Return the [Mode] that is the most generic between `self` and `other`.
    /// # Example
    /// ```
    /// use qrab_core::Mode;
    /// assert_eq!(Mode::Alnum.most_generic(Mode::Bytes), Mode::Bytes);
    /// assert_eq!(Mode::Alnum.most_generic(Mode::Num), Mode::Alnum);
    /// ```
    pub fn most_generic(self, other: Self) -> Self {
        std::cmp::max(self, other)
    }

    /// Determine whether `self` can be promoted to `other`, meaning that `other` is a more generic data representation.
    /// # Example
    /// ```
    /// use qrab_core::Mode;
    /// assert!(Mode::Alnum.could_be_promoted_to(Mode::Bytes));
    /// assert!(!Mode::Bytes.could_be_promoted_to(Mode::Num));
    /// ```
    pub fn could_be_promoted_to(self, other: Self) -> bool {
        self < other
    }
}

impl From<u8> for Mode {
    fn from(value: u8) -> Self {
        match value {
            0x30..=0x39 => Mode::Num,
            0x20 | 0x24 | 0x25 | 0x2a | 0x2b | 0x2d..=0x2f | 0x3a | 0x41..=0x5a => Mode::Alnum,
            _ => Mode::Bytes,
        }
    }
}

/// Contiguous segment of data with a [Mode].
#[derive(Debug, Clone)]
pub struct Segment {
    pub mode: Mode,
    pub len: usize,
}

impl Segment {
    /// Construct a new [Segment] with the given `mode` and `len`.
    pub fn new(mode: Mode, len: usize) -> Self {
        Self { mode, len }
    }

    /// Merge two data segments, choosing the most generic [Mode] of the two.
    pub fn merge(&self, other: &Self) -> Self {
        Self {
            mode: self.mode.most_generic(other.mode),
            len: self.len + other.len,
        }
    }
}
