/// The encoding mode of a data segment.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    Numeric,
    Alphanumeric,
    Bytes,
}

impl Mode {
    /// Select the strictest mode for `data` - a.k.a. the "exclusive set" to which `data` belongs.
    pub fn strictest(data: u8) -> Self {
        match data {
            0x30..=0x39 => Mode::Numeric,
            0x20 | 0x24 | 0x25 | 0x2a | 0x2b | 0x2d..=0x2f | 0x3a | 0x41..=0x5a => {
                Mode::Alphanumeric
            }
            _ => Mode::Bytes,
        }
    }

    /// Promote to the next more generic `SegmentKind`, promoting `Numeric` into `Alphanumeric` and `Alphanumeric` into
    /// `Bytes`. `Bytes` mode is unchanged.
    pub fn promote(self) -> Self {
        match self {
            Self::Numeric => Self::Alphanumeric,
            Self::Alphanumeric => Self::Bytes,
            Self::Bytes => Self::Bytes,
        }
    }

    /// Return `true` if `self` is equal to or more generic than `other`.
    pub fn is_eq_or_more_generic(self, other: Self) -> bool {
        self >= other
    }

    /// Get the most generic `SegmentKind` between `self` and `other`.
    pub fn most_generic(self, other: Self) -> Self {
        std::cmp::max(self, other)
    }
}

/// A segment of contiguous data of a common `SegmentKind`.
#[derive(Clone, Debug)]
pub struct Segment<'a> {
    data: &'a [u8],
    mode: Mode,
}

impl<'a> Segment<'a> {
    /// Create a new segment in the `range` of the `data` slice. Note that `data` refers to the slice that is yet-to-be
    /// segmented.
    pub fn new(data: &'a [u8], mode: Mode) -> Self {
        Self { data, mode }
    }

    /// Return the slice of data belonging to the segment.
    pub fn data(&self) -> &[u8] {
        self.data
    }

    /// Get the mode of this segment.
    pub fn mode(&self) -> Mode {
        self.mode
    }

    /// Get the length of this segment based on its range.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Return `true` if the segment has no data.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}
