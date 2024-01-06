/// The kind of a data segment.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum SegmentKind {
    Numeric,
    Alphanumeric,
    Bytes,
}

impl SegmentKind {
    /// Promote `self` to the next more generic `SegmentKind`, promoting `Numeric` into
    /// `Alphanumeric` and `Alphanumeric` into `Bytes`.
    pub fn promote(self) -> Self {
        match self {
            Self::Numeric => Self::Alphanumeric,
            Self::Alphanumeric => Self::Bytes,
            Self::Bytes => Self::Bytes,
        }
    }

    /// Get the most generic `SegmentKind` between `self` and `other`.
    pub fn most_generic(self, other: Self) -> Self {
        std::cmp::max(self, other)
    }
}

/// A segment of contiguous data of a common `SegmentKind`.
#[derive(Clone)]
pub(crate) struct Segment<'a> {
    data: &'a [u8],
    range: std::ops::Range<usize>,
    kind: SegmentKind,
}

impl<'a> Segment<'a> {
    /// Create a new segment in the `range` of the `data` slice. Note that `data` refers to the slice that is yet-to-be
    /// segmented.
    pub fn new(data: &'a [u8], range: std::ops::Range<usize>, kind: SegmentKind) -> Self {
        Self { data, range, kind }
    }

    /// Return the slice of data belonging to the segment.
    pub fn data(&self) -> &[u8] {
        &self.data[self.range.clone()]
    }

    /// Get the `SegmentKind`.
    pub fn kind(&self) -> SegmentKind {
        self.kind
    }

    /// Get the range of this segment.
    pub fn range(&self) -> &std::ops::Range<usize> {
        &self.range
    }

    /// Get the length of this segment based on its range.
    pub fn len(&self) -> usize {
        self.range.len()
    }

    /// Try to merge segments `s1` and `s2`. Merging can only happen if the two segments are
    /// contiguous (s1's range end is s2's range start) and if they share the same `SegmentKind`.
    /// If `s1` and `s2` cannot be merged, they will be returned unchanged in the `Err` variant
    /// of the result.
    pub fn try_merge(s1: Self, s2: Self) -> Result<Self, (Self, Self)> {
        // Check that the two segments are referencing the same data
        if !std::ptr::eq(s1.data.as_ptr(), s2.data.as_ptr()) {
            return Err((s1, s2));
        }
        // Check that the two segments are contiguous
        if s1.range.end != s2.range.start {
            return Err((s1, s2));
        }
        // Check that they share the same kind
        if s1.kind != s2.kind {
            return Err((s1, s2));
        }
        Ok(Self::new(s1.data, s1.range.start..s2.range.end, s1.kind))
    }
}

impl<'a> std::fmt::Debug for Segment<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{:?}[{:?}]({:?})",
            self.kind,
            self.range,
            self.data
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn successful_merge() {
        let data = "FirstSecond".as_bytes();
        let s1 = Segment::new(data, 0..5, SegmentKind::Bytes);
        let s2 = Segment::new(data, 5..data.len(), SegmentKind::Bytes);
        assert_eq!(s1.data(), "First".as_bytes());
        assert_eq!(s2.data(), "Second".as_bytes());
        let merged = Segment::try_merge(s1, s2).unwrap();
        assert_eq!(merged.range, 0..data.len());
        assert_eq!(merged.data, "FirstSecond".as_bytes());
        assert_eq!(merged.kind, SegmentKind::Bytes);
    }

    #[test]
    fn no_merge_different_data() {
        let d1 = "data1".as_bytes();
        let d2 = "data2".as_bytes();
        let r1 = 0..2;
        let r2 = 2..4;
        let s1 = Segment::new(d1, r1.clone(), SegmentKind::Bytes);
        let s2 = Segment::new(d2, r2.clone(), SegmentKind::Bytes);
        assert!(Segment::try_merge(s1, s2).is_err());
        // Same data instead merges
        let s1 = Segment::new(d1, r1, SegmentKind::Bytes);
        let s2 = Segment::new(d1, r2, SegmentKind::Bytes);
        assert!(Segment::try_merge(s1, s2).is_ok())
    }
}
