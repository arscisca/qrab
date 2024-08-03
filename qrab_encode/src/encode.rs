use std::ops::{Bound, RangeBounds, RangeInclusive};

use itertools::Itertools;

use qrab_core::{Ecl, Mask, MaskTable, Mode, QrCode, Segment, Version};

/// Encoder for a QR code.
pub struct Encoder {
    constraints: Constraints,
}

impl Encoder {
    /// Construct a new, unconstrained encoder.
    pub fn new() -> Self {
        Self {
            constraints: Default::default(),
        }
    }

    /// Encode `data`.
    pub fn encode<T: AsRef<[u8]>>(&self, data: T) -> Result<QrCode, EncodingError> {
        let data = data.as_ref();
        let segments = segment(data, &self.constraints)?;
        // TODO: Compression depends on version which we are fixing here. There may be more optimized compressions if we
        //  are forced to change version.
        let compressed = compress(&segments, *self.constraints.version.start());
        todo!("encode compressed segments {:?}", compressed)
    }

    /// Transform a range of any type to an inclusive range, given the absolute minimum and maximum values as well as
    /// functions to increment and decrement values.
    fn any_range_to_inclusive<T, R, I, D>(
        range: R,
        abs_min: T,
        abs_max: T,
        incr: I,
        decr: D,
    ) -> RangeInclusive<T>
    where
        T: Clone + Copy,
        R: RangeBounds<T>,
        I: FnOnce(T) -> Option<T>,
        D: FnOnce(T) -> Option<T>,
    {
        let min = match range.start_bound() {
            Bound::Included(&min) => min,
            Bound::Excluded(&min) => incr(min).unwrap_or(abs_max),
            Bound::Unbounded => abs_min,
        };
        let max = match range.end_bound() {
            Bound::Included(&max) => max,
            Bound::Excluded(&max) => decr(max).unwrap_or(abs_min),
            Bound::Unbounded => abs_max,
        };
        min..=max
    }

    /// Get the range of allowed [Version]s.
    pub fn allowed_versions(&self) -> &RangeInclusive<Version> {
        &self.constraints.version
    }

    /// Constrain to the specified `version`.
    pub fn with_version(mut self, version: Version) -> Self {
        self.constraints.version = version..=version;
        self
    }

    /// Constrain [Version] to be inside `range`.
    pub fn with_version_in<T: RangeBounds<Version>>(mut self, range: T) -> Self {
        self.constraints.version = Self::any_range_to_inclusive(
            range,
            Version::V01,
            Version::V40,
            Version::incr,
            Version::decr,
        );
        self
    }

    /// Get the range of allowed [Ecl]s.
    pub fn allowed_ecls(&self) -> &RangeInclusive<Ecl> {
        &self.constraints.ecl
    }

    /// Constrain to the specified `ecl`.
    pub fn with_ecl(mut self, ecl: Ecl) -> Self {
        self.constraints.ecl = ecl..=ecl;
        self
    }

    /// Constrain [Ecl] to be inside `range`.
    pub fn with_ecl_in<T: RangeBounds<Ecl>>(mut self, range: T) -> Self {
        self.constraints.ecl =
            Self::any_range_to_inclusive(range, Ecl::L, Ecl::H, Ecl::incr, Ecl::decr);
        self
    }

    /// Get the table of allowed [Mask]s.
    pub fn allowed_masks(&self) -> &MaskTable<bool> {
        &self.constraints.mask
    }

    /// Constrain [Mask] to be `mask`.
    pub fn with_mask(mut self, mask: Mask) -> Self {
        self.constraints.mask.fill(false);
        self.constraints.mask[mask] = true;
        self
    }

    /// Constrain [Mask] to be picked only from the elements yielded by `iter`
    pub fn with_mask_in<I: Iterator<Item = Mask>>(mut self, iter: I) -> Self {
        self.constraints.mask.fill(false);
        for mask in iter {
            self.constraints.mask[mask] = true;
        }
        self
    }

    /// Get the current [Mode] constraint.
    pub fn mode_constraint(&self) -> &ModeConstraint {
        &self.constraints.mode
    }

    /// Constrain the [Mode] selection with `constraint` while encoding.
    pub fn with_mode(mut self, constraint: ModeConstraint) -> Self {
        self.constraints.mode = constraint;
        self
    }
}

impl Default for Encoder {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
struct Constraints {
    ecl: RangeInclusive<Ecl>,
    version: RangeInclusive<Version>,
    mask: MaskTable<bool>,
    mode: ModeConstraint,
}

impl Default for Constraints {
    fn default() -> Self {
        let mut mask = MaskTable::default();
        mask.fill(true);
        Self {
            ecl: Ecl::L..=Ecl::H,
            version: Version::V01..=Version::V40,
            mask,
            mode: ModeConstraint::AnyMixed,
        }
    }
}

/// Constraint on the encoding mode of data segments.
#[derive(Debug, Clone)]
pub enum ModeConstraint {
    /// Allow the free choice of the best [Mode] for each data [Segment].
    AnyMixed,
    /// Allow the choice of any [Mode] which must be shared across all the [Segment]s. This implies that the chosen mode
    /// will be the most generic needed.
    AnySingle,
    /// Enforce a [Mode] or fail encoding if not applicable.
    Only(Mode),
}

#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error("cannot encode byte 0x{0:x} using mode {1:?}")]
    CannotEncodeWithMode(u8, Mode),
}

/// Segment `data` by associating a [Mode] to each byte, and then grouping contiguous stretches of the same mode.
fn segment(data: &[u8], constraints: &Constraints) -> Result<Vec<Segment>, EncodingError> {
    // Determine the ideal mode for each byte regardless of constraints.
    let free_modes = data.iter().map(|&byte| Mode::from(byte));
    // Apply the constraint to create the segments.
    match constraints.mode {
        ModeConstraint::AnyMixed => {
            Ok(free_modes
                .chunk_by(|mode| *mode)
                .into_iter()
                .map(|(mode, chunk)| Segment::new(mode, chunk.count()))
                .collect()
            )
        },
        ModeConstraint::AnySingle => {
            let most_generic_mode = free_modes
                .reduce(Mode::most_generic)
                .unwrap_or(Mode::Num);
            Ok(vec![
                Segment::new(most_generic_mode, data.len())
            ])
        },
        ModeConstraint::Only(constr) => {
            for &byte in data {
                let desired = Mode::from(byte);
                if !desired.could_be_promoted_to(constr) {
                    return Err(EncodingError::CannotEncodeWithMode(byte, constr))
                }
            }
            Ok(vec![
                Segment {
                    mode: constr,
                    len: data.len()
                }
            ])
        }
    }
}

/// Compress `segments` by applying some promotions to merge segments and minimize the overheads of a segment mode 
/// switch. The header size depends on the `version`, thus so does the compression.
fn compress(segments: &[Segment], version: Version) -> Vec<Segment> {
    use qrab_core::qrstandard::segment_encoding_len as encoding_len;

    // Only compress where applicable.
    if segments.len() <= 1 {
        return segments.to_owned();
    }

    let mut compressed = Vec::with_capacity(segments.len());
    let mut current = segments[0].clone();
    let mut current_enc_len = encoding_len(&current, version);
    for segment in &segments[1..] {
        let segment_enc_len = encoding_len(segment, version);
        let unmerged_enc_len = current_enc_len + segment_enc_len;
        let merged = current.merge(segment);
        let merged_enc_len = encoding_len(&merged, version);
        // Determine whether merging is convenient.
        if merged_enc_len <= unmerged_enc_len {
            current = merged;
            current_enc_len = merged_enc_len; 
        } else {
            compressed.push(current);
            current = segment.clone();
            current_enc_len = segment_enc_len;
        }
    }
    compressed
}

