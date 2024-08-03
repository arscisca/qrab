use std::ops::{Bound, RangeBounds, RangeInclusive};

use qrab_core::{Canvas, Ecl, Mask, MaskTable, QrCode, Version};

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
        // TODO: Implement actual encoding.
        let size = qrab_core::qrstandard::canvas_size(Version::V01);
        let mut canvas = Canvas::filled(size, qrab_core::Module::Light);
        for i in 0..size {
            for j in 0..size {
                let module = qrab_core::Module::from(data[(i * size + j) % data.len()] >= b'j');
                canvas.set(i, j, module);
            }
        }
        let meta = qrab_core::Meta {
            version: Version::V01,
            ecl: Ecl::L,
            mask: Mask::M000,
        };
        Ok(QrCode::new(canvas, meta).unwrap())
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
}

impl Default for Encoder {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct Constraints {
    ecl: RangeInclusive<Ecl>,
    version: RangeInclusive<Version>,
    mask: MaskTable<bool>,
}

impl Default for Constraints {
    fn default() -> Self {
        let mut mask = MaskTable::default();
        mask.fill(true);
        Self {
            ecl: Ecl::L..=Ecl::H,
            version: Version::V01..=Version::V40,
            mask,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum EncodingError {}
