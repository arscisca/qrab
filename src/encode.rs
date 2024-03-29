mod builder;
pub mod encoder;

use std::ops::{Bound, RangeBounds, RangeInclusive};

use crate::{
    info::Segment,
    {Ecl, Mask, MaskTable, Mode, QrCode, QrInfo, Version},
};
pub use encoder::Encoder;

// Constraints =========================================================================================================
/// Collection of user-defined constraints to enforce in the encoding process. Constraints are not checked while being
/// defined, so there may be instances where an encoder won't be able to encode some input data because the constraints
/// are too restrictive.
/// # Examples
/// ```rust
/// use qrab::{EncodingConstraints, Version, Ecl};
/// // Constrain to version range `Version::V10..Version::V15` and to `Ecl::L` only.
/// let constraints = EncodingConstraints::new()
///     .with_version_in(Version::V10..Version::V15)
///     .with_ecl(Ecl::L);
/// assert!(constraints.version().contains(&Version::V12));
/// assert!(!constraints.ecl().contains(&Ecl::M));
/// ```
pub struct EncodingConstraints {
    version: RangeInclusive<Version>,
    ecl: RangeInclusive<Ecl>,
    mode: ModeConstraint,
    masks: MaskTable<bool>,
}

impl EncodingConstraints {
    /// Construct with no constraints like `EncodingConstraints::none()`.
    /// # Examples
    /// ```rust
    /// use qrab::{EncodingConstraints, Version};
    /// let constraints = EncodingConstraints::none();
    /// assert!(constraints.version().contains(&Version::V1));
    /// assert!(constraints.version().contains(&Version::V40));
    /// ```
    pub fn new() -> Self {
        Self::none()
    }

    /// Construct with no constraints.
    /// # Examples
    /// ```rust
    /// use qrab::{EncodingConstraints, Version};
    /// let constraints = EncodingConstraints::none();
    /// assert!(constraints.version().contains(&Version::V1));
    /// assert!(constraints.version().contains(&Version::V40));
    /// ```
    pub fn none() -> Self {
        Self {
            version: Version::V1..=Version::V40,
            ecl: Ecl::L..=Ecl::H,
            mode: ModeConstraint::AnyMixed,
            masks: MaskTable::filled(true),
        }
    }

    /// Constrain version to the specified `range`.
    /// # Examples
    /// ```rust
    /// use qrab::{EncodingConstraints, Version};
    /// let constraints = EncodingConstraints::new().with_version_in(Version::V10..=Version::V20);
    /// assert!(constraints.version().contains(&Version::V15));
    /// assert!(!constraints.version().contains(&Version::V30));
    /// ```
    pub fn with_version_in<T: RangeBounds<Version>>(mut self, range: T) -> Self {
        let vmin = match range.start_bound() {
            Bound::Included(vmin) => *vmin,
            Bound::Excluded(vmin) => vmin.higher().unwrap_or(Version::V40),
            Bound::Unbounded => Version::V1,
        };
        let vmax = match range.end_bound() {
            Bound::Included(vmax) => *vmax,
            Bound::Excluded(vmax) => vmax.lower().unwrap_or(Version::V1),
            Bound::Unbounded => Version::V40,
        };
        self.version = vmin..=vmax;
        self
    }

    /// Constrain to a specific `version`.
    /// # Examples
    /// ```rust
    /// use qrab::{EncodingConstraints, Version};
    /// let constraints = EncodingConstraints::new().with_version(Version::V20);
    /// assert!(constraints.version().contains(&Version::V20));
    /// assert!(!constraints.version().contains(&Version::V21));
    /// ```
    pub fn with_version(self, version: Version) -> Self {
        self.with_version_in(version..=version)
    }

    /// Constrain error correction level to the specified `range`.
    /// # Examples
    /// ```rust
    /// use qrab::{EncodingConstraints, Ecl};
    /// let constraints = EncodingConstraints::new().with_ecl_in(Ecl::M..Ecl::H);
    /// assert!(constraints.ecl().contains(&Ecl::M));
    /// assert!(!constraints.ecl().contains(&Ecl::L));
    /// ```
    pub fn with_ecl_in<T: RangeBounds<Ecl>>(mut self, range: T) -> Self {
        let emin = match range.start_bound() {
            Bound::Included(emin) => *emin,
            Bound::Excluded(emin) => emin.higher().unwrap_or(Ecl::H),
            Bound::Unbounded => Ecl::L,
        };
        let emax = match range.end_bound() {
            Bound::Included(emax) => *emax,
            Bound::Excluded(emax) => emax.lower().unwrap_or(Ecl::L),
            Bound::Unbounded => Ecl::H,
        };
        self.ecl = emin..=emax;
        self
    }

    /// Constrain error correction level to the specified `ecl` only.
    /// # Examples
    /// ```rust
    /// use qrab::{EncodingConstraints, Ecl};
    /// let constraints = EncodingConstraints::new().with_ecl(Ecl::Q);
    /// assert!(constraints.ecl().contains(&Ecl::Q));
    /// assert!(!constraints.ecl().contains(&Ecl::M));
    /// ```
    pub fn with_ecl(self, ecl: Ecl) -> Self {
        self.with_ecl_in(ecl..=ecl)
    }

    /// Constrain data encoding according to the specified `mode`. This may result in encoding failures.
    pub fn with_mode(mut self, mode: ModeConstraint) -> Self {
        self.mode = mode;
        self
    }

    /// Constrain mask to the selected `values`.
    /// # Examples
    /// ```rust
    /// use qrab::{EncodingConstraints, Mask};
    /// let constraints = EncodingConstraints::new().with_mask_in([Mask::M010, Mask::M111]);
    /// assert_eq!(constraints.masks(), &[false, false, true, false, false, false, false, true].into());
    /// ```
    pub fn with_mask_in<I: IntoIterator<Item = Mask>>(mut self, values: I) -> Self {
        self.masks.fill(false);
        for mask in values {
            self.masks[mask] = true;
        }
        self
    }

    /// Get the current constraint on the version.
    pub fn version(&self) -> &RangeInclusive<Version> {
        &self.version
    }

    /// Get the current constraint on the error correction level.
    pub fn ecl(&self) -> &RangeInclusive<Ecl> {
        &self.ecl
    }

    /// Get the current constraint on the mixing modes.
    pub fn mode(&self) -> ModeConstraint {
        self.mode
    }

    /// Get the current constraint on QR masks, where the output is an array where the `i`th entry is true if the `i`th
    /// mask is accepted.
    pub fn masks(&self) -> &MaskTable<bool> {
        &self.masks
    }
}

impl Default for EncodingConstraints {
    fn default() -> Self {
        Self::none()
    }
}

/// Constraint on which segment modes can be used to encode data.
#[derive(Copy, Clone, Debug)]
pub enum ModeConstraint {
    /// Allow any mode, and allow mixing multiple modes in the same symbol. Doesn't fail.
    AnyMixed,
    /// Allow any mode, but only allow one for the entire symbol. Doesn't fail.
    AnySingle,
    /// Try to enforce the specified `Mode`, but allow promotions to more general modes when the requested `Mode` is not
    /// possible. Allow mixing multiple modes in the same symbol.
    TryOrPromoteMixed(Mode),
    /// Try to enforce the specified `Mode`, but allow promotions to more general modes when the requested one is not
    /// possible. Do not allow multiple modes in the same symbol: if any data segment cannot be encoded with `Mode`, the
    /// whole symbol will be encoded using the most general mode possible. Doesn't fail.
    TryOrPromoteSingle(Mode),
    /// Try to enforce the specified `Mode`, and fail when data cannot be encoded that way.
    TryOrFail(Mode),
}

impl ModeConstraint {
    /// Specify whether the constraint allows for the usage of multiple modes in the same QR symbol.
    pub fn allows_mixed_modes(&self) -> bool {
        matches!(self, Self::AnyMixed | Self::TryOrPromoteMixed(_))
    }
}

// Errors ==============================================================================================================
/// Encoding error.
#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    /// Data is too long for the encoder constraints.
    #[error(
    "the best possible encoding under the version {ver_constr:?} and error correction level {ecl_constr:?} constraints \
        cannot fit the input data."
    )]
    DataTooLong {
        ver_constr: RangeInclusive<Version>,
        ecl_constr: RangeInclusive<Ecl>,
    },
    /// The mask constraints did not allow for any mask.
    #[error("there are no available masks")]
    NoAvailableMasks,
    #[error("cannot use mode '{0:?}' to encode data")]
    CannotEncodeWithMode(Mode),
}
