pub(crate) mod builder;
pub(crate) mod encoder;
pub(crate) mod segment;

use std::collections::HashSet;

use crate::*;
pub use encoder::Encoder;

// Constraints =========================================================================================================
/// Constraint over the possible values of `T`. The `Constraint::Any` variant is the free constraint
/// that accepts any value of `T`.
/// # Type conversions
/// For convenience, `Constraint`s can be defined explicitly through the
/// available variants, or from various compatible types that include values of `T` (mapped to a `Constraint::Exact(T)`)
/// and inclusive ranges.
/// # Examples
/// ```rust
/// use qrab::Constraint;
/// // Direct usage
/// assert!(Constraint::Min(5).accepts(&6));
/// assert!(Constraint::Between{min: 10, max: 20}.accepts(&15));
/// // Handy way to construct constraints
/// assert!(Constraint::from(10..=20).accepts(&15));
/// assert!(Constraint::from(6..).accepts(&8));
/// assert!(Constraint::from(42).accepts(&42));
/// // Free constraint
/// assert!(Constraint::Any.accepts(&999));
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint<T>
where
    T: Ord + std::fmt::Debug,
{
    /// No constraint.
    Any,
    /// Exact value.
    Exact(T),
    /// Minimum, inclusive.
    Min(T),
    /// Maximum, inclusive.
    Max(T),
    /// Range, inclusive at both ends.
    Between { min: T, max: T },
}

impl<T: Ord + std::fmt::Debug> Constraint<T> {
    /// Check whether the constraints accepts `value`.
    /// # Example
    /// ```rust
    /// use qrab::Constraint;
    /// let c = Constraint::Between {min: 56, max: 78};
    /// assert!(c.accepts(&60));
    /// assert!(!c.accepts(&100));
    /// ```
    pub fn accepts(&self, value: &T) -> bool {
        match self {
            Self::Any => true,
            Self::Exact(e) => e.eq(value),
            Self::Min(min) => min.le(value),
            Self::Max(max) => max.ge(value),
            Self::Between { min, max } => (min..=max).contains(&value),
        }
    }

    /// Get the inclusive minimum allowed value according to the constraint.
    /// # Examples
    /// ```rust
    /// use qrab::Constraint;
    /// assert_eq!(Constraint::Min(12).min(), Some(&12));
    /// assert_eq!(Constraint::Between{min: 3, max: 8}.min(), Some(&3));
    /// assert_eq!(Constraint::Max(100).min(), None);
    /// // Note: minimum is inclusive
    /// assert_eq!(Constraint::Exact(42).min(), Some(&42));
    /// ```
    pub fn min(&self) -> Option<&T> {
        match self {
            Self::Exact(min) | Self::Min(min) | Self::Between { min, max: _ } => Some(min),
            _ => None,
        }
    }

    /// Get the inclusive maximum allowed value according to the constraint.
    /// # Examples
    /// ```rust
    /// use qrab::Constraint;
    /// assert_eq!(Constraint::Max(8).max(), Some(&8));
    /// assert_eq!(Constraint::Between{min: 5, max: 9}.max(), Some(&9));
    /// assert_eq!(Constraint::Min(10).max(), None);
    /// // Note: maximum is inclusive
    /// assert_eq!(Constraint::Exact(42).max(), Some(&42));
    ///
    pub fn max(&self) -> Option<&T> {
        match self {
            Self::Exact(max) | Self::Max(max) | Self::Between { min: _, max } => Some(max),
            _ => None,
        }
    }

    /// Get a `(min, max)` tuple representing the optional inclusive extremes of the range allowed by this constraint.
    /// # Examples
    /// ```rust
    /// assert_eq!(qrab::Constraint::Between{min: 10, max: 20}.extremes(), (Some(&10), Some(&20)));
    /// assert_eq!(qrab::Constraint::Min(4).extremes(), (Some(&4), None));
    /// // Note: extremes are inclusive
    /// assert_eq!(qrab::Constraint::Exact(8).extremes(), (Some(&8), Some(&8)));
    /// ```
    pub fn extremes(&self) -> (Option<&T>, Option<&T>) {
        (self.min(), self.max())
    }
}

impl<T: Copy + Clone + Ord + std::fmt::Debug> Constraint<T> {
    /// Get a `(min, max)` tuple representing the inclusive extremes of the range allowed by the constraint. If there is
    /// no constraint over an extreme, `default_min` or `default_max` will be returned instead.
    /// # Examples
    /// ```rust
    /// use qrab::Constraint;
    /// assert_eq!(Constraint::Between {min: 3, max: 5}.extremes_or_defaults(0, 10), (3, 5));
    /// assert_eq!(Constraint::Min(3).extremes_or_defaults(0, 10), (3, 10));
    /// assert_eq!(Constraint::Max(3).extremes_or_defaults(0, 10), (0, 3));
    /// assert_eq!(Constraint::Exact(4).extremes_or_defaults(0, 10), (4, 4));
    /// ```
    pub fn extremes_or_defaults(&self, default_min: T, default_max: T) -> (T, T) {
        let (min, max) = self.extremes();
        (
            min.cloned().unwrap_or(default_min),
            max.cloned().unwrap_or(default_max),
        )
    }
}

impl<T: Ord + std::fmt::Debug> From<T> for Constraint<T> {
    fn from(value: T) -> Self {
        Self::Exact(value)
    }
}

impl<T: Ord + std::fmt::Debug> From<std::ops::RangeInclusive<T>> for Constraint<T> {
    fn from(value: std::ops::RangeInclusive<T>) -> Self {
        let (min, max) = value.into_inner();
        Self::Between { min, max }
    }
}

impl<T: Ord + std::fmt::Debug> From<std::ops::RangeFrom<T>> for Constraint<T> {
    fn from(value: std::ops::RangeFrom<T>) -> Self {
        Self::Min(value.start)
    }
}

impl<T: Ord + std::fmt::Debug> From<std::ops::RangeToInclusive<T>> for Constraint<T> {
    fn from(value: std::ops::RangeToInclusive<T>) -> Self {
        Self::Max(value.end)
    }
}

impl<T: Ord + std::fmt::Debug> std::fmt::Display for Constraint<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

/// Collection of user-defined constraints to enforce in the encoding process. The constraints are arbitrary and there
/// is no encoder output guarantee: constraints that are too strict will cause an encoder to be unable to encode certain
/// inputs.
/// # Examples
/// ```rust
/// use qrab::{Constraint, EncodingConstraints, Version, Ecl, Mask};
/// // Define some custom, granular constraints. For convenience, constraints are compatible with inclusive ranges:
/// let constraints = EncodingConstraints::new()
///     .with_version(Version::V3..=Version::V6)    // Using a range
///     .with_ecl(Constraint::Min(Ecl::Q))          // Using an explicit constraint
///     .with_mask_in([Mask::M001, Mask::M100]);
/// assert!(constraints.version().accepts(&Version::V4));
/// assert!(constraints.ecl().accepts(&Ecl::H));
/// // It is also possible to define an empty `EncodingConstraints`:
/// let free = EncodingConstraints::none();
/// assert!(free.version().accepts(&Version::V30));
/// assert!(free.ecl().accepts(&Ecl::L));
/// ```
pub struct EncodingConstraints {
    version: Constraint<Version>,
    ecl: Constraint<Ecl>,
    masks: HashSet<Mask>,
}

impl EncodingConstraints {
    /// Construct with no constraints, alias of `EncodingConstraints::none()` for a better aesthetic when actually
    /// setting up constraints.
    pub fn new() -> Self {
        Self::none()
    }

    /// Construct with no constraints.
    /// # Example
    /// ```rust
    /// use qrab::{EncodingConstraints, Version};
    /// let constraints = EncodingConstraints::none();
    /// assert!(constraints.version().accepts(&Version::V1));
    /// assert!(constraints.version().accepts(&Version::V40));
    /// ```
    pub fn none() -> Self {
        let masks = HashSet::from([
            Mask::M000,
            Mask::M001,
            Mask::M010,
            Mask::M011,
            Mask::M100,
            Mask::M101,
            Mask::M110,
            Mask::M111,
        ]);
        Self {
            version: Constraint::Any,
            ecl: Constraint::Any,
            masks,
        }
    }

    /// Constrain the QR version. For convenience, this method is generic over all the types compatible with a
    /// `Constraint<Version>`.
    /// # Examples
    /// ```rust
    /// use qrab::{Constraint, EncodingConstraints, Version};
    /// // Using `Constraint`s directly
    /// let direct = EncodingConstraints::new().with_version(Constraint::Min(Version::V3));
    /// // Using handy conversions
    /// let from_range = EncodingConstraints::new().with_version(Version::V3..=Version::V5);
    /// let exact = EncodingConstraints::new().with_version(Version::V7);
    /// ```
    pub fn with_version<T: Into<Constraint<Version>>>(mut self, version: T) -> Self {
        self.version = version.into();
        self
    }

    /// Constrain the error correction level. For convenience, this method is generic over all the types compatible with
    /// a `Constraint<Ecl>`.
    pub fn with_ecl<T: Into<Constraint<Ecl>>>(mut self, ecl: T) -> Self {
        self.ecl = ecl.into();
        self
    }

    /// Constrain the mask.
    pub fn with_mask_in<I: IntoIterator<Item = Mask>>(mut self, values: I) -> Self {
        self.masks = values.into_iter().collect();
        self
    }

    /// Get the current constraint on the version.
    pub fn version(&self) -> &Constraint<Version> {
        &self.version
    }

    /// Get the current constraint on the error correction level.
    pub fn ecl(&self) -> &Constraint<Ecl> {
        &self.ecl
    }

    /// Get the current constraint on QR masks.
    pub fn masks(&self) -> impl Iterator<Item = &Mask> {
        self.masks.iter()
    }
}

impl Default for EncodingConstraints {
    fn default() -> Self {
        Self::none()
    }
}

// Errors ==============================================================================================================
/// Encoding error.
#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error(
        "the best possible encoding under the version {ver_constr} and error correction level {ecl_constr} constraints \
        cannot fit the input data."
    )]
    DataTooLong {
        ver_constr: Constraint<Version>,
        ecl_constr: Constraint<Ecl>,
    },
    #[error("building error")]
    BuildingError(#[from] builder::BuildingError),
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn constraints_from_range() {
        // Inclusive range
        let c = Constraint::from(Version::V4..=Version::V5);
        assert!(!c.accepts(&Version::V3));
        assert!(c.accepts(&Version::V4));
        assert!(c.accepts(&Version::V5));
        assert!(!c.accepts(&Version::V6));
        // Open ended ranges
        let c = Constraint::from(Ecl::M..);
        assert!(!c.accepts(&Ecl::L));
        assert!(c.accepts(&Ecl::M));
        assert!(c.accepts(&Ecl::Q));
        assert!(c.accepts(&Ecl::H));
        let c = Constraint::from(..=Ecl::L);
        assert!(c.accepts(&Ecl::L));
        assert!(!c.accepts(&Ecl::M));
    }
}
