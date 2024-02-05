mod builder;
pub mod encoder;
pub(crate) mod segment;

use crate::*;
pub use encoder::Encoder;

// Constraints =========================================================================================================
/// Trait for types that have an order and a well defined global minimum and maximum.
pub trait Bounded: Ord {
    const MIN: Self;
    const MAX: Self;
}

impl Bounded for Version {
    const MIN: Self = Version::V1;
    const MAX: Self = Version::V40;
}

impl Bounded for Ecl {
    const MIN: Self = Ecl::L;
    const MAX: Self = Ecl::H;
}

/// Trait for types that are ordered and have a well defined sequence, where (almost) each value has a `next` and a
/// `prev` value. It is assumed that only a type's potential extremes have no successor or predecessor.
pub trait Sequential: Sized + Ord + Copy + Clone {
    fn next(&self) -> Option<Self>;
    fn prev(&self) -> Option<Self>;
}

impl Sequential for Version {
    fn next(&self) -> Option<Self> {
        Self::try_from(self.number() + 1).ok()
    }

    fn prev(&self) -> Option<Self> {
        let prev_number = self.number().checked_sub(1)?;
        Self::try_from(prev_number).ok()
    }
}

impl Sequential for Ecl {
    fn next(&self) -> Option<Self> {
        match self {
            Self::L => Some(Self::M),
            Self::M => Some(Self::Q),
            Self::Q => Some(Self::H),
            Self::H => None,
        }
    }

    fn prev(&self) -> Option<Self> {
        match self {
            Self::L => None,
            Self::M => Some(Self::L),
            Self::Q => Some(Self::M),
            Self::H => Some(Self::Q),
        }
    }
}

/// Constraint over the possible values of `T`. The constraint `Constraint::any()` accepts any value of `T`.
/// # Constructing from ranges
/// Aside from the available like `Constraint::min()` and `Constraint::max()`, it is also possible to build constraints
/// from ranges. For example, these two declarations are equivalent:
/// ```rust
/// use qrab::{encode::Constraint, Version};
/// let c1 = Constraint::min(Version::V3);
/// let c2 = Constraint::from(Version::V3..);
/// assert_eq!(c1, c2);
/// ```
/// # Examples
/// ```rust
/// use qrab::{encode::Constraint, Version};
/// // Direct usage
/// assert!(Constraint::min(Version::V5).accepts(&Version::V6));
/// assert!(Constraint::between(Version::V10, Version::V20).accepts(&Version::V15));
/// // Handy way to construct constraints
/// assert!(Constraint::from(Version::V10..=Version::V20).accepts(&Version::V15));
/// assert!(Constraint::from(Version::V6..).accepts(&Version::V8));
/// assert!(Constraint::from(Version::V18).accepts(&Version::V18));
/// // Free constraint
/// assert!(Constraint::any().accepts(&Version::V40));
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint<T>
where
    T: Bounded + Sequential + std::fmt::Debug,
{
    range: std::ops::RangeInclusive<T>,
}

impl<T> Constraint<T>
where
    T: Bounded + Sequential + std::fmt::Debug,
{
    /// Construct a constraint that accepts any value of `T`.
    /// # Examples
    /// ```rust
    /// use qrab::{encode::Constraint, Version};
    /// let c = Constraint::any();
    /// assert!(c.accepts(&Version::V1));
    /// assert!(c.accepts(&Version::V40));
    /// ```
    pub fn any() -> Self {
        Self::from_range(T::MIN..=T::MAX)
    }

    /// Construct a constraint that accepts only values equal to `val`.
    /// ```rust
    /// use qrab::{encode::Constraint, Version};
    /// let c = Constraint::exactly(Version::V10);
    /// assert!(c.accepts(&Version::V10));
    /// assert!(!c.accepts(&Version::V11));
    /// ```
    pub fn exactly(val: T) -> Self {
        Self::from_range(val..=val)
    }

    /// Construct a constraint that accepts values that are at least `min` (included).
    /// # Examples
    /// ```rust
    /// use qrab::{encode::Constraint, Version};
    /// let c = Constraint::min(Version::V10);
    /// assert!(!c.accepts(&Version::V9));
    /// assert!(c.accepts(&Version::V10));
    /// ```
    pub fn min(min: T) -> Self {
        Self::from_range(min..)
    }

    /// Construct a constraint that accepts value that are at most `max` (included).
    /// # Examples
    /// ```rust
    /// use qrab::{encode::Constraint, Version};
    /// let c = Constraint::max(Version::V10);
    /// assert!(c.accepts(&Version::V10));
    /// assert!(!c.accepts(&Version::V11));
    /// ```
    pub fn max(max: T) -> Self {
        Self::from_range(..=max)
    }

    /// Construct a constraint that accepts value that are between `min` and `max`, both extremes included.
    /// # Examples
    /// ```rust
    /// use qrab::{encode::Constraint, Version};
    /// let c = Constraint::between(Version::V10, Version::V20);
    /// assert!(c.accepts(&Version::V10));
    /// assert!(!c.accepts(&Version::V30));
    /// ```
    pub fn between(min: T, max: T) -> Self {
        Self::from_range(min..=max)
    }

    /// Construct a constraint based on a `range`. Since the generic `T` is required to be `Sequential`, it doesn't
    /// matter whether either of the extremes of the range is included or not: whenever an extreme is not included, the
    /// constraint will use its previous (for the minimum) or following (for the maximum) value.
    fn from_range<R: std::ops::RangeBounds<T>>(range: R) -> Self {
        let min = match range.start_bound() {
            std::ops::Bound::Included(b) => b.clone(),
            std::ops::Bound::Excluded(b) => b.next().unwrap_or(T::MAX),
            std::ops::Bound::Unbounded => T::MIN,
        };
        let max = match range.end_bound() {
            std::ops::Bound::Included(b) => b.clone(),
            std::ops::Bound::Excluded(b) => b.prev().unwrap_or(T::MIN),
            std::ops::Bound::Unbounded => T::MAX,
        };
        Self { range: min..=max }
    }

    /// Check whether the constraints accepts `value`.
    /// # Example
    /// ```rust
    /// use qrab::{Version, encode::Constraint};
    /// let c = Constraint::from(Version::V4..Version::V12);
    /// assert!(c.accepts(&Version::V6));
    /// assert!(!c.accepts(&Version::V30));
    /// ```
    pub fn accepts(&self, value: &T) -> bool {
        self.range.contains(value)
    }

    /// Get a `(min, max)` tuple representing the inclusive extremes allowed by this constraint.
    /// # Examples
    /// ```rust
    /// use qrab::{encode::Constraint, Version};
    /// assert_eq!(Constraint::from(Version::V10..=Version::V20).extremes(), (&Version::V10, &Version::V20));
    /// assert_eq!(Constraint::min(Version::V4).extremes(), (&Version::V4, &Version::V40));
    /// // Note: extremes are inclusive
    /// assert_eq!(Constraint::exactly(Version::V8).extremes(), (&Version::V8, &Version::V8));
    /// ```
    pub fn extremes(&self) -> (&T, &T) {
        (self.range.start(), self.range.end())
    }
}

impl<T> From<T> for Constraint<T>
where
    T: Bounded + Sequential + std::fmt::Debug,
{
    fn from(value: T) -> Self {
        Self::from_range(value..=value)
    }
}

macro_rules! impl_from_range {
    ($generic: ident, $rtype: ty) => {
        impl<$generic> From<$rtype> for Constraint<$generic>
        where
            $generic: Bounded + Sequential + std::fmt::Debug,
        {
            fn from(value: $rtype) -> Self {
                Self::from_range(value)
            }
        }
    };
}

impl_from_range!(T, std::ops::Range<T>);
impl_from_range!(T, std::ops::RangeInclusive<T>);
impl_from_range!(T, std::ops::RangeFrom<T>);
impl_from_range!(T, std::ops::RangeTo<T>);
impl_from_range!(T, std::ops::RangeToInclusive<T>);
impl_from_range!(T, std::ops::RangeFull);

impl<T> std::fmt::Display for Constraint<T>
where
    T: Bounded + Sequential + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

/// Collection of user-defined constraints to enforce in the encoding process. The constraints are arbitrary and there
/// is no encoder output guarantee: constraints that are too strict will cause an encoder to be unable to encode certain
/// inputs.
/// # Examples
/// ```rust
/// use qrab::{encode::Constraint, EncodingConstraints, Version, Ecl, Mask};
/// // Define some custom, granular constraints. For convenience, constraints are compatible with inclusive ranges:
/// let constraints = EncodingConstraints::new()
///     .with_version(Version::V3..=Version::V6)    // Using a range
///     .with_ecl(Constraint::min(Ecl::Q))          // Using an explicit constraint
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
    masks: [bool; Mask::NMASKS],
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
        Self {
            version: Constraint::any(),
            ecl: Constraint::any(),
            masks: [true; 8],
        }
    }

    /// Constrain the QR version. For convenience, this method is generic over all the types compatible with a
    /// `Constraint<Version>`.
    /// # Examples
    /// ```rust
    /// use qrab::{encode::Constraint, EncodingConstraints, Version};
    /// // Using `Constraint`s directly
    /// let direct = EncodingConstraints::new().with_version(Constraint::min(Version::V3));
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
        for mask in values {
            self.masks[mask.code() as usize] = true;
        }
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

    /// Get the current constraint on QR masks, where the output is an array where the `i`th entry is true if the `i`th
    /// mask is accepted.
    pub fn masks(&self) -> &[bool; Mask::NMASKS] {
        &self.masks
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
    #[error("there are no available masks")]
    NoAvailableMasks,
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
