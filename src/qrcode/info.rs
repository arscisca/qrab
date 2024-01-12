use itertools::Itertools;

use crate::encode::segment::{Segment, SegmentKind};

// QrInfo ==============================================================================================================
/// Collection of the determinant information of a QR code, grouping its version, error correction level and mask.
#[derive(Clone, PartialEq, Eq)]
pub struct QrInfo {
    version: Version,
    ecl: Ecl,
}

impl QrInfo {
    /// Initialize information with the specified `version` and `ecl`.
    pub fn new(version: Version, ecl: Ecl) -> Self {
        Self { version, ecl }
    }

    pub fn version(&self) -> Version {
        self.version
    }

    pub fn set_version(&mut self, version: Version) {
        self.version = version;
    }

    pub fn ecl(&self) -> Ecl {
        self.ecl
    }

    pub fn set_ecl(&mut self, ecl: Ecl) {
        self.ecl = ecl;
    }

    /// Get the number of data-only (no ECC) codewords for a given `version` and `ecl`.
    #[rustfmt::skip]
    pub fn num_data_codewords(&self) -> usize {
        let table: [usize; 40] = match self.ecl {
            Ecl::L => [
                19,   34,   55,   80,  108,  136,  156,  194,  232,  274,
                324,  370,  428,  461,  523,  589,  647,  721,  795,  861,
                932, 1006, 1094, 1174, 1276, 1370, 1468, 1531, 1631, 1735,
                1843, 1955, 2071, 2191, 2306, 2434, 2566, 2702, 2812, 2956,
            ],
            Ecl::M => [
                16,   28,   44,   64,   86,  108,  124,  154,  182,  216,
                254,  290,  334,  365,  415,  453,  507,  563,  627,  669,
                714,  782,  860,  914, 1000, 1062, 1128, 1193, 1267, 1373,
                1455, 1541, 1631, 1725, 1812, 1914, 1992, 2102, 2216, 2334,
            ],
            Ecl::Q => [
                13,   22,   34,   48,   62,   76,   88,  110,  132,  154,
                180,  206,  244,  261,  295,  325,  367,  397,  445,  485,
                512,  568,  614,  664,  718,  754,  808,  871,  911,  985,
                1033, 1115, 1171, 1231, 1286, 1354, 1426, 1502, 1582, 1666,
            ],
            Ecl::H => [
                9,   16,   26,   36,   46,   60,   66,   86,   100,  122,
                140,   158,  180,  197,  223,  253,  283,  313,  341,  385,
                406,   442,  464,  514,  538,  596,  628,  661,  701,  745,
                793,   845,  901,  961,  986, 1054, 1096, 1142, 1222, 1276,
            ],
        };
        table[self.version.number() as usize - 1]
    }

    /// Get the number of data-only (no ECC) bits given `version` and `ecl`.
    pub fn num_data_bits(&self) -> usize {
        self.num_data_codewords() * 8
    }

    /// Get the length in bits of the character count indicator for a segment of a given
    /// `SegmentKind`.
    pub(crate) fn char_count_len(&self, kind: SegmentKind) -> usize {
        let table = match kind {
            SegmentKind::Bytes => [8, 16, 16],
            SegmentKind::Alphanumeric => [9, 11, 13],
            SegmentKind::Numeric => [10, 12, 14],
        };
        let group = match self.version.number() {
            1..=9 => 0,
            10..=26 => 1,
            27..=40 => 2,
            v => unreachable!("unexpected version number: {}", v),
        };
        table[group]
    }

    /// Predict the total encoding length (in bits) of `segments`.
    pub(crate) fn predict_encoding_len<'s, I: Iterator<Item = &'s Segment<'s>>>(
        &self,
        segments: I,
    ) -> usize {
        let mut total = 0;
        for segment in segments {
            let d = segment.len();
            let header = 4 + self.char_count_len(segment.kind());
            let data = match segment.kind() {
                SegmentKind::Numeric => 10 * (d / 3) + [0, 4, 7][d % 3],
                SegmentKind::Alphanumeric => 11 * (d / 2) + 6 * (d % 2),
                SegmentKind::Bytes => 8 * d,
            };
            total += header + data;
        }
        total
    }

    /// Get the total number of codewords (including data and ECC) for a given `version`.
    #[rustfmt::skip]
    pub fn num_codewords(&self) -> usize {
        const TABLE: [usize; 40] = [
            26,   44,   70,  100,  134,  172,  196,  242,  292,  346,
            404,  466,  532,  581,  655,  733,  815,  901,  991, 1085,
            1156, 1258, 1364, 1474, 1588, 1706, 1828, 1921, 2051, 2185,
            2323, 2465, 2611, 2761, 2876, 3034, 3196, 3362, 3532, 3706,
        ];
        TABLE[self.version.number() as usize - 1]
    }

    /// Get the number of ECC encoding blocks given `version` and `ecl`.
    #[rustfmt::skip]
    pub fn num_ecc_blocks(&self) -> usize {
        let table: [usize; 40] = match self.ecl {
            Ecl::L => [
                1,  1,  1,  1,  1,  2,  2,  2,  2,  4,
                4,  4,  4,  4,  6,  6,  6,  6,  7,  8,
                8,  9,  9, 10, 12, 12, 12, 13, 14, 15,
                16, 17, 18, 19, 19, 20, 21, 22, 24, 25,
            ],
            Ecl::M => [
                1,  1,  1,  2,  2,  4,  4,  4,  5,  5,
                5,  8,  9,  9, 10, 10, 11, 13, 14, 16,
                17, 17, 18, 20, 21, 23, 25, 26, 28, 29,
                31, 33, 35, 37, 38, 40, 43, 45, 47, 49,
            ],
            Ecl::Q => [
                1,  1,  2,  2,  4,  4,  6,  6,  8,  8,
                8, 10, 12, 16, 12, 17, 16, 18, 21, 20,
                23, 23, 25, 27, 29, 34, 34, 35, 38, 40,
                43, 45, 48, 51, 53, 56, 59, 62, 65, 68,
            ],
            Ecl::H => [
                1,  1,  2,  4,  4,  4,  5,  6,  8,  8,
                11, 11, 16, 16, 18, 16, 19, 21, 25, 25,
                25, 34, 30, 32, 35, 37, 40, 42, 45, 48,
                51, 54, 57, 60, 63, 66, 70, 74, 77, 81,
            ],
        };
        table[self.version.number() as usize - 1]
    }

    #[rustfmt::skip]
    pub fn num_ecc_codewords_per_block(&self) -> usize {
        let table: [usize; 40] = match self.ecl {
            Ecl::L => [
                7, 10, 15, 20, 26, 18, 20, 24, 30, 18,
                20, 24, 26, 30, 22, 24, 28, 30, 28, 28,
                28, 28, 30, 30, 26, 28, 30, 30, 30, 30,
                30, 30, 30, 30, 30, 30, 30, 30, 30, 30,
            ],
            Ecl::M => [
                10, 16, 26, 18, 24, 16, 18, 22, 22, 26,
                30, 22, 22, 24, 24, 28, 28, 26, 26, 26,
                26, 28, 28, 28, 28, 28, 28, 28, 28, 28,
                28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
            ],
            Ecl::Q => [
                13, 22, 18, 26, 18, 24, 18, 22, 20, 24,
                28, 26, 24, 20, 30, 24, 28, 28, 26, 30,
                28, 30, 30, 30, 30, 28, 30, 30, 30, 30,
                30, 30, 30, 30, 30, 30, 30, 30, 30, 30,
            ],
            Ecl::H => [
                17, 28, 22, 16, 22, 28, 26, 26, 24, 28,
                24, 28, 22, 24, 24, 30, 28, 28, 26, 28,
                30, 24, 30, 30, 30, 30, 30, 30, 30, 30,
                30, 30, 30, 30, 30, 30, 30, 30, 30, 30,
            ],
        };
        table[self.version.number() as usize - 1]
    }

    /// Get the size of a QR code symbol given its `version`, as in the length of its side.
    pub const fn symbol_size(&self) -> usize {
        self.version.number() as usize * 4 + 17
    }

    /// Get a list of alignment pattern top-left corner coordinates for a given `version`.
    pub fn alignment_pattern_coordinates(&self) -> impl Iterator<Item = (usize, usize)> {
        // Use version number to handle ranges and calculations easier
        let pivots: &[usize] = match self.version {
            Version::V1 => &[],
            Version::V2 => &[6, 18],
            Version::V3 => &[6, 22],
            Version::V4 => &[6, 26],
            Version::V5 => &[6, 30],
            Version::V6 => &[6, 34],
            Version::V7 => &[6, 22, 38],
            Version::V8 => &[6, 24, 42],
            Version::V9 => &[6, 26, 46],
            Version::V10 => &[6, 28, 50],
            Version::V11 => &[6, 30, 54],
            Version::V12 => &[6, 32, 58],
            Version::V13 => &[6, 34, 62],
            Version::V14 => &[6, 26, 46, 66],
            Version::V15 => &[6, 26, 48, 70],
            Version::V16 => &[6, 26, 50, 74],
            Version::V17 => &[6, 30, 54, 78],
            Version::V18 => &[6, 30, 56, 82],
            Version::V19 => &[6, 30, 58, 86],
            Version::V20 => &[6, 34, 62, 90],
            Version::V21 => &[6, 28, 50, 72, 94],
            Version::V22 => &[6, 26, 50, 74, 98],
            Version::V23 => &[6, 30, 54, 78, 102],
            Version::V24 => &[6, 28, 54, 80, 106],
            Version::V25 => &[6, 32, 58, 84, 110],
            Version::V26 => &[6, 30, 58, 86, 114],
            Version::V27 => &[6, 34, 62, 90, 118],
            Version::V28 => &[6, 26, 50, 74, 98, 122],
            Version::V29 => &[6, 30, 54, 78, 102, 126],
            Version::V30 => &[6, 26, 52, 78, 104, 130],
            Version::V31 => &[6, 30, 56, 82, 108, 134],
            Version::V32 => &[6, 34, 60, 86, 112, 138],
            Version::V33 => &[6, 30, 58, 86, 114, 142],
            Version::V34 => &[6, 34, 62, 90, 118, 146],
            Version::V35 => &[6, 30, 54, 78, 102, 126, 150],
            Version::V36 => &[6, 24, 50, 76, 102, 128, 154],
            Version::V37 => &[6, 28, 54, 80, 106, 132, 158],
            Version::V38 => &[6, 32, 58, 84, 110, 136, 162],
            Version::V39 => &[6, 26, 54, 82, 110, 138, 166],
            Version::V40 => &[6, 30, 58, 86, 114, 142, 170],
        };
        // Actual alignment pattern coordinates are the cartesian product of the pivot list with itself
        let i1 = pivots.iter().cloned();
        let i2 = pivots.iter().cloned();
        let product = i1.cartesian_product(i2);
        // Filter to avoid alignment patterns that fall inside locator patterns
        let size = self.symbol_size();
        const LOCATOR_TOT_SIZE: usize = 8;
        const ALIGN_SIZE: usize = 5;
        product.filter_map(move |(i, j)| {
            // Get the top-left corner coordinates
            let (i, j) = (i - 2, j - 2);
            let no_top_left_collision = i >= LOCATOR_TOT_SIZE || j >= LOCATOR_TOT_SIZE;
            let no_top_right_collision =
                i >= LOCATOR_TOT_SIZE || j <= size - LOCATOR_TOT_SIZE - ALIGN_SIZE;
            let no_bot_left_collision =
                i <= size - LOCATOR_TOT_SIZE - ALIGN_SIZE || j >= LOCATOR_TOT_SIZE;
            if no_top_left_collision && no_top_right_collision && no_bot_left_collision {
                Some((i, j))
            } else {
                None
            }
        })
    }
}

impl std::fmt::Debug for QrInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}{:?}", self.version, self.ecl)
    }
}

// Version =============================================================================================================
/// Version of a QR code symbol. It determines various attributes such as its size and data capacity. Micro QR versions
/// are not supported.
#[rustfmt::skip]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
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

// Ecl =================================================================================================================
/// Error correction level.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
    pub fn code(&self) -> u8 {
        match self {
            Self::L => 0b01,
            Self::M => 0b00,
            Self::Q => 0b11,
            Self::H => 0b10,
        }
    }

    /// Get the next higher error correction level, saturating at `Ecl::H`.
    pub(crate) fn next(&self) -> Self {
        match self {
            Self::L => Self::M,
            Self::M => Self::Q,
            Self::Q => Self::H,
            Self::H => Self::H,
        }
    }
}

impl From<&Ecl> for char {
    fn from(value: &Ecl) -> Self {
        match value {
            Ecl::L => 'L',
            Ecl::M => 'M',
            Ecl::Q => 'Q',
            Ecl::H => 'H',
        }
    }
}

impl std::fmt::Debug for Ecl {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let c: char = self.into();
        write!(f, "{}", c)
    }
}

impl std::fmt::Display for Ecl {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let c: char = self.into();
        write!(f, "{}", c)
    }
}

// Mask ================================================================================================================
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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
    pub fn code(&self) -> u8 {
        *self as u8
    }

    pub fn function(&self) -> impl Fn(usize, usize) -> bool {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn version_comparison() {
        assert_eq!(Version::V5, Version::V5);
        assert!(Version::V3 < Version::V8);
        assert!(Version::V29 >= Version::V17);
    }
}
