use itertools::Itertools;

use crate::{Ecl, Version, encode::segment::SegmentKind};

/// Get the number of bits of the segment character count indicator based on the `segment_kind` and `version`.
pub fn char_count_len(version: Version, segment_kind: SegmentKind) -> usize {
    let table = match segment_kind {
        SegmentKind::Bytes => [8, 16, 16],
        SegmentKind::Alphanumeric => [9, 11, 13],
        SegmentKind::Numeric => [10, 12, 14]
    };
    let group = match version.number() {
        1..=9 => 0,
        10..=26 => 1,
        27..=40 => 2,
        v => unreachable!("unexpected version number: {}", v),
    };
    table[group]
}

/// Get the total number of codewords (including data and ECC) for a given `version`.
#[rustfmt::skip]
pub fn num_codewords(version: Version) -> usize {
    const TABLE: [usize; 40] = [
          26,   44,   70,  100,  134,  172,  196,  242,  292,  346,
         404,  466,  532,  581,  655,  733,  815,  901,  991, 1085,
        1156, 1258, 1364, 1474, 1588, 1706, 1828, 1921, 2051, 2185,
        2323, 2465, 2611, 2761, 2876, 3034, 3196, 3362, 3532, 3706,
    ];
    TABLE[version.number() as usize - 1]
}

/// Get the number of data-only (no ECC) codewords for a given `version` and `ecl`.
#[rustfmt::skip]
pub fn num_data_codewords(version: Version, ecl: Ecl) -> usize {
    let table: [usize; 40] = match ecl {
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
    table[version.number() as usize - 1]
}

/// Get the number of data-only (no ECC) bits given `version` and `ecl`.
pub fn num_data_bits(version: Version, ecl: Ecl) -> usize {
    num_data_codewords(version, ecl) * 8
}

/// Get the number of ECC encoding blocks given `version` and `ecl`.
#[rustfmt::skip]
pub fn num_ecc_blocks(version: Version, ecl: Ecl) -> usize {
    let table: [usize; 40] = match ecl {
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
    table[version.number() as usize - 1]
}

#[rustfmt::skip]
pub fn num_ecc_codewords_per_block(version: Version, ecl: Ecl) -> usize {
    let table: [usize; 40] = match ecl {
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
    table[version.number() as usize - 1]
}

/// Get the size of a QR code symbol given its `version`, as in the length of its side.
pub fn symbol_size(version: Version) -> usize {
    version.number() as usize * 4 + 17
}

/// Get a list of alignment pattern top-left corner coordinates for a given `version`.
pub fn alignment_pattern_coordinates(version: Version) -> impl Iterator<Item=(usize, usize)> {
    // Use version number to handle ranges and calculations easier
    let pivots: &[usize] = match version {
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
    let size = symbol_size(version);
    const LOCATOR_TOT_SIZE: usize = 8;
    const ALIGN_SIZE: usize = 5;
    product.filter_map(move |(i, j)| {
        // Get the top-left corner coordinates
        let (i, j) = (i - 2, j - 2);
        let no_top_left_collision = i >= LOCATOR_TOT_SIZE || j >= LOCATOR_TOT_SIZE;
        let no_top_right_collision = i >= LOCATOR_TOT_SIZE || j <= size - LOCATOR_TOT_SIZE - ALIGN_SIZE;
        let no_bot_left_collision = i <= size - LOCATOR_TOT_SIZE - ALIGN_SIZE || j >= LOCATOR_TOT_SIZE;
        if no_top_left_collision && no_top_right_collision && no_bot_left_collision {
            Some((i, j))
        } else {
            None
        }
    })
}
