use crate::{Ecl, Version};
use crate::{Mode, Segment};

/// Determine the QR code's canvas size in pixels for the given `version`.
pub fn canvas_size(version: Version) -> usize {
    17 + version.number() as usize * 4
}

/// Determine which of the three groups relevant for determining the character count length
/// `version` belongs to.
pub fn char_count_version_group(version: Version) -> usize {
    match version.number() {
        1..=9 => 0,
        10..=26 => 1,
        27..=40 => 2,
        invalid => unreachable!("invalid version '{}'", invalid),
    }
}

/// Get the number of bits of the character count of `mode` for the given `version`.
pub fn char_count_len(mode: Mode, version: Version) -> usize {
    let base = match mode {
        Mode::Bytes => 8,
        Mode::Alnum => 9,
        Mode::Num => 10,
    };
    let factor = match (mode, char_count_version_group(version)) {
        (Mode::Bytes, 0) => 0,
        (Mode::Bytes, _) => 4,
        (Mode::Alnum, group) => group,
        (Mode::Num, group) => group,
    };
    base + 2 * factor
}

/// Determine the length in bits of the encoding of a given `segment`.
pub fn segment_encoding_len(segment: &Segment, version: Version) -> usize {
    const MODE_IDENTIFIER_LEN: usize = 4;
    let d = segment.len;
    let header = MODE_IDENTIFIER_LEN + char_count_len(segment.mode, version);
    let data = match segment.mode {
        Mode::Num => 10 * (d / 3) + [0, 4, 7][d % 3],
        Mode::Alnum => 11 * (d / 2) + 6 * (d % 2),
        Mode::Bytes => 8 * d,
    };
    header + data
}

/// Determine the number of bits available for pure data (without error correction).
pub fn num_data_bits(version: Version, ecl: Ecl) -> usize {
    num_data_codewords(version, ecl) * 8
}

/// Determine the number of codewords available for pure data (without error correction).
pub fn num_data_codewords(version: Version, ecl: Ecl) -> usize {
    let table: [usize; 40] = match ecl {
        Ecl::L => [
            19, 34, 55, 80, 108, 136, 156, 194, 232, 274, 324, 370, 428, 461, 523, 589, 647, 721,
            795, 861, 932, 1006, 1094, 1174, 1276, 1370, 1468, 1531, 1631, 1735, 1843, 1955, 2071,
            2191, 2306, 2434, 2566, 2702, 2812, 2956,
        ],
        Ecl::M => [
            16, 28, 44, 64, 86, 108, 124, 154, 182, 216, 254, 290, 334, 365, 415, 453, 507, 563,
            627, 669, 714, 782, 860, 914, 1000, 1062, 1128, 1193, 1267, 1373, 1455, 1541, 1631,
            1725, 1812, 1914, 1992, 2102, 2216, 2334,
        ],
        Ecl::Q => [
            13, 22, 34, 48, 62, 76, 88, 110, 132, 154, 180, 206, 244, 261, 295, 325, 367, 397, 445,
            485, 512, 568, 614, 664, 718, 754, 808, 871, 911, 985, 1033, 1115, 1171, 1231, 1286,
            1354, 1426, 1502, 1582, 1666,
        ],
        Ecl::H => [
            9, 16, 26, 36, 46, 60, 66, 86, 100, 122, 140, 158, 180, 197, 223, 253, 283, 313, 341,
            385, 406, 442, 464, 514, 538, 596, 628, 661, 701, 745, 793, 845, 901, 961, 986, 1054,
            1096, 1142, 1222, 1276,
        ],
    };
    table[version.number() as usize - 1]
}
