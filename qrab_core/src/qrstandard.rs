use crate::{Mode, Version};
use crate::Segment;

/// Determine the QR code's canvas size in pixels for the given `version`.
pub fn canvas_size(version: Version) -> usize {
    17 + version.number() as usize * 4
}

/// Get the number of bits of the character count of `mode` for the given `version`.
pub fn char_count_len(mode: Mode, version: Version) -> usize {
    let base = match mode {
        Mode::Bytes => 8,
        Mode::Alnum => 9,
        Mode::Num => 10,
    };
    let group = match version.number() {
        1..=9 => 0,
        10..=26 => 1,
        27..=40 => 2,
        invalid => unreachable!("invalid version '{}'", invalid)
    };
    let factor = match (mode, group) {
        (Mode::Bytes, 0) => 0,
        (Mode::Bytes, _) => 4,
        (Mode::Alnum, group) => group,
        (Mode::Num, group) => group,
    };
    base + 2 * factor
}

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
