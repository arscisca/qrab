use crate::Version;

/// Determine the QR code's canvas size in pixels for the given `version`.
pub fn canvas_size(version: Version) -> usize {
    17 + version.number() as usize * 4
}
