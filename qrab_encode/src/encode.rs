use std::ops::{Bound, RangeBounds, RangeInclusive};

use bitvec::{
    bits,
    order::{Lsb0, Msb0},
    vec::BitVec,
    view::BitView,
};
use itertools::Itertools;

use qrab_core::qrstandard;
use qrab_core::{Ecl, Mask, MaskTable, Version};
use qrab_core::{Meta, QrCode};
use qrab_core::{Mode, Segment};

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
        let mut segmenter = Segmenter::new(data, &self.constraints)?;
        let meta = self.resolve_constraints(data, &mut segmenter)?;
        let segments = segmenter.segment(meta.version);
        let codewords = SegmentEncoder::new(&meta).encode(data, segments);
        todo!("add error correction to codewords {:?}", codewords);
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

    /// Get the minimum and maximum allowed [Version]s according to the constraints.
    fn allowed_version_extremes(&self) -> (Version, Version) {
        (
            *self.constraints.version.start(),
            *self.constraints.version.end(),
        )
    }

    /// Get the minimum and maximum allowed [Ecl]s according to the constraints.
    fn allowed_ecl_extremes(&self) -> (Ecl, Ecl) {
        (*self.constraints.ecl.start(), *self.constraints.ecl.end())
    }

    /// Resolve the constraints and decide the [Version] and [Ecl].
    fn resolve_constraints(
        &self,
        data: &[u8],
        segmenter: &mut Segmenter,
    ) -> Result<Meta, EncodingError> {
        use qrstandard::segment_encoding_len as encoding_len;
        // Binary search the most conservative version to encode the segments.
        let (mut vmin, mut vmax) = self.allowed_version_extremes();
        let (emin, emax) = self.allowed_ecl_extremes();
        let (version, data_enc_len) = loop {
            let v = version_midpoint(vmin, vmax);
            let segments = segmenter.segment(v);
            let curr_encoding_len: usize = segments
                .iter()
                .map(|segment| encoding_len(segment, v))
                .sum();
            if curr_encoding_len < qrstandard::num_data_bits(v, emin) {
                // This setup can fit the data. Is there a more conservative one?
                if vmin < v && v < vmax {
                    vmax = v;
                } else {
                    break (v, curr_encoding_len);
                }
            } else {
                // This setup cannot fit the data.
                if v < vmax {
                    vmin = v;
                } else {
                    return Err(EncodingError::DataTooBig(
                        data.len(),
                        *self.constraints.version.end(),
                        emin,
                    ));
                }
            }
        };
        // Version has been chosen, choose the highest possible ECL.
        let mut ecl = emax;
        while ecl >= emin {
            if data_enc_len < qrstandard::num_data_bits(version, ecl) {
                break;
            }
            let Some(new_ecl) = ecl.decr() else {
                break;
            };
            ecl = new_ecl;
        }
        Ok(Meta {
            version,
            ecl,
            // For now mask doesn't matter.
            mask: Mask::M000,
        })
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
    #[error("cannot encode {0} B of data with best case version {1} and ECL {2} according to the constraints")]
    DataTooBig(usize, Version, Ecl),
}

/// Get the midpoint between versions `v1` and `v2`, useful for binary searching.
fn version_midpoint(v1: Version, v2: Version) -> Version {
    // The midpoint between two versions always has a valid number.
    Version::new((v1.number() + v2.number()) / 2).unwrap()
}

struct Segmenter {
    uncompressed: Vec<Segment>,
    compressed_cache: [Option<Vec<Segment>>; 3],
}

impl Segmenter {
    pub fn new(data: &[u8], constraints: &Constraints) -> Result<Self, EncodingError> {
        // Determine the ideal mode for each byte regardless of constraints.
        let free_modes = data.iter().map(|&byte| Mode::from(byte));
        // Apply the constraint to create the segments.
        let segmenter = match constraints.mode {
            ModeConstraint::AnyMixed => {
                let uncompressed = free_modes
                    .chunk_by(|mode| *mode)
                    .into_iter()
                    .map(|(mode, chunk)| Segment::new(mode, chunk.count()))
                    .collect();
                let compressed_cache = [None, None, None];
                Self {
                    uncompressed,
                    compressed_cache,
                }
            }
            ModeConstraint::AnySingle => {
                let most_generic_mode = free_modes.reduce(Mode::most_generic).unwrap_or(Mode::Num);
                let segments = vec![Segment::new(most_generic_mode, data.len())];
                Self {
                    uncompressed: segments.clone(),
                    compressed_cache: std::array::from_fn(|_| Some(segments.clone())),
                }
            }
            ModeConstraint::Only(constr) => {
                for &byte in data {
                    let desired = Mode::from(byte);
                    if !desired.could_be_promoted_to(constr) {
                        return Err(EncodingError::CannotEncodeWithMode(byte, constr));
                    }
                }
                let segments = vec![Segment::new(constr, data.len())];
                Self {
                    uncompressed: segments.clone(),
                    compressed_cache: std::array::from_fn(|_| Some(segments.clone())),
                }
            }
        };
        Ok(segmenter)
    }

    /// Segment `data` by associating a [Mode] to each group of bytes and smartly apply promotions to minimize the data
    /// storage space.
    pub fn segment(&mut self, version: Version) -> Vec<Segment> {
        // We already have an uncompressed segmentation, we only need to compress it according to
        // `version`.
        use qrstandard::segment_encoding_len as encoding_len;

        let group = qrstandard::char_count_version_group(version);

        // Do we already have some cache about this version group?
        if let Some(compressed) = self.compressed_cache[group].as_ref() {
            return compressed.clone();
        }

        let mut compressed = Vec::with_capacity(self.uncompressed.len());
        let mut current = self.uncompressed[0].clone();
        let mut current_enc_len = encoding_len(&current, version);
        for segment in &self.uncompressed[1..] {
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
        // Store the result in cache and return it.
        self.compressed_cache[group] = Some(compressed.clone());
        compressed
    }
}

struct SegmentEncoder {
    version: Version,
    bits: BitVec<u8, Msb0>,
}

impl SegmentEncoder {
    /// Construct a new segment encoder.
    pub fn new(meta: &Meta) -> Self {
        Self {
            version: meta.version,
            bits: BitVec::with_capacity(qrstandard::num_data_bits(meta.version, meta.ecl)),
        }
    }

    /// Encode segments into codewords.
    pub fn encode<S: IntoIterator<Item = Segment>>(mut self, data: &[u8], segments: S) -> Vec<u8> {
        let mut offset = 0;
        for segment in segments.into_iter() {
            self.encode_header(&segment);
            let data = &data[offset..(offset + segment.len)];
            match segment.mode {
                Mode::Bytes => self.encode_body_bytes(data),
                Mode::Alnum => self.encode_body_alnum(data),
                Mode::Num => self.encode_body_num(data),
            }
            offset += segment.len;
        }
        self.add_padding();
        self.bits.into_vec()
    }

    /// Encode the header of the segment, which contains the segment mode indicator and the
    /// character count.
    fn encode_header(&mut self, segment: &Segment) {
        // Encode the mode indicator.
        let mode_indicator = match segment.mode {
            Mode::Num => bits![static 0, 0, 0, 1],
            Mode::Alnum => bits![static 0, 0, 1, 0],
            Mode::Bytes => bits![static 0, 1, 0, 0],
        };
        self.bits.extend_from_bitslice(mode_indicator);
        // Encode the character count.
        let char_count_len = qrstandard::char_count_len(segment.mode, self.version);
        let char_count_bits = &segment.len.view_bits::<Msb0>();
        self.bits
            .extend_from_bitslice(&char_count_bits[char_count_bits.len() - char_count_len..])
    }

    /// Encode the body of a segment in byte mode.
    fn encode_body_bytes(&mut self, data: &[u8]) {
        self.bits.extend_from_raw_slice(data)
    }

    /// Encode the body of a segment in alphanumeric mode.
    fn encode_body_alnum(&mut self, data: &[u8]) {
        fn encode(c: u8) -> u16 {
            match c {
                c @ b'0'..=b'9' => (c - b'0') as u16,
                c @ b'A'..=b'Z' => (c - b'A') as u16 + 10,
                b' ' => 36,
                b'$' => 37,
                b'%' => 38,
                b'*' => 39,
                b'+' => 40,
                b'-' => 41,
                b'.' => 42,
                b'/' => 43,
                b':' => 44,
                invalid => panic!("cannot encode byte 0x{:x} in alphanumeric mode", invalid),
            }
        }
        const BIN_DIGITS_LONG: usize = u16::BITS as usize - 11;
        const BIN_DIGITS_SHORT: usize = u16::BITS as usize - 6;
        // Collect input into pairs.
        let mut pairs = data.chunks_exact(2);
        for pair in &mut pairs {
            let (first, second) = (encode(pair[0]), encode(pair[1]));
            let number = 45 * first + second;
            self.bits
                .extend_from_bitslice(&number.view_bits::<Msb0>()[BIN_DIGITS_LONG..])
        }
        // If there is any leftover:
        if let Some(&leftover) = pairs.remainder().first() {
            let number = encode(leftover);
            self.bits
                .extend_from_bitslice(&number.view_bits::<Msb0>()[BIN_DIGITS_SHORT..])
        }
    }

    /// Encode the body of a segment in numeric mode.
    fn encode_body_num(&mut self, data: &[u8]) {
        const DIGIT_GROUPING: usize = 3;
        for dec_digits in data.chunks(DIGIT_GROUPING) {
            let dec_digits = String::from_utf8_lossy(dec_digits);
            let number: u16 = dec_digits
                .parse()
                .unwrap_or_else(|_| panic!("cannot encode '{}' in numeric mode", dec_digits));
            let bin_digits_to_use = u16::BITS as usize - 3 * dec_digits.len() - 1;
            self.bits
                .extend_from_bitslice(&number.view_bits::<Msb0>()[bin_digits_to_use..])
        }
    }

    /// Append padding by completing any last incomplete codeword, and then appending the
    /// predefined padding codewords until the end.
    fn add_padding(&mut self) {
        // Pad with zeros until the last codeword is filled.
        let last_codeword_missing_bits = self.bits.len() % 8;
        self.bits
            .resize(self.bits.len() + last_codeword_missing_bits, false);
        // Append padding codewords.
        let mut padding = 0b11101100u8;
        const PADDING_TOGGLE: u8 = 0b11111101u8;
        while self.bits.capacity() - self.bits.len() >= 8 {
            self.bits.extend_from_bitslice(padding.view_bits::<Msb0>());
            padding ^= PADDING_TOGGLE;
        }
        // Pad with 0 until capacity is reached.
        self.bits.resize(self.bits.capacity(), false);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn encode_single_segment<T: AsRef<[u8]>>(
        data: T,
        mode: Mode,
        version: Version,
    ) -> BitVec<u8, Msb0> {
        let meta = Meta {
            version,
            ecl: Ecl::L,
            mask: Mask::M000,
        };
        let mut encoder = SegmentEncoder::new(&meta);
        let data = data.as_ref();
        encoder.encode_header(&Segment::new(mode, data.len()));
        match mode {
            Mode::Bytes => encoder.encode_body_bytes(data),
            Mode::Alnum => encoder.encode_body_alnum(data),
            Mode::Num => encoder.encode_body_num(data),
        }
        encoder.bits
    }

    #[test]
    fn test_segment_header_encoding() {
        let meta = Meta {
            version: Version::V01,
            ecl: Ecl::L,
            mask: Mask::M000,
        };
        let mut encoder = SegmentEncoder::new(&meta);
        encoder.encode_header(&Segment::new(Mode::Bytes, 5));
        assert_eq!(
            encoder.bits,
            bits![
                0, 1, 0, 0, // Mode indicator
                0, 0, 0, 0, 0, 1, 0, 1, // Character count
            ]
        )
    }

    #[test]
    fn test_bytes_encoding() {
        #[rustfmt::skip]
        assert_eq!(
            encode_single_segment("hello", Mode::Bytes, Version::V01),
            bits![
                // Header
                0, 1, 0, 0,             // Mode indicator
                0, 0, 0, 0, 0, 1, 0, 1, // Character count
                // Data
                0, 1, 1, 0, 1, 0, 0, 0, // h
                0, 1, 1, 0, 0, 1, 0, 1, // e
                0, 1, 1, 0, 1, 1, 0, 0, // l
                0, 1, 1, 0, 1, 1, 0, 0, // l
                0, 1, 1, 0, 1, 1, 1, 1, // o
            ]
        );
    }

    #[test]
    fn test_valid_num_encoding() {
        #[rustfmt::skip]
        assert_eq!(
            encode_single_segment("01234567", Mode::Num, Version::V01),
            bits![
                // Header
                0, 0, 0, 1,                     // Mode indicator
                0, 0, 0, 0, 0, 0, 1, 0, 0, 0,   // Character count
                // Data
                0, 0, 0, 0, 0, 0, 1, 1, 0, 0,   // 012
                0, 1, 0, 1, 0, 1, 1, 0, 0, 1,   // 345
                1, 0, 0, 0, 0, 1, 1,            // 67
            ]
        );
    }

    #[test]
    #[should_panic(expected = "cannot encode 'aaa' in numeric mode")]
    fn test_invalid_num_encoding() {
        encode_single_segment("012aaa567", Mode::Num, Version::V25);
    }

    #[test]
    fn test_valid_alnum_encoding() {
        #[rustfmt::skip]
        assert_eq!(
            encode_single_segment("AC-42", Mode::Alnum, Version::V01),
            bits![
                // Header
                0, 0, 1, 0,                         // Mode indicator
                0, 0, 0, 0, 0, 0, 1, 0, 1,          // Character count
                // Data
                0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0,    // AC
                1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1,    // -4
                0, 0, 0, 0, 1, 0                    // 2
            ]
        );
    }

    #[test]
    #[should_panic(expected = "cannot encode byte 0x63 in alphanumeric mode")]
    fn test_invalid_alnum_encoding() {
        encode_single_segment("Ac-42", Mode::Alnum, Version::V01);
    }

    /// Test the full encoding (header, data, and padding) of some data using a single mode.
    #[test]
    fn test_bytes_full_encoding() {
        let data = "hello".as_bytes();
        let meta = Meta {version: Version::V01, ecl: Ecl::M, mask: Mask::M000};
        let enc = SegmentEncoder::new(&meta);
        let codewords = enc.encode(data, [Segment::new(Mode::Bytes, data.len())]);
        assert_eq!(codewords.len()*8, qrstandard::num_data_bits(meta.version, meta.ecl));
        assert_eq!(
            &codewords,
            &vec![
                0x40, 0x56, 0x86, 0x56, 0xC6, 0xC6, 0xF0, 0xEC, 0x11, 0xEC, 0x11, 0xEC, 0x11, 0xEC,
                0x11, 0xEC
            ]
        );
    }
}
