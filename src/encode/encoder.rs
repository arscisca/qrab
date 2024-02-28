use bitvec::{
    prelude::{bits, BitVec, Lsb0, Msb0},
    view::BitView,
};
use itertools::Itertools;

use super::{
    builder::Builder, Ecl, EncodingConstraints, EncodingError, Mode, ModeConstraint, QrCode,
    QrInfo, Segment, Version,
};

/// An encoder to generate QR codes based on some input data with customizable constraints.
///
/// In general, an encoder will seek the best solution compliant to the constraint, where the "best solution" is an
/// encoding that results in the physically smallest QR code with the highest error correction level possible.
/// # Examples
/// ## Unconstrained
/// ```rust
/// use qrab::Encoder;
/// let encoder = Encoder::new();
/// let qrcode = encoder.encode("Hello, world!").unwrap();
/// // With no constraint, the smallest version is picked first, and then the highest error correction level possible.
/// assert_eq!(qrcode.version(), qrab::Version::V1);
/// assert_eq!(qrcode.ecl(), qrab::Ecl::M);
/// ```
/// ## Constrained
/// ```rust
/// use qrab::{Encoder, EncodingConstraints};
/// let encoder = Encoder::with_constraints(
///     EncodingConstraints::new()
///         .with_ecl(qrab::Ecl::H)
/// );
/// let qrcode = encoder.encode("Hello, world!").unwrap();
/// // The constraint on the error correction level forces a larger version.
/// assert_eq!(qrcode.version(), qrab::Version::V2);
/// assert_eq!(qrcode.ecl(), qrab::Ecl::H);
/// ```
pub struct Encoder {
    constraints: EncodingConstraints,
}

impl Encoder {
    /// Create a new encoder with no custom constraints.
    pub fn new() -> Self {
        Self::with_constraints(EncodingConstraints::none())
    }

    /// Create a new encoder that uses the specified constraints.
    pub fn with_constraints(constraints: EncodingConstraints) -> Self {
        Self { constraints }
    }

    /// Encode `data`. Encoding can fail when the constraints (whether the custom ones, or the default ones) are too
    /// restrictive.
    /// # Examples
    /// ```rust
    /// use qrab::Encoder;
    /// let encoder = Encoder::new();
    /// assert!(encoder.encode("Hello, world!").is_ok());
    /// assert!(encoder.encode("too long!".repeat(10_000)).is_err());
    /// ```
    pub fn encode<T: AsRef<[u8]>>(&self, data: T) -> Result<QrCode, EncodingError> {
        let segments = self.segment(data.as_ref())?;
        let info = self.resolve_constraints(&segments)?;
        let codewords = SegmentEncoder::new(&info).encode(segments)?;
        let encoded = EcEncoder::new(&info).encode(codewords);
        let qrcode = Builder::new(&info, &self.constraints).build(encoded)?;
        Ok(qrcode)
    }

    /// Segment input `data` according to the provided constraints.
    pub(crate) fn segment<'a>(&self, data: &'a [u8]) -> Result<Vec<Segment<'a>>, EncodingError> {
        // Get the mode groups and compress them.
        let blueprints = self.assign_modes_and_group(data)?;
        let blueprints = self.compress_mode_groups(blueprints);
        // Check whether we have multiple modes and if that's allowed by the constraint and act accordingly.
        match (blueprints.len(), self.constraints.mode()) {
            (0, _) => {
                unreachable!("there must be at least 1 group")
            }
            (1, _) | (2.., ModeConstraint::AnyMixed | ModeConstraint::TryOrPromoteMixed(_)) => {
                // There is either a single mode, or multiple ones which are allowed: simply collect the segments.
                let mut idx_start = 0;
                let segments: Vec<Segment> = blueprints
                    .into_iter()
                    .map(|bp| {
                        let range = idx_start..(idx_start + bp.len);
                        idx_start += bp.len;
                        Segment::new(&data[range], bp.mode)
                    })
                    .collect();
                // Check that no segments have been lost.
                debug_assert_eq!(data.len(), idx_start);
                Ok(segments)
            }
            (2.., ModeConstraint::AnySingle | ModeConstraint::TryOrPromoteSingle(_)) => {
                // There are multiple modes being used, and that's not allowed; but we are allowed to promote. Promote
                // everything to a single, most generic mode.
                let first_mode = blueprints[0].mode;
                let most_generic = blueprints
                    .into_iter()
                    .map(|bp| bp.mode)
                    .fold(first_mode, |curr_most_generic, mode| {
                        curr_most_generic.most_generic(mode)
                    });
                Ok(vec![Segment::new(data, most_generic)])
            }
            (2.., ModeConstraint::TryOrFail(_)) => {
                // There is a hard constraint on the mode, so we would have failed earlier if we generated any mode
                // other than the constraint itself.
                unreachable!("there cannot be multiple modes with a TryOrFail constraint")
            }
        }
    }

    /// Pick a version and ECL based on the encoder's constraints.
    fn resolve_constraints(&self, segments: &[Segment]) -> Result<QrInfo, EncodingError> {
        /// Compute the midpoint between two versions for binary searching.
        fn version_midpoint(v1: Version, v2: Version) -> Version {
            Version::try_from((v1.number() + v2.number()) / 2).unwrap()
        }
        // Generate ranges for version and ECL.
        let (mut vmin, mut vmax) = (
            *self.constraints.version().start(),
            *self.constraints.version().end(),
        );
        let (emin, emax) = (
            *self.constraints.ecl().start(),
            *self.constraints.ecl().end(),
        );

        // Binary search the most conservative version and ecl pair based on constraints.
        let mut info = QrInfo::new(version_midpoint(vmin, vmax), emin);
        let encoding_len = loop {
            let encoding_len = segments
                .iter()
                .map(|segment| info.predict_segment_encoding_len(segment.mode(), segment.len()))
                .sum();
            // Can the current choice fit the data?
            if encoding_len <= info.num_data_bits() {
                // This combination works, but check if version can be any smaller before confirming this as a choice.
                if vmin < info.version() && info.version() < vmax {
                    vmax = info.version();
                } else {
                    break encoding_len;
                }
            } else {
                // This combination doesn't work, check if version can be any larger and try again.
                if info.version() < vmax {
                    vmin = info.version();
                } else {
                    return Err(EncodingError::DataTooLong {
                        ver_constr: self.constraints.version().clone(),
                        ecl_constr: self.constraints.ecl().clone(),
                    });
                }
            }
            // Update version for next iteration.
            let vmid = version_midpoint(vmin, vmax);
            // When vmin - vmax == 1, the midpoint is vmin because of integer divisions, resulting in an infinite loop.
            // Catch this condition which is recognizable when v is not changing anymore.
            let vnext = if vmid != info.version() { vmid } else { vmax };
            info.set_version(vnext);
        };
        // Now pick the highest possible ECL given the chosen version.
        info.set_ecl(emin);
        let mut last_valid_ecl;
        while info.ecl() < emax {
            last_valid_ecl = info.ecl();
            // Try the next higher ECL and see if it can still fit the data.
            info.set_ecl(info.ecl().higher().unwrap_or(Ecl::H));
            let available_space_with_ecl = info.num_data_bits();
            if available_space_with_ecl < encoding_len {
                // The previous ECL was the limit.
                info.set_ecl(last_valid_ecl);
                return Ok(info);
            }
        }
        // All error correction levels were valid, including the last tested one.
        Ok(info)
    }

    /// Assign the most restrictive segment mode to each byte, then group contiguous mode into `(mode, count)` pairs.
    fn assign_modes_and_group(&self, data: &[u8]) -> Result<Vec<SegmentBlueprint>, EncodingError> {
        /// Review `mode` based on `constraint` and either return a compliant mode or an error.
        fn review(mode: Mode, constraint: ModeConstraint) -> Result<Mode, EncodingError> {
            use ModeConstraint as Constr;
            match constraint {
                Constr::AnyMixed | Constr::AnySingle => {
                    // Any mode is acceptable. Mixing modes will be dealt with later.
                    Ok(mode)
                }
                Constr::TryOrPromoteMixed(constr) | Constr::TryOrPromoteSingle(constr) => {
                    // Use the most generic between `mode` and `constr`. Mixing modes will be dealt with later.
                    Ok(mode.most_generic(constr))
                }
                Constr::TryOrFail(constr) => {
                    // There is a hard constraint. If it is possible to use the constraint (because it is equal to the
                    // proposed mode, or it is more generic), then do so; otherwise return an error.
                    if constr.is_eq_or_more_generic(mode) {
                        Ok(constr)
                    } else {
                        Err(EncodingError::CannotEncodeWithMode(constr))
                    }
                }
            }
        }

        // Assign (and review) the strictest possible mode to each byte.
        let modes: Vec<Mode> = data
            .iter()
            .map(|&byte| review(Mode::strictest(byte), self.constraints.mode()))
            .collect::<Result<_, _>>()?;
        // Group by common mode.
        let groups: Vec<SegmentBlueprint> = modes
            .into_iter()
            .group_by(|mode| *mode)
            .into_iter()
            .map(|(mode, group)| SegmentBlueprint::new(mode, group.count()))
            .collect();
        Ok(groups)
    }

    fn compress_mode_groups(&self, blueprints: Vec<SegmentBlueprint>) -> Vec<SegmentBlueprint> {
        // Only compress when meaningful.
        if blueprints.len() <= 1 {
            return blueprints;
        }

        // TODO: this should be improved. The number of character count bits depends on which of 3 ranges the version
        //  falls into, and it determines the overhead for a mode. Maybe, try to segment for each of the three groups
        //  and pick the best option according to constraints.
        // For now, let's just use the biggest possible version since the overhead will be the largest. This is
        // sub-optimal.
        let info = QrInfo::new(*self.constraints.version().start(), Ecl::L);
        let mut compressed = Vec::with_capacity(blueprints.len());

        let mut bps_iter = blueprints.into_iter();
        let mut curr = bps_iter.next().unwrap();
        for bp in bps_iter {
            // Determine whether it's beneficial to merge the current group to the new one.
            let unmerged_size = info.predict_segment_encoding_len(curr.mode, curr.len)
                + info.predict_segment_encoding_len(bp.mode, bp.len);
            let merged = curr.merge(&bp);
            let merged_size = info.predict_segment_encoding_len(merged.mode, merged.len);
            if merged_size <= unmerged_size {
                // Merging is convenient.
                curr = merged;
            } else {
                // Merging is not convenient: push the current segment blueprint.
                compressed.push(curr);
                curr = bp;
            }
        }
        // Push the last group.
        compressed.push(curr);
        compressed
    }
}

impl Default for Encoder {
    fn default() -> Self {
        Self::new()
    }
}

/// Blueprint of a segment to simplify segmentation.
#[derive(Debug, PartialEq, Eq)]
struct SegmentBlueprint {
    mode: Mode,
    len: usize,
}

impl SegmentBlueprint {
    pub fn new(mode: Mode, len: usize) -> Self {
        Self { mode, len }
    }

    pub fn merge(&self, other: &Self) -> Self {
        Self::new(self.mode.most_generic(other.mode), self.len + other.len)
    }
}

// SegmentEncoder ======================================================================================================
/// Segment encoder to convert a stream of data segments into codewords.
struct SegmentEncoder<'i> {
    info: &'i QrInfo,
    bits: BitVec<u8, Msb0>,
}

impl<'i> SegmentEncoder<'i> {
    pub fn new(info: &'i QrInfo) -> Self {
        Self {
            info,
            bits: BitVec::with_capacity(info.num_data_bits()),
        }
    }

    /// Encode the associated data.
    pub fn encode(mut self, segments: Vec<Segment>) -> Result<Vec<u8>, EncodingError> {
        // Encode each segment singularly
        for segment in segments {
            self.encode_append_segment(segment)?;
        }
        // If it fits, append end indicator
        if self.bits.capacity() - self.bits.len() >= 4 {
            self.bits.extend_from_bitslice(bits![0, 0, 0, 0]);
        }
        // Append padding
        self.append_padding();
        // Collect codewords
        let codewords = self.bits.into_vec();
        Ok(codewords)
    }

    fn encode_append_segment(&mut self, segment: Segment) -> Result<(), EncodingError> {
        self.encode_append_segment_header(&segment)?;
        let data = segment.data();
        match segment.mode() {
            Mode::Bytes => self.encode_append_bytes(data),
            Mode::Numeric => self.encode_append_num(data),
            Mode::Alphanumeric => self.encode_append_alnum(data),
        }
        Ok(())
    }

    fn encode_append_segment_header(&mut self, segment: &Segment) -> Result<(), EncodingError> {
        // Mode indicator.
        #[rustfmt::skip]
        let mode_indicator = match segment.mode() {
            Mode::Numeric =>      bits![static 0, 0, 0, 1],
            Mode::Alphanumeric => bits![static 0, 0, 1, 0],
            Mode::Bytes =>        bits![static 0, 1, 0, 0],
        };
        self.bits.extend_from_bitslice(mode_indicator);

        // Character count and its bits.
        let char_count = segment.len();
        let char_count_bits = char_count.view_bits::<Msb0>();
        // Number of bits of the character count to report in the header.
        let char_count_bits_len = self.info.char_count_len(segment.mode());
        debug_assert!(
            char_count < (1 << char_count_bits_len),
            "character count cannot fit in the header"
        );
        // Truncate and append the last `char_count_bits_len` of `char_count_bits`.
        let truncation_start = char_count_bits.len() - char_count_bits_len;
        self.bits
            .extend_from_bitslice(&char_count_bits[truncation_start..]);
        Ok(())
    }

    fn encode_append_bytes(&mut self, bytes: &[u8]) {
        self.bits.extend_from_raw_slice(bytes);
    }

    fn encode_append_alnum(&mut self, alnum: &[u8]) {
        const BIN_DIGITS_LONG: usize = 11;
        const BIN_DIGITS_SHORT: usize = 6;
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
        // Collect input into pairs.
        let mut pairs = alnum.chunks_exact(2);
        for pair in &mut pairs {
            let (first, second) = (encode(pair[0]), encode(pair[1]));
            let number: u16 = 45 * first + second;
            // Append the last `BIN_DIGITS_LONG` bits of `number`.
            let bits = &number.view_bits::<Msb0>()[u16::BITS as usize - BIN_DIGITS_LONG..];
            self.bits.extend_from_bitslice(bits);
        }
        // There might only be a single remainder.
        if let Some(&remainder) = pairs.remainder().first() {
            let number = encode(remainder);
            // Append the last `BIN_DIGITS_SHORT` bits of `number`.
            let bits = &number.view_bits::<Msb0>()[u16::BITS as usize - BIN_DIGITS_SHORT..];
            self.bits.extend_from_bitslice(bits);
        }
    }

    fn encode_append_num(&mut self, num: &[u8]) {
        const DIGITS_GROUPING: usize = 3;
        // Divide in groups of 3 digits
        for dec_digits in num.chunks(DIGITS_GROUPING) {
            let dec_digits = String::from_utf8_lossy(dec_digits);
            let number: u16 = dec_digits
                .parse()
                .unwrap_or_else(|_| panic!("cannot encode '{dec_digits}' in numeric mode"));
            let num_bin_digits = 1 + 3 * dec_digits.len();
            let bin_digits = &number.view_bits::<Msb0>()[u16::BITS as usize - num_bin_digits..];
            self.bits.extend_from_bitslice(bin_digits);
        }
    }

    fn append_padding(&mut self) {
        // Pad with zeros until last codeword is filled
        let last_codeword_missing_bits = self.bits.len() % 8;
        self.bits
            .resize(self.bits.len() + last_codeword_missing_bits, false);
        // Append pad codewords until there is enough space for the end symbol
        let mut pad_codeword = 0b11101100u8;
        const PADDING_TOGGLE_MASK: u8 = 0b11111101u8;
        while self.bits.capacity() - self.bits.len() >= 8 {
            self.bits
                .extend_from_bitslice(pad_codeword.view_bits::<Msb0>());
            pad_codeword ^= PADDING_TOGGLE_MASK;
        }
        // Append 0s until capacity is reached
        self.bits.resize(self.bits.capacity(), false);
    }
}

// EcEncoder ===========================================================================================================
pub struct EcEncoder {
    d_len: usize,
    n_blocks: usize,
    n_codewords: usize,
    n_ecc_per_block: usize,
}

impl EcEncoder {
    /// Construct a new encoder with the specified version and ecl `settings`.
    pub fn new(info: &QrInfo) -> Self {
        Self {
            d_len: info.num_data_codewords(),
            n_blocks: info.num_ecc_blocks(),
            n_codewords: info.num_codewords(),
            n_ecc_per_block: info.num_ecc_codewords_per_block(),
        }
    }

    pub fn encode(self, data: Vec<u8>) -> Vec<u8> {
        debug_assert_eq!(
            data.len(),
            self.d_len,
            "Unexpected data length based on the version and ecl this encoder was constructed with"
        );
        // Prepare encoder
        let encoder = reed_solomon::Encoder::new(self.n_ecc_per_block);
        // Initialize result vector and divide it in its short, long and error correction ranges
        let mut result = vec![0; self.n_codewords];
        let (sresult, lresult, result_ec) = self.split_result_ranges(&mut result);
        // Divide data in short and long blocks
        let (sdata, ldata) = self.split_data_blocks(&data);
        // Encode short blocks first
        let mut block = 0;
        let short_block_len = data.len() / self.n_blocks;
        for data in sdata.chunks(short_block_len) {
            let buffer = encoder.encode(data);
            self.place_codewords(sresult, buffer.data(), block);
            self.place_codewords(result_ec, buffer.ecc(), block);
            block += 1;
        }
        // Encode long blocks
        let long_block_len = short_block_len + 1;
        for (i, data) in ldata.chunks(long_block_len).enumerate() {
            let buffer = encoder.encode(data);
            // The base part of the data is placed like short blocks
            self.place_codewords(sresult, &buffer.data()[..short_block_len], block);
            // The extra codeword goes in its own section
            lresult[i] = *buffer
                .data()
                .last()
                .expect("Data buffers should not be empty");
            self.place_codewords(result_ec, buffer.ecc(), block);
            block += 1;
        }
        result
    }

    fn place_codewords(&self, result: &mut [u8], codewords: &[u8], block: usize) {
        for (i, codeword) in codewords.iter().enumerate() {
            result[block + i * self.n_blocks] = *codeword;
        }
    }

    fn split_data_blocks<'a>(&self, data: &'a [u8]) -> (&'a [u8], &'a [u8]) {
        let short_block_len = data.len() / self.n_blocks;
        let extra_codewords = data.len() % self.n_blocks;
        // Extra codewords are placed one per each long block
        let n_long_blocks = extra_codewords;
        let n_short_blocks = self.n_blocks - n_long_blocks;
        // Short blocks come first, then long blocks
        let short_data_end = short_block_len * n_short_blocks;
        data.split_at(short_data_end)
    }

    fn split_result_ranges<'a>(
        &self,
        result: &'a mut [u8],
    ) -> (&'a mut [u8], &'a mut [u8], &'a mut [u8]) {
        // Data and error correction split simply at the end of the data
        let (data, ec) = result.split_at_mut(self.d_len);
        // Data is then divided in short and long, where the long codewords come last
        let sdata_len = self.n_blocks * (self.d_len / self.n_blocks);
        let (sdata, ldata) = data.split_at_mut(sdata_len);
        (sdata, ldata, ec)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn encode_segment_full<T: AsRef<[u8]>>(
        data: T,
        mode: Mode,
        info: &QrInfo,
    ) -> Result<SegmentEncoder, EncodingError> {
        let data = data.as_ref();
        let segment = Segment::new(data, mode);
        let mut encoder = SegmentEncoder::new(info);
        encoder.encode_append_segment(segment).map(|_| encoder)
    }

    #[test]
    fn bytes_encoding() {
        let info = QrInfo::new(Version::V1, Ecl::L);
        let encoder = encode_segment_full("hello", Mode::Bytes, &info).unwrap();
        #[rustfmt::skip]
        assert_eq!(
            encoder.bits,
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
    fn num_valid_encoding() {
        let info = QrInfo::new(Version::V1, Ecl::H);
        let encoder = encode_segment_full("01234567", Mode::Numeric, &info).unwrap();
        #[rustfmt::skip]
        assert_eq!(
            encoder.bits,
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
    fn num_invalid_encoding() {
        let info = QrInfo::new(Version::V20, Ecl::L);
        encode_segment_full("012aaa567", Mode::Numeric, &info).unwrap();
    }

    #[test]
    fn alnum_valid_encoding() {
        let info = QrInfo::new(Version::V1, Ecl::H);
        let encoder = encode_segment_full("AC-42", Mode::Alphanumeric, &info).unwrap();
        #[rustfmt::skip]
        assert_eq!(
            encoder.bits,
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
    fn alnum_invalid_encoding() {
        let info = QrInfo::new(Version::V1, Ecl::H);
        encode_segment_full("Ac-42", Mode::Alphanumeric, &info).unwrap();
    }

    /// Test the full encoding (header, data, and padding) of some data using a single mode.
    #[test]
    fn bytes_full_encoding() {
        let data = "hello".as_bytes();
        let info = QrInfo::new(Version::V1, Ecl::M);
        let enc = SegmentEncoder::new(&info);
        let codewords = enc.encode(vec![Segment::new(data, Mode::Bytes)]).unwrap();
        assert_eq!(
            &codewords,
            &vec![
                0x40, 0x56, 0x86, 0x56, 0xC6, 0xC6, 0xF0, 0xEC, 0x11, 0xEC, 0x11, 0xEC, 0x11, 0xEC,
                0x11, 0xEC
            ]
        );
    }

    #[test]
    fn no_constraints() {
        // Try very short data with no constraints: this should result in a 1H code
        let e = Encoder::new();
        let segments = e.segment("short".as_ref()).unwrap();
        assert_eq!(
            e.resolve_constraints(&segments).unwrap(),
            QrInfo::new(Version::V1, Ecl::H)
        );
        // Try with data that should result in the largest QR. Leave some space for the encoding headers.
        const SPACE_FOR_ENCODING_HEADERS: usize = 4;
        let largest_data_size =
            QrInfo::new(Version::V40, Ecl::L).num_data_codewords() - SPACE_FOR_ENCODING_HEADERS;
        let data = vec![0; largest_data_size];
        let segments = e.segment(&data).unwrap();
        assert_eq!(
            e.resolve_constraints(&segments).unwrap(),
            QrInfo::new(Version::V40, Ecl::L)
        );
    }

    fn ec_encode<T: Into<Vec<u8>>>(data: T, version: Version, ecl: Ecl) -> Vec<u8> {
        let encoder = EcEncoder::new(&QrInfo::new(version, ecl));
        encoder.encode(data.into())
    }

    #[test]
    fn ec_encoding() {
        // Check simple encoding
        let data = [
            0x10, 0x20, 0x0c, 0x56, 0x61, 0x80, 0xec, 0x11, 0xec, 0x11, 0xec, 0x11, 0xec, 0x11,
            0xec, 0x11,
        ];
        let encoded = ec_encode(data, Version::V1, Ecl::M);
        let expected = [
            0x10, 0x20, 0x0c, 0x56, 0x61, 0x80, 0xec, 0x11, 0xec, 0x11, 0xec, 0x11, 0xec, 0x11,
            0xec, 0x11, 0xa5, 0x24, 0xd4, 0xc1, 0xed, 0x36, 0xc7, 0x87, 0x2c, 0x55,
        ];
        assert_eq!(encoded, expected);
    }

    #[test]
    fn mode_assignment_free() {
        let data = "AAA00000mBBabcde123";
        let encoder = Encoder::new();
        let groups = encoder.assign_modes_and_group(data.as_ref()).unwrap();
        // Check that no byte was unassigned.
        assert_eq!(data.len(), groups.iter().map(|bp| bp.len).sum());
        // Check that the modes were assigned correctly.
        assert_eq!(
            groups,
            [
                SegmentBlueprint::new(Mode::Alphanumeric, 3),
                SegmentBlueprint::new(Mode::Numeric, 5),
                SegmentBlueprint::new(Mode::Bytes, 1),
                SegmentBlueprint::new(Mode::Alphanumeric, 2),
                SegmentBlueprint::new(Mode::Bytes, 5),
                SegmentBlueprint::new(Mode::Numeric, 3),
            ]
        );
    }

    #[test]
    fn mode_assignment_constrained() {
        let data: &[u8] = "ABC123abc".as_ref();
        // Try successful mode constraints.
        // Note: at this stage, single / mixed constraints are not enforced yet - only individual mode constraints will
        // be valid.
        #[rustfmt::skip]
        let possible_constraints_results = [
            // Any.
            (ModeConstraint::AnySingle, vec![
                SegmentBlueprint::new(Mode::Alphanumeric, 3),
                SegmentBlueprint::new(Mode::Numeric, 3),
                SegmentBlueprint::new(Mode::Bytes, 3),
            ]),
            (ModeConstraint::AnyMixed, vec![
                SegmentBlueprint::new(Mode::Alphanumeric, 3),
                SegmentBlueprint::new(Mode::Numeric, 3),
                SegmentBlueprint::new(Mode::Bytes, 3),
            ]),
            // Try or promote.
            (ModeConstraint::TryOrPromoteSingle(Mode::Numeric), vec![
                SegmentBlueprint::new(Mode::Alphanumeric, 3),
                SegmentBlueprint::new(Mode::Numeric, 3),
                SegmentBlueprint::new(Mode::Bytes, 3),
            ]),
            (ModeConstraint::TryOrPromoteMixed(Mode::Numeric), vec![
                SegmentBlueprint::new(Mode::Alphanumeric, 3),
                SegmentBlueprint::new(Mode::Numeric, 3),
                SegmentBlueprint::new(Mode::Bytes, 3),
            ]),
            (ModeConstraint::TryOrPromoteSingle(Mode::Alphanumeric), vec![
                SegmentBlueprint::new(Mode::Alphanumeric, 6),
                SegmentBlueprint::new(Mode::Bytes, 3)
            ]),
            (ModeConstraint::TryOrPromoteMixed(Mode::Alphanumeric), vec![
                SegmentBlueprint::new(Mode::Alphanumeric, 6),
                SegmentBlueprint::new(Mode::Bytes, 3)
            ]),
            (ModeConstraint::TryOrPromoteSingle(Mode::Bytes), vec![SegmentBlueprint::new(Mode::Bytes, 9)]),
            (ModeConstraint::TryOrPromoteMixed(Mode::Bytes), vec![SegmentBlueprint::new(Mode::Bytes, 9)]),
            // Try or fail.
            (ModeConstraint::TryOrFail(Mode::Bytes), vec![SegmentBlueprint::new(Mode::Bytes, 9)]),
        ];
        for (constraint, result) in possible_constraints_results {
            let encoder =
                Encoder::with_constraints(EncodingConstraints::none().with_mode(constraint));
            let groups = encoder.assign_modes_and_group(data).unwrap();
            assert_eq!(
                groups, result,
                "encoding with constraint {:?} did not match the expected result",
                constraint
            );
        }
    }

    #[test]
    fn compression() {
        let groups = vec![
            SegmentBlueprint::new(Mode::Bytes, 10),
            SegmentBlueprint::new(Mode::Numeric, 1),
            SegmentBlueprint::new(Mode::Bytes, 10),
        ];
        let encoder = Encoder::new();
        let compressed = encoder.compress_mode_groups(groups);
        assert_eq!(compressed, vec![SegmentBlueprint::new(Mode::Bytes, 21)]);
    }
}
