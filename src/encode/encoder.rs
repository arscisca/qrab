use bitvec::prelude::*;

use super::{
    builder::Builder,
    segment::{Segment, SegmentKind},
    Ecl, EncodingConstraints, EncodingError, QrCode, QrInfo, Version,
};

/// A QR encoder with optional constraints on QR code error correction level and version.
/// # Examples
/// ## Basic usage
/// By default, an `Encoder` will generate the smallest QR code possible to fit the provided data.
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

    /// Encode `data`. It returns an error if the input data cannot be encoded with the given constraints - which could
    /// happen even with the default constraints e.g. when the data is just too big for any QR code.
    pub fn encode<T: AsRef<[u8]>>(self, data: T) -> Result<QrCode, EncodingError> {
        let data = data.as_ref();
        let segments = self.segment(data);
        let info = self.resolve_constraints(&segments)?;
        let codewords = SegmentEncoder::new(&info).encode(segments)?;
        // Add ECC encoding
        let encoded = EcEncoder::new(&info).encode(codewords);
        let qrcode = Builder::new(&info, &self.constraints).build(encoded)?;
        Ok(qrcode)
    }

    fn resolve_constraints(&self, segments: &[Segment]) -> Result<QrInfo, EncodingError> {
        // Compute the midpoint between two version for binary searching
        fn version_midpoint(v1: Version, v2: Version) -> Version {
            Version::try_from((v1.number() + v2.number()) / 2).unwrap()
        }
        // Generate ranges for version and ecl
        let (mut vmin, mut vmax) = (
            *self.constraints.version().start(),
            *self.constraints.version.end(),
        );
        let (emin, emax) = (
            *self.constraints.ecl().start(),
            *self.constraints.ecl().end(),
        );

        // Binary search the most conservative version and ecl pair based on constraints
        let mut info = QrInfo::new(version_midpoint(vmin, vmax), emin);
        let encoding_len = loop {
            let encoding_len = info.predict_encoding_len(segments.iter());
            if encoding_len <= info.num_data_bits() {
                // This combination works, check if version can be any smaller
                if info.version() > vmin && info.version() < vmax {
                    vmax = info.version();
                } else {
                    break encoding_len;
                }
            } else {
                // This combination doesn't work, check if version can be any larger
                if info.version() < vmax {
                    vmin = info.version();
                } else {
                    return Err(EncodingError::DataTooLong {
                        ver_constr: self.constraints.version().clone(),
                        ecl_constr: self.constraints.ecl().clone(),
                    });
                }
            }
            // Update version for next iteration
            let vmid = version_midpoint(vmin, vmax);
            // When vmin - vmax == 1, the midpoint is vmin because of integer divisions, resulting in an infinite loop.
            // Catch this condition which is recognizable when v is not changing anymore.
            let vnext = if vmid != info.version() { vmid } else { vmax };
            info.set_version(vnext);
        };
        // Now pick the highest possible ECL given the chosen version
        info.set_ecl(emin);
        let mut last_valid_ecl;
        while info.ecl() < emax {
            last_valid_ecl = info.ecl();
            info.set_ecl(info.ecl().higher().unwrap_or(Ecl::H));
            let available_space_with_ecl = info.num_data_bits();
            if available_space_with_ecl < encoding_len {
                // The previous ecl was the limit
                info.set_ecl(last_valid_ecl);
                return Ok(info);
            }
        }
        // All error correction levels were valid
        Ok(info)
    }

    pub(crate) fn segment<'a>(&'a self, data: &'a [u8]) -> Vec<Segment<'a>> {
        // TODO: Actually analyze data to perform the best segmentation.
        let segment = Segment::new(data, 0..data.len(), SegmentKind::Bytes);
        vec![segment]
    }
}

impl Default for Encoder {
    fn default() -> Self {
        Self::new()
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
        match segment.kind() {
            SegmentKind::Bytes => self.encode_append_bytes(data),
            SegmentKind::Numeric => self.encode_append_num(data),
            SegmentKind::Alphanumeric => self.encode_append_alnum(data),
        }
    }

    fn encode_append_segment_header(&mut self, segment: &Segment) -> Result<(), EncodingError> {
        // Mode indicator.
        #[rustfmt::skip]
        let mode_indicator = match segment.kind() {
            SegmentKind::Numeric =>      bits![static 0, 0, 0, 1],
            SegmentKind::Alphanumeric => bits![static 0, 0, 1, 0],
            SegmentKind::Bytes =>        bits![static 0, 1, 0, 0],
        };
        self.bits.extend_from_bitslice(mode_indicator);

        // Character count and its bits.
        let char_count = segment.len();
        let char_count_bits = char_count.view_bits::<Msb0>();
        // Number of bits of the character count to report in the header.
        let char_count_bits_len = self.info.char_count_len(segment.kind());
        debug_assert!(
            char_count < (1 << char_count_bits_len) - 1,
            "character count cannot fit in the header"
        );
        // Truncate and append the last `char_count_bits_len` of `char_count_bits`.
        let truncation_start = char_count_bits.len() - char_count_bits_len;
        self.bits
            .extend_from_bitslice(&char_count_bits[truncation_start..]);
        Ok(())
    }

    fn encode_append_bytes(&mut self, bytes: &[u8]) -> Result<(), EncodingError> {
        self.bits.extend_from_raw_slice(bytes);
        Ok(())
    }

    fn encode_append_alnum(&mut self, alnum: &[u8]) -> Result<(), EncodingError> {
        const BIN_DIGITS_LONG: usize = 11;
        const BIN_DIGITS_SHORT: usize = 6;
        fn encode(c: u8) -> Result<u16, EncodingError> {
            match c {
                c @ b'0'..=b'9' => Ok((c - b'0') as u16),
                c @ b'A'..=b'Z' => Ok((c - b'A') as u16 + 10),
                b' ' => Ok(36),
                b'$' => Ok(37),
                b'%' => Ok(38),
                b'*' => Ok(39),
                b'+' => Ok(40),
                b'-' => Ok(41),
                b'.' => Ok(42),
                b'/' => Ok(43),
                b':' => Ok(44),
                invalid => Err(EncodingError::InvalidSegmentKind(SegmentKind::Numeric, Box::new([invalid]))),
            }
        }
        // Collect input into pairs.
        let mut pairs = alnum.chunks_exact(2);
        for pair in &mut pairs {
            let (first, second) = (encode(pair[0])?, encode(pair[1])?);
            let number: u16 = 45 * first + second;
            // Append the last `BIN_DIGITS_LONG` bits of `number`.
            let bits = &number.view_bits::<Msb0>()[u16::BITS as usize - BIN_DIGITS_LONG..];
            self.bits.extend_from_bitslice(bits);
        }
        // There might only be a single remainder.
        if let Some(&remainder) = pairs.remainder().first() {
            let number = encode(remainder)?;
            // Append the last `BIN_DIGITS_SHORT` bits of `number`.
            let bits = &number.view_bits::<Msb0>()[u16::BITS as usize - BIN_DIGITS_SHORT..];
            self.bits.extend_from_bitslice(bits);
        }
        Ok(())
    }

    fn encode_append_num(&mut self, num: &[u8]) -> Result<(), EncodingError> {
        const DIGITS_GROUPING: usize = 3;
        // Divide in groups of 3 digits
        for dec_digits in num.chunks(DIGITS_GROUPING) {
            let dec_digits = String::from_utf8_lossy(dec_digits);
            let number: u16 = dec_digits.parse().map_err(|_| {
                EncodingError::InvalidSegmentKind(
                    SegmentKind::Numeric,
                    num.into(),
                )
            })?;
            let num_bin_digits = 1 + 3 * dec_digits.len();
            let bin_digits = &number.view_bits::<Msb0>()[u16::BITS as usize - num_bin_digits..];
            self.bits.extend_from_bitslice(bin_digits);
        }
        Ok(())
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
        kind: SegmentKind,
        info: &QrInfo,
    ) -> Result<SegmentEncoder, EncodingError> {
        let data = data.as_ref();
        let segment = Segment::new(data, 0..data.len(), kind);
        let mut encoder = SegmentEncoder::new(info);
        encoder.encode_append_segment(segment).map(|_| encoder)
    }

    #[test]
    fn bytes_encoding() {
        let info = QrInfo::new(Version::V1, Ecl::L);
        let encoder = encode_segment_full("hello", SegmentKind::Bytes, &info).unwrap();
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
        let encoder = encode_segment_full("01234567", SegmentKind::Numeric, &info).unwrap();
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
    fn num_invalid_encoding() {
        let info = QrInfo::new(Version::V20, Ecl::L);
        assert!(encode_segment_full("012aaa567", SegmentKind::Numeric, &info).is_err());
    }

    #[test]
    fn alnum_valid_encoding() {
        let info = QrInfo::new(Version::V1, Ecl::H);
        let encoder = encode_segment_full("AC-42", SegmentKind::Alphanumeric, &info).unwrap();
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
    fn alnum_invalid_encoding() {
        let info = QrInfo::new(Version::V1, Ecl::H);
        assert!(encode_segment_full("Ac-42", SegmentKind::Alphanumeric, &info).is_err());
    }

    /// Test the full encoding (header, data, and padding) of some data using a single mode.
    #[test]
    fn bytes_full_encoding() {
        let data = "hello".as_bytes();
        let info = QrInfo::new(Version::V1, Ecl::M);
        let enc = SegmentEncoder::new(&info);
        let codewords = enc
            .encode(vec![Segment::new(data, 0..data.len(), SegmentKind::Bytes)])
            .unwrap();
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
        let segments = e.segment("short".as_ref());
        assert_eq!(
            e.resolve_constraints(&segments).unwrap(),
            QrInfo::new(Version::V1, Ecl::H)
        );
        // Try with data that should result in the largest QR. Leave some space for the encoding headers.
        const SPACE_FOR_ENCODING_HEADERS: usize = 4;
        let largest_data_size =
            QrInfo::new(Version::V40, Ecl::L).num_data_codewords() - SPACE_FOR_ENCODING_HEADERS;
        let data = vec![0; largest_data_size];
        let segments = e.segment(&data);
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
}
