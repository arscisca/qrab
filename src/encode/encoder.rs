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
        let (mut vmin, mut vmax) = (*self.constraints.version().start(), *self.constraints.version.end());
        let (emin, emax) = (*self.constraints.ecl().start(), *self.constraints.ecl().end());

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
            // Header section (segment kind and length)
            self.append_segment_header(&segment);
            // Data section
            let slice = segment.data();
            match segment.kind() {
                SegmentKind::Bytes => self.encode_append_bytes(slice)?,
                SegmentKind::Alphanumeric => self.encode_append_alnum(slice)?,
                SegmentKind::Numeric => self.encode_append_num(slice)?,
            };
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

    fn append_segment_header(&mut self, segment: &Segment) {
        // Determine segment kind code
        // Determine segment length
        let len = segment.len();
        // Bytes header
        assert_eq!(
            segment.kind(),
            SegmentKind::Bytes,
            "Other segment headers are different"
        );
        self.bits.extend_from_bitslice(bits![0, 1, 0, 0]);
        // Character count indicator
        let char_count_len = self.info.char_count_len(segment.kind());
        let len_bits = len.view_bits::<Msb0>();
        let len_bits_view_start = len_bits.len() - char_count_len;
        let len_bits_view_end = len_bits.len();
        self.bits
            .extend_from_bitslice(&len_bits[len_bits_view_start..len_bits_view_end]);
    }

    fn encode_append_bytes(&mut self, bytes: &[u8]) -> Result<(), EncodingError> {
        self.bits.extend_from_raw_slice(bytes);
        Ok(())
    }

    fn encode_append_alnum(&mut self, alnum: &[u8]) -> Result<(), EncodingError> {
        todo!()
    }

    fn encode_append_num(&mut self, num: &[u8]) -> Result<(), EncodingError> {
        todo!()
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

    #[test]
    fn bytes_data_encoding() {
        let data = "hello".as_bytes();
        let info = QrInfo::new(Version::V1, Ecl::L);
        let mut e = SegmentEncoder::new(&info);
        // Encoding bytes does not change the bytes themselves
        e.encode_append_bytes(data).unwrap();
        assert_eq!(
            e.bits,
            bits![
                0, 1, 1, 0, 1, 0, 0, 0, // h
                0, 1, 1, 0, 0, 1, 0, 1, // e
                0, 1, 1, 0, 1, 1, 0, 0, // l
                0, 1, 1, 0, 1, 1, 0, 0, // l
                0, 1, 1, 0, 1, 1, 1, 1, // o
            ]
        );
    }

    #[test]
    fn bytes_header() {
        let data = "doggo".as_bytes();
        let info = QrInfo::new(Version::V1, Ecl::L);
        let s = Segment::new(data, 0..data.len(), SegmentKind::Bytes);
        let mut e = SegmentEncoder::new(&info);
        e.append_segment_header(&s);
        // ---------------------[.HEADER...|.CHARACTER COUNT.......]
        assert_eq!(e.bits, bits![0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1])
    }

    #[test]
    fn bytes_full() {
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
