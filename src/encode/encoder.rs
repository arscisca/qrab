use bitvec::prelude::*;

use super::{
    builder::Builder,
    ecc::ReedSolomonEncoder,
    segment::{Segment, SegmentKind},
    Ecl, EncodingConstraints, EncodingError, QrCode, Settings, Version,
};

use crate::qrcode::properties;

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
        let (version, ecl) = self.resolve_constraints(&segments)?;
        let constrained = ConstrainedEncoder::new(Settings::new(version, ecl));
        constrained.encode(segments)
    }

    fn resolve_constraints(&self, segments: &[Segment]) -> Result<(Version, Ecl), EncodingError> {
        // Compute the midpoint between two version for binary searching
        fn version_midpoint(v1: Version, v2: Version) -> Version {
            Version::try_from((v1.number() + v2.number()) / 2).unwrap()
        }
        // Compute the encoding length in bits given the version and ecl
        let encoding_len = |version| segments
                .iter()
                .map(|s| s.predicted_encoding_len(version))
                .sum::<usize>();
        // Generate ranges for version and ecl
        let (mut vmin, mut vmax) = self
            .constraints
            .version()
            .extremes_or_defaults(Version::V1, Version::V40);
        let (emin, emax) = self.constraints.ecl().extremes_or_defaults(Ecl::L, Ecl::H);

        // Binary search the most conservative version and ecl pair based on constraints
        let mut v = version_midpoint(vmin, vmax);
        let chosen_version = loop {
            if encoding_len(v) <= properties::num_data_bits(v, emin) {
                // This combination works, check if version can be any smaller
                if v > vmin && v < vmax {
                    vmax = v;
                } else {
                    break v;
                }
            } else {
                // This combination doesn't work, check if version can be any larger
                if v < vmax {
                    vmin = v;
                } else {
                    return Err(EncodingError::DataTooLong {
                        ver_constr: self.constraints.version().clone(),
                        ecl_constr: self.constraints.ecl().clone(),
                    });
                }
            }
            // Update version for next iteration
            let v_next = version_midpoint(vmin, vmax);
            // When vmin - vmax == 1, the midpoint is vmin because of integer divisions, resulting in an infinite loop.
            // Catch this condition which is recognizable when v is not changing anymore.
            v = if v_next != v { v_next } else { vmax };
        };
        // Now pick the highest possible ECL given the chosen version
        let encoding_len = encoding_len(chosen_version);
        let mut ecl = emin;
        let mut last_valid_ecl;
        while ecl < emax {
            last_valid_ecl = ecl;
            ecl = ecl.next();
            let available_space_with_ecl = properties::num_data_bits(chosen_version, ecl);
            if available_space_with_ecl < encoding_len {
                // The previous ecl was the limit
                return Ok((chosen_version, last_valid_ecl));
            }
        }
        // All error correction levels were valid, return the highest
        Ok((chosen_version, emax))
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

/// An encoder with a defined version and error correction level.
pub(crate) struct ConstrainedEncoder {
    settings: Settings,
}

impl ConstrainedEncoder {
    pub fn new(settings: Settings) -> Self {
        Self { settings }
    }

    /// Encode the associated data into the QR code.
    pub fn encode(self, segments: Vec<Segment>) -> Result<QrCode, EncodingError> {
        let settings = self.settings.clone();
        // Generate codewords
        let codewords = self.encode_segments(segments)?;
        // Add ECC encoding
        let blocks = ReedSolomonEncoder::new(settings.clone()).encode(codewords)?;
        // Reorder
        let mut reordered = Vec::with_capacity(properties::num_codewords(settings.version));
        for i in 0..blocks[0].data.len() {
            for block in &blocks {
                reordered.push(block.data[i])
            }
        }
        for i in 0..blocks[0].ecc.len() {
            for block in &blocks {
                reordered.push(block.ecc[i])
            }
        }
        // Build code
        let qrcode = Builder::new(settings).build(reordered)?;
        Ok(qrcode)
    }

    pub(crate) fn encode_segments(self, segments: Vec<Segment>) -> Result<Vec<u8>, EncodingError> {
        SegmentEncoder::new(self.settings).encode(segments)
    }
}

// SegmentEncoder ======================================================================================================
type Bits = BitVec<u8, Msb0>;
/// Segment encoder to convert a stream of data segments into codewords.
struct SegmentEncoder {
    settings: Settings,
    bits: Bits,
}

impl SegmentEncoder {
    pub fn new(settings: Settings) -> Self {
        Self {
            bits: Bits::with_capacity(properties::num_codewords(settings.version) * 8),
            settings,
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
        if self.data_capacity_bits() - self.bits.len() >= 4 {
            self.bits.extend_from_bitslice(bits![0, 0, 0, 0]);
        }
        // Append padding
        self.append_padding();
        // Collect codewords
        let codewords = self.bits.into_vec();
        Ok(codewords)
    }

    /// Get the data capacity in bits.
    fn data_capacity_bits(&self) -> usize {
        properties::num_data_bits(self.settings.version, self.settings.ecl)
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
        let char_count_len = properties::char_count_len(self.settings.version, segment.kind());
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
        while self.data_capacity_bits() - self.bits.len() >= 8 {
            self.bits
                .extend_from_bitslice(pad_codeword.view_bits::<Msb0>());
            pad_codeword ^= PADDING_TOGGLE_MASK;
        }
        // Append 0s until capacity is reached
        self.bits.resize(self.data_capacity_bits(), false);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bytes_data_encoding() {
        let data = "hello".as_bytes();
        let mut e = SegmentEncoder::new(Settings::new(Version::V1, Ecl::L));
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
        let s = Segment::new(data, 0..data.len(), SegmentKind::Bytes);
        let mut e = SegmentEncoder::new(Settings::new(Version::V1, Ecl::L));
        e.append_segment_header(&s);
        // ---------------------[.HEADER...|.CHARACTER COUNT.............]
        assert_eq!(e.bits, bits![0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1])
    }

    #[test]
    fn bytes_full() {
        let data = "hello".as_bytes();
        let settings = Settings::new(Version::V1, Ecl::M);
        let enc = ConstrainedEncoder::new(settings);
        let codewords = enc
            .encode_segments(vec![Segment::new(data, 0..data.len(), SegmentKind::Bytes)])
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
            (Version::V1, Ecl::H)
        );
        // Try with data that should result in the largest QR. Leave some space for the encoding headers.
        const SPACE_FOR_ENCODING_HEADERS: usize = 4;
        let largest_data_size =
            properties::num_data_codewords(Version::V40, Ecl::L) - SPACE_FOR_ENCODING_HEADERS;
        let data = vec![0; largest_data_size];
        let segments = e.segment(&data);
        assert_eq!(
            e.resolve_constraints(&segments).unwrap(),
            (Version::V40, Ecl::L)
        );
    }
}
