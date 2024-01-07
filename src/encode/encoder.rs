use bitvec::prelude::*;

use super::{
    segment::{Segment, SegmentKind},
    Builder, EncodingError,
};
use crate::encode::ecc::ReedSolomonEncoder;
use crate::{qrcode::properties, Ecl, QrCode, Version};
use std::ops::BitXorAssign;

/// Encoding constraint that determines what to prioritize when encoding the QR symbol.
pub enum Constraint {
    /// Find the smallest size that fits the data, then the highest error correction level.
    SmallestSize,
    /// Use the specified version.
    Version(Version),
    /// Use the specified error correction level.
    Ecl(Ecl),
    /// Use the specified version and error correction level.
    VersionAndEcl(Version, Ecl),
}

/// Settings for an encoder.
#[derive(Clone)]
pub(crate) struct Settings {
    pub version: Version,
    pub ecl: Ecl,
}

impl Settings {
    /// Initialize with the specified `version` and `ecl`.
    pub fn new(version: Version, ecl: Ecl) -> Self {
        Self { version, ecl }
    }
}

impl std::fmt::Debug for Settings {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}-{}", self.version, self.ecl)
    }
}

/// A QR encoder with optional constraints on QR code error correction level and version.
/// # Examples
/// ## Basic usage
/// By default, an `Encoder` will generate the smallest QR code possible to fit the provided data.
pub struct Encoder {
    constraint: Constraint,
}

impl Encoder {
    /// Create a new encoder with the default constraint that minimizes the output QR code's size.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new encoder with a specific constraint.
    pub fn with_constraint(constraint: Constraint) -> Self {
        Self { constraint }
    }

    /// Encode `data`. It returns an error if the input data cannot be encoded with the given constraints - which could
    /// happen even with the default constraints e.g. when the data is just too big for any QR code.
    pub fn encode<T: AsRef<[u8]>>(self, data: T) -> Result<QrCode, EncodingError> {
        let data = data.as_ref();
        let segments = Self::segment(data);
        let constrained = self.resolve_constraints(data)?;
        constrained.encode(segments)
    }

    /// Resolve the constraints based on the passed `data` and its `segments`.
    pub(crate) fn resolve_constraints(
        self,
        data: &[u8],
    ) -> Result<ConstrainedEncoder, EncodingError> {
        match self.constraint {
            Constraint::SmallestSize => {
                todo!("Automatic version and ecl")
            }
            Constraint::Version(version) => {
                todo!("Automatic ecl")
            }
            Constraint::Ecl(ecl) => {
                todo!("Automatic data")
            }
            Constraint::VersionAndEcl(version, ecl) => {
                let settings = Self::validate_settings(data, Settings::new(version, ecl))?;
                Ok(ConstrainedEncoder::new(settings))
            }
        }
    }

    pub(crate) fn segment(data: &[u8]) -> Vec<Segment> {
        // TODO: Actually analyze data to perform the best segmentation.
        let segment = Segment::new(data, 0..data.len(), SegmentKind::Bytes);
        vec![segment]
    }

    fn validate_settings(data: &[u8], settings: Settings) -> Result<Settings, EncodingError> {
        // TODO: This should depend on the codewords -not raw data- length, since compression will happen.
        if data.len() > properties::num_data_bits(settings.version, settings.ecl) / 8 {
            return Err(EncodingError::DataTooLarge {
                version: settings.version,
                ecl: settings.ecl,
            });
        }
        Ok(settings)
    }
}

impl Default for Encoder {
    fn default() -> Self {
        Self::with_constraint(Constraint::SmallestSize)
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

    pub(crate) fn encode_segments(
        self,
        segments: Vec<Segment>,
    ) -> Result<Codewords, EncodingError> {
        SegmentEncoder::new(self.settings).encode(segments)
    }
}

// SegmentEncoder ======================================================================================================
type Bits = BitVec<u8, Msb0>;

pub(crate) struct Codewords(Vec<u8>);
impl From<Codewords> for Vec<u8> {
    fn from(value: Codewords) -> Self {
        value.0
    }
}

/// Segment encoder to convert a stream of data segments into codewords.
struct SegmentEncoder {
    settings: Settings,
    bits: Bits,
}

impl SegmentEncoder {
    pub fn new(settings: Settings) -> Self {
        Self {
            bits: Bits::with_capacity(properties::num_data_bits(settings.version, settings.ecl)),
            settings,
        }
    }

    /// Encode the associated data.
    pub fn encode(mut self, segments: Vec<Segment>) -> Result<Codewords, EncodingError> {
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
        Ok(Codewords(codewords))
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
            pad_codeword.bitxor_assign(PADDING_TOGGLE_MASK);
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
            .unwrap()
            .0;
        assert_eq!(
            &codewords,
            &vec![
                0x40, 0x56, 0x86, 0x56, 0xC6, 0xC6, 0xF0, 0xEC, 0x11, 0xEC, 0x11, 0xEC, 0x11, 0xEC,
                0x11, 0xEC
            ]
        );
    }
}
