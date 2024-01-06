use reed_solomon::Encoder as RsEncoder;

use crate::encode::{
    encoder::{Codewords, Settings},
    EncodingError,
};

pub struct Block {
    pub data: Vec<u8>,
    pub ecc: Vec<u8>,
}

pub struct ReedSolomonEncoder {
    settings: Settings,
}

impl ReedSolomonEncoder {
    /// Create a new reed-solomon encoder based on the settings.
    pub fn new(settings: Settings) -> Self {
        Self { settings }
    }

    /// Get the number of error correction blocks.
    fn num_blocks(&self) -> usize {
        crate::qrcode::properties::num_ecc_blocks(self.settings.version, self.settings.ecl)
    }

    fn num_codewords_per_block(&self) -> usize {
        crate::qrcode::properties::num_codewords(self.settings.version) / self.num_blocks()
    }

    fn num_ecc_codewords_per_block(&self) -> usize {
        crate::qrcode::properties::num_ecc_codewords_per_block(
            self.settings.version,
            self.settings.ecl,
        )
    }

    fn num_data_codewords_per_block(&self) -> usize {
        self.num_codewords_per_block() - self.num_ecc_codewords_per_block()
    }

    pub fn encode(self, codewords: Codewords) -> Result<Vec<Block>, EncodingError> {
        let codewords: Vec<u8> = codewords.into();

        // Encode each block
        let encoder = RsEncoder::new(self.num_ecc_codewords_per_block());
        let mut blocks = Vec::with_capacity(self.num_blocks());
        for i in 0..self.num_blocks() {
            let range_start = i * self.num_data_codewords_per_block();
            let range_end = range_start + self.num_data_codewords_per_block();
            let data = &codewords[range_start..range_end];
            let ecc = encoder.encode(data);
            let block = Block {
                data: ecc.data().to_vec(),
                ecc: ecc.ecc().to_vec(),
            };
            blocks.push(block);
        }
        Ok(blocks)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{encode::encoder::ConstrainedEncoder, Ecl, Encoder, Version};

    fn codewords(data: &str, settings: Settings) -> Codewords {
        let constrained = ConstrainedEncoder::new(settings);
        constrained
            .encode_segments(Encoder::segment(data.as_bytes()))
            .unwrap()
    }

    #[test]
    fn ecc_generation() {
        let settings = Settings::new(Version::V1, Ecl::M);
        let ecc = ReedSolomonEncoder::new(settings.clone());
        let blocks = ecc
            .encode(codewords("eren yeager", settings.clone()))
            .unwrap();
        // 1M codes have a single block
        assert_eq!(blocks.len(), 1);
        assert_eq!(
            &blocks[0].ecc,
            &vec![0xAE, 0xA5, 0x16, 0x17, 0xA0, 0x1E, 0x57, 0xD1, 0x51, 0x91]
        );
    }
}
