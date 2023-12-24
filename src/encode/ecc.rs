use reed_solomon::Encoder as RsEncoder;

use crate::{
    encode::{
        encoder::{Codewords, Settings},
        EncodingError,
    },
};

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
        crate::qrcode::properties::num_ecc_codewords_per_block(self.settings.version, self.settings.ecl)
    }

    fn num_data_codewords_per_block(&self) -> usize {
        self.num_codewords_per_block() - self.num_ecc_codewords_per_block()
    }

    fn encode(self, codewords: Codewords) -> Result<Vec<u8>, EncodingError> {
        let mut codewords: Vec<u8> = codewords.into();
        // Allocate space for ECC
        let ecc_start = codewords.len();
        let new_len = codewords.len() + self.num_blocks() * self.num_ecc_codewords_per_block();
        codewords.resize(new_len, 0);

        let encoder = RsEncoder::new(self.num_ecc_codewords_per_block());
        // Encode each block
        for i in 0..self.num_blocks() {
            println!("Block {i}");
            let range_start = i * self.num_data_codewords_per_block();
            let range_end = range_start + self.num_data_codewords_per_block();
            let data = &codewords[range_start..range_end];
            let ecc = encoder.encode(data);
            let ecc = ecc.ecc();
            for (j, byte) in ecc.iter().enumerate() {
                let index = ecc_start + i * self.num_ecc_codewords_per_block() + j;
                codewords[index] = *byte;
            }

        }
        Ok(codewords)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::encode::encoder::ConstrainedEncoder;
    use crate::Encoder;
    use crate::{Ecl, Version};

    fn codewords(data: &str, settings: Settings) -> Codewords {
        let constrained = ConstrainedEncoder::new(settings);
        constrained
            .encode_segments(Encoder::segment(data.as_bytes()))
            .unwrap()
    }

    #[test]
    fn ecc_generation() {
        let settings = Settings::new(Version::new(1).unwrap(), Ecl::L);
        let ecc = ReedSolomonEncoder::new(settings.clone());
        let result = ecc
            .encode(codewords("eren yeager", settings.clone()))
            .unwrap();
        println!("{:?}", result);
        assert_eq!(result.len(), 1);
    }
}
