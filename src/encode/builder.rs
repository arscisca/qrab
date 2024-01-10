use bitvec::prelude::*;

use super::{
    QrCode,
    Version,
    Ecl,
    qrcode::{
        module::{Matrix, Module},
        properties, Mask,
    }
};
use super::Settings;

pub(crate) struct Builder {
    settings: Settings,
    matrix: Matrix,
}

impl Builder {
    const RESERVED_MODULE: Module = Matrix::DEFAULT_MODULE_COLOR.toggled();

    /// Construct a new builder with the given `settings`.
    pub fn new(settings: Settings) -> Self {
        let size = properties::symbol_size(settings.version);
        Self {
            settings,
            matrix: Matrix::new(size),
        }
    }

    /// Transform `codewords` into light/dark modules and place them inside the final QR code.
    pub fn build(mut self, codewords: Vec<u8>) -> Result<QrCode, BuildingError> {
        self.place_codewords(codewords)?;
        // Mask
        let mask = self.pick_mask();
        self.matrix.toggle_with(mask.function());
        // Locator and timing patterns
        self.draw_patterns();
        // Format information
        let format_info = Self::compute_format_info(self.settings.ecl, mask);
        self.draw_format_info(format_info);
        // Version information
        if self.settings.version >= Version::V7 {
            let vinfo = Self::compute_version_info(self.settings.version);
            self.draw_version_info(vinfo);
        }
        // Always dark module
        self.matrix.set(4 * self.settings.version.number() as usize + 9, 8, Module::Dark);
        Ok(QrCode::new(
            self.matrix,
            self.settings.version,
            self.settings.ecl,
            mask
        ))
    }

    fn size(&self) -> usize {
        self.matrix.size()
    }

    /// Mark modules that belong to reserved areas (like locator patterns, functional info, ...) on a blank matrix.
    /// These modules will be set to `~Matrix::DEFAULT_MODULE_COLOR` for simple checking when placing the data
    /// modules.
    fn mark_reserved_areas(&mut self) {
        fn mark_reserved_rect(builder: &mut Builder, i: usize, j: usize, height: usize, width: usize) {
            builder.fill_rect(Builder::RESERVED_MODULE, i, j, height, width);
        }

        let edge = self.size() - 1;
        // Timing patterns
        const TIMING_PATTERN_POS: usize = 6;
        mark_reserved_rect(self, TIMING_PATTERN_POS, 0, 1, self.size());
        mark_reserved_rect(self, 0, TIMING_PATTERN_POS, self.size(), 1);

        // Locator patterns
        const LOCATOR_PATTERN_SIZE: usize = 1 + 1 + 3 + 1 + 1;
        const LOCATOR_SPACING_SIZE: usize = 1;
        const LOCATOR_TOT_SIZE: usize = LOCATOR_PATTERN_SIZE + LOCATOR_SPACING_SIZE;
        mark_reserved_rect(self, 0, 0, LOCATOR_TOT_SIZE, LOCATOR_TOT_SIZE);
        mark_reserved_rect(self, self.size() - LOCATOR_TOT_SIZE, 0, LOCATOR_TOT_SIZE, LOCATOR_TOT_SIZE);
        mark_reserved_rect(self, 0, self.size() - LOCATOR_TOT_SIZE, LOCATOR_TOT_SIZE, LOCATOR_TOT_SIZE);

        // Alignment patterns
        for (i, j) in properties::alignment_pattern_coordinates(self.settings.version) {
            mark_reserved_rect(self, i, j, 5, 5);
        }

        // Format information (includes the always dark module)
        const FINFO_OFFSET: usize = LOCATOR_TOT_SIZE;
        mark_reserved_rect(self, FINFO_OFFSET, 0, 1, 9);
        mark_reserved_rect(self, FINFO_OFFSET, edge - 7, 1, 8);
        mark_reserved_rect(self, 0, FINFO_OFFSET, 8, 1);
        mark_reserved_rect(self, edge - 7, FINFO_OFFSET, 8, 1);

        // Version information
        if self.settings.version >= Version::V7 {
            const VINFO_WIDTH: usize = 3;
            const VINFO_HEIGHT: usize = 6;
            mark_reserved_rect(self, 0, self.size() - LOCATOR_TOT_SIZE - VINFO_WIDTH, VINFO_HEIGHT, VINFO_WIDTH);
            mark_reserved_rect(self, self.size() - LOCATOR_TOT_SIZE - VINFO_WIDTH, 0, VINFO_WIDTH, VINFO_HEIGHT);
        }
    }

    fn place_codewords(&mut self, codewords: Vec<u8>) -> Result<(), BuildingError> {
        // Mark reserved areas
        self.mark_reserved_areas();
        // Iterate on bits and place corresponding modules on nonreserved indices
        let bv: BitVec<u8, Msb0> = BitVec::from_vec(codewords);
        let total = bv.len();
        let mut indices = crate::qrcode::module::IndexSequenceIter::new(self.size());
        let mut placed = 0;
        for bit in bv {
            // Get the next non-reserved index
            let next_unreserved_index = indices.find(|(i, j)| self.matrix[(*i, *j)] != Self::RESERVED_MODULE);
            let Some(index) = next_unreserved_index else {
                break
            };
            self.matrix.set(index.0, index.1, bit.into());
            placed += 1;
        }
        if placed != total {
            return Err(BuildingError::CouldntPlaceAllModules { placed, total });
        }
        Ok(())
    }

    fn pick_mask(&self) -> Mask {
        // TODO: Actually choose
        Mask::M011
    }

    /// Draw the symbol patterns (locator, timing, info).
    fn draw_patterns(&mut self) {
        self.draw_alignment_patterns();
        self.draw_locator_patterns();
        self.draw_timing_patterns();
    }

    /// Fill a rectangle with its top-left corner in `(i0, j0)` and specified `height` and `width`.
    fn fill_rect(&mut self, module: Module, i0: usize, j0: usize, height: usize, width: usize) {
        for i in i0..(i0 + height) {
            for j in j0..(j0 + width) {
                self.matrix.set(i, j, module);
            }
        }
    }

    /// Fill a square with its top-left corner in `(i0, j0)` and specified `size`.
    fn fill_square(&mut self, module: Module, i0: usize, j0: usize, size: usize) {
        self.fill_rect(module, i0, j0, size, size);
    }

    /// Draw the locator patterns.
    fn draw_locator_patterns(&mut self) {
        fn draw_locator_pattern(builder: &mut Builder, i: usize, j: usize) {
            // Outer, ring, center
            builder.fill_square(Module::Dark, i, j, 7);
            builder.fill_square(Module::Light, i + 1, j + 1, 5);
            builder.fill_square(Module::Dark, i + 2, j + 2, 3);
        }
        // Fill with white first for spacing: top-left, top-right, bottom-left
        self.fill_square(Module::Light, 0, 0, 8);
        self.fill_square(Module::Light, 0, self.matrix.size() - 8, 8);
        self.fill_square(Module::Light, self.matrix.size() - 8, 0, 8);
        // Top-left, top-right, bottom-left
        draw_locator_pattern(self, 0, 0);
        draw_locator_pattern(self, 0, self.matrix.size() - 7);
        draw_locator_pattern(self, self.matrix.size() - 7, 0);
    }

    fn draw_alignment_patterns(&mut self) {
        fn draw_alignment_pattern(builder: &mut Builder, i: usize, j: usize) {
            // Outer, ring, center
            builder.fill_square(Module::Dark, i, j, 5);
            builder.fill_square(Module::Light, i + 1, j + 1, 3);
            builder.matrix.set(i + 2, j + 2, Module::Dark);
        }

        for (i, j) in properties::alignment_pattern_coordinates(self.settings.version) {
            draw_alignment_pattern(self, i, j);
        }
    }

    fn draw_timing_patterns(&mut self) {
        let mut current = Module::Dark;
        for k in 8..(self.size() - 8) {
            self.matrix.set(6, k, current);
            self.matrix.set(k, 6, current);
            current = current.toggled();
        }
    }

    fn compute_format_info(ecl: Ecl, mask: Mask) -> u16 {
        const MASK: u16 = 0b101010000010010;
        // TODO: Compute ECC rather than doing this :)
        let ecl_code = ecl.code() as u16;
        let mask_code = mask.code() as u16;
        let code = ecl_code << 3 | mask_code;
        let ecc = match code {
            0b00000 => 0b0000000000,
            0b00001 => 0b0100110111,
            0b00010 => 0b1001101110,
            0b00011 => 0b1101011001,
            0b00100 => 0b0111101011,
            0b00101 => 0b0011011100,
            0b00110 => 0b1110000101,
            0b00111 => 0b1010110010,
            0b01000 => 0b1111010110,
            0b01001 => 0b1011100001,
            0b01010 => 0b0110111000,
            0b01011 => 0b0010001111,
            0b01100 => 0b1000111101,
            0b01101 => 0b1100001010,
            0b01110 => 0b0001010011,
            0b01111 => 0b0101100100,
            0b10000 => 0b1010011011,
            0b10001 => 0b1110101100,
            0b10010 => 0b0011110101,
            0b10011 => 0b0111000010,
            0b10100 => 0b1101110000,
            0b10101 => 0b1001000111,
            0b10110 => 0b0100011110,
            0b10111 => 0b0000101001,
            0b11000 => 0b0101001101,
            0b11001 => 0b0001111010,
            0b11010 => 0b1100100011,
            0b11011 => 0b1000010100,
            0b11100 => 0b0010100110,
            0b11101 => 0b0110010001,
            0b11110 => 0b1011001000,
            0b11111 => 0b1111111111,
            p => unreachable!("invalid functional pattern {:b}", p),
        };
        let pattern: u16 = (code << 10) | ecc;
        pattern ^ MASK
    }

    fn compute_version_info(version: Version) -> u32 {
        match version {
            Version::V7 => 0x07C94,
            Version::V8 => 0x085BC,
            Version::V9 => 0x09A99,
            Version::V10 => 0x0A4D3,
            Version::V11 => 0x0BBF6,
            Version::V12 => 0x0C762,
            Version::V13 => 0x0D847,
            Version::V14 => 0x0E60D,
            Version::V15 => 0x0F928,
            Version::V16 => 0x10B78,
            Version::V17 => 0x1145D,
            Version::V18 => 0x12A17,
            Version::V19 => 0x13532,
            Version::V20 => 0x149A6,
            Version::V21 => 0x15683,
            Version::V22 => 0x168C9,
            Version::V23 => 0x177EC,
            Version::V24 => 0x18EC4,
            Version::V25 => 0x191E1,
            Version::V26 => 0x1AFAB,
            Version::V27 => 0x1B08E,
            Version::V28 => 0x1CC1A,
            Version::V29 => 0x1D33F,
            Version::V30 => 0x1ED75,
            Version::V31 => 0x1F250,
            Version::V32 => 0x209D5,
            Version::V33 => 0x216F0,
            Version::V34 => 0x228BA,
            Version::V35 => 0x2379F,
            Version::V36 => 0x24B0B,
            Version::V37 => 0x2542E,
            Version::V38 => 0x26A64,
            Version::V39 => 0x27541,
            Version::V40 => 0x28C69,
            _ => 0x0,
        }
    }

    fn draw_format_info(&mut self, pattern: u16) {
        const FINFO_OFFSET: usize = 8;
        let edge = self.size() - 1;

        let bits: &BitSlice<u16, Lsb0> = &pattern.view_bits()[0..=15];
        // Draw first 7 bits from the top and from the right
        for (k, bit) in bits[0..=7].iter().enumerate() {
            let module = Module::from(*bit);
            // Vertical line
            let ver_offset = if k <= 5 { 0 } else { 1 };
            self.matrix.set(k + ver_offset, FINFO_OFFSET, module);
            // Horizontal line
            self.matrix.set(FINFO_OFFSET, edge - k, module);
        }
        // Draw remaining from the bottom and from the left
        for (k, bit) in bits[8..=14].iter().enumerate() {
            let module = Module::from(*bit);
            // Vertical line
            let i_ver = edge - 6 + k;
            self.matrix.set(i_ver, FINFO_OFFSET, module);
            // Horizontal line
            let hor_offset = if k == 0 { 1 } else { 0 };
            let j_hor = 6 - k + hor_offset;
            self.matrix.set(FINFO_OFFSET, j_hor, module);
        }
    }

    fn draw_version_info(&mut self, vinfo: u32) {
        let bits: &BitSlice<u32, Lsb0> = &vinfo.view_bits()[0..18];
        let tr_base = (0, self.size() - 8 - 3);
        let bl_base = (self.size() - 8 - 3, 0);
        for (i, bit) in bits.iter().enumerate() {
            let module = Module::from(*bit);
            self.matrix.set(tr_base.0 + i / 3, tr_base.1 + i % 3, module);
            self.matrix.set(bl_base.0 + i % 3, bl_base.1 + i / 3, module);
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum BuildingError {
    #[error("could not place all modules: out of {total}, found space for only {placed}")]
    CouldntPlaceAllModules { placed: usize, total: usize },
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{Constraint, Ecl, Encoder, Version};

    #[test]
    fn functional_info_generation() {
        assert_eq!(
            Builder::compute_format_info(Ecl::L, Mask::M101),
            0b110001100011000
        );
        assert_eq!(
            Builder::compute_format_info(Ecl::M, Mask::M010),
            0b101111001111100
        );
        // TODO: Extend this test with other cases where ecl and mask are not palindromes.
        // I had issues with the order of the bits in the pattern computation because it was unclear in the standard,
        // the fact that the provided example uses palindrome data doesn't help.
    }

    #[test]
    fn building() {
        let qrcode = Encoder::with_constraint(Constraint::VersionAndEcl(Version::V5, Ecl::M))
            .encode("hello, this is a very long string. Can it fit?")
            .unwrap();
        let mut ascii = String::new();
        qrcode.render_ascii(&mut ascii).unwrap();
        println!("{}", ascii);
    }
}
