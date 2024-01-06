use crate::QrCode;
use bitvec::prelude::*;

use super::encoder::Settings;
use crate::qrcode::{
    ecl::Ecl,
    module::{Matrix, Module},
    properties, Mask,
};

pub(crate) struct Builder {
    settings: Settings,
    size: usize,
    matrix: Matrix,
}

impl Builder {
    /// Construct a new builder with the given `settings`.
    pub fn new(settings: Settings) -> Self {
        let size = properties::symbol_size(settings.version);
        Self {
            settings,
            size,
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
        if self.settings.version >= crate::Version::V7 {
            todo!("Version information")
        }
        // Always dark module
        self.matrix[(4 * self.settings.version.number() as usize + 9, 8)] = Module::Dark;
        Ok(QrCode::new(
            self.matrix,
            self.settings.version,
            self.settings.ecl,
        ))
    }

    // Function to support debugging
    // TODO: Remove this.
    #[cfg(test)]
    pub fn print_reserved(&self) {
        println!("{}", "██".repeat(self.size + 2));
        for i in 0..self.size {
            print!("██");
            for j in 0..self.size {
                let output = match self.is_reserved(i, j) {
                    true => "##",
                    false => "  ",
                };
                print!("{}", output);
            }
            println!("██");
        }
        println!("{}", "██".repeat(self.size + 2))
    }

    fn place_codewords(&mut self, codewords: Vec<u8>) -> Result<(), BuildingError> {
        // Create a temporary matrix to prevent `self` borrowing issues
        let mut m = std::mem::take(&mut self.matrix);
        // Place the codewords bit by bit based on the available indices
        let bv: BitVec<u8, Msb0> = BitVec::from_vec(codewords);
        let total = bv.len();
        let mut placed = 0;
        for (index, bit) in std::iter::zip(self.indices(), bv) {
            m[index] = Module::from(bit);
            placed += 1;
        }
        self.matrix = std::mem::take(&mut m);
        if placed != total {
            return Err(BuildingError::CouldntPlaceAllModules { placed, total });
        }
        Ok(())
    }

    /// Check whether an (`i`, `j`) index points inside a reserved area of the symbol.
    fn is_reserved(&self, i: usize, j: usize) -> bool {
        let edge = self.size - 1;
        // Timing patterns
        const TIMING_PATTERN_POS: usize = 6;
        if i == TIMING_PATTERN_POS || j == TIMING_PATTERN_POS {
            return true;
        }
        // Locator patterns
        const LOCATOR_PATTERN_SIZE: usize = 1 + 1 + 3 + 1 + 1;
        const LOCATOR_SPACING_SIZE: usize = 1;
        const LOCATOR_TOT_SIZE: usize = LOCATOR_PATTERN_SIZE + LOCATOR_SPACING_SIZE;
        if i < LOCATOR_TOT_SIZE {
            // Top left and top right
            if j < LOCATOR_TOT_SIZE || j > edge - LOCATOR_TOT_SIZE {
                return true;
            }
        } else if i > edge - LOCATOR_TOT_SIZE {
            // Bottom left
            if j < LOCATOR_TOT_SIZE {
                return true;
            }
        }
        // Format information (includes the always dark module)
        const FINFO_OFFSET: usize = LOCATOR_TOT_SIZE;
        if j == FINFO_OFFSET && (i <= 7 || i >= edge - 7) {
            // Vertical column
            return true;
        } else if i == FINFO_OFFSET && (j <= 8 || j >= edge - 7) {
            // Horizontal row
            return true;
        }
        if self.settings.version >= crate::Version::V7 {
            todo!("Version information areas")
        }
        false
    }

    /// Get an iterator over the valid indices where modules can be placed
    fn indices(&self) -> impl Iterator<Item = (usize, usize)> + '_ {
        let all = crate::qrcode::module::IndexSequenceIter::new(self.size);
        all.filter(|(i, j)| !self.is_reserved(*i, *j))
    }

    fn pick_mask(&self) -> Mask {
        // TODO: Actually choose
        Mask::M010
    }

    /// Draw the symbol patterns (locator, timing, info).
    fn draw_patterns(&mut self) {
        self.draw_locator_patterns();
        self.draw_timing_patterns();
    }

    fn fill_square(&mut self, module: Module, i0: usize, j0: usize, size: usize) {
        for i in i0..(i0 + size) {
            for j in j0..(j0 + size) {
                self.matrix[(i, j)] = module;
            }
        }
    }

    fn draw_locator_patterns(&mut self) {
        fn draw_big_pattern(builder: &mut Builder, i: usize, j: usize) {
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
        draw_big_pattern(self, 0, 0);
        draw_big_pattern(self, 0, self.matrix.size() - 7);
        draw_big_pattern(self, self.matrix.size() - 7, 0);
        // TODO: Smaller locator patterns for larger versions
    }

    fn draw_timing_patterns(&mut self) {
        let mut current = Module::Dark;
        for k in 8..(self.size - 8) {
            self.matrix[(6, k)] = current;
            self.matrix[(k, 6)] = current;
            current.toggle();
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
        let masked = pattern ^ MASK;
        masked
    }

    fn draw_format_info(&mut self, pattern: u16) {
        const FINFO_OFFSET: usize = 8;
        let edge = self.size - 1;

        let bits: &BitSlice<u16, Lsb0> = &pattern.view_bits()[0..=15];
        // Draw first 7 bits from the top and from the right
        for (k, bit) in bits[0..=7].iter().enumerate() {
            let module = Module::from(*bit);
            // Vertical line
            let ver_offset = if k <= 5 { 0 } else { 1 };
            self.matrix[(k + ver_offset, FINFO_OFFSET)] = module;
            // Horizontal line
            self.matrix[(FINFO_OFFSET, edge - k)] = module;
        }
        // Draw remaining from the bottom and from the left
        for (k, bit) in bits[8..=14].iter().enumerate() {
            let module = Module::from(*bit);
            // Vertical line
            let i_ver = edge - 6 + k;
            self.matrix[(i_ver, FINFO_OFFSET)] = module;
            // Horizontal line
            let hor_offset = if k == 0 { 1 } else { 0 };
            let j_hor = 6 - k + hor_offset;
            self.matrix[(FINFO_OFFSET, j_hor)] = module;
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
        let qrcode = Encoder::with_constraint(Constraint::VersionAndEcl(Version::V1, Ecl::M))
            .encode("hello")
            .unwrap();
        let mut ascii = String::new();
        qrcode.render_ascii(&mut ascii).unwrap();
        println!("{}", ascii);
    }
}
