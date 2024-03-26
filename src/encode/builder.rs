use bitvec::{
    prelude::{BitSlice, BitVec, Lsb0, Msb0},
    view::BitView,
};

use itertools::Itertools;

use super::{Ecl, EncodingConstraints, EncodingError, Mask, MaskTable, QrCode, QrInfo, Version};
use crate::qrcode::MatrixDataSlice;
use crate::{Matrix, Module};

pub(crate) struct Builder<'a> {
    info: &'a QrInfo,
    masks: &'a MaskTable<bool>,
    matrix: Matrix<Module>,
}

impl<'a> Builder<'a> {
    /// Construct a new builder.
    pub fn new(info: &'a QrInfo, constraints: &'a EncodingConstraints) -> Self {
        Self {
            info,
            masks: constraints.masks(),
            matrix: Matrix::filled(Module::Light, info.symbol_size()),
        }
    }

    /// Transform `codewords` into light/dark modules and place them inside the final QR code.
    pub fn build(mut self, codewords: Vec<u8>) -> Result<QrCode, EncodingError> {
        self.place_codewords(codewords);
        // Mask
        let mask = self.pick_mask()?;
        self.matrix.mask(mask);
        // Locator and timing patterns
        self.draw_patterns();
        // Format information
        let format_info = Self::compute_format_info(self.info.ecl(), mask);
        self.draw_format_info(format_info);
        // Version information
        if self.info.version() >= Version::V7 {
            let vinfo = Self::compute_version_info(self.info.version());
            self.draw_version_info(vinfo);
        }
        // Always dark module
        self.matrix.set(
            4 * self.info.version().number() as usize + 9,
            8,
            Module::Dark,
        );
        Ok(QrCode::new(
            self.matrix,
            self.info.version(),
            self.info.ecl(),
            mask,
        ))
    }

    fn size(&self) -> usize {
        self.matrix.size()
    }

    /// Mark modules that belong to reserved areas (like locator patterns, functional info, ...) on a blank matrix.
    /// These modules will be set to `~Matrix::DEFAULT_MODULE_COLOR` for simple checking when placing the data
    /// modules.

    fn place_codewords(&mut self, codewords: Vec<u8>) {
        // Map reserved areas.
        let reserved = self.info.map_reserved_areas();
        // Iterate on bits and place corresponding modules on nonreserved indices.
        let bv: BitVec<u8, Msb0> = BitVec::from_vec(codewords);
        let mut indices = crate::qrcode::IndexSequenceIter::new(self.size());
        for bit in bv {
            // Get the next non-reserved index
            let next_unreserved_index = indices.find(|&(i, j)| !reserved.get(i, j).unwrap());
            let Some(index) = next_unreserved_index else {
                break;
            };
            self.matrix.set(index.0, index.1, bit.into());
        }
    }

    fn pick_mask(&self) -> Result<Mask, EncodingError> {
        // Score available masks and choose the one with the smallest penalty.
        let evaluator = MaskEvaluator::new(self.masks);
        let penalties = evaluator.score_masks(self.matrix.clone());
        let chosen_code_penalty = penalties
            .into_iter()
            .enumerate()
            .filter(|(_, penalty)| penalty.is_some())
            .min_by(|m1, m2| m1.1.cmp(&m2.1))
            .ok_or(EncodingError::NoAvailableMasks)?;
        Ok(
            Mask::try_from(chosen_code_penalty.0 as u8)
                .expect("chosen code should be a valid code"),
        )
    }

    /// Draw the symbol patterns (locator, timing, info).
    fn draw_patterns(&mut self) {
        self.draw_alignment_patterns();
        self.draw_locator_patterns();
        self.draw_timing_patterns();
    }

    /// Fill a rectangle with its top-left corner in `(i0, j0)` and specified `height` and `width`.
    fn fill_rect(&mut self, module: Module, i0: usize, j0: usize, height: usize, width: usize) {
        self.matrix.fill_rect(module, i0, j0, height, width)
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

        for (i, j) in self.info.alignment_pattern_coordinates() {
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
            self.matrix
                .set(tr_base.0 + i / 3, tr_base.1 + i % 3, module);
            self.matrix
                .set(bl_base.0 + i % 3, bl_base.1 + i / 3, module);
        }
    }
}

/// Penalty calculator for all the available masks that can be applied to a matrix.
struct MaskEvaluator<'m> {
    mask_enables: &'m MaskTable<bool>,
}

impl<'m> MaskEvaluator<'m> {
    const PENALTY_LINE_SAME_COLOR: u32 = 3;
    const PENALTY_BLOCK_SAME_COLOR: u32 = 3;
    const PENALTY_LOCATOR_PATTERN: u32 = 40;
    const PENALTY_PROPORTION: u32 = 10;

    /// Create a new evaluator with the selected `mask_enables`, where `mask_enables[i] == true` if
    /// the mask with code `i` should be enabled.
    pub fn new(mask_enables: &'m MaskTable<bool>) -> Self {
        Self { mask_enables }
    }

    fn groups_in_row(row: &MatrixDataSlice<Module>) -> Vec<(Module, usize)> {
        row.iter()
            .group_by(|module| *module)
            .into_iter()
            .map(|(module, group)| (module, group.count()))
            .collect()
    }

    /// Assign a penalty score to each active mask.
    pub fn score_masks(self, mut matrix: Matrix<Module>) -> MaskTable<Option<u32>> {
        let mut scores = MaskTable::filled(None);
        for (mask, score) in scores.iter_mut() {
            if !self.mask_enables[mask] {
                continue;
            }
            matrix.mask(mask);
            let mut new_score = 0;
            // Score penalties that are not defined by rows and columns.
            new_score += self.score_proportions(&matrix);
            new_score += self.score_blocks(&matrix);
            // Score row/column penalties by looking at rows first, then transposing to look at columns.
            for i in 0..2 {
                new_score += self.score_same_color_groups(&matrix);
                new_score += self.score_locators(&matrix);
                if i == 0 {
                    matrix.transpose();
                }
            }
            *score = Some(new_score);
        }
        scores
    }

    /// Update score based on contiguous groups of the same module color in rows and columns.
    fn score_same_color_groups(&self, matrix: &Matrix<Module>) -> u32 {
        let mut score = 0;
        for row in matrix.rows() {
            for (_, group_len) in Self::groups_in_row(&row) {
                if group_len >= 5 {
                    score += MaskEvaluator::PENALTY_LINE_SAME_COLOR + group_len as u32 - 5;
                }
            }
        }
        score
    }

    /// Update scores based on the proportion of dark to light modules.
    fn score_proportions(&self, matrix: &Matrix<Module>) -> u32 {
        let tot_modules = (matrix.size() * matrix.size()) as u32;
        let percentage_dark = (matrix.count(Module::Dark) as u32 * 100) / tot_modules;
        let dist_from_50 = percentage_dark.abs_diff(50);
        // Add Self::PENALTY_PROPORTION for every 5% away from 50%. The `saturating_sub` is because 55% should still add
        // no penalty, whereas 56% should start adding it.
        Self::PENALTY_PROPORTION * (dist_from_50.saturating_sub(1) / 5)
    }

    fn score_blocks(&self, matrix: &Matrix<Module>) -> u32 {
        let mut score = 0;
        for i in 0..(matrix.size() - 1) {
            for j in 0..(matrix.size() - 1) {
                let module = matrix.get(i, j);
                if module == matrix.get(i, j + 1)
                    && module == matrix.get(i + 1, j)
                    && module == matrix.get(i + 1, j + 1)
                {
                    score += Self::PENALTY_BLOCK_SAME_COLOR;
                }
            }
        }
        score
    }

    fn score_locators(&self, matrix: &Matrix<Module>) -> u32 {
        const PATTERN_LEN: usize = 5;
        const PATTERN: [usize; PATTERN_LEN] = [1, 1, 3, 1, 1];
        const FULL_WINDOW_LEN: usize = PATTERN_LEN + 1;
        const MIN_SPACER_LEN: usize = 5;
        const SPACER_COLOR: Module = Module::Light;
        let mut score = 0;
        for row in matrix.rows() {
            // Collapse row into color groups.
            let groups = Self::groups_in_row(&row);
            // Look for a locator pattern's proportions preceded or followed by 4 light modules.
            for window in groups.windows(FULL_WINDOW_LEN) {
                let (first, last) = (window[0], window[FULL_WINDOW_LEN - 1]);
                // Check whether the spacer is the first or the last element of the window.
                let candidate_pattern = if first.0 == SPACER_COLOR && first.1 >= MIN_SPACER_LEN {
                    // Spacer is at the start.
                    &window[1..]
                } else if last.0 == SPACER_COLOR && last.1 >= MIN_SPACER_LEN {
                    // Spacer is at the end.
                    &window[..(FULL_WINDOW_LEN - 2)]
                } else {
                    // There is no spacer, no penalty.
                    continue;
                };
                // Compute the ratios of the candidate pattern. Note that the colors of the candidate pattern don't
                // matter because we already grouped by color (ensuring they are alternating) and extracted the light
                // spacer.
                let reference_len = candidate_pattern[0].1;
                let candidate_pattern_ratios =
                    candidate_pattern.iter().map(|(_, len)| len / reference_len);
                // Check whether the candidate pattern respects an actual pattern's ratios.
                let is_pattern = candidate_pattern_ratios
                    .zip(PATTERN.iter())
                    .all(|(candidate_ratio, ref_ratio)| candidate_ratio == *ref_ratio);
                if is_pattern {
                    score += Self::PENALTY_LOCATOR_PATTERN;
                }
            }
        }
        score
    }
}

#[cfg(test)]
mod test {
    use super::*;

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
}
