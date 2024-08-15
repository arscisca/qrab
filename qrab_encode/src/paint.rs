use bitvec::{order::{Lsb0, Msb0}, vec::BitVec, slice::BitSlice, view::BitView};

use qrab_core::qrstandard;
use qrab_core::{Ecl, Mask, Meta, Version};
use qrab_core::{Canvas, Module, ReservedAreaAtlas};

pub struct Painter {
    meta: Meta,
    canvas: Canvas,
}

impl Painter {
    /// Create a new [Painter].
    pub fn new(meta: &Meta) -> Self {
        Self {
            meta: meta.clone(),
            canvas: Canvas::filled(qrstandard::canvas_size(meta.version), Module::Dark),
        }
    }

    /// Paint a [Canvas] according to the QR code standards, filling it with `codewords` using the
    /// relevant format.
    pub fn paint(mut self, codewords: Vec<u8>) -> Canvas {
        // In a preliminary phase, paint data and choose the mask based on how the canvas will like.
        self.paint_codewords(codewords);
        self.meta.mask = self.choose_mask();
        self.apply_mask(self.meta.mask);
        // Now the canvas' data section is finalized and we can freely paint the rest.
        self.paint_patterns();
        let format_info = format_info(self.meta.ecl, self.meta.mask);
        self.paint_format_info(format_info);
        if self.meta.version >= Version::V07 {
            self.paint_version_info();
        }
        self.paint_always_dark_module();
        self.canvas
    }

    /// Place the codewords on the canvas avoiding the reserved areas like locator patterns,
    /// format information and so on.
    fn paint_codewords(&mut self, codewords: Vec<u8>) {
        let atlas = ReservedAreaAtlas::new(self.meta.version);
        let size = self.canvas.size();
        let mut indices = (0..(size*size)).map(|i| qrab_core::canvas_position(i, size));
        let bits: BitVec<u8, Msb0> = BitVec::from_vec(codewords);
        for bit in bits.into_iter() {
            let Some((i, j)) = indices.find(|&(i, j)| !atlas.is_reserved(i, j)) else {
                unreachable!("data size should match the amount of non-reserved indices")
            };
            self.canvas.set(i, j, bit.into());
        }
    }

    fn choose_mask(&self) -> Mask {
        // TODO: Actually choose the best mask
        Mask::M001
    }

    fn apply_mask(&mut self, mask: Mask) {
        let function = mask.function();
        self.canvas.toggle(function);
    }

    #[rustfmt::ignore]
    fn paint_patterns(&mut self) {
        let size = self.canvas.size();
        if self.meta.version > Version::V01 {
            todo!("alignment patterns")
        }
        // Locator patterns.
        self.paint_square(Module::Light,        0,        0, 8);
        self.paint_square(Module::Light,        0, size - 8, 8);
        self.paint_square(Module::Light, size - 8,        0, 8);
        for (i, j) in [(0, 0), (0, size - 7), (size - 7, 0)] {
            self.paint_square(Module::Dark,  i + 0, j + 0, 7);
            self.paint_square(Module::Light, i + 1, j + 1, 5);
            self.paint_square(Module::Dark,  i + 2, j + 2, 3);
        }
        // Timing patterns.
        let mut module = Module::Dark;
        for k in 8..(size - 8) {
            self.canvas.set(6, k, module);
            self.canvas.set(k, 6, module);
            module = module.inverted();
        }
    }

    fn paint_format_info(&mut self, format_info: u16) {
        const FINFO_OFFSET: usize = 8;
        let size = self.canvas.size();

        let bits: &BitSlice<u16, Lsb0> = &format_info.view_bits()[0..=15];
        // Draw first 7 bits from the top and from the right.
        for (k, bit) in bits[0..=7].iter().enumerate() {
            let module = Module::from(*bit);
            // Vertical line.
            let ver_offset = if k <= 5 { 0 } else { 1 };
            self.canvas.set(k + ver_offset, FINFO_OFFSET, module);
            // Horizontal line.
            self.canvas.set(FINFO_OFFSET, size - 1 - k, module);
        }
        // Draw remaining from the bottom and from the left.
        for (k, bit) in bits[8..=14].iter().enumerate() {
            let module = Module::from(*bit);
            // Vertical line.
            let i_ver = size - 7 + k;
            self.canvas.set(i_ver, FINFO_OFFSET, module);
            // Horizontal line.
            let hor_offset = if k == 0 { 1 } else { 0 };
            let j_hor = 6 - k + hor_offset;
            self.canvas.set(FINFO_OFFSET, j_hor, module);
        }
    }

    fn paint_version_info(&mut self) {
        todo!("Version info")
    }

    fn paint_always_dark_module(&mut self) {
        let v = self.meta.version.number() as usize;
        self.canvas.set(4 * v + 9, 8, Module::Dark);
    }

    fn paint_square(&mut self, value: Module, i: usize, j: usize, size: usize) {
        self.canvas.fill(value, i, j, size, size)
    }
}

fn format_info(ecl: Ecl, mask: Mask) -> u16 {
    const MASK: u16 = 0b101010000010010;
    let ecl_code = ecl.code() as u16;
    let mask_code = mask as u16;
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
