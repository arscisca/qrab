#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Mask {
    M000 = 0b000,
    M001 = 0b001,
    M010 = 0b010,
    M011 = 0b011,
    M100 = 0b100,
    M101 = 0b101,
    M110 = 0b110,
    M111 = 0b111,
}

impl Mask {
    pub fn code(&self) -> u8 {
        *self as u8
    }

    pub fn function(&self) -> impl Fn(usize, usize) -> bool {
        match self {
            Self::M000 => |i, j| (i + j) % 2 == 0,
            Self::M001 => |i, _| i % 2 == 0,
            Self::M010 => |_, j| j % 3 == 0,
            Self::M011 => |i, j| (i + j) % 3 == 0,
            Self::M100 => |i, j| (i / 2 + j / 3) % 2 == 0,
            Self::M101 => |i, j| (i * j) % 2 + (i * j) % 3 == 0,
            Self::M110 => |i, j| ((i * j) % 2 + (i * j) % 3) % 2 == 0,
            Self::M111 => |i, j| ((i + j) % 2 + (i * j) % 3) % 2 == 0,
        }
    }
}
