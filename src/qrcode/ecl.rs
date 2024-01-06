/// Error correction level.
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Ecl {
    /// Low: 7% recovery rate.
    L,
    /// Medium: 15% recovery rate.
    M,
    /// Quartile: 25% recovery rate.
    Q,
    /// High: 30% recovery rate.
    H,
}

impl Ecl {
    pub fn code(&self) -> u8 {
        match self {
            Self::L => 0b01,
            Self::M => 0b00,
            Self::Q => 0b11,
            Self::H => 0b10,
        }
    }
}

impl From<&Ecl> for char {
    fn from(value: &Ecl) -> Self {
        match value {
            Ecl::L => 'L',
            Ecl::M => 'M',
            Ecl::Q => 'Q',
            Ecl::H => 'H',
        }
    }
}

impl std::fmt::Debug for Ecl {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let c: char = self.into();
        write!(f, "{}", c)
    }
}

impl std::fmt::Display for Ecl {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let c: char = self.into();
        write!(f, "{}", c)
    }
}
