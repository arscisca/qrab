use super::{
    info::{Ecl, Mask, Version},
    module::{Matrix, Module},
};

/// A QR code symbol.
pub struct QrCode {
    matrix: Matrix,
    version: Version,
    ecl: Ecl,
    mask: Mask,
}

impl QrCode {
    pub(crate) fn new(matrix: Matrix, version: Version, ecl: Ecl, mask: Mask) -> Self {
        Self {
            matrix,
            version,
            ecl,
            mask,
        }
    }
    /// Get the module at row `i` and column `j`.
    pub fn get(&self, i: usize, j: usize) -> &Module {
        self.matrix.get(i, j)
    }

    /// Get the size of the QR code symbol.
    pub fn size(&self) -> usize {
        self.matrix.size()
    }

    /// Get the error correction level.
    pub fn ecl(&self) -> Ecl {
        self.ecl
    }

    /// Get the QR code version.
    pub fn version(&self) -> Version {
        self.version
    }

    /// Get the mask used in the QR code symbol.
    pub fn mask(&self) -> Mask {
        self.mask
    }

    /// Render the QR code into an ASCII representation that uses the "Full block" unicode character
    /// (U+2588, UTF-8: E2 96 88) and empty spaces for the dark and light modules respectively.
    pub fn render_ascii<W: std::fmt::Write>(&self, sink: &mut W) -> std::fmt::Result {
        writeln!(sink, "{}", "██".repeat(self.size() + 2))?;
        for i in 0..self.size() {
            write!(sink, "██")?;
            for j in 0..self.size() {
                let output = match self.get(i, j) {
                    Module::Light => "██",
                    Module::Dark => "  ",
                };
                write!(sink, "{}", output)?;
            }
            writeln!(sink, "██")?;
        }
        writeln!(sink, "{}", "██".repeat(self.size() + 2))
    }
}

impl std::fmt::Debug for QrCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "<QR code version {:?}, ecl {:?}>",
            self.version, self.ecl
        )
    }
}
