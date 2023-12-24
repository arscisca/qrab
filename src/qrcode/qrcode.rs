use super::{
    ecl::Ecl,
    module::{Matrix, Module},
    version::Version,
};

/// A QR code symbol.
pub struct QrCode {
    matrix: Matrix,
    version: Version,
    ecl: Ecl,
}

impl QrCode {
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

    /// Get the version.
    pub fn version(&self) -> Version {
        self.version
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
