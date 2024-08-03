use bitvec::vec::BitVec;

/// Module (aka, a pixel) of a QR code.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Module {
    Light,
    Dark,
}

impl From<bool> for Module {
    fn from(value: bool) -> Self {
        match value {
            true => Module::Dark,
            false => Module::Light,
        }
    }
}

impl From<Module> for bool {
    fn from(value: Module) -> Self {
        match value {
            Module::Dark => true,
            Module::Light => false,
        }
    }
}

/// A square canvas of modules.
pub struct Canvas {
    data: BitVec,
    size: usize,
}

impl Canvas {
    /// Return a canvas of size `size` filled with `module`.
    pub fn filled(size: usize, module: Module) -> Self {
        Self {
            data: BitVec::repeat(module.into(), size * size),
            size,
        }
    }

    /// Get the size of the canvas.
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get the 1D index of the data array corresponding to position `(i, j)`, checking for validity.
    fn linearized_index(&self, i: usize, j: usize) -> Option<usize> {
        if i < self.size && j < self.size {
            Some(self.linearized_index_unchecked(i, j))
        } else {
            None
        }
    }

    /// Get the 1D index of the data array corresponding to position `(i, j)`.
    fn linearized_index_unchecked(&self, i: usize, j: usize) -> usize {
        self.size * i + j
    }

    /// Get the module at position `(i, j)`.
    pub fn get(&self, i: usize, j: usize) -> Option<Module> {
        self.data
            .get(self.linearized_index(i, j)?)
            .map(|bit| (*bit).into())
    }

    /// Set the module at position `(i, j)`.
    /// # Panics
    /// Panics if position `(i, j)` is out of bounds.
    pub fn set(&mut self, i: usize, j: usize, value: Module) {
        let index = self
            .linearized_index(i, j)
            .unwrap_or_else(|| panic!("position ({}, {}) is out of bounds", i, j));
        self.data.set(index, value.into())
    }
}
