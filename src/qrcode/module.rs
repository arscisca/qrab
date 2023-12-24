/// A module of a QR code.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Module {
    Dark,
    Light,
}

impl Module {
    pub fn toggle(&mut self) {
        *self = match self {
            Self::Dark => Self::Light,
            Self::Light => Self::Dark,
        }
    }
}

impl From<bool> for Module {
    fn from(value: bool) -> Self {
        match value {
            true => Self::Dark,
            false => Self::Light,
        }
    }
}

impl Into<bool> for Module {
    fn into(self) -> bool {
        match self {
            Self::Dark => true,
            Self::Light => false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn toggle() {
        let mut m = Module::Dark;
        m.toggle();
        assert_eq!(m, Module::Light);
        m.toggle();
        assert_eq!(m, Module::Dark);
    }

    #[test]
    fn bool_conversions() {
        // Conversions are as expected
        assert_eq!(Module::from(false), Module::Light);
        assert_eq!(Module::from(true), Module::Dark);
        assert_eq!(<Module as Into<bool>>::into(Module::Light), false);
        assert_eq!(<Module as Into<bool>>::into(Module::Dark), true);
        // Double check they are self-consistent
        assert_eq!(
            Module::from(<Module as Into<bool>>::into(Module::Light)),
            Module::Light
        );
        assert_eq!(
            Module::from(<Module as Into<bool>>::into(Module::Dark)),
            Module::Dark
        );
    }
}

/// Matrix of modules
pub(crate) struct Matrix {
    modules: Vec<Module>,
    size: usize,
}

impl Matrix {
    /// Linearize a 2D index `(i, j)` into a 1D index based on the matrix's size.
    fn linearized_index(&self, i: usize, j: usize) -> usize {
        i * self.size + j
    }

    /// Construct a `Matrix` based on its `size`.
    pub fn new(size: usize) -> Self {
        let mut modules = vec![Module::Light; size * size];
        Self { modules, size }
    }

    /// Get the size of the matrix.
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get the module at row `i` and column `j` where (0, 0) is the top-left corner.
    pub fn get(&self, i: usize, j: usize) -> &Module {
        &self.modules[self.linearized_index(i, j)]
    }

    /// Get the module at row `i` and column `j` where (0, 0) is the top-left corner.
    pub fn get_mut(&mut self, i: usize, j: usize) -> &mut Module {
        let index = self.linearized_index(i, j);
        &mut self.modules[index]
    }

    /// Toggle elements of the matrix based on the passed `rule`. It is passed the indices `(i, j)`
    /// of each module and it must return `true` if the module at that position should be toggled,
    /// `false` otherwise.
    pub fn toggle_with<F: Fn(usize, usize) -> bool>(&mut self, rule: F) {
        for i in 0..self.size {
            for j in 0..self.size {
                if rule(i, j) {
                    self.get_mut(i, j).toggle();
                }
            }
        }
    }
}

impl std::ops::Index<(usize, usize)> for Matrix {
    type Output = Module;

    fn index(&self, index: (usize, usize)) -> &Self::Output {
        self.get(index.0, index.1)
    }
}

impl std::ops::IndexMut<(usize, usize)> for Matrix {
    fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
        self.get_mut(index.0, index.1)
    }
}
