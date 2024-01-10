use bitvec::prelude as bv;

/// A module of a QR code.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Module {
    Dark,
    Light,
}

impl Module {
    pub const fn toggled(self) -> Self {
        match self {
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

impl From<Module> for bool {
    fn from(value: Module) -> Self {
        match value {
            Module::Dark => true,
            Module::Light => false,
        }
    }
}

impl AsRef<Module> for bool {
    fn as_ref(&self) -> &Module {
        match self {
            true => &Module::Dark,
            false => &Module::Light,
        }
    }
}

/// Matrix of modules.
pub(crate) struct Matrix {
    modules: bv::BitVec,
    size: usize,
}

impl Matrix {
    /// Color of the modules on a newly initialized Matrix.
    pub const DEFAULT_MODULE_COLOR: Module = Module::Light;

    /// Linearize a 2D index `(i, j)` into a 1D index based on the matrix's size.
    fn linearized_index(&self, i: usize, j: usize) -> usize {
        i * self.size + j
    }

    /// Construct a `Matrix` based on its `size`.
    pub fn new(size: usize) -> Self {
        let modules = bv::BitVec::repeat(Self::DEFAULT_MODULE_COLOR.into(), size * size);
        Self { modules, size }
    }

    /// Get the size of the matrix.
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get the module at position `(i, j)` where `(0, 0)` is the top-left corner.
    pub fn get(&self, i: usize, j: usize) -> &Module {
        (&self.modules[self.linearized_index(i, j)]).as_ref()
    }

    /// Set the module at position `(i, j)` to `value`.
    pub fn set(&mut self, i: usize, j: usize, value: Module) {
        let index = self.linearized_index(i, j);
        self.modules.set(index, value.into());
    }

    /// Toggle elements of the matrix based on the passed `toggle`. It is passed the indices `(i, j)`
    /// of each module and it must return `true` if the module at that position should be toggled,
    /// `false` otherwise.
    pub fn toggle_with<F: Fn(usize, usize) -> bool>(&mut self, toggle: F) {
        for i in 0..self.size {
            for j in 0..self.size {
                if toggle(i, j) {
                    let index = self.linearized_index(i, j);
                    let toggled = !self.modules[index];
                    self.modules.set(index, toggled);
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

impl Default for Matrix {
    fn default() -> Self {
        Self::new(0)
    }
}

/// Iterator that yields the sequence of indices in a QR code matrix following the order in which the bits of the data
/// stream are placed.
pub struct IndexSequenceIter {
    size: usize,
    i: isize,
    j: isize,
}

impl IndexSequenceIter {
    /// Construct the `IndexSequenceIter` with the `size` of the symbol. The sequence always starts from the
    /// bottom-right corner of the symbol, i.e. the first index is (`size - 1`, `size - 1`).
    pub fn new(size: usize) -> Self {
        Self {
            size,
            i: size as isize - 1,
            j: size as isize - 1,
        }
    }

    /// Determine whether, at the current position, the indices are flowing upwards rather than downwards.
    fn flowing_up(&self) -> bool {
        ((self.size as isize - 1 - self.j) % 4) <= 1
    }

    /// Determine whether, independently of the upwards / downwards flow, the current step is a horizontal-only step.
    fn step_horizontally_only(&self) -> bool {
        let flowing_up = self.flowing_up();
        flowing_up && self.i == 0   // Flowing up but at the top
            || !flowing_up && self.i == self.size as isize - 1 // Flowing down but at the bottom
            || (self.size as isize - 1 - self.j) % 2 == 0 // Even column from the right
    }
}

impl Iterator for IndexSequenceIter {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        // Check that the iterator is not out of bounds.
        if self.i <= 0 && self.j < 0 {
            return None;
        }
        // Get the current position
        let current = (self.i, self.j);
        // Update the position (might temporarily go out of bounds)
        if self.step_horizontally_only() {
            self.j -= 1;
        } else {
            self.j += 1;
            // Go up or down depending on the current column flow
            if self.flowing_up() {
                self.i -= 1;
            } else {
                self.i += 1;
            }
        };
        // If current is valid, return it; otherwise do another step. Temporarily going out of bounds is allowed.as
        if current.0 >= 0 && current.1 >= 0 {
            Some((current.0 as usize, current.1 as usize))
        } else {
            self.next()
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.size * self.size;
        (len, Some(len))
    }
}

impl ExactSizeIterator for IndexSequenceIter {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn toggle() {
        let mut m = Module::Dark;
        m = m.toggled();
        assert_eq!(m, Module::Light);
        m = m.toggled();
        assert_eq!(m, Module::Dark);
    }


    #[test]
    #[allow(clippy::bool_assert_comparison)]
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

    #[test]
    fn index_seq_start() {
        let mut seq = IndexSequenceIter::new(21);
        assert_eq!(seq.next(), Some((20, 20)));
        assert_eq!(seq.next(), Some((20, 19)));
        assert_eq!(seq.next(), Some((19, 20)));
        assert_eq!(seq.next(), Some((19, 19)));
    }

    #[test]
    fn index_seq_at_vertical_boundaries() {
        let mut seq = IndexSequenceIter::new(21);
        // Advance until we reach the top
        assert_eq!(seq.find(|(i, _)| *i == 0), Some((0, 20)));
        assert_eq!(seq.next(), Some((0, 19)));
        // Should now move to the next column pair that flows downwards
        assert_eq!(seq.next(), Some((0, 18)));
        assert_eq!(seq.next(), Some((0, 17)));
        assert_eq!(seq.next(), Some((1, 18)));
        assert_eq!(seq.next(), Some((1, 17)));
        // Advance until the bottom
        assert_eq!(seq.find(|(i, _)| *i == 20), Some((20, 18)));
        assert_eq!(seq.next(), Some((20, 17)));
        assert_eq!(seq.next(), Some((20, 16)));
        assert_eq!(seq.next(), Some((20, 15)));
        assert_eq!(seq.next(), Some((19, 16)));
    }

    #[test]
    fn index_seq_at_left_boundary() {
        let mut seq = IndexSequenceIter::new(21);
        // Advance until we reach the left
        assert_eq!(seq.find(|(_, j)| *j == 0), Some((20, 0)));
        assert_eq!(seq.next(), Some((19, 0)));
        assert_eq!(seq.next(), Some((18, 0)));
        assert_eq!(seq.next(), Some((17, 0)));
    }

    #[test]
    fn index_seq_last() {
        let mut seq = IndexSequenceIter::new(21);
        seq.find(|(i, j)| *i == 0 && *j == 0).unwrap();
        assert_eq!(seq.next(), None);
    }
}
