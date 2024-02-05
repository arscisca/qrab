use bitvec::prelude as bv;
use itertools::Itertools;

use super::{
    info::{Ecl, Mask, Version},
    Encoder, EncodingConstraints, EncodingError,
};

/// A QR valid code symbol.
///
/// # Storage
/// Its modules (a.k.a. its pixels) are stored in a compacted form inside a row-major `Matrix`. The most efficient way
/// to interact with them is
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

    /// Generate a QR code by encoding `data`.
    pub fn encode<T: AsRef<[u8]>>(data: T) -> Result<Self, EncodingError> {
        let encoder = Encoder::default();
        encoder.encode(data)
    }

    pub fn encode_constrained<T: AsRef<[u8]>>(
        data: T,
        constraints: EncodingConstraints,
    ) -> Result<Self, EncodingError> {
        let encoder = Encoder::with_constraints(constraints);
        encoder.encode(data)
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

    /// Get an iterator over the rows of the QrCode.
    pub fn rows(&self) -> Rows {
        self.matrix.rows()
    }

    pub fn matrix(&self) -> &Matrix {
        &self.matrix
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

impl AsRef<Matrix> for QrCode {
    fn as_ref(&self) -> &Matrix {
        self.matrix()
    }
}

// Module ==============================================================================================================
/// Module of a QR code, a.k.a. one of its pixels.
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

impl AsRef<bool> for Module {
    fn as_ref(&self) -> &bool {
        match self {
            Module::Dark => &true,
            Module::Light => &true,
        }
    }
}

impl std::ops::BitXor<bool> for Module {
    type Output = Module;

    fn bitxor(self, rhs: bool) -> Self::Output {
        Module::from(bool::from(self) ^ rhs)
    }
}

// Matrix ==============================================================================================================
/// Editable row-major square matrix of QR code modules that may or may not yet qualify as a valid QR code. It can be
/// used to construct a QR code, to explore its modules and so on.
#[derive(Clone)]
pub struct Matrix {
    modules: bv::BitVec,
    size: usize,
}

impl Matrix {
    /// Color of the modules on a newly initialized Matrix.
    pub const DEFAULT_MODULE_COLOR: Module = Module::Light;

    /// Construct a `Matrix` based on its `size`.
    pub fn new(size: usize) -> Self {
        let modules = bv::BitVec::repeat(Self::DEFAULT_MODULE_COLOR.into(), size * size);
        Self { modules, size }
    }

    /// Linearize a 2D index `(i, j)` into a 1D index based on the matrix's size.
    fn linearized_index(&self, i: usize, j: usize) -> usize {
        i * self.size + j
    }

    /// Get the size (its width or its height) of the matrix.
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get the module at position `(i, j)`.
    pub fn get(&self, i: usize, j: usize) -> &Module {
        self.modules[self.linearized_index(i, j)].as_ref()
    }

    /// Set the module at position `(i, j)` to `value`.
    pub fn set(&mut self, i: usize, j: usize, value: Module) {
        let index = self.linearized_index(i, j);
        self.modules.set(index, value.into());
    }

    /// Get a `Vec` containing pairs `(Module, usize)` that represent continuous strikes of the same `Module` and their
    /// length.
    pub(crate) fn strikes_in_row(&self, row: usize) -> Vec<(Module, usize)> {
        self.row(row)
            .into_iter()
            .group_by(|b| *b)
            .into_iter()
            .map(|(key, group)| (Module::from(key), group.count()))
            .collect()
    }

    fn fill_row_chunk(&mut self, value: Module, i: usize, j0: usize, width: usize) {
        let row_range = (i * self.size)..((i + 1) * self.size);
        let row = &mut self.modules.as_mut_bitslice()[row_range];
        let col_range = j0..(j0 + width);
        row[col_range].fill(value.into());
    }

    pub fn transpose(self) -> Matrix {
        let mut transposed = Matrix::new(self.size);
        // TODO: This can be optimized by working with the bitvec's underlying data rather than raw bits
        for i in 0..self.size {
            for j in (i + 1)..self.size {
                let starting_index = self.linearized_index(i, j);
                let transposed_index = transposed.linearized_index(j, i);
                transposed
                    .modules
                    .set(transposed_index, self.modules[starting_index]);
            }
        }
        transposed
    }

    /// Fill the region with its top-left corner in `(i0. j0)` and of sizes `width` and `height` with `value`.
    ///
    /// # Examples
    /// ```rust
    /// use qrab::{Matrix, Module};
    /// // Construct a 10x10 matrix
    /// let mut m = Matrix::new(10);
    /// // Fill its bottom right 2x2 corner with dark modules
    /// m.fill(Module::Dark, 7, 7, 2, 2);
    /// ```
    pub fn fill(&mut self, value: Module, i0: usize, j0: usize, height: usize, width: usize) {
        for i in i0..(i0 + height) {
            self.fill_row_chunk(value, i, j0, width);
        }
    }

    /// Toggle elements of the matrix based on the passed `toggle`. It is passed the indices `(i, j)`
    /// of each module and it must return `true` if the module at that position should be toggled,
    /// `false` otherwise.
    fn toggle_with<F: Fn(usize, usize) -> bool>(&mut self, toggle: F) {
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

    /// Apply `mask` to toggle the modules in the matrix.
    pub fn mask(&mut self, mask: Mask) {
        let toggle = mask.function();
        for (i, chunk) in self.modules.chunks_mut(self.size).enumerate() {
            for (j, mut bit) in chunk.iter_mut().enumerate() {
                if toggle(i, j) {
                    bit.set(!*bit);
                }
            }
        }
        // self.toggle_with(mask.function());
    }

    pub(crate) fn num_dark_modules(&self) -> usize {
        self.modules.count_ones()
    }

    /// Get an alias to the `i`th row of the matrix.
    fn row(&self, i: usize) -> Row {
        Row {
            modules: &self.modules[(i * self.size)..((i + 1) * self.size)],
        }
    }

    /// Get an iterator over the rows of a matrix.
    pub fn rows(&self) -> Rows {
        Rows { i: 0, matrix: self }
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

pub struct Row<'r> {
    modules: &'r bv::BitSlice,
}

impl<'r> Row<'r> {
    pub fn len(&self) -> usize {
        self.modules.len()
    }
}

impl<'r> std::ops::Index<usize> for Row<'r> {
    type Output = Module;

    fn index(&self, index: usize) -> &Self::Output {
        self.modules[index].as_ref()
    }
}

impl<'r> IntoIterator for Row<'r> {
    type Item = Module;
    type IntoIter = RowIter<'r>;

    fn into_iter(self) -> Self::IntoIter {
        RowIter { i: 0, row: self }
    }
}

pub struct RowIter<'r> {
    i: usize,
    row: Row<'r>,
}

impl<'r> Iterator for RowIter<'r> {
    type Item = Module;

    fn next(&mut self) -> Option<Self::Item> {
        let i = self.i;
        if i < self.row.len() {
            self.i += 1;
            Some(self.row[i])
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.row.len(), Some(self.row.len()))
    }
}

impl<'r> ExactSizeIterator for RowIter<'r> {}

pub struct Rows<'r> {
    matrix: &'r Matrix,
    i: usize,
}

impl<'r> Iterator for Rows<'r> {
    type Item = Row<'r>;

    fn next(&mut self) -> Option<Self::Item> {
        let i = self.i;
        if i < self.matrix.size() {
            self.i += 1;
            Some(self.matrix.row(i))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.matrix.size, Some(self.matrix.size))
    }
}

impl<'r> ExactSizeIterator for Rows<'r> {}

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
