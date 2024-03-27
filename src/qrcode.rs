use std::marker::PhantomData;

use bitvec::{
    slice::{BitSlice, BitSliceIndex},
    vec::BitVec,
};

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
    matrix: Matrix<Module>,
    version: Version,
    ecl: Ecl,
    mask: Mask,
}

impl QrCode {
    pub(crate) fn new(matrix: Matrix<Module>, version: Version, ecl: Ecl, mask: Mask) -> Self {
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

    /// Get the module at row `i` and column `j` with a boundary check.
    /// # Examples
    /// ```rust
    /// use qrab::{QrCode, Module};
    /// let qrcode = QrCode::encode("hello, world!").unwrap();
    /// assert_eq!(qrcode.get(0, 0), Some(Module::Dark));
    /// assert!(qrcode.get(qrcode.size() + 1, qrcode.size() + 1).is_none())
    /// ```
    pub fn get(&self, i: usize, j: usize) -> Option<Module> {
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
    pub fn rows(&self) -> impl Iterator<Item = MatrixDataSlice<Module>> {
        self.matrix.rows()
    }

    pub fn matrix(&self) -> &Matrix<Module> {
        &self.matrix
    }
}

impl std::fmt::Debug for QrCode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "<QR code version {:?}, ecl {:?}, mask {:03b}>",
            self.version,
            self.ecl,
            self.mask.code()
        )
    }
}

impl AsRef<Matrix<Module>> for QrCode {
    fn as_ref(&self) -> &Matrix<Module> {
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

// Matrix ==============================================================================================================
#[derive(Clone, PartialEq, Eq)]
pub struct Matrix<T> {
    _phantom: PhantomData<T>,
    data: BitVec,
    size: usize,
}

impl<T: From<bool> + Into<bool>> Matrix<T> {
    /// Initialize a `size`x`size` matrix by filling it with `value`.
    pub fn filled(value: T, size: usize) -> Self {
        let data = BitVec::repeat(value.into(), size * size);
        Self {
            _phantom: PhantomData,
            data,
            size,
        }
    }

    /// Linearize a 2D index `(i, j)` into a 1D index with a boundary check to address the matrix data.
    fn linear_index(&self, i: usize, j: usize) -> Option<usize> {
        if i < self.size && j < self.size {
            Some(self.linear_index_unchecked(i, j))
        } else {
            None
        }
    }

    /// Linearize a 2D index `(i, j)` into a 1D index to address the matrix data.
    fn linear_index_unchecked(&self, i: usize, j: usize) -> usize {
        i * self.size + j
    }

    /// Get the size of the matrix.
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get the item at position `(i, j)` with a boundary check.
    /// # Example
    /// ```rust
    /// use qrab::{Matrix, Module};
    /// let modules = Matrix::filled(Module::Dark, 3);
    /// assert_eq!(modules.get(1, 2), Some(Module::Dark));
    /// assert!(modules.get(10, 10).is_none())
    /// ```
    pub fn get(&self, i: usize, j: usize) -> Option<T> {
        self.data
            .get(self.linear_index(i, j)?)
            .map(|bit| (*bit).into())
    }

    /// Set the item at position `(i, j)` to `value`.
    pub fn set(&mut self, i: usize, j: usize, value: Module) {
        let index = self
            .linear_index(i, j)
            .unwrap_or_else(|| panic!("index ({}, {}) is out of bounds", i, j));
        self.data.set(index, value.into());
    }

    /// Transpose the matrix in place, inverting rows and columns.
    /// # Examples
    /// ```rust
    /// use qrab::{Matrix, Module};
    /// let mut matrix = Matrix::filled(Module::Dark, 3);
    /// ```
    pub fn transpose(&mut self) {
        for i in 0..self.size {
            for j in (i + 1)..self.size {
                let index1 = self.linear_index_unchecked(i, j);
                let index2 = self.linear_index_unchecked(j, i);
                self.data.swap(index1, index2);
            }
        }
    }

    /// Fill the region with its top-left corner in `(i0. j0)` and of sizes `width` and `height` with `value`.
    ///
    /// # Examples
    /// ```rust
    /// use qrab::{Matrix, Module};
    /// // Construct a 10x10 matrix filled with light modules.
    /// let mut matrix = Matrix::filled(Module::Light, 10);
    /// // Fill its bottom right 2x2 corner with dark modules.
    /// matrix.fill_rect(Module::Dark, 7, 7, 2, 2);
    /// ```
    pub fn fill_rect(&mut self, value: T, i0: usize, j0: usize, height: usize, width: usize) {
        let bit: bool = value.into();
        for i in i0..(i0 + height) {
            let Some(mut row) = self.row_mut(i) else {
                break;
            };
            let Some(mut range) = row.get_slice_mut(j0..(j0 + width)) else {
                break;
            };
            range.fill(bit);
        }
    }

    /// Apply `mask` to toggle the modules in the matrix.
    pub fn mask(&mut self, mask: Mask) {
        let toggle = mask.function();
        for i in 0..self.size() {
            for j in 0..self.size() {
                if toggle(i, j) {
                    let index = self.linear_index_unchecked(i, j);
                    let original = self.data[index];
                    self.data.set(index, !original);
                }
            }
        }
    }

    /// Count the number of instances of `value`.
    pub fn count(&self, value: T) -> usize {
        match value.into() {
            true => self.data.count_ones(),
            false => self.data.count_zeros(),
        }
    }

    /// Get a reference to the `i`th row of the matrix.
    pub fn row(&self, i: usize) -> Option<MatrixDataSlice<T>> {
        let range = self.linear_index(i, 0)?..=self.linear_index(i, self.size() - 1)?;
        self.data.get(range).map(Into::into)
    }

    /// Get a mutable reference to the `i`th row of the matrix.
    fn row_mut(&mut self, i: usize) -> Option<MatrixDataSliceMut<T>> {
        let range = self.linear_index(i, 0)?..=self.linear_index(i, self.size() - 1)?;
        self.data.get_mut(range).map(Into::into)
    }

    /// Get an iterator over the rows of a matrix.
    pub fn rows(&self) -> impl ExactSizeIterator<Item = MatrixDataSlice<T>> {
        (0..self.size()).map(|i| self.row(i)).map(Option::unwrap)
    }
}

impl<const N: usize, T: From<bool> + Into<bool>> From<[[T; N]; N]> for Matrix<T> {
    fn from(value: [[T; N]; N]) -> Self {
        let data = BitVec::from_iter(value.into_iter().flatten().map(|value| value.into()));
        Self {
            _phantom: PhantomData,
            data,
            size: N,
        }
    }
}

impl<T: From<bool> + Into<bool>> std::fmt::Debug for Matrix<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        for row in self.rows() {
            let line = row.iter().fold(String::new(), |mut acc, item| {
                let _ = write!(acc, "{}", item.into() as u8);
                acc
            });
            writeln!(f, "|{}|", line)?;
        }
        Ok(())
    }
}

/// A slice of data from a `Matrix<T>`, usually representing a row or a row's subset of data.
#[derive(PartialEq, Eq)]
pub struct MatrixDataSlice<'a, T> {
    _phantom: PhantomData<T>,
    slice: &'a BitSlice,
}

impl<'a, T: From<bool> + Into<bool>> MatrixDataSlice<'a, T> {
    /// Get the length of the slice.
    pub fn len(&self) -> usize {
        self.slice.len()
    }

    /// Check whether the data slice is empty.
    pub fn is_empty(&self) -> bool {
        self.slice.is_empty()
    }

    /// Get the `i`th element of the slice.
    pub fn get(&self, i: usize) -> Option<T> {
        self.slice.get(i).map(|bit| (*bit).into())
    }

    /// Get a sub-slice defined by `index`.
    pub fn get_slice<Idx>(&self, index: Idx) -> Option<Self>
    where
        Idx: BitSliceIndex<'a, usize, bitvec::order::Lsb0, Immut = &'a BitSlice>,
    {
        let slice = self.slice.get(index)?;
        Some(Self {
            _phantom: PhantomData,
            slice,
        })
    }

    pub fn iter(&self) -> MatrixDataIter<T> {
        MatrixDataIter::new(self)
    }
}

impl<'a, T: From<bool> + Into<bool>> From<&'a BitSlice> for MatrixDataSlice<'a, T> {
    fn from(value: &'a BitSlice) -> Self {
        Self {
            _phantom: PhantomData,
            slice: value,
        }
    }
}

impl<'a, T: From<bool> + Into<bool>> IntoIterator for &'a MatrixDataSlice<'a, T> {
    type Item = T;
    type IntoIter = MatrixDataIter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T: std::fmt::Debug + From<bool> + Into<bool>> std::fmt::Debug for MatrixDataSlice<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, item) in self.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", item)?;
        }
        write!(f, "]")
    }
}

impl<'a, T: From<bool> + Into<bool>> PartialEq<&[bool]> for MatrixDataSlice<'a, T> {
    fn eq(&self, other: &&[bool]) -> bool {
        if self.len() != other.len() {
            return false;
        }
        return self
            .slice
            .iter()
            .zip(other.iter())
            .all(|(this, other)| this == other);
    }
}

/// Mutable version of a `MatrixDataSlice<T>`.
pub struct MatrixDataSliceMut<'a, T> {
    _phantom: PhantomData<T>,
    slice: &'a mut BitSlice,
}

impl<'a, T: From<bool> + Into<bool>> MatrixDataSliceMut<'a, T> {
    /// Get the length of the slice.
    pub fn len(&self) -> usize {
        self.slice.len()
    }

    /// Check whether the data slice is empty.
    pub fn is_empty(&self) -> bool {
        self.slice.is_empty()
    }

    /// Get the `i`th element.
    pub fn get(&self, i: usize) -> Option<T> {
        self.slice.get(i).map(|bit| (*bit).into())
    }

    /// Set the `i`th element.
    /// # Panics
    /// This panics if `i` is out of range.
    pub fn set(&mut self, i: usize, value: T) {
        self.slice.set(i, value.into())
    }

    /// Get an immutable sub-slice of this slice, pointed by `index`.
    pub fn get_slice<Idx>(&'a self, index: Idx) -> Option<MatrixDataSlice<'a, T>>
    where
        Idx: BitSliceIndex<'a, usize, bitvec::order::Lsb0, Immut = &'a BitSlice>,
    {
        let slice = self.slice.get(index)?;
        Some(MatrixDataSlice {
            _phantom: PhantomData,
            slice,
        })
    }

    /// Get a mutable sub-slice of this slice, pointed by `index`.
    pub fn get_slice_mut<Idx>(&'a mut self, index: Idx) -> Option<Self>
    where
        Idx: BitSliceIndex<'a, usize, bitvec::order::Lsb0, Mut = &'a mut BitSlice>,
    {
        let slice = self.slice.get_mut(index)?;
        Some(Self {
            _phantom: PhantomData,
            slice,
        })
    }

    /// Fill the slice with `value`, which may be of type `T` or any type that converts to a `bool`.
    pub fn fill<U: Into<bool>>(&mut self, value: U) {
        self.slice.fill(value.into());
    }
}

impl<'a, T: From<bool> + Into<bool>> From<&'a mut BitSlice> for MatrixDataSliceMut<'a, T> {
    fn from(value: &'a mut BitSlice) -> Self {
        Self {
            _phantom: PhantomData,
            slice: value,
        }
    }
}

/// Iterator over the elements of a `Matrix<T>`, yielding its elements row-wise.
pub struct MatrixDataIter<'a, T> {
    slice: &'a MatrixDataSlice<'a, T>,
    i: usize,
}

impl<'a, T: From<bool> + Into<bool>> MatrixDataIter<'a, T> {
    fn new(slice: &'a MatrixDataSlice<T>) -> Self {
        Self { slice, i: 0 }
    }
}

impl<'a, T: From<bool> + Into<bool>> Iterator for MatrixDataIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.slice.get(self.i)?;
        self.i += 1;
        Some(result)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<'a, T: From<bool> + Into<bool>> ExactSizeIterator for MatrixDataIter<'a, T> {}
impl<'a, T: From<bool> + Into<bool>> DoubleEndedIterator for MatrixDataIter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let result = self.slice.get(self.i)?;
        // This will make the next iteration go out of range. It's unlikely that we have a slice as big as usize::MAX.
        self.i = self.i.wrapping_sub(1);
        Some(result)
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
    fn module_toggle() {
        let mut m = Module::Dark;
        m = m.toggled();
        assert_eq!(m, Module::Light);
        m = m.toggled();
        assert_eq!(m, Module::Dark);
    }

    #[test]
    #[allow(clippy::bool_assert_comparison)]
    fn module_bool_conversions() {
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
