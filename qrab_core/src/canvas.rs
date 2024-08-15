use bitvec::{slice::BitSlice, vec::BitVec};

use crate::{qrstandard, Version};

/// Module (aka, a pixel) of a QR code.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Module {
    Light,
    Dark,
}

impl Module {
    /// Get the inverted module.
    /// # Example
    /// ```
    /// use qrab_core::Module;
    /// assert_eq!(Module::Dark.inverted(), Module::Light);
    /// assert_eq!(Module::Light.inverted(), Module::Dark);
    /// ```
    pub fn inverted(&self) -> Self {
        match self {
            Module::Dark => Module::Light,
            Module::Light => Module::Dark,
        }
    }
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

/// A square matrix of bits.
struct BitMatrix {
    data: BitVec,
    size: usize,
}

impl BitMatrix {
    /// Return a matrix of size `size` filled with `value`.
    pub fn filled(size: usize, value: bool) -> Self {
        Self {
            data: BitVec::repeat(value, size * size),
            size,
        }
    }

    /// Get the size of the matrix.
    pub fn size(&self) -> usize {
        self.size
    }

    /// Get the 1D index of the data array corresponding to position `(i, j)`, checking for validity.
    #[inline]
    fn linearized_index(&self, i: usize, j: usize) -> Option<usize> {
        if i < self.size && j < self.size {
            Some(self.linearized_index_unchecked(i, j))
        } else {
            None
        }
    }

    /// Get the 1D index of the data array corresponding to position `(i, j)`.
    #[inline]
    fn linearized_index_unchecked(&self, i: usize, j: usize) -> usize {
        self.size * i + j
    }

    /// Get the 1D index of the data array corresponding to position `(i, j)`, checking for validity and panicking if invalid.
    /// # Panics
    /// Panics if either `i` or `j` is out of bounds.
    #[inline]
    fn linearized_index_unwrapped(&self, i: usize, j: usize) -> usize {
        self.linearized_index(i, j).unwrap_or_else(|| {
            panic!(
                "index out of bounds: the size is {} but the index is ({}, {})",
                self.size, i, j
            )
        })
    }

    /// Get the bit at position `(i, j)`.
    #[inline]
    pub fn get(&self, i: usize, j: usize) -> Option<bool> {
        self.data
            .get(self.linearized_index(i, j)?)
            .map(|bit| (*bit))
    }

    /// Set the bit at position `(i, j)`.
    /// # Panics
    /// Panics if position `(i, j)` is out of bounds.
    #[inline]
    pub fn set(&mut self, i: usize, j: usize, value: bool) {
        let index = self.linearized_index(i, j).unwrap_or_else(|| {
            panic!(
                "index out of bounds: the size is {} but the index is ({}, {})",
                self.size, i, j
            )
        });
        self.data.set(index, value)
    }

    /// Get the `i`th row of the matrix.
    pub fn row(&self, i: usize) -> Option<&BitSlice> {
        let range = self.linearized_index(i, 0)?..=self.linearized_index(i, self.size - 1)?;
        Some(&self.data[range])
    }

    /// Get a mutable reference to the `i`th row of the matrix.
    pub fn row_mut(&mut self, i: usize) -> Option<&mut BitSlice> {
        let range = self.linearized_index(i, 0)?..=self.linearized_index(i, self.size - 1)?;
        Some(&mut self.data[range])
    }

    /// Fill a rectangle with it upper-left corner at (`i`, `j`) of size `width` and `height` with
    /// `value`.
    /// # Panics
    /// Panics if any access is out of bounds.
    #[inline]
    pub fn fill(&mut self, value: bool, i: usize, j: usize, width: usize, height: usize) {
        let (jmin, jmax) = (j, j + width);
        for line in i..(i + height) {
            let range = self.linearized_index_unwrapped(line, jmin)
                ..=self.linearized_index_unwrapped(line, jmax - 1);
            self.data[range].fill(value)
        }
    }
}

/// A square canvas of modules.
pub struct Canvas {
    matrix: BitMatrix,
}

impl Canvas {
    /// Return a canvas of size `size` filled with `module`.
    #[inline]
    pub fn filled(size: usize, module: Module) -> Self {
        Self {
            matrix: BitMatrix::filled(size, module.into()),
        }
    }

    /// Get the size of the canvas.
    #[inline]
    pub fn size(&self) -> usize {
        self.matrix.size()
    }

    /// Get the module at position `(i, j)`.
    #[inline]
    pub fn get(&self, i: usize, j: usize) -> Option<Module> {
        self.matrix.get(i, j).map(Module::from)
    }

    /// Set the module at position `(i, j)`.
    /// # Panics
    /// Panics if position `(i, j)` is out of bounds.
    #[inline]
    pub fn set(&mut self, i: usize, j: usize, value: Module) {
        self.matrix.set(i, j, value.into())
    }

    #[inline]
    /// Toggle the pixels in the canvas according to `rule`: wherever `rule(i, j)` is true, the
    /// pixel at position `(i, j)` is toggled.
    pub fn toggle(&mut self, rule: fn(usize, usize) -> bool) {
        let size = self.size();
        for i in 0..size {
            for j in 0..size {
                self.matrix.set(i, j, self.matrix.get(i, j).unwrap() ^ rule(i, j))
            }
        }
    }

    /// Fill a rectangle with it upper-left corner at (`i`, `j`) of size `width` and `height` with
    /// `value`.
    /// # Panics
    /// Panics if any access is out of bounds.
    #[inline]
    pub fn fill(&mut self, value: Module, i: usize, j: usize, width: usize, height: usize) {
        let size = self.size();
        let value = value.into();
        let (jmin, jmax) = (j, j + width);
        for row in i..(i + height) {
            let row = self.matrix.row_mut(row).unwrap_or_else(|| {
                panic!(
                    "index out of bounds: the size is {} but the index is ({}, {})",
                    size, row, 0
                )
            });
            row[jmin..jmax].fill(value);
        }
    }
}

/// An atlas to check whether certain indices of a [Canvas] are reserved for QR code standard
/// specific information such as timing or locator patterns.
pub struct ReservedAreaAtlas {
    matrix: BitMatrix,
}

impl ReservedAreaAtlas {
    /// Create a new atlas for the given `version`.
    pub fn new(version: Version) -> Self {
        let mut matrix = BitMatrix::filled(qrstandard::canvas_size(version), false);
        let size = matrix.size();

        // Timing patterns.
        const TIMING_PATTERN_POS: usize = 6;
        matrix.fill(true, TIMING_PATTERN_POS, 0, size, 1);
        matrix.fill(true, 0, TIMING_PATTERN_POS, 1, size);

        // Locator patterns.
        const LOCATOR_PATTERN_SIZE: usize = 1 + 1 + 3 + 1 + 1;
        const LOCATOR_SPACING_SIZE: usize = 1;
        const LOCATOR_TOT_SIZE: usize = LOCATOR_PATTERN_SIZE + LOCATOR_SPACING_SIZE;
        for (i, j) in [
            (0, 0),
            (size - LOCATOR_TOT_SIZE, 0),
            (0, size - LOCATOR_TOT_SIZE),
        ] {
            matrix.fill(true, i, j, LOCATOR_TOT_SIZE, LOCATOR_TOT_SIZE);
        }

        // Format information (includes the always dark module).
        const FINFO_OFFSET: usize = LOCATOR_TOT_SIZE;
        const FINFO_SIZE: usize = 8;
        matrix.fill(true, FINFO_OFFSET, 0, FINFO_SIZE + 1, 1);
        matrix.fill(true, FINFO_OFFSET, size - FINFO_SIZE, FINFO_SIZE, 1);
        matrix.fill(true, 0, FINFO_OFFSET, 1, FINFO_SIZE);
        matrix.fill(true, size - FINFO_SIZE, FINFO_OFFSET, 1, FINFO_SIZE);

        // TODO: Alignment patterns
        if version > Version::V01 {
            todo!("Implement reserved areas for alignment patterns")
        }

        // Version information
        const VINFO_WIDTH: usize = 3;
        const VINFO_HEIGHT: usize = 6;
        if version >= Version::V07 {
            for (i, j) in [
                (0, size - LOCATOR_TOT_SIZE - VINFO_WIDTH),
                (size - LOCATOR_TOT_SIZE - VINFO_WIDTH, 0),
            ] {
                matrix.fill(true, i, j, VINFO_WIDTH, VINFO_HEIGHT);
            }
        }

        Self { matrix }
    }

    #[inline]
    /// Get the size of the atlas.
    pub fn size(&self) -> usize {
        self.matrix.size()
    }

    #[inline]
    /// Check whether position (`i`, `j`) is reserved.
    /// # Example
    /// ```
    /// use qrab_core::{Version, ReservedAreaAtlas};
    /// let atlas = ReservedAreaAtlas::new(Version::V01);
    /// assert!(atlas.is_reserved(0, 0));
    /// assert!(!atlas.is_reserved(20, 20));
    /// ```
    pub fn is_reserved(&self, i: usize, j: usize) -> bool {
        self.matrix.get(i, j).unwrap_or_else(|| {
            panic!(
                "index out of bounds: the size is {} but the index is ({}, {})",
                self.size(),
                i,
                j
            )
        })
    }
}

/// Calculate the (`i`, `j`) position of bit at `index` in a canvas of the given `size`.
pub fn canvas_position(index: usize, size: usize) -> (usize, usize) {
    const TIMING_COL: usize = 7;
    // `i` and `j` move in a zig-zag.
    let (mut i, mut j) = (index / 2, index % 2);
    // `i` is limited by `size`, but keep track of which cycle we are in.
    let k = i / size;
    i %= size;
    // On odd cycles `i` is inverted.
    if k % 2 != 0 {
        i = size - 1 - i;
    }
    // `j` is shifted depending on the cycle.
    j += 2 * k;
    // The vertical timing column pushes `j` by 1.
    if j > size - 1 - TIMING_COL {
        j += 1;
    }
    // Invert becasue in fact bits are placed bottom-right to top-left.
    (size - 1 - i, size - 1 - j)
}
