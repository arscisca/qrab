use qrab::{Matrix, Module};

#[test]
fn test_matrix_get() {
    // Valid get.
    let matrix = Matrix::from([
        [false, false, false],
        [false, false, false],
        [false, false, true]
    ]);
    assert_eq!(matrix.get(0, 0), Some(false));
    assert_eq!(matrix.get(2, 2), Some(true));
    // Out of range.
    assert!(matrix.get(3, 0).is_none());
    assert!(matrix.get(0, 3).is_none());
    assert!(matrix.get(3, 3).is_none());
}

#[test]
fn test_matrix_rows() {
    let ref_rows = [
        [false, false, true],
        [false, true, false],
        [true, false, false],
    ];
    let matrix = Matrix::from(ref_rows);
    // Extract the rows by index and compare.
    for (i, ref_row) in ref_rows.iter().enumerate() {
        assert_eq!(matrix.row(i).unwrap(), &ref_row[..]);
    }
    // Get a rows iterator and compare.
    let rows = matrix.rows();
    assert_eq!(rows.len(), 3);
    for (i, row) in rows.enumerate() {
        assert_eq!(row, &ref_rows[i][..]);
    }
}

#[test]
fn test_matrix_transpose() {
    // Basic transposing.
    let mut matrix = Matrix::from([
        [false, true, true],
        [false, false, false],
        [true, true, false],
    ]);
    let exp_transposed = [
        [false, false, true],
        [true, false, true],
        [true, false, false]
    ];
    matrix.transpose();
    for (i, row) in matrix.rows().enumerate() {
        assert_eq!(row, &exp_transposed[i][..], "comparison of row {i} failed");
    }
    // Empty matrix must not panic nor change anything in the data.
    let mut matrix = Matrix::filled(false, 0);
    matrix.transpose();
    assert_eq!(matrix.size(), 0);
    assert!(matrix.get(0, 0).is_none());
    // 1x1 matrix must not panic nor do anything strange.
    let mut matrix = Matrix::filled(false, 1);
    matrix.transpose();
    assert_eq!(matrix.size(), 1);
    assert_eq!(matrix.get(0, 0), Some(false));
}

#[test]
fn test_matrix_fill() {
    // Basic example.
    let mut matrix = Matrix::filled(Module::Dark, 3);
    matrix.fill_rect(Module::Light, 1, 1, 2, 2);
    assert_eq!(matrix, Matrix::from([
        [Module::Dark, Module::Dark, Module::Dark],
        [Module::Dark, Module::Light, Module::Light],
        [Module::Dark, Module::Light, Module::Light],
    ]));
    // Fill with start points inside the matrix, but width/height too big. No effect.
    matrix.fill_rect(Module::Dark, 2, 2, 100, 100);
    assert_eq!(matrix, Matrix::from([
        [Module::Dark, Module::Dark, Module::Dark],
        [Module::Dark, Module::Light, Module::Light],
        [Module::Dark, Module::Light, Module::Light],
    ]));
    // Fill with the start point outside the matrix. No effect.
    matrix.fill_rect(Module::Light, 100, 100, 100, 100);
    assert_eq!(matrix, Matrix::from([
        [Module::Dark, Module::Dark, Module::Dark],
        [Module::Dark, Module::Light, Module::Light],
        [Module::Dark, Module::Light, Module::Light],
    ]));
    // Empty matrix.
    let mut matrix = Matrix::filled(Module::Light, 0);
    matrix.fill_rect(Module::Dark, 0, 0, 1, 1);
}
