use qrab::*;

#[test]
/// Check that an encoder will choose the highest possible error correction level after setting the version.
fn test_choosing_max_ecl() {
    let data = "Hello, world!";
    // Unconstrained: this must result in a 1M.
    let qrcode = Encoder::new().encode(data).unwrap();
    assert_eq!(qrcode.version(), Version::V1);
    assert_eq!(qrcode.ecl(), Ecl::M);
    // Force version 2: this leaves enough space for a H ECL.
    let qrcode = Encoder::with_constraints(EncodingConstraints::new().with_version(Version::V2))
        .encode(data)
        .unwrap();
    assert_eq!(qrcode.version(), Version::V2);
    assert_eq!(qrcode.ecl(), Ecl::H);
    // Check that forcing version 2 as well as a cap on the ECL works as expected.
    let qrcode = Encoder::with_constraints(
        EncodingConstraints::new()
            .with_version(Version::V2)
            .with_ecl_in(..=Ecl::Q),
    )
    .encode(data)
    .unwrap();
    assert_eq!(qrcode.version(), Version::V2);
    assert_eq!(qrcode.ecl(), Ecl::Q);
}
