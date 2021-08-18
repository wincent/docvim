use std::error::Error;

use docvim_snapshot::check_snapshot;

/// Demo function that uppercases its input.
fn transform(input: &str) -> String {
    input.to_uppercase()
}

#[test]
fn test_check_snapshot_macro() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot!("sample", &transform)?);
    Ok(())
}
