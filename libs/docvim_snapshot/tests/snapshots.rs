use std::env::{remove_var, set_var};
use std::error::Error;

use docvim_snapshot::{check_snapshot, UPDATE_SNAPSHOTS};

/// Demo function that uppercases its input.
fn transform(input: &str) -> String {
    input.to_uppercase()
}

#[test]
fn test_check_snapshot_macro_with_matching_snapshot() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot!("valid_sample", &transform)?);
    Ok(())
}

#[test]
fn test_check_snapshot_macro_with_mismatching_snapshot() {
    // Save/clear UPDATE_SNAPSHOTS.
    let env = option_env!("UPDATE_SNAPSHOTS");
    remove_var(UPDATE_SNAPSHOTS);

    // Do actual test.
    let snapshot_mismatch = matches!(check_snapshot!("invalid_sample", &transform), Ok(false));

    // Restore UPDATE_SNAPSHOTS.
    match env {
        Some(value) => set_var(UPDATE_SNAPSHOTS, value),
        None => (),
    }

    assert!(snapshot_mismatch);
}

#[test]
fn test_check_snapshot_macro_in_subdirectory() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot!("subdirectory/sample", &transform)?);
    Ok(())
}
