use std::error::Error;

use docvim_snapshot::{check_snapshot, check_snapshot_dry_run};

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
    // Use dry-run variant because we never want to update this snapshot, even when
    // UPDATE_SNAPSHOTS is set.
    let snapshot_mismatch = matches!(check_snapshot_dry_run!("invalid_sample", &transform), Ok(false));

    assert!(snapshot_mismatch);
}

#[test]
fn test_check_snapshot_macro_in_subdirectory() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot!("subdirectory/sample", &transform)?);
    Ok(())
}
