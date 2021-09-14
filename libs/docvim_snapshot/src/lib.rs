use std::error::Error;
use std::fs;
use std::path::Path;

use docvim_diff::format;
use docvim_diff::histogram::diff;

/// Divider consisting of 72 downward-pointing arrows with a blank line before and after. The
/// string "OUTPUT" appears in the middle to make it easy to jump to the divider using search.
const DIVIDER: &str =
    "\n\n↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ OUTPUT ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓\n\n";

/// Environment variable that can be set (to any value) to force `check_snapshot()` to update the
/// snapshot files on disk.
pub const UPDATE_SNAPSHOTS: &str = "UPDATE_SNAPSHOTS";

/// Convenience macro for checking a snapshot relative to the current package.
///
/// Mostly superseded by the `#[check_snapshots]` procedural macro defined in the docvim_macros
/// package.
#[macro_export]
macro_rules! check_snapshot {
    ($path:expr, $callback:expr) => {
        docvim_snapshot::check_snapshot_relative_to_base(
            $path,
            std::env!("CARGO_MANIFEST_DIR"),
            $callback,
            false,
        )
    };
}

/// Really only used in the test_suite (ignores effect of UPDATE_SNAPSHOTS).
#[macro_export]
macro_rules! check_snapshot_dry_run {
    ($path:expr, $callback:expr) => {
        docvim_snapshot::check_snapshot_relative_to_base(
            $path,
            std::env!("CARGO_MANIFEST_DIR"),
            $callback,
            true,
        )
    };
}

pub fn check_snapshot_relative_to_base(
    path: &str,
    base: &str,
    callback: &dyn Fn(&str) -> String,
    dry_run: bool,
) -> Result<bool, Box<dyn Error>> {
    let mut snapshot = Path::new(base).to_path_buf();
    snapshot.push("tests/snapshots");
    snapshot.push(Path::new(path));
    snapshot.set_extension("snap");
    check_snapshot(snapshot.as_path(), callback, dry_run)
}

pub fn check_snapshot(
    snapshot: &Path,
    callback: &dyn Fn(&str) -> String,
    dry_run: bool,
) -> Result<bool, Box<dyn Error>> {
    // Read snapshot file.
    let contents = fs::read_to_string(snapshot)?;

    // Extract input and expected output.
    if let Some(divider_idx) = contents.find(DIVIDER) {
        let output_idx = divider_idx + DIVIDER.len();
        let input = &contents[0..divider_idx];
        let expected = &contents[output_idx..contents.len()];
        let mut transformed = String::from(callback(&input));
        if !transformed.ends_with("\n") {
            transformed.push('\n');
        }

        if std::env::var_os(UPDATE_SNAPSHOTS).is_some() && !dry_run {
            let mut updated = String::from(input);
            updated.push_str(DIVIDER);
            updated.push_str(&transformed);
            fs::write(snapshot, updated)?;
            Ok(true)
        } else if expected == transformed {
            Ok(true)
        } else {
            let expected_lines = expected.lines().collect::<Vec<&str>>();
            let transformed_lines = transformed.lines().collect::<Vec<&str>>();

            // Show what change would be applied if we updated the snapshots.
            let ses = diff(&expected_lines, &transformed_lines);
            let formatted = format(ses, &expected_lines, &transformed_lines);
            println!("{}", formatted);

            println!("If output is correct, re-run with {}=1", UPDATE_SNAPSHOTS);
            Ok(false)
        }
    } else {
        // No divider found, so assume this is a new snapshot that we need to fill out.
        let mut input = String::from(contents);
        let transformed = String::from(callback(&input));
        input.push_str(DIVIDER);
        input.push_str(&transformed);

        if !transformed.ends_with("\n") {
            input.push('\n');
        }
        if !dry_run {
            fs::write(snapshot, input)?;
        }
        Ok(true)
    }
}
