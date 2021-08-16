use std::error::Error;
use std::fs;
use std::path::Path;

use docvim_diff::format;
use docvim_diff::histogram::diff;

/// Divider consisting of 72 downward-pointing arrows with a blank line before and after. The
/// string "OUTPUT" appears in the middle to make it easy to jump to the divider using search.
const DIVIDER: &str =
    "\n\n↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ OUTPUT ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓\n\n";

/// Convenience macro for checking a snapshot relative to the current package.
#[macro_export]
macro_rules! check_snapshot {
    ($path:expr, $callback:expr) => {
        docvim_snapshot::check_snapshot_relative_to_base(
            $path,
            std::env!("CARGO_MANIFEST_DIR"),
            $callback,
        )
    };
}

pub fn check_snapshot_relative_to_base(
    path: &str,
    base: &str,
    callback: &dyn Fn(&str) -> String,
) -> Result<bool, Box<dyn Error>> {
    // Read snapshot file.
    let mut snapshot = Path::new(base).to_path_buf();
    snapshot.push("tests/snapshots");
    snapshot.push(Path::new(path));
    snapshot.set_extension("snap");
    let snapshot = snapshot.as_path();
    let contents = fs::read_to_string(snapshot)?;

    // Extract input and expected output.
    // TODO: think about whether to slurp and/or require blank lines around divider.
    if let Some(divider_idx) = contents.find(DIVIDER) {
        let output_idx = divider_idx + DIVIDER.len();
        let input = contents[0..divider_idx].trim();
        let expected = contents[output_idx..contents.len()].trim();
        let transformed = String::from(callback(&input).trim_end());

        if option_env!("UPDATE_SNAPSHOTS").is_some() {
            let mut updated = String::from(input);
            updated.push_str(DIVIDER);
            updated.push_str(&transformed);
            updated.push('\n');
            fs::write(snapshot, updated)?;
            Ok(true)
        } else if expected == transformed {
            Ok(true)
        } else {
            // TODO: maybe don't use lines(); do i actually want to keep the trailing line endings?
            let expected_lines = expected.lines().collect::<Vec<&str>>();
            let transformed_lines = transformed.lines().collect::<Vec<&str>>();

            // Show what change would be applied if we updated the snapshots.
            let ses = diff(&expected_lines, &transformed_lines);
            let formatted = format(ses, &expected_lines, &transformed_lines);
            println!("{}", formatted);

            println!("If output is correct, re-run with UPDATE_SNAPSHOTS=1");
            Ok(false)
        }
    } else {
        // No divider found, so assume this is a new snapshot that we need to fill out.
        let mut input = String::from(contents.trim());
        let transformed = String::from(callback(&input).trim_end());
        input.push_str(DIVIDER);
        input.push_str(&transformed);
        input.push('\n');
        fs::write(snapshot, input)?;
        Ok(true)
    }
}
