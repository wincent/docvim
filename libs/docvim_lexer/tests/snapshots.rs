use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;

use docvim_diff::format;
use docvim_diff::histogram::diff;
use docvim_lexer::lua::Lexer;

/// Divider consisting of 72 downward-pointing arrows with a blank line before and after. The
/// string "OUTPUT" appears in the middle to make it easy to jump to the divider using search.
const DIVIDER: &str =
    "\n\n↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ OUTPUT ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓\n\n";

fn check_snapshot(path: &str, callback: &dyn Fn(&str) -> String) -> Result<bool, Box<dyn Error>> {
    // Read snapshot file.
    let mut snapshot = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
    snapshot.push("tests/snapshots");
    snapshot.push(Path::new(path));
    snapshot.set_extension("snap");
    let snapshot = snapshot.as_path();
    let contents = fs::read_to_string(snapshot).expect("unreadable snapshot");

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
        panic!("Couldn't find divider");
    }
}

fn transform(input: &str) -> String {
    let mut output = vec![];
    for result in Lexer::new(input).tokens() {
        output.push(match result {
            Ok(token) => {
                format!("{:?}: {}", token, &input[token.byte_start..token.byte_end])
            }

            // Note we stringify errors too, so that we can test bad inputs.
            Err(err) => err.to_string(),
        });
    }
    output.join("\n")
}

// TODO: once this is sorted, move it into a crate so it can be used in parser too
// TODO: see if we can make this more pleasant with a macro... (and have it print the right line
// numbers etc
#[test]
fn test_lexes_numbers() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot("numbers", &transform)?);
    Ok(())
}

// TODO: this is just the main file from corpus; add subdirectories and ability to update easily
// without having to manage .snap files by hand
#[test]
fn test_lexes_corpus() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot("corpus", &transform)?);
    Ok(())
}
