use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;

use docvim_lexer::lua::Lexer;

/// 72 downward-pointing arrows with a blank line before and after.
const DIVIDER: &str = "\n\n↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓\n\n";

fn check_snapshot(
    path: &str,
    callback: &dyn Fn(&str) -> String
) -> Result<bool, Box<dyn Error>> {
    // Read snapshot file.
    let mut snapshot = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
    snapshot.push("tests/snapshots");
    snapshot.push(Path::new(path));
    snapshot.set_extension("snap");
    let snapshot = snapshot.as_path();
    let contents = fs::read_to_string(snapshot).expect("unreadable snapshot");

    // Extract input and expected output.
    // TODO: think about whether to slurp and/or require blank lines around divider.
    let mut iter = contents.char_indices().peekable();
    let mut divider_idx = None;
    let mut last_idx = 0;
    while let Some((i, ch)) = iter.next() {
        if divider_idx.is_none() {
            if ch == '\n' {
                let mut count = 0;
                while let Some(&(_, ch)) = iter.peek() {
                    if ch == '↓' {
                        iter.next();
                        count += 1;
                    } else {
                        break;
                    }
                }
                if count > 5 {
                    if let Some(&(j, ch)) = iter.peek() {
                        if ch == '\n' {
                            iter.next();
                            divider_idx = Some((i, j));
                        }
                    }
                }
            }
        } else {
            last_idx = i;
        }
    }
    if let Some((divider_start, divider_end)) = divider_idx {
        let input = contents[0..divider_start].trim();
        let expected = contents[divider_end..last_idx].trim();
        let transformed = String::from(callback(&input).trim_end());

        if expected == transformed {
            Ok(true)
        } else if option_env!("UPDATE_SNAPSHOTS").is_some() {
            let mut updated = String::from(input);
            updated.push_str(DIVIDER);
            updated.push_str(&transformed);
            updated.push('\n');
            println!("WOULD WRITE:\n{}", updated);
            fs::write(snapshot, updated)?;
            Ok(true)
        } else {
            // TODO report differences properly
            println!("INPUT:\n{}", input);
            println!("EXPECTED OUTPUT:\n{}", expected);
            println!("ACTUAL OUTPUT:\n{}", transformed);
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
fn test_lexes_numbers() {
    if let Ok(result) = check_snapshot("numbers", &transform) {
        assert!(result);
    } else {
        panic!("unexpected error");
    }
}
