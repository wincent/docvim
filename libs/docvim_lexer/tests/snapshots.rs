use std::error::Error;

use docvim_lexer::lua::Lexer;
use docvim_snapshot::check_snapshot;

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

#[test]
fn test_lexes_index_expressions() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot!("index_expressions", &transform)?);
    Ok(())
}

#[test]
fn test_lexes_numbers() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot!("numbers", &transform)?);
    Ok(())
}

// TODO: this is just the main file from corpus; add subdirectories and ability to update easily
// without having to manage .snap files by hand
#[test]
fn test_lexes_corpus() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot!("corpus", &transform)?);
    Ok(())
}
