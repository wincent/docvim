use docvim_lexer::markdown::Lexer;
use docvim_macros::check_snapshots;

#[check_snapshots("libs/docvim_lexer/tests/markdown/snapshots")]
fn transform(input: &str) -> String {
    let mut output = vec![];
    for result in Lexer::new(input).tokens {
        output.push(match result {
            Ok(token) => {
                let text = &input[token.byte_start..token.byte_end];

                // Special case whitespace-only tokens to make them more readable.
                let display_text = if text.trim().is_empty() {
                    format!("\"{}\"", text.escape_default())
                } else {
                    text.to_string()
                };

                format!("{:?}: {}", token, display_text)
            }

            // Note we stringify errors too, so that we can test bad inputs.
            Err(err) => err.to_string(),
        });
    }
    output.join("\n")
}
