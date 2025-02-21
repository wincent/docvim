use docvim_lexer::markdown::Lexer;
use docvim_macros::check_snapshots;

#[check_snapshots(libs/docvim_lexer/tests/markdown/snapshots)]
fn transform(input: &str) -> String {
    let mut output = vec![];
    for result in Lexer::new(input).tokens {
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
