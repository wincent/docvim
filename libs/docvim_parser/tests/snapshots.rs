use docvim_macros::check_snapshots;
use docvim_parser::lua::Parser;

#[check_snapshots("libs/docvim_parser/tests/snapshots")]
fn transform(input: &str) -> String {
    let mut parser = Parser::new(input);
    match parser.parse() {
        Ok(ast) => format!("{:#?}", ast),
        Err(error) => parser.pretty_error(error),
    }
}
