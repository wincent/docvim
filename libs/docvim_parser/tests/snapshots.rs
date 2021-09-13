use docvim_macros::check_snapshots;
use docvim_parser::lua::Parser;

#[check_snapshots(docvim_parser)]
fn transform(input: &str) -> String {
    let parser = Parser::new(input);
    match parser.parse() {
        Ok(ast) => format!("{:#?}", ast),
        Err(error) => parser.pretty_error(error),
    }
}
