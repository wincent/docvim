use crate::LuaParser;
use docvim_macros::check_snapshots;

#[check_snapshots("libs/docvim_parser/tests/lua/snapshots")]
fn transform(input: &str) -> String {
    let mut parser = LuaParser::new(input);
    match parser.parse() {
        Ok(ast) => format!("{:#?}", ast),
        Err(error) => parser.pretty_error(error),
    }
}
