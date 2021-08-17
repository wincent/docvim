use docvim_parser::lua::Parser;
use docvim_snapshot::check_snapshots;

#[check_snapshots(docvim_parser)]
fn transform(input: &str) -> String {
    let mut parser = Parser::new(input);
    let ast = parser.parse();
    format!("{:#?}", ast)
}
