use std::error::Error;

use docvim_parser::lua::Parser;
use docvim_snapshot::check_snapshot;

fn transform(input: &str) -> String {
    let mut parser = Parser::new(input);
    let ast = parser.parse();
    format!("{:#?}", ast)
}

// TODO: given that these are all basically the same, make a macro (or fn) for just running all the
// snapshot tests
//
// check_all_snapshots!();

#[test]
fn test_parses_index_expressions() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot!("index_expressions", &transform)?);
    Ok(())
}

#[test]
fn test_parses_unary_expressions() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot!("unary_expressions", &transform)?);
    Ok(())
}

#[test]
fn test_parses_a_complex_expression() -> Result<(), Box<dyn Error>> {
    assert!(check_snapshot!("complex_expression", &transform)?);
    Ok(())
}
