use std::error::Error;

use docvim_parser::lua::Parser;
use docvim_snapshot::check_snapshot;

fn transform(input: &str) -> String {
    let mut parser = Parser::new(input);
    let ast = parser.parse();
    format!("{:#?}", ast)
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
