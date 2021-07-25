use std::error::Error;

use docvim_lexer::lua::KeywordKind::*;
use docvim_lexer::lua::NameKind::*;
use docvim_lexer::lua::TokenKind::*;
use docvim_lexer::lua::{Lexer, Token};

// TODO these node types will eventually wind up in another file, and end up referring specifically
// to Lua, but for now developing them "in situ".

/// Root AST node for a compilation unit (eg. a file).
pub struct Chunk {
    pub block: Block,
}

pub struct Block {
    pub statements: Vec<Statement>,
}

pub enum Exp {
    Number,
}

pub struct LocalDeclaration<'a> {
    pub namelist: Vec<Name<'a>>,
    pub explist: Vec<Exp>,
}

pub struct Name<'a> {
    pub text: &'a str,
}

pub struct Number<'a> {
    pub text: &'a str,
}

pub enum Statement {
    LocalDeclaration,
}

// pub struct ParserError {
//     pub kind: ParserErrorKind,
// }

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    ast: Chunk,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input),
            ast: Chunk {
                block: Block { statements: vec![] },
            },
        }
    }

    pub fn parse(&mut self) -> Result<(), Box<dyn Error>> {
        loop {
            let result = self.lexer.next();
            match result {
                Some(Ok(
                    token
                    @
                    Token {
                        kind: Name(Keyword(Local)),
                        ..
                    },
                )) => {
                    self.parse_local();
                }
                Some(Ok(token)) => {
                    println!("token: {:?}", token);
                }
                Some(Err(err)) => {
                    return Err(Box::new(err));
                }
                None => {
                    return Ok(());
                }
            }
        }
    }

    pub fn parse_local(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_a_statement() {
        let ast = Parser::new("local x = 1").parse();

        assert_eq!(ast.unwrap(), ());

        // chunk [
        //   statement = LocalDeclaration,
        //   ...
        // ]
        //
        // where LocalDeclaration
        //      namelist = (optional) explist
        //
        // where namelist = [Name, ...]
        //       explist = [exp, ...]
        //
        // where exp = lots of things, but we'll start with... number
    }
}
