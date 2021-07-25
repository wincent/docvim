// use std::error::Error;

use docvim_lexer::lua::KeywordKind::*;
use docvim_lexer::lua::NameKind::*;
use docvim_lexer::lua::TokenKind::*;
use docvim_lexer::lua::{Lexer, LexerError, LexerErrorKind, Token};

// TODO these node types will eventually wind up in another file, and end up referring specifically
// to Lua, but for now developing them "in situ".
pub struct Chunk {
    pub statments: Vec<Statement>,
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
    input: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    pub fn parse(&self) -> Result<(), LexerError> {
        let mut lexer = Lexer::new(self.input);
        loop {
            let result = lexer.next_token();
            match result {
                Ok(
                    token
                    @
                    Token {
                        kind: Name(Keyword(Local)),
                        ..
                    },
                ) => {
                    println!("[local]: {:?}", token);
                }
                Ok(token) => {
                    println!("token: {:?}", token);
                }
                Err(LexerError {
                    kind: LexerErrorKind::EndOfInput,
                    ..
                }) => {
                    return Ok(());
                }
                Err(err) => {
                    return Err(err);
                }
            }

            // match token {
            // Lexer::Token({kind: Lexer::KeywordKind}) => {}
            // _ => {}
            // }
        }
    }
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
