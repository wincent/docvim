use std::error::Error;

use docvim_lexer::lua::KeywordKind::*;
use docvim_lexer::lua::NameKind::*;
use docvim_lexer::lua::TokenKind::*;
use docvim_lexer::lua::{Lexer, LexerError, LexerErrorKind, Token};

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
            // TODO: make lexer implement Iterator trait after all?, as we are going to want to
            // peek at tokens much like the lexer needs to peek at characters.

            // TODO: think about changing return type to: Option<Result<Token, LexerError>>
            // so I can just use iterator trait:
            // let result = self.lexer.next();
            // ie. None = end of input reached
            //     Some(Err(...)) = something went wrong
            //     Some(Ok(Token)) = all good
            //
            // that way, i am not overloading Errors to mean "something went wrong OR i got to the
            // end just fine)
            let result = self.lexer.next_token();
            match result {
                Ok(
                    token
                    @
                    Token {
                        kind: Name(Keyword(Local)),
                        ..
                    },
                ) => {
                    self.parse_local();
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
                    return Err(Box::new(err));
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
