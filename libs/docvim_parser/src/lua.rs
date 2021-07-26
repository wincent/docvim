use std::error;
use std::error::Error;
use std::fmt;

use docvim_lexer::lua::KeywordKind;
use docvim_lexer::lua::NameKind;
use docvim_lexer::lua::TokenKind;
use docvim_lexer::lua::{Lexer, Token, Tokens};

// TODO these node types will eventually wind up in another file, and end up referring specifically
// to Lua, but for now developing them "in situ".

/// Root AST node for a compilation unit (eg. a file, in the case of docvim; in other contexts,
/// could also be a string to be dynamically compiled and evaluated by the Lua virtual machine).
#[derive(Debug)]
pub struct Chunk<'a>(Block<'a>);

#[derive(Debug)]
pub struct Block<'a>(Vec<Statement<'a>>);

#[derive(Debug)]
pub enum Exp {
    Number,
}

// pub struct LocalDeclaration<'a> {
//     pub namelist: Vec<Name<'a>>,
//     pub explist: Vec<Exp>,
// }

#[derive(Debug)]
pub struct Name<'a>(&'a str);

pub struct Number<'a>(&'a str);

#[derive(Debug)]
pub enum Statement<'a> {
    LocalDeclaration {
        namelist: Vec<Name<'a>>,
        explist: Vec<Exp>,
    },
}

#[derive(Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    // TODO: position info etc?
}

impl fmt::Display for ParserError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.kind.to_str())
    }
}
impl error::Error for ParserError {}

#[derive(Debug)]
pub enum ParserErrorKind {
    LocalDeclarationWithoutName,
}

impl ParserErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            ParserErrorKind::LocalDeclarationWithoutName => "local declaration without name",
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    ast: Chunk<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input),
            ast: Chunk(Block(vec![])),
        }
    }

    pub fn parse(&mut self) -> Result<(), Box<dyn Error>> {
        let mut tokens = self.lexer.tokens().peekable();
        loop {
            if let Some(&result) = tokens.peek() {
                match result {
                    Ok(
                        token
                        @
                        Token {
                            kind: TokenKind::Name(NameKind::Keyword(KeywordKind::Local)),
                            ..
                        },
                    ) => {
                        let node = self.parse_local(&mut tokens)?;
                        println!("{:?}", node);
                        self.ast.0.0.push(node);
                    }
                    Ok(token) => {
                        // println!("token: {:?}", token);
                        tokens.next(); // TODO: move this
                    }
                    Err(err) => {
                        return Err(Box::new(err));
                    }
                }
            } else {
                // TODO: return the AST
                println!("ast: {:?}", self.ast);
                return Ok(());
            }
        }
    }

    // passing in tokens as a param because I can't seem to read it off self (because it won't
    // let me do a mutable borrow off self).
    fn parse_local(
        &self,
        tokens: &mut std::iter::Peekable<Tokens>,
    ) -> Result<Statement<'a>, ParserError> {
        tokens.next();
        // TODO: loop properly here, detect destructuring, assignments etc
        let node = tokens.next();
        match node {
            Some(Ok(
                token
                @
                Token {
                    kind: TokenKind::Name(NameKind::Identifier),
                    ..
                },
            )) => {
                return Ok(Statement::LocalDeclaration {
                    explist: vec![],
                    namelist: vec![Name(&self.lexer.input[token.byte_start..token.byte_end])],
                });
            }
            // could also just do UnexpectedEndOfInput
            /*None*/
            _ => {
                return Err(ParserError {
                    kind: ParserErrorKind::LocalDeclarationWithoutName,
                });
            }
        }
        // self.ast.block.statements.push(
        //     Statement::LocalDeclaration {
        //         namelist: vec![],
        //         explist: vec![],
        //     }
        // )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_a_statement() {
        let ast = Parser::new("local foo").parse();
        assert_eq!(ast.unwrap(), ());

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
