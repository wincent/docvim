use std::error::Error;

use docvim_lexer::lua::KeywordKind::*;
use docvim_lexer::lua::NameKind::*;
use docvim_lexer::lua::TokenKind::*;
use docvim_lexer::lua::{Lexer, Token, Tokens};

// TODO these node types will eventually wind up in another file, and end up referring specifically
// to Lua, but for now developing them "in situ".

/// Root AST node for a compilation unit (eg. a file, in the case of docvim; in other contexts,
/// could also be a string to be dynamically compiled and evaluated by the Lua virtual machine).
pub struct Chunk<'a> {
    pub block: Block<'a>,
}

pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}

pub enum Exp {
    Number,
}

// pub struct LocalDeclaration<'a> {
//     pub namelist: Vec<Name<'a>>,
//     pub explist: Vec<Exp>,
// }

pub struct Name<'a> {
    pub text: &'a str,
}

pub struct Number<'a> {
    pub text: &'a str,
}

pub enum Statement<'a> {
    LocalDeclaration {
        namelist: Vec<Name<'a>>,
        explist: Vec<Exp>,
    },
}

// pub struct ParserError {
//     pub kind: ParserErrorKind,
// }

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    ast: Chunk<'a>,
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

    pub fn parse(&self) -> Result<(), Box<dyn Error>> {
        let mut tokens = self.lexer.tokens().peekable();
        loop {
            if let Some(&result) = tokens.peek() {
                match result {
                    Ok(
                        token
                        @
                        Token {
                            kind: Name(Keyword(Local)),
                            ..
                        },
                    ) => {
                        self.parse_local(); //&tokens);
                    }
                    Ok(token) => {
                        println!("token: {:?}", token);
                    }
                    Err(err) => {
                        return Err(Box::new(err));
                    }
                }
            } else {
                return Ok(());
            }
            tokens.next(); // TODO: move this
        }
    }

    fn parse_local(&self) { //, tokens: &std::iter::Peekable<Tokens>) {
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
