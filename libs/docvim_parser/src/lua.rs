use std::error;
use std::error::Error;
use std::fmt;

// Lexer token types are imported aliased to avoid collisions with parser node types of the same
// name.
use docvim_lexer::lua::KeywordKind::Local as LocalToken;
use docvim_lexer::lua::LiteralKind::Number as NumberToken;
use docvim_lexer::lua::NameKind::Identifier as IdentifierToken;
use docvim_lexer::lua::NameKind::Keyword as KeywordToken;
use docvim_lexer::lua::OpKind::Assign as AssignToken;
use docvim_lexer::lua::PunctuatorKind::Comma as CommaToken;
use docvim_lexer::lua::PunctuatorKind::Semi as SemiToken;
use docvim_lexer::lua::TokenKind::Literal as LiteralToken;
use docvim_lexer::lua::TokenKind::Name as NameToken;
use docvim_lexer::lua::TokenKind::Op as OpToken;
use docvim_lexer::lua::TokenKind::Punctuator as PunctuatorToken;
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
pub enum Exp<'a> {
    // Number,
    Number(&'a str),
}

// pub struct LocalDeclaration<'a> {
//     pub namelist: Vec<Name<'a>>,
//     pub explist: Vec<Exp>,
// }

#[derive(Debug)]
pub struct Name<'a>(&'a str);

// pub struct Number<'a>(&'a str);

#[derive(Debug)]
pub enum Statement<'a> {
    LocalDeclaration {
        namelist: Vec<Name<'a>>,
        explist: Vec<Exp<'a>>,
    },
}

#[derive(Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub position: usize,
}

impl fmt::Display for ParserError {
    // TODO include position info as well.
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.kind.to_str())
    }
}
impl error::Error for ParserError {}

#[derive(Debug)]
pub enum ParserErrorKind {
    LocalDeclarationWithoutName,
    UnexpectedToken, // TODO: this is a bit of a catch-all; replace with more specific things
}

impl ParserErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            ParserErrorKind::LocalDeclarationWithoutName => "local declaration without name",
            ParserErrorKind::UnexpectedToken => "unexpected token",
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
                            kind: NameToken(KeywordToken(LocalToken)),
                            ..
                        },
                    ) => {
                        let node = self.parse_local(&mut tokens)?;
                        // println!("{:?}", node);
                        self.ast.0 .0.push(node);
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
    ) -> Result<Statement<'a>, Box<dyn Error>> {
        let mut previous = tokens.next().unwrap().ok().unwrap(); // Consume "local" keyword token.

        // Example inputs:
        //
        // local x
        // local x = 10
        // local x, y = 10, 20
        //
        // -- and (can do this later)...
        // local function Name funcbody
        //
        // Spelling this all out verbosely now, until I find the right abstraction for it...
        let mut explist: Vec<Exp> = Vec::new();
        let mut namelist = Vec::new();
        let mut expect_assign = false;
        let mut expect_comma = false;
        let mut expect_name = true;
        let mut expect_semi = false;
        loop {
            if let Some(&token) = tokens.peek() {
                previous = token.ok().unwrap();
                match token {
                    Ok(
                        token
                        @
                        Token {
                            kind: NameToken(IdentifierToken),
                            ..
                        },
                    ) => {
                        if expect_name {
                            tokens.next();
                            namelist
                                .push(Name(&self.lexer.input[token.byte_start..token.byte_end]));
                            expect_assign = true;
                            expect_comma = true;
                            expect_name = false;
                            expect_semi = true;
                        } else {
                            return Ok(Statement::LocalDeclaration {
                                explist: vec![],
                                namelist,
                            });
                        }
                    }
                    Ok(
                        token
                        @
                        Token {
                            kind: OpToken(AssignToken),
                            ..
                        },
                    ) => {
                        if expect_assign {
                            tokens.next();
                            break;
                        } else {
                            return Err(Box::new(ParserError {
                                kind: ParserErrorKind::UnexpectedToken,
                                position: token.char_start,
                            }));
                        }
                    }
                    Ok(
                        token
                        @
                        Token {
                            kind: PunctuatorToken(CommaToken),
                            ..
                        },
                    ) => {
                        if expect_comma {
                            tokens.next();
                            expect_assign = false;
                            expect_comma = false;
                            expect_name = true;
                            expect_semi = false;
                        } else {
                            return Err(Box::new(ParserError {
                                kind: ParserErrorKind::UnexpectedToken,
                                position: token.char_start,
                            }));
                        }
                    }
                    Ok(
                        token
                        @
                        Token {
                            kind: PunctuatorToken(SemiToken),
                            ..
                        },
                    ) => {
                        if expect_semi {
                            tokens.next();
                            return Ok(Statement::LocalDeclaration {
                                explist: vec![],
                                namelist,
                            });
                        } else {
                            return Err(Box::new(ParserError {
                                kind: ParserErrorKind::UnexpectedToken,
                                position: token.char_start,
                            }));
                        }
                    }
                    Ok(token) => {
                        if !expect_comma {
                            // Error, because we must have just seen a comma but didn't see a name.
                            return Err(Box::new(ParserError {
                                kind: ParserErrorKind::UnexpectedToken,
                                position: token.char_start,
                            }));
                        }
                        break;
                    }
                    Err(err) => {
                        return Err(Box::new(err));
                    }
                }
            } else {
                // Reached end of input.
                if expect_comma {
                    return Ok(Statement::LocalDeclaration { explist, namelist });
                } else {
                    // Error, because we must have just seen a comma but didn't see a name.
                    return Err(Box::new(ParserError {
                        kind: ParserErrorKind::UnexpectedToken,
                        position: previous.char_start,
                    }));
                }
            }
        }

        // Note that Lua doesn't require the number of items on LHS and RHS to match.
        expect_comma = false;
        expect_name = true;
        expect_semi = false;
        loop {
            if let Some(&token) = tokens.peek() {
                match token {
                    Ok(
                        token
                        @
                        Token {
                            kind: LiteralToken(NumberToken),
                            ..
                        },
                    ) => {
                        if expect_name {
                            tokens.next();
                            // TODO: see if I can make this Number instead of Exp::Number
                            explist.push(Exp::Number(
                                &self.lexer.input[token.byte_start..token.byte_end],
                            ));
                            expect_comma = true;
                            expect_name = false;
                            expect_semi = true;
                        } else {
                            return Ok(Statement::LocalDeclaration { explist, namelist });
                        }
                    }
                    Ok(
                        token
                        @
                        Token {
                            kind: PunctuatorToken(CommaToken),
                            ..
                        },
                    ) => {
                        if expect_comma {
                            tokens.next();
                            expect_comma = false;
                            expect_name = true;
                            expect_semi = false;
                        } else {
                            return Err(Box::new(ParserError {
                                kind: ParserErrorKind::UnexpectedToken,
                                position: token.char_start,
                            }));
                        }
                    }
                    Ok(
                        token
                        @
                        Token {
                            kind: PunctuatorToken(SemiToken),
                            ..
                        },
                    ) => {
                        if expect_semi {
                            tokens.next();
                            return Ok(Statement::LocalDeclaration { explist, namelist });
                        } else {
                            return Err(Box::new(ParserError {
                                kind: ParserErrorKind::UnexpectedToken,
                                position: token.char_start,
                            }));
                        }
                    }
                    Ok(token) => {
                        if !expect_comma {
                            // Error, because we must have just seen a comma but didn't see a name.
                            return Err(Box::new(ParserError {
                                kind: ParserErrorKind::UnexpectedToken,
                                position: token.char_start,
                            }));
                        }
                        break;
                    }
                    Err(err) => {
                        return Err(Box::new(err));
                    }
                }
            } else {
                // Reached end of input.
                if !expect_comma {
                    // Error, because we must have just seen a comma (or an assignment operator)
                    // but didn't see a name.
                    return Err(Box::new(ParserError {
                        kind: ParserErrorKind::UnexpectedToken,
                        position: previous.char_start,
                    }));
                }
                break;
            }
        }

        return Ok(Statement::LocalDeclaration { explist, namelist });
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
