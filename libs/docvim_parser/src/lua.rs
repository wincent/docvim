use std::error;
use std::error::Error;
use std::fmt;

// Lexer token types are imported aliased to avoid collisions with parser node types of the same
// name.
use docvim_lexer::lua::KeywordKind::False as FalseToken;
use docvim_lexer::lua::KeywordKind::Local as LocalToken;
use docvim_lexer::lua::KeywordKind::Nil as NilToken;
use docvim_lexer::lua::KeywordKind::True as TrueToken;
use docvim_lexer::lua::LiteralKind::Number as NumberToken;
use docvim_lexer::lua::LiteralKind::Str as StrToken;
use docvim_lexer::lua::NameKind::Identifier as IdentifierToken;
use docvim_lexer::lua::NameKind::Keyword as KeywordToken;
use docvim_lexer::lua::OpKind::Assign as AssignToken;
use docvim_lexer::lua::PunctuatorKind::Comma as CommaToken;
use docvim_lexer::lua::PunctuatorKind::Semi as SemiToken;
use docvim_lexer::lua::StrKind::DoubleQuoted as DoubleQuotedToken;
use docvim_lexer::lua::StrKind::Long as LongToken;
use docvim_lexer::lua::StrKind::SingleQuoted as SingleQuotedToken;
use docvim_lexer::lua::TokenKind::Literal as LiteralToken;
use docvim_lexer::lua::TokenKind::Name as NameToken;
use docvim_lexer::lua::TokenKind::Op as OpToken;
use docvim_lexer::lua::TokenKind::Punctuator as PunctuatorToken;
use docvim_lexer::lua::{Lexer, Token, Tokens};

// TODO these node types will eventually wind up in another file, and end up referring specifically
// to Lua, but for now developing them "in situ".

/// Root AST node for a compilation unit (eg. a file, in the case of docvim; in other contexts,
/// could also be a string to be dynamically compiled and evaluated by the Lua virtual machine).
#[derive(Debug, PartialEq)]
pub struct Chunk<'a>(Block<'a>);

#[derive(Debug, PartialEq)]
pub struct Block<'a>(Vec<Statement<'a>>);

#[derive(Debug, PartialEq)]
pub enum Exp<'a> {
    CookedStr(Box<String>),
    False,
    Nil,
    Number(&'a str),
    RawStr(&'a str),
    True,
}

#[derive(Debug, PartialEq)]
pub struct Name<'a>(&'a str);

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    LocalDeclaration { namelist: Vec<Name<'a>>, explist: Vec<Exp<'a>> },
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
    InvalidEscapeSequence,
    LocalDeclarationWithoutName,
    UnexpectedEndOfInput,
    UnexpectedToken, // TODO: this is a bit of a catch-all; replace with more specific things
}

impl ParserErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            ParserErrorKind::InvalidEscapeSequence => "invalid escape sequence",
            ParserErrorKind::LocalDeclarationWithoutName => "local declaration without name",
            ParserErrorKind::UnexpectedEndOfInput => "unexpected end-of-input",
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
        Self { lexer: Lexer::new(input), ast: Chunk(Block(vec![])) }
    }

    pub fn parse(&mut self) -> Result<&Chunk, Box<dyn Error>> {
        let mut tokens = self.lexer.tokens().peekable();
        while let Some(&result) = tokens.peek() {
            match result {
                Ok(token @ Token { kind: NameToken(KeywordToken(LocalToken)), .. }) => {
                    let node = self.parse_local(&mut tokens)?;
                    self.ast.0 .0.push(node);
                }
                Ok(token) => {
                    tokens.next(); // TODO: move this
                }
                Err(err) => {
                    return Err(Box::new(err));
                }
            }
        }

        Ok(&self.ast)
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
                    Ok(token @ Token { kind: NameToken(IdentifierToken), .. }) => {
                        if expect_name {
                            tokens.next();
                            namelist
                                .push(Name(&self.lexer.input[token.byte_start..token.byte_end]));
                            expect_assign = true;
                            expect_comma = true;
                            expect_name = false;
                            expect_semi = true;
                        } else {
                            return Ok(Statement::LocalDeclaration { explist: vec![], namelist });
                        }
                    }
                    Ok(token @ Token { kind: OpToken(AssignToken), .. }) => {
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
                    Ok(token @ Token { kind: PunctuatorToken(CommaToken), .. }) => {
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
                    Ok(token @ Token { kind: PunctuatorToken(SemiToken), .. }) => {
                        if expect_semi {
                            tokens.next();
                            return Ok(Statement::LocalDeclaration { explist: vec![], namelist });
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
                    Ok(Token { kind: NameToken(KeywordToken(FalseToken)), .. })
                    | Ok(Token { kind: NameToken(KeywordToken(NilToken)), .. })
                    | Ok(Token { kind: LiteralToken(NumberToken), .. })
                    | Ok(Token { kind: LiteralToken(StrToken(_)), .. })
                    | Ok(Token { kind: NameToken(KeywordToken(TrueToken)), .. }) => {
                        if expect_name {
                            explist.push(self.parse_exp(tokens)?);
                            expect_comma = true;
                            expect_name = false;
                            expect_semi = true;
                        } else {
                            return Ok(Statement::LocalDeclaration { explist, namelist });
                        }
                    }
                    Ok(token @ Token { kind: PunctuatorToken(CommaToken), .. }) => {
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
                    Ok(token @ Token { kind: PunctuatorToken(SemiToken), .. }) => {
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

    fn parse_exp(
        &self,
        tokens: &mut std::iter::Peekable<Tokens>,
    ) -> Result<Exp<'a>, Box<dyn Error>> {
        let token = tokens.next().unwrap()?;
        match token {
            Token { kind: NameToken(KeywordToken(FalseToken)), .. } => Ok(Exp::False),
            Token { kind: NameToken(KeywordToken(NilToken)), .. } => Ok(Exp::Nil),
            Token { kind: LiteralToken(NumberToken), .. } => {
                Ok(Exp::Number(&self.lexer.input[token.byte_start..token.byte_end]))
            }
            Token { kind: LiteralToken(StrToken(DoubleQuotedToken)), .. }
            | Token { kind: LiteralToken(StrToken(SingleQuotedToken)), .. } => {
                // TODO: will want to extract this somewhere.
                let byte_start = token.byte_start + 1;
                let byte_end = token.byte_end - 1;
                let char_start = token.char_start + 1;
                let mut escaped = self.lexer.input[byte_start..byte_end].char_indices().peekable();
                let mut unescaped = String::with_capacity(byte_end - byte_start);
                while let Some((idx, ch)) = escaped.next() {
                    let unescaped_char = match ch {
                        '\\' => {
                            if let Some((_, next)) = escaped.next() {
                                match next {
                                    'a' => '\x07', // bell
                                    'b' => '\x08', // backspace
                                    'f' => '\x0c', // line feed
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    'v' => '\x0b', // vertical tab
                                    '\\' => '\\',
                                    '"' => '"',
                                    '\'' => '\'',
                                    '\n' => '\n',
                                    '0'..='9' => {
                                        let mut digits = vec![next as u8];
                                        // TODO: DRY this up; it is a bit similar to what we have in the
                                        // lexer.
                                        let mut digit_count = 1;
                                        while digit_count < 3 {
                                            if let Some(&(_, digit)) = escaped.peek() {
                                                match digit {
                                                    '0'..='9' => {
                                                        digits.push(digit as u8);
                                                        escaped.next();
                                                        digit_count += 1;
                                                    }
                                                    _ => {
                                                        break;
                                                    }
                                                }
                                            } else {
                                                break;
                                            }
                                        }
                                        let number = std::str::from_utf8(&digits)?.parse::<u8>()?;
                                        number as char
                                    }
                                    _ => {
                                        return Err(Box::new(ParserError {
                                            kind: ParserErrorKind::InvalidEscapeSequence,
                                            position: char_start + idx,
                                        }));
                                    }
                                }
                            } else {
                                return Err(Box::new(ParserError {
                                    kind: ParserErrorKind::UnexpectedEndOfInput,
                                    position: token.char_start,
                                }));
                            }
                        }
                        other => other,
                    };
                    unescaped.push(unescaped_char);
                }

                Ok(Exp::CookedStr(Box::new(unescaped)))
            }
            Token { kind: LiteralToken(StrToken(LongToken { level })), .. } => {
                let start = token.byte_start + 2 + level;
                let end = token.byte_end - 2 - level;
                Ok(Exp::RawStr(&self.lexer.input[start..end]))
            }
            Token { kind: NameToken(KeywordToken(TrueToken)), .. } => Ok(Exp::True),

            // TODO: not yet implemented
            _ => Err(Box::new(ParserError {
                kind: ParserErrorKind::UnexpectedToken,
                position: token.char_start,
            })),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_local_declarations() {
        let mut parser = Parser::new("local foo");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("foo")],
                explist: vec![],
            }]))
        );

        let mut parser = Parser::new("local bar = false");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("bar")],
                explist: vec![Exp::False],
            }]))
        );

        let mut parser = Parser::new("local baz = nil");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("baz")],
                explist: vec![Exp::Nil],
            }]))
        );

        let mut parser = Parser::new("local w = 1");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("w")],
                explist: vec![Exp::Number("1")],
            }]))
        );

        let mut parser = Parser::new("local x = 'wat'");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("x")],
                explist: vec![Exp::CookedStr(Box::new("wat".to_string()))],
            }]))
        );

        let mut parser = Parser::new("local y = \"don't say \\\"hello\\\"!\"");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("y")],
                explist: vec![Exp::CookedStr(Box::new("don't say \"hello\"!".to_string()))],
            }]))
        );

        let mut parser = Parser::new("local z = [[loooong]]");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("z")],
                explist: vec![Exp::RawStr("loooong")],
            }]))
        );

        let mut parser = Parser::new("local qux = true");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("qux")],
                explist: vec![Exp::True],
            }]))
        );
    }
}
