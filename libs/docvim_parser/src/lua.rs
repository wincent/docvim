use std::error::Error;
use std::fmt;

// Lexer token types are imported aliased (all with a "Token" suffix) to avoid collisions with
// parser node types of the same name.
use docvim_lexer::lua::KeywordKind::And as AndToken;
use docvim_lexer::lua::KeywordKind::False as FalseToken;
use docvim_lexer::lua::KeywordKind::Local as LocalToken;
use docvim_lexer::lua::KeywordKind::Nil as NilToken;
use docvim_lexer::lua::KeywordKind::Not as NotToken;
use docvim_lexer::lua::KeywordKind::Or as OrToken;
use docvim_lexer::lua::KeywordKind::True as TrueToken;
use docvim_lexer::lua::LiteralKind::Number as NumberToken;
use docvim_lexer::lua::LiteralKind::Str as StrToken;
use docvim_lexer::lua::NameKind::Identifier as IdentifierToken;
use docvim_lexer::lua::NameKind::Keyword as KeywordToken;
use docvim_lexer::lua::OpKind::Assign as AssignToken;
use docvim_lexer::lua::OpKind::Caret as CaretToken;
use docvim_lexer::lua::OpKind::Concat as ConcatToken;
use docvim_lexer::lua::OpKind::Eq as EqToken;
use docvim_lexer::lua::OpKind::Gt as GtToken;
use docvim_lexer::lua::OpKind::Gte as GteToken;
use docvim_lexer::lua::OpKind::Hash as HashToken;
use docvim_lexer::lua::OpKind::Lt as LtToken;
use docvim_lexer::lua::OpKind::Lte as LteToken;
use docvim_lexer::lua::OpKind::Minus as MinusToken;
use docvim_lexer::lua::OpKind::Ne as NeToken;
use docvim_lexer::lua::OpKind::Percent as PercentToken;
use docvim_lexer::lua::OpKind::Plus as PlusToken;
use docvim_lexer::lua::OpKind::Slash as SlashToken;
use docvim_lexer::lua::OpKind::Star as StarToken;
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
pub enum BinOp {
    And,
    Caret,
    Concat,
    Eq,
    Gt,
    Gte,
    Lt,
    Lte,
    Minus,
    Ne,
    Or,
    Percent,
    Plus,
    Slash,
    Star,
}

#[derive(Debug, PartialEq)]
pub struct Block<'a>(Vec<Statement<'a>>);

#[derive(Debug, PartialEq)]
pub enum Exp<'a> {
    Binary { lexp: Box<Exp<'a>>, op: BinOp, rexp: Box<Exp<'a>> },
    CookedStr(Box<String>),
    False,
    Nil,
    Number(&'a str),
    RawStr(&'a str),
    True,
    Unary { exp: Box<Exp<'a>>, op: UnOp },
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Length,
    Minus,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct Name<'a>(&'a str);

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    LocalDeclaration { namelist: Vec<Name<'a>>, explist: Vec<Exp<'a>> },
}

pub enum Op {
    BinOp(BinOp),
    UnOp(UnOp),
}

/// Returns the left and right "binding" power for a given operator, which enables us to parse
/// binary and unary expressions with left or right associativity via Pratt's "Top Down Operator
/// Precendence" algorithm, as described in doc/lua.md.
fn operator_binding(op: Op) -> (Option<u8>, Option<u8>) {
    match op {
        Op::BinOp(op) => match op {
            BinOp::Or => (Some(1), Some(2)),
            BinOp::And => (Some(3), Some(4)),
            BinOp::Eq => (Some(5), Some(6)),
            BinOp::Gt => (Some(5), Some(6)),
            BinOp::Gte => (Some(5), Some(6)),
            BinOp::Lt => (Some(5), Some(6)),
            BinOp::Lte => (Some(5), Some(6)),
            BinOp::Ne => (Some(5), Some(6)),
            BinOp::Concat => (Some(8), Some(7)), // Right-associative.
            BinOp::Plus => (Some(9), Some(10)),
            BinOp::Minus => (Some(11), Some(12)),
            BinOp::Percent => (Some(13), Some(14)),
            BinOp::Slash => (Some(13), Some(14)),
            BinOp::Star => (Some(13), Some(14)),
            BinOp::Caret => (Some(18), Some(17)), // Right-associative.
        },
        Op::UnOp(op) => match op {
            UnOp::Length | UnOp::Minus | UnOp::Not => (None, Some(15)),
        },
    }
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

impl Error for ParserError {}

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
                Ok(Token { kind: NameToken(KeywordToken(LocalToken)), .. }) => {
                    let node = self.parse_local(&mut tokens)?;
                    self.ast.0 .0.push(node);
                }
                Ok(_) => {
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
                    Ok(Token { kind: LiteralToken(NumberToken), .. })
                    | Ok(Token { kind: LiteralToken(StrToken(_)), .. })
                    | Ok(Token { kind: NameToken(KeywordToken(FalseToken)), .. })
                    | Ok(Token { kind: NameToken(KeywordToken(NilToken)), .. })
                    | Ok(Token { kind: NameToken(KeywordToken(NotToken)), .. })
                    | Ok(Token { kind: NameToken(KeywordToken(TrueToken)), .. })
                    | Ok(Token { kind: OpToken(HashToken), .. })
                    | Ok(Token { kind: OpToken(MinusToken), .. }) => {
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

    fn cook_str(&self, token: Token) -> Result<Exp<'a>, Box<dyn Error>> {
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

    fn parse_exp(
        &self,
        tokens: &mut std::iter::Peekable<Tokens>,
    ) -> Result<Exp<'a>, Box<dyn Error>> {
        let token = tokens.next().unwrap()?;
        let exp = match token {
            Token { kind: NameToken(KeywordToken(FalseToken)), .. } => Exp::False,
            Token { kind: NameToken(KeywordToken(NilToken)), .. } => Exp::Nil,
            Token { kind: LiteralToken(NumberToken), .. } => {
                Exp::Number(&self.lexer.input[token.byte_start..token.byte_end])
            }
            Token { kind: LiteralToken(StrToken(DoubleQuotedToken)), .. }
            | Token { kind: LiteralToken(StrToken(SingleQuotedToken)), .. } => {
                self.cook_str(token)?
            }
            Token { kind: LiteralToken(StrToken(LongToken { level })), .. } => {
                let start = token.byte_start + 2 + level;
                let end = token.byte_end - 2 - level;
                Exp::RawStr(&self.lexer.input[start..end])
            }
            Token { kind: NameToken(KeywordToken(TrueToken)), .. } => (Exp::True),
            Token { kind: NameToken(KeywordToken(NotToken)), .. } => {
                let exp = Box::new(self.parse_exp(tokens)?);
                Exp::Unary { exp, op: UnOp::Not }
            }
            Token { kind: OpToken(HashToken), .. } => {
                let exp = Box::new(self.parse_exp(tokens)?);
                Exp::Unary { exp, op: UnOp::Length }
            }
            Token { kind: OpToken(MinusToken), .. } => {
                let exp = Box::new(self.parse_exp(tokens)?);
                Exp::Unary { exp, op: UnOp::Minus }
            }

            // TODO: handle other expression types instead of erroring
            _ => {
                return Err(Box::new(ParserError {
                    kind: ParserErrorKind::UnexpectedToken,
                    position: token.char_start,
                }));
            }
        };

        // Peek ahead to see if there's a binary operator.
        if let Some(&Ok(token)) = tokens.peek() {
            match token {
                // TODO: deal with associativity
                Token { kind: NameToken(KeywordToken(AndToken)), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::And,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: NameToken(KeywordToken(OrToken)), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Or,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(CaretToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Caret,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(ConcatToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Concat,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(EqToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Eq,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(GtToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Gt,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(GteToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Gte,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(LtToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Lt,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(LteToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Lte,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(MinusToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Minus,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(NeToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Ne,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(PercentToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Percent,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(PlusToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Plus,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(SlashToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Slash,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                Token { kind: OpToken(StarToken), .. } => {
                    tokens.next();
                    return Ok(Exp::Binary {
                        lexp: Box::new(exp),
                        op: BinOp::Star,
                        rexp: Box::new(self.parse_exp(tokens)?),
                    });
                }
                _ => (),
            }
        }

        Ok(exp)
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
                explist: vec![Exp::CookedStr(Box::new(String::from("wat")))],
            }]))
        );

        let mut parser = Parser::new("local y = \"don't say \\\"hello\\\"!\"");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("y")],
                explist: vec![Exp::CookedStr(Box::new(String::from("don't say \"hello\"!")))],
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

        let mut parser = Parser::new("local neg = not true");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("neg")],
                explist: vec![Exp::Unary { exp: Box::new(Exp::True), op: UnOp::Not }],
            }]))
        );

        let mut parser = Parser::new("local len = #'sample'");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("len")],
                explist: vec![Exp::Unary {
                    exp: Box::new(Exp::CookedStr(Box::new(String::from("sample")))),
                    op: UnOp::Length,
                }],
            }]))
        );

        let mut parser = Parser::new("local small = -1000");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("small")],
                explist: vec![Exp::Unary { exp: Box::new(Exp::Number("1000")), op: UnOp::Minus }],
            }]))
        );

        let mut parser = Parser::new("local sum = 7 + 8");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("sum")],
                explist: vec![Exp::Binary {
                    lexp: Box::new(Exp::Number("7")),
                    op: BinOp::Plus,
                    rexp: Box::new(Exp::Number("8"))
                }],
            }]))
        );

        // TODO: write a test for input like this, motivating the implementation of associativity:
        //      let mut parser = Parser::new("local complex = 7 + #'foo' * -100");
        // eg. all binops are left associative except fro "^" and "..", so that is equivalent to:
        //      ((7 + (#'foo')) * (-100))
        // at the moment that parses as right associative; ie:
        //      (7 + (#('foo' * (-100))))
        //      Chunk(
        //          Block(
        //              [
        //                  LocalDeclaration {
        //                      namelist: [Name("complex")],
        //                      explist: [
        //                          Binary {
        //                              lexp: Number("7"),
        //                              op: Plus,
        //                              rexp: Unary {
        //                                  exp: Binary {
        //                                      lexp: CookedStr("foo"),
        //                                      op: Star,
        //                                      rexp: Unary {
        //                                          exp: Number("100"),
        //                                          op: Minus
        //                                      }
        //                                  },
        //                                  op: Length
        //                              }
        //                          }
        //                      ]
        //                  }
        //              ]
        //          )
        //      )`,
    }
}
