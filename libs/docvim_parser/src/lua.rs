use std::error::Error;

use crate::error::*;

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
use docvim_lexer::lua::PunctuatorKind::Colon as ColonToken;
use docvim_lexer::lua::PunctuatorKind::Comma as CommaToken;
use docvim_lexer::lua::PunctuatorKind::Dot as DotToken;
use docvim_lexer::lua::PunctuatorKind::Lbracket as LbracketToken;
use docvim_lexer::lua::PunctuatorKind::Lcurly as LcurlyToken;
use docvim_lexer::lua::PunctuatorKind::Lparen as LparenToken;
use docvim_lexer::lua::PunctuatorKind::Rbracket as RbracketToken;
use docvim_lexer::lua::PunctuatorKind::Rcurly as RcurlyToken;
use docvim_lexer::lua::PunctuatorKind::Rparen as RparenToken;
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

// TODO: rename these to reflect operation instead of token; eg "Add" insted of "Plus", "Multiply"
// instead of "Star", "Divide" instead of "Slash" etc.
#[derive(Clone, Copy, Debug, PartialEq)]
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
    Binary {
        lexp: Box<Exp<'a>>,
        op: BinOp,
        rexp: Box<Exp<'a>>,
    },
    CookedStr(Box<String>),
    False,
    FunctionCall {
        name: &'a str, // TODO: probably change this (see MethodCall)
                       // args: TODO  figure out how best to represent this
    },
    Index {
        /// The "prefix expression" (ie. LHS in `foo[bar]; that is, `foo`), which is the table to
        /// be indexed.
        // TODO: decide whether we should type this more narrowly (bc prefixexp is a subset of Exp)...
        pexp: Box<Exp<'a>>,
        /// The "key expression" (ie. RHS in `foo[bar]`; that is, `bar`).
        kexp: Box<Exp<'a>>,
    },
    MethodCall {
        // TODO: decide whether we should type this more narrowly (bc prefixexp is a subset of Exp)...
        pexp: Box<Exp<'a>>,
        name: &'a str,
        // args: TODO figure out how best to represent this
    },
    NamedVar(&'a str),
    Nil,
    Number(&'a str),
    RawStr(&'a str),
    Table(Vec<Field<'a>>),
    True,
    Unary {
        exp: Box<Exp<'a>>,
        op: UnOp,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnOp {
    Length,
    Minus,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct Field<'a> {
    index: Option<usize>,
    lexp: Box<Exp<'a>>,
    rexp: Box<Exp<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Table<'a>(Vec<Field<'a>>);

#[derive(Debug, PartialEq)]
pub struct Name<'a>(&'a str);

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    LocalDeclaration { namelist: Vec<Name<'a>>, explist: Vec<Exp<'a>> },
}

/// Returns the left and right "binding" power for a given operator, which enables us to parse
/// binary and unary expressions with left or right associativity via Pratt's "Top Down Operator
/// Precendence" algorithm, as described in doc/lua.md.
///
/// See also unop_binding.
fn binop_binding(op: BinOp) -> (u8, u8) {
    match op {
        BinOp::Or => (1, 2),
        BinOp::And => (3, 4),
        BinOp::Eq => (5, 6),
        BinOp::Gt => (5, 6),
        BinOp::Gte => (5, 6),
        BinOp::Lt => (5, 6),
        BinOp::Lte => (5, 6),
        BinOp::Ne => (5, 6),
        BinOp::Concat => (8, 7), // Right-associative.
        BinOp::Plus => (9, 10),
        BinOp::Minus => (11, 12),
        BinOp::Percent => (13, 14),
        BinOp::Slash => (13, 14),
        BinOp::Star => (13, 14),
        BinOp::Caret => (18, 17), // Right-associative.
    }
}

fn unop_binding(op: UnOp) -> u8 {
    match op {
        UnOp::Length => 15,
        UnOp::Minus => 15,
        UnOp::Not => 15,
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
                    // TODO instead of spelling all this out here, probably just want to try
                    // calling parse_exp _after_ checking CommmaToken, SemiToken branches...
                    Ok(Token { kind: LiteralToken(NumberToken), .. })
                    | Ok(Token { kind: LiteralToken(StrToken(_)), .. })
                    | Ok(Token { kind: NameToken(KeywordToken(FalseToken)), .. })
                    | Ok(Token { kind: NameToken(KeywordToken(NilToken)), .. })
                    | Ok(Token { kind: NameToken(KeywordToken(NotToken)), .. })
                    | Ok(Token { kind: NameToken(KeywordToken(TrueToken)), .. })
                    | Ok(Token { kind: NameToken(IdentifierToken), .. })
                    | Ok(Token { kind: OpToken(HashToken), .. })
                    | Ok(Token { kind: OpToken(MinusToken), .. })
                    | Ok(Token { kind: PunctuatorToken(LcurlyToken), .. }) => {
                        if expect_name {
                            explist.push(self.parse_exp(tokens, 0)?);
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
                            position: char_start + idx,
                        }));
                    }
                }
                other => other,
            };
            unescaped.push(unescaped_char);
        }

        Ok(Exp::CookedStr(Box::new(unescaped)))
    }

    fn parse_unop_exp(
        &self,
        tokens: &mut std::iter::Peekable<Tokens>,
        op: UnOp,
    ) -> Result<Exp<'a>, Box<dyn Error>> {
        let bp = unop_binding(op);
        let rhs = self.parse_exp(tokens, bp)?;
        Ok(Exp::Unary { exp: Box::new(rhs), op })
    }

    /// See doc/lua.md for an explanation of `minimum_bp`.
    fn parse_exp(
        &self,
        tokens: &mut std::iter::Peekable<Tokens>,
        minimum_bp: u8,
    ) -> Result<Exp<'a>, Box<dyn Error>> {
        let mut lhs = match tokens.next() {
            //
            // Punctuators (parens).
            //
            Some(Ok(token @ Token { kind: PunctuatorToken(LparenToken), .. })) => {
                let lhs = self.parse_exp(tokens, 0)?;
                let char_start = token.char_start;
                let token = tokens.next();
                match token {
                    Some(Ok(Token { kind: PunctuatorToken(RparenToken), .. })) => lhs,
                    Some(Ok(token)) => {
                        return Err(Box::new(ParserError {
                            kind: ParserErrorKind::UnexpectedToken,
                            position: token.char_start,
                        }))
                    }
                    Some(Err(err)) => return Err(Box::new(err)),
                    None => {
                        return Err(Box::new(ParserError {
                            kind: ParserErrorKind::UnexpectedEndOfInput,
                            // BUG: this is location of the "(", and ignores recursive call to lhs.
                            position: char_start,
                        }));
                    }
                }
            }

            //
            // Primaries (literals etc).
            //
            Some(Ok(Token { kind: NameToken(KeywordToken(FalseToken)), .. })) => Exp::False,
            Some(Ok(Token { kind: NameToken(KeywordToken(NilToken)), .. })) => Exp::Nil,
            Some(Ok(token @ Token { kind: LiteralToken(NumberToken), .. })) => {
                Exp::Number(&self.lexer.input[token.byte_start..token.byte_end])
            }
            Some(Ok(token @ Token { kind: LiteralToken(StrToken(DoubleQuotedToken)), .. }))
            | Some(Ok(token @ Token { kind: LiteralToken(StrToken(SingleQuotedToken)), .. })) => {
                self.cook_str(token)?
            }
            Some(Ok(token @ Token { kind: LiteralToken(StrToken(LongToken { .. })), .. })) => {
                // Unforunate workaround needed until: https://github.com/rust-lang/rust/issues/65490
                //
                // Can't write: Some(Ok(token @ Token { kind: LiteralToken(StrToken(LongToken { level })), .. }))
                //                this: ^^^^^                             at same time as this: ^^^^^
                //
                // Rust says, "pattern bindings after an `@` are unstable".
                let level = if let Token {
                    kind: LiteralToken(StrToken(LongToken { level })), ..
                } = token
                {
                    level
                } else {
                    panic!();
                };

                // As a convenience, Lua omits any newline at position 0 in a long format string.
                let first = self.lexer.input.as_bytes()[token.byte_start + 2 + level];
                let start = if first == ('\n' as u8) {
                    token.byte_start + 2 + level + 1
                } else {
                    token.byte_start + 2 + level
                };
                let end = token.byte_end - 2 - level;
                Exp::RawStr(&self.lexer.input[start..end])
            }
            Some(Ok(Token { kind: NameToken(KeywordToken(TrueToken)), .. })) => (Exp::True),

            // TODO: handle remaining "primaries":
            // - function
            // - tableconstructor
            // - ...
            // - var
            // - functioncall
            Some(Ok(Token { kind: NameToken(IdentifierToken), byte_start, byte_end, .. })) => {
                let name = &self.lexer.input[byte_start..byte_end];

                // note: these are all left associative so:
                //   x[1][2][3] is same as: (((x [1]) [2]) [3])
                //   x.a.b.c is same as (((x['a']) ['b']) ['c'])
                let token = tokens.peek();
                match token {
                    Some(&Ok(Token { kind: PunctuatorToken(ColonToken), .. })) => {
                        // `prefixexp : Name args`
                        tokens.next();
                        // NOPE, this isn't the pexp, that's the one on the other side
                        // let method_name = self.parse_name(tokens)?;
                        let method_name = match tokens.next() {
                            Some(Ok(Token {
                                kind: NameToken(IdentifierToken),
                                byte_start,
                                byte_end,
                                ..
                            })) => &self.lexer.input[byte_start..byte_end],
                            Some(Ok(Token { char_start, .. })) => {
                                return Err(Box::new(ParserError {
                                    kind: ParserErrorKind::UnexpectedToken,
                                    position: char_start,
                                }));
                            }
                            Some(Err(err)) => {
                                return Err(Box::new(err));
                            }
                            None => {
                                return Err(Box::new(ParserError {
                                    kind: ParserErrorKind::UnexpectedEndOfInput,
                                    position: self.lexer.input.chars().count(),
                                }));
                            }
                        };
                        // TODO: expect args - LparenToken, String, or tableconstructor
                        // TODO: also note that via left associativity, prefixexp could be complex eg. thing.foo("bingo"):blah()
                        Exp::MethodCall {
                            pexp: Box::new(Exp::NamedVar(name)),
                            name: method_name,
                            // args: ...
                        }
                    }
                    Some(&Ok(Token { kind: PunctuatorToken(DotToken), .. })) => {
                        // ie. `foo.bar`, which is syntactic sugar for `foo['bar']`
                        // TODO: handle complex pexps, not just NamedVar
                        tokens.next();
                        let kexp = self.parse_name(tokens)?; // TODO: change this it string!
                        Exp::Index { pexp: Box::new(Exp::NamedVar(name)), kexp: Box::new(kexp) }
                    }
                    Some(&Ok(Token { kind: PunctuatorToken(LbracketToken), .. })) => {
                        // ie. `foo[bar]`
                        // TODO: handle complex pexps, not just NamedVar
                        tokens.next();
                        let kexp = self.parse_exp(tokens, 0)?;
                        Exp::Index { pexp: Box::new(Exp::NamedVar(name)), kexp: Box::new(kexp) }
                    }
                    Some(&Ok(Token { kind: PunctuatorToken(LcurlyToken), .. })) => {
                        tokens.next();
                        self.parse_table_constructor(tokens)?
                    }
                    Some(&Ok(Token { kind: PunctuatorToken(LparenToken), .. })) => {
                        tokens.next();
                        // TODO: scan (, args, )
                        Exp::FunctionCall {
                            name,
                            // args
                        }
                    }
                    // if string, could be a function call too
                    _ => Exp::NamedVar(name),
                }
            }
            Some(Ok(Token { kind: PunctuatorToken(LcurlyToken), .. })) => {
                self.parse_table_constructor(tokens)?
            }

            //
            // Unary operators.
            //
            Some(Ok(Token { kind: NameToken(KeywordToken(NotToken)), .. })) => {
                self.parse_unop_exp(tokens, UnOp::Not)?
            }
            Some(Ok(Token { kind: OpToken(HashToken), .. })) => {
                self.parse_unop_exp(tokens, UnOp::Length)?
            }
            Some(Ok(Token { kind: OpToken(MinusToken), .. })) => {
                self.parse_unop_exp(tokens, UnOp::Minus)?
            }

            Some(Ok(token)) => {
                return Err(Box::new(ParserError {
                    kind: ParserErrorKind::UnexpectedToken,
                    position: token.char_start,
                }))
            }

            Some(Err(err)) => return Err(Box::new(err)),

            None => {
                return Err(Box::new(ParserError {
                    kind: ParserErrorKind::UnexpectedEndOfInput,
                    position: self.lexer.input.chars().count(),
                }))
            }
        };

        loop {
            let token = tokens.peek();
            let op = match token {
                Some(&Ok(Token { kind: NameToken(KeywordToken(AndToken)), .. })) => {
                    Some(BinOp::And)
                }
                Some(&Ok(Token { kind: NameToken(KeywordToken(OrToken)), .. })) => Some(BinOp::Or),
                Some(&Ok(Token { kind: OpToken(CaretToken), .. })) => Some(BinOp::Caret),
                Some(&Ok(Token { kind: OpToken(ConcatToken), .. })) => Some(BinOp::Concat),
                Some(&Ok(Token { kind: OpToken(EqToken), .. })) => Some(BinOp::Eq),
                Some(&Ok(Token { kind: OpToken(GtToken), .. })) => Some(BinOp::Gt),
                Some(&Ok(Token { kind: OpToken(GteToken), .. })) => Some(BinOp::Gte),
                Some(&Ok(Token { kind: OpToken(LtToken), .. })) => Some(BinOp::Lt),
                Some(&Ok(Token { kind: OpToken(LteToken), .. })) => Some(BinOp::Lte),
                Some(&Ok(Token { kind: OpToken(MinusToken), .. })) => Some(BinOp::Minus),
                Some(&Ok(Token { kind: OpToken(NeToken), .. })) => Some(BinOp::Ne),
                Some(&Ok(Token { kind: OpToken(PercentToken), .. })) => Some(BinOp::Percent),
                Some(&Ok(Token { kind: OpToken(PlusToken), .. })) => Some(BinOp::Plus),
                Some(&Ok(Token { kind: OpToken(SlashToken), .. })) => Some(BinOp::Slash),
                Some(&Ok(Token { kind: OpToken(StarToken), .. })) => Some(BinOp::Star),
                Some(&Ok(_)) => None, // No operator, we're done, will return lhs.
                Some(&Err(err)) => return Err(Box::new(err)),
                None => None, // End of input, will return lhs.
            };

            if let Some(op) = op {
                let (left_bp, right_bp) = binop_binding(op);
                if left_bp < minimum_bp {
                    break;
                } else {
                    tokens.next();
                    let rhs = self.parse_exp(tokens, right_bp)?;
                    lhs = Exp::Binary { lexp: Box::new(lhs), op, rexp: Box::new(rhs) };
                }
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn parse_name(
        &self,
        tokens: &mut std::iter::Peekable<Tokens>,
    ) -> Result<Exp<'a>, Box<dyn Error>> {
        // TODO ^^ narrower return type... we know this is going to return an Exp::NamedVar
        // might not be possible with current type system though; see: https://github.com/rust-lang/rfcs/pull/2593
        match tokens.next() {
            Some(Ok(Token { kind: NameToken(IdentifierToken), byte_start, byte_end, .. })) => {
                Ok(Exp::NamedVar(&self.lexer.input[byte_start..byte_end]))
            }
            Some(Ok(Token { char_start, .. })) => {
                // TODO: more specific error here
                Err(Box::new(ParserError {
                    kind: ParserErrorKind::UnexpectedToken,
                    position: char_start,
                }))
            }
            Some(Err(err)) => {
                return Err(Box::new(err));
            }
            None => Err(Box::new(ParserError {
                kind: ParserErrorKind::UnexpectedEndOfInput,
                position: self.lexer.input.chars().count(),
            })),
        }
    }

    // fieldlist ::= field {fieldsep field} [fieldsep]
    // fieldsep ::= `,´ | `;´
    fn parse_table_constructor(
        &self,
        tokens: &mut std::iter::Peekable<Tokens>,
    ) -> Result<Exp<'a>, Box<dyn Error>> {
        // Ugly, but we've already consumed LcurlyToken by the time we get here.
        let mut fieldsep_allowed = false;
        let mut fields = vec![];
        let mut index = 1;
        loop {
            let token = tokens.peek();
            match token {
                Some(&Ok(Token { kind: PunctuatorToken(RcurlyToken), .. })) => {
                    tokens.next();
                    return Ok(Exp::Table(fields));
                }
                Some(&Ok(Token { kind: PunctuatorToken(CommaToken), char_start, .. }))
                | Some(&Ok(Token { kind: PunctuatorToken(SemiToken), char_start, .. })) => {
                    if fieldsep_allowed {
                        tokens.next();
                        fieldsep_allowed = false;
                    } else {
                        return Err(Box::new(ParserError {
                            kind: ParserErrorKind::UnexpectedFieldSeparator,
                            position: char_start,
                        }));
                    }
                }
                Some(&Ok(Token { .. })) => {
                    let field = self.parse_table_field(tokens, index)?;
                    if matches!(field, Field { index: Some(_), .. }) {
                        index += 1;
                    }
                    fields.push(field);
                    fieldsep_allowed = true;
                }
                Some(&Err(err)) => {
                    return Err(Box::new(err));
                }
                None => {
                    return Err(Box::new(ParserError {
                        kind: ParserErrorKind::UnexpectedEndOfInput,
                        position: self.lexer.input.chars().count(),
                    }))
                }
            }
        }
    }

    fn parse_equals(&self, tokens: &mut std::iter::Peekable<Tokens>) -> Result<(), Box<dyn Error>> {
        match tokens.next() {
            Some(Ok(Token { kind: OpToken(EqToken), .. })) => Ok(()),
            Some(Ok(Token { char_start, .. })) => Err(Box::new(ParserError {
                kind: ParserErrorKind::ExpectedEq,
                position: char_start,
            })),
            Some(Err(err)) => {
                return Err(Box::new(err));
            }
            None => Err(Box::new(ParserError {
                kind: ParserErrorKind::UnexpectedEndOfInput,
                position: self.lexer.input.chars().count(),
            })),
        }
    }

    // TODO: functions like parse_equals and parse_rbracket are incredibly boilerplate-ish and repetitive; see if we can DRY that up a bit.
    fn parse_rbracket(
        &self,
        tokens: &mut std::iter::Peekable<Tokens>,
    ) -> Result<(), Box<dyn Error>> {
        match tokens.next() {
            Some(Ok(Token { kind: PunctuatorToken(RbracketToken), .. })) => Ok(()),
            Some(Ok(Token { char_start, .. })) => Err(Box::new(ParserError {
                kind: ParserErrorKind::ExpectedRbracket,
                position: char_start,
            })),
            Some(Err(err)) => {
                return Err(Box::new(err));
            }
            None => Err(Box::new(ParserError {
                kind: ParserErrorKind::UnexpectedEndOfInput,
                position: self.lexer.input.chars().count(),
            })),
        }
    }

    fn parse_rcurly(&self, tokens: &mut std::iter::Peekable<Tokens>) -> Result<(), Box<dyn Error>> {
        match tokens.next() {
            Some(Ok(Token { kind: PunctuatorToken(RcurlyToken), .. })) => Ok(()),
            Some(Ok(Token { char_start, .. })) => Err(Box::new(ParserError {
                kind: ParserErrorKind::ExpectedRcurly,
                position: char_start,
            })),
            Some(Err(err)) => {
                return Err(Box::new(err));
            }
            None => Err(Box::new(ParserError {
                kind: ParserErrorKind::UnexpectedEndOfInput,
                position: self.lexer.input.chars().count(),
            })),
        }
    }

    // field ::= `[´ exp `]´ `=´ exp | Name `=´ exp | exp
    fn parse_table_field(
        &self,
        tokens: &mut std::iter::Peekable<Tokens>,
        index: usize,
    ) -> Result<Field<'a>, Box<dyn Error>> {
        let token = tokens.peek();
        match token {
            Some(&Ok(Token { kind: NameToken(IdentifierToken), byte_start, byte_end, .. })) => {
                let name = Exp::RawStr(&self.lexer.input[byte_start..byte_end]);
                tokens.next();
                if matches!(tokens.peek(), Some(&Ok(Token { kind: OpToken(EqToken), .. }))) {
                    // `name = exp`; equivalent to `["name"] = exp`.
                    self.parse_equals(tokens)?;
                    let exp = self.parse_exp(tokens, 0)?;
                    Ok(Field { index: None, lexp: Box::new(name), rexp: Box::new(exp) })
                } else {
                    // `exp`; syntactic sugar for `[index] = exp`
                    Ok(Field { index: Some(index), lexp: Box::new(Exp::Nil), rexp: Box::new(name) })
                }
            }
            Some(&Ok(Token { kind: PunctuatorToken(LbracketToken), .. })) => {
                // `[exp] = exp`
                let lexp = self.parse_exp(tokens, 0)?;
                self.parse_rbracket(tokens)?;
                self.parse_equals(tokens)?;
                let rexp = self.parse_exp(tokens, 0)?; // TODO: confirm binding power of 0 is appropriate here
                Ok(Field { index: None, lexp: Box::new(lexp), rexp: Box::new(rexp) })
            }
            Some(&Ok(Token { .. })) => {
                // `exp`; syntactic sugar for `[index] = exp`
                let exp = self.parse_exp(tokens, 0)?; // TODO: confirm binding power of 0 is appropriate here
                Ok(Field {
                    index: Some(index),
                    lexp: Box::new(Exp::Nil), // A hack because we can't create an Exp::Number here without upsetting the borrow checker.
                    rexp: Box::new(exp),
                })
                // TODO: implement this:
                // "If the last field in the list has the form exp and the expression is a function
                // call or a vararg expression, then all values returned by this expression enter the
                // list consecutively"
            }
            Some(&Err(err)) => {
                return Err(Box::new(err));
            }
            None => Err(Box::new(ParserError {
                kind: ParserErrorKind::UnexpectedEndOfInput,
                position: self.lexer.input.chars().count(),
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

        // Based on the example from doc/lua.md plus some extra parens and a "not" thrown in for
        // good measure:
        //
        //      not (1 * 2 + 3 - 4 / 5 ^ -6 > -7 ^ 8 + (9 - 10) * 11)
        //
        // equivalent to:
        //
        //      not (((1 * 2) + (3 - (4 / (5 ^ (-6))))) > (-(7 ^ 8) + ((9 - 10) * 11)))
        //
        let mut parser =
            Parser::new("local demo =  not (1 * 2 + 3 - 4 / 5 ^ -6 > -7 ^ 8 + (9 - 10) * 11)");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("demo")],
                explist: vec![Exp::Unary {
                    exp: Box::new(Exp::Binary {
                        lexp: Box::new(Exp::Binary {
                            lexp: Box::new(Exp::Binary {
                                lexp: Box::new(Exp::Number("1")),
                                op: BinOp::Star,
                                rexp: Box::new(Exp::Number("2")),
                            }),
                            op: BinOp::Plus,
                            rexp: Box::new(Exp::Binary {
                                lexp: Box::new(Exp::Number("3")),
                                op: BinOp::Minus,
                                rexp: Box::new(Exp::Binary {
                                    lexp: Box::new(Exp::Number("4")),
                                    op: BinOp::Slash,
                                    rexp: Box::new(Exp::Binary {
                                        lexp: Box::new(Exp::Number("5")),
                                        op: BinOp::Caret,
                                        rexp: Box::new(Exp::Unary {
                                            exp: Box::new(Exp::Number("6")),
                                            op: UnOp::Minus,
                                        })
                                    })
                                })
                            })
                        }),
                        op: BinOp::Gt,
                        rexp: Box::new(Exp::Binary {
                            lexp: Box::new(Exp::Unary {
                                exp: Box::new(Exp::Binary {
                                    lexp: Box::new(Exp::Number("7")),
                                    op: BinOp::Caret,
                                    rexp: Box::new(Exp::Number("8")),
                                }),
                                op: UnOp::Minus,
                            }),
                            op: BinOp::Plus,
                            rexp: Box::new(Exp::Binary {
                                lexp: Box::new(Exp::Binary {
                                    lexp: Box::new(Exp::Number("9")),
                                    op: BinOp::Minus,
                                    rexp: Box::new(Exp::Number("10")),
                                }),
                                op: BinOp::Star,
                                rexp: Box::new(Exp::Number("11"))
                            })
                        })
                    }),
                    op: UnOp::Not
                }]
            }]))
        );
    }

    #[test]
    fn parses_unary_not_with_name() {
        let mut parser = Parser::new("local foo = not bar");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("foo")],
                explist: vec![Exp::Unary { exp: Box::new(Exp::NamedVar("bar")), op: UnOp::Not }],
            }]))
        );
    }

    #[test]
    fn parses_table_constructor() {
        let mut parser = Parser::new("local stuff = { one }");
        let ast = parser.parse();
        assert_eq!(
            *ast.unwrap(),
            Chunk(Block(vec![Statement::LocalDeclaration {
                namelist: vec![Name("stuff")],
                explist: vec![Exp::Table(vec![Field {
                    index: Some(1),
                    lexp: Box::new(Exp::Nil),
                    rexp: Box::new(Exp::RawStr("one"))
                }])],
            }]))
        );
    }
}
