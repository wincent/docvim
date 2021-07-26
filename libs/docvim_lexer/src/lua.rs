use std::error;
use std::fmt;

use crate::peekable::*;

use self::CommentKind::*;
use self::KeywordKind::*;
use self::LiteralKind::*;
use self::NameKind::*;
use self::OpKind::*;
use self::PunctuatorKind::*;
use self::StrKind::*;
use self::TokenKind::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub char_start: usize,
    pub char_end: usize,
    pub byte_start: usize,
    pub byte_end: usize,
}

impl Token {
    fn new(
        kind: TokenKind,
        char_start: usize,
        char_end: usize,
        byte_start: usize,
        byte_end: usize,
    ) -> Token {
        Token { kind, char_start, char_end, byte_start, byte_end }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CommentKind {
    BlockComment,
    LineComment,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum KeywordKind {
    And,
    Break,
    Do,
    Else,
    Elseif,
    End,
    False,
    For,
    Function,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NameKind {
    Identifier,
    Keyword(KeywordKind),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PunctuatorKind {
    Colon,
    Comma,
    Dot,
    Lcurly,
    Lparen,
    Lbracket,
    Rparen,
    Rcurly,
    Rbracket,
    Semi,
}

// Note that "and" and "or" are _operators_ in Lua, but we lex them as _keywords_ (see
// `KeywordKind`).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OpKind {
    Assign,  // = (assign)
    Caret,   // ^ (exponentiate)
    Concat,  // .. (concatenate)
    Eq,      // == (equal)
    Gt,      // > (greater than)
    Gte,     // >= (greater than or equal)
    Hash,    // # (length, unary)
    Lt,      // < (less than)
    Lte,     // <= (less than or equal)
    Minus,   // - (negate, unary / subtract, binary)
    Ne,      // == (not equal)
    Percent, // % (modulo)
    Plus,    // + (add)
    Slash,   // / (divide)
    Star,    // * (multiply)
    Vararg,  // ... (varargs, technically not an "operator", just syntax)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LiteralKind {
    Number,
    Str(StrKind),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StrKind {
    DoubleQuoted,
    SingleQuoted,
    Long { level: usize },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
    Op(OpKind),
    Comment(CommentKind),
    Literal(LiteralKind),
    Name(NameKind),
    Punctuator(PunctuatorKind),
    Unknown,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LexerErrorKind {
    InvalidEscapeSequence,
    InvalidOperator,
    InvalidNumberLiteral,
    UnterminatedBlockComment,
    UnterminatedEscapeSequence,
    UnterminatedStringLiteral,
}

impl LexerErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            LexerErrorKind::InvalidEscapeSequence => "invalid escape sequence",
            LexerErrorKind::InvalidNumberLiteral => "invalid number literal",
            LexerErrorKind::InvalidOperator => "invalid operator",
            LexerErrorKind::UnterminatedBlockComment => "unterminated block comment",
            LexerErrorKind::UnterminatedEscapeSequence => "unterminated escape sequence",
            LexerErrorKind::UnterminatedStringLiteral => "unterminated string literal",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub position: usize,
}

impl LexerError {
    pub fn new(kind: LexerErrorKind, char_idx: usize) -> LexerError {
        Self { kind, position: char_idx }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} ({})", self.kind.to_str(), self.position)
    }
}

impl error::Error for LexerError {}

pub struct Lexer<'a> {
    pub input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    /// Returns an iterator over the tokens produced by the lexer.
    pub fn tokens(&self) -> Tokens<'_> {
        Tokens { iter: Peekable::new(self.input) }
    }

    // TODO: Probably don't need this; as tests show, can just do `lexer.input[..]` directly.
    // #[cfg(test)]
    // fn slice(&self, byte_start: usize, byte_end: usize) -> &str {
    //     &self.input[byte_start..byte_end]
    // }

    // TODO: consider something like this...
    // pub fn str_for_token(&self, token: Token) -> &str {
    //     &self.input[token.start..token.end]
    // }

    /// Consumes the lexer's input and returns `Some(LexerError)` on encountering an error, or
    /// `None` if the input is valid.
    #[cfg(test)]
    fn validate(&self) -> Option<LexerError> {
        loop {
            match self.tokens().next() {
                Some(Err(e)) => {
                    return Some(e);
                }
                None => {
                    return None;
                }
                _ => (),
            }
        }
    }
}

pub struct Tokens<'a> {
    iter: Peekable<'a>,
}

impl<'a> Tokens<'a> {
    /// Consumes the specified `char`, returning `true` on success and `false` if nothing was
    /// consumed.
    fn consume_char(&mut self, ch: char) -> bool {
        match self.iter.peek() {
            Some(&seen) if seen == ch => {
                self.iter.next();
                true
            }
            _ => false,
        }
    }

    fn scan_comment(&mut self) -> Result<Token, LexerError> {
        let char_start = self.iter.char_idx - 2; // Subtract length of "--" prefix.
        let byte_start = self.iter.byte_idx - 2;
        if self.consume_char('[') && self.consume_char('[') {
            self.scan_block_comment(char_start, byte_start)
        } else {
            self.scan_line_comment(char_start, byte_start)
        }
    }

    /// Scans until seeing "--]]".
    fn scan_block_comment(
        &mut self,
        char_start: usize,
        byte_start: usize,
    ) -> Result<Token, LexerError> {
        loop {
            let ch = self.iter.next();
            match ch {
                Some('-') => {
                    // Can't just chain `consume_char` calls here (for "-", "-", "[", and "[")
                    // because the greedy match would fail for text like "---[[" which is also a
                    // valid marker to end a block comment.
                    let mut dash_count = 1;
                    while self.consume_char('-') {
                        dash_count += 1;
                    }
                    if dash_count >= 2 && self.consume_char(']') && self.consume_char(']') {
                        return Ok(Token::new(
                            Comment(BlockComment),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        ));
                    }
                }
                None => {
                    return Err(LexerError {
                        kind: LexerErrorKind::UnterminatedBlockComment,
                        position: self.iter.char_idx,
                    });
                }
                _ => (),
            }
        }
    }

    /// Scans until end of line, or end of input.
    fn scan_line_comment(
        &mut self,
        char_start: usize,
        byte_start: usize,
    ) -> Result<Token, LexerError> {
        loop {
            let ch = self.iter.next();
            match ch {
                Some('\n') | None => {
                    return Ok(Token::new(
                        Comment(LineComment),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    ));
                }
                _ => (),
            }
        }
    }

    // From Lua docs: "The definition of letter depends on the current locale: any character
    // considered alphabetic by the current locale can be used in an identifier", so the below
    // could use a bit of work...
    // See: https://stackoverflow.com/a/4843653/2103996
    fn scan_name(&mut self) -> Result<Token, LexerError> {
        let byte_start = self.iter.byte_idx;
        let char_start = self.iter.char_idx;
        let mut name = Vec::new();
        while let Some(&c) = self.iter.peek() {
            match c {
                'A'..='Z' | 'a'..='z' | '0'..='9' | '_' => {
                    name.push(self.iter.next().unwrap());
                }
                _ => {
                    break;
                }
            }
        }
        let name: String = name.into_iter().collect();
        match &name[..] {
            "and" => Ok(Token::new(
                Name(Keyword(And)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "break" => Ok(Token::new(
                Name(Keyword(Break)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "do" => Ok(Token::new(
                Name(Keyword(Do)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "else" => Ok(Token::new(
                Name(Keyword(Else)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "elseif" => Ok(Token::new(
                Name(Keyword(Elseif)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "end" => Ok(Token::new(
                Name(Keyword(End)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "false" => Ok(Token::new(
                Name(Keyword(False)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "for" => Ok(Token::new(
                Name(Keyword(For)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "function" => Ok(Token::new(
                Name(Keyword(Function)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "if" => Ok(Token::new(
                Name(Keyword(If)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "in" => Ok(Token::new(
                Name(Keyword(In)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "local" => Ok(Token::new(
                Name(Keyword(Local)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "nil" => Ok(Token::new(
                Name(Keyword(Nil)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "not" => Ok(Token::new(
                Name(Keyword(Not)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "or" => Ok(Token::new(
                Name(Keyword(Or)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "repeat" => Ok(Token::new(
                Name(Keyword(Repeat)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "return" => Ok(Token::new(
                Name(Keyword(Return)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "then" => Ok(Token::new(
                Name(Keyword(Then)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "true" => Ok(Token::new(
                Name(Keyword(True)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "until" => Ok(Token::new(
                Name(Keyword(Until)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            "while" => Ok(Token::new(
                Name(Keyword(While)),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
            _ => Ok(Token::new(
                Name(Identifier),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            )),
        }
    }

    fn scan_number(&mut self) -> Result<Token, LexerError> {
        let byte_start = self.iter.byte_idx;
        let char_start = self.iter.char_idx;
        let ch = self.iter.next().unwrap();
        if ch == '0' && self.consume_char('x') {
            let mut seen_separator = false;
            while let Some(&next) = self.iter.peek() {
                match next {
                    '0'..='9' | 'A'..='F' | 'a'..='f' => {
                        self.iter.next();
                    }
                    '.' => {
                        if seen_separator {
                            return Err(LexerError {
                                kind: LexerErrorKind::InvalidNumberLiteral,
                                position: self.iter.char_idx,
                            });
                        } else {
                            seen_separator = true;
                            self.iter.next();
                        }
                    }
                    | ' ' | '\n' | '\t' | '\r' // whitespace
                    | ':' | ',' | '{' | '(' | '[' |  '}' | ')' | ']' | ';' | '\\' // punctuators
                    | '=' | '^' | '>' | '<' | '-' | '%' | '+' | '/' | '*' // operators
                    => {
                        break;
                    }
                    _ => {
                        return Err(LexerError {
                            kind: LexerErrorKind::InvalidNumberLiteral,
                            position: self.iter.char_idx,
                        });
                    }
                }
            }
            return Ok(Token::new(
                Literal(Number),
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            ));
        } else {
            let mut seen_separator = false;
            while let Some(&next) = self.iter.peek() {
                match next {
                    '0'..='9' => {
                        self.iter.next();
                    }
                    '.' => {
                        if seen_separator {
                            return Err(LexerError {
                                kind: LexerErrorKind::InvalidNumberLiteral,
                                position: self.iter.char_idx,
                            });
                        } else {
                            seen_separator = true;
                            self.iter.next();
                        }
                    }
                    'E' | 'e' => {
                        self.iter.next();
                        self.consume_char('-');
                        let mut exp_digits_count = 0;
                        while let Some(&next) = self.iter.peek() {
                            match next {
                                '0'..='9' => {
                                    exp_digits_count += 1;
                                    self.iter.next();
                                }
                                | ' ' | '\n' | '\t' | '\r' // whitespace
                                | ':' | ',' | '{' | '(' | '[' |  '}' | ')' | ']' | ';' | '\\' // punctuators
                                | '=' | '^' | '>' | '<' | '-' | '%' | '+' | '/' | '*' // operators
                                => {
                                    break;
                                }
                                _ => {
                                    return Err(LexerError {
                                        kind: LexerErrorKind::InvalidNumberLiteral,
                                        position: self.iter.char_idx,
                                    });
                                }
                            }
                        }
                        if exp_digits_count > 0 {
                            return Ok(Token::new(
                                Literal(Number),
                                char_start,
                                self.iter.char_idx,
                                byte_start,
                                self.iter.byte_idx,
                            ));
                        } else {
                            return Err(LexerError {
                                kind: LexerErrorKind::InvalidNumberLiteral,
                                position: self.iter.char_idx,
                            });
                        }
                    }
                    _ => {
                        break;
                    }
                }
            }
        }
        Ok(Token::new(
            Literal(Number),
            char_start,
            self.iter.char_idx,
            byte_start,
            self.iter.byte_idx,
        ))
    }

    fn scan_string(&mut self) -> Result<Token, LexerError> {
        let byte_start = self.iter.byte_idx;
        let char_start = self.iter.char_idx;
        let quote = self.iter.next().unwrap();
        while let Some(c) = self.iter.next() {
            if c == quote {
                if quote == '"' {
                    return Ok(Token::new(
                        Literal(Str(DoubleQuoted)),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    ));
                } else {
                    return Ok(Token::new(
                        Literal(Str(SingleQuoted)),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    ));
                }
            }
            match c {
                '\\' => {
                    if let Some(escape) = self.iter.next() {
                        match escape {
                            | 'a' // bell
                            | 'b' // backspace
                            | 'f' // form feed
                            | 'n' // new line
                            | 'r' // carriage return
                            | 't' // horizontal tab
                            | 'v' // vertical tab
                            | '\\' // backslash
                            | '"' // double quote
                            | '\'' // single quote
                            | '\n' // literal newline
                            => (), // legit escape sequence
                            '0'..='9' => {
                                let mut digit_count = 1;
                                while digit_count < 3 {
                                    // TODO: make is_digit helper fn
                                    if let Some(&digit) = self.iter.peek() {
                                        match digit {
                                            '0'..='9' => {
                                                self.iter.next();
                                                digit_count += 1;
                                            },
                                            _ => {
                                                break;
                                            }
                                        }
                                    } else {
                                        break;
                                    }
                                }
                            },
                            _ => {
                                return Err(LexerError {
                                    kind: LexerErrorKind::InvalidEscapeSequence,
                                    position: self.iter.char_idx,
                                });
                            }
                        }
                    } else {
                        return Err(LexerError {
                            kind: LexerErrorKind::UnterminatedEscapeSequence,
                            position: self.iter.char_idx,
                        });
                    }
                }
                _ => (), // Non-escaped string contents.
            }
        }
        Err(LexerError {
            kind: LexerErrorKind::UnterminatedStringLiteral,
            position: self.iter.char_idx,
        })
    }

    /// Long format strings do not interpret escape sequences.
    ///
    /// Examples:
    ///
    /// - [[a level 0 string]]
    /// - [=[a level 1 string]=]
    /// - [==[a level 2 string]==]
    ///
    fn scan_long_string(&mut self, level: usize) -> Result<Token, LexerError> {
        let byte_start = self.iter.byte_idx - level - 2;
        let char_start = self.iter.char_idx - level - 2;
        while let Some(c) = self.iter.next() {
            if c == ']' {
                let mut eq_count = 0;
                while eq_count < level {
                    if self.consume_char('=') {
                        eq_count += 1;
                    } else {
                        break;
                    }
                }
                if eq_count == level && self.consume_char(']') {
                    return Ok(Token::new(
                        Literal(Str(Long { level })),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    ));
                }
            }
        }
        Err(LexerError {
            kind: LexerErrorKind::UnterminatedStringLiteral,
            position: self.iter.char_idx,
        })
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.iter.peek() {
            match c {
                ' ' | '\n' | '\r' | '\t' => {
                    self.iter.next();
                }
                _ => {
                    break;
                }
            }
        }
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Result<Token, LexerError>> {
        self.skip_whitespace();
        let char_start = self.iter.char_idx;
        let byte_start = self.iter.byte_idx;
        if let Some(&c) = self.iter.peek() {
            match c {
                '-' => {
                    self.iter.next();
                    if self.consume_char('-') {
                        Some(self.scan_comment())
                    } else {
                        Some(Ok(Token::new(
                            Op(Minus),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        )))
                    }
                }
                '+' => {
                    // TODO: make macro to reduce verbosity here (once overall shape has settled
                    // down).
                    self.iter.next();
                    Some(Ok(Token::new(
                        Op(Plus),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                '*' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Op(Star),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                '/' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Op(Slash),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                '%' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Op(Percent),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                '^' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Op(Caret),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                '#' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Op(Hash),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                '=' => {
                    let mut eq_count = 0;
                    while self.consume_char('=') {
                        eq_count += 1;
                    }
                    match eq_count {
                        1 => Some(Ok(Token::new(
                            Op(Assign),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        ))),
                        2 => Some(Ok(Token::new(
                            Op(Eq),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        ))),
                        _ => Some(Err(LexerError {
                            kind: LexerErrorKind::InvalidOperator,
                            position: char_start,
                        })),
                    }
                }
                '~' => {
                    // TODO might want to think about some general rules here instead of coding
                    // these one at a time... as in, having punctuators or operators one after the
                    // other, is almost certainly invalid except for rare cases(? eg. ;;;; or x=-1)
                    // eg. (1) + (-1) is legit, luajit accepts `(1+-1)`
                    // could also let parser deal with it
                    self.iter.next();
                    if self.consume_char('=') {
                        Some(Ok(Token::new(
                            Op(Ne),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        )))
                    } else {
                        Some(Err(LexerError {
                            kind: LexerErrorKind::InvalidOperator,
                            position: char_start,
                        }))
                    }
                }
                '<' => {
                    self.iter.next();
                    if self.consume_char('=') {
                        Some(Ok(Token::new(
                            Op(Lte),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        )))
                    } else {
                        Some(Ok(Token::new(
                            Op(Lt),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        )))
                    }
                }
                '>' => {
                    self.iter.next();
                    if self.consume_char('=') {
                        Some(Ok(Token::new(
                            Op(Gte),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        )))
                    } else {
                        Some(Ok(Token::new(
                            Op(Gt),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        )))
                    }
                }
                '(' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Punctuator(Lparen),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                ')' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Punctuator(Rparen),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                '{' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Punctuator(Lcurly),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                '}' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Punctuator(Rcurly),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                '[' => {
                    self.iter.next();
                    if self.consume_char('[') {
                        Some(self.scan_long_string(0))
                    } else {
                        let mut eq_count = 0;
                        while self.consume_char('=') {
                            eq_count += 1;
                        }
                        if eq_count > 0 {
                            Some(self.scan_long_string(eq_count))
                        } else {
                            Some(Ok(Token::new(
                                Punctuator(Lbracket),
                                char_start,
                                self.iter.char_idx,
                                byte_start,
                                self.iter.byte_idx,
                            )))
                        }
                    }
                }
                ']' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Punctuator(Rbracket),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                ';' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Punctuator(Semi),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                ':' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Punctuator(Colon),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                ',' => {
                    self.iter.next();
                    Some(Ok(Token::new(
                        Punctuator(Comma),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
                '.' => {
                    let mut dot_count = 0;
                    while self.consume_char('.') {
                        dot_count += 1;
                    }
                    match dot_count {
                        1 => Some(Ok(Token::new(
                            Punctuator(Dot),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        ))),
                        2 => Some(Ok(Token::new(
                            Op(Concat),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        ))),
                        3 => Some(Ok(Token::new(
                            Op(Vararg),
                            char_start,
                            self.iter.char_idx,
                            byte_start,
                            self.iter.byte_idx,
                        ))),
                        _ => Some(Err(LexerError {
                            kind: LexerErrorKind::InvalidOperator,
                            position: char_start,
                        })),
                    }
                }
                '\'' | '"' => Some(self.scan_string()),
                '0'..='9' => Some(self.scan_number()),
                'A'..='Z' | 'a'..='z' | '_' => Some(self.scan_name()),
                _ => {
                    // For docvim's purposes it is better to fail gracefully and emit some
                    // "Unknown" tokens for stuff we don't recognize, so that it can at least take
                    // its best shot at generating documentation.
                    self.iter.next();
                    Some(Ok(Token::new(
                        Unknown,
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    )))
                }
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use LexerErrorKind;

    macro_rules! assert_lexes {
        ($input:expr, $expected:expr) => {
            assert_eq!(
                Lexer::new(&$input).tokens().map(|x| x.unwrap()).collect::<Vec<Token>>(),
                $expected
            )
        };
    }

    #[test]
    fn lexes_unicode() {
        assert_lexes!(
            "-- cañón\n-- träumen",
            vec![
                Token {
                    byte_end: 11,
                    byte_start: 0,
                    char_end: 9,
                    char_start: 0,
                    kind: Comment(LineComment),
                },
                Token {
                    byte_end: 22,
                    byte_start: 11,
                    char_end: 19,
                    char_start: 9,
                    kind: Comment(LineComment),
                }
            ]
        );
    }

    #[test]
    fn lexes_line_comments() {
        assert_lexes!(
            "-- TODO: something",
            vec![Token {
                byte_end: 18,
                byte_start: 0,
                char_end: 18,
                char_start: 0,
                kind: Comment(LineComment),
            }]
        );
        assert_lexes!(
            "--[ Almost a block comment, but not quite",
            vec![Token {
                byte_end: 41,
                byte_start: 0,
                char_end: 41,
                char_start: 0,
                kind: Comment(LineComment),
            }]
        );
    }

    #[test]
    fn lexes_block_comments() {
        assert_lexes!(
            "--[[\nstuff\n--]]",
            vec![Token {
                byte_end: 15,
                byte_start: 0,
                char_end: 15,
                char_start: 0,
                kind: Comment(BlockComment),
            }]
        );
    }

    #[test]
    fn lexes_valid_numbers() {
        // Examples from Lua docs.
        assert_lexes!(
            "3",
            vec![Token {
                byte_end: 1,
                byte_start: 0,
                char_end: 1,
                char_start: 0,
                kind: Literal(Number),
            }]
        );
        assert_lexes!(
            "3.0",
            vec![Token {
                byte_end: 3,
                byte_start: 0,
                char_end: 3,
                char_start: 0,
                kind: Literal(Number),
            }]
        );
        assert_lexes!(
            "3.1416",
            vec![Token {
                byte_end: 6,
                byte_start: 0,
                char_end: 6,
                char_start: 0,
                kind: Literal(Number),
            }]
        );
        assert_lexes!(
            "314.16e-2",
            vec![Token {
                byte_end: 9,
                byte_start: 0,
                char_end: 9,
                char_start: 0,
                kind: Literal(Number),
            }]
        );
        assert_lexes!(
            "0.31416E1",
            vec![Token {
                byte_end: 9,
                byte_start: 0,
                char_end: 9,
                char_start: 0,
                kind: Literal(Number),
            }]
        );
        assert_lexes!(
            "0xff",
            vec![Token {
                byte_end: 4,
                byte_start: 0,
                char_end: 4,
                char_start: 0,
                kind: Literal(Number),
            }]
        );
        assert_lexes!(
            "0x56",
            vec![Token {
                byte_end: 4,
                byte_start: 0,
                char_end: 4,
                char_start: 0,
                kind: Literal(Number),
            }]
        );

        // These ones look fishy but actually aren't.
        assert_lexes!(
            "0xff.1", // ie. 255.0625
            vec![Token {
                byte_end: 6,
                byte_start: 0,
                char_end: 6,
                char_start: 0,
                kind: Literal(Number),
            }]
        );
        assert_lexes!(
            "0xff.ff", // ie. 255.99609375.
            vec![Token {
                byte_end: 7,
                byte_start: 0,
                char_end: 7,
                char_start: 0,
                kind: Literal(Number),
            }]
        );
        assert_lexes!(
            "0xffe10", // 1048080 because "e" doesn't mean exponent here.
            vec![Token {
                byte_end: 7,
                byte_start: 0,
                char_end: 7,
                char_start: 0,
                kind: Literal(Number),
            }]
        );
        assert_lexes!(
            "0xffe-10", // "e" not exponent; this is `(0xffe) - 10` ie. 4084.
            vec![
                Token {
                    byte_end: 5,
                    byte_start: 0,
                    char_end: 5,
                    char_start: 0,
                    kind: Literal(Number),
                },
                Token { byte_end: 6, byte_start: 5, char_end: 6, char_start: 5, kind: Op(Minus) },
                Token {
                    byte_end: 8,
                    byte_start: 6,
                    char_end: 8,
                    char_start: 6,
                    kind: Literal(Number),
                }
            ]
        );
        assert_lexes!(
            "0xff.ffe2", // ie. 255.99954223633.
            vec![Token {
                byte_end: 9,
                char_end: 9,
                kind: Literal(Number),
                char_start: 0,
                byte_start: 0,
            }]
        );
    }

    #[test]
    fn rejects_invalid_numbers() {
        assert_eq!(
            Lexer::new("3.0.1").validate(),
            Some(LexerError { kind: LexerErrorKind::InvalidNumberLiteral, position: 3 })
        );
        assert_eq!(
            Lexer::new("3e-2.1").validate(),
            Some(LexerError { kind: LexerErrorKind::InvalidNumberLiteral, position: 4 })
        );
        assert_eq!(
            Lexer::new("0xffx").validate(),
            Some(LexerError { kind: LexerErrorKind::InvalidNumberLiteral, position: 4 })
        );
        assert_eq!(
            Lexer::new("0xff.0xff").validate(),
            Some(LexerError { kind: LexerErrorKind::InvalidNumberLiteral, position: 6 })
        );
    }

    #[test]
    fn lexes_strings() {
        assert_lexes!(
            "'hello'",
            vec![Token {
                kind: Literal(Str(SingleQuoted)),
                char_start: 0,
                char_end: 7,
                byte_start: 0,
                byte_end: 7,
            }]
        );
    }

    #[test]
    fn can_extract_a_slice() {
        // This shows that we don't need tokens to embed a copy of their text, because we can
        // borrow an immutable slice whenever we want.
        let lexer = Lexer::new("local foo = 1");
        assert_eq!(&lexer.input[6..9], "foo");
    }

    // TODO: consider something like this...
    // #[test]
    // fn returns_text_for_a_token() {
    //     let mut lexer = Lexer::new("local foo = 1");
    //     lexer.next().unwrap().expect("must yield a token");
    //     let token = lexer.next().unwrap().expect("must yield a token");
    //     assert_eq!(lexer.str_for_token(token), "foo");
    // }
}
