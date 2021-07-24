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

#[derive(Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
    pub contents: Option<&'a str>,
}

impl<'a> Token<'a> {
    fn new(kind: TokenKind, start: usize, end: usize) -> Token<'a> {
        Token {
            kind,
            start,
            end,
            contents: None,
        }
    }
}

fn build_token(
    kind: TokenKind,
    input: &str,
    char_start: usize,
    char_end: usize,
    byte_start: usize,
    byte_end: usize,
) -> Token {
    Token {
        kind,
        start: char_start,
        end: char_end,
        contents: Some(&input[byte_start..byte_end]),
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

    EndOfInput, // Not a real error; just used to signify we got to the end.
}

impl LexerErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            LexerErrorKind::EndOfInput => "end of input",
            LexerErrorKind::InvalidEscapeSequence => "invalid escape sequence",
            LexerErrorKind::InvalidNumberLiteral => "invalid number literal",
            LexerErrorKind::InvalidOperator => "invalid operator",
            LexerErrorKind::UnterminatedBlockComment => "unterminated block comment",
            LexerErrorKind::UnterminatedEscapeSequence => "unterminated escape sequence",
            LexerErrorKind::UnterminatedStringLiteral => "unterminated string literal",
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub position: usize,
}

impl LexerError {
    pub fn new(kind: LexerErrorKind, char_idx: usize) -> LexerError {
        Self {
            kind,
            position: char_idx,
        }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} ({})", self.kind.to_str(), self.position)
    }
}

impl error::Error for LexerError {}

pub struct Lexer<'a> {
    input: &'a str,
    iter: Peekable<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,

            // TODO: investigate changing this to char_indices()
            // once/if I can confirm that we make cheap slice "copies" based on indices (will just
            // test with ascii to start with, where byte index === char index)
            iter: Peekable::new(input),
        }
    }

    /// Consumes the specified `char`, returning `true` on success and `false` if nothing was
    /// consumed.
    fn consume_char(&mut self, ch: char) -> bool {
        match self.iter.peek() {
            Some(seen) => {
                if seen == ch {
                    self.iter.next();
                    true
                } else {
                    false
                }
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
                        return Ok(build_token(
                            Comment(BlockComment),
                            self.input,
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
                    return Ok(build_token(
                        Comment(LineComment),
                        self.input,
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
    fn scan_name(&mut self) -> Result<Token, LexerError> {
        let byte_start = self.iter.byte_idx;
        let char_start = self.iter.char_idx;
        let mut name = Vec::new();
        while let Some(c) = self.iter.peek() {
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
            )),
            "break" => Ok(Token::new(
                Name(Keyword(Break)),
                char_start,
                self.iter.char_idx,
            )),
            "do" => Ok(Token::new(
                Name(Keyword(Do)),
                char_start,
                self.iter.char_idx,
            )),
            "else" => Ok(Token::new(
                Name(Keyword(Else)),
                char_start,
                self.iter.char_idx,
            )),
            "elseif" => Ok(Token::new(
                Name(Keyword(Elseif)),
                char_start,
                self.iter.char_idx,
            )),
            "end" => Ok(Token::new(
                Name(Keyword(End)),
                char_start,
                self.iter.char_idx,
            )),
            "false" => Ok(Token::new(
                Name(Keyword(False)),
                char_start,
                self.iter.char_idx,
            )),
            "for" => Ok(Token::new(
                Name(Keyword(For)),
                char_start,
                self.iter.char_idx,
            )),
            "function" => Ok(Token::new(
                Name(Keyword(Function)),
                char_start,
                self.iter.char_idx,
            )),
            "if" => Ok(Token::new(
                Name(Keyword(If)),
                char_start,
                self.iter.char_idx,
            )),
            "in" => Ok(Token::new(
                Name(Keyword(In)),
                char_start,
                self.iter.char_idx,
            )),
            "local" => Ok(Token::new(
                Name(Keyword(Local)),
                char_start,
                self.iter.char_idx,
            )),
            "nil" => Ok(Token::new(
                Name(Keyword(Nil)),
                char_start,
                self.iter.char_idx,
            )),
            "not" => Ok(Token::new(
                Name(Keyword(Not)),
                char_start,
                self.iter.char_idx,
            )),
            "or" => Ok(Token::new(
                Name(Keyword(Or)),
                char_start,
                self.iter.char_idx,
            )),
            "repeat" => Ok(Token::new(
                Name(Keyword(Repeat)),
                char_start,
                self.iter.char_idx,
            )),
            "return" => Ok(Token::new(
                Name(Keyword(Return)),
                char_start,
                self.iter.char_idx,
            )),
            "then" => Ok(Token::new(
                Name(Keyword(Then)),
                char_start,
                self.iter.char_idx,
            )),
            "true" => Ok(Token::new(
                Name(Keyword(True)),
                char_start,
                self.iter.char_idx,
            )),
            "until" => Ok(Token::new(
                Name(Keyword(Until)),
                char_start,
                self.iter.char_idx,
            )),
            "while" => Ok(Token::new(
                Name(Keyword(While)),
                char_start,
                self.iter.char_idx,
            )),
            _ => Ok(build_token(
                Name(Identifier),
                self.input,
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
            while let Some(next) = self.iter.peek() {
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
            return Ok(build_token(
                Literal(Number),
                self.input,
                char_start,
                self.iter.char_idx,
                byte_start,
                self.iter.byte_idx,
            ));
        } else {
            let mut seen_separator = false;
            while let Some(next) = self.iter.peek() {
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
                        while let Some(next) = self.iter.peek() {
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
                            return Ok(build_token(
                                Literal(Number),
                                self.input,
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
        Ok(build_token(
            Literal(Number),
            self.input,
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
                    return Ok(build_token(
                        Literal(Str(DoubleQuoted)),
                        self.input,
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                    ));
                } else {
                    return Ok(build_token(
                        Literal(Str(SingleQuoted)),
                        self.input,
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
                                    if let Some(digit) = self.iter.peek() {
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
                    return Ok(build_token(
                        Literal(Str(Long { level })),
                        self.input,
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

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();
        let start = self.iter.char_idx;
        if let Some(c) = self.iter.peek() {
            match c {
                '-' => {
                    self.iter.next();
                    if self.consume_char('-') {
                        Ok(self.scan_comment()?)
                    } else {
                        Ok(Token::new(Op(Minus), start, self.iter.char_idx))
                    }
                }
                '+' => {
                    // TODO: make macro to reduce verbosity here (once overall shape has settled
                    // down).
                    self.iter.next();
                    Ok(Token::new(Op(Plus), start, self.iter.char_idx))
                }
                '*' => {
                    self.iter.next();
                    Ok(Token::new(Op(Star), start, self.iter.char_idx))
                }
                '/' => {
                    self.iter.next();
                    Ok(Token::new(Op(Slash), start, self.iter.char_idx))
                }
                '%' => {
                    self.iter.next();
                    Ok(Token::new(Op(Percent), start, self.iter.char_idx))
                }
                '^' => {
                    self.iter.next();
                    Ok(Token::new(Op(Caret), start, self.iter.char_idx))
                }
                '#' => {
                    self.iter.next();
                    Ok(Token::new(Op(Hash), start, self.iter.char_idx))
                }
                '=' => {
                    let mut eq_count = 0;
                    while self.consume_char('=') {
                        eq_count += 1;
                    }
                    match eq_count {
                        1 => Ok(Token::new(Op(Assign), start, self.iter.char_idx)),
                        2 => Ok(Token::new(Op(Eq), start, self.iter.char_idx)),
                        _ => Err(LexerError {
                            kind: LexerErrorKind::InvalidOperator,
                            position: start,
                        }),
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
                        Ok(Token::new(Op(Ne), start, self.iter.char_idx))
                    } else {
                        Err(LexerError {
                            kind: LexerErrorKind::InvalidOperator,
                            position: start,
                        })
                    }
                }
                '<' => {
                    self.iter.next();
                    if self.consume_char('=') {
                        Ok(Token::new(Op(Lte), start, self.iter.char_idx))
                    } else {
                        Ok(Token::new(Op(Lt), start, self.iter.char_idx))
                    }
                }
                '>' => {
                    self.iter.next();
                    if self.consume_char('=') {
                        Ok(Token::new(Op(Gte), start, self.iter.char_idx))
                    } else {
                        Ok(Token::new(Op(Gt), start, self.iter.char_idx))
                    }
                }
                '(' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Lparen), start, self.iter.char_idx))
                }
                ')' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Rparen), start, self.iter.char_idx))
                }
                '{' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Lcurly), start, self.iter.char_idx))
                }
                '}' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Rcurly), start, self.iter.char_idx))
                }
                '[' => {
                    self.iter.next();
                    if self.consume_char('[') {
                        Ok(self.scan_long_string(0)?)
                    } else {
                        let mut eq_count = 0;
                        while self.consume_char('=') {
                            eq_count += 1;
                        }
                        if eq_count > 0 {
                            Ok(self.scan_long_string(eq_count)?)
                        } else {
                            Ok(Token::new(Punctuator(Lbracket), start, self.iter.char_idx))
                        }
                    }
                }
                ']' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Rbracket), start, self.iter.char_idx))
                }
                ';' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Semi), start, self.iter.char_idx))
                }
                ':' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Colon), start, self.iter.char_idx))
                }
                ',' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Comma), start, self.iter.char_idx))
                }
                '.' => {
                    let mut dot_count = 0;
                    while self.consume_char('.') {
                        dot_count += 1;
                    }
                    match dot_count {
                        1 => Ok(Token::new(Punctuator(Dot), start, self.iter.char_idx)),
                        2 => Ok(Token::new(Op(Concat), start, self.iter.char_idx)),
                        3 => Ok(Token::new(Op(Vararg), start, self.iter.char_idx)),
                        _ => Err(LexerError {
                            kind: LexerErrorKind::InvalidOperator,
                            position: start,
                        }),
                    }
                }
                '\'' | '"' => Ok(self.scan_string()?),
                '0'..='9' => Ok(self.scan_number()?),
                'A'..='Z' | 'a'..='z' | '_' => Ok(self.scan_name()?),
                _ => {
                    // For docvim's purposes it is better to fail gracefully and emit some
                    // "Unknown" tokens for stuff we don't recognize, so that it can at least take
                    // its best shot at generating documentation.
                    self.iter.next();
                    Ok(Token::new(Unknown, start, self.iter.char_idx))
                }
            }
        } else {
            Err(LexerError {
                kind: LexerErrorKind::EndOfInput,
                position: start,
            })
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.iter.peek() {
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

    /// Consumes the lexer's input and returns `Some(LexerError)` on encountering an error, or
    /// `None` if the input is valid.
    #[cfg(test)]
    fn validate(&mut self) -> Option<LexerError> {
        if self.iter.char_idx > 0 {
            panic!("validate() called on partially consumed Lexer");
        }

        loop {
            match self.next_token() {
                Err(e) => match e {
                    LexerError {
                        kind: LexerErrorKind::EndOfInput,
                        position: _,
                    } => {
                        return None;
                    }
                    _ => {
                        return Some(e);
                    }
                },
                _ => (),
            }
        }
    }
}

// Jump through some hoops to pacify the borrow-checker.
#[cfg(test)]
fn collect(input: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(input);
    let mut vec = Vec::new();
    loop {
        let byte_start = lexer.iter.byte_idx;
        if let Ok(t) = lexer.next_token() {
            if t.contents.is_some() {
                vec.push(build_token(
                    t.kind,
                    input,
                    t.start,
                    t.end,
                    byte_start,
                    lexer.iter.byte_idx,
                ));
            } else {
                vec.push(Token {
                    kind: t.kind,
                    start: t.start,
                    end: t.end,
                    contents: None,
                });
            }
        } else {
            break;
        }
    }
    vec
}

#[cfg(test)]
mod tests {
    use super::*;
    use LexerErrorKind;

    macro_rules! assert_lexes {
        ($input:expr, $expected:expr) => {
            assert_eq!(collect($input), $expected)
        };
    }

    #[should_panic(expected = "called on partially consumed Lexer")]
    #[test]
    fn validate_panics_if_iteration_has_started() {
        let mut lexer = Lexer::new("print('1')");
        lexer.next_token().expect("failed to produce a token");
        lexer.validate();
    }

    #[test]
    fn lexes_unicode() {
        assert_lexes!(
            "-- cañón\n-- träumen",
            vec![
                Token {
                    contents: Some("-- cañón\n"),
                    end: 9,
                    kind: Comment(LineComment),
                    start: 0,
                },
                Token {
                    contents: Some("-- träumen"),
                    end: 19,
                    kind: Comment(LineComment),
                    start: 9,
                }
            ]
        );
    }

    #[test]
    fn lexes_line_comments() {
        assert_lexes!(
            "-- TODO: something",
            vec![Token {
                contents: Some("-- TODO: something"),
                end: 18,
                kind: Comment(LineComment),
                start: 0,
            }]
        );
        assert_lexes!(
            "--[ Almost a block comment, but not quite",
            vec![Token {
                contents: Some("--[ Almost a block comment, but not quite"),
                end: 41,
                kind: Comment(LineComment),
                start: 0,
            }]
        );
    }

    #[test]
    fn lexes_block_comments() {
        assert_lexes!(
            "--[[\nstuff\n--]]",
            vec![Token {
                contents: Some("--[[\nstuff\n--]]"),
                end: 15,
                kind: Comment(BlockComment),
                start: 0,
            }]
        );
    }

    #[test]
    fn lexes_valid_numbers() {
        // Examples from Lua docs.
        assert_lexes!(
            "3",
            vec![Token {
                contents: Some("3"),
                end: 1,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "3.0",
            vec![Token {
                contents: Some("3.0"),
                end: 3,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "3.1416",
            vec![Token {
                contents: Some("3.1416"),
                end: 6,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "314.16e-2",
            vec![Token {
                contents: Some("314.16e-2"),
                end: 9,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0.31416E1",
            vec![Token {
                contents: Some("0.31416E1"),
                end: 9,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0xff",
            vec![Token {
                contents: Some("0xff"),
                end: 4,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0x56",
            vec![Token {
                contents: Some("0x56"),
                end: 4,
                kind: Literal(Number),
                start: 0,
            }]
        );

        // These ones look fishy but actually aren't.
        assert_lexes!(
            "0xff.1", // ie. 255.0625
            vec![Token {
                contents: Some("0xff.1"),
                end: 6,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0xff.ff", // ie. 255.99609375.
            vec![Token {
                contents: Some("0xff.ff"),
                end: 7,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0xffe10", // 1048080 because "e" doesn't mean exponent here.
            vec![Token {
                contents: Some("0xffe10"),
                end: 7,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0xffe-10", // "e" not exponent; this is `(0xffe) - 10` ie. 4084.
            vec![
                Token {
                    contents: Some("0xffe"),
                    end: 5,
                    kind: Literal(Number),
                    start: 0,
                },
                Token {
                    contents: None,
                    end: 6,
                    kind: Op(Minus),
                    start: 5,
                },
                Token {
                    contents: Some("10"),
                    end: 8,
                    kind: Literal(Number),
                    start: 6,
                }
            ]
        );
        assert_lexes!(
            "0xff.ffe2", // ie. 255.99954223633.
            vec![Token {
                contents: Some("0xff.ffe2"),
                end: 9,
                kind: Literal(Number),
                start: 0,
            }]
        );
    }

    #[test]
    fn rejects_invalid_numbers() {
        assert_eq!(
            Lexer::new("3.0.1").validate(),
            Some(LexerError {
                kind: LexerErrorKind::InvalidNumberLiteral,
                position: 3
            })
        );
        assert_eq!(
            Lexer::new("3e-2.1").validate(),
            Some(LexerError {
                kind: LexerErrorKind::InvalidNumberLiteral,
                position: 4
            })
        );
        assert_eq!(
            Lexer::new("0xffx").validate(),
            Some(LexerError {
                kind: LexerErrorKind::InvalidNumberLiteral,
                position: 4
            })
        );
        assert_eq!(
            Lexer::new("0xff.0xff").validate(),
            Some(LexerError {
                kind: LexerErrorKind::InvalidNumberLiteral,
                position: 6
            })
        );
    }

    #[test]
    fn lexes_strings() {
        assert_lexes!(
            "'hello'",
            vec![Token {
                contents: Some("'hello'"),
                end: 7,
                kind: Literal(Str(SingleQuoted)),
                start: 0,
            }]
        );
    }
}
