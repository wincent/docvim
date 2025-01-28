use crate::error::*;
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

    // 1-indexed, as these are all editor-centric fields.
    pub column_start: usize,
    pub column_end: usize,
    pub line_start: usize,
    pub line_end: usize,
}

impl Token {
    fn new(
        kind: TokenKind,
        char_start: usize,
        char_end: usize,
        byte_start: usize,
        byte_end: usize,
        column_start: usize,
        column_end: usize,
        line_start: usize,
        line_end: usize,
    ) -> Token {
        Token {
            kind,
            char_start,
            char_end,
            byte_start,
            byte_end,
            column_start,
            column_end,
            line_start,
            line_end,
        }
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
    Ne,      // ~= (not equal)
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

macro_rules! make_token {
    ($self:expr, $kind:expr) => {
        Some(Ok(Token::new(
            $kind,
            $self.char_start,
            $self.iter.char_idx,
            $self.byte_start,
            $self.iter.byte_idx,
            $self.column_start,
            $self.iter.column_idx,
            $self.line_start,
            $self.iter.line_idx,
        )))
    };
}

pub struct Lexer<'a> {
    pub input: &'a str,
    pub tokens: std::iter::Peekable<Tokens<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            tokens: Tokens {
                iter: Peekable::new(input),
                char_start: 0,
                byte_start: 0,
                column_start: 1,
                line_start: 1,
            }
            .peekable(),
        }
    }

    /// Consumes the lexer's input and returns `Some(LexerError)` on encountering an error, or
    /// `None` if the input is valid.
    #[cfg(test)]
    fn validate(&mut self) -> Option<LexerError> {
        loop {
            match self.tokens.next() {
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
    char_start: usize,
    byte_start: usize,
    column_start: usize,
    line_start: usize,
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

    fn scan_comment(&mut self) -> Option<Result<Token, LexerError>> {
        self.char_start = self.iter.char_idx - 2; // Subtract length of "--" prefix.
        self.byte_start = self.iter.byte_idx - 2;
        if self.consume_char('[') {
            let mut eq_count = 0;
            loop {
                if self.consume_char('=') {
                    eq_count += 1;
                } else {
                    break;
                }
            }
            if self.consume_char('[') {
                return self.scan_block_comment(eq_count);
            }
        }
        self.scan_line_comment()
    }

    /// Scans until seeing "]]" (or "]=]", or "]==]" etc).
    fn scan_block_comment(&mut self, eq_count: i32) -> Option<Result<Token, LexerError>> {
        loop {
            let ch = self.iter.next();
            match ch {
                Some(']') => {
                    let mut found_eq = 0;
                    while found_eq < eq_count {
                        if self.consume_char('=') {
                            found_eq += 1;
                        } else {
                            break;
                        }
                    }
                    if found_eq == eq_count && self.consume_char(']') {
                        return make_token!(self, Comment(BlockComment));
                    }
                }
                None => {
                    return Some(Err(LexerError {
                        kind: LexerErrorKind::UnterminatedBlockComment,
                        position: self.iter.char_idx,
                    }));
                }
                _ => (),
            }
        }
    }

    /// Scans until end of line, or end of input.
    fn scan_line_comment(&mut self) -> Option<Result<Token, LexerError>> {
        loop {
            let ch = self.iter.next();
            match ch {
                Some('\n') | None => {
                    return make_token!(self, Comment(LineComment));
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
        let column_start = self.iter.column_idx;
        let line_start = self.iter.line_idx;
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
        let kind = match &name[..] {
            "and" => Name(Keyword(And)),
            "break" => Name(Keyword(Break)),
            "do" => Name(Keyword(Do)),
            "else" => Name(Keyword(Else)),
            "elseif" => Name(Keyword(Elseif)),
            "end" => Name(Keyword(End)),
            "false" => Name(Keyword(False)),
            "for" => Name(Keyword(For)),
            "function" => Name(Keyword(Function)),
            "if" => Name(Keyword(If)),
            "in" => Name(Keyword(In)),
            "local" => Name(Keyword(Local)),
            "nil" => Name(Keyword(Nil)),
            "not" => Name(Keyword(Not)),
            "or" => Name(Keyword(Or)),
            "repeat" => Name(Keyword(Repeat)),
            "return" => Name(Keyword(Return)),
            "then" => Name(Keyword(Then)),
            "true" => Name(Keyword(True)),
            "until" => Name(Keyword(Until)),
            "while" => Name(Keyword(While)),
            _ => Name(Identifier),
        };
        Ok(Token::new(
            kind,
            char_start,
            self.iter.char_idx,
            byte_start,
            self.iter.byte_idx,
            column_start,
            self.iter.column_idx,
            line_start,
            self.iter.line_idx,
        ))
    }

    fn scan_number(&mut self) -> Result<Token, LexerError> {
        let byte_start = self.iter.byte_idx;
        let char_start = self.iter.char_idx;
        let column_start = self.iter.column_idx;
        let line_start = self.iter.line_idx;
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
                column_start,
                self.iter.column_idx,
                line_start,
                self.iter.line_idx,
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
                                column_start,
                                self.iter.column_idx,
                                line_start,
                                self.iter.line_idx,
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
            column_start,
            self.iter.column_idx,
            line_start,
            self.iter.line_idx,
        ))
    }

    fn scan_string(&mut self) -> Result<Token, LexerError> {
        let byte_start = self.iter.byte_idx;
        let char_start = self.iter.char_idx;
        let column_start = self.iter.column_idx;
        let line_start = self.iter.line_idx;
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
                        column_start,
                        self.iter.column_idx,
                        line_start,
                        self.iter.line_idx,
                    ));
                } else {
                    return Ok(Token::new(
                        Literal(Str(SingleQuoted)),
                        char_start,
                        self.iter.char_idx,
                        byte_start,
                        self.iter.byte_idx,
                        column_start,
                        self.iter.column_idx,
                        line_start,
                        self.iter.line_idx,
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
        let column_start = self.iter.column_idx - level - 2;
        let line_start = self.iter.line_idx;
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
                        column_start,
                        self.iter.column_idx,
                        line_start,
                        self.iter.line_idx,
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
        self.char_start = self.iter.char_idx;
        self.byte_start = self.iter.byte_idx;
        self.column_start = self.iter.column_idx;
        self.line_start = self.iter.line_idx;
        if let Some(&c) = self.iter.peek() {
            match c {
                '-' => {
                    self.iter.next();
                    if self.consume_char('-') {
                        self.scan_comment()
                    } else {
                        make_token!(self, Op(Minus))
                    }
                }
                '+' => {
                    // TODO: make macro to reduce verbosity here (once overall shape has settled
                    // down).
                    self.iter.next();
                    make_token!(self, Op(Plus))
                }
                '*' => {
                    self.iter.next();
                    make_token!(self, Op(Star))
                }
                '/' => {
                    self.iter.next();
                    make_token!(self, Op(Slash))
                }
                '%' => {
                    self.iter.next();
                    make_token!(self, Op(Percent))
                }
                '^' => {
                    self.iter.next();
                    make_token!(self, Op(Caret))
                }
                '#' => {
                    self.iter.next();
                    make_token!(self, Op(Hash))
                }
                '=' => {
                    let mut eq_count = 0;
                    while self.consume_char('=') {
                        eq_count += 1;
                    }
                    match eq_count {
                        1 => make_token!(self, Op(Assign)),
                        2 => make_token!(self, Op(Eq)),
                        _ => Some(Err(LexerError {
                            kind: LexerErrorKind::InvalidOperator,
                            position: self.char_start,
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
                        make_token!(self, Op(Ne))
                    } else {
                        Some(Err(LexerError {
                            kind: LexerErrorKind::InvalidOperator,
                            position: self.char_start,
                        }))
                    }
                }
                '<' => {
                    self.iter.next();
                    if self.consume_char('=') {
                        make_token!(self, Op(Lte))
                    } else {
                        make_token!(self, Op(Lt))
                    }
                }
                '>' => {
                    self.iter.next();
                    if self.consume_char('=') {
                        make_token!(self, Op(Gte))
                    } else {
                        make_token!(self, Op(Gt))
                    }
                }
                '(' => {
                    self.iter.next();
                    make_token!(self, Punctuator(Lparen))
                }
                ')' => {
                    self.iter.next();
                    make_token!(self, Punctuator(Rparen))
                }
                '{' => {
                    self.iter.next();
                    make_token!(self, Punctuator(Lcurly))
                }
                '}' => {
                    self.iter.next();
                    make_token!(self, Punctuator(Rcurly))
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
                            if self.consume_char('[') {
                                Some(self.scan_long_string(eq_count))
                            } else {
                                Some(Err(LexerError {
                                    kind: LexerErrorKind::InvalidOperator,
                                    position: self.char_start,
                                }))
                            }
                        } else {
                            make_token!(self, Punctuator(Lbracket))
                        }
                    }
                }
                ']' => {
                    self.iter.next();
                    make_token!(self, Punctuator(Rbracket))
                }
                ';' => {
                    self.iter.next();
                    make_token!(self, Punctuator(Semi))
                }
                ':' => {
                    self.iter.next();
                    make_token!(self, Punctuator(Colon))
                }
                ',' => {
                    self.iter.next();
                    make_token!(self, Punctuator(Comma))
                }
                '.' => {
                    let mut dot_count = 0;
                    while self.consume_char('.') {
                        dot_count += 1;
                    }
                    match dot_count {
                        1 => make_token!(self, Punctuator(Dot)),
                        2 => make_token!(self, Op(Concat)),
                        3 => make_token!(self, Op(Vararg)),
                        _ => Some(Err(LexerError {
                            kind: LexerErrorKind::InvalidOperator,
                            position: self.char_start,
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
                    make_token!(self, Unknown)
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
                Lexer::new(&$input).tokens.map(|token| token.unwrap()).collect::<Vec<Token>>(),
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
                    column_start: 1,
                    column_end: 1,
                    line_start: 1,
                    line_end: 2,
                    kind: Comment(LineComment),
                },
                Token {
                    byte_end: 22,
                    byte_start: 11,
                    char_end: 19,
                    char_start: 9,
                    column_start: 1,
                    column_end: 11,
                    line_start: 2,
                    line_end: 2,
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
                column_start: 1,
                column_end: 19,
                line_start: 1,
                line_end: 1,
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
                column_start: 1,
                column_end: 42,
                line_start: 1,
                line_end: 1,
                kind: Comment(LineComment),
            }]
        );
    }

    #[test]
    fn lexes_block_comments() {
        assert_lexes!(
            "--[[\nstuff\n]]",
            vec![Token {
                byte_end: 13,
                byte_start: 0,
                char_end: 13,
                char_start: 0,
                column_start: 1,
                column_end: 3,
                line_start: 1,
                line_end: 3,
                kind: Comment(BlockComment),
            }]
        );
        assert_lexes!(
            "--[=[\nstuff\n]=]",
            vec![Token {
                byte_end: 15,
                byte_start: 0,
                char_end: 15,
                char_start: 0,
                column_start: 1,
                column_end: 4,
                line_start: 1,
                line_end: 3,
                kind: Comment(BlockComment),
            }]
        );
        assert_lexes!(
            "--[==[\nstuff\n]==]",
            vec![Token {
                byte_end: 17,
                byte_start: 0,
                char_end: 17,
                char_start: 0,
                column_start: 1,
                column_end: 5,
                line_start: 1,
                line_end: 3,
                kind: Comment(BlockComment),
            }]
        );
        assert_lexes!(
            "--[===[\ncomment continues after: ]]\n]===]",
            vec![Token {
                byte_end: 41,
                byte_start: 0,
                char_end: 41,
                char_start: 0,
                column_start: 1,
                column_end: 6,
                line_start: 1,
                line_end: 3,
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
                column_start: 1,
                column_end: 2,
                line_start: 1,
                line_end: 1,
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
                column_start: 1,
                column_end: 4,
                line_start: 1,
                line_end: 1,
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
                column_start: 1,
                column_end: 7,
                line_start: 1,
                line_end: 1,
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
                column_start: 1,
                column_end: 10,
                line_start: 1,
                line_end: 1,
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
                column_start: 1,
                column_end: 10,
                line_start: 1,
                line_end: 1,
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
                column_start: 1,
                column_end: 5,
                line_start: 1,
                line_end: 1,
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
                column_start: 1,
                column_end: 5,
                line_start: 1,
                line_end: 1,
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
                column_start: 1,
                column_end: 7,
                line_start: 1,
                line_end: 1,
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
                column_start: 1,
                column_end: 8,
                line_start: 1,
                line_end: 1,
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
                column_start: 1,
                column_end: 8,
                line_start: 1,
                line_end: 1,
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
                    column_start: 1,
                    column_end: 6,
                    line_start: 1,
                    line_end: 1,
                    kind: Literal(Number),
                },
                Token {
                    byte_end: 6,
                    byte_start: 5,
                    char_end: 6,
                    char_start: 5,
                    column_start: 6,
                    column_end: 7,
                    line_start: 1,
                    line_end: 1,
                    kind: Op(Minus)
                },
                Token {
                    byte_end: 8,
                    byte_start: 6,
                    char_end: 8,
                    char_start: 6,
                    column_start: 7,
                    column_end: 9,
                    line_start: 1,
                    line_end: 1,
                    kind: Literal(Number),
                }
            ]
        );
        assert_lexes!(
            "0xff.ffe2", // ie. 255.99954223633.
            vec![Token {
                byte_end: 9,
                char_end: 9,
                char_start: 0,
                byte_start: 0,
                column_start: 1,
                column_end: 10,
                line_start: 1,
                line_end: 1,
                kind: Literal(Number),
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
                column_start: 1,
                column_end: 8,
                line_start: 1,
                line_end: 1,
            }]
        );
        assert_lexes!(
            "\"foo\"",
            vec![Token {
                kind: Literal(Str(DoubleQuoted)),
                char_start: 0,
                char_end: 5,
                byte_start: 0,
                byte_end: 5,
                column_start: 1,
                column_end: 6,
                line_start: 1,
                line_end: 1,
            }]
        );
    }

    #[test]
    fn lexes_long_strings() {
        assert_lexes!(
            "[[hello]]",
            vec![Token {
                kind: Literal(Str(Long { level: 0 })),
                char_start: 0,
                char_end: 9,
                byte_start: 0,
                byte_end: 9,
                column_start: 1,
                column_end: 10,
                line_start: 1,
                line_end: 1,
            }]
        );
        assert_lexes!(
            "[===[there]===]",
            vec![Token {
                kind: Literal(Str(Long { level: 3 })),
                char_start: 0,
                char_end: 15,
                byte_start: 0,
                byte_end: 15,
                column_start: 1,
                column_end: 16,
                line_start: 1,
                line_end: 1,
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
}
