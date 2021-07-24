use std::fmt;

mod peekable;

use crate::peekable::*;

use self::CommentKind::*;
use self::KeywordKind::*;
use self::LiteralKind::*;
use self::NameKind::*;
use self::OpKind::*;
use self::PunctuatorKind::*;
use self::StrKind::*;
use self::TokenKind::*;

// TODO: given that we can't index efficiently into strings, i had thought that we could just store
// start/end and do a slow extraction whenever we need to do things like report errors... of
// course, now I realize that there are many node types where we want the token text outside of
// error pathways. so, we need to store the text in many cases (eg. comments, literals, names etc)
//
// i can either make an owned String (copying) or try Box<str> (owned pointer to slice, which would
// be faster, but harder to reason about... have to make sure lifetime of input extends beyond
// lifetime of parser)... perhaps String::into_boxed_str() to make a Box<str>
// because Box<str> is relatively cheap (not a copy of the whole string), i actually could include
// it with every single token, which might be nice

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
    pub contents: Option<String>
}

impl Token {
    fn new(kind: TokenKind, start: usize, end: usize) -> Token {
        Token { kind, start, end, contents: None }
    }
}

// TODO: use builder pattern to enable us to construct a token in steps
// ie. we can set kind, start at beginning, accumulate contents into it (optionally), and then set
// end at end

pub struct TokenBuilder {
    kind: TokenKind,
    start: usize,
    end: usize,
    contents: Option<String>
}

impl TokenBuilder {
    fn new(kind: TokenKind, start: usize) -> TokenBuilder {
        TokenBuilder {
            kind,
            start,
            end: start,
            contents: None
        }
    }

    fn build(&self) -> Token {
        Token {
            kind: self.kind,
            start: self.start,
            end: self.end,
            contents: self.contents.clone()
        }
    }

    fn push(&mut self, ch: char) {
        match self.contents.as_mut() {
            Some(contents) => {
                contents.push(ch);
            },
            None => {
                self.contents = Some(ch.to_string());
            }
        }
        self.end += 1;
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
    kind: LexerErrorKind,
    position: usize,
}

impl LexerError {
    pub fn new(kind: LexerErrorKind, position: usize) -> LexerError {
        Self { kind, position }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} ({})", self.kind.to_str(), self.position)
    }
}

// This is pretty useless if I can't pass position info into it.
impl From<LexerErrorKind> for LexerError {
    fn from(kind: LexerErrorKind) -> LexerError {
        LexerError::new(kind, 0)
    }
}

pub struct Lexer<'a> {
    iter: Peekable<std::str::Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            iter: Peekable::new(input.chars()),
        }
    }

    /// Consumes the specified `char`, returning `true` on success and `false` if nothing was
    /// consumed.
    fn consume_char(&mut self, ch: char) -> bool {
        match self.iter.peek() {
            Some(seen) => {
                if *seen == ch {
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
        let start = self.iter.position - 2; // Subtract length of "--" prefix.
        let mut bracket_count = 0;
        if self.consume_char('[') { bracket_count += 1 }
        if self.consume_char('[') { bracket_count += 1 }
        if bracket_count == 2 {
            // TODO: route consume through the builder, or something?, because this is all super
            // ugly; instead of passing lexer into builder, might be a sign that i should just
            // squish the builder into the lexer :ugh
            let mut builder = TokenBuilder::new(Comment(BlockComment), start);
            builder.push('-'); // TODO find a better pattern for this.
            builder.push('-');
            builder.push('[');
            builder.push('[');
            self.scan_block_comment(&mut builder)
        } else {
            let mut builder = TokenBuilder::new(Comment(LineComment), start);
            builder.push('-'); // TODO find a better pattern for this.
            builder.push('-');
            if bracket_count == 1 {
                builder.push('[');
            }
            self.scan_line_comment(&mut builder)
        }
    }

    /// Scans until seeing "--]]".
    fn scan_block_comment(&mut self, builder: &mut TokenBuilder) -> Result<Token, LexerError> {
        loop {
            let ch = self.iter.next();
            match ch {
                Some('-') => {
                    builder.push('-');
                    // Can't just chain `consume_char` calls here (for "-", "-", "[", and "[")
                    // because the greedy match would fail for text like "---[[" which is also a
                    // valid marker to end a block comment.
                    let mut dash_count = 1;
                    while self.consume_char('-') {
                        builder.push('-');
                        dash_count += 1;
                    }
                    if dash_count >= 2 {
                        let mut bracket_count = 0;
                        if self.consume_char(']') { builder.push(']'); bracket_count += 1 }
                        if self.consume_char(']') { builder.push(']'); bracket_count += 1 }
                        if bracket_count == 2 {
                            return Ok(builder.build());
                        }
                    }
                }
                Some(ch) => {
                    builder.push(ch);
                }
                None => {
                    return Err(LexerError {
                        kind: LexerErrorKind::UnterminatedBlockComment,
                        position: self.iter.position,
                    });
                }
            }
        }
    }

    /// Scans until end of line, or end of input.
    fn scan_line_comment(&mut self, builder: &mut TokenBuilder) -> Result<Token, LexerError> {
        loop {
            let ch = self.iter.next();
            match ch {
                Some('\n') => {
                    builder.push('\n');
                    return Ok(builder.build());
                }
                Some(ch) => {
                    builder.push(ch);
                }
                None => {
                    return Ok(builder.build());
                }
            }
        }
    }

    // From Lua docs: "The definition of letter depends on the current locale: any character
    // considered alphabetic by the current locale can be used in an identifier", so the below
    // could use a bit of work...
    fn scan_name(&mut self) -> Result<Token, LexerError> {
        let start = self.iter.position;
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
        Ok(Token::new(
            match &name[..] {
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
            },
            start,
            self.iter.position,
        ))
    }

    fn scan_number(&mut self) -> Result<Token, LexerError> {
        let start = self.iter.position;
        let err = |iter: &Peekable<std::str::Chars>| {
            Err(LexerError {
                kind: LexerErrorKind::InvalidNumberLiteral,
                position: iter.position,
            })
        };
        let token = |iter: &Peekable<std::str::Chars>| {
            Ok(Token::new(Literal(Number), start, iter.position))
        };
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
                            return err(&self.iter);
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
                        return err(&self.iter);
                    }
                }
            }
            return token(&self.iter);
        } else {
            let mut seen_exp = false;
            let mut seen_separator = false;
            while let Some(next) = self.iter.peek() {
                match next {
                    '0'..='9' => {
                        self.iter.next();
                    }
                    '.' => {
                        if seen_separator {
                            return err(&self.iter);
                        } else {
                            seen_separator = true;
                            self.iter.next();
                        }
                    }
                    'E' | 'e' => {
                        if seen_exp {
                            return err(&self.iter);
                        } else {
                            self.iter.next();
                            seen_exp = true;
                            seen_separator = false;
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
                                        return err(&self.iter);
                                    }
                                }
                            }
                            if exp_digits_count > 0 {
                                return token(&self.iter);
                            } else {
                                return err(&self.iter);
                            }
                        }
                    }
                    _ => {
                        break;
                    }
                }
            }
        }
        token(&self.iter)
    }

    fn scan_string(&mut self) -> Result<Token, LexerError> {
        let start = self.iter.position;
        let quote = self.iter.next().unwrap();
        while let Some(c) = self.iter.next() {
            if c == quote {
                if quote == '"' {
                    return Ok(Token::new(
                        Literal(Str(DoubleQuoted)),
                        start,
                        self.iter.position,
                    ));
                } else {
                    return Ok(Token::new(
                        Literal(Str(SingleQuoted)),
                        start,
                        self.iter.position,
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
                                    position: self.iter.position,
                                });
                            }
                        }
                    } else {
                        return Err(LexerError {
                            kind: LexerErrorKind::UnterminatedEscapeSequence,
                            position: self.iter.position,
                        });
                    }
                }
                _ => (), // Non-escaped string contents.
            }
        }
        Err(LexerError {
            kind: LexerErrorKind::UnterminatedStringLiteral,
            position: self.iter.position,
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
        let start = self.iter.position - level - 2;
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
                        start,
                        self.iter.position,
                    ));
                }
            }
        }
        Err(LexerError {
            kind: LexerErrorKind::UnterminatedStringLiteral,
            position: self.iter.position,
        })
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();
        let start = self.iter.position;
        if let Some(&c) = self.iter.peek() {
            match c {
                '-' => {
                    self.iter.next();
                    if self.consume_char('-') {
                        Ok(self.scan_comment()?)
                    } else {
                        Ok(Token::new(Op(Minus), start, self.iter.position))
                    }
                }
                '+' => {
                    // TODO: make macro to reduce verbosity here (once overall shape has settled
                    // down).
                    self.iter.next();
                    Ok(Token::new(Op(Plus), start, self.iter.position))
                }
                '*' => {
                    self.iter.next();
                    Ok(Token::new(Op(Star), start, self.iter.position))
                }
                '/' => {
                    self.iter.next();
                    Ok(Token::new(Op(Slash), start, self.iter.position))
                }
                '%' => {
                    self.iter.next();
                    Ok(Token::new(Op(Percent), start, self.iter.position))
                }
                '^' => {
                    self.iter.next();
                    Ok(Token::new(Op(Caret), start, self.iter.position))
                }
                '#' => {
                    self.iter.next();
                    Ok(Token::new(Op(Hash), start, self.iter.position))
                }
                '=' => {
                    let mut eq_count = 0;
                    while self.consume_char('=') {
                        eq_count += 1;
                    }
                    match eq_count {
                        1 => Ok(Token::new(Op(Assign), start, self.iter.position)),
                        2 => Ok(Token::new(Op(Eq), start, self.iter.position)),
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
                        Ok(Token::new(Op(Ne), start, self.iter.position))
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
                        Ok(Token::new(Op(Lte), start, self.iter.position))
                    } else {
                        Ok(Token::new(Op(Lt), start, self.iter.position))
                    }
                }
                '>' => {
                    self.iter.next();
                    if self.consume_char('=') {
                        Ok(Token::new(Op(Gte), start, self.iter.position))
                    } else {
                        Ok(Token::new(Op(Gt), start, self.iter.position))
                    }
                }
                '(' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Lparen), start, self.iter.position))
                }
                ')' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Rparen), start, self.iter.position))
                }
                '{' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Lcurly), start, self.iter.position))
                }
                '}' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Rcurly), start, self.iter.position))
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
                            Ok(Token::new(Punctuator(Lbracket), start, self.iter.position))
                        }
                    }
                }
                ']' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Rbracket), start, self.iter.position))
                }
                ';' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Semi), start, self.iter.position))
                }
                ':' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Colon), start, self.iter.position))
                }
                ',' => {
                    self.iter.next();
                    Ok(Token::new(Punctuator(Comma), start, self.iter.position))
                }
                '.' => {
                    let mut dot_count = 0;
                    while self.consume_char('.') {
                        dot_count += 1;
                    }
                    match dot_count {
                        1 => Ok(Token::new(Punctuator(Dot), start, self.iter.position)),
                        2 => Ok(Token::new(Op(Concat), start, self.iter.position)),
                        3 => Ok(Token::new(Op(Vararg), start, self.iter.position)),
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
                    Ok(Token::new(Unknown, start, self.iter.position))
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

    /// Consumes the lexer's input and returns `Some(LexerError)` on encountering an error, or
    /// `None` if the input is valid.
    #[cfg(test)]
    fn validate(&mut self) -> Option<LexerError> {
        if self.iter.position > 0 {
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

impl<'a> std::iter::Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        return self.next_token().ok();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use LexerErrorKind;

    macro_rules! assert_lexes {
        ($input:expr, $expected:expr) => {
            assert_eq!(Lexer::new(&$input).collect::<Vec<Token>>(), $expected)
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
    fn lexes_line_comments() {
        assert_lexes!(
            "-- TODO: something",
            vec![Token {
                contents: Some("-- TODO: something".to_string()),
                end: 18,
                kind: Comment(LineComment),
                start: 0,
            }]
        );
        assert_lexes!(
            "--[ Almost a block comment, but not quite",
            vec![Token {
                contents: Some("--[ Almost a block comment, but not quite".to_string()),
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
                contents: Some("--[[\nstuff\n--]]".to_string()),
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
                contents: None,
                end: 1,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "3.0",
            vec![Token {
                contents: None,
                end: 3,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "3.1416",
            vec![Token {
                contents: None,
                end: 6,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "314.16e-2",
            vec![Token {
                contents: None,
                end: 9,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0.31416E1",
            vec![Token {
                contents: None,
                end: 9,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0xff",
            vec![Token {
                contents: None,
                end: 4,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0x56",
            vec![Token {
                contents: None,
                end: 4,
                kind: Literal(Number),
                start: 0,
            }]
        );

        // These ones look fishy but actually aren't.
        assert_lexes!(
            "0xff.1", // ie. 255.0625
            vec![Token {
                contents: None,
                end: 6,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0xff.ff", // ie. 255.99609375.
            vec![Token {
                contents: None,
                end: 7,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0xffe10", // 1048080 because "e" doesn't mean exponent here.
            vec![Token {
                contents: None,
                end: 7,
                kind: Literal(Number),
                start: 0,
            }]
        );
        assert_lexes!(
            "0xffe-10", // "e" not exponent; this is `(0xffe) - 10` ie. 4084.
            vec![
                Token {
                    contents: None,
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
                    contents: None,
                    end: 8,
                    kind: Literal(Number),
                    start: 6,
                }
            ]
        );
        assert_lexes!(
            "0xff.ffe2", // ie. 255.99954223633.
            vec![Token {
                contents: None,
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
                contents: None,
                end: 7,
                kind: Literal(Str(SingleQuoted)),
                start: 0,
            }]
        );
    }
}
