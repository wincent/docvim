use std::fmt;
use std::fs;

use self::CommentKind::*;
use self::KeywordKind::*;
use self::LiteralKind::*;
use self::NameKind::*;
use self::OpKind::*;
use self::PunctuatorKind::*;
use self::StrKind::*;
use self::TokenKind::*;

#[derive(Debug, Eq, PartialEq)]
struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
}

impl Token {
    fn new(kind: TokenKind, start: usize, end: usize) -> Token {
        Token { kind, start, end }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum CommentKind {
    BlockComment,
    LineComment,
}

#[derive(Debug, Eq, PartialEq)]
enum KeywordKind {
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

#[derive(Debug, Eq, PartialEq)]
enum NameKind {
    Identifier,
    Keyword(KeywordKind),
}

#[derive(Debug, Eq, PartialEq)]
enum PunctuatorKind {
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
#[derive(Debug, Eq, PartialEq)]
enum OpKind {
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

#[derive(Debug, Eq, PartialEq)]
enum LiteralKind {
    Number,
    Str(StrKind),
}

#[derive(Debug, Eq, PartialEq)]
enum StrKind {
    DoubleQuoted,
    SingleQuoted,
    Long,
}

#[derive(Debug, Eq, PartialEq)]
enum TokenKind {
    Op(OpKind),
    Comment(CommentKind),
    Literal(LiteralKind),
    Name(NameKind),
    Punctuator(PunctuatorKind),
    Unknown,
}

// TODO: move all Lexer stuff into Lexer mod
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum LexerErrorKind {
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
struct LexerError {
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

// Wrapper around standard Peekable iterator that tracks position.
struct Peekable<I: std::iter::Iterator> {
    iter: std::iter::Peekable<I>,
    position: usize,
}

impl<I: std::iter::Iterator> Peekable<I> {
    pub fn new(iter: I) -> Self {
        Peekable {
            iter: iter.peekable(),
            position: 0,
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        self.iter.peek()
    }
}

impl<I: std::iter::Iterator> std::iter::Iterator for Peekable<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        let c = self.iter.next();
        match c {
            None => (),
            _ => {
                self.position += 1;
            }
        }
        c
    }
}

struct Lexer<'a> {
    iter: Peekable<std::str::Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
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

    // fn DEAD_CODE_expect_char(&mut self, ch: char, err: LexerErrorKind) -> Result<char, LexerError> {
    //     let next = self.iter.next().ok_or(err)?;
    //     if next == ch {
    //         Ok(ch)
    //     } else {
    //         Err(LexerError::from(err))
    //     }
    // }

    fn scan_comment(&mut self) -> Result<Token, LexerError> {
        if self.consume_char('[') && self.consume_char('[') {
            self.scan_block_comment()
        } else {
            self.scan_line_comment()
        }
    }

    /// Scans until seeing "--]]".
    fn scan_block_comment(&mut self) -> Result<Token, LexerError> {
        let start = self.iter.position - 2; // Subtract length of "--" prefix.
        loop {
            match self.iter.next() {
                Some('-') => {
                    // Can't just chain `consume_char` calls here (for "-", "-", "[", and "[")
                    // because the greedy match would fail for text like "---[[" which is also a
                    // valid marker to end a block comment.
                    let mut dash_count = 1;
                    while self.consume_char('-') {
                        dash_count += 1;
                    }
                    if dash_count >= 2 && self.consume_char(']') && self.consume_char(']') {
                        return Ok(Token::new(Comment(BlockComment), start, self.iter.position));
                    }
                }
                Some(_) => {
                    ();
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
    fn scan_line_comment(&mut self) -> Result<Token, LexerError> {
        let start = self.iter.position - 2; // Subtract length of "--" prefix.
        loop {
            match self.iter.next() {
                Some('\n') | None => {
                    return Ok(Token::new(Comment(LineComment), start, self.iter.position));
                }
                _ => (),
            }
        }
    }

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
                    _ => {
                        self.iter.next();
                        break;
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
                            if let Some(next) = self.iter.next() {
                                match next {
                                    '0'..='9' => {
                                        continue;
                                    }
                                    _ => {
                                        return err(&self.iter);
                                    }
                                }
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
                    // TODO: Include level info in struct?
                    return Ok(Token::new(Literal(Str(Long)), start, self.iter.position));
                }
            }
        }
        Err(LexerError {
            kind: LexerErrorKind::UnterminatedStringLiteral,
            position: self.iter.position,
        })
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
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

pub fn run(args: Vec<String>) {
    // TODO: actual arg parsing
    let input = "sample/init.lua";

    let contents = fs::read_to_string(input).expect("unable to read file");

    println!("Text:\n{}", contents);

    let mut lexer = Lexer::new(&contents);
    loop {
        match lexer.next_token() {
            Ok(t) => println!("token: {:?}", t),
            Err(e) => {
                println!("error: {}", e);
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use LexerErrorKind;

    macro_rules! assert_lexes {
        ($input:expr, $expected:expr) => {
            let tokens: Vec<Token> = Lexer::new(&$input).collect();
            assert_eq!(tokens, $expected);
        };
    }

    #[should_panic]
    #[test]
    fn lexer_validate_panics_if_iteration_has_started() {
        let mut lexer = Lexer::new("print('1')");
        lexer.next_token();
        lexer.validate();
    }

    #[test]
    fn lexer_lexes_valid_numbers() {
        // Examples from Lua docs.
        assert_lexes!(
            "3",
            vec![Token {
                kind: Literal(Number),
                start: 0,
                end: 1,
            }]
        );
        assert_lexes!(
            "3.0",
            vec![Token {
                kind: Literal(Number),
                start: 0,
                end: 3,
            }]
        );
        assert_lexes!(
            "3.1416",
            vec![Token {
                kind: Literal(Number),
                start: 0,
                end: 6,
            }]
        );
        assert_lexes!(
            "314.16e-2",
            vec![Token {
                kind: Literal(Number),
                start: 0,
                end: 9,
            }]
        );
        assert_lexes!(
            "0.31416E1",
            vec![Token {
                kind: Literal(Number),
                start: 0,
                end: 9,
            }]
        );
        assert_lexes!(
            "0xff",
            vec![Token {
                kind: Literal(Number),
                start: 0,
                end: 4,
            }]
        );
        assert_lexes!(
            "0x56",
            vec![Token {
                kind: Literal(Number),
                start: 0,
                end: 4,
            }]
        );
    }

    // But note, all these _are_ valid.
    //
    // 0xff.1 = 255.0625
    // 0xff.ff = 255.99...
    // 0xffe10 = 1048080 because "e" doesn't mean exponent here...
    // 0xffe-10 = 4084 (ie. (0xffe) - (10))
    // 0xff.ffe2 = 255.99
    //
    #[test]
    fn lexer_rejects_invalid_numbers() {
        assert_eq!(
            Lexer::new("3.0.1").validate(),
            Some(LexerError {
                kind: LexerErrorKind::InvalidNumberLiteral,
                position: 3
            })
        );
        // assert_eq!(
        //     Lexer::new("3e-2.1").validate(),
        //     Some(LexerError {
        //         kind: LexerErrorKind::InvalidNumberLiteral,
        //         position: 3
        //     })
        // );
        // assert_eq!(
        //     Lexer::new("0xffx").validate(),
        //     Some(LexerError {
        //         kind: LexerErrorKind::InvalidNumberLiteral,
        //         position: 3
        //     })
        // );
        // assert_eq!(
        //     Lexer::new("0xff.0xff").validate(),
        //     Some(LexerError {
        //         kind: LexerErrorKind::InvalidNumberLiteral,
        //         position: 3
        //     })
        // );
        // assert_eq!(
        //     Lexer::new("0xff.0xff").validate(),
        //     Some(LexerError {
        //         kind: LexerErrorKind::InvalidNumberLiteral,
        //         position: 3
        //     })
        // );
    }

    #[test]
    fn lexer_lexes_strings() {
        assert_lexes!(
            "'hello'",
            vec![Token {
                kind: Literal(Str(SingleQuoted)),
                start: 0,
                end: 7,
            }]
        );
    }

    #[test]
    fn peekable_tracks_position() {
        let sample = "hello";
        let mut iter = Peekable::new(sample.chars());
        assert_eq!(iter.position, 0);
        assert_eq!(*iter.peek().unwrap(), 'h');
        assert_eq!(iter.position, 0);
        assert_eq!(iter.next(), Some('h'));
        assert_eq!(iter.position, 1);
        assert_eq!(iter.next(), Some('e'));
        assert_eq!(iter.position, 2);
        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.position, 3);
        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.position, 4);
        assert_eq!(iter.next(), Some('o'));
        assert_eq!(iter.position, 5);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position, 5);
    }
}
