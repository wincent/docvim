use std::fmt;
use std::fs;

use self::CommentKind::*;
use self::LiteralKind::*;
use self::TokenKind::*;

#[derive(Debug)]
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

#[derive(Debug)]
enum CommentKind {
    BlockComment,
    LineComment,
}

#[derive(Debug)]
enum BinaryOpToken {
    And,     // and (logical and)
    Caret,   // ^ (exponentiate)
    Concat,  // .. (concatenate)
    Eq,      // == (equal)
    Gt,      // > (greater than)
    Gte,     // >= (greater than or equal)
    Lt,      // < (less than)
    Lte,     // <= (less than or equal)
    Minus,   // - (subtract): note can also be a unary operator (negation)
    Ne,      // == (not equal)
    Or,      // or (logical or)
    Percent, // % (modulo)
    Plus,    // + (add)
    Slash,   // / (divide)
    Star,    // * (multiply)
}

#[derive(Debug)]
enum UnaryOpToken {
    Hash,  // # (length)
    Minus, // - (negate)
}

enum LiteralKind {
    Str,
}

#[derive(Debug)]
enum TokenKind {
    BinaryOp(BinaryOpToken),
    UnaryOp(UnaryOpToken),
    Comment(CommentKind),
    Unknown,
}

// TODO: move all Lexer stuff into Lexer mod
#[derive(Copy, Clone)]
enum LexerErrorKind {
    ExpectedComment,
    UnterminatedBlockComment,

    EndOfInput, // Not a real error; just used to signify we got to the end.
}

impl LexerErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            LexerErrorKind::EndOfInput => "end of input",
            LexerErrorKind::ExpectedComment => "expected comment",
            LexerErrorKind::UnterminatedBlockComment => "unterminated block comment",
        }
    }
}

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

    fn expect_char(&mut self, ch: char, err: LexerErrorKind) -> Result<char, LexerError> {
        let next = self.iter.next().ok_or(err)?;
        if next == ch {
            Ok(ch)
        } else {
            Err(LexerError::from(err))
        }
    }

    fn scan_comment(&mut self) -> Result<Token, LexerError> {
        if self.consume_char('[') && self.consume_char('[') {
            self.scan_block_comment()
        } else {
            self.scan_line_comment()
        }
    }

    /// Scans until seeing "--]]".
    fn scan_block_comment(&mut self) -> Result<Token, LexerError> {
        let start = self.iter.position;
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
                        if dash_count >= 2 && self.consume_char('[') && self.consume_char('[') {
                            return Ok(Token::new(Comment(BlockComment), start, self.iter.position));
                        }
                },
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
        // BUG: position starts right after the "--", should probably start at the beginning.
        let start = self.iter.position;
        loop {
            match self.iter.next() {
                Some('\n') | None => {
                    return Ok(Token::new(Comment(LineComment), start, self.iter.position));
                },
                _ => (),
            }
        }
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();
        let start = self.iter.position;
        match self.iter.peek() {
            Some('-') => {
                self.iter.next();
                if self.consume_char('-') {
                    Ok(self.scan_comment()?)
                } else {
                    // TODO: operator cases (unary, binary)
                    Ok(Token::new(Unknown, start, self.iter.position))
                }
            }
            Some(_c) => {
                self.iter.next();
                Ok(Token::new(Unknown, start, self.iter.position))
            }
            None => Err(LexerError {
                kind: LexerErrorKind::EndOfInput,
                position: start,
            }),
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
