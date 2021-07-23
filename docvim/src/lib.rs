use std::fmt;
use std::fs;

use self::LiteralKind::*;
use self::TokenKind::*;

#[derive(Debug)]
struct Token {
    pub kind: TokenKind,
    // TODO: tokens should have contents too (some of them, at least, like literals and
    // comments); could also encode range info in them and then extract content later (for the ones
    // where it matters) -- not sure which is better...
    pos: usize,
    len: usize,
}

impl Token {
    fn new(kind: TokenKind, pos: usize, len: usize) -> Token {
        Token { kind, pos, len }
    }
}

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
    Comment,
    Unknown,
}

// TODO: move all Lexer stuff into Lexer mod
#[derive(Copy, Clone)]
enum LexerErrorKind {
    ExpectedComment,

    EndOfInput, // Not a real error; just used to signify we got to the end.
}

impl LexerErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            LexerErrorKind::EndOfInput => "end of input",
            LexerErrorKind::ExpectedComment => "expected comment",
        }
    }
}

struct LexerError {
    kind: LexerErrorKind,
    position: usize
}

impl LexerError {
    pub fn new(kind: LexerErrorKind, position: usize) -> LexerError {
        Self {
            kind,
            position,
        }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} ({})", self.kind.to_str(), self.position)
    }
}

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
            _ => { self.position += 1; },
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

    fn expect(&mut self, ch: char, err: LexerErrorKind) -> Result<char, LexerError> {
        let next = self.iter.next().ok_or(err)?;
        if next == ch {
            Ok(ch)
        } else {
            Err(LexerError::from(err))
        }
    }

    fn scan_comment(&mut self) -> Result<Token, LexerError> {
        self.expect('-', LexerErrorKind::ExpectedComment)?;
        self.expect('-', LexerErrorKind::ExpectedComment)?;
        let position = self.iter.position;
        match self.iter.peek() {
            Some('[') => {
                // might be a multiline comment start...
                // want multiple lookahead for this...
                // could clone iterator.. (if i can implement clone trait on it and all its
                // fields)
                // or could pull in itertools dep
                // or could extend peekable with some more methods for peek_nth() etc
                // TODO: deal with this later
                Ok(Token::new(Comment, position, 1))
            },
            Some('\n') => {
                // end of line
                Ok(Token::new(Comment, position, 1))
            },
            Some(_) => {
                // everything else...
                // want "consume until" closure...
                // let length: usize = 0;
                // loop {
                //     let c = self.iter.peek();
                //     match 
                // }
                Ok(Token::new(Comment, position, 0))
            },
            None => {
                // end of input
                Ok(Token::new(Comment, position, 0))
            },
        }
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();
        let position = self.iter.position;
        match self.iter.peek() {
            Some('-') => {
                Ok(self.scan_comment()?)
            },
            Some(_c) => {
                self.iter.next();
                Ok(Token::new(Unknown, position, 1))
            }
            None => Err(LexerError { kind: LexerErrorKind::EndOfInput, position })
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.iter.peek() {
            match c {
                ' ' | '\n' | '\r' | '\t' => { self.iter.next(); }
                _ => { break; }
            }
        }
    }
}

pub fn run(args: Vec<String>) {
    // TODO: actual arg parsing
    let input = "sample/init.lua";

    let contents = fs::read_to_string(input)
        .expect("unable to read file");

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
