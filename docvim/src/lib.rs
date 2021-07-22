use std::fmt;
use std::fs;

use self::LiteralKind::*;
use self::TokenKind::*;

#[derive(Debug)]
struct Token {
    pub kind: TokenKind,
}

impl Token {
    fn new(kind: TokenKind) -> Token {
        Token { kind }
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

struct Lexer<'a> {
    iter: std::iter::Peekable<std::str::Chars<'a>>,
    position: usize
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            iter: input.chars().peekable(),
            position: 0,
        }
    }

    fn advance(&mut self) {
        self.position = self.position + 1;
        self.iter.next();
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
        Ok(Token::new(Comment))
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        let position = self.position;
        self.skip_whitespace();
        match self.iter.peek() {
            Some('-') => {
                Ok(self.scan_comment()?)
            },
            Some(_c) => {
                self.advance();
                Ok(Token::new(Unknown))
            }
            None => Err(LexerError { kind: LexerErrorKind::EndOfInput, position })
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.iter.peek() {
            match c {
                ' ' | '\n' | '\r' | '\t' => self.advance(),
                _ => break
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

    let mut iter = contents.chars().peekable();
    while let Some(&c) = iter.peek() {
        let token = match c {
            '-' => {
                Token::new(Comment)
            }
            '0'..='9' => {
                // println!("Got digit");
                Token::new(Unknown)
            }
            _ => {
                // println!("Something else {}", c);
                Token::new(Unknown)
            }
        };
        println!("Token {:?}", token);
        iter.next();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blinking_light() {
        assert_eq!(1, 1)
    }
}
