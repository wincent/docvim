use std::fs;

use self::LiteralKind::*;
use self::TokenKind::*;

// Error message strings.
const EXPECTED_COMMENT: &str= "Expected Comment token";

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

    // TODO: see if a type alias can help us avoid explicit lifetime here.
    // TODO: make a struct with position info in it and use that for richer errors
    fn expect<'b>(&mut self, ch: char, err: &'b str) -> Result<char, &'b str> {
        let next = self.iter.next().ok_or(err)?;
        if next == ch {
            Ok(ch)
        } else {
            Err(err)
        }
    }

    fn scan_comment(&mut self) -> Result<Token, &str> {
        let ch = self.iter.next().ok_or(EXPECTED_COMMENT)?;
        if ch != '-' {
            return Err(EXPECTED_COMMENT);
        }
        match self.iter.next() {
            Some(ch) => {
                Ok(Token::new(Comment))
            }
            None => {
                Err(EXPECTED_COMMENT)
            }
        }
    }

    fn next_token(&mut self) -> Result<Token, &str> {
        self.skip_whitespace();
        match self.iter.peek() {
            Some('-') => {
                self.advance();
                Ok(Token::new(Comment))
            },
            Some(_c) => {
                self.advance();
                Ok(Token::new(Unknown))
            }
            None => Err("end of input") // TODO: use self.position here
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
