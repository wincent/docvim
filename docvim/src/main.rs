use std::env;
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

struct Lexer<'a> {
    iter: std::iter::Peekable<std::str::Chars<'a>>,
    position: usize
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            iter: input.chars().peekable(),
            position: 0,
        }
    }

    pub fn advance(&mut self) {
        self.position = self.position + 1;
        self.iter.next();
    }

    pub fn next_token(&mut self) -> Result<Token, &str> {
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

    pub fn skip_whitespace(&mut self) {
        while let Some(&c) = self.iter.peek() {
            match c {
                ' ' | '\n' | '\r' | '\t' => self.advance(),
                _ => break
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    let contents = fs::read_to_string("sample/init.lua")
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
