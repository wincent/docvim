use crate::error::*;
use crate::make_token;
use crate::peekable::*;
use crate::token::*;

use self::HeadingKind::*;
use self::MarkdownToken::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MarkdownToken {
    Heading(HeadingKind),
    Unknown,
}

impl TokenKind for MarkdownToken {}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HeadingKind {
    Heading1,
    Heading2,
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

    fn scan_subheading(&self) -> Option<Result<Token<MarkdownToken>, LexerError>> {
        make_token!(self, Heading(Heading2))
    }

    fn scan_heading(&self) -> Option<Result<Token<MarkdownToken>, LexerError>> {
        make_token!(self, Heading(Heading1))
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Result<Token<MarkdownToken>, LexerError>;

    fn next(&mut self) -> Option<Result<Token<MarkdownToken>, LexerError>> {
        self.skip_whitespace();
        self.char_start = self.iter.char_idx;
        self.byte_start = self.iter.byte_idx;
        self.column_start = self.iter.column_idx;
        self.line_start = self.iter.line_idx;
        if let Some(&c) = self.iter.peek() {
            match c {
                '#' => {
                    self.iter.next();
                    if self.consume_char('#') {
                        self.scan_subheading()
                    } else {
                        self.scan_heading()
                    }
                }
                _ => {
                    // TODO: Be less lenient in Markdown parser than in Lua parser (want to alert
                    // users of mistakes in the way they authored their Markdown, but not bother
                    // users if we can't fully grok their Lua code).
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

    macro_rules! assert_lexes {
        ($input:expr, $expected:expr) => {
            assert_eq!(
                Lexer::new(&$input)
                    .tokens
                    .map(|token| token.unwrap())
                    .collect::<Vec<Token<MarkdownToken>>>(),
                $expected
            )
        };
    }

    #[test]
    fn lexes_something() {
        assert_lexes!(
            "##",
            vec![Token {
                byte_end: 2,
                byte_start: 0,
                char_end: 2,
                char_start: 0,
                column_start: 1,
                column_end: 3,
                line_start: 1,
                line_end: 1,
                kind: Heading(Heading2),
            }]
        );
    }
}
