use std::fmt;
use std::fmt::Display;

use crate::error::*;
use crate::make_token;
use crate::peekable::*;
use crate::token::*;

use self::HeadingKind::*;
use self::MarkdownToken::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum MarkdownLexerError {
    InvalidHeading,
    UnterminatedCodeFence,
    UnterminatedHorizontalRule,
}

impl Display for MarkdownLexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let description = match *self {
            MarkdownLexerError::InvalidHeading => "invalid heading",
            MarkdownLexerError::UnterminatedCodeFence => "unterminated code fence",
            MarkdownLexerError::UnterminatedHorizontalRule => "unterminated horizontal rule",
        };
        write!(f, "{}", description)
    }
}

impl LexerErrorKind for MarkdownLexerError {}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MarkdownToken {
    // Standard Markdown.
    Backtick,
    Bang,
    Break,
    BlockQuote,
    CodeFence,
    Heading(HeadingKind),
    HorizontalRule,
    Hyphen,
    Lbracket,
    Lparen,
    Newline,
    Rbracket,
    Rparen,
    Space,
    Text,

    // Docvim-flavored Markdown.
    At,
    Bar,
    Star,
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

    /// Consumes the lexer's input and returns `Some(LexerError<MarkdownLexerError>)` on encountering an error, or
    /// `None` if the input is valid.
    #[cfg(test)]
    fn validate(&mut self) -> Option<LexerError<MarkdownLexerError>> {
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
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Result<Token<MarkdownToken>, LexerError<MarkdownLexerError>>;

    fn next(&mut self) -> Option<Result<Token<MarkdownToken>, LexerError<MarkdownLexerError>>> {
        self.char_start = self.iter.char_idx;
        self.byte_start = self.iter.byte_idx;
        self.column_start = self.iter.column_idx;
        self.line_start = self.iter.line_idx;
        loop {
            if let Some(&c) = self.iter.peek() {
                let pending = || {
                    self.iter.line_idx > self.line_start || self.iter.column_idx > self.column_start
                };
                match c {
                    '@' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        break make_token!(self, At);
                    }
                    '|' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        break make_token!(self, Bar);
                    }
                    '*' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        break make_token!(self, Star);
                    }
                    ' ' | '\t' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        while let Some(&next) = self.iter.peek() {
                            if !matches!(next, ' ' | '\t') {
                                break;
                            }
                            self.iter.next();
                        }
                        break make_token!(self, Space);
                    }
                    '\r' => {
                        if pending() {
                            break make_token!(self, Text);
                        }

                        // Both "\r\n" and "\r" produce a `Newline` token.
                        self.iter.next();
                        self.consume_char('\n');
                        break make_token!(self, Newline);
                    }
                    '\n' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        break make_token!(self, Newline);
                    }
                    '!' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        break make_token!(self, Bang);
                    }
                    '<' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        if self.consume_char('b') && self.consume_char('r') {
                            // Consume optional whitespace.
                            while let Some(&next) = self.iter.peek() {
                                if next != ' ' && next != '\t' {
                                    break;
                                }
                                self.iter.next();
                            }
                            if self.consume_char('>') {
                                break make_token!(self, Break);
                            } else if self.consume_char('/') && self.consume_char('>') {
                                break make_token!(self, Break);
                            } else {
                                continue;
                            }
                        }
                        break make_token!(self, BlockQuote);
                    }
                    '>' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        break make_token!(self, BlockQuote);
                    }
                    '[' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        break make_token!(self, Lbracket);
                    }
                    ']' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        break make_token!(self, Rbracket);
                    }
                    '(' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        break make_token!(self, Lparen);
                    }
                    ')' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        break make_token!(self, Rparen);
                    }
                    '#' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        if self.consume_char('#') {
                            if self.consume_char('#') {
                                break Some(Err(LexerError {
                                    kind: MarkdownLexerError::InvalidHeading,
                                    line: self.iter.line_idx,
                                    column: self.iter.column_idx - 3,
                                }));
                            } else {
                                break make_token!(self, Heading(Heading2));
                            }
                        } else {
                            break make_token!(self, Heading(Heading1));
                        }
                    }
                    '`' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        if self.consume_char('`') {
                            if self.consume_char('`') {
                                break make_token!(self, CodeFence);
                            } else {
                                break Some(Err(LexerError {
                                    kind: MarkdownLexerError::UnterminatedCodeFence,
                                    line: self.iter.line_idx,
                                    column: self.iter.column_idx,
                                }));
                            }
                        } else {
                            break make_token!(self, Backtick);
                        }
                    }
                    // TODO: only treat these a special in column 0? same for headings
                    '-' => {
                        if pending() {
                            break make_token!(self, Text);
                        }
                        self.iter.next();
                        if self.consume_char('-') {
                            if self.consume_char('-') {
                                break make_token!(self, HorizontalRule);
                            } else {
                                break Some(Err(LexerError {
                                    kind: MarkdownLexerError::UnterminatedHorizontalRule,
                                    line: self.iter.line_idx,
                                    column: self.iter.column_idx,
                                }));
                            }
                        } else {
                            break make_token!(self, Hyphen);
                        }
                    }
                    _ => {
                        self.iter.next();
                        while let Some(&next) = self.iter.peek() {
                            if matches!(
                                next,
                                ' ' | '\t'
                                    | '\n'
                                    | '\r'
                                    | '@'
                                    | '|'
                                    | '*'
                                    | '!'
                                    | '<'
                                    | '>'
                                    | '['
                                    | ']'
                                    | '('
                                    | ')'
                                    | '#'
                                    | '`'
                                    | '-'
                            ) {
                                break;
                            }
                            self.iter.next();
                        }
                        break make_token!(self, Text);
                    }
                }
            } else {
                let pending = || {
                    self.iter.line_idx > self.line_start || self.iter.column_idx > self.column_start
                };
                if pending() {
                    break make_token!(self, Text);
                }
                break None;
            }
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
    fn lexes_a_heading() {
        // TODO: more
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

    #[test]
    fn lexes_a_horizontal_rule() {
        assert_lexes!(
            "---",
            vec![Token {
                byte_end: 3,
                byte_start: 0,
                char_end: 3,
                char_start: 0,
                column_start: 1,
                column_end: 4,
                line_start: 1,
                line_end: 1,
                kind: HorizontalRule,
            }]
        );
    }

    #[test]
    fn lexes_a_block_quote() {
        assert_lexes!(
            ">",
            vec![Token {
                byte_end: 1,
                byte_start: 0,
                char_end: 1,
                char_start: 0,
                column_start: 1,
                column_end: 2,
                line_start: 1,
                line_end: 1,
                kind: BlockQuote,
            }]
        );
    }

    #[test]
    fn lexes_a_fenced_code_marker() {
        assert_lexes!(
            "```",
            vec![Token {
                byte_end: 3,
                byte_start: 0,
                char_end: 3,
                char_start: 0,
                column_start: 1,
                column_end: 4,
                line_start: 1,
                line_end: 1,
                kind: CodeFence,
            }]
        );
    }

    #[test]
    fn rejects_bad_code_fence_markers() {
        assert_eq!(
            Lexer::new("foo``bar").validate(),
            Some(LexerError {
                kind: MarkdownLexerError::UnterminatedCodeFence,
                line: 1,
                column: 6
            })
        );
    }

    #[test]
    fn rejects_bad_horizontal_rules() {
        assert_eq!(
            Lexer::new("foo--bar").validate(),
            Some(LexerError {
                kind: MarkdownLexerError::UnterminatedHorizontalRule,
                line: 1,
                column: 6
            })
        );
    }

    #[test]
    fn rejects_bad_headings() {
        assert_eq!(
            Lexer::new("\n\n###").validate(),
            Some(LexerError { kind: MarkdownLexerError::InvalidHeading, line: 3, column: 1 })
        );
    }
}
