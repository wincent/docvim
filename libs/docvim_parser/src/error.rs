use std::error::Error;
use std::fmt;

use docvim_lexer::error::{LexerError, LexerErrorKind};

#[derive(Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub position: usize,
}

impl fmt::Display for ParserError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.kind.to_str())
    }
}

impl Error for ParserError {}

impl From<LexerError> for ParserError {
    fn from(error: LexerError) -> Self {
        let kind = error.kind;
        let position = error.position;
        match kind {
            LexerErrorKind::InvalidEscapeSequence => {
                ParserError { kind: ParserErrorKind::InvalidEscapeSequence, position }
            }
            LexerErrorKind::InvalidNumberLiteral => {
                ParserError { kind: ParserErrorKind::InvalidNumberLiteral, position }
            }
            LexerErrorKind::InvalidOperator => {
                ParserError { kind: ParserErrorKind::InvalidOperator, position }
            }
            LexerErrorKind::UnterminatedBlockComment => {
                ParserError { kind: ParserErrorKind::UnterminatedBlockComment, position }
            }
            LexerErrorKind::UnterminatedEscapeSequence => {
                ParserError { kind: ParserErrorKind::UnterminatedEscapeSequence, position }
            }
            LexerErrorKind::UnterminatedStringLiteral => {
                ParserError { kind: ParserErrorKind::UnterminatedStringLiteral, position }
            }
        }
    }
}

#[derive(Debug)]
pub enum ParserErrorKind {
    // Own errors.
    UnexpectedComma,
    UnexpectedEndOfInput,
    UnexpectedFieldSeparator,
    UnexpectedToken, // TODO: this is a bit of a catch-all; replace with more specific things

    // Wrapped LexerErrors.
    InvalidEscapeSequence,
    InvalidNumberLiteral,
    InvalidOperator,
    UnterminatedBlockComment,
    UnterminatedEscapeSequence,
    UnterminatedStringLiteral,

    // Other wrapped errors.
    ParseIntError,
    Utf8Error,
}

impl ParserErrorKind {
    pub fn to_str(&self) -> &'static str {
        match *self {
            ParserErrorKind::ParseIntError => "parse int error",
            ParserErrorKind::UnexpectedComma => "unexpected comma",
            ParserErrorKind::UnexpectedEndOfInput => "unexpected end-of-input",
            ParserErrorKind::UnexpectedFieldSeparator => "unexpected field separator",
            ParserErrorKind::UnexpectedToken => "unexpected token",
            ParserErrorKind::Utf8Error => "UTF-8 error",

            // Mirrored from LexerError.
            ParserErrorKind::InvalidEscapeSequence => "invalid escape sequence",
            ParserErrorKind::InvalidNumberLiteral => "invalid number literal",
            ParserErrorKind::InvalidOperator => "invalid operator",
            ParserErrorKind::UnterminatedBlockComment => "unterminated block comment",
            ParserErrorKind::UnterminatedEscapeSequence => "unterminated escape sequence",
            ParserErrorKind::UnterminatedStringLiteral => "unterminated string literal",
        }
    }
}
