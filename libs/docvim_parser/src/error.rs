use std::error::Error;
use std::fmt;
use std::num::ParseIntError;
use std::str::Utf8Error;

use docvim_lexer::error::LexerError;

#[derive(Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub position: usize,
}

impl fmt::Display for ParserError {
    // TODO include position info as well.
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.kind.to_str())
    }
}

impl Error for ParserError {}

impl From<LexerError> for ParserError {
    fn from(error: LexerError) -> Self {
        ParserError { kind: ParserErrorKind::LexerError, position: error.position }
    }
}

impl From<ParseIntError> for ParserError {
    // TODO: probably to_string() this error and jam it in the struct somewhere...
    fn from(_error: ParseIntError) -> Self {
        ParserError {
            kind: ParserErrorKind::Utf8Error,
            // position: error.position, // what here?
            position: 0,
        }
    }
}

impl From<Utf8Error> for ParserError {
    // TODO: probably to_string() this error and jam it in the struct somewhere...
    fn from(_error: Utf8Error) -> Self {
        ParserError {
            kind: ParserErrorKind::Utf8Error,
            // position: error.position, // what here?
            position: 0,
        }
    }
}

#[derive(Debug)]
pub enum ParserErrorKind {
    // Own errors.
    InvalidEscapeSequence,
    UnexpectedComma,
    UnexpectedEndOfInput,
    UnexpectedFieldSeparator,
    UnexpectedToken, // TODO: this is a bit of a catch-all; replace with more specific things

    // Wrapped errors.
    LexerError,
    ParseIntError,
    Utf8Error,
}

impl ParserErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            ParserErrorKind::InvalidEscapeSequence => "invalid escape sequence",
            ParserErrorKind::LexerError => "lexer error",
            ParserErrorKind::ParseIntError => "parse int error",
            ParserErrorKind::UnexpectedComma => "unexpected comma",
            ParserErrorKind::UnexpectedEndOfInput => "unexpected end-of-input",
            ParserErrorKind::UnexpectedFieldSeparator => "unexpected field separator",
            ParserErrorKind::UnexpectedToken => "unexpected token",
            ParserErrorKind::Utf8Error => "UTF-8 error",
        }
    }
}
