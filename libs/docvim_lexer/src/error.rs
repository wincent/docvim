use std::error;
use std::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LexerErrorKind {
    InvalidEscapeSequence,
    InvalidOperator,
    InvalidNumberLiteral,
    UnterminatedBlockComment,
    UnterminatedEscapeSequence,
    UnterminatedStringLiteral,
}

impl LexerErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            LexerErrorKind::InvalidEscapeSequence => "invalid escape sequence",
            LexerErrorKind::InvalidNumberLiteral => "invalid number literal",
            LexerErrorKind::InvalidOperator => "invalid operator",
            LexerErrorKind::UnterminatedBlockComment => "unterminated block comment",
            LexerErrorKind::UnterminatedEscapeSequence => "unterminated escape sequence",
            LexerErrorKind::UnterminatedStringLiteral => "unterminated string literal",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub position: usize,
}

impl LexerError {
    pub fn new(kind: LexerErrorKind, char_idx: usize) -> LexerError {
        Self { kind, position: char_idx }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} ({})", self.kind.to_str(), self.position)
    }
}

impl error::Error for LexerError {}
