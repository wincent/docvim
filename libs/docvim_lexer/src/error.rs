use std::error;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

pub trait LexerErrorKind: Clone + Copy + Debug + Display + Eq + PartialEq {}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LexerError<T: LexerErrorKind> {
    pub kind: T,
    pub line: usize,
    pub column: usize,
}

impl<T: LexerErrorKind> LexerError<T> {
    pub fn new(kind: T, line: usize, column: usize) -> Self {
        Self { kind, line, column }
    }
}

impl<T: LexerErrorKind> fmt::Display for LexerError<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} (line {}, column {})", self.kind, self.line, self.column)
    }
}

impl<T: LexerErrorKind> error::Error for LexerError<T> {}
