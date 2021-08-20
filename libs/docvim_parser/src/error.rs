use std::error::Error;
use std::fmt;

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

#[derive(Debug)]
pub enum ParserErrorKind {
    ExpectedEq,
    ExpectedRbracket,
    ExpectedRcurly,
    InvalidEscapeSequence,
    LocalDeclarationWithoutName,
    UnexpectedComma,
    UnexpectedEndOfInput,
    UnexpectedFieldSeparator,
    UnexpectedToken, // TODO: this is a bit of a catch-all; replace with more specific things
}

impl ParserErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            ParserErrorKind::ExpectedEq => "expected equals (\"=\")",
            ParserErrorKind::ExpectedRbracket => "expected right-bracket (\"]\")",
            ParserErrorKind::ExpectedRcurly => "expected right-curly (\"}\")",
            ParserErrorKind::InvalidEscapeSequence => "invalid escape sequence",
            ParserErrorKind::LocalDeclarationWithoutName => "local declaration without name",
            ParserErrorKind::UnexpectedComma => "unexpected comma",
            ParserErrorKind::UnexpectedEndOfInput => "unexpected end-of-input",
            ParserErrorKind::UnexpectedFieldSeparator => "unexpected field separator",
            ParserErrorKind::UnexpectedToken => "unexpected token",
        }
    }
}
