use std::error::Error;
use std::fmt;

use docvim_lexer::error::LexerError;
use docvim_lexer::lua::LuaLexerError;
use docvim_lexer::markdown::MarkdownLexerError;

#[derive(Debug)]
pub struct ParserError<T> {
    pub kind: T,
    pub line: usize,
    pub column: usize,
}

pub trait ErrorKind {
    fn to_str(&self) -> &'static str;
}

impl<T> fmt::Display for ParserError<T>
where
    T: ErrorKind,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.kind.to_str())
    }
}

impl<T> Error for ParserError<T> where
    T: fmt::Debug + fmt::Display + Send + Sync + 'static + ErrorKind
{
}

impl From<LexerError<LuaLexerError>> for ParserError<LuaParserErrorKind> {
    fn from(error: LexerError<LuaLexerError>) -> Self {
        let kind = error.kind;
        let line = error.line;
        let column = error.column;
        match kind {
            LuaLexerError::InvalidEscapeSequence => {
                ParserError { kind: LuaParserErrorKind::InvalidEscapeSequence, line, column }
            }
            LuaLexerError::InvalidNumberLiteral => {
                ParserError { kind: LuaParserErrorKind::InvalidNumberLiteral, line, column }
            }
            LuaLexerError::InvalidOperator => {
                ParserError { kind: LuaParserErrorKind::InvalidOperator, line, column }
            }
            LuaLexerError::UnterminatedBlockComment => {
                ParserError { kind: LuaParserErrorKind::UnterminatedBlockComment, line, column }
            }
            LuaLexerError::UnterminatedEscapeSequence => {
                ParserError { kind: LuaParserErrorKind::UnterminatedEscapeSequence, line, column }
            }
            LuaLexerError::UnterminatedStringLiteral => {
                ParserError { kind: LuaParserErrorKind::UnterminatedStringLiteral, line, column }
            }
        }
    }
}

impl From<LexerError<MarkdownLexerError>> for ParserError<MarkdownParserErrorKind> {
    fn from(error: LexerError<MarkdownLexerError>) -> Self {
        let kind = error.kind;
        let line = error.line;
        let column = error.column;
        match kind {
            MarkdownLexerError::InvalidHeading => {
                ParserError { kind: MarkdownParserErrorKind::UnexpectedToken, line, column }
            }
            MarkdownLexerError::UnterminatedCodeFence => {
                ParserError { kind: MarkdownParserErrorKind::UnexpectedToken, line, column }
            }
            MarkdownLexerError::UnterminatedHorizontalRule => {
                ParserError { kind: MarkdownParserErrorKind::UnexpectedToken, line, column }
            }
        }
    }
}

#[derive(Debug)]
pub enum LuaParserErrorKind {
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

#[derive(Debug)]
pub enum MarkdownParserErrorKind {
    UnexpectedToken,
}

impl ErrorKind for LuaParserErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            LuaParserErrorKind::ParseIntError => "parse int error",
            LuaParserErrorKind::UnexpectedComma => "unexpected comma",
            LuaParserErrorKind::UnexpectedEndOfInput => "unexpected end-of-input",
            LuaParserErrorKind::UnexpectedFieldSeparator => "unexpected field separator",
            LuaParserErrorKind::UnexpectedToken => "unexpected token",
            LuaParserErrorKind::Utf8Error => "UTF-8 error",

            // Mirrored from LexerError.
            LuaParserErrorKind::InvalidEscapeSequence => "invalid escape sequence",
            LuaParserErrorKind::InvalidNumberLiteral => "invalid number literal",
            LuaParserErrorKind::InvalidOperator => "invalid operator",
            LuaParserErrorKind::UnterminatedBlockComment => "unterminated block comment",
            LuaParserErrorKind::UnterminatedEscapeSequence => "unterminated escape sequence",
            LuaParserErrorKind::UnterminatedStringLiteral => "unterminated string literal",
        }
    }
}

impl std::fmt::Display for LuaParserErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl ErrorKind for MarkdownParserErrorKind {
    fn to_str(&self) -> &'static str {
        match *self {
            MarkdownParserErrorKind::UnexpectedToken => "unexpected token",
        }
    }
}

impl std::fmt::Display for MarkdownParserErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}
