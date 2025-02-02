use std::fmt::Debug;

pub trait TokenKind: Clone + Copy + Debug + Eq + PartialEq {}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Token<T: TokenKind> {
    pub kind: T,
    pub char_start: usize,
    pub char_end: usize,
    pub byte_start: usize,
    pub byte_end: usize,

    // 1-indexed, as these are all editor-centric fields.
    pub column_start: usize,
    pub column_end: usize,
    pub line_start: usize,
    pub line_end: usize,
}

impl<T: TokenKind> Token<T> {
    pub fn new(
        kind: T,
        char_start: usize,
        char_end: usize,
        byte_start: usize,
        byte_end: usize,
        column_start: usize,
        column_end: usize,
        line_start: usize,
        line_end: usize,
    ) -> Token<T> {
        Token {
            kind,
            char_start,
            char_end,
            byte_start,
            byte_end,
            column_start,
            column_end,
            line_start,
            line_end,
        }
    }
}

#[macro_export]
macro_rules! make_token {
    ($self:expr, $kind:expr) => {
        Some(Ok(Token::new(
            $kind,
            $self.char_start,
            $self.iter.char_idx,
            $self.byte_start,
            $self.iter.byte_idx,
            $self.column_start,
            $self.iter.column_idx,
            $self.line_start,
            $self.iter.line_idx,
        )))
    };
}
