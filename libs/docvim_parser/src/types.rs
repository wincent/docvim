#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Location {
    pub line_start: usize,
    pub line_end: usize,
    pub column_start: usize,
    pub column_end: usize,
}
