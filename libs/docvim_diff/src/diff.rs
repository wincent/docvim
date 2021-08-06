/// 1-based indexing because Myers' original paper used 1-based indexing, and because text files
/// (which is what we are typically diffing) are viewed/edited in editors which generally use
/// 1-based indexing too.
#[derive(Debug, PartialEq)]
pub struct Idx(pub usize);

#[derive(Debug, PartialEq)]
pub enum Edit {
    Delete(Idx),
    Insert(Idx),
}

/// Represents SES (Shortest Edit Script) for a given pair of documents.
#[derive(Debug, PartialEq)]
pub struct Diff(pub Vec<Edit>);
