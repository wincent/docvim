use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::ops::Index;

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

pub fn eq<T>(a: &T, a_idx: usize, b: &T, b_idx: usize) -> bool
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    let mut hasher = DefaultHasher::new();
    a[a_idx].hash(&mut hasher);
    let a_hash = hasher.finish();
    let mut hasher = DefaultHasher::new();
    b[b_idx].hash(&mut hasher);
    let b_hash = hasher.finish();
    a_hash == b_hash
}
