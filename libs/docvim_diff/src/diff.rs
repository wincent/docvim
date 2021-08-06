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

// TODO: if it ends up turning out that only myers.rs needs this, move it back there.
// could also just use `hash()` (see further down).
pub fn eq<T>(a: &T, a_idx: usize, b: &T, b_idx: usize) -> bool
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    hash(a, a_idx) == hash(b, b_idx)
}

pub fn hash<T>(val: &T, idx: usize) -> u64
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    let mut hasher = DefaultHasher::new();
    val[idx].hash(&mut hasher);
    hasher.finish()
}
