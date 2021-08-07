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
pub fn eq<T>(
    a: &Vec<T>,
    a_idx: usize,
    a_hashes: &Vec<u64>,
    b: &Vec<T>,
    b_idx: usize,
    b_hashes: &Vec<u64>,
) -> bool
where
    T: Hash + PartialEq,
{
    a_hashes[a_idx] == b_hashes[b_idx] && a[a_idx] == b[b_idx]
}

pub fn hash<T>(val: T) -> u64
where
    T: Hash + PartialEq,
{
    // See docs/hash.md for why we're using the "djb2a" hash.
    let mut hasher = Djb2aHasher::new();
    val.hash(&mut hasher);
    hasher.finish()
}

/// Hasher based on the latest version of Daniel J Bernstein's "djb2" hash. Note that the original
/// function was designed to produce 32-bit values, but here we extend that to 64-bits because
/// that's what the Rust Hasher trait requires. Strictly speaking, this makes it a different hash,
/// but for practical purposes, it appears to be a valid substitute.
///
/// In pseudo-code, the hash function is basically:
///
/// ```ignore
/// h[0] = 5381
/// h[1] = ((h[0] << 5) + h[0]) ^ byte
/// h[2] = ((h[1] << 5) + h[1]) ^ byte
/// etc
/// ```
///
/// We call this "djb2a" to distinguish from an earlier version of the function that used addition
/// instead of XOR.
///
/// In practice, tests show little difference between the two variants.
///
pub struct Djb2aHasher {
    state: u64,
}

impl Djb2aHasher {
    pub fn new() -> Self {
        Djb2aHasher { state: 5381 }
    }
}

impl Hasher for Djb2aHasher {
    fn finish(&self) -> u64 {
        self.state
    }

    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.state = (self.state << 5).wrapping_add(self.state) ^ (b as u64);
        }
    }
}

/// Hasher based on the original version of Daniel J Bernstein's "djb2" hash. Note that the
/// original function was designed to produce 32-bit values, but here we extend that to 64-bits
/// because that's what the Rust Hasher trait requires. Strictly speaking, this makes it a
/// different hash, but for practical purposes, it appears to be a valid substitute.
///
/// In pseudo-code, the hash function is basically:
///
/// ```ignore
/// h[0] = 5381
/// h[1] = ((h[0] << 5) + h[0]) + byte
/// h[2] = ((h[1] << 5) + h[1]) + byte
/// etc
/// ```
///
/// We call this "djb2" to distinguish from the later version of the function that uses XOR instead
/// of addition.
///
/// In practice, tests show little difference between the two variants.
///
pub struct Djb2Hasher {
    state: u64,
}

impl Djb2Hasher {
    pub fn new() -> Self {
        Djb2Hasher { state: 5381 }
    }
}

impl Hasher for Djb2Hasher {
    fn finish(&self) -> u64 {
        self.state
    }

    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.state = (self.state << 5).wrapping_add(self.state).wrapping_add(b as u64);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn djb2a(input: &[u8]) -> u64 {
        let mut hasher = Djb2aHasher::new();
        hasher.write(input);
        hasher.finish()
    }

    fn djb2(input: &[u8]) -> u64 {
        let mut hasher = Djb2Hasher::new();
        hasher.write(input);
        hasher.finish()
    }

    #[test]
    fn test_djb2ahasher() {
        // Short strings from the alphabet we're using in the diff tests.
        assert_eq!(djb2a(b"A"), 177636);
        assert_eq!(djb2a(b"B"), 177639);
        assert_eq!(djb2a(b"C"), 177638);

        // Common source code strings.
        assert_eq!(djb2a(b"end"), 193405706);
        assert_eq!(djb2a(b"endfunction"), 13753033479064092648);
        assert_eq!(djb2a(b"return"), 6951380998799);
        assert_eq!(djb2a(b"}"), 177624);
        assert_eq!(djb2a(b"  }"), 193341144);

        // Typical source code.
        assert_eq!(djb2a(b"local adder = function (a, b) return a + 1 end"), 7838213174359335825);
        assert_eq!(djb2a(b"use super::*;"), 5476728439767262262);
    }

    #[test]
    fn test_djb2hasher() {
        // Short strings from the alphabet we're using in the diff tests.
        assert_eq!(djb2(b"A"), 177638);
        assert_eq!(djb2(b"B"), 177639);
        assert_eq!(djb2(b"C"), 177640);

        // Common source code strings.
        assert_eq!(djb2(b"end"), 193490716);
        assert_eq!(djb2(b"endfunction"), 13872598002697550178);
        assert_eq!(djb2(b"return"), 6953974653989);
        assert_eq!(djb2(b"}"), 177698);
        assert_eq!(djb2(b"  }"), 193413026);

        // Typical source code.
        assert_eq!(djb2(b"local adder = function (a, b) return a + 1 end"), 12518546827141428551);
        assert_eq!(djb2(b"use super::*;"), 7866307544415970426);
    }
}
