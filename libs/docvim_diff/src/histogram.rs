use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Index, Range};

use crate::diff::*;
use crate::myers;

// TODO: move this into tests if it is only used in tests.
// use Edit::*;

const MAX_CHAIN_LENGTH: usize = 64;

pub fn diff<T>(a: &Vec<T>, a_range: Range<usize>, b: &Vec<T>, b_range: Range<usize>) -> Diff
where
    T: Hash + PartialEq,
{
    // Create histogram of frequencies for each element in `a`.
    let mut a_freqs: HashMap<u64, usize> = HashMap::new();
    for idx in a_range.clone() {
        let key = hash(a, idx);
        let count = match a_freqs.get(&key) {
            Some(value) => value + 1,
            None => 1,
        };
        a_freqs.insert(key, count);
    }
    println!("hashmap:\n{:?}", a_freqs);

    // Compute frequencies for `b`.
    let mut b_freqs: HashMap<u64, usize> = HashMap::new();
    for idx in b_range.clone() {
        let key = hash(a, idx);
        let count = match b_freqs.get(&key) {
            Some(value) => value + 1,
            None => 1,
        };
        b_freqs.insert(key, count);
    }

    myers::diff(a, a_range, b, b_range)
}

#[cfg(test)]
mod tests {
    use super::*;

    use Edit::*;

    #[test]
    fn blinking_light() {
        let a = vec!["A", "B", "C", "A", "B", "B", "A"];
        let b = vec!["C", "B", "A", "B", "A", "C"];

        assert_eq!(
            diff(&a, 0..a.len(), &b, 0..b.len()),
            Diff(vec![
                // comment out one of these to break test so i can see my println, if desired
                Delete(Idx(1)),
                Delete(Idx(2)),
                Delete(Idx(4)),
                Insert(Idx(3)),
                Insert(Idx(6)),
            ])
        );
    }
}
