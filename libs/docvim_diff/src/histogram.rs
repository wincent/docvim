use std::hash::Hash;
use std::ops::{Index, Range};

use crate::diff::*;
use crate::myers;

use Edit::*;

pub fn diff<T>(a: &T, a_range: Range<usize>, b: &T, b_range: Range<usize>) -> Diff
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    // Create histogram of frequences for each element in `a`.
    myers::diff(a, a_range, b, b_range)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blinking_light() {
        let a = vec!["A", "B", "C", "A", "B", "B", "A"];
        let b = vec!["C", "B", "A", "B", "A", "C"];

        assert_eq!(
            diff(&a, 0..a.len(), &b, 0..b.len()),
            Diff(vec![
                Delete(Idx(1)),
                Delete(Idx(2)),
                Delete(Idx(4)),
                Insert(Idx(3)),
                Insert(Idx(6)),
            ])
        );
    }
}
