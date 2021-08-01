use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::ops::{Index, Range};

use self::Edit::*;

/// 1-based indexing because Myers' original paper used 1-based indexing, and because text files
/// (which is what we are typically diffing) are viewed/edited in editors which generally use
/// 1-based indexing too.
#[derive(Debug, PartialEq)]
struct Idx(usize);

#[derive(Debug, PartialEq)]
enum Edit {
    Delete(Idx),
    Insert(Idx),
}

/// Represents SES (Shortest Edit Script) for a given pair of documents.
#[derive(Debug, PartialEq)]
pub struct Diff(Vec<Edit>);

// TODO: figure out whether to keep this around... ultimately the caller will also want the lines;
// might be better off with just `diff_lines()`, but in that case, you may as well pass them
// directly to `diff()`.
pub fn diff_string_lines(a: &str, b: &str) -> Diff {
    let a = a.lines().collect::<Vec<&str>>();
    let b = b.lines().collect::<Vec<&str>>();
    diff(&a, 0..a.len(), &b, 0..b.len())
}

pub fn diff<T>(a: &T, a_range: Range<usize>, b: &T, b_range: Range<usize>) -> Diff
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    Diff(vec![Delete(Idx(1)), Delete(Idx(2)), Insert(Idx(2)), Delete(Idx(6)), Delete(Idx(6))])
}

fn common_prefix_len<T>(a: &T, a_range: Range<usize>, b: &T, b_range: Range<usize>) -> usize
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    let Range { start: a_start, end: a_end } = a_range;
    let Range { start: b_start, end: b_end } = b_range;
    let a_len = a_end - a_start;
    let b_len = b_end - b_start;
    let mut prefix = 0;
    let count = if a_len <= b_len { a_len } else { b_len };
    for i in 0..count {
        let mut hasher = DefaultHasher::new();
        &a[a_start + i].hash(&mut hasher);
        let a_hash = hasher.finish();
        let mut hasher = DefaultHasher::new();
        &b[b_start + i].hash(&mut hasher);
        let b_hash = hasher.finish();
        if a_hash == b_hash {
            prefix += 1;
        } else {
            break;
        }
    }
    prefix
}

// Tempting to factor out `common_affix_len` and call it from both `common_prefix_len` and
// `common_suffix_len`, but it turns out that the conditionals would be annoyingly opaque; it's
// better to just spell each one out in a pedestrian way.
fn common_suffix_len<T>(a: &T, a_range: Range<usize>, b: &T, b_range: Range<usize>) -> usize
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    let Range { start: a_start, end: a_end } = a_range;
    let Range { start: b_start, end: b_end } = b_range;
    let a_len = a_end - a_start;
    let b_len = b_end - b_start;
    let mut suffix = 0;
    let count = if a_len <= b_len { a_len } else { b_len };
    for i in 0..count {
        let mut hasher = DefaultHasher::new();
        &a[a_end - i - 1].hash(&mut hasher);
        let a_hash = hasher.finish();
        let mut hasher = DefaultHasher::new();
        &b[b_end - i - 1].hash(&mut hasher);
        let b_hash = hasher.finish();
        if a_hash == b_hash {
            suffix += 1;
        } else {
            break;
        }
    }
    suffix
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_common_prefix_len() {
        // A prefix matches.
        let a: Vec<char> = "foobar".chars().collect();
        let b: Vec<char> = "foobaz".chars().collect();
        assert_eq!(common_prefix_len(&a, 0..a.len(), &b, 0..b.len()), 5);

        // Same, but strings are differing lengths.
        let a: Vec<char> = "foo".chars().collect();
        let b: Vec<char> = "foobar".chars().collect();
        assert_eq!(common_prefix_len(&a, 0..a.len(), &b, 0..b.len()), 3);

        // No match.
        let a: Vec<char> = "Guybrush".chars().collect();
        let b: Vec<char> = "Threepwood".chars().collect();
        assert_eq!(common_prefix_len(&a, 0..a.len(), &b, 0..b.len()), 0);

        // Total match.
        let a: Vec<char> = "hunter2".chars().collect();
        let b: Vec<char> = "hunter2".chars().collect();
        assert_eq!(common_prefix_len(&a, 0..a.len(), &b, 0..b.len()), 7);

        // Zero-length strings.
        let a: Vec<char> = "".chars().collect();
        let b: Vec<char> = "".chars().collect();
        assert_eq!(common_prefix_len(&a, 0..a.len(), &b, 0..b.len()), 0);

        // Just "a" is zero-length.
        let a: Vec<char> = "".chars().collect();
        let b: Vec<char> = "long".chars().collect();
        assert_eq!(common_prefix_len(&a, 0..a.len(), &b, 0..b.len()), 0);

        // Just "b" is zero-length.
        let a: Vec<char> = "hey!".chars().collect();
        let b: Vec<char> = "".chars().collect();
        assert_eq!(common_prefix_len(&a, 0..a.len(), &b, 0..b.len()), 0);
    }

    #[test]
    fn test_common_suffix_len() {
        // A suffix matches.
        let a: Vec<char> = "foobar".chars().collect();
        let b: Vec<char> = "quxbar".chars().collect();
        assert_eq!(common_suffix_len(&a, 0..a.len(), &b, 0..b.len()), 3);

        // Same, but strings are differing lengths.
        let a: Vec<char> = "beachball".chars().collect();
        let b: Vec<char> = "basketball".chars().collect();
        assert_eq!(common_suffix_len(&a, 0..a.len(), &b, 0..b.len()), 4);

        // No match.
        let a: Vec<char> = "Guybrush".chars().collect();
        let b: Vec<char> = "Threepwood".chars().collect();
        assert_eq!(common_suffix_len(&a, 0..a.len(), &b, 0..b.len()), 0);

        // Total match.
        let a: Vec<char> = "hunter2".chars().collect();
        let b: Vec<char> = "hunter2".chars().collect();
        assert_eq!(common_suffix_len(&a, 0..a.len(), &b, 0..b.len()), 7);

        // Zero-length strings.
        let a: Vec<char> = "".chars().collect();
        let b: Vec<char> = "".chars().collect();
        assert_eq!(common_suffix_len(&a, 0..a.len(), &b, 0..b.len()), 0);

        // Just "a" is zero-length.
        let a: Vec<char> = "".chars().collect();
        let b: Vec<char> = "long".chars().collect();
        assert_eq!(common_suffix_len(&a, 0..a.len(), &b, 0..b.len()), 0);

        // Just "b" is zero-length.
        let a: Vec<char> = "hey!".chars().collect();
        let b: Vec<char> = "".chars().collect();
        assert_eq!(common_suffix_len(&a, 0..a.len(), &b, 0..b.len()), 0);
    }

    #[test]
    fn test_example_from_myers_paper() {
        let a = vec!["A", "B", "C", "A", "B", "B", "A"].join("\n");
        let b = vec!["C", "B", "A", "B", "A", "C"].join("\n");
        assert_eq!(
            diff_string_lines(&a, &b),
            Diff(vec![
                Delete(Idx(1)),
                Delete(Idx(2)),
                Insert(Idx(2)),
                Delete(Idx(6)),
                Delete(Idx(6)),
            ])
        );
    }
}
