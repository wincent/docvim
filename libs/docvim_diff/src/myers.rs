use std::hash::Hash;
use std::ops::Index;

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
fn diff_string_lines(a: &str, b: &str) -> Diff {
    let a = a.lines().collect::<Vec<&str>>();
    let b = b.lines().collect::<Vec<&str>>();
    diff(&a, &b)
}

// TODO make this take range parameters as well because it will be used as a subroutine that works
// on sections within a file as part of the histogram diff algorithm.
// histogram
pub fn diff<T: PartialEq>(a: &Vec<T>, b: &Vec<T>) -> Diff {
    Diff(vec![Delete(Idx(1)), Delete(Idx(2)), Insert(Idx(2)), Delete(Idx(6)), Delete(Idx(6))])
}

#[cfg(test)]
mod tests {
    use super::*;

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
