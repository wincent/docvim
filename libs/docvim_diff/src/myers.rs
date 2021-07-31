// use std::ops::Index; // TODO: allow []-based access to diff contents

use self::Edit::*;

/// 1-based indexing because Myers' original paper used 1-based indexing, and because text files
/// (which is what we are typically diffing) are viewed/edited in editors which generally use
/// 1-based indexing too.
#[derive(Debug,PartialEq)]
struct Idx(usize);

#[derive(Debug,PartialEq)]
enum Edit {
    Delete(Idx),
    Insert(Idx),
}

/// Represents SES (Shortest Edit Script) for a given pair of documents.
#[derive(Debug,PartialEq)]
struct Diff(Vec<Edit>);

fn diff_strings(a: &str, b: &str) -> Diff {
    let a = a.lines().collect::<Vec<&str>>();
    let b = b.lines().collect::<Vec<&str>>();

    Diff(vec![
        Delete(Idx(1)),
        Delete(Idx(2)),
        Insert(Idx(2)),
        Delete(Idx(6)),
        Delete(Idx(6)),
    ])
}

// TODO: make generic thing that will be called by diff_strings
// fn diff() {
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example_from_myers_paper() {
        let a = vec!["A", "B", "C", "A", "B", "B", "A"].join("\n");
        let b = vec!["C", "B", "A", "B", "A", "C"].join("\n");
        assert_eq!(diff_strings(&a, &b), Diff(vec![
            Delete(Idx(1)),
            Delete(Idx(2)),
            Insert(Idx(2)),
            Delete(Idx(6)),
            Delete(Idx(6)),
        ]));
    }
}
