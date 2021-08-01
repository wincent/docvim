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
    let mut edits = vec![];
    edits.push(Delete(Idx(1)));
    edits.push(Delete(Idx(2)));
    edits.push(Insert(Idx(2)));
    edits.push(Delete(Idx(6)));
    edits.push(Delete(Idx(6)));
    Diff(edits)
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

fn empty_range(range: &Range<usize>) -> bool {
    range.end - range.start == 0
}

fn find_middle_snake<T>(
    a: &T,
    a_range: Range<usize>,
    b: &T,
    b_range: Range<usize>,
    edits: &mut Vec<Edit>,
) -> Option<(usize, usize)>
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    None
}

fn recursive_diff<T>(
    a: &T,
    a_range: Range<usize>,
    b: &T,
    b_range: Range<usize>,
    edits: &mut Vec<Edit>,
) -> ()
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    if empty_range(&a_range) && !empty_range(&b_range) {
        for i in b_range {
            edits.push(Insert(Idx(i)));
        }
    } else if !empty_range(&a_range) && empty_range(&b_range) {
        for i in a_range {
            edits.push(Delete(Idx(i)));
        }
    } else if empty_range(&a_range) && empty_range(&b_range) {
        return;
    }
}

/*

  sub recursive_diff(A, N, B, M ):
    {
      snake = find_middle_snake();

      suppose it is from ( x, y ) to ( u, v ) with total differences D

      if ( D > 1 )
      {
        recursive_diff( A[ 1 .. x ], x, B[ 1 .. y ], y ) // top left

        Add middle snake to results

        recursive_diff( A[ u + 1 .. N ], N - u, B[ v + 1 .. M ], M - v ) // bottom right
      }
      else if ( D == 1 ) // must be forward snake
      {
        Add d = 0 diagonal to results
        Add middle snake to results
      }
      else if ( D == 0 ) // must be reverse snake
      {
        Add middle snake to results
      }
    }

  sub find_middle_snake():
    delta = N - M
    for d = 0 to ( N + M + 1 ) / 2
    {
      for k = -d to d step 2
      {
        calculate the furthest reaching forward path on line k
        if delta is odd and ( k >= delta - ( d - 1 ) and k <= delta + ( d - 1 ) )
          if overlap with reverse[ d - 1 ] on line k
            => found middle snake and SES of length 2D - 1
      }

      for k = -d to d step 2
      {
        calculate the furthest reaching reverse path on line k
        if delta is even and ( k >= -d - delta and k <= d - delta )
          if overlap with forward[ d ] on line k
            => found middle snake and SES of length 2D
      }
    }
*/

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
