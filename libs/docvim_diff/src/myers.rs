use std::collections::hash_map::DefaultHasher;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};
use std::ops::{Index, IndexMut, Range};

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

/// The Myers paper specifies an array (`V[-MAX..MAX]`) that allows negative indices, so we
/// substitute a ring buffer for that.
struct RingBuffer {
    capacity: usize,
    // TODO: try and make this generic, maybe, once:
    // https://github.com/rust-lang/rust/issues/52662
    storage: Vec<usize>,
}

impl RingBuffer {
    fn new(capacity: usize) -> Self {
        RingBuffer { capacity, storage: vec![0; capacity] }
    }
}

// Note to self: this is ridiculous; I should probably just keep two vectors.
impl Index<isize> for RingBuffer {
    type Output = usize;

    fn index(&self, index: isize) -> &Self::Output {
        if index == 0 {
            &self.storage[0]
        } else if index > 0 {
            &self.storage[(index as usize) % self.capacity]
        } else {
            let offset = (index * -1) as usize;
            if offset <= self.capacity {
                &self.storage[self.capacity - offset]
            } else {
                &self.storage[self.capacity - offset % self.capacity]
            }
        }
    }
}

impl IndexMut<isize> for RingBuffer {
    fn index_mut(&mut self, index: isize) -> &mut Self::Output {
        if index == 0 {
            &mut self.storage[0]
        } else if index > 0 {
            &mut self.storage[(index as usize) % self.capacity]
        } else {
            let offset = (index * -1) as usize;
            if offset <= self.capacity {
                &mut self.storage[self.capacity - offset]
            } else {
                &mut self.storage[self.capacity - offset % self.capacity]
            }
        }
    }
}

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
    let n = a_range.len();
    let m = b_range.len();
    let max = n + m;
    let mut v_top = RingBuffer::new(max * 2 + 1);
    let mut v_bottom = RingBuffer::new(max * 2 + 1);
    recursive_diff(a, a_range, b, b_range, &mut v_top, &mut v_bottom, &mut edits);
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

fn usize_to_isize(n: usize) -> isize {
    isize::try_from(n).expect("overflow converting from usize to isize")
}

fn find_middle_snake<T>(
    a: &T,
    a_range: Range<usize>,
    b: &T,
    b_range: Range<usize>,
    v_top: &mut RingBuffer,
    v_bottom: &mut RingBuffer,
    edits: &mut Vec<Edit>,
) -> (usize, usize)
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    v_top[1] = 0;
    v_bottom[1] = 0;

    let n = usize_to_isize(a_range.len());
    let m = usize_to_isize(b_range.len());
    let delta = n - m;
    let odd = delta % 2 == 1;

    let mut x = 0;

    for d in 0..((n + m + 1) / 2) {
        // Search forward from top-left.
        for k in (-d..=d).rev().step_by(2) {
            if k == -d || k != d && v_top[k - 1] < v_top[k + 1] {
                x = v_top[k + 1];
            } else {
                x = v_top[k - 1] + 1;
            }
            let y = ((x as isize) - k) as usize;
            let initial_x = x;
            let initial_y = y;
            if x < (n as usize) && y < (m as usize) {
                x += common_prefix_len(
                    a,
                    (a_range.start + x)..a_range.end,
                    b,
                    (b_range.start + y)..b_range.end,
                );
            }
            v_top[k] = x;
            if odd
                && (-(k - delta)) >= -(d - 1)
                && (-(k - delta)) <= (d - 1)
                && (v_top[k] as isize) + (v_bottom[-(k - delta)] as isize) >= n
            {
                return (a_range.start + initial_x, b_range.start + initial_y);
            }
        }

        // Search backward from bottom-right.
        for k in (-d..=d).rev().step_by(2) {
            if k == -d || k != d && v_bottom[k - 1] < v_bottom[k + 1] {
                x = v_bottom[k + 1];
            } else {
                x = v_bottom[k - 1] + 1;
            }
            let mut y = ((x as isize) - k) as usize;
            if x < (n as usize) && y < (m as usize) {
                let increment = common_suffix_len(
                    a,
                    a_range.start..(a_range.end - x),
                    b,
                    b_range.start..(b_range.end - y),
                );
                x += increment;
                y += increment;
            }
            v_bottom[k] = x;

            if !odd
                && (-(k - delta)) >= -d
                && (-(k - delta)) <= d
                && (v_bottom[k] as isize) + (v_top[(-(k - delta))] as isize) >= n
            {
                return ((n as usize) - x + a_range.start, (m as usize) - y + b_range.end);
            }
        }
    }
    panic!("did not find middle snake");
}

fn recursive_diff<T>(
    a: &T,
    a_range: Range<usize>,
    b: &T,
    b_range: Range<usize>,
    v_top: &mut RingBuffer,
    v_bottom: &mut RingBuffer,
    edits: &mut Vec<Edit>,
) -> ()
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    let n = a_range.len();
    let m = b_range.len();
    if n == 0 && m != 0 {
        for i in b_range.clone() {
            edits.push(Insert(Idx(i + 1)));
        }
    } else if n != 0 && m == 0 {
        for i in a_range.clone() {
            edits.push(Delete(Idx(i + 1)));
        }
    } else if n == 0 && m == 0 {
        return;
    } else {
        let snake = find_middle_snake(a, a_range.clone(), b, b_range.clone(), v_top, v_bottom, edits);
        let a_left = a_range.start..snake.0;
        let a_right = (a_range.start + snake.0)..a_range.end;
        let b_left = b_range.start..snake.1;
        let b_right = (b_range.start + snake.1)..b_range.end;
        // if a_left.len() > 0 && b_left.len() > 0 {
        recursive_diff(a, a_left, b, b_left, v_top, v_bottom, edits);
        // }
        // if a_right.len() > 0 && b_right.len() > 0 {
        recursive_diff(a, a_right, b, b_right, v_top, v_bottom, edits);
        // }
    }
}

/*

  sub recursive_diff(A, N, B, M ):
    {
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
*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ring_buffer() {
        let mut buffer = RingBuffer::new(9);

        // Write values.
        buffer[-4] = 40;
        buffer[-3] = 30;
        buffer[-2] = 20;
        buffer[-1] = 10;
        buffer[0] = 100;
        buffer[1] = 1000;
        buffer[2] = 2000;
        buffer[3] = 3000;
        buffer[4] = 4000;

        // Read values.
        assert_eq!(buffer[-4], 40);
        assert_eq!(buffer[-3], 30);
        assert_eq!(buffer[-2], 20);
        assert_eq!(buffer[-1], 10);
        assert_eq!(buffer[0], 100);
        assert_eq!(buffer[1], 1000);
        assert_eq!(buffer[2], 2000);
        assert_eq!(buffer[3], 3000);
        assert_eq!(buffer[4], 4000);

        // Read values with wrap-around downwards...
        assert_eq!(buffer[-5], 4000);
        assert_eq!(buffer[-6], 3000);
        assert_eq!(buffer[-7], 2000);
        assert_eq!(buffer[-8], 1000);
        assert_eq!(buffer[-9], 100);
        assert_eq!(buffer[-10], 10);

        // And upwards...
        assert_eq!(buffer[5], 40);
        assert_eq!(buffer[6], 30);
        assert_eq!(buffer[7], 20);
        assert_eq!(buffer[8], 10);
        assert_eq!(buffer[9], 100);
        assert_eq!(buffer[10], 1000);
    }

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
    fn test_find_middle_snake() {
        // From Myer's paper:
        let a = vec!["A", "B", "C", "A", "B", "B", "A"];
        let b = vec!["C", "B", "A", "B", "A", "C"];
        let a_len = a.len();
        let b_len = b.len();
        let mut v_top = RingBuffer::new((a_len + b_len) * 2 + 1);
        let mut v_bottom = RingBuffer::new((a_len + b_len) * 2 + 1);
        let mut edits = vec![];
        let snake =
            find_middle_snake(&a, 0..a_len, &b, 0..b_len, &mut v_top, &mut v_bottom, &mut edits);
        assert_eq!(snake, (4, 1));
    }

    #[test]
    fn test_delete_everything() {
        let a = vec!["goodbye", "cruel", "world"].join("\n");
        let b = "";
        assert_eq!(
            diff_string_lines(&a, &b),
            Diff(vec![Delete(Idx(1)), Delete(Idx(2)), Delete(Idx(3)),])
        );
    }

    #[test]
    fn test_add_to_empty_file() {
        let a = "";
        let b = vec!["hi", "there"].join("\n");
        assert_eq!(diff_string_lines(&a, &b), Diff(vec![Insert(Idx(1)), Insert(Idx(2)),]));
    }

    #[test]
    fn test_empty_to_empty_diff() {
        let a = "";
        let b = "";
        assert_eq!(diff_string_lines(&a, &b), Diff(vec![]));
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
