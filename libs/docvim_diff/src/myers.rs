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
#[derive(Clone, Debug)]
struct RingBuffer {
    capacity: usize,
    // TODO: try and make this generic, maybe, once this is resolved:
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

// TODO: figure out whether to keep these around... ultimately the caller will also want the lines;
// might be better off with just `diff_lines()`, but in that case, you may as well pass them
// directly to `diff()`.
pub fn diff_string_lines(a: &str, b: &str) -> Diff {
    let a = a.lines().collect::<Vec<&str>>();
    let b = b.lines().collect::<Vec<&str>>();
    diff(&a, 0..a.len(), &b, 0..b.len())
}

pub fn diff_string_lines_nd(a: &str, b: &str) -> Diff {
    let a = a.lines().collect::<Vec<&str>>();
    let b = b.lines().collect::<Vec<&str>>();
    myers_nd_diff(&a, 0..a.len(), &b, 0..b.len())
}

pub fn diff<T>(a: &T, a_range: Range<usize>, b: &T, b_range: Range<usize>) -> Diff
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    let mut edits = vec![];
    recursive_diff(a, a_range, b, b_range, &mut edits);
    Diff(edits)
}

/// Finds a Shortest Edit Script to transform `a` into `b`. This is equivalent to finding the
/// shortest path through the edit graph from the origin `(0, 0)` at the top-right to the target
/// `(n, m)` at the bottom left. In an edit graph, horizontal edges represent a deletion from `a`,
/// vertical edges represent insertion from `b`, and diagonal edges indicate match points between
/// `a` and `b` where no edit is required. In the following diagram, `*` shows a shortest path from
/// input `a` ("ABCABBA") to `b` ("CBABAC"):
///
///
/// ```ignore
///               (0, 0)
///                    \
///                     #*A*B-C-A-B-B-A 0                   SES (Shortest Edit Script):
///                     ¦ ¦ ¦*¦ ¦ ¦ ¦ ¦                       Delete index 1 from `a`
///                     C-¦-¦-*-¦-¦-¦-¦ 1                     Delete index 2 from `a`
///                     ¦ ¦ ¦ * ¦ ¦ ¦ ¦                       Insert index 2 from `b`
///                     B-¦-¦-*-¦-¦-¦-¦ 2                     Delete index 6 from `a`
///                     ¦ ¦ ¦ ¦*¦ ¦ ¦ ¦                       Insert index 6 from `b`
///                     A-¦-¦-¦-*-¦-¦-¦ 3
///                     ¦ ¦ ¦ ¦ ¦*¦ ¦ ¦                     LCS (Longest Common Subsequence):
///                     B-¦-¦-¦-¦-***-¦ 4                     C, A, B, A
///                     ¦ ¦ ¦ ¦ ¦ ¦ ¦*¦
///                     A-¦-¦-¦-¦-¦-¦-* 5
///                     ¦ ¦ ¦ ¦ ¦ ¦ ¦ *
///                     C-¦-¦-¦-¦-¦-¦-# 6
///                                    \
///                     0 1 2 3 4 5 6 7 \
///                                     (7, 6)
/// ```
///
/// The SES corresponds to the horizontal and vertical edges. The LCS corresponds to the diagonal
/// edges. In the Myers paper runs of 0 or more diagonal edges are called "snakes". Note that there
/// may be more than one SES, but the algorithm is greedy and favors deletions, so it returns the
/// SES shown above.
///
/// This is the `O(ND)` time and `O(ND)` space version of the diff algorithm presented in the
/// first part of the Myers paper. Note that `N` here is the _sum_ of the lengths of the inputs
/// and "D" is the length of the minimal edit script, so, following the convention used elsewhere
/// in the paper (where `N` and `M` refer to the input lengths), we could also write the time and
/// space requirement as `D(N + M)`. Given that the upper bound for `D` is `N + M`, we have have
/// quadratic growth which makes this algorithm impractical for anything but the most modest input
/// sizes.
///
pub fn myers_nd_diff<T>(a: &T, a_range: Range<usize>, b: &T, b_range: Range<usize>) -> Diff
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    // TODO: figure out how to reduce the number of casts here...
    // it's a frick'n' cast-fest
    let n = a_range.len();
    let m = b_range.len();
    let max = n + m;
    let mut v = RingBuffer::new(max * 2 + 1); // Range from -max to +max, with extra slot for 0.
    let mut vs = vec![];
    v[1] = 0; // Technically it's already 0, but to make it explicit.

    // Find all "D-paths" between the origin (0, 0) and the destination (n, m). A D-path represents
    // all the locations that can be reached in the edit graph after D edits. The algorithm is
    // greedy, so the first path that reaches (n, m) terminates.
    //
    // The best case scenario is a graph where (n, m) can be reached via a 0-path; that is, where
    // no edits are needed to transform input `a` into input `b` (eg. "abcd" to "abcd"). This
    // corresponds to a path where all edges are diagonal.
    //
    // The worst case scenario is where an "n+m" path is required to reach the destination; this
    // occurs when every item in `a` must be deleted, then every item in `b` inserted (eg. "abcd"
    // to "wxyz"). This corresponds to a path where no edges are diagonal.
    //
    // For typical inputs, there will be multiple D-paths corresponding to the different locations
    // in the graph that can be reached after 0, 1, 2 .. `max` edits, but the first D-path that
    // reaches the destination will terminate the algorithm.
    for d in 0..=(usize_to_isize(max)) {
        // We step by 2 because for any given k-line, the paths that reach that k-line originate
        // from `k - 1` or `k + 1`. That is, a path ending on, say, `k = 3` must consist of either:
        //
        // - A horizontal (insertion) edge coming from `k - 1` (2) followed by a (possibly
        //   zero-length) snake; or:
        // - A vertical (deletion) edge coming from `k + 1` (4) followed by a (possibly
        //   zero-length) snake.
        //
        // Thus, when `d` is even, we end up writing new values for even k-lines, and reading
        // values from odd k-lines; when `d` is odd, we write new vales for odd k-lines, and read
        // values from even k-lines.
        for k in (-d..=d).step_by(2) {
            let mut x: usize;
            let mut y: usize;
            if k == -d || k != d && v[k - 1] < v[k + 1] {
                // Extend path (downwards) using k-line above because:
                //
                // - `k == -d`: You're on the left edge, so there is only one place you can extend
                //    from (the k-line above); or:
                // - `k != d`: You're not on the top border (which would force you to use the
                //    k-line below); and:
                // - `v[k - 1] < v[k + 1]`: The k-line below has a smaller value than the k-line
                //    above; we take the bigger value because this is a greedy algorithm.
                x = v[k + 1];
            } else {
                // Extend path (rightwards) using k-line below because:
                //
                // - `k != -d`: You're not on the left edge (which would force you to use the
                //   k-line above); and:
                // - `k == d`: You're on the top edge (forcing you to use the k-line below); or:
                // - `v[k - 1] == v[k + 1]`: Both k-lines offer equal progress, but we bias for
                //   deletions and the k-line below means we can do that (ie. by increasing `x`).
                // - `v[k - 1] > v[k + 1]`: The k-line below offers more progress _and_ we get to
                //   do a deletion.
                x = v[k - 1] + 1;
            }
            y = ((x as isize) - k) as usize;

            // If there is a snake (matching items), extend path diagonally down as long as they
            // match.
            //
            // Note that the Myers paper checks for equality at x + 1 and y + 1 (1-based indexing),
            // but we are using 0-based indexing here so as not to overflow:
            while x < n && y < m && eq(a, x, b, y) {
                x += 1;
                y += 1;
            }

            // Remember the farthest x coordinate reached on given k line at edit-depth d.
            // We can later recover the y coordinate (because `y = x - k`).
            v[k] = x;

            if x >= n && y >= m {
                vs.push(v.clone());
                let mut edits = vec![];
                myers_nd_generate_path(&vs, d as usize, n, m, &mut edits);
                return Diff(edits);
            }
        }
        vs.push(v.clone());
    }

    // The length of the SES is > max; I don't know why we'd ever get here.
    panic!("no SES found");
}

/// The basic Myers algorithm only computes the length of the SES (Shortest Edit Script). To
/// recover the actual script we have to traverse the snapshots of the `v` vector that we took for
/// each depth `d`.
fn myers_nd_generate_path(
    vs: &Vec<RingBuffer>,
    d: usize,
    n: usize,
    m: usize,
    edits: &mut Vec<Edit>,
) {
    // The Myers paper expresses this recursively, but we frame it iteratively here in order to
    // avoid stack overflows for very long edit scripts.
    let mut d = d;
    let mut n = n;
    let mut m = m;
    while d > 0 {
        let k = (n as isize) - (m as isize);
        let v = &vs[d];
        let x_end = v[k]; // "end" = Where the edit finished (ie. after potentially zero-length snake).
        let y_end = ((x_end as isize) - k) as isize;
        let down = k == -(d as isize) || k != (d as isize) && v[k - 1] < v[k + 1]; // "down" = true (insertion) or false (deletion)
        let k_prev = if down { k + 1 } else { k - 1 };
        let x_start = v[k_prev];
        let y_start = ((x_start as isize) - k_prev) as usize; // "start" = Where preceding edit started.
        let x_mid = if down { x_start } else { x_start + 1 }; // "mid" = Where the snake (diagonal part) starts; note: diagonal part may be empty.
        let y_mid = ((x_mid as isize) - k) as usize;
        if down {
            edits.push(Insert(Idx(y_mid)));
        } else {
            edits.push(Delete(Idx(x_mid)));
        }
        d = d - 1;
        n = x_start;
        m = y_start;
        if x_start == 0 && y_start == 0 {
            break;
        }
    }
    edits.reverse();
}

fn eq<T>(a: &T, a_idx: usize, b: &T, b_idx: usize) -> bool
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

fn usize_to_isize(n: usize) -> isize {
    isize::try_from(n).expect("overflow converting from usize to isize")
}

// TODO: when input lengths are very different, switch to more complicated alg that avoids
// exploring beyond graph bounds.
fn find_middle_snake<T>(
    a: &T,
    a_range: Range<usize>,
    b: &T,
    b_range: Range<usize>,
) -> (usize, usize, usize, usize, usize)
// x_mid, y_mid, x_end, y_end, D (SES length)
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    let n = usize_to_isize(a_range.len());
    let m = usize_to_isize(b_range.len());
    let max = n + m;
    let mut v_forwards = RingBuffer::new((max * 2) as usize + 1);
    let mut v_reverse = RingBuffer::new((max * 2) as usize + 1);
    let delta = n - m;

    // SES length will be odd when delta is odd. If it's odd, we'll check for overlap when
    // extending forward paths; if it's even, we'll check for overlap when extending reverse paths.
    let odd = delta & 1 == 1;
    println!("find middle: n={} m={} d={} odd?={} a_range={:?} b_range={:?}", n, m, delta, odd, a_range, b_range);

    v_forwards[1] = 0; // Technically it's already 0, but to make it explicit.
    v_reverse[1] = 0; // Technically it's already 0, but to make it explicit.

    // `(max + 1) / 2` here is a shorthand for `ceil(max / 2)`
    for d in 0..=((max + 1) / 2) {
        // Forward search; note that unlike the Myers paper, we iterate in reverse order in order
        // to match Git's behavior. We still get a valid Shortest Edit Script, but it may be a
        // different one because the greedy algorithm will stop at the first overlapping path it
        // finds (and when we go in reverse, we may encounter a different overlapping path first).
        for k in (-d..=d).rev().step_by(2) {
            // Both `x` and `y` are relative to the top-left corner of the region we're searching
            // in; this may be a subregion of the graph, so the origin is not necessarily (0, 0).
            let mut x;
            let mut y;
            if k == -d || k != d && v_forwards[k - 1] < v_forwards[k + 1] {
                x = v_forwards[k + 1];
            } else {
                x = v_forwards[k - 1] + 1;
            }
            y = ((x as isize) - k) as usize;
            let mid_x = x;
            let mid_y = y;
            while x < (n as usize) && y < (m as usize)
                && eq(a, a_range.start + x, b, b_range.start + y)
            {
                x += 1;
                y += 1;
            }
            v_forwards[k] = x;

            // Check for overlap. We must adjust by `delta` because forward k-lines are centered
            // around `0` and start from `(0, 0)`, while reverse k-lines are centered around
            // `delta` (ie. `n - m`) and start from `(n, m)`. (Note that here, `(0, 0)` and `(n,
            // m)` may be scoped to subregions, during recursive calls.)
            if odd
                // NOTE TO SELF: in example input, forwards k-line 0 corresponds with reverse
                // k-line 1 and delta is one... forwards k-line -1 === reverse k-line 2
                // forwards k-line 1 === reverse k-line 0
                // forwards k-line 2 === reverse k-line -1
                // say when d = 2 and we're looking at forwards k-line -2
                // matching reverse k-line = (-2 -1) * -1 eg. 3
                //                        eg (-(k - delta))
                // so the v_reverse[...] looks up max on reciprocal k-line
                // x + that >= n means "x travel on theses two matching k-lines overlaps") (3)
                //
                // the other two lines mean: reciprocal k-line >= -d + 1
                // AND reciprocal k-line <= d - 1
                // which I think are bounds checks to see if there even is a reciprocal line
                //
                //
                && (-(k - delta)) >= -(d - 1)
                && (-(k - delta)) <= (d - 1)
                && (x as isize) + (v_reverse[-(k - delta)] as isize) >= n
            // 3
            {
                // Paths overlap. Last snake of forward path is the middle one. Convert back to
                // "absolute" coordinates before returning.
                return (
                    a_range.start + mid_x,
                    b_range.start + mid_y,
                    a_range.start + x,
                    b_range.start + y,
                    2 * d as usize - 1,
                );
            }
        }

        // Reverse search.
        for k in (-d..=d).rev().step_by(2) {
            let mut x;
            let mut y;
            // When searching in reverse, we actually want to maximize `y` instead of `x`; if we
            // favor upwards movement, that bias will require compensating horizontal movement --
            // ie. deletions -- in the forward path in order to produce an overlapping snake, and
            // deletions are what we want.  Also note that the `k - 1` k-lines are "above" and the
            // `k + 1` k-lines are "below" when searching in reverse.  As such, we switch the `<`
            // comparison here to `<=` (ie. we'll choose the vertical path unless the horizontal
            // one is strictly better, or unavoidable).
            if k == -d || k != d && v_reverse[k - 1] <= v_reverse[k + 1] {
                x = v_reverse[k + 1];
            } else {
                x = v_reverse[k - 1] + 1;
            }

            // `y` = units from the _bottom_ of region where we started (may be a subregion of the
            // graph), just as `x` = units from _right_ of the region where we started (again, may
            // be a subregion of the graph).
            y = ((x as isize) - k) as usize;
            let mid_x = x;
            let mid_y = y;
            while x < (n as usize)
                && y < (m as usize)
                && eq(a, a_range.end - x - 1, b, b_range.end - y - 1)
            {
                x += 1;
                y += 1;
            }
            v_reverse[k] = x;

            if !odd
                && (-(k - delta)) >= -d
                && (-(k - delta)) <= d
                && (x as isize) + (v_forwards[-(k - delta)] as isize) >= n
            {
                // Because we're searching in reverse, our "mid" coordinates (the start of the
                // snake) and "end" coordinates (the end of the snake) need to be swapped to match
                // the direction fo the forward-facing snakes. Here, again, we use "absolute"
                // coordinates, which is what our caller cares about.
                return (
                    a_range.end - x,
                    b_range.end - y,
                    a_range.end - mid_x,
                    b_range.end - mid_y,
                    2 * d as usize,
                );
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
    edits: &mut Vec<Edit>,
) -> ()
where
    T: Index<usize> + ?Sized,
    T::Output: Hash,
{
    let n = a_range.len();
    let m = b_range.len();
    if n > 0 && m > 0 {
        let (x_mid, y_mid, x_end, y_end, d) =
            find_middle_snake(a, a_range.clone(), b, b_range.clone());

        if d > 1 || x_mid != x_end && y_mid != y_end {
            {
                let a_range = a_range.start..x_mid;
                let b_range = b_range.start..y_mid;
                recursive_diff(a, a_range, b, b_range, edits);
            }
            {
                let a_range = x_end..a_range.end;
                let b_range = y_end..b_range.end;
                recursive_diff(a, a_range, b, b_range, edits);
            }
        } else if m > n {
            // TODO: sort this out...
            // There is one edit left to do (d == 1); it's going to be an insertion.
            // let a_range = (a_range.start + x_end)..a_range.end;
            // let b_range = (b_range.start + y_end)..b_range.end;
            // recursive_diff(a, a_range, b, b_range, edits);
        } else if m < n {
            // TODO: handle this
            // There is one edit left to do (d == 1); it's going to be an deletion.
            // let a_range = a_range.start..x_mid;
            // let b_range = b_range.start..y_mid;
            // recursive_diff(a, a_range, b, b_range, edits);
        }
    } else if n > 0 {
        for i in a_range.clone() {
            edits.push(Delete(Idx(i + 1)));
        }
    } else {
        for i in b_range.clone() {
            edits.push(Insert(Idx(i + 1)));
        }
    }
}

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
    fn test_find_middle_snake() {
        // From Myers paper:
        let a = vec!["A", "B", "C", "A", "B", "B", "A"];
        let b = vec!["C", "B", "A", "B", "A", "C"];
        let a_len = a.len();
        let b_len = b.len();
        let snake = find_middle_snake(&a, 0..a_len, &b, 0..b_len);

        // x_mid, y_mid, x_end, x_start, d (SES length)
        assert_eq!(snake, (4, 1, 5, 2, 5));

        // First sub-problem from Myers paper; ie. looking at prefixes:
        // a = vec!["A", "B", "C", ...] <- First 3 items.
        // b = vec!["C", "B", ...] <-- First 2 items.
        let snake = find_middle_snake(&a, 0..3, &b, 0..2);

        // This demonstrates bias for deletion, compared to other equal-distance paths like
        // (1, 1, 2, 2, 3).
        assert_eq!(snake, (2, 0, 3, 1, 3));

        // Second sub-problem from Myers paper; ie. looking at suffixes:
        // a = vec![..., "B", "A"] <-- Last 2 items.
        // b = vec![..., "A", "C"] <-- Last 2 items.
        let snake = find_middle_snake(&a, 5..a_len, &b, 4..b_len);

        // Was a regression (was finding a zero-length middle snake when there was a 1-long one).
        assert_eq!(snake, (6, 4, 7, 5, 2));
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

        // Note this is different from the SES on page 6, but it is still a valid (5-long) script;
        // see the implementation for details, but in the linear variant we follow Git's lead and
        // reverse the iteration direction in our inner loops, producing a different graph.
        assert_eq!(
            diff_string_lines(&a, &b),
            Diff(vec![
                Delete(Idx(1)),
                Delete(Idx(2)),
                Delete(Idx(4)),
                Insert(Idx(3)),
                Insert(Idx(6)),
            ])
        );
    }

    #[test]
    fn test_delete_everything_nd_variant() {
        let a = vec!["goodbye", "cruel", "world"].join("\n");
        let b = "";
        assert_eq!(
            diff_string_lines_nd(&a, &b),
            Diff(vec![Delete(Idx(1)), Delete(Idx(2)), Delete(Idx(3)),])
        );
    }

    #[test]
    fn test_add_to_empty_file_nd_variant() {
        let a = "";
        let b = vec!["hi", "there"].join("\n");
        assert_eq!(diff_string_lines_nd(&a, &b), Diff(vec![Insert(Idx(1)), Insert(Idx(2)),]));
    }

    #[test]
    fn test_empty_to_empty_diff_nd_variant() {
        let a = "";
        let b = "";
        assert_eq!(diff_string_lines_nd(&a, &b), Diff(vec![]));
    }

    #[test]
    fn test_example_from_myers_paper_nd_variant() {
        let a = vec!["A", "B", "C", "A", "B", "B", "A"].join("\n");
        let b = vec!["C", "B", "A", "B", "A", "C"].join("\n");

        // This SES corresponds to the graph shown on page 6 of the paper.
        assert_eq!(
            diff_string_lines_nd(&a, &b),
            Diff(vec![
                Delete(Idx(1)),
                Delete(Idx(2)),
                Insert(Idx(2)),
                Delete(Idx(6)),
                Insert(Idx(6)),
            ])
        );
    }
}
