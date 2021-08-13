use std::cmp::min;
use std::hash::Hash;
use std::ops::Range;

use crate::diff::*;
use crate::myers;

/// Represents an "LCS" selected as a region around which to split two sequences. If I understand
/// the Histogram diff algorithm correctly, this is an unfortunate choice of terminology carried
/// over from the Patience diff algorithm, where it applied more strictly.
///
/// In Patience diff, unique lines from each sequence are compared and an actual LCS (Longest
/// Common Subsequence) is computed for the two sequences of lines, using patience sorting. The
/// indices corresponding to the items in the LCS are used as split points in a divide-and-conquer
/// approach that recurses on the sections bordered by the splits.
///
/// Histogram diff, on the other hand, will choose the lowest-count non-unique lines available, if
/// no unique lines are available. It will then look for matching lines on either side, seeking
/// to identify the longest possible _contiguous_ subsequence on either side. This section is
/// called an "LCS" in the Histogram diff code comments; it is not necessarily an LCS in the
/// technical sense (although it may be, by coincidence). The best section found is then used to
/// divide and conquer, similar to the Patience algorithm (but note, we only ever do one split at
/// a time). Here "best" means around the "rarest lines". In the presence of unique line pairs,
/// Histogram is said to behave identically to Patience.
#[derive(Debug)]
struct Region {
    a_start: usize,
    a_end: usize,
    b_start: usize,
    b_end: usize,
}

#[derive(Clone, Debug)]
struct Record {
    /// Pointer to the next record in the chain.
    next: Option<usize>,

    /// Line index of the item within the sequence.
    index: usize,

    /// Count of items in the chain after and including this item. Capped at MAX_CHAIN_LENGTH.
    count: usize,
}

/// Histogram hash table and supporting indices.
#[derive(Debug)]
struct Histogram {
    mask: usize,

    /// Actual record storage.
    pub records: Vec<Option<Record>>,

    /// The hash table index itself. For a given hash, maps to an index in the `records` vector.
    pub table: Vec<Option<usize>>,
}

impl Histogram {
    pub fn new(len: usize) -> Self {
        let size = len.checked_next_power_of_two().expect("Input length too large");

        // Compute mask for deriving table index from hash code; eg.
        // 1. Consider a table size of 256 (2 ** 8).
        // 2. As binary, that's: 0b100000000 (ie. 8 zeros).
        // 3. Take 1, yielding: 0b11111111 (ie. 8 ones).
        let mask = size - 1;

        Histogram {
            mask,
            records: Vec::with_capacity(len),
            table: vec![None; size],
        }
    }
    pub fn idx_for_hash(&self, item_hash: u64) -> usize {
        // Note it's ok to truncate from u64 to usize (if we were to run on a 32-bit system, where
        // usize is 32 bits), because we only want at most usize bits.
        self.mask & (item_hash as usize)
    }
}

/// Used to limit cost of the algorithm in two ways:
///
/// 1. Any items appearing > 64 times in the input sequence are capped at 64. Both the Git and JGit
///    implementations bail out completely if this happens; we're not actually do that yet, but
///    probably should.
/// 2. In the event of a pathological number of hash collisions, we abort after visiting 64 hash
///    table slots.
const MAX_CHAIN_LENGTH: usize = 64;

pub fn diff<T>(a: &Vec<T>, b: &Vec<T>) -> Diff
where
    T: Hash + PartialEq,
{
    let a_hashes: Vec<u64> = a.iter().map(hash).collect();
    let b_hashes: Vec<u64> = b.iter().map(hash).collect();
    let a_range = 0..a.len();
    let b_range = 0..b.len();

    histogram_diff(a, a_range, &a_hashes, b, b_range, &b_hashes)
}

fn histogram_diff<T>(
    a: &Vec<T>,
    a_range: Range<usize>,
    a_hashes: &Vec<u64>,
    b: &Vec<T>,
    b_range: Range<usize>,
    b_hashes: &Vec<u64>) -> Diff
where
    T: Hash + PartialEq,
{
    if a_range.len() == 0 || b_range.len() == 0 {
        println!("falling back to myers... lengths: {} {}", a_range.len(), b_range.len());
        let mut edits = vec![];
        myers::recursive_diff(a, a_range, a_hashes, b, b_range, b_hashes, &mut edits);
        Diff(edits)
    } else {
        if let Some(region) = find_split_region(a, a_range.clone(), a_hashes, b, b_range.clone(), b_hashes) {
            println!("got region {:?}", region);
            // recurse. may want to special case match at start (ie. empty side of split)
            // let left = vec![a_range.start..region.a_start, b_range.start..region.b_start];
            // let right = vec![region.a_end..a_range.end, region.b_end..b_range.end];
            let left = histogram_diff(
                a,
                a_range.start..region.a_start,
                a_hashes,
                b,
                b_range.start..region.b_start,
                b_hashes,
            );
            // println!("left recursive call {:?}", left);
            let right = histogram_diff(
                a,
                region.a_end..a_range.end,
                a_hashes,
                b,
                region.b_end..b_range.end,
                b_hashes,
            );
            // println!("right recursive call {:?}", right);
            // Diff([&left.0[..], &right.0[..]].concat())
            let mut edits = vec![];
            edits.extend(left.0);
            edits.extend(right.0);
            Diff(edits)
        } else {
            println!("did not get a region will have to fall back");
            // or emit a "replace" edit...
            //
            let mut edits = vec![];
            myers::recursive_diff(a, a_range, a_hashes, b, b_range, b_hashes, &mut edits);
            Diff(edits)
        }
    }
}

fn find_split_region<T>(
    a: &Vec<T>,
    a_range: Range<usize>,
    a_hashes: &Vec<u64>,
    b: &Vec<T>,
    b_range: Range<usize>,
    b_hashes: &Vec<u64>,
) -> Option<Region>
where
    T: Hash + PartialEq,
{
    let a_len = a_range.len();
    if let Some(histogram) = scan(a, a_range.clone(), a_hashes) {
        let mut region = Region { a_start: a_range.start, b_start: b_range.start, a_end: a_range.start, b_end: b_range.end };
        let mut count = MAX_CHAIN_LENGTH + 1;
        let mut b_index = b_range.start;
        let mut has_common = false;
        'scan_b: while b_index < b_range.end {
            let mut b_next = b_index + 1;
            let b_hash = b_hashes[b_index];
            let starting_table_index = histogram.idx_for_hash(b_hash);
            let mut table_index = starting_table_index;

            let mut chain_length = 0;
            while chain_length < MAX_CHAIN_LENGTH {
                match histogram.table[table_index] {
                    Some(record_index) => {
                        chain_length += 1;
                        let record = histogram.records[record_index].as_ref().unwrap();
                        if a_hashes[record.index] == b_hashes[b_index] {
                            if a[record.index] == b[b_index] {
                                // Hashes match and items are equal; this is our chain.
                                has_common = true;
                                if record.count > count {
                                    // Item occurs more times than current "best seen" item.
                                    // Skip this one (prefer the rarer one).
                                    b_index = b_next;
                                    continue 'scan_b;
                                }

                                // Look at each place the item appears in `a` and see if that place
                                // plus surrounding adjacent matching lines beats our "best seen"
                                // region.
                                let mut a_start = record.index;
                                'try_locations: loop {
                                    let mut record_count = record.count;
                                    let mut a_end = a_start + 1;
                                    let mut b_start = b_index;
                                    let mut b_end = b_start + 1;

                                    while region.a_start < a_start
                                        && region.b_start < b_start
                                        && a_hashes[a_start - 1] == b_hashes[b_start - 1]
                                        && a[a_start - 1] == b[b_start - 1]
                                    {
                                        a_start -= 1;
                                        b_start -= 1;
                                        if record_count >= 1 {
                                            // NOTE: i'm not convinced this bit makes much sense: it
                                            // is basically saying, "take whatever is smallest:
                                            // the smallest occurence count seen for this chain so
                                            // far, or the occurence count for the first line in
                                            // the region". Note that because we extend backwards
                                            // first, then upwards last, the upwards checks are
                                            // going to find successively smaller counts for any
                                            // items they hit in chains (because chains are built
                                            // in reverse).
                                            record_count = min(
                                                record_count,
                                                histogram.records[a_len - (a_start - a_range.start)]
                                                    .as_ref()
                                                    .unwrap()
                                                    .count,
                                            );
                                        }
                                    }

                                    while a_end < region.a_end
                                        && b_end < region.b_end
                                        && a_hashes[a_end] == b_hashes[b_end]
                                        && a[a_end] == b[b_end]
                                    {
                                        a_end += 1;
                                        b_end += 1;
                                        if record_count >= 1 {
                                            record_count = min(
                                                record_count,
                                                histogram.records[a_len - (a_end - a_range.start)]
                                                    .as_ref()
                                                    .unwrap()
                                                    .count,
                                            );
                                        }
                                    }

                                    if b_next < b_end {
                                        b_next = b_end;
                                    }

                                    if (region.a_end - region.a_start) < (a_end - a_start) || record_count < count {
                                        // Region is longest found, or chain is rarer; so it is our current best
                                        // region.
                                        region.a_start = a_start;
                                        region.b_start = b_start;
                                        region.a_end = a_end;
                                        region.b_end = b_end;
                                        count = record_count;
                                    }

                                    if record.next.is_none() {
                                        break 'try_locations;
                                    }
                                    let mut next_record = record.next;
                                    while let Some(next_record_index) = next_record {
                                        let record = histogram.records[next_record_index].as_ref().unwrap();
                                        if record.index >= a_end {
                                            // We've skipped over any occurences within the region
                                            // we already explored.
                                            a_start = record.index;
                                            continue 'try_locations;
                                        }
                                        next_record = record.next;
                                    }

                                    // No more locations to consider for this chain.
                                    break;
                                }

                                b_index = b_next;
                                continue 'scan_b;
                            }
                        }

                        // Hashes didn't match, or they did match and items weren't equal (ie. a
                        // hash collision). Try next chain in table, if there is one.
                        let table_len = histogram.table.len();
                        if table_index + 1 < starting_table_index ||
                            table_index + 1 < table_len {
                            table_index += 1;
                        } else if table_index + 1 == table_len && starting_table_index > 0 {
                            table_index = 0;
                        } else {
                            b_index = b_next;
                            continue 'scan_b;
                        }
                    },
                    None => {
                        // Item from `b` appears nowhere in `a`.
                        b_index = b_next;
                        continue 'scan_b;
                    }
                }
            }
            // We hit MAX_CHAIN_LENGTH before finding a matching chain (rather unlikely, but still
            // possible).
            b_index = b_next;
        }

        if !has_common || count < MAX_CHAIN_LENGTH {
            return Some(region);
        }
    }
    None
}

fn scan<T>(a: &Vec<T>, a_range: Range<usize>, a_hashes: &Vec<u64>) -> Option<Histogram>
where
    T: Hash + PartialEq,
{
    let mut histogram = Histogram::new(a_range.len());

    // Iterate in reverse prepending matching items to chains (ie. earliest match will appear at
    // head of chain).
    'scan_line: for sequence_index in a_range.clone().rev() {
        let item = &a[sequence_index];
        let item_hash = a_hashes[sequence_index];
        let mut table_index = histogram.idx_for_hash(item_hash);

        // Combined length of chains traversed to find insertion point.
        let mut chain_length = 0;

        if let Some(record_index) = histogram.table[table_index] {
            let mut record_index = record_index;
            while chain_length <= MAX_CHAIN_LENGTH {
                chain_length += 1;
                let record = histogram.records[record_index].as_ref().unwrap();
                let line_index = record.index;
                let record_count = record.count;
                if a_hashes[line_index] == item_hash && &a[line_index] == item {
                    let new_index = histogram.records.len();
                    histogram.records.push(Some(Record {
                        next: Some(record_index),
                        index: sequence_index,
                        count: min(MAX_CHAIN_LENGTH, record_count + 1),
                    }));
                    histogram.table[table_index] = Some(new_index);
                    continue 'scan_line;
                } else {
                    // Hash collision or slot already occupied; try next slot.
                    table_index += 1;
                    match histogram.table[table_index] {
                        Some(idx) => {
                            record_index = idx;
                            continue;
                        }
                        None => break,
                    }
                }
            }
        }

        if chain_length == MAX_CHAIN_LENGTH {
            return None;
        }

        // First time we've seen this element. Start a new chain for it.
        let new_index = histogram.records.len();
        histogram.records.push(Some(Record { next: None, index: sequence_index, count: 1 }));
        histogram.table[table_index] = Some(new_index);
    }

    Some(histogram)
}

#[cfg(test)]
mod tests {
    use super::*;

    use Edit::*;

    #[test]
    fn blinking_light() {
        let a = vec!["A", "B", "C", "A", "B", "B", "A"];
        let b = vec!["C", "B", "A", "B", "A", "C"];

        // Note this is not acutally an SES (Shortest Edit Script), but agrees with the output of
        // `git diff --diff-algorithm=histogram`:
        //
        //      diff --git a/before b/after
        //      index fd113b0..0075e6d 100644
        //      --- a/before
        //      +++ b/after
        //      @@ -1,7 +1,6 @@
        //      -A
        //      -B
        //       C
        //      -A
        //      -B
        //       B
        //       A
        //      +B
        //      +A
        //      +C
        assert_eq!(
            diff(&a, &b),
            Diff(vec![
                Delete(Idx(1)),
                Delete(Idx(2)),
                Delete(Idx(4)),
                Delete(Idx(5)),
                Insert(Idx(4)),
                Insert(Idx(5)),
                Insert(Idx(6)),
            ])
        );
    }
}
