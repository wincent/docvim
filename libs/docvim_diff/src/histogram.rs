use std::cmp::min;
use std::hash::Hash;

use crate::diff::*;
use crate::myers;

// TODO: move this into tests if it is only used in tests.
// use Edit::*;

/// Represents an "LCS" selected as a region around which to split two sequences. If I understand
/// the Histogram diff algorithm correctly, this is an unfortunate choice of terminology carried
/// over from the Patience diff algorithm, where it applied more strictly.
///
/// In Patience diff, unique lines from each sequence are compared and an actual LCS is computed
/// for the two sequences of lines, using patience sorting. The indices corresponding to the items
/// in the LCS are used as split points in a divide-and-conquer approach that recurses on the
/// sections bordered by the splits.
///
/// Histogram diff, on the other hand, will choose the lowest-count non-unique lines available, if
/// no unique lines are available. It will then look for matching lines on either side, seeking to
/// identify the longest possible _contiguous_ subsequence on either side. This section is called
/// an "LCS" in the Histogram diff code comments, but it is not an LCS in the technical sense. The
/// best section found is then used to divide and conquer, similar to the Patience algorithm (but
/// note, we only ever do one split at a time). Here "best" means around the "rarest lines". In the
/// presence of unique line pairs, Histogram is said to behave identically to Patience.
///
/// TODO: see if the above comment is accurate -- does the weird while loop end up being equivalent
/// to patience sort?
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

    /// Map from item in the `records` vector to next identical item's line index in the sequence.
    pub next_map: Vec<Option<usize>>,

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
            next_map: vec![None; len],
            records: Vec::with_capacity(len),
            table: vec![None; size],
        }
    }
    pub fn idx_for_hash(&self, item_hash: u64) -> usize {
        // Note it's ok to truncate from u64 to usize (if we were to run on a 32-bit system, where
        // usize is 32 bits), because we only want at most usize bits.
        self.mask & (item_hash as usize)
    }

    // starts to get ugly...
    // pub fn get(&self, item_hash: usize) -> Option<Record> {
    //     // Note it's ok to truncate from u64 to usize (if we were to run on a 32-bit system, where
    //     // usize is 32 bits), because we only want at most usize bits.
    //     let table_idx = item_hash & self.mask;
    //
    //     self.table[table_idx]
    // }
    //
    // pub fn prepend(&self,
}

const MAX_CHAIN_LENGTH: usize = 64;

pub fn diff<T>(a: &Vec<T>, b: &Vec<T>) -> Diff
where
    T: Hash + PartialEq,
{
    // Precompute line hashes.
    let a_hashes: Vec<u64> = a.iter().map(hash).collect();
    let b_hashes: Vec<u64> = b.iter().map(hash).collect();

    histogram_diff(a, &a_hashes, b, &b_hashes)
}

fn histogram_diff<T>(a: &Vec<T>, a_hashes: &Vec<u64>, b: &Vec<T>, b_hashes: &Vec<u64>) -> Diff
where
    T: Hash + PartialEq,
{
    // TODO: find infinite loop in this line:
    println!("{:?}", find_split_region(a, a_hashes, b, b_hashes));
    myers::diff(a, b)
}

fn find_split_region<T>(
    a: &Vec<T>,
    a_hashes: &Vec<u64>,
    b: &Vec<T>,
    b_hashes: &Vec<u64>,
) -> Option<Region>
where
    T: Hash + PartialEq,
{
    let a_len = a.len();
    if let Some(histogram) = scan(a, a_hashes) {
        let mut region = Region { a_start: 0, b_start: 0, a_end: 0, b_end: 0 };
        let mut count = MAX_CHAIN_LENGTH + 1;
        let mut b_index = 0;
        let mut has_common = false;
        while b_index < b.len() {
            let mut b_next = b_index + 1;
            let b_hash = b_hashes[b_index];
            let table_index = histogram.idx_for_hash(b_hash);

            if let Some(record_index) = histogram.table[table_index] {
                let mut next_record = &histogram.records[record_index];
                while let Some(record) = next_record {
                    let mut record_count = record.count;
                    if record_count > count {
                        if !has_common {
                            has_common = a_hashes[record.index] == b_hashes[b_index]
                                && a[record.index] == b[b_index];
                        }
                        match record.next {
                            Some(next_index) => {
                                let next_record_index = a_len - next_index;
                                next_record = &histogram.records[next_record_index];
                            }
                            None => break,
                        };
                        continue;
                    }

                    let mut a_start = record.index;
                    if a_hashes[a_start] != b_hashes[b_index] && a[a_start] != b[b_index] {
                        match record.next {
                            Some(next_index) => {
                                let next_record_index = a_len - next_index;
                                next_record = &histogram.records[next_record_index];
                            },
                            None => break,
                        };
                        continue;
                    }

                    has_common = true;

                    'try_locations: loop {
                        let mut next_index = match record.next {
                            Some(idx) => histogram.next_map[idx],
                            None => None,
                        };
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
                                record_count = min(
                                    record_count,
                                    histogram.records[a_len - a_start]
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
                                    histogram.records[a_len - a_start]
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

                        if next_index.is_none() {
                            break 'try_locations;
                        }
                        while let Some(next) = next_index {
                            if next < a_end {
                                next_index = histogram.next_map[next];
                                if next_index.is_none() {
                                    break 'try_locations;
                                }
                            } else {
                                break;
                            }
                        }
                        a_start = next_index.unwrap();
                    }
                    match record.next {
                        Some(idx) => next_record = &histogram.records[idx],
                        None => break,
                    }
                }

                b_index = b_next;
            }
        }

        if !has_common || count < MAX_CHAIN_LENGTH {
            return Some(region);
        }
    }
    None
}

fn scan<T>(a: &Vec<T>, a_hashes: &Vec<u64>) -> Option<Histogram>
where
    T: Hash + PartialEq,
{
    let mut histogram = Histogram::new(a.len());

    // Iterate in reverse prepending matching items to chains (ie. earliest match will appear at
    // head of chain).
    'scan_line: for (sequence_index, item) in a.iter().enumerate().rev() {
        let item_hash = a_hashes[sequence_index];
        let mut table_idx = histogram.idx_for_hash(item_hash);

        // Combined length of chains traversed to find insertion point.
        let mut chain_length = 0;

        if let Some(record_index) = histogram.table[table_idx] {
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
                    histogram.next_map[new_index] = Some(line_index);
                    histogram.table[table_idx] = Some(new_index);
                    continue 'scan_line;
                } else {
                    // Hash collision; try next slot.
                    table_idx += 1;
                    match histogram.table[table_idx] {
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
        histogram.table[table_idx] = Some(new_index);
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

        assert_eq!(
            diff(&a, &b),
            Diff(vec![
                // comment out one of these to break test so i can see my println, if desired
                // Delete(Idx(1)),
                Delete(Idx(2)),
                Delete(Idx(4)),
                Insert(Idx(3)),
                Insert(Idx(6)),
            ])
        );
    }
}
