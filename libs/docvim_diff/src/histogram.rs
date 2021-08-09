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
    T: Hash + PartialEq
{
    // Map from line index to corresponding record in records array.
    let mut line_map: Vec<Option<usize>> = vec![None; a.len()];

    // Map from item in records list to next identical item's index in records hash table.
    let mut next_map: Vec<Option<usize>> = vec![None; a.len()];

    // Actual record storage.
    let mut records: Vec<Option<Record>> = Vec::with_capacity(a.len());

    // Create records hash table for `a`.
    let size = a.len().checked_next_power_of_two().expect("Input length too large");
    let mut table: Vec<Option<usize>> = vec![None; size];

    // Compute mask for deriving table index from hash code; eg.
    // 1. Consider a table size of 256 (2 ** 8).
    // 2. As binary, that's: 0b100000000 (ie. 8 zeros).
    // 3. Take 1, yielding: 0b11111111 (ie. 8 ones).
    let mask = size - 1;

    // Iterate in reverse prepending matching items to chains (ie. earliest match will appear at
    // head of chain).
    for (sequence_index, item) in a.iter().enumerate().rev() {
        let item_hash = a_hashes[sequence_index];

        // Note it's ok to truncate from u64 to usize (if we were to run on a 32-bit system, where
        // usize is 32 bits), because we only want at most usize bits.
        let mut table_idx = (item_hash as usize) & mask;

        let mut chain_length = 0;
        let mut found_identical = false;
        while let Some(record_index) = table[table_idx] {
            let record = records[record_index].as_ref().unwrap();
            let line_index = record.index;
            let record_count = record.count;
            if a_hashes[line_index] == item_hash && &a[line_index] == item {
                let new_index = records.len();
                line_map[sequence_index] = Some(new_index);
                records.push(Some(Record {
                    next: Some(record_index),
                    index: sequence_index,
                    count: min(MAX_CHAIN_LENGTH, record_count + 1),
                }));
                next_map[new_index] = Some(line_index);
                found_identical = true;
                table[table_idx] = Some(new_index);
                break;
            }
            chain_length += 1;
            if let Some(next_idx) = record.next {
                table_idx = next_idx;
            } else {
                break;
            }
        }

        if !found_identical {
            if chain_length == MAX_CHAIN_LENGTH {
                // bail
            }

            // First time we've seen this element. Start a new chain for it.
            let new_index = records.len();
            line_map[sequence_index] = Some(new_index);
            records.push(Some(Record {
                next: None,
                index: sequence_index,
                count: 1,
            }));
            table[table_idx] = Some(new_index);
        }
    }

    println!("table:\n{:?}", table);
    println!("records:\n{:#?}", records);
    println!("line_map:\n{:?}", line_map); // TODO: convince myself that this is necessary... given that I iterate backwards, records[] indices are pretty predictable
    println!("next_map:\n{:?}", next_map);

    myers::diff(a, b)
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
