pub mod diff;
pub mod histogram;
pub mod myers;
mod ring_buffer;

//mod util; // string splitting and such?

use crate::diff::*;
use Edit::*;

const GREEN: &str = "\x1b[0;32m";
const RED: &str = "\x1b[0;31m";
const RESET: &str = "\x1b[0m";

pub fn format_ses<T>(ses: Diff, a: &Vec<T>, b: &Vec<T>) -> String
where
    T: std::fmt::Display + PartialEq,
{
    let mut diff = String::new();

    // Insert(Idx(1)), Insert(Idx(3))
    let mut a_idx = 1;
    let mut b_idx = 1;
    // TODO: deal with hunks (reduce amount of context shown)
    for edit in ses.0 {
        match edit {
            Delete(Idx(delete)) => {
                while a_idx < delete {
                    diff.push_str(&format!(" {}\n", &a[a_idx - 1]));
                    a_idx += 1;
                    b_idx += 1;
                }
                diff.push_str(&format!("{}-{}{}\n", GREEN, &a[a_idx - 1], RESET));
                a_idx += 1;
            }
            Insert(Idx(insert)) => {
                while b_idx < insert {
                    diff.push_str(&format!(" {}\n", &b[b_idx - 1]));
                    a_idx += 1;
                    b_idx += 1;
                }
                diff.push_str(&format!("{}+{}{}\n", RED, &b[b_idx - 1], RESET));
                b_idx += 1;
            }
        }
    }

    diff
}

#[cfg(test)]
mod tests {
    #[test]
    fn blinking_light() {
        assert!(true);
    }
}
