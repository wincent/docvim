pub mod diff;
pub mod histogram;
pub mod myers;
mod ring_buffer;

use crate::diff::*;
use Edit::*;

const CONTEXT_LINES: usize = 3;
const GREEN: &str = "\x1b[0;32m";
const RED: &str = "\x1b[0;31m";
const RESET: &str = "\x1b[0m";

pub fn format_es<T>(ses: Diff, a: &Vec<T>, b: &Vec<T>) -> String
where
    T: std::fmt::Display + PartialEq,
{
    let mut diff = String::new();

    let mut a_idx = 1; // Where we are in `a`.
    let mut b_idx = 1; // Where we are in `b`.

    // TODO: print hunk headers; eg. @@ -44,6 +47,7 @@
    for edit in ses.0 {
        match edit {
            Delete(Idx(delete)) => {
                while a_idx < delete {
                    if delete < CONTEXT_LINES || a_idx >= delete - CONTEXT_LINES {
                        diff.push_str(&format!(" {}\n", &a[a_idx - 1]));
                    }
                    a_idx += 1;
                    b_idx += 1;
                }
                diff.push_str(&format!("{}-{}{}\n", RED, &a[a_idx - 1], RESET));
                a_idx += 1;
            }
            Insert(Idx(insert)) => {
                while b_idx < insert {
                    if insert < CONTEXT_LINES || b_idx >= insert - CONTEXT_LINES {
                        diff.push_str(&format!(" {}\n", &b[b_idx - 1]));
                    }
                    a_idx += 1;
                    b_idx += 1;
                }
                diff.push_str(&format!("{}+{}{}\n", GREEN, &b[b_idx - 1], RESET));
                b_idx += 1;
            }
        }
        // TODO: after each edit, may want to print up to three lines of context...
    }

    // BUG: there is almost certainly an off-by-one or a boundary violation hiding (ha!) in here
    for i in (a_idx - 1)..=a.len() {
        diff.push_str(&format!(" {}\n", &a[i - 1]));
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
