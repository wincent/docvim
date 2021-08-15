use std::cmp::min;
use std::fmt::Display;

use crate::diff::*;
use Edit::*;

const CONTEXT_LINES: usize = 3;
const CYAN: &str = "\x1b[0;36m";
const GREEN: &str = "\x1b[0;32m";
const RED: &str = "\x1b[0;31m";
const RESET: &str = "\x1b[0m";

pub fn format<T>(ses: Diff, a: &Vec<T>, b: &Vec<T>) -> String
where
    T: Display + PartialEq,
{
    let mut diff = String::new();
    let mut hunk = String::new();
    let mut a_idx = 1;
    let mut b_idx = 1;
    let mut a_hunk_start = None;
    let mut b_hunk_start = None;
    let a_len = a.len();
    let b_len = b.len();

    for edit in ses.0 {
        match edit {
            Delete(Idx(delete)) => {
                if delete > a_idx + CONTEXT_LINES {
                    // Print trailing context.
                    let last_context_line = min(a_idx + CONTEXT_LINES, a_len);
                    while a_idx <= last_context_line {
                        hunk.push_str(&format!(" {}\n", &a[a_idx - 1]));
                        a_idx += 1;
                        b_idx += 1;
                    }
                    // BUG: almost certainly have off-by-ones in here
                    diff.push_str(&format!(
                        "{}@@ -{},{} +{},{} @@{}\n",
                        CYAN,
                        a_hunk_start.unwrap(),
                        a_idx - a_hunk_start.unwrap(),
                        b_hunk_start.unwrap(),
                        b_idx - b_hunk_start.unwrap(),
                        RESET
                    ));
                    diff.push_str(&hunk);
                    hunk.clear();
                    a_hunk_start = None;
                    b_hunk_start = None;
                }

                if a_idx < delete {
                    // Print leading context.
                    if a_hunk_start.is_none() {
                        if delete <= CONTEXT_LINES {
                            a_hunk_start = Some(1);
                            b_hunk_start = Some(1);
                        } else if a_idx >= delete - CONTEXT_LINES {
                            a_hunk_start = Some(a_idx);
                            b_hunk_start = Some(b_idx);
                        } else {
                            let skip = (delete - CONTEXT_LINES) - a_idx;
                            a_hunk_start = Some(a_idx + skip);
                            b_hunk_start = Some(b_idx + skip);
                        }
                    }
                    while a_idx < delete {
                        if delete < CONTEXT_LINES || a_idx >= delete - CONTEXT_LINES {
                            hunk.push_str(&format!(" {}\n", &a[a_idx - 1]));
                        }
                        a_idx += 1;
                        b_idx += 1;
                    }
                }
                hunk.push_str(&format!("{}-{}{}\n", RED, &a[a_idx - 1], RESET));
                a_idx += 1;
            }
            Insert(Idx(insert)) => {
                if insert > b_idx + CONTEXT_LINES {
                    // Print trailing context.
                    let last_context_line = min(b_idx + CONTEXT_LINES, b_len);
                    while b_idx <= last_context_line {
                        hunk.push_str(&format!(" {}\n", &b[b_idx - 1]));
                        a_idx += 1;
                        b_idx += 1;
                    }
                    diff.push_str(&format!(
                        "{}@@ -{},{} +{},{} @@{}\n",
                        CYAN,
                        a_hunk_start.unwrap(),
                        a_idx - a_hunk_start.unwrap(),
                        b_hunk_start.unwrap(),
                        b_idx - b_hunk_start.unwrap(),
                        RESET
                    ));
                    diff.push_str(&hunk);
                    hunk.clear();
                    a_hunk_start = None;
                    b_hunk_start = None;
                }

                if b_idx < insert {
                    // Print leading context.
                    if b_hunk_start.is_none() {
                        if insert <= CONTEXT_LINES {
                            a_hunk_start = Some(1);
                            b_hunk_start = Some(1);
                        } else if b_idx >= insert - CONTEXT_LINES {
                            a_hunk_start = Some(a_idx);
                            b_hunk_start = Some(b_idx);
                        } else {
                            let skip = (insert - CONTEXT_LINES) - b_idx;
                            a_hunk_start = Some(a_idx + skip);
                            b_hunk_start = Some(b_idx + skip);
                        }
                    }
                    while b_idx < insert {
                        if insert < CONTEXT_LINES || b_idx >= insert - CONTEXT_LINES {
                            hunk.push_str(&format!(" {}\n", &b[b_idx - 1]));
                        }
                        a_idx += 1;
                        b_idx += 1;
                    }
                }
                hunk.push_str(&format!("{}+{}{}\n", GREEN, &b[b_idx - 1], RESET));
                b_idx += 1;
            }
        }
    }
    // Flush any pending hunks.
    if hunk.len() > 0 {
        // Append trailing context lines before flushing.
        let last_context_line = min(a_idx + CONTEXT_LINES, a_len);
        while a_idx <= last_context_line {
            hunk.push_str(&format!(" {}\n", &a[a_idx - 1]));
            a_idx += 1;
            b_idx += 1;
        }
        diff.push_str(&format!(
            "{}@@ -{},{} +{},{} @@{}\n",
            CYAN,
            a_hunk_start.unwrap(),
            a_idx - a_hunk_start.unwrap(),
            b_hunk_start.unwrap(),
            b_idx - b_hunk_start.unwrap(),
            RESET
        ));
        diff.push_str(&hunk);
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
