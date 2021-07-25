use std::str::CharIndices;

/// Wrapper around standard Peekable iterator that tracks position as a char index (number of
/// Unicode "chars" returned so far) and byte index (number of 8-bit UTF-8 bytes returned so far,
/// suitable for indexing into a string slice).
pub struct Peekable<'a> {
    pub byte_idx: usize,
    pub char_idx: usize,
    iter: std::iter::Peekable<CharIndices<'a>>,
}

impl<'a> Peekable<'a> {
    pub fn new(input: &'a str) -> Self {
        Peekable {
            byte_idx: 0,
            char_idx: 0,
            iter: input.char_indices().peekable(),
        }
    }

    // BUG: Technically, peek is supposed to return a reference to the item type (ie.
    // `Option<&char>` instead of `Option<char>`), but I couldn't figure out how to do that; see:
    // https://doc.rust-lang.org/stable/std/iter/struct.Peekable.html
    pub fn peek(&mut self) -> Option<char> {
        match self.iter.peek() {
            Some(&(_, ch)) => Some(ch),
            None => None,
        }
    }
}

impl<'a> std::iter::Iterator for Peekable<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        match self.iter.next() {
            Some((i, ch)) => {
                self.char_idx += 1;
                self.byte_idx = i + ch.len_utf8();
                Some(ch)
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tracks_byte_idx() {
        let mut iter = Peekable::new("cañón");
        assert_eq!(iter.byte_idx, 0);
        assert_eq!(iter.peek().unwrap(), 'c');
        assert_eq!(iter.byte_idx, 0);
        assert_eq!(iter.next(), Some('c'));
        assert_eq!(iter.byte_idx, 1);
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.byte_idx, 2);
        assert_eq!(iter.next(), Some('ñ'));
        assert_eq!(iter.byte_idx, 4);
        assert_eq!(iter.next(), Some('ó'));
        assert_eq!(iter.byte_idx, 6);
        assert_eq!(iter.next(), Some('n'));
        assert_eq!(iter.byte_idx, 7);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.byte_idx, 7);
    }

    #[test]
    fn tracks_char_idx() {
        let mut iter = Peekable::new("cañón");
        assert_eq!(iter.char_idx, 0);
        assert_eq!(iter.peek().unwrap(), 'c');
        assert_eq!(iter.char_idx, 0);
        assert_eq!(iter.next(), Some('c'));
        assert_eq!(iter.char_idx, 1);
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.char_idx, 2);
        assert_eq!(iter.next(), Some('ñ'));
        assert_eq!(iter.char_idx, 3);
        assert_eq!(iter.next(), Some('ó'));
        assert_eq!(iter.char_idx, 4);
        assert_eq!(iter.next(), Some('n'));
        assert_eq!(iter.char_idx, 5);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.char_idx, 5);
    }
}
