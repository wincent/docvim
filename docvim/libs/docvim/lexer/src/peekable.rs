/// Wrapper around standard Peekable iterator that tracks position.
pub struct Peekable<I: std::iter::Iterator> {
    pub iter: std::iter::Peekable<I>,
    pub position: usize,
}

impl<I: std::iter::Iterator> Peekable<I> {
    pub fn new(iter: I) -> Self {
        Peekable {
            iter: iter.peekable(),
            position: 0,
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        self.iter.peek()
    }
}

impl<I: std::iter::Iterator> std::iter::Iterator for Peekable<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        let c = self.iter.next();
        match c {
            None => (),
            _ => {
                self.position += 1;
            }
        }
        c
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tracks_position() {
        let sample = "hello";
        let mut iter = Peekable::new(sample.chars());
        assert_eq!(iter.position, 0);
        assert_eq!(*iter.peek().unwrap(), 'h');
        assert_eq!(iter.position, 0);
        assert_eq!(iter.next(), Some('h'));
        assert_eq!(iter.position, 1);
        assert_eq!(iter.next(), Some('e'));
        assert_eq!(iter.position, 2);
        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.position, 3);
        assert_eq!(iter.next(), Some('l'));
        assert_eq!(iter.position, 4);
        assert_eq!(iter.next(), Some('o'));
        assert_eq!(iter.position, 5);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.position, 5);
    }
}
