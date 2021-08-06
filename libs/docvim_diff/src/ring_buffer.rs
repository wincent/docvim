use std::ops::{Index, IndexMut};

/// The Myers paper specifies an array (`V[-MAX..MAX]`) that allows negative indices, so we
/// substitute a ring buffer for that.
#[derive(Clone, Debug)]
pub struct RingBuffer {
    capacity: usize,
    // TODO: try and make this generic, maybe, once this is resolved:
    // https://github.com/rust-lang/rust/issues/52662
    storage: Vec<usize>,
}

impl RingBuffer {
    pub fn new(capacity: usize) -> Self {
        RingBuffer { capacity, storage: vec![0; capacity] }
    }

    pub fn clear(&mut self, max: usize) {
        let count = max * 2 + 1;
        if count > self.capacity {
            panic!("cannot clear max that would exceed capacity");
        }
        let slice = &mut self.storage[0..count];
        slice.fill(0);
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
}
