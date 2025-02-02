pub mod error;
pub mod lexer;
pub mod lua;
pub mod markdown;
pub mod token;

mod peekable;

#[cfg(test)]
mod tests {
    #[test]
    fn blinking_light() {
        assert!(true);
    }
}
