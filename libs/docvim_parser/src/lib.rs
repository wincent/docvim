pub mod error;

mod markdown;
mod lua;
mod types;

pub use lua::LuaParser;
pub use markdown::MarkdownParser;

#[cfg(test)]
mod tests {
    #[test]
    fn blinking_light() {
        assert!(true);
    }
}
