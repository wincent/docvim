use std::fs;

// TODO: would like this to be:
// docvim::lexer::Lexer -- and later:
// docvim::lexer::lua::Lexer -- because later will have vimscript::Lexer
// but this underscore pattern is what rustc_* itself usess
// use docvim_lexer::Lexer;
use docvim_parser::Parser;

pub fn run(args: Vec<String>) {
    // TODO: actual arg parsing
    println!("Args: {:?}", args);
    let input = "sample/init.lua";

    let contents = fs::read_to_string(input).expect("unable to read file");

    println!("Text:\n{}", contents);

    let parser = Parser::new(&contents);

    // TODO: pretty print this error
    parser.parse().expect("Failed to parse");
}
