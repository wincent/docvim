use std::fs;

// TODO[future]: docvim_parser::vimscript::Parser;
use docvim_parser::lua::Parser;

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
