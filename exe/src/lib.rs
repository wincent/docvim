use std::fs;

// TODO[future]: docvim_parser::vimscript::Parser;
use docvim_parser::lua::Parser;

/*
docvim - a documentation generator for Vim plug-ins

Usage: docvim [--version] [OUTFILES...] [-d|--debug] [-c|--directory DIRECTORY]
              [-v|--verbose]
  Generate documentation for a Vim plug-in

Available options:
  -h,--help                Show this help text
  --version                Print version information
  OUTFILES...              Target file(s) for generated output (default:
                           standard output)
  -d,--debug               Print debug information during processing
  -c,--directory DIRECTORY Change to DIRECTORY before processing (default: ".")
  -v,--verbose             Be verbose during processing
*/

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
