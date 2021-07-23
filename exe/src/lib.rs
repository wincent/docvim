use std::fs;

// TODO: would like this to be:
// docvim::lexer::Lexer -- and later:
// docvim::lexer::lua::Lexer -- because later will have vimscript::Lexer
// but this underscore pattern is what rustc_* itself usess
use docvim_lexer::Lexer;

pub fn run(args: Vec<String>) {
    // TODO: actual arg parsing
    let input = "sample/init.lua";

    let contents = fs::read_to_string(input).expect("unable to read file");

    println!("Text:\n{}", contents);

    let mut lexer = Lexer::new(&contents);
    loop {
        match lexer.next_token() {
            Ok(t) => println!("token: {:?}", t),
            Err(e) => {
                println!("error: {}", e);
                break;
            }
        }
    }
}
