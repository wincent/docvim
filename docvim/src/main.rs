use std::env;

use docvim::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    process("sample/init.lua")
}
