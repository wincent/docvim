use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    docvim::run(std::env::args().collect())
}
