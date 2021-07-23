use docvim_lexer::Lexer; // later on will want token types etc

pub struct Parser {
}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(&self, input: &str) {
        let mut lexer = Lexer::new(&input);
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
}
