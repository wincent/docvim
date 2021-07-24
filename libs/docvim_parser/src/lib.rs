use docvim_lexer::Lexer; // later on will want token types etc

// A note on storing str slices in AST nodes: given that Rust doesn't have fast access based on
// index, instead of storing indices into the original input, we actually copy the strings.

// TODO these node types will eventually wind up in another file, and end up referring specifically
// to Lua, but for now developing them "in situ".
pub struct Chunk {
    pub statments: Vec<Statement>,
}

pub enum Exp {
    Number
}

pub struct LocalDeclaration<'a> {
    pub namelist: Vec<Name<'a>>,
    pub explist: Vec<Exp>
}

pub struct Name<'a> {
    pub text: &'a str
}

pub struct Number<'a> {
    pub text: &'a str
}

pub enum Statement {
    LocalDeclaration,
}

pub struct Parser<'a> {
    input: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input
        }
    }

    pub fn parse(&self) {
        let mut lexer = Lexer::new(self.input);
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

mod tests {
    use super::*;

    #[test]
    fn parses_a_statement() {
        let ast = Parser::new("local x = 1").parse();

        // chunk [
        //   statement = LocalDeclaration,
        //   ...
        // ]
        //
        // where LocalDeclaration
        //      namelist = (optional) explist
        //
        // where namelist = [Name, ...]
        //       explist = [exp, ...]
        //
        // where exp = lots of things, but we'll start with... number
    }
}
