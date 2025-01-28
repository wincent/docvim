use crate::error::*;

// Lexer token types are imported aliased (all with a "Token" suffix) to avoid collisions with
// parser node types of the same name.
use docvim_lexer::lua::CommentKind::BlockComment as BlockCommentToken;
use docvim_lexer::lua::CommentKind::LineComment as LineCommentToken;
use docvim_lexer::lua::KeywordKind::And as AndToken;
use docvim_lexer::lua::KeywordKind::Break as BreakToken;
use docvim_lexer::lua::KeywordKind::Do as DoToken;
use docvim_lexer::lua::KeywordKind::Else as ElseToken;
use docvim_lexer::lua::KeywordKind::Elseif as ElseifToken;
use docvim_lexer::lua::KeywordKind::End as EndToken;
use docvim_lexer::lua::KeywordKind::False as FalseToken;
use docvim_lexer::lua::KeywordKind::For as ForToken;
use docvim_lexer::lua::KeywordKind::Function as FunctionToken;
use docvim_lexer::lua::KeywordKind::If as IfToken;
use docvim_lexer::lua::KeywordKind::In as InToken;
use docvim_lexer::lua::KeywordKind::Local as LocalToken;
use docvim_lexer::lua::KeywordKind::Nil as NilToken;
use docvim_lexer::lua::KeywordKind::Not as NotToken;
use docvim_lexer::lua::KeywordKind::Or as OrToken;
use docvim_lexer::lua::KeywordKind::Repeat as RepeatToken;
use docvim_lexer::lua::KeywordKind::Return as ReturnToken;
use docvim_lexer::lua::KeywordKind::Then as ThenToken;
use docvim_lexer::lua::KeywordKind::True as TrueToken;
use docvim_lexer::lua::KeywordKind::Until as UntilToken;
use docvim_lexer::lua::KeywordKind::While as WhileToken;
use docvim_lexer::lua::LiteralKind::Number as NumberToken;
use docvim_lexer::lua::LiteralKind::Str as StrToken;
use docvim_lexer::lua::NameKind::Identifier as IdentifierToken;
use docvim_lexer::lua::NameKind::Keyword as KeywordToken;
use docvim_lexer::lua::OpKind::Assign as AssignToken;
use docvim_lexer::lua::OpKind::Caret as CaretToken;
use docvim_lexer::lua::OpKind::Concat as ConcatToken;
use docvim_lexer::lua::OpKind::Eq as EqToken;
use docvim_lexer::lua::OpKind::Gt as GtToken;
use docvim_lexer::lua::OpKind::Gte as GteToken;
use docvim_lexer::lua::OpKind::Hash as HashToken;
use docvim_lexer::lua::OpKind::Lt as LtToken;
use docvim_lexer::lua::OpKind::Lte as LteToken;
use docvim_lexer::lua::OpKind::Minus as MinusToken;
use docvim_lexer::lua::OpKind::Ne as NeToken;
use docvim_lexer::lua::OpKind::Percent as PercentToken;
use docvim_lexer::lua::OpKind::Plus as PlusToken;
use docvim_lexer::lua::OpKind::Slash as SlashToken;
use docvim_lexer::lua::OpKind::Star as StarToken;
use docvim_lexer::lua::OpKind::Vararg as VarargToken;
use docvim_lexer::lua::PunctuatorKind::Colon as ColonToken;
use docvim_lexer::lua::PunctuatorKind::Comma as CommaToken;
use docvim_lexer::lua::PunctuatorKind::Dot as DotToken;
use docvim_lexer::lua::PunctuatorKind::Lbracket as LbracketToken;
use docvim_lexer::lua::PunctuatorKind::Lcurly as LcurlyToken;
use docvim_lexer::lua::PunctuatorKind::Lparen as LparenToken;
use docvim_lexer::lua::PunctuatorKind::Rbracket as RbracketToken;
use docvim_lexer::lua::PunctuatorKind::Rcurly as RcurlyToken;
use docvim_lexer::lua::PunctuatorKind::Rparen as RparenToken;
use docvim_lexer::lua::PunctuatorKind::Semi as SemiToken;
use docvim_lexer::lua::StrKind::DoubleQuoted as DoubleQuotedToken;
use docvim_lexer::lua::StrKind::Long as LongToken;
use docvim_lexer::lua::StrKind::SingleQuoted as SingleQuotedToken;
use docvim_lexer::lua::TokenKind::Comment as CommentToken;
use docvim_lexer::lua::TokenKind::Literal as LiteralToken;
use docvim_lexer::lua::TokenKind::Name as NameToken;
use docvim_lexer::lua::TokenKind::Op as OpToken;
use docvim_lexer::lua::TokenKind::Punctuator as PunctuatorToken;
use docvim_lexer::lua::{Lexer, Token};

// TODO these node types will eventually wind up in another file, and end up referring specifically
// to Lua, but for now developing them "in situ".

// TODO: rename these to reflect operation instead of token; eg "Add" insted of "Plus", "Multiply"
// instead of "Star", "Divide" instead of "Slash" etc.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOp {
    And,
    Caret,
    Concat,
    Eq,
    Gt,
    Gte,
    Lt,
    Lte,
    Minus,
    Ne,
    Or,
    Percent,
    Plus,
    Slash,
    Star,
}

#[derive(Debug, PartialEq)]
pub struct Node<'a, T> {
    pub comments: Vec<Comment<'a>>,
    pub node: T,
}

pub type Statement<'a> = Node<'a, StatementKind<'a>>;

#[derive(Debug, PartialEq)]
pub struct Block<'a>(Vec<Statement<'a>>);

#[derive(Debug, PartialEq)]
pub struct Comment<'a> {
    pub content: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum Exp<'a> {
    Binary {
        lexp: Box<Exp<'a>>,
        op: BinOp,
        rexp: Box<Exp<'a>>,
    },

    /// See `cook_str()` for a description of how a "raw" string becomes "cooked".
    CookedStr(Box<String>),

    False,
    FunctionCall {
        /// The "prefix expression" (ie. LHS in `foo(bar)`; that is, `foo`), which is the function
        /// to be called.
        pexp: Box<Exp<'a>>,
        args: Vec<Exp<'a>>,
    },
    Function {
        parlist: Vec<Name<'a>>,
        varargs: bool,
        block: Block<'a>,
    },
    Index {
        /// The "prefix expression" (ie. LHS in `foo[bar]; that is, `foo`), which is the table to
        /// be indexed.
        // TODO: decide whether we should type this more narrowly (bc prefixexp is a subset of Exp)...
        pexp: Box<Exp<'a>>,
        /// The "key expression" (ie. RHS in `foo[bar]`; that is, `bar`).
        kexp: Box<Exp<'a>>,
    },
    MethodCall {
        /// The "prefix expression" (ie. LHS in `foo:bar(baz)`; that is, `foo`), which is the table
        /// containing the method to be called.
        pexp: Box<Exp<'a>>,

        /// The method name (ie. the `bar` in `foo:bar(baz`) to be called.
        name: &'a str,
        args: Vec<Exp<'a>>,
    },
    NamedVar(&'a str),
    Nil,
    Number(&'a str),
    RawStr(&'a str),
    Table(Vec<Field<'a>>),
    True,
    Unary {
        exp: Box<Exp<'a>>,
        op: UnOp,
    },
    Varargs,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnOp {
    Length,
    Minus,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct Field<'a> {
    index: Option<usize>,
    lexp: Box<Exp<'a>>,
    rexp: Box<Exp<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Table<'a>(Vec<Field<'a>>);

#[derive(Debug, PartialEq)]
pub struct Name<'a>(&'a str);

#[derive(Debug, PartialEq)]
pub struct Consequent<'a> {
    cexp: Box<Exp<'a>>,
    block: Block<'a>,
}

#[derive(Debug, PartialEq)]
pub enum StatementKind<'a> {
    // TODO: think about naming consistency here (some enum variants end in "Statement", others don't etc)
    Break,
    DoBlock(Block<'a>),
    ForIn {
        namelist: Vec<Name<'a>>,
        explist: Vec<Exp<'a>>,
        block: Block<'a>,
    },
    For {
        name: Name<'a>,
        startexp: Box<Exp<'a>>,
        endexp: Box<Exp<'a>>,
        stepexp: Option<Box<Exp<'a>>>,
        block: Block<'a>,
    },

    // Ugh... is there a better way to do this? (ie. embed Exp::FunctionCall directly instead of duplicating the fields?)
    // Same for MethodCallStatement below.
    FunctionCallStatement {
        pexp: Box<Exp<'a>>,
        args: Vec<Exp<'a>>,
    },
    FunctionDeclaration {
        /// A name may consist of a single Name (eg. "foo") or multiple via property access (eg.
        /// "foo.bar", "foo.bar.baz" etc).
        name: Vec<Name<'a>>,

        /// A name may optionally terminate with a final "method" component (eg. "bar" in
        /// "foo:bar").
        method: Option<Name<'a>>,

        /// Named parameters (eg. "a" and "b" in "(a, b)").
        parlist: Vec<Name<'a>>,

        /// Does the function accept varags (eg. "...")?
        varargs: bool,
        block: Block<'a>,
    },
    IfStatement {
        consequents: Vec<Consequent<'a>>,
        alternate: Option<Block<'a>>,
    },
    LocalDeclaration {
        namelist: Vec<Name<'a>>,
        explist: Vec<Exp<'a>>,
    },
    LocalFunctionDeclaration {
        name: Name<'a>,
        parlist: Vec<Name<'a>>,
        varargs: bool,
        block: Block<'a>,
    },
    MethodCallStatement {
        pexp: Box<Exp<'a>>,
        name: &'a str,
        args: Vec<Exp<'a>>,
    },

    Repeat {
        block: Block<'a>,
        cexp: Box<Exp<'a>>,
    },
    Return(Option<Vec<Exp<'a>>>),
    // TODO: explore stricter typing for this; not all Exp are legit var values
    VarlistDeclaration {
        varlist: Vec<Exp<'a>>,
        explist: Vec<Exp<'a>>,
    },
    While {
        cexp: Box<Exp<'a>>,
        block: Block<'a>,
    },
}

/// Returns the left and right "binding" power for a given operator, which enables us to parse
/// binary and unary expressions with left or right associativity via Pratt's "Top Down Operator
/// Precedence" algorithm, as described in doc/lua.md.
///
/// See also unop_binding.
fn binop_binding(op: BinOp) -> (u8, u8) {
    match op {
        BinOp::Or => (1, 2),
        BinOp::And => (3, 4),
        BinOp::Eq => (5, 6),
        BinOp::Gt => (5, 6),
        BinOp::Gte => (5, 6),
        BinOp::Lt => (5, 6),
        BinOp::Lte => (5, 6),
        BinOp::Ne => (5, 6),
        BinOp::Concat => (8, 7), // Right-associative.
        BinOp::Plus => (9, 10),
        BinOp::Minus => (11, 12),
        BinOp::Percent => (13, 14),
        BinOp::Slash => (13, 14),
        BinOp::Star => (13, 14),
        BinOp::Caret => (18, 17), // Right-associative.
    }
}

fn unop_binding(op: UnOp) -> u8 {
    match op {
        UnOp::Length => 15,
        UnOp::Minus => 15,
        UnOp::Not => 15,
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    comments: Vec<Comment<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { lexer: Lexer::new(input), comments: vec![] }
    }

    pub fn pretty_error(&self, error: ParserError) -> String {
        let mut iter = self.lexer.input.char_indices();
        let mut column = 1;
        let mut line = 1;
        let mut context = String::new();
        let mut found = false;
        while let Some((idx, ch)) = iter.next() {
            if idx == error.position {
                found = true;
            }
            match ch {
                '\n' => {
                    if found {
                        break;
                    } else {
                        line = line + 1;
                        column = 1;
                        context.clear();
                    }
                }
                _ => {
                    if !found {
                        column = column + 1;
                    }
                    context.push(ch);
                }
            }
        }

        let mut output = String::new();
        let line_str = line.to_string();
        if line > 1 {
            output.push_str(&(" ".repeat(line_str.len())));
            output.push_str(" |");
            output.push('\n');
        }
        output.push_str(&line_str);
        output.push_str(" | ");
        output.push_str(&context);
        output.push('\n');
        output.push_str(&(" ".repeat(line_str.len())));
        output.push_str(" | ");
        output.push_str(&(" ".repeat(column - 1)));
        output.push_str(&format!("^ {:} at {:}:{:}", error.kind.to_str(), line, column));
        output.push('\n');
        output
    }

    fn unexpected_end_of_input(&self) -> ParserError {
        return ParserError {
            kind: ParserErrorKind::UnexpectedEndOfInput,
            position: self.lexer.input.chars().count(),
        };
    }

    pub fn parse(&mut self) -> Result<Block, ParserError> {
        self.parse_block()
        // TODO: if there are pending comments here, need to emit them as trailing comments...
    }

    fn parse_block(&mut self) -> Result<Block<'a>, ParserError> {
        let mut block = Block(vec![]);
        loop {
            match self.lexer.tokens.peek() {
                Some(&Ok(Token { kind: NameToken(KeywordToken(DoToken)), .. })) => {
                    block.0.push(self.parse_do_block()?);
                    self.slurp(PunctuatorToken(SemiToken));
                }
                Some(&Ok(Token {
                    kind: NameToken(KeywordToken(ElseToken | ElseifToken | EndToken | UntilToken)),
                    ..
                })) => {
                    break;
                }
                Some(&Ok(Token { kind: NameToken(KeywordToken(ForToken)), .. })) => {
                    block.0.push(self.parse_for()?);
                    self.slurp(PunctuatorToken(SemiToken));
                }
                Some(&Ok(Token { kind: NameToken(KeywordToken(FunctionToken)), .. })) => {
                    block.0.push(self.parse_function()?);
                    self.slurp(PunctuatorToken(SemiToken));
                }
                Some(&Ok(Token { kind: NameToken(KeywordToken(IfToken)), .. })) => {
                    block.0.push(self.parse_if()?);
                    self.slurp(PunctuatorToken(SemiToken));
                }
                Some(&Ok(Token { kind: NameToken(KeywordToken(LocalToken)), .. })) => {
                    block.0.push(self.parse_local()?);
                    self.slurp(PunctuatorToken(SemiToken));
                }
                Some(&Ok(Token { kind: NameToken(KeywordToken(RepeatToken)), .. })) => {
                    block.0.push(self.parse_repeat()?);
                    self.slurp(PunctuatorToken(SemiToken));
                }
                Some(&Ok(Token { kind: NameToken(KeywordToken(WhileToken)), .. })) => {
                    block.0.push(self.parse_while()?);
                    self.slurp(PunctuatorToken(SemiToken));
                }
                Some(&Ok(token @ Token { kind: NameToken(IdentifierToken), .. })) => {
                    // prefixexp ::= var | functioncall | `(´ exp `)´
                    // functioncall ::=  prefixexp args | prefixexp `:´ Name args
                    let comments = std::mem::take(&mut self.comments);
                    let pexp = self.parse_prefixexp()?;
                    match pexp {
                        Exp::FunctionCall { pexp, args } => {
                            block.0.push(self.make_node(
                                comments,
                                StatementKind::FunctionCallStatement { pexp, args },
                            ));
                            self.slurp(PunctuatorToken(SemiToken));
                        }
                        Exp::MethodCall { pexp, name, args } => {
                            block.0.push(self.make_node(
                                comments,
                                StatementKind::MethodCallStatement { pexp, name, args },
                            ));
                            self.slurp(PunctuatorToken(SemiToken));
                        }
                        Exp::NamedVar(_) | Exp::Index { .. } => {
                            // varlist `=´ explist
                            // varlist ::= var {`,´ var}
                            // var ::=  Name | prefixexp `[´ exp `]´ | prefixexp `.´ Name
                            let mut varlist = vec![pexp];
                            let mut allow_comma = true;
                            loop {
                                match self.lexer.tokens.peek() {
                                    Some(&Ok(Token { kind: OpToken(AssignToken), .. })) => {
                                        self.lexer.tokens.next();
                                        let explist = self.parse_explist()?;
                                        block.0.push(self.make_node(
                                            comments,
                                            StatementKind::VarlistDeclaration { varlist, explist },
                                        ));
                                        self.slurp(PunctuatorToken(SemiToken));
                                        break;
                                    }
                                    Some(&Ok(Token {
                                        kind: PunctuatorToken(CommaToken),
                                        char_start,
                                        ..
                                    })) => {
                                        self.lexer.tokens.next();
                                        if allow_comma {
                                            allow_comma = false;
                                        } else {
                                            return Err(ParserError {
                                                kind: ParserErrorKind::UnexpectedComma,
                                                position: char_start,
                                            });
                                        }
                                    }
                                    Some(&Ok(Token {
                                        kind: PunctuatorToken(SemiToken),
                                        char_start,
                                        ..
                                    })) => {
                                        return Err(ParserError {
                                            kind: ParserErrorKind::UnexpectedToken,
                                            position: char_start,
                                        });
                                    }
                                    Some(&Ok(Token { .. })) => {
                                        let pexp = self.parse_prefixexp()?;
                                        match pexp {
                                            Exp::NamedVar(_) | Exp::Index { .. } => {
                                                varlist.push(pexp);
                                            }
                                            _ => {
                                                return Err(ParserError {
                                                    kind: ParserErrorKind::UnexpectedToken,
                                                    position: token.char_start,
                                                })
                                            }
                                        }
                                        allow_comma = true;
                                    }
                                    Some(&Err(err)) => return Err(ParserError::from(err)),
                                    None => return Err(self.unexpected_end_of_input()),
                                }
                            }
                        }

                        // TODO: might want to come up with a better error than this
                        _ => {
                            return Err(ParserError {
                                kind: ParserErrorKind::UnexpectedToken,
                                position: token.char_start,
                            })
                        }
                    }
                }
                Some(&Ok(Token { kind: NameToken(KeywordToken(BreakToken)), .. })) => {
                    let comments = std::mem::take(&mut self.comments);
                    self.lexer.tokens.next();
                    block.0.push(self.make_node(comments, StatementKind::Break));
                    self.slurp(PunctuatorToken(SemiToken));
                    break;
                }
                Some(&Ok(Token { kind: NameToken(KeywordToken(ReturnToken)), .. })) => {
                    let comments = std::mem::take(&mut self.comments);
                    self.lexer.tokens.next();
                    let explist = if self.peek_exp() { Some(self.parse_explist()?) } else { None };
                    block.0.push(self.make_node(comments, StatementKind::Return(explist)));
                    self.slurp(PunctuatorToken(SemiToken));
                    break;
                }
                Some(&Ok(Token {
                    kind: CommentToken(BlockCommentToken | LineCommentToken),
                    byte_start,
                    byte_end,
                    ..
                })) => {
                    let content = &self.lexer.input[byte_start..byte_end];
                    self.comments.push(Comment { content });
                    self.lexer.tokens.next();
                }
                Some(&Ok(token @ Token { .. })) => {
                    // TODO: include token UnexpectedToken error message
                    return Err(ParserError {
                        kind: ParserErrorKind::UnexpectedToken,
                        position: token.char_start,
                    });
                }
                Some(&Err(err)) => return Err(ParserError::from(err)),
                None => break,
            }
        }
        Ok(block)
    }

    fn make_node<T>(&mut self, comments: Vec<Comment<'a>>, node: T) -> Node<'a, T> {
        let node = Node { comments, node };
        node
    }

    fn parse_do_block(&mut self) -> Result<Statement<'a>, ParserError> {
        let comments = std::mem::take(&mut self.comments);
        self.consume(NameToken(KeywordToken(DoToken)))?;
        let block = self.parse_block()?;
        self.consume(NameToken(KeywordToken(EndToken)))?;
        Ok(self.make_node(comments, StatementKind::DoBlock(block)))
    }

    fn parse_name(&mut self) -> Result<Name<'a>, ParserError> {
        match self.lexer.tokens.next() {
            Some(Ok(token @ Token { kind: NameToken(IdentifierToken), .. })) => {
                Ok(Name(&self.lexer.input[token.byte_start..token.byte_end]))
            }
            Some(Ok(token)) => Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken,
                position: token.char_start,
            }),
            Some(Err(err)) => Err(ParserError::from(err)),
            None => Err(self.unexpected_end_of_input()),
        }
    }

    /// Returns a list of named parameters, and boolean to indicate whether the list terminates
    /// with vararg syntax ("...").
    fn parse_parlist(&mut self) -> Result<(Vec<Name<'a>>, bool), ParserError> {
        self.consume(PunctuatorToken(LparenToken))?;
        let mut namelist = vec![];
        if self.slurp(PunctuatorToken(RparenToken)) {
            return Ok((namelist, false));
        }
        let mut expect_name = true;
        loop {
            match self.lexer.tokens.peek() {
                Some(&Ok(token @ Token { kind: NameToken(IdentifierToken), .. })) => {
                    if expect_name {
                        self.lexer.tokens.next();
                        namelist.push(Name(&self.lexer.input[token.byte_start..token.byte_end]));
                        expect_name = false;
                    } else {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedToken,
                            position: token.char_start,
                        });
                    }
                }
                Some(&Ok(token @ Token { kind: PunctuatorToken(CommaToken), .. })) => {
                    if expect_name {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedComma,
                            position: token.char_start,
                        });
                    } else {
                        self.lexer.tokens.next();
                        expect_name = true;
                    }
                }
                Some(&Ok(token @ Token { kind: PunctuatorToken(RparenToken), .. })) => {
                    if expect_name {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedToken,
                            position: token.char_start,
                        });
                    } else {
                        self.lexer.tokens.next();
                        return Ok((namelist, false));
                    }
                }
                Some(&Ok(token @ Token { kind: OpToken(VarargToken), .. })) => {
                    if expect_name {
                        self.lexer.tokens.next();
                        self.consume(PunctuatorToken(RparenToken))?;
                        return Ok((namelist, true));
                    } else {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedToken,
                            position: token.char_start,
                        });
                    }
                }
                Some(&Ok(token @ Token { .. })) => {
                    return Err(ParserError {
                        kind: ParserErrorKind::UnexpectedToken,
                        position: token.char_start,
                    });
                }
                Some(&Err(err)) => return Err(ParserError::from(err)),
                None => return Err(self.unexpected_end_of_input()),
            }
        }
    }

    fn parse_namelist(&mut self) -> Result<Vec<Name<'a>>, ParserError> {
        let mut namelist = vec![];
        let mut expect_name = true;
        loop {
            match self.lexer.tokens.peek() {
                Some(&Ok(token @ Token { kind: NameToken(IdentifierToken), .. })) => {
                    if expect_name {
                        self.lexer.tokens.next();
                        namelist.push(Name(&self.lexer.input[token.byte_start..token.byte_end]));
                        expect_name = false;
                    } else {
                        break;
                    }
                }
                Some(&Ok(token @ Token { kind: PunctuatorToken(CommaToken), .. })) => {
                    if expect_name {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedComma,
                            position: token.char_start,
                        });
                    } else {
                        self.lexer.tokens.next();
                        expect_name = true;
                    }
                }
                Some(&Ok(token @ Token { .. })) => {
                    if expect_name {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedToken,
                            position: token.char_start,
                        });
                    } else {
                        break;
                    }
                }
                Some(&Err(err)) => return Err(ParserError::from(err)),
                None => {
                    if expect_name {
                        return Err(self.unexpected_end_of_input());
                    } else {
                        break;
                    }
                }
            }
        }
        Ok(namelist)
    }

    fn parse_explist(&mut self) -> Result<Vec<Exp<'a>>, ParserError> {
        let mut explist = vec![];
        let mut allow_comma = false;
        loop {
            match self.lexer.tokens.peek() {
                Some(&Ok(Token { kind: PunctuatorToken(CommaToken), char_start, .. })) => {
                    if allow_comma {
                        self.lexer.tokens.next();
                        allow_comma = false;
                    } else {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedComma,
                            position: char_start,
                        });
                    }
                }
                Some(&Ok(Token {
                    kind:
                        LiteralToken(NumberToken)
                        | LiteralToken(StrToken(DoubleQuotedToken | SingleQuotedToken))
                        | LiteralToken(StrToken(LongToken { .. }))
                        | NameToken(IdentifierToken)
                        | NameToken(KeywordToken(
                            FalseToken | FunctionToken | NilToken | NotToken | TrueToken,
                        ))
                        | OpToken(HashToken | MinusToken | VarargToken)
                        | PunctuatorToken(LcurlyToken | LparenToken),
                    ..
                })) => {
                    if !allow_comma {
                        explist.push(self.parse_exp(0)?);
                        allow_comma = true;
                    } else {
                        break;
                    }
                }
                Some(&Ok(Token { kind: CommentToken(..), byte_start, byte_end, .. })) => {
                    let content = &self.lexer.input[byte_start..byte_end];
                    self.comments.push(Comment { content });
                    self.lexer.tokens.next();
                }
                Some(&Ok(_)) => {
                    break;
                }
                Some(&Err(err)) => return Err(ParserError::from(err)),
                None => {
                    if allow_comma {
                        break;
                    } else {
                        return Err(self.unexpected_end_of_input());
                    }
                }
            }
        }

        Ok(explist)
    }

    fn parse_for(&mut self) -> Result<Statement<'a>, ParserError> {
        let comments = std::mem::take(&mut self.comments);
        self.consume(NameToken(KeywordToken(ForToken)))?;
        let mut namelist = self.parse_namelist()?;

        if namelist.len() == 1 && self.slurp(OpToken(AssignToken)) {
            // Parse: for Name `=´ exp `,´ exp [`,´ exp] do block end
            let name = namelist.remove(0);
            let startexp = Box::new(self.parse_exp(0)?);
            self.consume(PunctuatorToken(CommaToken))?;
            let endexp = Box::new(self.parse_exp(0)?);
            let stepexp = if self.slurp(PunctuatorToken(CommaToken)) {
                Some(Box::new(self.parse_exp(0)?))
            } else {
                None
            };
            let block = self.parse_block()?;
            Ok(self
                .make_node(comments, StatementKind::For { name, startexp, endexp, stepexp, block }))
        } else {
            // Parse: for namelist in explist do block end
            self.consume(NameToken(KeywordToken(InToken)))?;
            let explist = self.parse_explist()?;
            self.consume(NameToken(KeywordToken(DoToken)))?;
            let block = self.parse_block()?;
            self.consume(NameToken(KeywordToken(EndToken)))?;
            Ok(self.make_node(comments, StatementKind::ForIn { namelist, explist, block }))
        }
    }

    fn parse_if(&mut self) -> Result<Statement<'a>, ParserError> {
        let comments = std::mem::take(&mut self.comments);
        self.consume(NameToken(KeywordToken(IfToken)))?;
        let cexp = Box::new(self.parse_exp(0)?);
        self.consume(NameToken(KeywordToken(ThenToken)))?;
        let block = self.parse_block()?;
        let mut consequents = vec![Consequent { cexp, block }];

        while self.slurp(NameToken(KeywordToken(ElseifToken))) {
            let cexp = Box::new(self.parse_exp(0)?);
            self.consume(NameToken(KeywordToken(ThenToken)))?;
            let block = self.parse_block()?;
            consequents.push(Consequent { cexp, block });
        }

        let alternate = match self.slurp(NameToken(KeywordToken(ElseToken))) {
            true => Some(self.parse_block()?),
            false => None,
        };

        self.consume(NameToken(KeywordToken(EndToken)))?;
        Ok(self.make_node(comments, StatementKind::IfStatement { consequents, alternate }))
    }

    fn parse_repeat(&mut self) -> Result<Statement<'a>, ParserError> {
        let comments = std::mem::take(&mut self.comments);
        self.consume(NameToken(KeywordToken(RepeatToken)))?;
        let block = self.parse_block()?;
        self.consume(NameToken(KeywordToken(UntilToken)))?;
        let cexp = Box::new(self.parse_exp(0)?);
        Ok(self.make_node(comments, StatementKind::Repeat { block, cexp }))
    }

    fn parse_while(&mut self) -> Result<Statement<'a>, ParserError> {
        let comments = std::mem::take(&mut self.comments);
        self.consume(NameToken(KeywordToken(WhileToken)))?;
        let cexp = Box::new(self.parse_exp(0)?);
        self.consume(NameToken(KeywordToken(DoToken)))?;
        let block = self.parse_block()?;
        self.consume(NameToken(KeywordToken(EndToken)))?;
        Ok(self.make_node(comments, StatementKind::While { cexp, block }))
    }

    fn parse_function(&mut self) -> Result<Statement<'a>, ParserError> {
        let comments = std::mem::take(&mut self.comments);
        self.consume(NameToken(KeywordToken(FunctionToken)))?;
        let mut name = vec![self.parse_name()?];
        while self.slurp(PunctuatorToken(DotToken)) {
            name.push(self.parse_name()?);
        }
        let method =
            if self.slurp(PunctuatorToken(ColonToken)) { Some(self.parse_name()?) } else { None };
        let (parlist, varargs) = self.parse_parlist()?;
        let block = self.parse_block()?;
        self.consume(NameToken(KeywordToken(EndToken)))?;
        Ok(self.make_node(
            comments,
            StatementKind::FunctionDeclaration { name, method, parlist, varargs, block },
        ))
    }

    fn parse_local(&mut self) -> Result<Statement<'a>, ParserError> {
        // Example inputs:
        //
        // local x
        // local x = 10
        // local x, y = 10, 20
        // local function Name funcbody
        let comments = std::mem::take(&mut self.comments);
        self.consume(NameToken(KeywordToken(LocalToken)))?;
        if self.slurp(NameToken(KeywordToken(FunctionToken))) {
            let name = self.parse_name()?;
            let (parlist, varargs) = self.parse_parlist()?;
            let block = self.parse_block()?;
            self.consume(NameToken(KeywordToken(EndToken)))?;
            Ok(self.make_node(
                comments,
                StatementKind::LocalFunctionDeclaration { name, parlist, varargs, block },
            ))
        } else {
            let namelist = self.parse_namelist()?;
            let explist =
                if self.slurp(OpToken(AssignToken)) { self.parse_explist()? } else { vec![] };
            Ok(self.make_node(comments, StatementKind::LocalDeclaration { explist, namelist }))
        }
    }

    /// A "cooked" string is one in which escape sequences have been replaced with their equivalent
    /// bytes. For example, the sequence "\n" is replaced with an actual newline, and so on.
    ///
    /// In contrast, a "raw" string preserves the exact form it had in the original source.
    fn cook_str(&self, token: Token) -> Result<Exp<'a>, ParserError> {
        let byte_start = token.byte_start + 1;
        let byte_end = token.byte_end - 1;
        let char_start = token.char_start + 1;
        let mut escaped = self.lexer.input[byte_start..byte_end].char_indices().peekable();
        let mut unescaped = String::with_capacity(byte_end - byte_start);
        while let Some((idx, ch)) = escaped.next() {
            let unescaped_char = match ch {
                '\\' => {
                    if let Some((_, next)) = escaped.next() {
                        match next {
                            'a' => '\x07', // bell
                            'b' => '\x08', // backspace
                            'f' => '\x0c', // line feed
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            'v' => '\x0b', // vertical tab
                            '\\' => '\\',
                            '"' => '"',
                            '\'' => '\'',
                            '\n' => '\n',
                            '0'..='9' => {
                                let mut digits = vec![next as u8];
                                // TODO: DRY this up; it is a bit similar to what we have in the
                                // lexer.
                                let mut digit_count = 1;
                                while digit_count < 3 {
                                    if let Some(&(_, digit)) = escaped.peek() {
                                        match digit {
                                            '0'..='9' => {
                                                digits.push(digit as u8);
                                                escaped.next();
                                                digit_count += 1;
                                            }
                                            _ => {
                                                break;
                                            }
                                        }
                                    } else {
                                        break;
                                    }
                                }
                                let number = std::str::from_utf8(&digits)
                                    .map_err(|_err| ParserError {
                                        kind: ParserErrorKind::Utf8Error,
                                        position: char_start + idx,
                                    })?
                                    .parse::<u8>()
                                    .map_err(|_err| ParserError {
                                        kind: ParserErrorKind::ParseIntError,
                                        position: char_start + idx,
                                    })?;
                                number as char
                            }
                            _ => {
                                return Err(ParserError {
                                    kind: ParserErrorKind::InvalidEscapeSequence,
                                    position: char_start + idx,
                                });
                            }
                        }
                    } else {
                        return Err(self.unexpected_end_of_input());
                    }
                }
                other => other,
            };
            unescaped.push(unescaped_char);
        }

        Ok(Exp::CookedStr(Box::new(unescaped)))
    }

    fn parse_unop_exp(&mut self, op: UnOp) -> Result<Exp<'a>, ParserError> {
        let bp = unop_binding(op);
        let rhs = self.parse_exp(bp)?;
        Ok(Exp::Unary { exp: Box::new(rhs), op })
    }

    /// args ::=  `(´ [explist] `)´ | tableconstructor | String
    fn parse_args(&mut self) -> Result<Vec<Exp<'a>>, ParserError> {
        match self.lexer.tokens.peek() {
            Some(&Ok(Token { kind: PunctuatorToken(LparenToken), .. })) => {
                self.lexer.tokens.next();
                let args = self.parse_explist()?;
                self.consume(PunctuatorToken(RparenToken))?;
                Ok(args)
            }
            Some(&Ok(Token { kind: PunctuatorToken(LcurlyToken), .. })) => {
                Ok(vec![self.parse_table_constructor()?])
            }
            Some(&Ok(Token { kind: LiteralToken(StrToken(DoubleQuotedToken)), .. }))
            | Some(&Ok(Token { kind: LiteralToken(StrToken(SingleQuotedToken)), .. }))
            | Some(&Ok(Token { kind: LiteralToken(StrToken(LongToken { .. })), .. })) => {
                Ok(vec![self.parse_exp(0)?])
            }
            // TODO: probably need to skip comments here too
            // Some(&Ok(Token { kind: CommentToken(_) .. })) => {
            // }
            Some(&Ok(Token { char_start, .. })) => {
                Err(ParserError { kind: ParserErrorKind::UnexpectedToken, position: char_start })
            }
            Some(&Err(err)) => Err(ParserError::from(err)),
            None => Err(self.unexpected_end_of_input()),
        }
    }

    /// ```ignore
    /// prefixexp = ( exp )
    ///           | Name
    ///           | prefixexp[exp]
    ///           | prefixexp.Name
    ///           | prefixexp "..." (functioncall)
    ///           | prefixexp {...} (functioncall)
    ///           | prefixexp (...) (functioncall)
    ///           | prefixexp:Name "..." (methodcall)
    ///           | prefixexp:Name {...} (methodcall)
    ///           | prefixexp:Name (...) (methodcall)
    /// ```
    fn parse_prefixexp(&mut self) -> Result<Exp<'a>, ParserError> {
        let mut pexp = match self.lexer.tokens.next() {
            Some(Ok(Token { kind: PunctuatorToken(LparenToken), .. })) => {
                let lhs = self.parse_exp(0)?;
                let token = self.lexer.tokens.next();
                match token {
                    Some(Ok(Token { kind: PunctuatorToken(RparenToken), .. })) => lhs,
                    Some(Ok(token)) => {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedToken,
                            position: token.char_start,
                        })
                    }
                    Some(Err(err)) => return Err(ParserError::from(err)),
                    None => return Err(self.unexpected_end_of_input()),
                }
            }
            Some(Ok(Token { kind: NameToken(IdentifierToken), byte_start, byte_end, .. })) => {
                Exp::NamedVar(&self.lexer.input[byte_start..byte_end])
            }
            Some(Ok(Token { char_start, .. })) => {
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken,
                    position: char_start,
                });
            }
            Some(Err(err)) => return Err(ParserError::from(err)),
            None => return Err(self.unexpected_end_of_input()),
        };
        loop {
            match self.lexer.tokens.peek() {
                Some(&Ok(Token { kind: PunctuatorToken(ColonToken), .. })) => {
                    // `prefixexp : Name args`
                    self.lexer.tokens.next();
                    let method_name = match self.lexer.tokens.next() {
                        Some(Ok(Token {
                            kind: NameToken(IdentifierToken),
                            byte_start,
                            byte_end,
                            ..
                        })) => &self.lexer.input[byte_start..byte_end],
                        Some(Ok(Token { char_start, .. })) => {
                            return Err(ParserError {
                                kind: ParserErrorKind::UnexpectedToken,
                                position: char_start,
                            });
                        }
                        Some(Err(err)) => return Err(ParserError::from(err)),
                        None => return Err(self.unexpected_end_of_input()),
                    };
                    pexp = Exp::MethodCall {
                        pexp: Box::new(pexp),
                        name: method_name,
                        args: self.parse_args()?,
                    };
                }
                Some(&Ok(Token { kind: PunctuatorToken(DotToken), .. })) => {
                    // ie. `foo.bar`, which is syntactic sugar for `foo['bar']`
                    self.lexer.tokens.next();
                    let kexp = match self.lexer.tokens.next() {
                        Some(Ok(Token {
                            kind: NameToken(IdentifierToken),
                            byte_start,
                            byte_end,
                            ..
                        })) => {
                            let name = &self.lexer.input[byte_start..byte_end];
                            Exp::RawStr(name)
                        }
                        Some(Ok(Token { char_start, .. })) => {
                            return Err(ParserError {
                                kind: ParserErrorKind::UnexpectedToken,
                                position: char_start,
                            });
                        }
                        Some(Err(err)) => return Err(ParserError::from(err)),
                        None => return Err(self.unexpected_end_of_input()),
                    };
                    pexp = Exp::Index { pexp: Box::new(pexp), kexp: Box::new(kexp) };
                }
                Some(&Ok(Token { kind: PunctuatorToken(LbracketToken), .. })) => {
                    // ie. `foo[bar]`
                    self.lexer.tokens.next();
                    let kexp = self.parse_exp(0)?;
                    self.consume(PunctuatorToken(RbracketToken))?;
                    pexp = Exp::Index { pexp: Box::new(pexp), kexp: Box::new(kexp) };
                }
                Some(&Ok(Token { kind: PunctuatorToken(LcurlyToken | LparenToken), .. }))
                | Some(&Ok(Token {
                    kind: LiteralToken(StrToken(DoubleQuotedToken | SingleQuotedToken)),
                    ..
                }))
                | Some(&Ok(Token { kind: LiteralToken(StrToken(LongToken { .. })), .. })) => {
                    pexp = Exp::FunctionCall { pexp: Box::new(pexp), args: self.parse_args()? };
                }
                _ => break,
            }
        }
        Ok(pexp)
    }

    /// See doc/lua.md for an explanation of `minimum_bp`.
    fn parse_exp(&mut self, minimum_bp: u8) -> Result<Exp<'a>, ParserError> {
        while let Some(&Ok(Token {
            kind: CommentToken(BlockCommentToken | LineCommentToken),
            byte_start,
            byte_end,
            ..
        })) = self.lexer.tokens.peek()
        {
            let content = &self.lexer.input[byte_start..byte_end];
            self.comments.push(Comment { content });
            self.lexer.tokens.next();
        }
        let mut lhs = match self.lexer.tokens.peek() {
            //
            // Primaries (literals etc).
            //
            Some(&Ok(Token { kind: NameToken(KeywordToken(FalseToken)), .. })) => {
                self.lexer.tokens.next();
                Exp::False
            }
            Some(&Ok(Token { kind: NameToken(KeywordToken(NilToken)), .. })) => {
                self.lexer.tokens.next();
                Exp::Nil
            }
            Some(&Ok(token @ Token { kind: LiteralToken(NumberToken), .. })) => {
                self.lexer.tokens.next();
                Exp::Number(&self.lexer.input[token.byte_start..token.byte_end])
            }
            Some(&Ok(
                token @ Token {
                    kind: LiteralToken(StrToken(DoubleQuotedToken | SingleQuotedToken)),
                    ..
                },
            )) => {
                self.lexer.tokens.next();
                self.cook_str(token)?
            }
            Some(&Ok(token @ Token { kind: LiteralToken(StrToken(LongToken { level })), .. })) => {
                self.lexer.tokens.next();

                // As a convenience, Lua omits any newline at position 0 in a long format string.
                let first = self.lexer.input.as_bytes()[token.byte_start + 2 + level];
                let start = if first == ('\n' as u8) {
                    token.byte_start + 2 + level + 1
                } else {
                    token.byte_start + 2 + level
                };
                let end = token.byte_end - 2 - level;
                Exp::RawStr(&self.lexer.input[start..end])
            }
            Some(&Ok(Token { kind: NameToken(KeywordToken(TrueToken)), .. })) => {
                self.lexer.tokens.next();
                Exp::True
            }
            Some(&Ok(Token { kind: OpToken(VarargToken), .. })) => {
                self.lexer.tokens.next();
                Exp::Varargs
            }
            Some(&Ok(Token {
                kind: NameToken(IdentifierToken) | PunctuatorToken(LparenToken),
                ..
            })) => self.parse_prefixexp()?,
            Some(&Ok(Token { kind: NameToken(KeywordToken(FunctionToken)), .. })) => {
                self.consume(NameToken(KeywordToken(FunctionToken)))?;
                let (parlist, varargs) = self.parse_parlist()?;
                let block = self.parse_block()?;
                self.consume(NameToken(KeywordToken(EndToken)))?;
                Exp::Function { parlist, varargs, block }
            }
            Some(&Ok(Token { kind: PunctuatorToken(LcurlyToken), .. })) => {
                self.parse_table_constructor()?
            }

            //
            // Unary operators.
            //
            Some(&Ok(Token { kind: NameToken(KeywordToken(NotToken)), .. })) => {
                self.lexer.tokens.next();
                self.parse_unop_exp(UnOp::Not)?
            }
            Some(&Ok(Token { kind: OpToken(HashToken), .. })) => {
                self.lexer.tokens.next();
                self.parse_unop_exp(UnOp::Length)?
            }
            Some(&Ok(Token { kind: OpToken(MinusToken), .. })) => {
                self.lexer.tokens.next();
                self.parse_unop_exp(UnOp::Minus)?
            }

            Some(&Ok(token)) => {
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken,
                    position: token.char_start,
                })
            }

            Some(&Err(err)) => return Err(ParserError::from(err)),

            None => return Err(self.unexpected_end_of_input()),
        };

        loop {
            let token = self.lexer.tokens.peek();
            let op = match token {
                Some(&Ok(Token { kind: NameToken(KeywordToken(AndToken)), .. })) => {
                    Some(BinOp::And)
                }
                Some(&Ok(Token { kind: NameToken(KeywordToken(OrToken)), .. })) => Some(BinOp::Or),
                Some(&Ok(Token { kind: OpToken(CaretToken), .. })) => Some(BinOp::Caret),
                Some(&Ok(Token { kind: OpToken(ConcatToken), .. })) => Some(BinOp::Concat),
                Some(&Ok(Token { kind: OpToken(EqToken), .. })) => Some(BinOp::Eq),
                Some(&Ok(Token { kind: OpToken(GtToken), .. })) => Some(BinOp::Gt),
                Some(&Ok(Token { kind: OpToken(GteToken), .. })) => Some(BinOp::Gte),
                Some(&Ok(Token { kind: OpToken(LtToken), .. })) => Some(BinOp::Lt),
                Some(&Ok(Token { kind: OpToken(LteToken), .. })) => Some(BinOp::Lte),
                Some(&Ok(Token { kind: OpToken(MinusToken), .. })) => Some(BinOp::Minus),
                Some(&Ok(Token { kind: OpToken(NeToken), .. })) => Some(BinOp::Ne),
                Some(&Ok(Token { kind: OpToken(PercentToken), .. })) => Some(BinOp::Percent),
                Some(&Ok(Token { kind: OpToken(PlusToken), .. })) => Some(BinOp::Plus),
                Some(&Ok(Token { kind: OpToken(SlashToken), .. })) => Some(BinOp::Slash),
                Some(&Ok(Token { kind: OpToken(StarToken), .. })) => Some(BinOp::Star),
                Some(&Ok(_)) => None, // No operator, we're done, will return lhs.
                Some(&Err(err)) => return Err(ParserError::from(err)),
                None => None, // End of input, will return lhs.
            };

            if let Some(op) = op {
                let (left_bp, right_bp) = binop_binding(op);
                if left_bp < minimum_bp {
                    break;
                } else {
                    self.lexer.tokens.next();
                    let rhs = self.parse_exp(right_bp)?;
                    lhs = Exp::Binary { lexp: Box::new(lhs), op, rexp: Box::new(rhs) };
                }
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    // fieldlist ::= field {fieldsep field} [fieldsep]
    // fieldsep ::= `,´ | `;´
    fn parse_table_constructor(&mut self) -> Result<Exp<'a>, ParserError> {
        match self.lexer.tokens.next() {
            Some(Ok(Token { kind: PunctuatorToken(LcurlyToken), .. })) => {
                let mut fieldsep_allowed = false;
                let mut fields = vec![];
                let mut index = 1;
                loop {
                    let token = self.lexer.tokens.peek();
                    match token {
                        Some(&Ok(Token { kind: PunctuatorToken(RcurlyToken), .. })) => {
                            self.lexer.tokens.next();
                            return Ok(Exp::Table(fields));
                        }
                        Some(&Ok(Token {
                            kind: PunctuatorToken(CommaToken | SemiToken),
                            char_start,
                            ..
                        })) => {
                            if fieldsep_allowed {
                                self.lexer.tokens.next();
                                fieldsep_allowed = false;
                            } else {
                                return Err(ParserError {
                                    kind: ParserErrorKind::UnexpectedFieldSeparator,
                                    position: char_start,
                                });
                            }
                        }
                        Some(&Ok(Token {
                            kind: CommentToken(BlockCommentToken | LineCommentToken),
                            byte_start,
                            byte_end,
                            ..
                        })) => {
                            let content = &self.lexer.input[byte_start..byte_end];
                            self.comments.push(Comment { content });
                            self.lexer.tokens.next(); // TODO: don't skip, accumulate
                        }
                        Some(&Ok(Token { .. })) => {
                            let field = self.parse_table_field(index)?;
                            if matches!(field, Field { index: Some(_), .. }) {
                                index += 1;
                            }
                            fields.push(field);
                            fieldsep_allowed = true;
                        }
                        Some(&Err(err)) => return Err(ParserError::from(err)),
                        None => return Err(self.unexpected_end_of_input()),
                    }
                }
            }
            Some(Ok(Token { char_start, .. })) => {
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedToken,
                    position: char_start,
                })
            }
            Some(Err(err)) => return Err(ParserError::from(err)),
            None => return Err(self.unexpected_end_of_input()),
        }
    }

    /// Consume the specified TokenKind, returning an Err if not found.
    ///
    /// Contrast with `slurp()`, which will consume a token if it is present, but does not error.
    fn consume(&mut self, kind: docvim_lexer::lua::TokenKind) -> Result<Token, ParserError> {
        match self.lexer.tokens.next() {
            Some(Ok(token @ Token { .. })) => {
                if token.kind == kind {
                    Ok(token)
                } else {
                    Err(ParserError {
                        kind: ParserErrorKind::UnexpectedToken,
                        position: token.char_start,
                    })
                }
            }
            Some(Err(err)) => Err(ParserError::from(err)),
            None => Err(self.unexpected_end_of_input()),
        }
    }

    /// Returns true if the next token starts an expression.
    fn peek_exp(&mut self) -> bool {
        match self.lexer.tokens.peek() {
            Some(&Ok(Token {
                kind:
                    LiteralToken(NumberToken)
                    | LiteralToken(StrToken(DoubleQuotedToken | SingleQuotedToken))
                    | LiteralToken(StrToken(LongToken { .. }))
                    | NameToken(IdentifierToken)
                    | NameToken(KeywordToken(
                        FalseToken | FunctionToken | NilToken | NotToken | TrueToken,
                    ))
                    | OpToken(HashToken | MinusToken | VarargToken)
                    | PunctuatorToken(LcurlyToken | LparenToken),
                ..
            })) => true,
            _ => false,
        }
    }

    /// Consumes the specified TokenKind, if present.
    ///
    /// Returns `true`/`false` to indicate whether a token was slurped.
    fn slurp(&mut self, kind: docvim_lexer::lua::TokenKind) -> bool {
        if let Some(&Ok(token @ Token { .. })) = self.lexer.tokens.peek() {
            if token.kind == kind {
                self.consume(kind).expect("Failed to consume token");
                return true;
            }
        }
        false
    }

    // field ::= `[´ exp `]´ `=´ exp | Name `=´ exp | exp
    fn parse_table_field(&mut self, index: usize) -> Result<Field<'a>, ParserError> {
        let token = self.lexer.tokens.peek();
        match token {
            Some(&Ok(Token { kind: NameToken(IdentifierToken), .. })) => {
                let lexp = self.parse_exp(0)?;
                if let Exp::NamedVar(name) = lexp {
                    // Name = exp
                    if self.slurp(OpToken(AssignToken)) {
                        // `name = exp`; equivalent to `["name"] = exp`.
                        let rexp = self.parse_exp(0)?;
                        Ok(Field {
                            index: None,
                            lexp: Box::new(Exp::RawStr(name)),
                            rexp: Box::new(rexp),
                        })
                    } else {
                        // `exp`; syntactic sugar for `[index] = exp`
                        Ok(Field {
                            index: Some(index),
                            lexp: Box::new(Exp::Nil),
                            rexp: Box::new(lexp),
                        })
                    }
                } else {
                    // exp; syntactic sugar for `[index] = exp`
                    Ok(Field { index: Some(index), lexp: Box::new(Exp::Nil), rexp: Box::new(lexp) })
                }
            }
            Some(&Ok(Token { kind: PunctuatorToken(LbracketToken), .. })) => {
                // `[exp] = exp`
                self.lexer.tokens.next();
                let lexp = self.parse_exp(0)?;
                self.consume(PunctuatorToken(RbracketToken))?;
                self.consume(OpToken(AssignToken))?;
                let rexp = self.parse_exp(0)?; // TODO: confirm binding power of 0 is appropriate here
                Ok(Field { index: None, lexp: Box::new(lexp), rexp: Box::new(rexp) })
            }
            Some(&Ok(Token { .. })) => {
                // `exp`; syntactic sugar for `[index] = exp`
                let exp = self.parse_exp(0)?; // TODO: confirm binding power of 0 is appropriate here
                Ok(Field {
                    index: Some(index),
                    lexp: Box::new(Exp::Nil), // A hack because we can't create an Exp::Number here without upsetting the borrow checker.
                    rexp: Box::new(exp),
                })
                // TODO: implement this:
                // "If the last field in the list has the form exp and the expression is a function
                // call or a vararg expression, then all values returned by this expression enter the
                // list consecutively"
            }
            Some(&Err(err)) => return Err(ParserError::from(err)),
            None => Err(self.unexpected_end_of_input()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_local_declarations() {
        let mut parser = Parser::new("local foo");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("foo")],
                    explist: vec![]
                }
            }])
        );

        let mut parser = Parser::new("local bar = false");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("bar")],
                    explist: vec![Exp::False],
                }
            }])
        );

        let mut parser = Parser::new("local baz = nil");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("baz")],
                    explist: vec![Exp::Nil],
                }
            }])
        );

        let mut parser = Parser::new("local w = 1");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("w")],
                    explist: vec![Exp::Number("1")],
                }
            }])
        );

        let mut parser = Parser::new("local x = 'wat'");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("x")],
                    explist: vec![Exp::CookedStr(Box::new(String::from("wat")))],
                }
            }])
        );

        let mut parser = Parser::new("local y = \"don't say \\\"hello\\\"!\"");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("y")],
                    explist: vec![Exp::CookedStr(Box::new(String::from("don't say \"hello\"!")))],
                }
            }])
        );

        let mut parser = Parser::new("local z = [[loooong]]");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("z")],
                    explist: vec![Exp::RawStr("loooong")],
                }
            }])
        );

        let mut parser = Parser::new("local qux = true");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("qux")],
                    explist: vec![Exp::True],
                }
            }])
        );

        let mut parser = Parser::new("local neg = not true");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("neg")],
                    explist: vec![Exp::Unary { exp: Box::new(Exp::True), op: UnOp::Not }],
                }
            }])
        );

        let mut parser = Parser::new("local len = #'sample'");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("len")],
                    explist: vec![Exp::Unary {
                        exp: Box::new(Exp::CookedStr(Box::new(String::from("sample")))),
                        op: UnOp::Length,
                    }],
                }
            }])
        );

        let mut parser = Parser::new("local small = -1000");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("small")],
                    explist: vec![Exp::Unary {
                        exp: Box::new(Exp::Number("1000")),
                        op: UnOp::Minus
                    }],
                }
            }])
        );

        let mut parser = Parser::new("local sum = 7 + 8");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("sum")],
                    explist: vec![Exp::Binary {
                        lexp: Box::new(Exp::Number("7")),
                        op: BinOp::Plus,
                        rexp: Box::new(Exp::Number("8"))
                    }],
                }
            }])
        );

        // Based on the example from doc/lua.md plus some extra parens and a "not" thrown in for
        // good measure:
        //
        //      not (1 * 2 + 3 - 4 / 5 ^ -6 > -7 ^ 8 + (9 - 10) * 11)
        //
        // equivalent to:
        //
        //      not (((1 * 2) + (3 - (4 / (5 ^ (-6))))) > (-(7 ^ 8) + ((9 - 10) * 11)))
        //
        let mut parser =
            Parser::new("local demo =  not (1 * 2 + 3 - 4 / 5 ^ -6 > -7 ^ 8 + (9 - 10) * 11)");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("demo")],
                    explist: vec![Exp::Unary {
                        exp: Box::new(Exp::Binary {
                            lexp: Box::new(Exp::Binary {
                                lexp: Box::new(Exp::Binary {
                                    lexp: Box::new(Exp::Number("1")),
                                    op: BinOp::Star,
                                    rexp: Box::new(Exp::Number("2")),
                                }),
                                op: BinOp::Plus,
                                rexp: Box::new(Exp::Binary {
                                    lexp: Box::new(Exp::Number("3")),
                                    op: BinOp::Minus,
                                    rexp: Box::new(Exp::Binary {
                                        lexp: Box::new(Exp::Number("4")),
                                        op: BinOp::Slash,
                                        rexp: Box::new(Exp::Binary {
                                            lexp: Box::new(Exp::Number("5")),
                                            op: BinOp::Caret,
                                            rexp: Box::new(Exp::Unary {
                                                exp: Box::new(Exp::Number("6")),
                                                op: UnOp::Minus,
                                            })
                                        })
                                    })
                                })
                            }),
                            op: BinOp::Gt,
                            rexp: Box::new(Exp::Binary {
                                lexp: Box::new(Exp::Unary {
                                    exp: Box::new(Exp::Binary {
                                        lexp: Box::new(Exp::Number("7")),
                                        op: BinOp::Caret,
                                        rexp: Box::new(Exp::Number("8")),
                                    }),
                                    op: UnOp::Minus,
                                }),
                                op: BinOp::Plus,
                                rexp: Box::new(Exp::Binary {
                                    lexp: Box::new(Exp::Binary {
                                        lexp: Box::new(Exp::Number("9")),
                                        op: BinOp::Minus,
                                        rexp: Box::new(Exp::Number("10")),
                                    }),
                                    op: BinOp::Star,
                                    rexp: Box::new(Exp::Number("11"))
                                })
                            })
                        }),
                        op: UnOp::Not
                    }]
                }
            }])
        );
    }

    #[test]
    fn parses_unary_not_with_name() {
        let mut parser = Parser::new("local foo = not bar");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("foo")],
                    explist: vec![Exp::Unary {
                        exp: Box::new(Exp::NamedVar("bar")),
                        op: UnOp::Not
                    }],
                }
            }])
        );
    }

    #[test]
    fn parses_table_constructor() {
        let mut parser = Parser::new("local stuff = { [\"foo\"] = bar }");
        let ast = parser.parse();
        assert_eq!(
            ast.unwrap(),
            Block(vec![Node {
                comments: vec![],
                node: StatementKind::LocalDeclaration {
                    namelist: vec![Name("stuff")],
                    explist: vec![Exp::Table(vec![Field {
                        index: None,
                        lexp: Box::new(Exp::CookedStr(Box::new(String::from("foo")))),
                        rexp: Box::new(Exp::NamedVar("bar"))
                    }])],
                }
            }])
        );
    }
}
