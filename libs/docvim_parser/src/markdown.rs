use crate::types::Location;

// Lexer token types are imported aliased (all with a "Token" suffix) to avoid collisions with
// parser node types of the same name.
use docvim_lexer::markdown::HeadingKind::Heading1 as Heading1Token;
use docvim_lexer::markdown::HeadingKind::Heading2 as Heading2Token;
use docvim_lexer::markdown::MarkdownToken::At as AtToken;
use docvim_lexer::markdown::MarkdownToken::Backtick as BacktickToken;
use docvim_lexer::markdown::MarkdownToken::Bang as BangToken;
use docvim_lexer::markdown::MarkdownToken::Bar as BarToken;
use docvim_lexer::markdown::MarkdownToken::BlockQuote as BlockQuoteToken;
use docvim_lexer::markdown::MarkdownToken::Break as BreakToken;
use docvim_lexer::markdown::MarkdownToken::CodeFence as CodeFenceToken;
use docvim_lexer::markdown::MarkdownToken::HorizontalRule as HorizontalRuleToken;
use docvim_lexer::markdown::MarkdownToken::Hyphen as HyphenToken;
use docvim_lexer::markdown::MarkdownToken::Lbracket as LbracketToken;
use docvim_lexer::markdown::MarkdownToken::Lparen as LparenToken;
use docvim_lexer::markdown::MarkdownToken::Newline as NewlineToken;
use docvim_lexer::markdown::MarkdownToken::Rbracket as RbracketToken;
use docvim_lexer::markdown::MarkdownToken::Rparen as RparenToken;
use docvim_lexer::markdown::MarkdownToken::Space as SpaceToken;
use docvim_lexer::markdown::MarkdownToken::Star as StarToken;
use docvim_lexer::markdown::MarkdownToken::Text as TextToken;
use docvim_lexer::markdown::{Lexer, MarkdownToken};
use docvim_lexer::token::Token;

pub struct Project(Vec<DocBlock>);

pub struct DocBlock {
    pub children: Vec<BlockElement>,
}

pub enum BlockElement {
    HorizontalRule,
}

//#[derive(Debug, PartialEq)]
//pub struct Node<'a, T> {
//    pub node: T,
//    pub location: Location,
//}

pub struct MarkdownParser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> MarkdownParser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { lexer: Lexer::new(input) }
    }

    pub fn parse(&mut self) -> Result<Project, ParserError> {
        Ok(Project(vec![]))
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub line: usize,
    pub column: usize,
}
