pub mod node;
mod parser;
use crate::{
    position::*,
    lexer::token::*,
    error::Error
};
use node::Node;

pub fn parse(path: Option<String>, tokens: Vec<Located<Token>>) -> Result<Located<Node>, Error> {
    parser::Parser::new(path, tokens).parse()
}