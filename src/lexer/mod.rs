pub mod token;
mod lexer;

use crate::{
    position::Located,
    error::Error
};
use token::Token;

pub fn lex(path: Option<String>, text: String) -> Result<Vec<Located<Token>>, Error> {
    lexer::Lexer::new(path, text).lex()
}