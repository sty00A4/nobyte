use crate::{
    *,
    position::*,
    lexer::token::Token, error::Error
};
use super::node::Node;

pub struct Parser {
    pub path: Option<String>,
    tokens: Vec<Located<Token>>
}
impl Parser {
    pub fn new(path: Option<String>, tokens: Vec<Located<Token>>) -> Self {
        Self { path, tokens }
    }
    pub fn token(&mut self) -> Option<Located<Token>> {
        if self.tokens.len() > 0 { Some(self.tokens.remove(0)) } else { None }
    }
    pub fn token_ref(&self) -> Option<&Located<Token>> {
        self.tokens.first()
    }
    pub fn token_expect(&mut self) -> Result<Located<Token>, Error> {
        let Some(token) = self.token() else {
            return unexpected_end_error!(self.path.clone())
        };
        Ok(token)
    }
    pub fn token_ref_expect(&self) -> Result<&Located<Token>, Error> {
        let Some(token) = self.token_ref() else {
            return unexpected_end_error!(self.path.clone())
        };
        Ok(token)
    }
    pub fn expect(&mut self, expect: Token) -> Result<Located<Token>, Error> {
        let Located { item: token, pos } = self.token_expect()?;
        if expect != token {
            return expected_token_error!(expect, token, self.path.clone(), pos)
        }
        Ok(Located { item: token, pos })
    }

    pub fn node(&mut self) -> Result<Located<Node>, Error> {
        let Located { item: token, mut pos } = self.token_expect()?;
        match token {
            Token::Word(word) => Ok(Located::new(Node::Word(word), pos)),
            Token::Int(int) => Ok(Located::new(Node::Int(int), pos)),
            Token::Float(float) => Ok(Located::new(Node::Float(float), pos)),
            Token::Bool(bool) => Ok(Located::new(Node::Bool(bool), pos)),
            Token::Char(char) => Ok(Located::new(Node::Char(char), pos)),
            Token::String(string) => Ok(Located::new(Node::String(string), pos)),
            Token::CallIn => {
                let head = Box::new(self.node()?);
                let mut args = vec![];
                while let Some(Located { item: token, pos: _ }) = self.token_ref() {
                    if token == &Token::CallOut { break }
                    args.push(self.node()?);
                }
                let Located { item: _, pos: end_pos } = self.expect(Token::CallOut)?;
                pos.extend(&end_pos);
                Ok(Located::new(Node::Call { head, args }, pos))
            }
            Token::VecIn => {
                let mut nodes = vec![];
                while let Some(Located { item: token, pos: _ }) = self.token_ref() {
                    if token == &Token::VecOut { break }
                    nodes.push(self.node()?);
                }
                let Located { item: _, pos: end_pos } = self.expect(Token::VecOut)?;
                pos.extend(&end_pos);
                Ok(Located::new(Node::Vector(nodes), pos))
            }
            Token::BodyIn => {
                let mut nodes = vec![];
                while let Some(Located { item: token, pos: _ }) = self.token_ref() {
                    if token == &Token::BodyOut { break }
                    nodes.push(self.node()?);
                }
                let Located { item: _, pos: end_pos } = self.expect(Token::BodyOut)?;
                pos.extend(&end_pos);
                Ok(Located::new(Node::Body(nodes), pos))
            }
            Token::Closure => Ok(Located::new(Node::Closure(Box::new(self.node()?)), pos)),
            Token::Key => Ok(Located::new(Node::Key(Box::new(self.node()?)), pos)),
            token => unexpected_token_error!(token, self.path.clone(), pos),
        }
    }
    pub fn parse(&mut self) -> Result<Located<Node>, Error> {
        let mut nodes: Vec<Located<Node>> = vec![];
        while self.token_ref().is_some() {
            let node = self.node()?;
            nodes.push(node);
        }
        if nodes.len() == 1 {
            Ok(nodes.pop().unwrap())
        } else if nodes.len() == 0 {
            Err(Error::new(format!("empty file"), self.path.clone(), Some(Position::default())))
        } else {
            let (start, end) = (nodes.first().unwrap().pos.clone(), nodes.last().unwrap().pos.clone());
            Ok(Located::new(Node::Body(nodes), Position::new(start.ln.start..end.ln.end, start.col.start..start.col.end)))
        }
    }
}