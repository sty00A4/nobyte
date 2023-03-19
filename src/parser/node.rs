use std::fmt::Display;
use crate::{position::Located, *};

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Word(String), Int(i64), Float(f64), Bool(bool), Char(char), String(String),
    Call { head: Box<Located<Self>>, args: Vec<Located<Self>> },
    Vector(Vec<Located<Self>>), Body(Vec<Located<Self>>),
    Closure(Box<Located<Self>>), Key(Box<Located<Self>>)
}
impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Word(word) => write!(f, "{word}"),
            Self::Int(int) => write!(f, "{int:?}"),
            Self::Float(float) => write!(f, "{float:?}"),
            Self::Bool(bool) => write!(f, "{bool:?}"),
            Self::Char(char) => write!(f, "{char:?}"),
            Self::String(string) => write!(f, "{string:?}"),
            Self::Call { head, args } => write!(f, "({head} {})", join!(args, " ")),
            Self::Vector(nodes) => write!(f, "[{}]", join!(nodes, " ")),
            Self::Body(nodes) => write!(f, "{{{}}}", join!(nodes, " ")),
            Self::Closure(node) => write!(f, "#{node}"),
            Self::Key(node) => write!(f, "@{node}"),
        }
    }
}