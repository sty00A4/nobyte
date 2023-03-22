use std::fmt::Display;
use crate::{position::Located, *};

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Word(String), Int(i64), Float(f64), Bool(bool), Char(char), String(String),
    Call { head: Box<Located<Self>>, args: Vec<Located<Self>> },
    Vector(Vec<Located<Self>>), Body(Vec<Located<Self>>),
    Closure(Box<Located<Self>>), Key(String), Params(Vec<ParamNode>)
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
            Self::Key(word) => write!(f, "@{word}"),
            Self::Params(params) => write!(f, "@({})", join!(params, " ")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParamNode {
    pub word: String,
    pub typ: Located<Node>,
    pub multi: bool
}
impl ParamNode {
    pub fn new(word: String, typ: Located<Node>, multi: bool) -> Self {
        Self { word, typ, multi }
    }
}
impl Display for ParamNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}{}", self.word, self.typ, if self.multi { " ..." } else { "" })
    }
}