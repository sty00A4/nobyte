use std::fmt::Display;

pub const SYMBOLS: [char; 11] = ['(', ')', '[', ']', '{', '}', '#', '@', ';', '\'', '"'];

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Word(String), Int(i64), Float(f64), Bool(bool), Char(char), String(String),
    CallIn, CallOut, VecIn, VecOut, BodyIn, BodyOut,
    Closure, Key(String),
}
impl Token {
    pub fn from_word(word: String) -> Self {
        match word.as_str() {
            "true" => Self::Bool(true),
            "false" => Self::Bool(false),
            _ => Self::Word(word)
        }
    }
    pub fn name(&self) -> String {
        match self {
            Self::Word(_) => format!("word"),
            Self::Int(_) => format!("int"),
            Self::Float(_) => format!("float"),
            Self::Bool(_) => format!("bool"),
            Self::Char(_) => format!("char"),
            Self::String(_) => format!("string"),
            Self::Key(_) => format!("key"),
            _ => format!("'{self}'")
        }
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Word(word) => write!(f, "{word}"),
            Self::Int(int) => write!(f, "{int:?}"),
            Self::Float(float) => write!(f, "{float:?}"),
            Self::Bool(bool) => write!(f, "{bool:?}"),
            Self::Char(char) => write!(f, "{char:?}"),
            Self::String(string) => write!(f, "{string:?}"),
            Self::CallIn => write!(f, "("),
            Self::CallOut => write!(f, ")"),
            Self::VecIn => write!(f, "["),
            Self::VecOut => write!(f, "]"),
            Self::BodyIn => write!(f, "{{"),
            Self::BodyOut => write!(f, "}}"),
            Self::Closure => write!(f, "#"),
            Self::Key(key) => write!(f, "@{key}"),
        }
    }
}