use std::{io, fmt::{Display, self}};

use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    msg: String,
    path: Option<String>,
    pos: Option<Position>
}
impl Error {
    pub fn new(msg: String, path: Option<String>, pos: Option<Position>) -> Self {
        Self { msg, path, pos }
    }
    pub fn msg(msg: String) -> Self {
        Self { msg, path: None, pos: None }
    }
}
impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::msg(value.to_string())
    }
}
impl From<fmt::Error> for Error {
    fn from(value: fmt::Error) -> Self {
        Self::msg(value.to_string())
    }
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(path) = &self.path {
            write!(f, "{path}")?;
        }
        if let Some(pos) = &self.pos {
            if self.path.is_some() {
                write!(f, ":")?;
            }
            write!(f, "{}:{}", pos.ln.start + 1, pos.col.start + 1)?;
        }
        if self.path.is_some() || self.pos.is_some() {
            write!(f, ": ")?;
        }
        write!(f, "{}", self.msg)
    }
}

#[macro_export]
macro_rules! bad_char_error {
    ($c:literal, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("bad character {:?}", $c), Some(path), Some(pos)))
    };
}
#[macro_export]
macro_rules! cant_open_error {
    ($path:expr) => {
        Err(self::Error::new(format!("couldn't open path {:?}", $path), None, None))
    };
}