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
macro_rules! cant_open_error {
    ($path:expr) => {
        Err(self::Error::new(format!("couldn't open path {:?}", $path), None, None))
    };
}
#[macro_export]
macro_rules! parse_int_error {
    ($n:expr, $err:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("error while parsing int {:?}: {}", $n, $err), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! parse_float_error {
    ($n:expr, $err:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("error while parsing float {:?}: {}", $n, $err), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! expected_char_error {
    ($path:expr, $pos:expr) => {
        Err(self::Error::new(format!("expected a character, not end of input"), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! unclosed_char_error {
    ($path:expr, $pos:expr) => {
        Err(self::Error::new(format!("unclosed character"), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! unclosed_char_but_error {
    ($c:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("unclosed character, got {:?}", $c), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! unclosed_string_error {
    ($path:expr, $pos:expr) => {
        Err(self::Error::new(format!("unclosed string"), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! unexpected_end_error {
    ($path:expr) => {
        Err(self::Error::new(format!("unexpected end of file"), $path, None))
    };
}
#[macro_export]
macro_rules! unexpected_token_error {
    ($token:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("unexpected {}", $token.name()), $path, None))
    };
}
#[macro_export]
macro_rules! expected_token_error {
    ($expect:expr, $token:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("expected {}, got {}", $expect.name(), $token.name()), $path, None))
    };
}