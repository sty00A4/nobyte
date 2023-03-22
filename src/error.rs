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
        Err(self::Error::new(format!("unexpected {}", $token.name()), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! expected_token_error {
    ($expect:expr, $token:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("expected {}, got {}", $expect.name(), $token.name()), $path, Some($pos)))
    };
}

#[macro_export]
macro_rules! not_defined_error {
    ($word:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("{:?} is not defined in this scope", $word), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! no_func_with_args_error {
    ($args:expr, $defs:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("no definition of function found with args ({}), valid:\n{}",
        join_debug!($args, " "), $defs.iter().map(|(params, _)|
            format!("({})", params.iter().map(|param|
                format!("{}{}", param.typ, if param.multi { "..." } else { "" })
            ).collect::<Vec<String>>().join(" "))
        ).collect::<Vec<String>>().join("\n")),
        $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! invalid_head_error {
    ($args:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("type {} is invalid as a call head", $args), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! illegal_cast_to_error {
    ($from:expr, $to:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("cannot cast from {} to {}", $from, $to), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! illegal_cast_from_error {
    ($from:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("cannot cast from {}", $from), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! illegal_index_value_error {
    ($typ:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("cannot index type {}", $typ), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! can_only_index_string_with_int {
    ($path:expr, $pos:expr) => {
        Err(self::Error::new(format!("can only index str with int"), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! can_only_index_vector_with_int {
    ($path:expr, $pos:expr) => {
        Err(self::Error::new(format!("can only index vec with int"), $path, Some($pos)))
    };
}
#[macro_export]
macro_rules! param_no_type {
    ($index:expr, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!("param #{} is not a type", $index), $path, Some($pos)))
    };
}