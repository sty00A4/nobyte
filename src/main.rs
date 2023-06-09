#![allow(dead_code, unused_variables, unused_macros)]
mod position;
mod error;
mod lexer;
mod parser;
mod code;
mod program;

use std::{process::exit, env, io::{self, Write}, fs};
use error::Error;
use program::value::Value;

#[macro_export]
macro_rules! join {
    ($v:expr, $sep:expr) => {
        $v.iter().map(|x| x.to_string()).collect::<Vec<String>>().join($sep)
    };
}
#[macro_export]
macro_rules! join_debug {
    ($v:expr, $sep:expr) => {
        $v.iter().map(|x| format!("{x:?}")).collect::<Vec<String>>().join($sep)
    };
}
#[macro_export]
macro_rules! join_enum {
    ($v:expr, $sep:expr) => {
        $v.iter().enumerate().map(|(i, x)| format!("{i}\t{x}")).collect::<Vec<String>>().join($sep)
    };
}

pub fn run(path: Option<String>, text: String) -> Result<(), Error> {
    let tokens = lexer::lex(path.clone(), text)?;
    // println!("{tokens:?}");
    let ast = parser::parse(path.clone(), tokens)?;
    // println!("{ast}");
    let (code, strings, closures) = code::generate(path.clone(), ast)?;
    // println!("{}", join_enum!(code, "\n"));
    let ret = program::run(path.clone(), code, strings, closures)?;
    if ret != Value::None {
        println!("{ret}");
    }
    Ok(())
}

fn entry() -> Result<(), Error> {
    let mut args = env::args();
    args.next();
    if let Some(path) = args.next() {
        let Ok(text) = fs::read_to_string(&path) else {
            return cant_open_error!(path)
        };
        run(Some(path), text)?;
    } else {
        loop {
            let mut input = String::new();
            print!("> ");
            io::stdout().flush()?;
            io::stdin().read_line(&mut input)?;
            run(None, input)?;
        }
    }
    Ok(())
}

fn main() {
    if let Some(err) = entry().err() {
        eprintln!("ERROR: {err}");
        exit(1);
    }
}