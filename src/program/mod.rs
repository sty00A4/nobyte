pub mod value;
mod program;

use crate::{
    position::Located,
    code::instr::Instr,
    code::codegen::Closure,
    error::Error
};
use value::Value;
use program::std_program;


pub fn run(path: Option<String>, code: Vec<Located<Instr>>, strings: Vec<String>, closures: Vec<Closure>) -> Result<Value, Error> {
    let mut program = std_program(path, strings, closures);
    program.execute(code)
}