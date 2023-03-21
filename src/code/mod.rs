pub mod instr;
mod codegen;
use instr::Instr;
use crate::{
    position::{Located, Position},
    parser::node::Node,
    error::Error
};

pub fn generate(path: Option<String>, node: Located<Node>) -> Result<Vec<Located<Instr>>, Error> {
    let mut code = vec![];
    let mut generator = codegen::Generator::new(path);
    generator.generate(node, &mut code)?;
    code.push(Located::new(Instr::Halt, Position::default()));
    Ok(code)
}