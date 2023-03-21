use crate::{
    position::*,
    parser::node::Node,
    error::Error
};
use super::instr::Instr;

pub type Closure = Vec<Located<Instr>>;
pub const SDLCL: usize = 35; // string duplicate length check limit

pub struct Generator {
    pub path: Option<String>,
    strings: Vec<String>,
    closures: Vec<Closure>,
}
impl Generator {
    pub fn new(path: Option<String>) -> Self {
        Self {
            path,
            strings: vec![], closures: vec![],
        }
    }

    pub fn new_string(&mut self, string: String) -> usize {
        if string.len() < SDLCL {
            for (addr, _string) in self.strings.iter().enumerate() {
                if _string == &string {
                    return addr
                }
            }
        }
        let addr = self.strings.len();
        self.strings.push(string);
        addr
    }
    pub fn new_closure(&mut self, closure: Closure) -> usize {
        let addr = self.closures.len();
        self.closures.push(closure);
        addr
    }

    pub fn write(&mut self, instr: Instr, pos: Position, code: &mut Closure) {
        code.push(Located::new(instr, pos))
    }
    pub fn generate(&mut self, node: Located<Node>, code: &mut Closure) -> Result<(), Error> {
        let Located { item: node, pos } = node;
        match node {
            Node::Word(word) => {
                let addr = self.new_string(word);
                self.write(Instr::Var(addr), pos, code);
            }
            Node::Int(int) => {
                self.write(Instr::Int(int), pos, code);
            }
            Node::Float(float) => {
                self.write(Instr::Float(float), pos, code);
            }
            Node::Bool(bool) => {
                self.write(Instr::Bool(bool), pos, code);
            }
            Node::Char(char) => {
                self.write(Instr::Char(char), pos, code);
            }
            Node::String(string) => {
                let addr = self.new_string(string);
                self.write(Instr::String(addr), pos, code);
            }
            Node::Call { head, args } => {
                let len = args.len();
                for arg in args {
                    self.generate(arg, code)?;
                }
                let head_pos = head.pos.clone();
                self.generate(*head, code)?;
                self.write(Instr::Call(len), head_pos.clone(), code);
            }
            Node::Vector(nodes) => {
                for node in nodes {
                    self.generate(node, code)?;
                }
            }
            Node::Body(nodes) => {
                for node in nodes {
                    self.generate(node, code)?;
                }
            }
            Node::Closure(node) => {
                let mut closure = vec![];
                self.generate(*node, &mut closure)?;
                let addr = self.new_closure(closure);
                self.write(Instr::Closure(addr), pos, code);
            }
            Node::Key(key) => {
                let addr = self.new_string(key);
                self.write(Instr::String(addr), pos, code);
            }
        }
        Ok(())
    }
}