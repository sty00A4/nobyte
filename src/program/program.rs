use std::collections::HashMap;

use crate::{
    *,
    code::codegen::Closure,
    error::Error, position::{Located, Position},
    code::instr::Instr
};
use super::value::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    name: String,
    typ: Type
}
impl Param {
    pub fn new(name: String, typ: Type) -> Self {
        Self { name, typ }
    }
}
macro_rules! param {
    ($name:literal, $typ:ident) => {
        Param::new($name.into(), Type::$typ)
    };
}

pub struct Program {
    pub path: Option<String>,
    strings: Vec<String>,
    closures: Vec<Closure>,

    functions: Vec<HashMap<Vec<Param>, Function>>,
    vars: Vec<HashMap<String, Value>>,

    stack: Vec<Value>,
}
impl Program {
    pub fn new(path: Option<String>, strings: Vec<String>, closures: Vec<Closure>,
               functions: Vec<HashMap<Vec<Param>, Function>>, vars: Vec<HashMap<String, Value>>) -> Self {
        Self {
            path, strings, closures,
            functions, vars,
            stack: vec![],
        }
    }
    pub fn new_func(&mut self, params: Vec<Param>, func: Function) {
        let mut defs = HashMap::new();
        defs.insert(params, func);
        self.functions.push(defs);
    }
    pub fn var(&self, word: &String) -> Option<Value> {
        for scope in self.vars.iter() {
            if let Some(value) = scope.get(word) {
                return Some(value.clone())
            }
        }
        None
    }
    pub fn func(&mut self, addr: usize, args: Vec<Value>, pos: Position) -> Result<Function, Error> {
        let funcs = self.functions.get(addr).unwrap();
        let types: Vec<Type> = args.iter().map(|value| value.typ()).collect();
        'search: for (params, func) in funcs.iter() {
            for (i, param) in params.iter().enumerate() {
                if let Some(typ) = types.get(i) {
                    if &param.typ != typ {
                        continue 'search;
                    }
                    match self.vars.last_mut() {
                        Some(scope) => { scope.insert(param.name.clone(), args[i].clone()); }
                        None => {}
                    }
                } else {
                    continue 'search;
                }
            }
            return Ok(func.clone())
        }
        no_func_with_args_error!(args, self.path.clone(), pos)
    }

    pub fn execute(&mut self, code: Closure) -> Result<Value, Error> {
        for Located { item: instr, pos } in code {
            use Instr::*;
            match instr {
                Halt => break,
                Int(int) => self.stack.push(Value::Int(int)),
                Float(float) => self.stack.push(Value::Float(float)),
                Bool(bool) => self.stack.push(Value::Bool(bool)),
                Char(char) => self.stack.push(Value::Char(char)),
                String(addr) => self.stack.push(Value::String(self.strings[addr].clone())),
                Closure(addr) => self.stack.push(Value::Closure(addr)),
                Var(addr) => match self.var(&self.strings[addr]) {
                    Some(value) => self.stack.push(value.clone()),
                    None => return not_defined_error!(self.strings[addr].clone(), self.path.clone(), pos)
                }
                Params(_) => todo!(),
                Call(len) => {
                    let head = self.stack.pop().unwrap();
                    let mut args = vec![];
                    for _ in 0..len {
                        args.push(self.stack.pop().unwrap());
                    }
                    let args: Vec<Value> = args.into_iter().rev().collect();
                    match head {
                        Value::Int(index) => todo!(),
                        Value::Bool(cond) => todo!(),
                        Value::Type(typ) => todo!(),
                        Value::Closure(addr) => todo!(),
                        Value::Function(addr) => match self.func(addr, args, pos)? {
                            Function::Native(func) => {
                                let value = func(self)?;
                                self.stack.push(value);
                            }
                            Function::Function(closure) => {
                                let value = self.execute(closure)?;
                                self.stack.push(value);
                            }
                        }
                        head => return invalid_head_error!(head.typ(), self.path.clone(), pos)
                    }
                }
            }
        }
        if let Some(top_stack) = self.stack.pop() {
            Ok(top_stack)
        } else {
            Ok(Value::None)
        }
    }
}

#[derive(Clone)]
pub enum Function {
    Native(fn(&mut Program) -> Result<Value, Error>), Function(Closure)
}

pub fn std_program(path: Option<String>, mut strings: Vec<String>, closures: Vec<Closure>) -> Program {
    let mut functions = vec![];
    let mut vars = HashMap::new();

    let mut defs = HashMap::new();
    defs.insert(vec![param!("var", String), param!("value", Any)], Function::Native(_set));
    strings.push("set".into());
    vars.insert("set".into(), Value::Function(functions.len()));
    functions.push(defs);

    let mut defs = HashMap::new();
    defs.insert(vec![param!("x", Any)], Function::Native(_print));
    strings.push("print".into());
    vars.insert("print".into(), Value::Function(functions.len()));
    functions.push(defs);

    Program::new(path, strings, closures, functions, vec![vars])
}

pub fn _set(program: &mut Program) -> Result<Value, Error> {
    let Value::String(var) = program.var(&"var".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let value = program.var(&"value".into()).unwrap();
    if let Some(scope) = program.vars.last_mut() {
        scope.insert(var, value);
    }
    Ok(Value::None)
}
pub fn _print(program: &mut Program) -> Result<Value, Error> {
    let x = program.var(&"x".into()).unwrap();
    println!("{x}");
    Ok(Value::None)
}