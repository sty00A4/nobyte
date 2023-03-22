use std::{collections::HashMap, fmt::Display};

use crate::{
    *,
    code::codegen::Closure,
    error::Error, position::{Located, Position},
    code::instr::Instr
};
use super::value::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
// todo: Param enum
pub struct Param {
    name: String,
    typ: Type,
    multi: bool
}
impl Param {
    pub fn new(name: String, typ: Type, multi: bool) -> Self {
        Self { name, typ, multi }
    }
}
impl Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}{}", self.name, self.typ, if self.multi { "..." } else { "" })
    }
}
macro_rules! param {
    ($name:literal, $typ:ident) => {
        Param::new($name.into(), Type::$typ, false)
    };
}
macro_rules! param_union {
    ($name:literal, $types:expr) => {
        Param::new($name.into(), Type::Union(Vec::from($types)), false)
    };
}
macro_rules! param_multi {
    ($name:literal, $typ:ident) => {
        Param::new($name.into(), Type::$typ, true)
    };
}
macro_rules! param_union_multi {
    ($name:literal, $types:expr) => {
        Param::new($name.into(), Type::Union(Vec::from($types)), true)
    };
}

pub struct Program {
    pub path: Option<String>,
    strings: Vec<String>,
    closures: Vec<Closure>,

    functions: Vec<Vec<(Vec<Param>, Function)>>,
    vars: Vec<HashMap<String, Value>>,

    stack: Vec<Value>,
}
impl Program {
    pub fn new(path: Option<String>, strings: Vec<String>, closures: Vec<Closure>,
               functions: Vec<Vec<(Vec<Param>, Function)>>, vars: Vec<HashMap<String, Value>>) -> Self {
        Self {
            path, strings, closures,
            functions, vars,
            stack: vec![],
        }
    }
    pub fn new_func(&mut self, params: Vec<Param>, func: Function) {
        let mut defs = vec![];
        defs.push((params, func));
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
    pub fn func(&mut self, addr: usize, mut args: Vec<Value>, pos: Position) -> Result<Function, Error> {
        let defs = self.functions.get(addr).unwrap();
        let types: Vec<Type> = args.iter().map(|value| value.typ()).collect();
        'search: for (params, func) in defs.iter() {
            let mut arg_idx = 0;
            for param in params.iter() {
                if let Some(typ) = types.get(arg_idx) {
                    if &param.typ != typ {
                        continue 'search;
                    }
                    arg_idx += 1;
                    if param.multi {
                        while let Some(typ) = types.get(arg_idx) {
                            if &param.typ != typ {
                                break;
                            }
                            arg_idx += 1;
                        }
                    }
                } else {
                    continue 'search;
                }
            }
            arg_idx = 0;
            for param in params.iter() {
                match self.vars.last_mut() {
                    Some(scope) => if param.multi {
                        let mut vector = vec![args.remove(0)];
                        arg_idx += 1;
                        while let Some(typ) = types.get(arg_idx) {
                            if &param.typ != typ {
                                break;
                            }
                            vector.push(args.remove(0));
                            arg_idx += 1;
                        }
                        scope.insert(param.name.clone(), Value::Vector(vector));
                    } else {
                        scope.insert(param.name.clone(), args.remove(0));
                        arg_idx += 1;
                    }
                    None => {}
                }
            }
            return Ok(func.clone())
        }
        no_func_with_args_error!(args, defs, self.path.clone(), pos)
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

    // set
    let mut defs = vec![];
    defs.push((vec![param!("var", String), param!("value", Any)], Function::Native(_set)));
    strings.push("set".into());
    vars.insert("set".into(), Value::Function(functions.len()));
    functions.push(defs);

    // print
    let mut defs = vec![];
    defs.push((vec![param_multi!("x", Any)], Function::Native(_print)));
    strings.push("print".into());
    vars.insert("print".into(), Value::Function(functions.len()));
    functions.push(defs);
    
    // +
    let mut defs = vec![];
    defs.push((vec![param_union_multi!("values", [Type::Int, Type::Float])], Function::Native(_add)));
    strings.push("+".into());
    vars.insert("+".into(), Value::Function(functions.len()));
    functions.push(defs);
    // -
    let mut defs = vec![];
    defs.push((vec![param_union_multi!("values", [Type::Int, Type::Float])], Function::Native(_sub)));
    strings.push("-".into());
    vars.insert("-".into(), Value::Function(functions.len()));
    functions.push(defs);
    // *
    let mut defs = vec![];
    defs.push((vec![param_union_multi!("values", [Type::Int, Type::Float])], Function::Native(_mul)));
    strings.push("*".into());
    vars.insert("*".into(), Value::Function(functions.len()));
    functions.push(defs);
    // /
    let mut defs = vec![];
    defs.push((vec![param_union_multi!("values", [Type::Int, Type::Float])], Function::Native(_div)));
    strings.push("/".into());
    vars.insert("/".into(), Value::Function(functions.len()));
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
    let Value::Vector(x) = program.var(&"x".into()).unwrap() else {
        panic!("type checking doesn't work")
    };
    println!("{}", join!(x, " "));
    Ok(Value::None)
}

pub fn _add(program: &mut Program) -> Result<Value, Error> {
    let Value::Vector(mut values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let mut sum = values.remove(0);
    while values.len() > 0 {
        match values.remove(0) {
            Value::Int(n2) => match &sum {
                Value::Int(n1) => sum = Value::Int(*n1 + n2),
                Value::Float(n1) => sum = Value::Float(*n1 + n2 as f64),
                _ => panic!("type checking doesn't work")
            }
            Value::Float(n2) => match &sum {
                Value::Int(n1) => sum = Value::Float(*n1 as f64 + n2),
                Value::Float(n1) => sum = Value::Float(*n1 + n2),
                _ => panic!("type checking doesn't work")
            }
            _ => panic!("type checking doesn't work")
        }
    }
    Ok(sum)
}
pub fn _sub(program: &mut Program) -> Result<Value, Error> {
    let Value::Vector(mut values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let mut sum = values.remove(0);
    if values.len() == 0 {
        return match sum {
            Value::Int(sum) => Ok(Value::Int(-sum)),
            Value::Float(sum) => Ok(Value::Float(-sum)),
            _ => panic!("type checking doesn't work")
        }
    }
    while values.len() > 0 {
        match values.remove(0) {
            Value::Int(n2) => match &sum {
                Value::Int(n1) => sum = Value::Int(*n1 - n2),
                Value::Float(n1) => sum = Value::Float(*n1 - n2 as f64),
                _ => panic!("type checking doesn't work")
            }
            Value::Float(n2) => match &sum {
                Value::Int(n1) => sum = Value::Float(*n1 as f64 - n2),
                Value::Float(n1) => sum = Value::Float(*n1 - n2),
                _ => panic!("type checking doesn't work")
            }
            _ => panic!("type checking doesn't work")
        }
    }
    Ok(sum)
}
pub fn _mul(program: &mut Program) -> Result<Value, Error> {
    let Value::Vector(mut values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let mut sum = values.remove(0);
    while values.len() > 0 {
        match values.remove(0) {
            Value::Int(n2) => match &sum {
                Value::Int(n1) => sum = Value::Int(*n1 * n2),
                Value::Float(n1) => sum = Value::Float(*n1 * n2 as f64),
                _ => panic!("type checking doesn't work")
            }
            Value::Float(n2) => match &sum {
                Value::Int(n1) => sum = Value::Float(*n1 as f64 * n2),
                Value::Float(n1) => sum = Value::Float(*n1 * n2),
                _ => panic!("type checking doesn't work")
            }
            _ => panic!("type checking doesn't work")
        }
    }
    Ok(sum)
}
pub fn _div(program: &mut Program) -> Result<Value, Error> {
    let Value::Vector(mut values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let mut sum = values.remove(0);
    while values.len() > 0 {
        match values.remove(0) {
            Value::Int(n2) => match &sum {
                Value::Int(n1) => sum = Value::Float(*n1 as f64 / n2 as f64),
                Value::Float(n1) => sum = Value::Float(*n1 / n2 as f64),
                _ => panic!("type checking doesn't work")
            }
            Value::Float(n2) => match &sum {
                Value::Int(n1) => sum = Value::Float(*n1 as f64 / n2),
                Value::Float(n1) => sum = Value::Float(*n1 / n2),
                _ => panic!("type checking doesn't work")
            }
            _ => panic!("type checking doesn't work")
        }
    }
    Ok(sum)
}