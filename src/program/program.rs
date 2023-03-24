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

    pub fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
    }
    pub fn pop_scope(&mut self) -> Option<HashMap<String, Value>> {
        self.vars.pop()
    }

    pub fn new_func(&mut self, params: Vec<Param>, func: Function) -> usize {
        let mut defs = vec![];
        defs.push((params, func));
        let addr = self.functions.len();
        self.functions.push(defs);
        addr
    }
    pub fn set(&mut self, word: String, value: Value) -> Option<Value> {
        for scope in self.vars.iter_mut().rev() {
            if let Some(old_value) = scope.get_mut(&word) {
                *old_value = value;
                return None
            }
        }
        self.vars.last_mut()?.insert(word, value)
    }
    pub fn setn(&mut self, n: usize, value: Value) -> Option<Value> {
        self.set(n.to_string(), value)
    }
    pub fn var(&self, word: &String) -> Option<Value> {
        for scope in self.vars.iter().rev() {
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
            match instr {
                Instr::Halt => break,
                Instr::Int(int) => self.stack.push(Value::Int(int)),
                Instr::Float(float) => self.stack.push(Value::Float(float)),
                Instr::Bool(bool) => self.stack.push(Value::Bool(bool)),
                Instr::Char(char) => self.stack.push(Value::Char(char)),
                Instr::String(addr) => self.stack.push(Value::String(self.strings[addr].clone())),
                Instr::Closure(addr) => self.stack.push(Value::Closure(addr)),
                Instr::Vector(len) => {
                    let mut values = vec![];
                    for _ in 0..len {
                        values.push(self.stack.pop().unwrap());
                    }
                    let values: Vec<Value> = values.into_iter().rev().collect();
                    self.stack.push(Value::Vector(values))
                }
                Instr::Var(addr) => match self.var(&self.strings[addr]) {
                    Some(value) => self.stack.push(value.clone()),
                    None => return not_defined_error!(self.strings[addr].clone(), self.path.clone(), pos)
                }
                Instr::Params(len) => {
                    let mut params: Vec<Param> = vec![];
                    for i in 0..len {
                        let Value::Bool(multi) = self.stack.pop().unwrap() else {
                            panic!("parsing params wrong")
                        };
                        let Value::Type(typ) = self.stack.pop().unwrap() else {
                            return param_no_type!(len - i, self.path.clone(), pos)
                        };
                        let Value::String(word) = self.stack.pop().unwrap() else {
                            panic!("parsing params wrong")
                        };
                        params.push(Param::new(word, typ, multi));
                    }
                    self.stack.push(Value::Params(params.into_iter().rev().collect()));
                }
                Instr::Call(len) => {
                    let head = self.stack.pop().unwrap();
                    let mut args = vec![];
                    for _ in 0..len {
                        args.push(self.stack.pop().unwrap());
                    }
                    let mut args: Vec<Value> = args.into_iter().rev().collect();
                    let value = (|program: &mut Self| match head {
                        Value::Bool(cond) => if cond {
                            if args.get(0).is_some() {
                                Ok(args.remove(0))
                            } else {
                                Ok(Value::None)
                            }
                        } else {
                            if args.get(1).is_some() {
                                Ok(args.remove(1))
                            } else {
                                Ok(Value::None)
                            }
                        }
                        Value::String(string) => if args.len() > 0 {
                            if args.len() > 1 {
                                let (start, end) = (args.remove(0), args.remove(0));
                                if let (Value::Int(start), Value::Int(end)) = (start, end) {
                                    let (start, end) = (start.max(0) as usize, end.max(0) as usize);
                                    if let Some(sub) = string.get(start..end) {
                                        Ok(Value::String(sub.to_string()))
                                    } else {
                                        Ok(Value::None)
                                    }
                                } else {
                                    can_only_index_string_with_int!(program.path.clone(), pos)
                                }
                            } else {
                                let index = args.remove(0);
                                if let Value::Int(index) = index {
                                    let index = index.max(0) as usize;
                                    if let Some(sub) = string.get(index..index + 1) {
                                        Ok(Value::Char(sub.chars().next().unwrap()))
                                    } else {
                                        Ok(Value::None)
                                    }
                                } else {
                                    can_only_index_string_with_int!(program.path.clone(), pos)
                                }
                            }
                        } else {
                            Ok(Value::None)
                        }
                        Value::Vector(vector) => if args.len() > 0 {
                            let index = args.remove(0);
                            if let Value::Int(index) = index {
                                let index = index.max(0) as usize;
                                if let Some(value) = vector.get(index) {
                                    Ok(value.clone())
                                } else {
                                    Ok(Value::None)
                                }
                            } else {
                                can_only_index_vector_with_int!(program.path.clone(), pos)
                            }
                        } else {
                            Ok(Value::None)
                        }
                        Value::Type(typ) => match typ {
                            Type::None => Ok(Value::None),
                            Type::Any => Ok(args.remove(0)),
                            Type::Int => match args.get(0).unwrap_or_else(|| &Value::None) {
                                Value::Int(int) => Ok(Value::Int(*int)),
                                Value::Float(float) => Ok(Value::Int(float.floor() as i64)),
                                Value::Bool(bool) => Ok(Value::Int(if *bool { 1 } else { 0 })),
                                Value::Char(char) => Ok(Value::Int(*char as u8 as i64)),
                                Value::String(string) => Ok(match string.parse() {
                                    Ok(int) => Value::Int(int),
                                    Err(_) => Value::None,
                                }),
                                cast_typ => illegal_cast_to_error!(typ, cast_typ.typ(), program.path.clone(), pos)
                            }
                            Type::Float => match args.get(0).unwrap_or_else(|| &Value::None) {
                                Value::Int(int) => Ok(Value::Float(*int as f64)),
                                Value::Float(float) => Ok(Value::Float(*float)),
                                Value::Bool(bool) => Ok(Value::Float(if *bool { 1. } else { 0. })),
                                Value::Char(char) => Ok(Value::Float(*char as u8 as i64 as f64)),
                                Value::String(string) => Ok(match string.parse() {
                                    Ok(float) => Value::Float(float),
                                    Err(_) => Value::None,
                                }),
                                cast_typ => illegal_cast_to_error!(typ, cast_typ.typ(), program.path.clone(), pos)
                            }
                            Type::Bool => match args.get(0).unwrap_or_else(|| &Value::None) {
                                Value::Int(int) => Ok(Value::Bool(*int != 0)),
                                Value::Float(float) => Ok(Value::Bool(*float != 0.)),
                                Value::Bool(bool) => Ok(Value::Bool(*bool)),
                                Value::Char(char) => Ok(Value::Bool(*char as u8 != 0)),
                                Value::String(string) => Ok(match string.parse() {
                                    Ok(bool) => Value::Bool(bool),
                                    Err(_) => Value::None,
                                }),
                                cast_typ => illegal_cast_to_error!(typ, cast_typ.typ(), program.path.clone(), pos)
                            }
                            Type::Char => match args.get(0).unwrap_or_else(|| &Value::None) {
                                Value::Int(int) => Ok(Value::Char((*int % 256) as u8 as char)),
                                Value::Char(char) => Ok(Value::Char(*char)),
                                cast_typ => return illegal_cast_to_error!(typ, cast_typ.typ(), program.path.clone(), pos)
                            }
                            Type::String => Ok(Value::String(args.get(0).unwrap_or_else(|| &Value::None).to_string())),
                            Type::Vector => Ok(Value::Vector(args)),
                            Type::Type => Ok(Value::Type(args.get(0).unwrap_or_else(|| &Value::None).typ())),
                            typ => illegal_cast_from_error!(typ, program.path.clone(), pos)
                        }
                        Value::Closure(addr) => {
                            program.push_scope();
                            let closure = program.closures[addr].clone();
                            for i in 0..args.len() {
                                program.setn(i, args.remove(0));
                            }
                            let value = program.execute(closure)?;
                            program.pop_scope();
                            Ok(value)
                        }
                        Value::Function(addr) => match program.func(addr, args, pos.clone())? {
                            Function::Native(func) => {
                                program.push_scope();
                                let value = func(program, pos)?;
                                program.pop_scope();
                                Ok(value)
                            }
                            Function::Function(closure) => {
                                program.push_scope();
                                let value = program.execute(closure)?;
                                program.pop_scope();
                                Ok(value)
                            }
                        }
                        head => invalid_head_error!(head.typ(), program.path.clone(), pos)
                    })(self)?;
                    self.stack.push(value);
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
    Native(fn(&mut Program, Position) -> Result<Value, Error>), Function(Closure)
}

pub fn std_program(path: Option<String>, mut strings: Vec<String>, closures: Vec<Closure>) -> Program {
    let mut functions = vec![];
    let mut vars = HashMap::new();
    vars.insert("nonetype".into(), Value::Type(Type::None));
    vars.insert("any".into(), Value::Type(Type::Any));
    vars.insert("int".into(), Value::Type(Type::Int));
    vars.insert("float".into(), Value::Type(Type::Float));
    vars.insert("bool".into(), Value::Type(Type::Bool));
    vars.insert("char".into(), Value::Type(Type::Char));
    vars.insert("str".into(), Value::Type(Type::String));
    vars.insert("vec".into(), Value::Type(Type::Vector));
    vars.insert("closure".into(), Value::Type(Type::Closure));
    vars.insert("function".into(), Value::Type(Type::Function));
    vars.insert("type".into(), Value::Type(Type::Type));
    vars.insert("number".into(), Value::Type(Type::Union(vec![Type::Int, Type::Float])));
    vars.insert("indexable".into(), Value::Type(Type::Union(vec![Type::Vector, Type::String])));

    // set
    let mut defs = vec![];
    defs.push((vec![param!("var", String), param!("value", Any)], Function::Native(_set)));
    strings.push("set".into());
    vars.insert("set".into(), Value::Function(functions.len()));
    functions.push(defs);
    // global
    let mut defs = vec![];
    defs.push((vec![param!("var", String), param!("value", Any)], Function::Native(_global)));
    strings.push("global".into());
    vars.insert("global".into(), Value::Function(functions.len()));
    functions.push(defs);
    // get
    let mut defs = vec![];
    defs.push((vec![param!("var", String)], Function::Native(_get)));
    strings.push("get".into());
    vars.insert("get".into(), Value::Function(functions.len()));
    functions.push(defs);
    // def
    let mut defs = vec![];
    defs.push((vec![param!("name", String), param!("params", Params), param!("closure", Closure)], Function::Native(_def)));
    strings.push("def".into());
    vars.insert("def".into(), Value::Function(functions.len()));
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
    
    // =
    let mut defs = vec![];
    defs.push((vec![param!("start", Int), param_multi!("values", Int)], Function::Native(_eq)));
    defs.push((vec![param!("start", Float), param_multi!("values", Float)], Function::Native(_eq)));
    defs.push((vec![param!("start", Bool), param_multi!("values", Bool)], Function::Native(_eq)));
    defs.push((vec![param!("start", Char), param_multi!("values", Char)], Function::Native(_eq)));
    defs.push((vec![param!("start", String), param_multi!("values", String)], Function::Native(_eq)));
    defs.push((vec![param!("start", Vector), param_multi!("values", Vector)], Function::Native(_eq)));
    defs.push((vec![param!("start", Closure), param_multi!("values", Closure)], Function::Native(_eq)));
    defs.push((vec![param!("start", Function), param_multi!("values", Function)], Function::Native(_eq)));
    defs.push((vec![param!("start", Params), param_multi!("values", Params)], Function::Native(_eq)));
    strings.push("=".into());
    vars.insert("=".into(), Value::Function(functions.len()));
    functions.push(defs);
    // !=
    let mut defs = vec![];
    defs.push((vec![param!("start", Int), param_multi!("values", Int)], Function::Native(_ne)));
    defs.push((vec![param!("start", Float), param_multi!("values", Float)], Function::Native(_ne)));
    defs.push((vec![param!("start", Bool), param_multi!("values", Bool)], Function::Native(_ne)));
    defs.push((vec![param!("start", Char), param_multi!("values", Char)], Function::Native(_ne)));
    defs.push((vec![param!("start", String), param_multi!("values", String)], Function::Native(_ne)));
    defs.push((vec![param!("start", Vector), param_multi!("values", Vector)], Function::Native(_ne)));
    defs.push((vec![param!("start", Closure), param_multi!("values", Closure)], Function::Native(_ne)));
    defs.push((vec![param!("start", Function), param_multi!("values", Function)], Function::Native(_ne)));
    defs.push((vec![param!("start", Params), param_multi!("values", Params)], Function::Native(_ne)));
    strings.push("!=".into());
    vars.insert("!=".into(), Value::Function(functions.len()));
    functions.push(defs);
    // <
    let mut defs = vec![];
    defs.push((vec![param!("start", Int), param_multi!("values", Int)], Function::Native(_lt)));
    defs.push((vec![param!("start", Float), param_multi!("values", Float)], Function::Native(_lt)));
    strings.push("<".into());
    vars.insert("<".into(), Value::Function(functions.len()));
    functions.push(defs);
    // >
    let mut defs = vec![];
    defs.push((vec![param!("start", Int), param_multi!("values", Int)], Function::Native(_gt)));
    defs.push((vec![param!("start", Float), param_multi!("values", Float)], Function::Native(_gt)));
    strings.push(">".into());
    vars.insert(">".into(), Value::Function(functions.len()));
    functions.push(defs);
    // <=
    let mut defs = vec![];
    defs.push((vec![param!("start", Int), param_multi!("values", Int)], Function::Native(_le)));
    defs.push((vec![param!("start", Float), param_multi!("values", Float)], Function::Native(_le)));
    strings.push("<=".into());
    vars.insert("<=".into(), Value::Function(functions.len()));
    functions.push(defs);
    // >=
    let mut defs = vec![];
    defs.push((vec![param!("start", Int), param_multi!("values", Int)], Function::Native(_ge)));
    defs.push((vec![param!("start", Float), param_multi!("values", Float)], Function::Native(_ge)));
    strings.push(">=".into());
    vars.insert(">=".into(), Value::Function(functions.len()));
    functions.push(defs);
    
    // union
    let mut defs = vec![];
    defs.push((vec![param_multi!("values", Type)], Function::Native(_union)));
    strings.push("union".into());
    vars.insert("union".into(), Value::Function(functions.len()));
    functions.push(defs);
    
    // map
    let mut defs = vec![];
    defs.push((vec![param!("f", Closure), param!("values", Vector)], Function::Native(_map)));
    defs.push((vec![param!("values", Vector), param!("f", Closure)], Function::Native(_map)));
    strings.push("map".into());
    vars.insert("map".into(), Value::Function(functions.len()));
    functions.push(defs);
    // reduce
    let mut defs = vec![];
    defs.push((vec![param!("f", Closure), param!("values", Vector)], Function::Native(_reduce)));
    defs.push((vec![param!("values", Vector), param!("f", Closure)], Function::Native(_reduce)));
    strings.push("reduce".into());
    vars.insert("reduce".into(), Value::Function(functions.len()));
    functions.push(defs);
    // filter
    let mut defs = vec![];
    defs.push((vec![param!("f", Closure), param!("values", Vector)], Function::Native(_filter)));
    defs.push((vec![param!("values", Vector), param!("f", Closure)], Function::Native(_filter)));
    strings.push("filter".into());
    vars.insert("filter".into(), Value::Function(functions.len()));
    functions.push(defs);

    Program::new(path, strings, closures, functions, vec![vars])
}

pub fn _set(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let Value::String(var) = program.var(&"var".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let value = program.var(&"value".into()).unwrap();
    let len = program.vars.len();
    if let Some(scope) = program.vars.get_mut(len - 2) {
        scope.insert(var, value);
    }
    Ok(Value::None)
}
pub fn _global(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let Value::String(var) = program.var(&"var".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let value = program.var(&"value".into()).unwrap();
    let len = program.vars.len();
    if let Some(scope) = program.vars.first_mut() {
        scope.insert(var, value);
    }
    Ok(Value::None)
}
pub fn _get(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let Value::String(var) = program.var(&"var".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    Ok(program.var(&var).unwrap_or_default())
}
pub fn _def(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let Value::String(name) = program.var(&"name".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let Value::Params(params) = program.var(&"params".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let Value::Closure(closure) = program.var(&"closure".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let func = Function::Function(program.closures[closure].clone());
    let len = program.vars.len();
    if let Some(value) = program.var(&name) {
        if let Value::Function(addr) = value {
            let defs = program.functions.get_mut(addr).unwrap();
            defs.push((params, func));
        } else {
            return Err(Error::new(format!("{name:?} is already defined as a {}", value.typ()), program.path.clone(), Some(pos)))
        }
    } else {
        let addr = program.new_func(params, func);
        if let Some(scope) = program.vars.get_mut(len - 2) {
            scope.insert(name, Value::Function(addr));
        }
    }
    Ok(Value::None)
}

pub fn _print(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let Value::Vector(x) = program.var(&"x".into()).unwrap() else {
        panic!("type checking doesn't work")
    };
    println!("{}", join!(x, " "));
    Ok(Value::None)
}

pub fn _add(program: &mut Program, pos: Position) -> Result<Value, Error> {
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
pub fn _sub(program: &mut Program, pos: Position) -> Result<Value, Error> {
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
pub fn _mul(program: &mut Program, pos: Position) -> Result<Value, Error> {
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
pub fn _div(program: &mut Program, pos: Position) -> Result<Value, Error> {
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

pub fn _eq(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let mut start = program.var(&"start".into()).unwrap();
    let Value::Vector(mut values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    while values.len() > 0 {
        let value = values.remove(0);
        if start != value {
            return Ok(Value::Bool(false))
        }
        start = value;
    }
    Ok(Value::Bool(true))
}
pub fn _ne(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let mut start = program.var(&"start".into()).unwrap();
    let Value::Vector(mut values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    while values.len() > 0 {
        let value = values.remove(0);
        if start == value {
            return Ok(Value::Bool(false))
        }
        start = value;
    }
    Ok(Value::Bool(true))
}
pub fn _lt(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let mut start = program.var(&"start".into()).unwrap();
    let Value::Vector(mut values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    while values.len() > 0 {
        let value = values.remove(0);
        match value {
            Value::Int(n2) => match &start {
                Value::Int(n1) => if n1 >= &n2 { return Ok(Value::Bool(false)) }
                Value::Float(n1) => if n1 >= &(n2 as f64) { return Ok(Value::Bool(false)) }
                _ => panic!("type checking doesn't work")
            }
            Value::Float(n2) => match &start {
                Value::Int(n1) => if *n1 as f64 >= n2 { return Ok(Value::Bool(false)) }
                Value::Float(n1) => if n1 >= &(n2 as f64) { return Ok(Value::Bool(false)) }
                _ => panic!("type checking doesn't work")
            }
            _ => panic!("type checking doesn't work")
        }
        start = value;
    }
    Ok(Value::Bool(true))
}
pub fn _gt(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let mut start = program.var(&"start".into()).unwrap();
    let Value::Vector(mut values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    while values.len() > 0 {
        let value = values.remove(0);
        match value {
            Value::Int(n2) => match &start {
                Value::Int(n1) => if n1 <= &n2 { return Ok(Value::Bool(false)) }
                Value::Float(n1) => if n1 <= &(n2 as f64) { return Ok(Value::Bool(false)) }
                _ => panic!("type checking doesn't work")
            }
            Value::Float(n2) => match &start {
                Value::Int(n1) => if *n1 as f64 <= n2 { return Ok(Value::Bool(false)) }
                Value::Float(n1) => if n1 <= &(n2 as f64) { return Ok(Value::Bool(false)) }
                _ => panic!("type checking doesn't work")
            }
            _ => panic!("type checking doesn't work")
        }
        start = value;
    }
    Ok(Value::Bool(true))
}
pub fn _le(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let mut start = program.var(&"start".into()).unwrap();
    let Value::Vector(mut values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    while values.len() > 0 {
        let value = values.remove(0);
        match value {
            Value::Int(n2) => match &start {
                Value::Int(n1) => if n1 > &n2 { return Ok(Value::Bool(false)) }
                Value::Float(n1) => if n1 > &(n2 as f64) { return Ok(Value::Bool(false)) }
                _ => panic!("type checking doesn't work")
            }
            Value::Float(n2) => match &start {
                Value::Int(n1) => if *n1 as f64 > n2 { return Ok(Value::Bool(false)) }
                Value::Float(n1) => if n1 > &(n2 as f64) { return Ok(Value::Bool(false)) }
                _ => panic!("type checking doesn't work")
            }
            _ => panic!("type checking doesn't work")
        }
        start = value;
    }
    Ok(Value::Bool(true))
}
pub fn _ge(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let mut start = program.var(&"start".into()).unwrap();
    let Value::Vector(mut values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    while values.len() > 0 {
        let value = values.remove(0);
        match value {
            Value::Int(n2) => match &start {
                Value::Int(n1) => if n1 < &n2 { return Ok(Value::Bool(false)) }
                Value::Float(n1) => if n1 < &(n2 as f64) { return Ok(Value::Bool(false)) }
                _ => panic!("type checking doesn't work")
            }
            Value::Float(n2) => match &start {
                Value::Int(n1) => if (*n1 as f64) < n2 { return Ok(Value::Bool(false)) }
                Value::Float(n1) => if n1 < &(n2 as f64) { return Ok(Value::Bool(false)) }
                _ => panic!("type checking doesn't work")
            }
            _ => panic!("type checking doesn't work")
        }
        start = value;
    }
    Ok(Value::Bool(true))
}

pub fn _union(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let Value::Vector(values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let mut types = vec![];
    for value in values {
        let Value::Type(typ) = value else {
            panic!("type checking doesn't work");
        };
        types.push(typ);
    }
    Ok(Value::Type(Type::Union(types)))
}

pub fn _map(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let Value::Closure(f_addr) = program.var(&"f".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let f = program.closures[f_addr].clone();
    let Value::Vector(values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let mut new_values = vec![];
    for value in values {
        program.setn(0, value);
        let new_value = program.execute(f.clone())?;
        new_values.push(new_value);
    }
    Ok(Value::Vector(new_values))
}
pub fn _reduce(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let Value::Closure(f_addr) = program.var(&"f".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let f = program.closures[f_addr].clone();
    let Value::Vector(values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let mut sum: Option<Value> = None;
    for value in values {
        if sum.is_some() {
            program.setn(0, sum.unwrap().clone());
            program.setn(1, value);
            sum = Some(program.execute(f.clone())?);
        } else {
            sum = Some(value);
        }
    }
    Ok(sum.unwrap_or_default())
}
pub fn _filter(program: &mut Program, pos: Position) -> Result<Value, Error> {
    let Value::Closure(f_addr) = program.var(&"f".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let f = program.closures[f_addr].clone();
    let Value::Vector(values) = program.var(&"values".into()).unwrap() else {
        panic!("type checking doesn't work");
    };
    let mut new_values = vec![];
    for value in values {
        program.setn(0, value.clone());
        let cond = program.execute(f.clone())?;
        if let Value::Bool(cond) = cond {
            if cond {
                new_values.push(value);
            }
        } else {
            return Err(Error::new(format!("expected closure to return a bool, got {}", cond.typ()), program.path.clone(), Some(pos)))
        }
    }
    Ok(Value::Vector(new_values))
}