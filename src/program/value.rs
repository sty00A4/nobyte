use std::fmt::{Display, Debug};
use crate::{join_debug, join};
use super::program::Param;

#[derive(Clone)]
pub enum Value {
    None,
    Int(i64), Float(f64), Bool(bool), Char(char), String(String), Type(Type),
    Vector(Vec<Self>),
    Closure(usize),
    Function(usize), Params(Vec<Param>)
}
impl Value {
    pub fn typ(&self) -> Type {
        match self {
            Self::None => Type::None,
            Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::Bool(_) => Type::Bool,
            Self::Char(_) => Type::Char,
            Self::String(_) => Type::String,
            Self::Type(_) => Type::Type,
            Self::Vector(_) => Type::Vector,
            Self::Closure(_) => Type::Closure,
            Self::Function(_) => Type::Function,
            Self::Params(_) => Type::Params,
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::Char(v) => write!(f, "{v}"),
            Self::String(v) => write!(f, "{v}"),
            Self::Type(v) => write!(f, "{v}"),
            Self::Vector(values) => write!(f, "[{}]", join_debug!(values, ", ")),
            Self::Closure(addr) => write!(f, "closure:{addr:x}"),
            Self::Function(addr) => write!(f, "function:{addr:x}"),
            Self::Params(params) => write!(f, "params:({})", join!(params, " ")),
        }
    }
}
impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Int(v) => write!(f, "{v:?}"),
            Self::Float(v) => write!(f, "{v:?}"),
            Self::Bool(v) => write!(f, "{v:?}"),
            Self::Char(v) => write!(f, "{v:?}"),
            Self::String(v) => write!(f, "{v:?}"),
            Self::Type(v) => write!(f, "{v:?}"),
            Self::Vector(values) => write!(f, "[{}]", join_debug!(values, ", ")),
            Self::Closure(addr) => write!(f, "closure:{addr:x}"),
            Self::Function(addr) => write!(f, "function:{addr:x}"),
            Self::Params(params) => write!(f, "params:({})", join!(params, " ")),
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::None, Self::None) => true,
            (Self::Int(v1), Self::Int(v2)) => v1 == v2,
            (Self::Float(v1), Self::Float(v2)) => v1 == v2,
            (Self::Bool(v1), Self::Bool(v2)) => v1 == v2,
            (Self::Char(v1), Self::Char(v2)) => v1 == v2,
            (Self::String(v1), Self::String(v2)) => v1 == v2,
            (Self::Vector(v1), Self::Vector(v2)) => v1 == v2,
            (Self::Closure(addr1), Self::Closure(addr2)) => addr1 == addr2,
            (Self::Function(addr1), Self::Function(addr2)) => addr1 == addr2,
            (Self::Params(params1), Self::Params(params2)) => params1 == params2,
            _ => false
        }
    }
}
impl Default for Value {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Clone, Eq, Hash)]
pub enum Type {
    None, Any,
    Int, Float, Bool, Char, String, Vector,
    Closure, Function, Params,
    Type,
    Union(Vec<Type>)
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Any => write!(f, "any"),
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::String => write!(f, "str"),
            Self::Vector => write!(f, "vec"),
            Self::Closure => write!(f, "closure"),
            Self::Function => write!(f, "function"),
            Self::Params => write!(f, "params"),
            Self::Type => write!(f, "type"),
            Self::Union(types) => write!(f, "{}", join!(types, "|")),
        }
    }
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Any, _) | (_, Self::Any) => true,
            (Self::None, Self::None) => true,
            (Self::Int, Self::Int) => true,
            (Self::Float, Self::Float) => true,
            (Self::Bool, Self::Bool) => true,
            (Self::Char, Self::Char) => true,
            (Self::String, Self::String) => true,
            (Self::Vector, Self::Vector) => true,
            (Self::Closure, Self::Closure) => true,
            (Self::Function, Self::Function) => true,
            (Self::Params, Self::Params) => true,
            (Self::Type, Self::Type) => true,
            (Self::Union(types1), Self::Union(types2)) => {
                for type2 in types2 {
                    if !types1.contains(type2) {
                        return false
                    }
                }
                true
            }
            (Self::Union(types), typ) => types.contains(typ),
            _ => false
        }
    }
}
impl Default for Type {
    fn default() -> Self {
        Self::None
    }
}