use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Instr {
    Halt,
    Int(i64), Float(f64), Bool(bool), Char(char),
    String(usize), Closure(usize),
    Var(usize), Params(usize), Call(usize)
}
impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Halt =>          write!(f, "HALT"),
            Self::Int(int) =>      write!(f, "INT       {int:?}"),
            Self::Float(float) =>  write!(f, "FLOAT     {float:?}"),
            Self::Bool(bool) =>    write!(f, "BOOL      {bool:?}"),
            Self::Char(char) =>    write!(f, "CHAR      {char:?}"),
            Self::String(addr) =>  write!(f, "STRING    #{addr:x?}"),
            Self::Closure(addr) => write!(f, "CLOSURE   #{addr:x?}"),
            Self::Var(addr) =>     write!(f, "VAR       #{addr:x?}"),
            Self::Params(len) =>   write!(f, "PARAMS    {len}"),
            Self::Call(len) =>     write!(f, "CALL      {len}"),
        }
    }
}