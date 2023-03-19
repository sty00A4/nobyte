use std::{ops::Range, fmt::{Debug, Display}};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Position {
    pub ln: Range<usize>,
    pub col: Range<usize>,
}
impl Position {
    pub fn new(ln: Range<usize>, col: Range<usize>) -> Self {
        Self { ln, col }
    }
    pub fn extend(&mut self, pos: &Self) {
        self.ln.end = pos.ln.end;
        self.col.end = pos.col.end;
    }
}

pub struct Located<T> {
    item: T,
    pos: Position
}
impl<T> Located<T> {
    pub fn new(item: T, pos: Position) -> Self {
        Self { item, pos }
    }
}
impl<T: Debug> Debug for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.item)
    }
}
impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.item)
    }
}
impl<T: Clone> Clone for Located<T> {
    fn clone(&self) -> Self {
        Self::new(self.item.clone(), self.pos.clone())
    }
}
impl<T: PartialEq> PartialEq for Located<T> {
    fn eq(&self, other: &Self) -> bool {
        self.item == other.item && self.pos == self.pos
    }
}
impl<T: Default> Default for Located<T> {
    fn default() -> Self {
        Self::new(T::default(), Position::default())
    }
}
impl<T> From<T> for Located<T> {
    fn from(value: T) -> Self {
        Self::new(value, Position::default())
    }
}