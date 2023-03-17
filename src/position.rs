use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub ln: Range<usize>,
    pub col: Range<usize>,
}