pub mod ast;
pub mod mir;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Radix {
    Bin,
    Oct,
    Dec,
    Hex,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn default() -> Self {
        Self { start: 0, end: 0 }
    }
    pub fn from_pest(pest_span: pest::Span) -> Self {
        Self {
            start: pest_span.start(),
            end: pest_span.end(),
        }
    }
}
