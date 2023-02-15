use gc::{Finalize, Trace};

pub mod ast;
pub mod mir;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Trace, Finalize)]
pub enum Radix {
  Bin,
  Oct,
  Dec,
  Hex,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Trace, Finalize)]
pub struct Span {
  pub lineno: usize,
  pub start: usize,
  pub length: usize,
}

impl Span {
  pub fn default() -> Self {
    Self {
      lineno: 0,
      start: 0,
      length: 0,
    }
  }

  pub fn from_pest(line_col: (usize, usize), pest_span: pest::Span) -> Self {
    let lineno = line_col.0;
    let start = line_col.1;
    let length = pest_span.end() - pest_span.start();
    Self {
      lineno,
      start,
      length,
    }
  }
}
