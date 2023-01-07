use super::ast::{Chained, Expr, Field, FieldInit, Lit};

#[derive(Debug, Clone)]
pub enum Value {
    /// Unit type
    Unit,
    /// `Type`
    Type(u32),
    /// A Literal
    Literal(Lit),
    /// Chained namesapces
    Chained(Chained),
    /// Dot(indexing)
    Dot(Box<Value>, Box<Value>),
    /// Tuple type
    TupleTy(Vec<Field>),
    /// Tuple value
    Tuple(Vec<FieldInit>),
    /// Function type
    FnTy(Vec<Field>, Box<Value>),
    /// Function(lambda) value
    Fn(Vec<Field>, Box<Value>, Option<Box<Expr>>),
    /// Application(function call)
    Apply(Box<Value>, Vec<FieldInit>),
    /// Record type
    Record(Vec<Field>),
    /// Record initialization
    RecordInit(Chained, Vec<(String, Value)>),
}
