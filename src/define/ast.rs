use either::Either;

use super::context::{mk_empty_symtable, SymbolTablePtr};
use super::value::Value;

/// Binary operators
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    /// Power
    Pow,
    /// Shift left
    Shl,
    /// Shift right
    Shr,
    /// Fraction (rational number constructor)
    Frac,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Remainder
    Rem,
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Bitwise and
    BitAnd,
    /// Bitwise or
    BitOr,
    /// Bitwise xor
    BitXor,
    /// Piping argument to function
    Pipe,
    /// Equality
    Eq,
    /// Inequality
    Ne,
    /// Less than
    Lt,
    /// Less than or equal to
    Le,
    /// Greater than
    Gt,
    /// Greater than or equal to
    Ge,
    /// Logical and
    And,
    /// Logical or
    Or,
    /// As
    As,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// Positive
    Pos,
    /// Negative (unary minus)
    Neg,
    /// Logical not
    Not,
    /// Bitwise not
    BitNot,
    /// Reference
    Ref,
    /// Dereference
    Deref,
}

/// Literal kind
#[derive(Debug, Clone)]
pub enum LitKind {
    /// Integer literal
    Integer,
    /// Floating literal
    Floating,
}

/// A literal (number).
#[derive(Debug, Clone)]
pub struct Lit {
    /// Literal kind
    pub kind: LitKind,
    /// Radix for number
    pub radix: Radix,
    /// Text of literal
    pub text: String,
    /// Suffix for literal
    pub suffix: Option<Suffix>,
}

impl Lit {
    pub fn new(kind: LitKind, radix: Radix, text: String, suffix: Option<Suffix>) -> Self {
        Self {
            kind,
            radix,
            text,
            suffix,
        }
    }
}

/// Number radix
#[derive(Debug, Clone)]
pub enum Radix {
    Bin,
    Oct,
    Dec,
    Hex,
}

/// Literal suffix indicating type.
#[derive(Debug, Clone)]
pub enum Suffix {
    /// Signed
    I(u8),
    /// Unsigned
    U(u8),
    /// Float
    F(u8),
    /// Signed (platform depandent)
    ISize,
    /// Unsigned (platform depandent)
    USize,
}

/// Specification of parameter, let binding and type declaration.
#[derive(Debug, Clone)]
pub enum Spec {
    /// Mutable
    Mut,
    /// Extern
    Extern,
    /// Built-in
    Builtin,
    /// Compile-time
    Comptime,
    /// Implicit
    Implicit,
    /// Not specified
    None,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub spec: Spec,
    pub name: Option<String>,
    pub ty: Option<Expr>,
    pub with: Vec<String>,
}

impl Field {
    pub fn new(spec: Spec, name: Option<String>, ty: Option<Expr>, with: Vec<String>) -> Self {
        Self {
            spec,
            name,
            ty,
            with,
        }
    }
}

/// Field initializer(argument).
#[derive(Debug, Clone)]
pub struct FieldInit {
    pub name: Option<String>,
    pub expr: Expr,
}

impl FieldInit {
    pub fn new(name: Option<String>, expr: Expr) -> Self {
        Self { name, expr }
    }
}

/// Chained identifiers.
///
/// For example, `std::collections::HashMap`.
pub type Chained = Vec<String>;

#[derive(Debug, Clone)]
pub enum RangeBound {
    Literal(Lit),
    Chained(Vec<String>),
}

#[derive(Debug, Clone)]
pub enum RangeKind {
    Inclusive,
    Exclusive,
}

#[derive(Debug, Clone)]
pub enum RecordPatternElem {
    Field(String, Pattern),
    Rest,
}

/// Pattern kinds.
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Wildcard(`_`).
    Wildcard,
    /// Rest(`..`).
    Rest,
    /// Range.
    Range(RangeKind, RangeBound, RangeBound),
    /// A literal.
    Literal(Lit),
    /// A constructor.
    Constructor(Chained, Vec<Pattern>),
    /// A record.
    Record(Vec<RecordPatternElem>),
    /// A tuple.
    Tuple(Vec<Pattern>),
    /// Patterns chained by `|`.
    Or(Vec<Pattern>),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub params: Vec<Field>,
    pub ret: Option<Box<Expr>>,
    pub body: Box<Expr>,
    pub symtable: SymbolTablePtr,
}

impl Fn {
    pub fn new(params: Vec<Field>, ret: Option<Box<Expr>>, body: Box<Expr>) -> Self {
        Self {
            params,
            ret,
            body,
            symtable: mk_empty_symtable(),
        }
    }
}

/// Expression kinds.
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A unit type.
    ///
    /// This is used for `()` and `Unit`.
    Unit,
    /// A `Type[level]`.
    Type(u32),
    /// A Literal number.
    Literal(Lit),
    /// A chained(namespaces).
    ///
    /// This can represent variables, modules and more.
    Chained(Chained),
    /// A dot(indexing) expression.
    Dot(Box<Expr>, Box<Expr>),
    /// A binary expression.
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// An unary expression.
    Unary(UnaryOp, Box<Expr>),
    /// A tupleyao expression.
    Tuple(Vec<Either<Field, FieldInit>>),
    /// A function application expression.
    Apply(Box<Expr>, Vec<FieldInit>),
    /// A match expression.
    Match(Box<Expr>, Vec<(Pattern, Expr)>),
    /// An expression for function type.
    FnTy(Vec<Field>, Box<Expr>),
    /// A function expression.
    Fn(Fn),
    /// A block expression.
    Block(Block),
    /// A loop expression.
    Loop(Block),
    /// A while expression.
    While(Box<Expr>, Block),
    /// An if expression.
    If(Box<Expr>, Block, Option<Box<Expr>>),
    /// A record initialization expression.
    RecordInit(Chained, Vec<(String, Expr)>),
}

/// An expression.
#[derive(Debug, Clone)]
pub struct Expr {
    /// Expression kind
    pub kind: ExprKind,
    /// Type of expression
    pub ty: Option<Value>,
}

impl Expr {
    pub fn mk_unit() -> Self {
        Self {
            kind: ExprKind::Unit,
            ty: None,
        }
    }

    pub fn mk_type(level: u32) -> Self {
        Self {
            kind: ExprKind::Type(level),
            ty: None,
        }
    }

    pub fn mk_literal(lit: Lit) -> Self {
        Self {
            kind: ExprKind::Literal(lit),
            ty: None,
        }
    }

    pub fn mk_chained(chained: Chained) -> Self {
        Self {
            kind: ExprKind::Chained(chained),
            ty: None,
        }
    }

    pub fn mk_dot(lhs: Box<Expr>, rhs: Box<Expr>) -> Self {
        Self {
            kind: ExprKind::Dot(lhs, rhs),
            ty: None,
        }
    }

    pub fn mk_binary(op: BinOp, lhs: Box<Expr>, rhs: Box<Expr>) -> Self {
        Self {
            kind: ExprKind::Binary(op, lhs, rhs),
            ty: None,
        }
    }

    pub fn mk_unary(op: UnaryOp, expr: Box<Expr>) -> Self {
        Self {
            kind: ExprKind::Unary(op, expr),
            ty: None,
        }
    }

    pub fn mk_tuple(args: Vec<Either<Field, FieldInit>>) -> Self {
        Self {
            kind: ExprKind::Tuple(args),
            ty: None,
        }
    }

    pub fn mk_fn_ty(params: Vec<Field>, expr: Box<Expr>) -> Self {
        Self {
            kind: ExprKind::FnTy(params, expr),
            ty: None,
        }
    }

    pub fn mk_apply(expr: Box<Expr>, args: Vec<FieldInit>) -> Self {
        Self {
            kind: ExprKind::Apply(expr, args),
            ty: None,
        }
    }

    pub fn mk_match(expr: Box<Expr>, arms: Vec<(Pattern, Expr)>) -> Self {
        Self {
            kind: ExprKind::Match(expr, arms),
            ty: None,
        }
    }

    pub fn mk_fn(params: Vec<Field>, ret: Option<Box<Expr>>, body: Box<Expr>) -> Self {
        Self {
            kind: ExprKind::Fn(Fn::new(params, ret, body)),
            ty: None,
        }
    }

    pub fn mk_block(block: Block) -> Self {
        Self {
            kind: ExprKind::Block(block),
            ty: None,
        }
    }

    pub fn mk_loop(body: Block) -> Self {
        Self {
            kind: ExprKind::Loop(body),
            ty: None,
        }
    }

    pub fn mk_while(cond: Box<Expr>, body: Block) -> Self {
        Self {
            kind: ExprKind::While(cond, body),
            ty: None,
        }
    }

    pub fn mk_if(cond: Box<Expr>, then: Block, else_: Option<Box<Expr>>) -> Self {
        Self {
            kind: ExprKind::If(cond, then, else_),
            ty: None,
        }
    }

    pub fn mk_record_init(chained: Chained, fields: Vec<(String, Expr)>) -> Self {
        Self {
            kind: ExprKind::RecordInit(chained, fields),
            ty: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub symtable: SymbolTablePtr,
}

impl Block {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self {
            stmts,
            symtable: mk_empty_symtable(),
        }
    }
}

/// Statement kinds.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// An item
    Item(Item),
    /// An expression.
    Expr(Expr),
    /// Assign an expression to a variable(from an expression).
    ///
    /// Assign is treated as a statement and does not has a type nor a value.
    Assign(Expr, Expr),
}

/// A data constructor
#[derive(Debug, Clone)]
pub struct Constructor {
    /// Name of the constructor
    pub name: String,
    pub params: Vec<Field>,
    pub ty: Option<Expr>,
}

impl Constructor {
    pub fn new(name: String, params: Vec<Field>, ty: Option<Expr>) -> Self {
        Self { name, params, ty }
    }
}

/// Type declaration body.
#[derive(Debug, Clone)]
pub enum TypeBody {
    /// A record with fields.
    Record(Vec<Field>),
    /// Data constructors.
    Constructors(Vec<Constructor>),
}

/// An use tree.
#[derive(Debug, Clone)]
pub struct UseTree {
    /// Chained identifiers.
    pub chained: Chained,
    /// Name alias.
    pub alias: Option<String>,
    /// Sub trees.
    pub children: Vec<UseTree>,
    /// Whether to import all the items in the module.
    pub all: bool,
}

impl UseTree {
    pub fn new(chained: Chained, alias: Option<String>, children: Vec<UseTree>, all: bool) -> Self {
        Self {
            chained,
            alias,
            children,
            all,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub items: Vec<Item>,
    pub symtable: SymbolTablePtr,
}

impl Module {
    pub fn new(name: String, items: Vec<Item>) -> Self {
        Self {
            name,
            items,
            symtable: mk_empty_symtable(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub spec: Spec,
    pub name: String,
    pub params: Vec<Field>,
    pub ty: Option<Expr>,
    pub body: Option<TypeBody>,
    pub symtable: SymbolTablePtr,
}

impl Type {
    pub fn new(
        spec: Spec,
        name: String,
        params: Vec<Field>,
        ty: Option<Expr>,
        body: Option<TypeBody>,
    ) -> Self {
        Self {
            spec,
            name,
            params,
            ty,
            body,
            symtable: mk_empty_symtable(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: String,
    pub params: Vec<Field>,
    pub items: Vec<Item>,
    pub symtable: SymbolTablePtr,
}

impl Trait {
    pub fn new(name: String, params: Vec<Field>, items: Vec<Item>) -> Self {
        Self {
            name,
            params,
            items,
            symtable: mk_empty_symtable(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Let {
    pub spec: Spec,
    pub name: String,
    pub ty: Option<Expr>,
    pub init: Option<Expr>,
}

impl Let {
    pub fn new(spec: Spec, name: String, ty: Option<Expr>, init: Option<Expr>) -> Self {
        Self {
            spec,
            name,
            ty,
            init,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub name: String,
    pub args: Vec<Field>,
    pub inits: Vec<(String, Expr)>,
}

impl Instance {
    pub fn new(name: String, args: Vec<Field>, inits: Vec<(String, Expr)>) -> Self {
        Self { name, args, inits }
    }
}

/// Item kinds.
#[derive(Debug, Clone)]
pub enum ItemKind {
    /// An use declaration.
    Use(UseTree),
    /// An import.
    Import(String),
    /// A module.
    Module(Module),
    /// Type declaration.
    ///
    /// The parameters are:
    /// - `spec`: The specification of the type declaration.
    /// - `name`: The name of the type declaration.
    /// - `params`: The type parameters of the type declaration.
    /// - `ty`: The type of the type declaration, `Type` if not specified.
    /// - `body`: The body of the type declaration.
    ///   If the body is not specified, there will be no constructors for the type.
    Type(Type),
    /// Let binding.
    ///
    /// The parameters are:
    /// - `spec`: The specification of the let binding.
    /// - `name`: The name of the let binding.
    /// - `ty`: The type of the let binding.
    ///   If the type is not specified, the type is inferred from the context.
    /// - `expr`: The initial expression of the let binding.
    Let(Let),
    /// Trait declaration.
    Trait(Trait),
    /// Instantiation of a trait.
    Instance(Instance),
}

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
}

impl Item {
    pub fn mk_use(tree: UseTree) -> Self {
        Self {
            kind: ItemKind::Use(tree),
        }
    }

    pub fn mk_import(name: String) -> Self {
        Self {
            kind: ItemKind::Import(name),
        }
    }

    pub fn mk_module(name: String, items: Vec<Item>) -> Self {
        Self {
            kind: ItemKind::Module(Module::new(name, items)),
        }
    }

    pub fn mk_type(
        spec: Spec,
        name: String,
        params: Vec<Field>,
        ty: Option<Expr>,
        body: Option<TypeBody>,
    ) -> Self {
        Self {
            kind: ItemKind::Type(Type::new(spec, name, params, ty, body)),
        }
    }

    pub fn mk_let(spec: Spec, name: String, ty: Option<Expr>, init: Option<Expr>) -> Self {
        Self {
            kind: ItemKind::Let(Let::new(spec, name, ty, init)),
        }
    }

    pub fn mk_trait(name: String, params: Vec<Field>, items: Vec<Item>) -> Self {
        Self {
            kind: ItemKind::Trait(Trait::new(name, params, items)),
        }
    }

    pub fn mk_instance(name: String, args: Vec<Field>, inits: Vec<(String, Expr)>) -> Self {
        Self {
            kind: ItemKind::Instance(Instance::new(name, args, inits)),
        }
    }
}

/// A compilation unit.
///
/// This is the root of the whole AST.
#[derive(Debug, Clone)]
pub struct CompUnit {
    pub items: Vec<Item>,
    pub symtable: SymbolTablePtr,
}

impl CompUnit {
    pub fn new(items: Vec<Item>) -> Self {
        Self {
            items,
            symtable: mk_empty_symtable(),
        }
    }
}
