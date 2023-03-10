//! Abstract syntax tree (AST) definitions.

use super::{Radix, Span};
use pest;

/// Binary operators
#[derive(Debug)]
pub enum BinOp {
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
  /// Path `===`
  Path,
  /// Min of intervals
  IntervalMin,
  /// Max of intervals
  IntervalMax,
}

/// Unary operators
#[derive(Debug)]
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
  /// Negation of intervals
  IntervalNeg,
}

#[derive(Debug)]
pub enum RangeKind {
  /// a..b
  Exclusive,
  /// a..=b
  Inclusive,
  /// ..b
  To,
  /// a..
  From,
  /// ..=b
  ToInclusive,
  /// ..
  Full,
}

#[derive(Debug)]
pub struct FnArg {
  pub name: Option<String>,
  pub expr: Expr,
}

impl FnArg {
  pub fn new(name: Option<String>, expr: Expr) -> Self {
    Self { name, expr }
  }
}

#[derive(Debug)]
pub struct FnParam {
  pub name: Option<String>,
  pub ty: Option<Expr>,
  pub implicit: bool,
}

impl FnParam {
  pub fn new(name: Option<String>, ty: Option<Expr>, implicit: bool) -> Self {
    Self { name, ty, implicit }
  }
}

#[derive(Debug)]
pub enum LitKind {
  Integer,
  Floating,
}

#[derive(Debug)]
pub enum LitSuffix {
  I8,
  I16,
  I32,
  I64,
  U8,
  U16,
  U32,
  U64,
  F32,
  F64,
  ISize,
  USize,
}

#[derive(Debug)]
pub struct Lit {
  pub kind: LitKind,
  pub radix: Radix,
  pub suffix: Option<LitSuffix>,
  pub text: String,
}

#[derive(Debug)]
pub struct Fn {
  pub name: Option<String>,
  /// Parameters
  pub params: Vec<FnParam>,
  /// Return type.
  pub ret_ty: Option<Box<Expr>>,
  /// Function body.
  /// If `None`, this is the left hand side (just type) of a function.
  pub expr: Option<Box<Expr>>,
}

impl Fn {
  pub fn new(
    name: Option<String>,
    params: Vec<FnParam>,
    ret_ty: Option<Expr>,
    expr: Option<Expr>,
  ) -> Self {
    Self {
      name,
      params,
      ret_ty: ret_ty.map(Box::new),
      expr: expr.map(Box::new),
    }
  }
}

pub type Label = String;

#[derive(Debug)]
pub struct Block {
  pub label: Option<Label>,
  pub stmts: Vec<Stmt>,
  pub comptime: bool,
}

impl Block {
  pub fn new(label: Option<Label>, stmts: Vec<Stmt>, comptime: bool) -> Self {
    Self {
      label,
      stmts,
      comptime,
    }
  }
}

#[derive(Debug)]
pub struct IfLet {
  pub pat: Pat,
  pub expr: Box<Expr>,
  pub block: Block,
  pub else_: Option<Box<Expr>>,
}

impl IfLet {
  pub fn new(pat: Pat, expr: Expr, block: Block, else_: Option<Expr>) -> Self {
    Self {
      pat,
      expr: Box::new(expr),
      block,
      else_: else_.map(Box::new),
    }
  }
}

#[derive(Debug)]
pub struct MatchArm {
  pub pat: Pat,
  pub expr: Expr,
}

impl MatchArm {
  pub fn new(pat: Pat, expr: Expr) -> Self {
    Self { pat, expr }
  }
}

#[derive(Debug)]
pub enum ExprKind {
  /// `Type`
  Universe,
  /// `Unit`
  Unit,
  /// Literal
  Lit(Lit),
  /// Identifier
  Ident(String),
  /// Range
  Range(RangeKind, Option<Box<Expr>>, Option<Box<Expr>>),
  /// Binary expression.
  Binary(BinOp, Box<Expr>, Box<Expr>),
  /// Unary expression.
  Unary(UnaryOp, Box<Expr>),
  /// Qualified path of namespaces.
  Qualify(Box<Expr>, Box<Expr>),
  /// Access by dot
  Access(Box<Expr>, Box<Expr>),
  /// Application.
  Apply(Box<Expr>, Vec<FnArg>),
  /// Tuple.
  Tuple(Vec<Expr>),
  /// Function type.
  FnTy(Vec<FnParam>, Box<Expr>),
  /// Function
  Fn(Fn),
  /// Return
  Return(Option<Box<Expr>>),
  /// Break expression with optional label and expression(value).
  Break(Option<Label>, Option<Box<Expr>>),
  /// Continue exprssion with optional label.
  Continue(Option<Label>),
  /// Assignment
  Assign(Box<Expr>, Box<Expr>),
  /// Block
  Block(Block),
  /// Loop
  Loop(Option<Label>, Block),
  /// While
  While(Option<Label>, Box<Expr>, Block),
  /// For
  For(Option<Label>, Pat, Box<Expr>, Block),
  /// If
  If(Box<Expr>, Block, Option<Box<Expr>>),
  /// If let.
  IfLet(IfLet),
  /// Match
  Match(Box<Expr>, Vec<MatchArm>),
  /// Struct Initialization
  Struct(Box<Expr>, Vec<(String, Expr)>),
}

#[derive(Debug)]
pub struct Expr {
  pub kind: ExprKind,
  pub span: Span,
}

impl Expr {
  pub fn set_span(&mut self, line_col: (usize, usize), span: pest::Span) {
    self.span = Span::from_pest(line_col, span);
  }

  pub fn mk_range(
    range: RangeKind,
    lhs: Option<Expr>,
    rhs: Option<Expr>,
  ) -> Self {
    Self {
      kind: ExprKind::Range(range, lhs.map(Box::new), rhs.map(Box::new)),
      span: Span::default(),
    }
  }

  pub fn mk_binary(op: BinOp, lhs: Expr, rhs: Expr) -> Self {
    Self {
      kind: ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
      span: Span::default(),
    }
  }

  pub fn mk_unary(op: UnaryOp, expr: Expr) -> Self {
    Self {
      kind: ExprKind::Unary(op, Box::new(expr)),
      span: Span::default(),
    }
  }

  pub fn mk_qualify_expr(from: Expr, to: Expr) -> Self {
    Self {
      kind: ExprKind::Qualify(Box::new(from), Box::new(to)),
      span: Span::default(),
    }
  }

  pub fn mk_access(from: Expr, to: Expr) -> Self {
    Self {
      kind: ExprKind::Access(Box::new(from), Box::new(to)),
      span: Span::default(),
    }
  }

  pub fn mk_apply(expr: Expr, args: Vec<FnArg>) -> Self {
    Self {
      kind: ExprKind::Apply(Box::new(expr), args),
      span: Span::default(),
    }
  }
  pub fn mk_universe() -> Self {
    Self {
      kind: ExprKind::Universe,
      span: Span::default(),
    }
  }

  pub fn mk_unit() -> Self {
    Self {
      kind: ExprKind::Unit,
      span: Span::default(),
    }
  }

  pub fn mk_ident(name: String) -> Self {
    Self {
      kind: ExprKind::Ident(name),
      span: Span::default(),
    }
  }

  pub fn mk_tuple(exprs: Vec<Expr>) -> Self {
    Self {
      kind: ExprKind::Tuple(exprs),
      span: Span::default(),
    }
  }

  pub fn mk_fn_ty(params: Vec<FnParam>, ret_ty: Expr) -> Self {
    Self {
      kind: ExprKind::FnTy(params, Box::new(ret_ty)),
      span: Span::default(),
    }
  }

  pub fn mk_lit(lit: Lit) -> Self {
    Self {
      kind: ExprKind::Lit(lit),
      span: Span::default(),
    }
  }

  pub fn mk_return(expr: Option<Expr>) -> Self {
    Self {
      kind: ExprKind::Return(expr.map(Box::new)),
      span: Span::default(),
    }
  }

  pub fn mk_break(label: Option<String>, expr: Option<Expr>) -> Self {
    Self {
      kind: ExprKind::Break(label, expr.map(Box::new)),
      span: Span::default(),
    }
  }

  pub fn mk_continue(label: Option<String>) -> Self {
    Self {
      kind: ExprKind::Continue(label),
      span: Span::default(),
    }
  }

  pub fn mk_assign(lhs: Expr, rhs: Expr) -> Self {
    Self {
      kind: ExprKind::Assign(Box::new(lhs), Box::new(rhs)),
      span: Span::default(),
    }
  }

  pub fn mk_fn(func: Fn) -> Self {
    Self {
      kind: ExprKind::Fn(func),
      span: Span::default(),
    }
  }

  pub fn mk_block(block: Block) -> Self {
    Self {
      kind: ExprKind::Block(block),
      span: Span::default(),
    }
  }

  pub fn mk_loop(label: Option<Label>, block: Block) -> Self {
    Self {
      kind: ExprKind::Loop(label, block),
      span: Span::default(),
    }
  }

  pub fn mk_while(label: Option<Label>, cond: Expr, block: Block) -> Self {
    Self {
      kind: ExprKind::While(label, Box::new(cond), block),
      span: Span::default(),
    }
  }

  pub fn mk_if(cond: Expr, then_block: Block, else_: Option<Expr>) -> Self {
    Self {
      kind: ExprKind::If(Box::new(cond), then_block, else_.map(Box::new)),
      span: Span::default(),
    }
  }

  pub fn mk_for(
    label: Option<Label>,
    pat: Pat,
    expr: Expr,
    block: Block,
  ) -> Self {
    Self {
      kind: ExprKind::For(label, pat, Box::new(expr), block),
      span: Span::default(),
    }
  }

  pub fn mk_if_let(if_let: IfLet) -> Self {
    Self {
      kind: ExprKind::IfLet(if_let),
      span: Span::default(),
    }
  }

  pub fn mk_match(expr: Expr, arms: Vec<MatchArm>) -> Self {
    Self {
      kind: ExprKind::Match(Box::new(expr), arms),
      span: Span::default(),
    }
  }

  pub fn mk_struct(qualified: Expr, fields: Vec<(String, Expr)>) -> Self {
    Self {
      kind: ExprKind::Struct(Box::new(qualified), fields),
      span: Span::default(),
    }
  }
}

#[derive(Debug)]
pub enum IdentPatSpec {
  Comptime,
  Ref,
  Mut,
  RefMut,
  None,
}

#[derive(Debug)]
pub enum StructPatElem {
  Field(String, Pat),
  Rest,
}

#[derive(Debug)]
pub enum RangePatBound {
  Lit(Lit),
  Qualify(Box<Expr>),
}

#[derive(Debug)]
pub struct ConstructorPatArg {
  pub name: Option<String>,
  pub pat: Pat,
}

impl ConstructorPatArg {
  pub fn new(name: Option<String>, pat: Pat) -> Self {
    Self { name, pat }
  }
}

#[derive(Debug)]
pub enum RangePatKind {
  /// a..b
  Exclusive,
  /// a..=b
  Inclusive,
  /// ..b
  To,
  /// a..
  From,
  /// ..=b
  ToInclusive,
}

#[derive(Debug)]
pub enum PatKind {
  Lit(Lit),
  Wildcard,
  Rest,
  Ident(IdentPatSpec, String),
  Struct(Box<Expr>, Vec<StructPatElem>),
  Constructor(Box<Expr>, Vec<ConstructorPatArg>),
  Tuple(Vec<Pat>),
  Qualify(Box<Expr>),
  Range(RangePatKind, Option<RangePatBound>, Option<RangePatBound>),
  Or(Vec<Pat>),
}

#[derive(Debug)]
pub struct Pat {
  pub kind: PatKind,
  pub span: Span,
}

impl Pat {
  pub fn set_span(&mut self, line_col: (usize, usize), span: pest::Span) {
    self.span = Span::from_pest(line_col, span);
  }

  pub fn mk_lit(lit: Lit) -> Self {
    Self {
      kind: PatKind::Lit(lit),
      span: Span::default(),
    }
  }

  pub fn mk_wildcard() -> Self {
    Self {
      kind: PatKind::Wildcard,
      span: Span::default(),
    }
  }

  pub fn mk_rest() -> Self {
    Self {
      kind: PatKind::Rest,
      span: Span::default(),
    }
  }

  pub fn mk_ident(spec: IdentPatSpec, name: String) -> Self {
    Self {
      kind: PatKind::Ident(spec, name),
      span: Span::default(),
    }
  }

  pub fn mk_struct(qualified: Expr, elems: Vec<StructPatElem>) -> Self {
    Self {
      kind: PatKind::Struct(Box::new(qualified), elems),
      span: Span::default(),
    }
  }

  pub fn mk_tuple(elems: Vec<Pat>) -> Self {
    Self {
      kind: PatKind::Tuple(elems),
      span: Span::default(),
    }
  }

  pub fn mk_qualify(qualified: Expr) -> Self {
    Self {
      kind: PatKind::Qualify(Box::new(qualified)),
      span: Span::default(),
    }
  }

  pub fn mk_or(pats: Vec<Pat>) -> Self {
    Self {
      kind: PatKind::Or(pats),
      span: Span::default(),
    }
  }

  pub fn mk_range(
    kind: RangePatKind,
    lhs: Option<RangePatBound>,
    rhs: Option<RangePatBound>,
  ) -> Self {
    Self {
      kind: PatKind::Range(kind, lhs, rhs),
      span: Span::default(),
    }
  }

  pub fn mk_constructor(qualified: Expr, args: Vec<ConstructorPatArg>) -> Self {
    Self {
      kind: PatKind::Constructor(Box::new(qualified), args),
      span: Span::default(),
    }
  }
}

#[derive(Debug)]
pub struct Let {
  pub pat: Pat,
  pub ty: Option<Expr>,
  pub init: Option<Expr>,
}

impl Let {
  pub fn new(pat: Pat, ty: Option<Expr>, init: Option<Expr>) -> Self {
    Self { pat, ty, init }
  }
}

#[derive(Debug)]
pub enum StmtKind {
  Let(Let),
  Item(Item),
  Expr(Expr),
}

#[derive(Debug)]
pub struct Stmt {
  pub kind: StmtKind,
  pub span: Span,
}

impl Stmt {
  pub fn set_span(&mut self, line_col: (usize, usize), span: pest::Span) {
    self.span = Span::from_pest(line_col, span);
  }

  pub fn mk_let(decl: Let) -> Self {
    Self {
      kind: StmtKind::Let(decl),
      span: Span::default(),
    }
  }

  pub fn mk_item(item: Item) -> Self {
    Self {
      kind: StmtKind::Item(item),
      span: Span::default(),
    }
  }

  pub fn mk_expr(expr: Expr) -> Self {
    Self {
      kind: StmtKind::Expr(expr),
      span: Span::default(),
    }
  }
}

#[derive(Debug)]
pub struct UseTree {
  pub name: String,
  pub alias: Option<String>,
  pub use_all: bool,
  pub children: Vec<UseTree>,
}

impl UseTree {
  pub fn new(
    name: String,
    alias: Option<String>,
    use_all: bool,
    children: Vec<UseTree>,
  ) -> Self {
    Self {
      name,
      alias,
      use_all,
      children,
    }
  }
}

#[derive(Debug)]
pub struct Def {
  pub builtin: bool,
  pub body: Fn,
}

impl Def {
  pub fn new(builtin: bool, body: Fn) -> Self {
    Self { builtin, body }
  }
}

#[derive(Debug)]
pub struct Const {
  pub name: String,
  pub ty: Expr,
  pub init: Expr,
}

impl Const {
  pub fn new(name: String, ty: Expr, init: Expr) -> Self {
    Self { name, ty, init }
  }
}

#[derive(Debug)]
pub struct Module {
  pub name: String,
  pub items: Vec<Item>,
}

impl Module {
  pub fn new(name: String, items: Vec<Item>) -> Self {
    Self { name, items }
  }
}

#[derive(Debug)]
pub enum TypeBody {
  Struct(Vec<(String, Expr)>),
  Expr(Expr),
  Constructors(Vec<(String, Option<Fn>)>),
}

#[derive(Debug)]
pub struct Type {
  pub builtin: bool,
  pub name: String,
  pub ty: Option<Fn>,
  pub body: Option<TypeBody>,
}

impl Type {
  pub fn new(
    builtin: bool,
    name: String,
    ty: Option<Fn>,
    body: Option<TypeBody>,
  ) -> Self {
    Self {
      builtin,
      name,
      ty,
      body,
    }
  }
}

#[derive(Debug)]
pub struct Interface {
  pub name: String,
  pub params: Option<Vec<FnParam>>,
  pub body: Vec<Item>,
}

impl Interface {
  pub fn new(
    name: String,
    params: Option<Vec<FnParam>>,
    body: Vec<Item>,
  ) -> Self {
    Self { name, params, body }
  }
}

#[derive(Debug)]
pub enum ItemKind {
  Use(UseTree),
  Import(String),
  Def(Def),
  Const(Const),
  Type(Type),
  Impl(Expr, Vec<Item>),
  Module(Module),
  Interface(Interface),
}

#[derive(Debug)]
pub struct Item {
  pub kind: ItemKind,
  pub span: Span,
}

impl Item {
  pub fn set_span(&mut self, line_col: (usize, usize), span: pest::Span) {
    self.span = Span::from_pest(line_col, span);
  }

  pub fn mk_def(def: Def) -> Self {
    Self {
      kind: ItemKind::Def(def),
      span: Span::default(),
    }
  }

  pub fn mk_import(name: String) -> Self {
    Self {
      kind: ItemKind::Import(name),
      span: Span::default(),
    }
  }

  pub fn mk_use(tree: UseTree) -> Self {
    Self {
      kind: ItemKind::Use(tree),
      span: Span::default(),
    }
  }

  pub fn mk_module(module: Module) -> Self {
    Self {
      kind: ItemKind::Module(module),
      span: Span::default(),
    }
  }

  pub fn mk_impl(expr: Expr, items: Vec<Item>) -> Self {
    Self {
      kind: ItemKind::Impl(expr, items),
      span: Span::default(),
    }
  }

  pub fn mk_const(decl: Const) -> Self {
    Self {
      kind: ItemKind::Const(decl),
      span: Span::default(),
    }
  }

  pub fn mk_type(ty: Type) -> Self {
    Self {
      kind: ItemKind::Type(ty),
      span: Span::default(),
    }
  }

  pub fn mk_interface(interface: Interface) -> Self {
    Self {
      kind: ItemKind::Interface(interface),
      span: Span::default(),
    }
  }
}

#[derive(Debug)]
pub struct CompUnit {
  pub items: Vec<Item>,
  pub span: Span,
}

impl CompUnit {
  pub fn new(
    items: Vec<Item>,
    line_col: (usize, usize),
    span: pest::Span,
  ) -> Self {
    Self {
      items,
      span: Span::from_pest(line_col, span),
    }
  }
}
