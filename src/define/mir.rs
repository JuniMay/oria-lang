pub mod check;
pub mod codegen;
pub mod pretty;

use self::check::TypeCheck;
use self::codegen::MirCodegenContext;
use super::{ast, Radix, Span};
use crate::{ptr, Counter, Ptr};
use std::{collections::HashMap, fmt};

pub type Level = u64;
pub type Label = String;

#[derive(Debug, Clone)]
pub struct FnParam {
  pub name: Option<String>,
  pub ty: Ptr<Value>,
  pub implicit: bool,
}

impl FnParam {
  pub fn new(name: Option<String>, ty: Ptr<Value>, implicit: bool) -> FnParam {
    FnParam { name, ty, implicit }
  }
}

#[derive(Debug, Clone)]
pub struct Fn {
  pub name: String,
  pub params: Vec<FnParam>,
  pub ret_ty: Ptr<Value>,
  pub body: Option<Block>,
  pub symbol_table: Ptr<SymbolTable>,
}

impl Fn {
  pub fn new(
    name: String,
    params: Vec<FnParam>,
    ret_ty: Ptr<Value>,
    body: Option<Block>,
    symbol_table: Ptr<SymbolTable>,
  ) -> Fn {
    Fn {
      name,
      params,
      ret_ty,
      body,
      symbol_table,
    }
  }
}

/// Value kinds.
#[derive(Debug, Clone)]
pub enum ValueKind {
  /// Unit type.
  Unit,
  /// Type
  Type(Level),
  /// Literal
  Lit(Radix, String),
  /// Identifier
  Ident(String),
  /// Function type
  FnTy(Vec<FnParam>, Ptr<Value>),
  /// Function
  Fn(Fn),
  /// Block
  Block(Block),
  /// Type variable.
  /// This is used for type inference.
  Var(u64),
}

/// A value.
#[derive(Debug, Clone)]
pub struct Value {
  /// The kind of the value.
  pub kind: ValueKind,
  /// The type of the value.
  /// If this is None, the type is not yet inferred.
  pub ty: Option<Ptr<Value>>,
  /// The span of the corresponding ast node and its source code.
  pub span: Option<Span>,
}

impl Value {
  pub(self) fn new(kind: ValueKind, ty: Option<Ptr<Value>>) -> Ptr<Value> {
    ptr(Value {
      kind,
      ty,
      span: None,
    })
  }

  /// Substitute the value kind.
  pub(self) fn substitute(&mut self, other: Ptr<Value>) {
    self.kind = other.borrow().kind.clone();
  }

  pub(self) fn maybe_type(&self) -> bool {
    match self.kind {
      ValueKind::Unit
      | ValueKind::Type(_)
      | ValueKind::Ident(_)
      | ValueKind::FnTy(..)
      | ValueKind::Var(_) => true,
      _ => false,
    }
  }

  pub(self) fn maybe_term(&self) -> bool {
    match self.kind {
      ValueKind::Lit(..)
      | ValueKind::Ident(_)
      | ValueKind::Fn(..)
      | ValueKind::Block(..)
      | ValueKind::Var(_) => true,
      _ => false,
    }
  }

  pub(self) fn is_var(&self) -> bool {
    match &self.kind {
      ValueKind::Var(_) => true,
      _ => false,
    }
  }

  pub(self) fn mk_unit() -> Ptr<Value> {
    Value::new(ValueKind::Unit, Some(Value::mk_type(0)))
  }

  /// Make a type value.
  /// The `ty` of type will be computed lazily.
  pub(self) fn mk_type(level: Level) -> Ptr<Value> {
    Value::new(ValueKind::Type(level), None)
  }

  pub(self) fn mk_lit(
    radix: Radix,
    text: String,
    ty: Ptr<Value>,
  ) -> Ptr<Value> {
    Value::new(ValueKind::Lit(radix, text), Some(ty))
  }

  pub(self) fn mk_ident(name: String) -> Ptr<Value> {
    Value::new(ValueKind::Ident(name), None)
  }

  pub(self) fn mk_fn_ty(params: Vec<FnParam>, ret: Ptr<Value>) -> Ptr<Value> {
    Value::new(ValueKind::FnTy(params, ret), Some(Value::mk_type(0)))
  }

  pub(self) fn mk_block(block: Block) -> Ptr<Value> {
    Value::new(ValueKind::Block(block), None)
  }

  pub(self) fn mk_fn(
    name: String,
    params: Vec<FnParam>,
    ty: Ptr<Value>,
    body: Option<Block>,
    symbol_table: Ptr<SymbolTable>,
  ) -> Ptr<Value> {
    Value::new(
      ValueKind::Fn(Fn::new(name, params, ty, body, symbol_table)),
      None,
    )
  }

  pub(self) fn mk_var(mir_codegen_ctx: &mut MirCodegenContext) -> Ptr<Value> {
    Value::new(ValueKind::Var(mir_codegen_ctx.value_var_cnt.next()), None)
  }

  /// Upgrades the type of the value to the next level.
  /// If the value is not a type, no change will be made.
  pub(self) fn level_up(&mut self) {
    if let ValueKind::Type(level) = self.kind {
      self.kind = ValueKind::Type(level + 1);
    }
  }
}

#[derive(Debug, Clone)]
pub enum VarSpec {
  Mutable,
  Comptime,
  None,
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
  Param(Ptr<Value>),
  Var(VarSpec, Option<Ptr<Value>>),
  Const(Ptr<Value>),
  Def(bool, Fn),
  Temporary(Ptr<Value>),
  Module(Ptr<Module>),
}

#[derive(Debug, Clone)]
pub struct Symbol {
  pub name: String,
  pub kind: SymbolKind,
}

impl Symbol {
  pub fn new(name: String, kind: SymbolKind) -> Ptr<Symbol> {
    ptr(Symbol { name, kind })
  }

  pub fn mk_temporary(name: String, value: Ptr<Value>) -> Ptr<Symbol> {
    Symbol::new(name, SymbolKind::Temporary(value))
  }

  pub fn mk_module(name: String, module: Ptr<Module>) -> Ptr<Symbol> {
    Symbol::new(name, SymbolKind::Module(module))
  }

  pub fn mk_param(name: String, ty: Ptr<Value>) -> Ptr<Symbol> {
    Symbol::new(name, SymbolKind::Param(ty))
  }

  pub fn mk_def(name: String, builtin: bool, func: Fn) -> Ptr<Symbol> {
    Symbol::new(name, SymbolKind::Def(builtin, func))
  }
}

#[derive(Clone)]
pub struct SymbolTable {
  pub table: HashMap<String, Ptr<Symbol>>,
  pub parent: Option<Ptr<SymbolTable>>,
}

impl fmt::Debug for SymbolTable {
  fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
    f.debug_struct("SymbolTable")
      .field("table", &self.table)
      .finish()
  }
}

impl SymbolTable {
  pub fn new(parent: Option<Ptr<SymbolTable>>) -> Ptr<SymbolTable> {
    ptr(SymbolTable {
      table: HashMap::new(),
      parent,
    })
  }

  pub fn set_parent(&mut self, parent: Ptr<SymbolTable>) {
    self.parent = Some(parent);
  }

  pub fn lookup(&self, name: &String) -> Option<Ptr<Symbol>> {
    if let Some(symbol) = self.table.get(name) {
      return Some(symbol.clone());
    }
    if let Some(parent) = &self.parent {
      return parent.borrow().lookup(name);
    }
    return None;
  }

  pub fn register_temporary(
    &mut self,
    value: Ptr<Value>,
    mir_codegen_ctx: &mut MirCodegenContext,
  ) -> Ptr<Symbol> {
    let name = mir_codegen_ctx.fetch_temprary_name();
    let symbol = Symbol::mk_temporary(name, value);
    self
      .table
      .insert(symbol.borrow().name.clone(), symbol.clone());
    return symbol;
  }

  pub fn register_module(&mut self, module: Ptr<Module>) -> Ptr<Symbol> {
    let name = module.borrow().name.clone();
    let symbol = Symbol::mk_module(name, module);
    self
      .table
      .insert(symbol.borrow().name.clone(), symbol.clone());
    return symbol;
  }

  pub fn register_param(&mut self, param: &FnParam) -> Ptr<Symbol> {
    let name = param.name.clone();
    if let None = name {
      todo!()
    }
    let symbol = Symbol::mk_param(name.unwrap(), param.ty.clone());
    self
      .table
      .insert(symbol.borrow().name.clone(), symbol.clone());
    return symbol;
  }

  pub fn register_def(
    &mut self,
    name: String,
    builtin: bool,
    func: Fn,
  ) -> Ptr<Symbol> {
    let symbol = Symbol::mk_def(name, builtin, func);
    self
      .table
      .insert(symbol.borrow().name.clone(), symbol.clone());
    return symbol;
  }
}

#[derive(Debug, Clone)]
pub enum StmtKind {
  /// Return
  Return(Option<Ptr<Symbol>>),
  /// Break
  Break(Label, Option<Ptr<Symbol>>),
  /// Continue
  Continue(Option<Label>),
  /// Block
  Block(Ptr<Block>),
  /// If
  If(Ptr<Symbol>, Block, Option<Block>),
  /// Loop
  Loop(Ptr<Block>),
}

#[derive(Debug, Clone)]
pub struct Stmt {
  pub kind: StmtKind,
  pub span: Span,
}

impl Stmt {
  pub fn new(kind: StmtKind, span: Span) -> Ptr<Stmt> {
    ptr(Stmt { kind, span })
  }

  pub fn mk_return(value: Option<Ptr<Symbol>>) -> Ptr<Stmt> {
    Stmt::new(StmtKind::Return(value), Span::default())
  }

  pub fn mk_break(label: Label, value: Option<Ptr<Symbol>>) -> Ptr<Stmt> {
    Stmt::new(StmtKind::Break(label, value), Span::default())
  }

  pub fn mk_if(
    cond: Ptr<Symbol>,
    then_block: Block,
    else_block: Option<Block>,
  ) -> Ptr<Stmt> {
    Stmt::new(StmtKind::If(cond, then_block, else_block), Span::default())
  }

  pub fn mk_block(block: Block) -> Ptr<Stmt> {
    Stmt::new(StmtKind::Block(ptr(block)), Span::default())
  }
}

#[derive(Debug, Clone)]
pub struct Block {
  pub label: Label,
  pub stmts: Vec<Ptr<Stmt>>,
  pub symbol_table: Ptr<SymbolTable>,
}

impl Block {
  pub fn new(label: String, parent_symbol_table: Ptr<SymbolTable>) -> Block {
    Block {
      label,
      stmts: Vec::new(),
      symbol_table: SymbolTable::new(Some(parent_symbol_table)),
    }
  }

  pub fn push(&mut self, stmt: Ptr<Stmt>) {
    self.stmts.push(stmt);
  }
}

#[derive(Debug, Clone)]
pub struct Module {
  pub name: String,
  pub symbol_table: Ptr<SymbolTable>,
}

impl Module {
  pub fn new(name: String) -> Ptr<Module> {
    ptr(Module {
      name,
      symbol_table: SymbolTable::new(None),
    })
  }
}
