pub mod check;
pub mod codegen;
pub mod mangling;
pub mod pretty;

use self::check::TypeCheck;
use self::codegen::MirCodegenContext;
use super::{ast, Radix, Span};
use crate::{ptr, Counter, Ptr};
use std::{collections::HashMap, fmt};

pub type Level = u64;
pub type Label = String;
pub type Namespaces = Vec<String>;

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

/// A function.
#[derive(Debug, Clone)]
pub struct Fn {
  /// Function name.
  pub name: String,
  /// Parameters
  pub params: Vec<FnParam>,
  /// Return type.
  pub ret_ty: Ptr<Value>,
  /// Function body.
  ///
  /// The expressions in AST will be converted into blocks in MIR.
  pub body: Option<Block>,
  /// Symbol table of the function scope.
  pub symbol_table: Ptr<SymbolTable>,
  /// Namespaces which the function belongs to and can be indexed from.
  pub namespaces: Namespaces,
}

impl Fn {
  pub fn new(
    name: String,
    params: Vec<FnParam>,
    ret_ty: Ptr<Value>,
    body: Option<Block>,
    symbol_table: Ptr<SymbolTable>,
    namespaces: Namespaces,
  ) -> Fn {
    Fn {
      name,
      params,
      ret_ty,
      body,
      symbol_table,
      namespaces,
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
    namespaces: Namespaces,
  ) -> Ptr<Value> {
    Value::new(
      ValueKind::Fn(Fn::new(name, params, ty, body, symbol_table, namespaces)),
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

/// Symbol kinds.
#[derive(Debug, Clone)]
pub enum SymbolKind {
  /// Function parameter with the type.
  Param(Ptr<Value>),
  /// Variable with specification and the type.
  ///
  /// The variable can be declared through `let`.
  /// The symbol can be used in when type-checking the assign statement.
  Var(VarSpec, Option<Ptr<Value>>),
  /// Constant with the value.
  Const(Ptr<Value>),
  /// Definition of a function.
  ///
  /// The bool indicates whether the function is builtin,
  Def(bool, Fn),
  /// Temporary symbol with the value.
  Temporary(Ptr<Value>),
  /// A module.
  Module(Ptr<Module>),
}

/// A symbol.
#[derive(Debug, Clone)]
pub struct Symbol {
  /// The name of the symbol.
  ///
  /// The name is also used as the key in the symbol table.
  pub name: String,
  /// Symbol kind.
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

/// A symbol table.
#[derive(Clone)]
pub struct SymbolTable {
  /// The symbol table.
  ///
  /// The key is the name of the symbol.
  pub table: HashMap<String, Ptr<Symbol>>,
  /// Symbol table of the parent scope.
  pub parent: Option<Ptr<SymbolTable>>,
}

impl fmt::Debug for SymbolTable {
  fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
    // The debug implementation of `SymbolTable` does not print the parent
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

  /// Set the parent of the symbol table.
  pub fn set_parent(&mut self, parent: Ptr<SymbolTable>) {
    self.parent = Some(parent);
  }

  /// Look up a symbol by its name.
  ///
  /// If the symbol is not found in the current scope, the parent scope will be
  /// searched recursively.
  pub fn lookup(&self, name: &String) -> Option<Ptr<Symbol>> {
    if let Some(symbol) = self.table.get(name) {
      return Some(symbol.clone());
    }
    if let Some(parent) = &self.parent {
      return parent.borrow().lookup(name);
    }
    return None;
  }

  /// Register a temporary symbol with the value.
  ///
  /// The name of the temporary symbol will be derived from `mir_codegen_ctx`.
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

  /// Register a module.
  pub fn register_module(&mut self, module: Ptr<Module>) -> Ptr<Symbol> {
    let name = module.borrow().name.clone();
    let symbol = Symbol::mk_module(name, module);
    self
      .table
      .insert(symbol.borrow().name.clone(), symbol.clone());
    return symbol;
  }

  /// Register a parameter.
  ///
  /// This is used when handling functions.
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

  /// Register a function definition.
  pub fn register_def(&mut self, builtin: bool, func: Fn) -> Ptr<Symbol> {
    let symbol = Symbol::mk_def(func.name.clone(), builtin, func);
    self
      .table
      .insert(symbol.borrow().name.clone(), symbol.clone());
    return symbol;
  }
}

/// Statement kinds.
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

/// A statement.
#[derive(Debug, Clone)]
pub struct Stmt {
  /// The kind of the statement.
  pub kind: StmtKind,
  /// The span of the corresponding part in the source code.
  pub span: Span,
}

impl Stmt {
  pub fn new(kind: StmtKind, span: Span) -> Ptr<Stmt> {
    ptr(Stmt { kind, span })
  }

  /// Make a return statement with optional return symbol.
  pub fn mk_return(maybe_symbol: Option<Ptr<Symbol>>) -> Ptr<Stmt> {
    Stmt::new(StmtKind::Return(maybe_symbol), Span::default())
  }

  /// Make a break statement with label and optional symbol.
  pub fn mk_break(
    label: Label,
    maybe_symbol: Option<Ptr<Symbol>>,
  ) -> Ptr<Stmt> {
    Stmt::new(StmtKind::Break(label, maybe_symbol), Span::default())
  }

  /// Make an if statement.
  pub fn mk_if(
    cond: Ptr<Symbol>,
    then_block: Block,
    maybe_else_block: Option<Block>,
  ) -> Ptr<Stmt> {
    Stmt::new(
      StmtKind::If(cond, then_block, maybe_else_block),
      Span::default(),
    )
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
  pub namespaces: Namespaces,
}

impl Module {
  pub fn new(name: String, namespaces: Namespaces) -> Ptr<Module> {
    ptr(Module {
      name,
      symbol_table: SymbolTable::new(None),
      namespaces,
    })
  }
}
