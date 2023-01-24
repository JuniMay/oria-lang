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
  pub body: Option<Ptr<Value>>,
  pub symbol_table: Ptr<SymbolTable>,
}

impl Fn {
  pub fn new(
    name: String,
    params: Vec<FnParam>,
    ret_ty: Ptr<Value>,
    body: Option<Ptr<Value>>,
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

  pub fn fetch_type(&self) -> Ptr<Value> {
    let mut params = Vec::new();
    for param in &self.params {
      params.push(FnParam::new(
        param.name.clone(),
        param.ty.clone(),
        param.implicit,
      ));
    }
    return Value::mk_fn_ty(params, self.ret_ty.clone());
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
  pub span: Span,
}

impl Value {
  pub fn new(
    kind: ValueKind,
    ty: Option<Ptr<Value>>,
    span: Span,
  ) -> Ptr<Value> {
    ptr(Value { kind, ty, span })
  }

  pub fn mk_unit() -> Ptr<Value> {
    Value::new(ValueKind::Unit, Some(Value::mk_type(0)), Span::default())
  }

  /// Make a type value.
  /// The `ty` of type will be computed lazily.
  pub fn mk_type(level: Level) -> Ptr<Value> {
    Value::new(ValueKind::Type(level), None, Span::default())
  }

  pub fn mk_lit(radix: Radix, text: String, ty: Ptr<Value>) -> Ptr<Value> {
    Value::new(ValueKind::Lit(radix, text), Some(ty), Span::default())
  }

  pub fn mk_ident(name: String) -> Ptr<Value> {
    Value::new(ValueKind::Ident(name), None, Span::default())
  }

  pub fn mk_fn_ty(params: Vec<FnParam>, ret: Ptr<Value>) -> Ptr<Value> {
    Value::new(
      ValueKind::FnTy(params, ret),
      Some(Value::mk_type(0)),
      Span::default(),
    )
  }

  pub fn mk_block(block: Block) -> Ptr<Value> {
    Value::new(ValueKind::Block(block), None, Span::default())
  }

  pub fn mk_fn(
    name: String,
    params: Vec<FnParam>,
    ty: Ptr<Value>,
    body: Option<Ptr<Value>>,
    symbol_table: Ptr<SymbolTable>,
  ) -> Ptr<Value> {
    Value::new(
      ValueKind::Fn(Fn::new(name, params, ty, body, symbol_table)),
      None,
      Span::default(),
    )
  }

  pub fn mk_var(global_ctx: &mut GlobalContext) -> Ptr<Value> {
    Value::new(
      ValueKind::Var(global_ctx.value_var_cnt.next()),
      None,
      Span::default(),
    )
  }

  /// Upgrades the type of the value to the next level.
  /// If the value is not a type, no change will be made.
  pub fn level_up(&mut self) {
    if let ValueKind::Type(level) = self.kind {
      self.kind = ValueKind::Type(level + 1);
    }
  }

  /// Get the type of the value.
  pub fn fetch_type(&mut self, symbol_table: Ptr<SymbolTable>) -> Ptr<Value> {
    if let Some(ref value) = self.ty {
      return value.clone();
    } else {
      match &self.kind {
        ValueKind::Type(level) => {
          self.ty = Some(Value::mk_type(level + 1));
          return self.ty.clone().unwrap();
        }
        ValueKind::FnTy(..) => {
          self.ty = Some(Value::mk_type(0));
          return self.ty.clone().unwrap();
        }
        ValueKind::Fn(func) => {
          self.ty = Some(func.fetch_type());
          return self.ty.clone().unwrap();
        }
        _ => todo!(),
      }
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

  pub fn register_temporary(
    &mut self,
    value: Ptr<Value>,
    global_ctx: &mut GlobalContext,
  ) -> Ptr<Symbol> {
    let name = global_ctx.fetch_temprary_name();
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
  If(Ptr<Symbol>, Ptr<Block>, Option<Ptr<Block>>),
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

/// Global context.
#[derive(Debug, Clone)]
pub struct GlobalContext {
  /// Constraints of types.
  pub constraints: Vec<(Ptr<Value>, Ptr<Value>)>,
  /// Global symbol table.
  pub symbol_table: Ptr<SymbolTable>,
  /// Counter for type variables.
  pub value_var_cnt: Counter,
  /// Counter for block labels.
  pub block_lbl_cnt: Counter,
  /// Counter for temporary symbols.
  pub temporary_cnt: Counter,
  /// Counter for anonymous functions.
  pub lambda_fn_cnt: Counter,
}

impl GlobalContext {
  pub fn new() -> GlobalContext {
    GlobalContext {
      constraints: Vec::new(),
      symbol_table: SymbolTable::new(None),
      value_var_cnt: Counter::new(),
      block_lbl_cnt: Counter::new(),
      temporary_cnt: Counter::new(),
      lambda_fn_cnt: Counter::new(),
    }
  }
  /// Fetch a block label.
  /// This will increase the counter of block labels.
  pub fn fetch_block_label(&mut self) -> String {
    let label = format!("__BLOCK_{}", self.block_lbl_cnt.next());
    return label;
  }

  /// Fetch a temporary symbol name.
  /// This will increase the counter of temporary symbols.
  pub fn fetch_temprary_name(&mut self) -> String {
    let name = format!("__TEMP_{}", self.temporary_cnt.next());
    return name;
  }

  pub fn fetach_lambda_fn_name(&mut self) -> String {
    let name = format!("__LAMBDA_{}", self.lambda_fn_cnt.next());
    return name;
  }
}

impl ast::Expr {
  pub fn to_mir(
    &self,
    symbol_table: Ptr<SymbolTable>,
    global_ctx: &mut GlobalContext,
  ) -> Ptr<Value> {
    use ast::ExprKind as AstExprKind;
    let value = match &self.kind {
      AstExprKind::Unit => Value::mk_unit(),
      AstExprKind::Universe => Value::mk_type(0),
      AstExprKind::Lit(lit) => {
        use ast::LitKind as AstLitKind;
        use ast::LitSuffix as AstLitSuffix;
        let ty = match &lit.suffix {
          Some(suffix) => Value::mk_ident(
            match suffix {
              AstLitSuffix::I8 => "Int8",
              AstLitSuffix::I16 => "Int16",
              AstLitSuffix::I32 => "Int32",
              AstLitSuffix::I64 => "Int64",
              AstLitSuffix::U8 => "UInt8",
              AstLitSuffix::U16 => "UInt16",
              AstLitSuffix::U32 => "UInt32",
              AstLitSuffix::U64 => "UInt64",
              AstLitSuffix::F32 => "Float32",
              AstLitSuffix::F64 => "Float64",
              AstLitSuffix::ISize => "ISize",
              AstLitSuffix::USize => "USize",
            }
            .to_string(),
          ),
          None => match lit.kind {
            // TODO: More precise types for literal without suffix.
            AstLitKind::Integer => Value::mk_ident("Int32".to_string()),
            AstLitKind::Floating => Value::mk_ident("Float32".to_string()),
          },
        };
        let text = lit.text.clone();
        Value::mk_lit(lit.radix.clone(), text, ty)
      }
      AstExprKind::Ident(name) => Value::mk_ident(name.clone()),
      AstExprKind::FnTy(params, ret_ty) => {
        let params = params
          .into_iter()
          .map(|param| {
            FnParam::new(
              param.name.clone(),
              param
                .ty
                .as_ref()
                .unwrap()
                .to_mir(symbol_table.clone(), global_ctx),
              param.implicit,
            )
          })
          .collect::<Vec<_>>();

        let ret_ty = ret_ty.to_mir(symbol_table.clone(), global_ctx);

        Value::mk_fn_ty(params, ret_ty)
      }
      AstExprKind::Return(..)
      | AstExprKind::Break(..)
      | AstExprKind::Continue(..)
      | AstExprKind::Assign(..) => unreachable!(),
      _ => unimplemented!(),
    };

    value.borrow_mut().span = self.span.clone();
    return value;
  }
}

impl ast::Block {
  pub fn to_mir(
    &self,
    symbol_table: Ptr<SymbolTable>,
    global_ctx: &mut GlobalContext,
  ) -> Block {
    let label = match &self.label {
      // Remove the leading `'`.
      Some(label) => label.as_str()[1..].to_string(),
      None => global_ctx.fetch_block_label(),
    };

    let mut block = Block::new(label, symbol_table.clone());

    for stmt in &self.stmts {
      use ast::StmtKind as AstStmtKind;
      match &stmt.kind {
        AstStmtKind::Expr(expr) => {
          use ast::ExprKind::*;
          match &expr.kind {
            Return(expr) => {
              let value = expr
                .as_ref()
                .map(|expr| expr.to_mir(symbol_table.clone(), global_ctx));
              let stmt = if value.is_some() {
                Stmt::mk_return(Some(
                  symbol_table
                    .borrow_mut()
                    .register_temporary(value.unwrap(), global_ctx),
                ))
              } else {
                Stmt::mk_return(None)
              };
              block.push(stmt);
            }
            Break(_label, _expr) => {
              todo!()
            }
            Continue(_label) => {
              todo!()
            }
            _ => unimplemented!(),
          }
        }
        _ => unimplemented!(),
      }
    }

    block
  }
}

impl ast::Item {
  pub fn to_mir(
    &self,
    symbol_table: Ptr<SymbolTable>,
    global_ctx: &mut GlobalContext,
  ) {
    use ast::ItemKind as AstItemKind;
    match &self.kind {
      AstItemKind::Def(def) => {
        let builtin = def.builtin;

        let func = &def.body;
        let name = match &func.name {
          Some(name) => name.clone(),
          // The anonymous function shall not occur in the `def` item.
          None => global_ctx.fetach_lambda_fn_name(),
        };
        let func_symbol_table = SymbolTable::new(Some(symbol_table.clone()));

        let params = (&func.params)
          .into_iter()
          .map(|param| {
            let param = FnParam::new(
              param.name.clone(),
              param
                .ty
                .as_ref()
                .unwrap()
                .to_mir(func_symbol_table.clone(), global_ctx),
              param.implicit,
            );
            func_symbol_table.borrow_mut().register_param(&param);
            param
          })
          .collect::<Vec<_>>();

        let block = if let Some(expr) = &func.expr {
          Some(match &expr.kind {
            ast::ExprKind::Block(block) => {
              let block = block.to_mir(func_symbol_table.clone(), global_ctx);
              Value::mk_block(block)
            }
            _ => {
              let label = global_ctx.fetch_block_label();
              let mut block =
                self::Block::new(label, func_symbol_table.clone());
              let value = expr.to_mir(block.symbol_table.clone(), global_ctx);
              let symbol = block
                .symbol_table
                .borrow_mut()
                .register_temporary(value, global_ctx);
              let stmt = Stmt::mk_return(Some(symbol));
              block.push(stmt);

              Value::mk_block(block)
            }
          })
        } else {
          if builtin {
            None
          } else {
            let block = self::Block::new(
              global_ctx.fetch_block_label(),
              func_symbol_table.clone(),
            );
            Some(Value::mk_block(block))
          }
        };

        let ret_ty = if let Some(ref expr) = func.ret_ty {
          expr.to_mir(func_symbol_table.clone(), global_ctx)
        } else {
          Value::mk_var(global_ctx)
        };

        let func = Fn::new(
          name.clone(),
          params,
          ret_ty,
          block,
          func_symbol_table.clone(),
        );
        let _symbol =
          symbol_table
            .borrow_mut()
            .register_def(name.clone(), builtin, func);
      }
      _ => unimplemented!(),
    }
  }
}

impl ast::CompUnit {
  pub fn to_mir(&self, name: String) -> Ptr<Module> {
    let module = Module::new(name);
    let mut global_ctx = GlobalContext::new();

    module
      .borrow_mut()
      .symbol_table
      .borrow_mut()
      .set_parent(global_ctx.symbol_table.clone());

    let _symbol = global_ctx
      .symbol_table
      .borrow_mut()
      .register_module(module.clone());

    for item in &self.items {
      item.to_mir(module.borrow().symbol_table.clone(), &mut global_ctx);
    }

    return module;
  }
}
