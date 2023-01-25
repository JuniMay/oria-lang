use super::*;

/// Global context.
#[derive(Debug, Clone)]
pub struct MirCodegenContext {
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
  /// The namespaces.
  pub namespaces: Vec<String>,
}

impl MirCodegenContext {
  pub fn new() -> MirCodegenContext {
    MirCodegenContext {
      constraints: Vec::new(),
      symbol_table: SymbolTable::new(None),
      value_var_cnt: Counter::new(),
      block_lbl_cnt: Counter::new(),
      temporary_cnt: Counter::new(),
      lambda_fn_cnt: Counter::new(),
      namespaces: Vec::new(),
    }
  }

  pub(super) fn add_constraint(
    &mut self,
    target: Ptr<Value>,
    source: Ptr<Value>,
  ) {
    self.constraints.push((target, source));
  }

  /// Fetch a block label.
  /// This will increase the counter of block labels.
  pub(super) fn fetch_block_label(&mut self) -> String {
    // The leading double underscore will be added when generating target code.
    let label = format!("BLOCK_{}", self.block_lbl_cnt.next());
    return label;
  }

  /// Fetch a temporary symbol name.
  /// This will increase the counter of temporary symbols.
  pub(super) fn fetch_temprary_name(&mut self) -> String {
    let name = format!("__TEMP_{}", self.temporary_cnt.next());
    return name;
  }

  /// Fetch a lambda function name.
  /// This will increase the counter of lambda functions.
  pub(super) fn fetach_lambda_fn_name(&mut self) -> String {
    let name = format!("__LAMBDA_{}", self.lambda_fn_cnt.next());
    return name;
  }

  fn push_namespace(&mut self, namespace: String) {
    self.namespaces.push(namespace);
  }

  fn pop_namespace(&mut self) -> Option<String> {
    return self.namespaces.pop();
  }

  fn from_ast_expr(
    &mut self,
    symbol_table: Ptr<SymbolTable>,
    ast_expr: &ast::Expr,
  ) -> Ptr<Value> {
    use ast::ExprKind as AstExprKind;
    let value = match &ast_expr.kind {
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
      AstExprKind::FnTy(ast_params, ast_ret_ty) => {
        let mir_params = ast_params
          .into_iter()
          .map(|ast_param| {
            FnParam::new(
              ast_param.name.clone(),
              self.from_ast_expr(
                symbol_table.clone(),
                ast_param.ty.as_ref().unwrap(),
              ),
              ast_param.implicit,
            )
          })
          .collect::<Vec<_>>();

        let mir_ret_ty = self.from_ast_expr(symbol_table.clone(), ast_ret_ty);

        Value::mk_fn_ty(mir_params, mir_ret_ty)
      }
      AstExprKind::Return(..)
      | AstExprKind::Break(..)
      | AstExprKind::Continue(..)
      | AstExprKind::Assign(..) => unreachable!(),
      _ => unimplemented!(),
    };

    value.borrow_mut().span = Some(ast_expr.span.clone());
    return value;
  }

  fn from_ast_block(
    &mut self,
    symbol_table: Ptr<SymbolTable>,
    ast_block: &ast::Block,
  ) -> Block {
    let label = match &ast_block.label {
      // Remove the leading `'`.
      Some(label) => label.as_str()[1..].to_string(),
      None => self.fetch_block_label(),
    };

    let mut block = Block::new(label, symbol_table.clone());

    for stmt in &ast_block.stmts {
      use ast::StmtKind as AstStmtKind;
      match &stmt.kind {
        AstStmtKind::Expr(expr) => {
          use ast::ExprKind::*;
          match &expr.kind {
            Return(expr) => {
              let value = expr
                .as_ref()
                .map(|expr| self.from_ast_expr(symbol_table.clone(), expr));
              let stmt = if value.is_some() {
                Stmt::mk_return(Some(
                  symbol_table
                    .borrow_mut()
                    .register_temporary(value.unwrap(), self),
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
    return block;
  }

  fn from_ast_item(
    &mut self,
    symbol_table: Ptr<SymbolTable>,
    ast_item: &ast::Item,
  ) {
    use ast::ItemKind as AstItemKind;
    match &ast_item.kind {
      AstItemKind::Def(def) => {
        let builtin = def.builtin;
        let ast_func = &def.body;
        let mir_func_name = match &ast_func.name {
          Some(name) => name.clone(),
          // The anonymous function shall not occur in the `def` item.
          None => self.fetach_lambda_fn_name(),
        };
        let mir_func_symbol_table =
          SymbolTable::new(Some(symbol_table.clone()));

        let mir_params = (&ast_func.params)
          .into_iter()
          .map(|ast_param| {
            let mir_param = FnParam::new(
              ast_param.name.clone(),
              self.from_ast_expr(
                mir_func_symbol_table.clone(),
                ast_param.ty.as_ref().unwrap(),
              ),
              ast_param.implicit,
            );
            mir_func_symbol_table
              .borrow_mut()
              .register_param(&mir_param);
            mir_param
          })
          .collect::<Vec<_>>();

        let mut mir_block = if let Some(ast_expr) = &ast_func.expr {
          Some(match &ast_expr.kind {
            ast::ExprKind::Block(block) => {
              let block =
                self.from_ast_block(mir_func_symbol_table.clone(), block);
              block
            }
            _ => {
              let label = self.fetch_block_label();
              let mut block =
                self::Block::new(label, mir_func_symbol_table.clone());
              let value =
                self.from_ast_expr(block.symbol_table.clone(), ast_expr);
              let symbol = block
                .symbol_table
                .borrow_mut()
                .register_temporary(value, self);
              let stmt = Stmt::mk_return(Some(symbol));
              block.push(stmt);

              block
            }
          })
        } else {
          if builtin {
            None
          } else {
            let block = self::Block::new(
              self.fetch_block_label(),
              mir_func_symbol_table.clone(),
            );
            Some(block)
          }
        };

        let mir_ret_ty = if let Some(ref ast_expr) = ast_func.ret_ty {
          self.from_ast_expr(mir_func_symbol_table.clone(), ast_expr)
        } else {
          Value::mk_var(self)
        };

        if !builtin {
          let constraint = (
            mir_ret_ty.clone(),
            mir_block
              .as_mut()
              .unwrap()
              .fetch_type(mir_func_symbol_table.clone(), self),
          );

          self.add_constraint(constraint.0, constraint.1)
        }

        let mir_func = Fn::new(
          mir_func_name.clone(),
          mir_params,
          mir_ret_ty,
          mir_block,
          mir_func_symbol_table.clone(),
        );
        let _symbol = symbol_table.borrow_mut().register_def(
          mir_func_name.clone(),
          builtin,
          mir_func,
        );
      }
      _ => unimplemented!(),
    }
  }

  pub fn from_ast_compunit(
    &mut self,
    name: String,
    ast_compunit: &ast::CompUnit,
  ) -> Ptr<Module> {
    let mir_module = Module::new(name);

    mir_module
      .borrow_mut()
      .symbol_table
      .borrow_mut()
      .set_parent(self.symbol_table.clone());

    let _symbol = self
      .symbol_table
      .borrow_mut()
      .register_module(mir_module.clone());

    self.push_namespace(mir_module.borrow().name.clone());
    for ast_item in &ast_compunit.items {
      self.from_ast_item(mir_module.borrow().symbol_table.clone(), ast_item);
    }
    self.pop_namespace();

    return mir_module;
  }

  pub fn solve_constraints(self) {
    todo!()
  }
}
