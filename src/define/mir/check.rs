use super::*;

pub trait TypeCheck {
  fn fetch_type(
    &mut self,
    symbol_table: Ptr<SymbolTable>,
    mir_codegen_ctx: &mut MirCodegenContext,
  ) -> Ptr<Value>;
}

impl TypeCheck for Fn {
  fn fetch_type(
    &mut self,
    symbol_table: Ptr<SymbolTable>,
    mir_codegen_ctx: &mut MirCodegenContext,
  ) -> Ptr<Value> {
    let mut params = Vec::new();
    for param in &self.params {
      params.push(FnParam::new(
        param.name.clone(),
        param.ty.clone(),
        param.implicit,
      ));
    }
    if let Some(ref mut block) = self.body {
      let maybe_ty =
        block.fetch_type_helper(symbol_table, mir_codegen_ctx, None);
      if let Some(ty) = maybe_ty {
        mir_codegen_ctx.add_constraint(self.ret_ty.clone(), ty.clone());
      } else {
        mir_codegen_ctx.add_constraint(self.ret_ty.clone(), Value::mk_unit());
      }
    }
    return Value::mk_fn_ty(params, self.ret_ty.clone());
  }
}

impl TypeCheck for Value {
  fn fetch_type(
    &mut self,
    symbol_table: Ptr<SymbolTable>,
    mir_codegen_ctx: &mut MirCodegenContext,
  ) -> Ptr<Value> {
    if let Some(ref value) = self.ty {
      return value.clone();
    } else {
      match &mut self.kind {
        ValueKind::Type(level) => {
          self.ty = Some(Value::mk_type(*level + 1));
          return self.ty.clone().unwrap();
        }
        ValueKind::FnTy(..) => {
          self.ty = Some(Value::mk_type(0));
          return self.ty.clone().unwrap();
        }
        ValueKind::Fn(ref mut func) => {
          self.ty =
            Some(func.fetch_type(symbol_table.clone(), mir_codegen_ctx));
          return self.ty.clone().unwrap();
        }
        ValueKind::Ident(name) => {
          let symbol = symbol_table.as_ref().borrow().lookup(name);
          if let Some(symbol) = symbol {
            let ty = symbol
              .borrow_mut()
              .fetch_type(symbol_table, mir_codegen_ctx);
            self.ty = Some(ty);
            return self.ty.clone().unwrap();
          } else {
            panic!("Undefined variable {}", name);
          }
        }
        _ => todo!(),
      }
    }
  }
}

impl TypeCheck for Symbol {
  fn fetch_type(
    &mut self,
    symbol_table: Ptr<SymbolTable>,
    mir_codegen_ctx: &mut MirCodegenContext,
  ) -> Ptr<Value> {
    match &mut self.kind {
      SymbolKind::Var(_spec, maybe_mir_value) => {
        if let Some(ref mut mir_value) = maybe_mir_value {
          return mir_value.clone();
        } else {
          panic!("Variable {} has no type", self.name);
        }
      }
      SymbolKind::Temporary(mir_value) => {
        return mir_value
          .borrow_mut()
          .fetch_type(symbol_table, mir_codegen_ctx);
      }
      SymbolKind::Param(ty) => {
        return ty.clone();
      }
      _ => todo!(),
    }
  }
}

impl Block {
  fn fetch_type_helper(
    &mut self,
    symbol_table: Ptr<SymbolTable>,
    mir_codegen_ctx: &mut MirCodegenContext,
    expected_label: Option<Label>,
  ) -> Option<Ptr<Value>> {
    let mut maybe_ty: Option<Ptr<Value>> = None;
    for stmt in &self.stmts {
      match &mut stmt.borrow_mut().kind {
        StmtKind::Return(Some(mir_symbol)) => {
          if expected_label.is_none() {
            let ty = mir_symbol
              .borrow_mut()
              .fetch_type(symbol_table.clone(), mir_codegen_ctx);
            if let Some(ref prev_ty) = maybe_ty {
              mir_codegen_ctx.add_constraint(ty.clone(), prev_ty.clone());
            }
            maybe_ty = Some(ty);
          }
        }
        StmtKind::Return(None) => {
          if expected_label.is_none() {
            let ty = Value::mk_unit();
            if let Some(ref prev_ty) = maybe_ty {
              mir_codegen_ctx.add_constraint(ty.clone(), prev_ty.clone());
            }
            maybe_ty = Some(ty);
          }
        }
        StmtKind::Break(label, None) => {
          if expected_label.is_some() {
            if *label != *expected_label.as_ref().unwrap() {
              continue;
            }
            let ty = Value::mk_unit();
            if let Some(ref prev_ty) = maybe_ty {
              mir_codegen_ctx.add_constraint(ty.clone(), prev_ty.clone());
            }
            maybe_ty = Some(ty);
          }
        }
        StmtKind::Break(label, Some(mir_symbol)) => {
          if expected_label.is_some() {
            if *label != *expected_label.as_ref().unwrap() {
              continue;
            }
            let ty = mir_symbol
              .borrow_mut()
              .fetch_type(symbol_table.clone(), mir_codegen_ctx);
            if let Some(ref prev_ty) = maybe_ty {
              mir_codegen_ctx.add_constraint(ty.clone(), prev_ty.clone());
            }
            maybe_ty = Some(ty);
          }
        }
        StmtKind::Block(block) => {
          let inner_maybe_ty = block.borrow_mut().fetch_type_helper(
            symbol_table.clone(),
            mir_codegen_ctx,
            expected_label.clone(),
          );
          if let Some(ref inner_ty) = inner_maybe_ty {
            if let Some(ref prev_ty) = maybe_ty {
              mir_codegen_ctx.add_constraint(inner_ty.clone(), prev_ty.clone());
            }
            maybe_ty = Some(inner_ty.clone());
          }
        }
        StmtKind::If(
          _mir_cond_symbol,
          ref mut mir_block,
          maybe_mir_else_block,
        ) => {
          let maybe_block_ty = mir_block.fetch_type_helper(
            symbol_table.clone(),
            mir_codegen_ctx,
            expected_label.clone(),
          );
          if maybe_mir_else_block.is_none() {
            if let Some(ref prev_ty) = maybe_ty {
              mir_codegen_ctx.add_constraint(
                maybe_block_ty.clone().unwrap(),
                prev_ty.clone(),
              );
            }
            maybe_ty = maybe_block_ty;
          } else {
            let maybe_else_block_ty =
              maybe_mir_else_block.as_mut().unwrap().fetch_type_helper(
                symbol_table.clone(),
                mir_codegen_ctx,
                expected_label.clone(),
              );
            if let Some(ref prev_ty) = maybe_ty {
              mir_codegen_ctx.add_constraint(
                maybe_block_ty.clone().unwrap(),
                prev_ty.clone(),
              );
              mir_codegen_ctx.add_constraint(
                maybe_else_block_ty.clone().unwrap(),
                prev_ty.clone(),
              );
            }
            maybe_ty = maybe_else_block_ty
          }
        }
        _ => {}
      }
    }
    return maybe_ty;
  }
}

impl TypeCheck for Block {
  fn fetch_type(
    &mut self,
    symbol_table: Ptr<SymbolTable>,
    mir_codegen_ctx: &mut MirCodegenContext,
  ) -> Ptr<Value> {
    let maybe_ty = self.fetch_type_helper(
      symbol_table,
      mir_codegen_ctx,
      Some(self.label.clone()),
    );
    if let Some(ty) = maybe_ty {
      return ty;
    } else {
      return Value::mk_unit();
    }
  }
}
