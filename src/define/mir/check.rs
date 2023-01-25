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
    _symbol_table: Ptr<SymbolTable>,
    _mir_codegen_ctx: &mut MirCodegenContext,
  ) -> Ptr<Value> {
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
          return mir_value
            .borrow_mut()
            .fetch_type(symbol_table, mir_codegen_ctx);
        } else {
          panic!("Variable {} has no type", self.name);
        }
      }
      SymbolKind::Temporary(mir_value) => {
        return mir_value
          .borrow_mut()
          .fetch_type(symbol_table, mir_codegen_ctx);
      }
      _ => todo!(),
    }
  }
}

impl TypeCheck for Block {
  fn fetch_type(
    &mut self,
    symbol_table: Ptr<SymbolTable>,
    mir_codegen_ctx: &mut MirCodegenContext,
  ) -> Ptr<Value> {
    let mut maybe_ty: Option<Ptr<Value>> = None;
    for stmt in &self.stmts {
      match &stmt.borrow_mut().kind {
        StmtKind::Return(Some(mir_symbol)) => {
          let ty = mir_symbol
            .borrow_mut()
            .fetch_type(symbol_table.clone(), mir_codegen_ctx);
          if let Some(ref prev_ty) = maybe_ty {
            mir_codegen_ctx.add_constraint(ty.clone(), prev_ty.clone());
          }
          maybe_ty = Some(ty);
        }
        StmtKind::Return(None) => {
          let ty = Value::mk_unit();
          if let Some(ref prev_ty) = maybe_ty {
            mir_codegen_ctx.add_constraint(ty.clone(), prev_ty.clone());
          }
          maybe_ty = Some(ty);
        }
        _ => todo!(),
      }
    }

    if let None = maybe_ty {
      return Value::mk_unit();
    } else {
      // TODO: Type check the return type.
      return maybe_ty.unwrap();
    }
  }
}
