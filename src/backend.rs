use crate::define::mir::{
  check::TypeCheck, codegen::MirCodegenContext, Block as MirBlock,
  Module as MirModule, StmtKind as MirStmtKind, SymbolKind as MirSymbolKind,
  Value as MirValue, ValueKind as MirValueKind,
};
use crate::define::Radix;
use crate::Ptr;
use inkwell::module::Linkage;
use inkwell::types::{AnyType, AnyTypeEnum, StringRadix};
use inkwell::{
  builder::Builder as LlvmBuilder, context::Context as LlvmContext,
  module::Module as LlvmModule, types::BasicTypeEnum as LlvmBasicTypeEnum,
  values::BasicValueEnum as LlvmBasicValueEnum,
};

pub struct LlvmIrCodegenContext<'ctx> {
  pub context: &'ctx LlvmContext,
  pub builder: LlvmBuilder<'ctx>,
  pub module: LlvmModule<'ctx>,
}

impl<'ctx> LlvmIrCodegenContext<'ctx> {
  pub fn new(context: &'ctx LlvmContext) -> Self {
    Self {
      context,
      builder: context.create_builder(),
      module: context.create_module(""),
    }
  }

  pub fn print(&self) -> String {
    self.module.print_to_string().to_string()
  }

  pub fn codegen(
    &mut self,
    mir_module: Ptr<MirModule>,
    mir_codegen_ctx: &mut MirCodegenContext,
  ) -> Result<(), String> {
    let symbol_table = mir_module.borrow().symbol_table.clone();

    for (name, symbol) in &symbol_table.borrow().table {
      match &mut symbol.borrow_mut().kind {
        MirSymbolKind::Def(builtin, ref mut func) => {
          if *builtin {
            let llvm_ty = self.codegen_type(
              func.fetch_type(symbol_table.clone(), mir_codegen_ctx),
            )?;
            let llvm_fn_ty = match llvm_ty {
              AnyTypeEnum::FunctionType(ty) => ty,
              _ => return Err("Invalid function type".to_string()),
            };
            let _llvm_func = self.module.add_function(
              name.as_str(),
              llvm_fn_ty,
              Some(Linkage::External),
            );
          } else {
            let llvm_ty = self.codegen_type(
              func.fetch_type(symbol_table.clone(), mir_codegen_ctx),
            )?;
            let llvm_fn_ty = match llvm_ty {
              AnyTypeEnum::FunctionType(ty) => ty,
              _ => return Err("Invalid function type".to_string()),
            };

            let llvm_func =
              self.module.add_function(name.as_str(), llvm_fn_ty, None);

            let label = func.body.as_ref().unwrap().label.clone();
            let llvm_curr_block = self.context.append_basic_block(
              llvm_func,
              (label.clone() + "__BEGIN").as_str(),
            );

            self.builder.position_at_end(llvm_curr_block);
            self.codegen_block(func.body.as_ref().unwrap())?;
          }
        }
        MirSymbolKind::Module(mir_module) => {
          self.codegen(mir_module.clone(), mir_codegen_ctx)?;
        }
        _ => return Err("Unimplemented".to_string()),
      }
    }

    self.module.set_name(mir_module.borrow().name.as_str());
    Ok(())
  }

  fn codegen_basic_type(
    &mut self,
    mir_value: Ptr<MirValue>,
  ) -> Result<LlvmBasicTypeEnum<'ctx>, String> {
    match &mir_value.borrow().kind {
      MirValueKind::Ident(name) => match name.as_str() {
        "Int8" => Ok(self.context.i8_type().into()),
        "Int16" => Ok(self.context.i16_type().into()),
        "Int32" => Ok(self.context.i32_type().into()),
        "Int64" => Ok(self.context.i64_type().into()),
        "UInt8" => Ok(self.context.i8_type().into()),
        "UInt16" => Ok(self.context.i16_type().into()),
        "UInt32" => Ok(self.context.i32_type().into()),
        "UInt64" => Ok(self.context.i64_type().into()),
        "Float32" => Ok(self.context.f32_type().into()),
        "Float64" => Ok(self.context.f64_type().into()),
        _ => Err("Type is not supported".to_string()),
      },
      _ => Err("Type is not supported".to_string()),
    }
  }

  fn codegen_type(
    &mut self,
    mir_value: Ptr<MirValue>,
  ) -> Result<AnyTypeEnum<'ctx>, String> {
    match &mir_value.borrow().kind {
      MirValueKind::FnTy(params, ret_ty) => {
        let mut param_types = Vec::new();
        for param in params {
          param_types.push(self.codegen_basic_type(param.ty.clone())?.into());
        }

        let ret_ty = self.codegen_basic_type(ret_ty.clone())?.into();

        use LlvmBasicTypeEnum::*;
        match ret_ty {
          IntType(ty) => Ok(ty.fn_type(&param_types, false).into()),
          FloatType(ty) => Ok(ty.fn_type(&param_types, false).into()),
          _ => Err("Type is not supported".to_string()),
        }
      }
      _ => Ok(
        self
          .codegen_basic_type(mir_value.clone())?
          .as_any_type_enum(),
      ),
    }
  }

  fn codegen_basic_value(
    &mut self,
    mir_value: Ptr<MirValue>,
  ) -> Result<LlvmBasicValueEnum<'ctx>, String> {
    match &mir_value.borrow().kind {
      MirValueKind::Lit(radix, text) => {
        let llvm_ty = self
          .codegen_basic_type(mir_value.borrow().ty.clone().unwrap())
          .unwrap();

        let ll_radix = match radix {
          Radix::Bin => StringRadix::Binary,
          Radix::Dec => StringRadix::Decimal,
          Radix::Hex => StringRadix::Hexadecimal,
          Radix::Oct => StringRadix::Octal,
        };

        use LlvmBasicTypeEnum::*;
        match llvm_ty {
          IntType(ty) => Ok(
            ty.const_int_from_string(text.as_str(), ll_radix)
              .unwrap()
              .into(),
          ),
          FloatType(ty) => Ok(ty.const_float_from_string(text.as_str()).into()),
          _ => Err("Invalid Literal type.".to_string()),
        }
      }
      _ => Err("Value is not supported".to_string()),
    }
  }

  fn codegen_block(&mut self, block: &MirBlock) -> Result<(), String> {
    for stmt in &block.stmts {
      match &stmt.borrow().kind {
        MirStmtKind::Return(value) => {
          if let Some(symbol) = value {
            let mir_value = match &symbol.borrow().kind {
              MirSymbolKind::Var(_spec, maybe_mir_value) => {
                if let Some(mir_value) = maybe_mir_value {
                  mir_value.clone()
                } else {
                  return Err("Uninitialized variable".to_string());
                }
              }

              MirSymbolKind::Temporary(mir_value) => mir_value.clone(),
              _ => unimplemented!(),
            };
            let llvm_value = self.codegen_basic_value(mir_value.clone())?;
            self.builder.build_return(match &llvm_value {
              LlvmBasicValueEnum::IntValue(v) => Some(v),
              LlvmBasicValueEnum::FloatValue(v) => Some(v),
              _ => unimplemented!(),
            });
          } else {
            self.builder.build_return(None);
          }
        }
        _ => unimplemented!(),
      }
    }
    Ok(())
  }
}
