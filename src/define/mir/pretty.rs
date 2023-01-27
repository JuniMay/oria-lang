use crate::define::mir::mangling::NameMangling;

use super::*;
use std::fmt;
use textwrap::indent;

impl fmt::Display for Module {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "# {}\n", self.mangle())?;
    write!(f, "module {} {{\n", self.name)?;
    for (_, symbol) in &self.symbol_table.borrow().table {
      write!(f, "{}\n", indent(&format!("{}", symbol.borrow()), "  "))?;
    }
    write!(f, "}}")?;
    Ok(())
  }
}

impl fmt::Display for Symbol {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match &self.kind {
      SymbolKind::Def(builtin, func) => {
        write!(f, "# {}\n", func.mangle())?;
        if *builtin {
          write!(f, "def builtin {}", func)?;
        } else {
          write!(f, "def {}", func)?;
        }
      }
      SymbolKind::Temporary(value) => {
        write!(f, "{} /* temporary */", value.borrow())?;
      }
      SymbolKind::Module(module) => write!(f, "{}", module.borrow())?,
      _ => unimplemented!(),
    }
    Ok(())
  }
}

impl fmt::Display for Fn {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}(", self.name)?;
    for (i, param) in self.params.iter().enumerate() {
      if i > 0 {
        write!(f, ", ")?;
      }
      write!(f, "{}", param)?;
    }
    write!(f, ") : {} => ", self.ret_ty.borrow())?;
    match &self.body {
      Some(block) => write!(f, "{}", block)?,
      None => write!(f, "{{}}")?,
    }
    Ok(())
  }
}

impl fmt::Display for FnParam {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let name = if self.name.is_some() {
      self.name.as_ref().unwrap().clone()
    } else {
      "_".to_string()
    };
    if self.implicit {
      write!(f, "implicit {} : {}", name, self.ty.borrow())?;
    } else {
      write!(f, "{}: {}", name, self.ty.borrow())?;
    }
    Ok(())
  }
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match &self.kind {
      ValueKind::Unit => write!(f, "Unit")?,
      ValueKind::Type(level) => write!(f, "Type{}", level)?,
      ValueKind::Lit(radix, text) => {
        write!(
          f,
          "{}{}",
          match radix {
            Radix::Bin => "0b",
            Radix::Oct => "0o",
            Radix::Dec => "",
            Radix::Hex => "0x",
          },
          text
        )?;
      }
      ValueKind::Ident(name) => write!(f, "{}", name)?,
      ValueKind::FnTy(params, ret_ty) => {
        write!(f, "(")?;
        for (i, param) in params.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", param)?;
        }
        write!(f, ") -> {}", ret_ty.borrow())?;
      }
      ValueKind::Fn(func) => write!(f, "{}", func)?,
      ValueKind::Block(block) => write!(f, "{}", block)?,
      ValueKind::Var(count) => write!(f, "Var{}", count)?,
    }

    Ok(())
  }
}

impl fmt::Display for Block {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "'{} {{\n", self.label)?;
    for stmt in &self.stmts {
      write!(f, "{}\n", indent(&format!("{}", stmt.borrow()), "  "))?;
    }
    write!(f, "}}")?;
    Ok(())
  }
}

impl fmt::Display for Stmt {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match &self.kind {
      StmtKind::Return(maybe_symbol) => {
        write!(f, "return")?;
        if maybe_symbol.is_some() {
          write!(f, " {}", maybe_symbol.as_ref().unwrap().borrow())?;
        }
      }
      StmtKind::Break(label, maybe_symbol) => {
        write!(f, "break '{}", label)?;
        if maybe_symbol.is_some() {
          write!(f, " {}", maybe_symbol.as_ref().unwrap().borrow())?;
        }
      }
      StmtKind::Continue(maybe_label) => {
        write!(f, "continue")?;
        if maybe_label.is_some() {
          write!(f, " '{}", maybe_label.as_ref().unwrap())?;
        }
      }
      StmtKind::Block(block) => write!(f, "{}", block.borrow())?,
      StmtKind::If(cond, then_block, else_block) => {
        write!(f, "if {} {}", cond.borrow(), then_block)?;
        if else_block.is_some() {
          write!(f, " else {}", else_block.as_ref().unwrap())?;
        }
      }
      StmtKind::Loop(block) => {
        write!(f, "loop {}", block.borrow())?;
      } // _ => unimplemented!(),
    }
    Ok(())
  }
}
