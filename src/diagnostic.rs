use std::fmt;

use crate::define::Span;

pub enum DiagLevel {
  Error,
  Warning,
}

pub enum DiagKind {
  AstTooManyUnderscoreInIdent(String),
  AstInvalidDeclForBuiltinIdent(String),
}

impl fmt::Display for DiagKind {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      DiagKind::AstTooManyUnderscoreInIdent(ident) => {
        write!(f, "too many underscores in identifier `{}`", ident)
      }
      DiagKind::AstInvalidDeclForBuiltinIdent(ident) => {
        write!(f, "invalid declaration for builtin identifier `{}`", ident)
      }
    }
  }
}

pub struct Diag(pub DiagLevel, pub DiagKind, pub Span);

pub struct Diagnostics {
  lines: Vec<String>,
  diags: Vec<Diag>,
}

impl Diagnostics {
  pub fn new(src: String) -> Diagnostics {
    Diagnostics {
      lines: src.lines().map(|s| s.to_string()).collect(),
      diags: Vec::new(),
    }
  }

  pub fn add_diag(&mut self, diag: Diag) {
    self.diags.push(diag);
  }
}

impl fmt::Display for Diagnostics {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    use ansi_term::Color::{Cyan, Red, Yellow};
    use ansi_term::Style;
    for diag in &self.diags {
      match diag.0 {
        DiagLevel::Error => write!(f, "{}", Red.bold().paint("error"))?,
        DiagLevel::Warning => write!(f, "{}", Yellow.bold().paint("warning"))?,
      }
      write!(
        f,
        ": {}\n",
        Style::new().bold().paint(format!("{}", diag.1))
      )?;

      let space_cnt = diag.2.lineno.to_string().len();
      let space = " ".repeat(space_cnt);
      let line = &self.lines[diag.2.lineno - 1];
      let line = line.replace("\t", "  ");

      write!(
        f,
        "{}\n",
        Cyan
          .bold()
          .paint(format!("{}--> {}:{}", space, diag.2.lineno, diag.2.start))
      )?;
      write!(f, "{} {}\n", space, Cyan.bold().paint("|"))?;
      write!(
        f,
        "{} {} {}\n",
        Cyan.bold().paint(format!("{}", diag.2.lineno)),
        Cyan.bold().paint("|"),
        line
      )?;
      write!(
        f,
        "{} {} {}{}\n",
        space,
        Cyan.bold().paint("|"),
        " ".repeat(diag.2.start - 1),
        Yellow.paint("~".repeat(diag.2.length))
      )?;
    }
    Ok(())
  }
}
