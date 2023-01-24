use clap::{Arg, ArgAction, Command};
use inkwell::context::Context;
use inkwell::targets::TargetTriple;
use oria::backend::LlvmIrCodegenContext;
use oria::front::parser::handle_compunit;
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
  let matches = Command::new("oriac")
    .arg(
      Arg::new("output")
        .short('o')
        .long("output")
        .help("Output filename")
        .action(ArgAction::Set),
    )
    .arg(
      Arg::new("emit")
        .short('e')
        .long("emit")
        .default_value("ir")
        .help("Emit ast | mir | ir")
        .action(ArgAction::Set),
    )
    .arg(
      Arg::new("input")
        .help("Input filename")
        .action(ArgAction::Set),
    )
    .arg(
      Arg::new("mtriple")
        .long("mtriple")
        .default_value("unknown-unknown-unknown")
        .help("Triple of llvm ir.")
        .action(ArgAction::Set),
    )
    .get_matches();

  let input = matches
    .get_one::<String>("input")
    .expect("No input file specified.");
  let output = matches
    .get_one::<String>("output")
    .expect("No output file specified.");
  let emit = matches
    .get_one::<String>("emit")
    .expect("No emit type specified.");
  let triple = matches
    .get_one::<String>("mtriple")
    .expect("No triple specified.");

  let module_name = match std::path::Path::new(input).file_stem() {
    Some(name) => name.to_str().unwrap().to_string(),
    None => {
      return Err(std::io::Error::new(
        std::io::ErrorKind::InvalidInput,
        "Invalid input file name.",
      ));
    }
  };

  let mut source_file = File::open(input)?;
  let mut output_file = File::create(output)?;
  let mut source = String::new();

  source_file.read_to_string(&mut source)?;

  let parse_result = handle_compunit(&source);
  let compunit = match parse_result {
    Ok(compunit) => compunit,
    Err(e) => {
      return Err(std::io::Error::new(
        std::io::ErrorKind::InvalidInput,
        format!("Parse error: {}", e),
      ));
    }
  };

  match emit.as_str() {
    "ast" => {
      write!(output_file, "{:#?}", compunit)?;
    }
    "mir" => {
      let mir_module = compunit.to_mir(module_name);
      write!(output_file, "{:#?}", mir_module)?;
    }
    "ir" => {
      let mir_module = compunit.to_mir(module_name);
      let llvm_module = Context::create();
      let mut codegen_ctx = LlvmIrCodegenContext::new(&llvm_module);
      codegen_ctx
        .module
        .set_triple(&TargetTriple::create(triple.as_str()));
      let codegen_result = codegen_ctx.codegen(mir_module);
      if let Ok(..) = codegen_result {
        let ir_str = codegen_ctx.print();
        write!(output_file, "{}", ir_str)?;
      } else {
        return Err(std::io::Error::new(
          std::io::ErrorKind::InvalidInput,
          format!("Codegen error: {:?}", codegen_result),
        ));
      }
    }
    _ => {
      unimplemented!()
    }
  }

  Ok(())
}
