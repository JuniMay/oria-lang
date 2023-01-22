use clap::{Arg, ArgAction, Command};
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
				.default_value("ast")
				.help("Emit ast")
				.action(ArgAction::Set),
		)
		.arg(
			Arg::new("input")
				.help("Input filename")
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

	let mut source_file = File::open(input)?;
	let mut output_file = File::create(output)?;
	let mut source = String::new();

	source_file.read_to_string(&mut source)?;

	match emit.as_str() {
		"ast" => {
			let parse_result = handle_compunit(&source);
			match parse_result {
				Ok(compunit) => {
					output_file.write(format!("{:#?}", compunit).as_bytes())?;
				}
				Err(e) => {
					println!("Error: {:?}", e);
				}
			}
		}
		_ => {
			unimplemented!()
		}
	}

	Ok(())
}
