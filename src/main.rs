#![allow(clippy::needless_return)]
// #![allow(dead_code)]
// #![warn(clippy::pedantic)]
#![warn(clippy::if_same_then_else)]
#![warn(clippy::needless_pass_by_value)]
#![warn(clippy::inefficient_to_string)]
#![warn(clippy::unnecessary_wraps)]
#![warn(clippy::implicit_return)]
#![warn(clippy::enum_glob_use)]
#![warn(clippy::slow_vector_initialization)]
#![warn(clippy::redundant_pattern_matching)]
#![warn(clippy::branches_sharing_code)]
#![warn(clippy::doc_markdown)]
#![warn(clippy::needless_borrow)]
#![warn(clippy::needless_for_each)]
#![warn(clippy::shadow_reuse)]
// #![warn(clippy::todo)]

use std::{fs, process::exit};

use self::desuger::Desugarer;
use self::lexer::Lexer;
use self::parser::Parser;

mod desuger;
mod lexer;
mod parser;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Config {}

fn main()
{
	let config: Config = Config::default();
	let file: String = if let Ok(f) = fs::read_to_string("slang-test/main.sl") {
		f
	} else {
		println!("could not open file");
		exit(1)
	};
	let lexed: Lexer = Lexer::new(&config, &file);
	// println!("{:#?}", lexed.clone().collect::<Vec<_>>());
	let parsed: Parser = lexed.into();
	let program: Result<parser::Program, parser::ParseError> = parsed.into();
	match &program {
		Ok(ast) => {
			println!("{ast:#?}");
		}
		Err(e) => {
			println!("{e}");
		}
	}

	let mut desugager: Desugarer = Desugarer::new();

	let desugared = desugager.desugar_program(program.unwrap());
	println!("{desugared}");
}
