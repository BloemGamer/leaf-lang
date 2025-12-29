#![allow(clippy::needless_return)]
#![allow(dead_code)]

use std::{fs, process::exit};

use self::lexer::Lexer;
use self::parser::Parser;

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
		Ok(program) => {
			println!("{:#?}", program);
		}
		Err(e) => {
			println!("{}", e);
		}
	}
}
