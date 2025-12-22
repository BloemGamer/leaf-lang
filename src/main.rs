#![allow(clippy::needless_return)]

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
	let mut parsed: Parser = lexed.into(); // for now this has to be mute because of the parse program, this function will be replaced in the future
	let program = parsed.parse_program();
	println!("{:#?}", program);
}
