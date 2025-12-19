use std::fs;
use std::process::exit;

mod lexer;

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
	let lexed = lexer::Lexer::new(&config, &file);
	println!("{:#?}", lexed.into_iter().collect::<Vec<_>>());
}
