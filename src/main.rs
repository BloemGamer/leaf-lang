mod lexer;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Config {}

fn main()
{
	let config: Config = Config::default();
	let lexed = lexer::Lexer::new(&config, "fn main{i64 a = 0; return a}");
	println!("{:#?}", lexed.into_iter().collect::<Vec<_>>());
}
