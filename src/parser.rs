use std::iter::Peekable;

use crate::Config;
use crate::lexer::Lexer;

pub struct Parser<'source, 'config>
{
	config: &'config Config,
	lexer: Peekable<Lexer<'source, 'config>>,
}

impl<'source, 'config> From<Lexer<'source, 'config>> for Parser<'source, 'config>
{
	fn from(lexer: Lexer<'source, 'config>) -> Self
	{
		Self::new(lexer)
	}
}

impl<'source, 'config> Parser<'source, 'config>
{
	pub fn new(lexer: Lexer<'source, 'config>) -> Self
	{
		let (config, lexer) = lexer.into_parts();

		Self {
			config,
			lexer: lexer.peekable(),
		}
	}
}
