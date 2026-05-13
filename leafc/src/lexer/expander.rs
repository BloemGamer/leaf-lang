use super::{Lexer, LexerTrait, ParseError, Span, Token, TokenKind};
use crate::config::Config;
use crate::diagnostics::CompileDiagnostic;
use crate::diagnostics::DiagnosticBuilder;
use crate::lexer::Directive;
use crate::lexer::Spanned;
use crate::parser::{Expr, Parser};
use crate::source_map::SourceIndex;

#[derive(Clone, Debug)]
pub struct ExpandedLexer<'source, 'config>
{
	lexer: Lexer<'source, 'config>,
	stack: Vec<IfContext>,
	pending: Option<Token>,
}

#[derive(Clone, Debug)]
pub struct IfContext
{
	condition_true: bool,
	in_else: bool,
	braces: usize,
}

impl<'s, 'c> ExpandedLexer<'s, 'c>
{
	pub const fn new(lexer: Lexer<'s, 'c>) -> Self
	{
		return Self {
			lexer,
			stack: Vec::new(),
			pending: None,
		};
	}

	pub fn is_allowed(&self) -> bool
	{
		return self.stack.iter().all(|ctx| {
			if ctx.in_else {
				return !ctx.condition_true;
			}
			return ctx.condition_true;
		});
	}

	fn enter_if(&mut self, condition_true: bool)
	{
		self.stack.push(IfContext {
			condition_true,
			in_else: false,
			braces: 1,
		});
	}

	fn enter_else(&mut self)
	{
		if let Some(ctx) = self.stack.last_mut() {
			ctx.in_else = true;
			ctx.braces = 1;
		}
	}

	fn exit_if(&mut self)
	{
		self.stack.pop();
	}

	fn consume_until_open_brace(&mut self) -> Result<(), Box<DiagnosticBuilder>>
	{
		loop {
			let t: Token = self.lexer.next_token()?;
			match t.kind {
				TokenKind::LeftBrace => return Ok(()),
				TokenKind::Eof => {
					return Err(Box::new(ParseError::unexpected_eof(t.span()).build()));
				}
				_ => {}
			}
		}
	}
}

impl<'s, 'c> LexerTrait<'s, 'c> for ExpandedLexer<'s, 'c>
{
	fn into_parts(self) -> (&'c Config, SourceIndex, Self)
	{
		let config: &'c Config = self.lexer.config;
		let source_index: SourceIndex = self.lexer.source_index;
		return (config, source_index, self);
	}

	fn read_eof_returned(&self) -> bool
	{
		return self.lexer.read_eof_returned();
	}

	fn set_eof_returned(&mut self, eof_returned: bool)
	{
		self.lexer.eof_returned = eof_returned;
	}

	fn next_token(&mut self) -> Result<Token, Box<DiagnosticBuilder>>
	{
		loop {
			let tok: Token = if let Some(t) = self.pending.take() {
				t
			} else {
				self.lexer.next_token()?
			};

			match &tok.kind {
				TokenKind::Directive(Directive::Custom(s)) if s == "if" => {
					let config = self.lexer.config;
					let source_index = self.lexer.source_index;

					let condition_tokens = self
						.lexer
						.by_ref()
						.map_while(|t| {
							let Ok(ref to) = t else { return Some(t) };
							return if matches!(to.kind, TokenKind::LeftBrace) {
								None
							} else {
								Some(t)
							};
						})
						.chain(std::iter::once(Ok(Token {
							kind: TokenKind::LeftBrace,
							span: Span::default(),
						})))
						.collect::<Result<Vec<_>, _>>()?;

					let cond_lexer = IterLexer {
						tokens: condition_tokens.into_iter(),
						config,
						source_index,
						eof_returned: false,
					};

					let mut parser: Parser<_> = cond_lexer.into();
					let cond: Expr = parser.parse_expr_no_struct()?;
					let cond_true: bool = cond.comp_time_check(self.lexer.config)?;

					self.enter_if(cond_true);
				}

				TokenKind::Directive(Directive::Custom(s)) if s == "else" => {
					if self.stack.is_empty() {
						return Err(Box::new(
							ParseError::generic(tok.span(), "`@else` without a preceding `@if`").build(),
						));
					}
					self.consume_until_open_brace()?;
					self.enter_else();
				}

				TokenKind::LeftBrace => {
					if let Some(ctx) = self.stack.last_mut() {
						ctx.braces += 1;
					}
					if self.is_allowed() {
						return Ok(tok);
					}
				}

				TokenKind::RightBrace => {
					let closes_block: bool = if let Some(ctx) = self.stack.last_mut() {
						ctx.braces -= 1;
						ctx.braces == 0
					} else {
						false
					};

					if closes_block {
						let in_else: bool = self.stack.last().expect("Bug: there should be something here").in_else;

						if in_else {
							self.exit_if();
						} else {
							let next: Token = self.lexer.next_token()?;

							match &next.kind {
								TokenKind::Directive(Directive::Custom(s)) if s == "else" => {
									self.consume_until_open_brace()?;
									self.enter_else();
								}
								_ => {
									self.exit_if();
									self.pending = Some(next);
								}
							}
						}
					} else if self.is_allowed() {
						return Ok(tok);
					}
				}

				_ => {
					if self.is_allowed() {
						return Ok(tok);
					}
				}
			}
		}
	}
}

impl Iterator for ExpandedLexer<'_, '_>
{
	type Item = Result<Token, Box<DiagnosticBuilder>>;

	fn next(&mut self) -> Option<Self::Item>
	{
		let mut token: Token = match self.next_token() {
			err @ Err(_) => return Some(err),
			Ok(t) => t,
		};
		while matches!(token.kind, TokenKind::LineComment(_) | TokenKind::BlockComment(_)) {
			token = match self.next_token() {
				err @ Err(_) => return Some(err),
				Ok(t) => t,
			};
		}

		return if matches!(token.kind, TokenKind::Eof | TokenKind::Invalid) {
			if self.read_eof_returned() {
				None
			} else {
				self.set_eof_returned(true);
				Some(Ok(token))
			}
		} else {
			Some(Ok(token))
		};
	}
}

#[derive(Clone, Debug)]
pub struct IterLexer<'config, T>
where
	T: Iterator<Item = Token> + Clone,
{
	tokens: T,
	config: &'config Config,
	source_index: SourceIndex,
	eof_returned: bool,
}

impl<'config, T> LexerTrait<'static, 'config> for IterLexer<'config, T>
where
	T: Iterator<Item = Token> + Clone,
{
	fn into_parts(self) -> (&'config Config, SourceIndex, Self)
	{
		let config: &'config Config = self.config;
		let source_index: SourceIndex = self.source_index;
		return (config, source_index, self);
	}

	fn read_eof_returned(&self) -> bool
	{
		return self.eof_returned;
	}

	fn set_eof_returned(&mut self, eof_returned: bool)
	{
		self.eof_returned = eof_returned;
	}

	fn next_token(&mut self) -> Result<Token, Box<DiagnosticBuilder>>
	{
		let Some(tok) = self.tokens.next() else {
			return Ok(Token {
				kind: TokenKind::Eof,
				span: Span::default(),
			});
		};
		return Ok(tok);
	}
}

impl<T> Iterator for IterLexer<'_, T>
where
	T: Iterator<Item = Token> + Clone,
{
	type Item = Result<Token, Box<DiagnosticBuilder>>;

	fn next(&mut self) -> Option<Self::Item>
	{
		return self.tokens.next().map(|t| return Ok(t));
	}
}
