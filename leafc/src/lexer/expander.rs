use super::{Lexer, LexerTrait, ParseError, Span, Token, TokenKind};
use crate::config::Config;
use crate::lexer::Directive;
use crate::lexer::Spanned;
use crate::parser::{Expr, Parser};
use crate::source_map::SourceIndex;

#[derive(Clone, Debug)]
pub struct ExpandedLexer<'source, 'config>
{
	lexer: Lexer<'source, 'config>,
	stack: Vec<IfContext>,
	/// One-token lookahead buffer. After consuming the closing `}` of an `#if`
	/// block, we peek at the next token to check for `#else`. If it turns out
	/// not to be `#else`, the peeked token is stashed here so it isn't lost.
	pending: Option<Token>,
}

#[derive(Clone, Debug)]
pub struct IfContext
{
	condition_true: bool,
	in_else: bool,
	/// Tracks brace nesting depth inside this if/else block.
	/// Starts at 1 (for the opening `{` that was already consumed when the
	/// block was entered), and is incremented/decremented as we see further
	/// `{`/`}` tokens. When it reaches 0 the block is finished.
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

	/// Returns `true` when the current position is inside an active (enabled)
	/// branch, i.e. every `IfContext` on the stack agrees that its branch
	/// should be emitted.
	pub fn is_allowed(&self) -> bool
	{
		return self.stack.iter().all(|ctx| {
			if ctx.in_else {
				return !ctx.condition_true;
			}
			return ctx.condition_true;
		});
	}

	/// Push a new `#if` block onto the stack.
	/// `braces` is initialised to 1 because the opening `{` was already
	/// consumed by the time we call this.
	fn enter_if(&mut self, condition_true: bool)
	{
		self.stack.push(IfContext {
			condition_true,
			in_else: false,
			braces: 1,
		});
	}

	/// Transition the top-most context from the `#if` branch to the `#else`
	/// branch, and reset the brace counter to 1 (the opening `{` of the else
	/// block has already been consumed by the caller).
	fn enter_else(&mut self)
	{
		if let Some(ctx) = self.stack.last_mut() {
			ctx.in_else = true;
			ctx.braces = 1;
		}
	}

	/// Pop the completed `#if`/`#else` block from the stack.
	fn exit_if(&mut self)
	{
		self.stack.pop();
	}

	/// Consume tokens from `self.lexer` until (and including) the first
	/// `TokenKind::LeftBrace`, which signals the start of a block body.
	/// Returns an error if EOF is reached before any `{`.
	fn consume_until_open_brace(&mut self) -> Result<(), ParseError>
	{
		loop {
			let t: Token = self.lexer.next_token()?;
			match t.kind {
				TokenKind::LeftBrace => return Ok(()),
				TokenKind::Eof => {
					return Err(ParseError::unexpected_eof(t.span()));
				}
				_ => {} // skip whitespace / other tokens between `#else` and `{`
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

	fn next_token(&mut self) -> Result<Token, ParseError>
	{
		loop {
			// Drain the one-token lookahead buffer first, if populated.
			let tok: Token = if let Some(t) = self.pending.take() {
				t
			} else {
				self.lexer.next_token()?
			};

			// We match on a reference so that `tok` is not moved and can be
			// returned unchanged in the common `_ =>` arm below.
			match &tok.kind {
				// ── #if <expr> { ... } ────────────────────────────────────────
				TokenKind::Directive(Directive::Custom(s)) if s == "if" => {
					// Save these before the mutable borrow on `self.lexer`
					// that `by_ref()` introduces.
					let config = self.lexer.config;
					let source_index = self.lexer.source_index;

					// Collect all tokens that form the condition expression,
					// stopping as soon as we see the `{` that opens the body.
					// The `{` itself is consumed from the stream (map_while
					// returns None for it) but we append a synthetic one so
					// that `parse_expr_no_struct` has a terminator to stop at.
					let condition_tokens = self
						.lexer
						.by_ref()
						.map_while(|t| {
							// Always forward errors so they can be collected.
							let Ok(ref to) = t else { return Some(t) };
							// Stop (and consume) the opening brace.
							if matches!(to.kind, TokenKind::LeftBrace) {
								None
							} else {
								Some(t)
							}
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
					let cond_true: bool = cond.comp_time_check(self.lexer.config, self.lexer.source_index)?;

					// Push a new context; `enter_if` sets `braces = 1` because
					// the opening `{` was already consumed above.
					self.enter_if(cond_true);
					// Do NOT return the `#if` directive token; continue the loop.
				}

				// ── #else { ... } ─────────────────────────────────────────────
				TokenKind::Directive(Directive::Custom(s)) if s == "else" => {
					if self.stack.is_empty() {
						return Err(ParseError::generic(tok.span(), "`#else` without a preceding `#if`"));
					}
					// Consume the opening `{` of the else body.
					self.consume_until_open_brace()?;
					// Flip to the else branch and reset the brace counter.
					self.enter_else();
					// Do NOT return the `#else` directive token.
				}

				// ── `{` ───────────────────────────────────────────────────────
				TokenKind::LeftBrace => {
					// Always track depth regardless of whether this branch is
					// active, so that the brace counter stays consistent.
					if let Some(ctx) = self.stack.last_mut() {
						ctx.braces += 1;
					}
					if self.is_allowed() {
						return Ok(tok);
					}
					// Suppressed branch: discard the token, keep looping.
				}

				// ── `}` ───────────────────────────────────────────────────────
				TokenKind::RightBrace => {
					// Determine whether this `}` closes the current if/else block.
					let closes_block = if let Some(ctx) = self.stack.last_mut() {
						ctx.braces -= 1;
						ctx.braces == 0
					} else {
						false
					};

					if closes_block {
						let in_else = self.stack.last().unwrap().in_else;

						if in_else {
							// The else branch has ended; pop the context entirely.
							self.exit_if();
							// The closing `}` of an if/else construct is not
							// part of the user's code — don't return it.
						} else {
							// The if-branch just ended. Peek at the next token
							// to see whether an `#else` follows.
							let next = self.lexer.next_token()?;

							match &next.kind {
								TokenKind::Directive(Directive::Custom(s)) if s == "else" => {
									// Consume the `{` that opens the else body.
									self.consume_until_open_brace()?;
									// Transition to else without popping the stack,
									// so `is_allowed()` naturally inverts the condition.
									self.enter_else();
									// Don't return `#else` or the closing `}`.
								}
								_ => {
									// No else clause; the whole construct is done.
									self.exit_if();
									// The peeked token belongs to the surrounding
									// code — stash it so the next iteration returns it.
									self.pending = Some(next);
									// Don't return the closing `}` of the if block.
								}
							}
						}
						// In all cases the closing `}` is consumed internally.
					} else if self.is_allowed() {
						// An ordinary `}` inside an active branch.
						return Ok(tok);
					}
					// Suppressed branch: discard, keep looping.
				}

				// ── everything else ───────────────────────────────────────────
				_ => {
					if self.is_allowed() {
						return Ok(tok);
					}
					// Suppressed branch: discard, keep looping.
				}
			}
		}
	}
}

impl Iterator for ExpandedLexer<'_, '_>
{
	type Item = Result<Token, ParseError>;

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

// ─────────────────────────────────────────────────────────────────────────────
// IterLexer — a lexer backed by an existing token iterator (used to parse the
// condition expression of an `#if` directive from a pre-collected Vec<Token>).
// ─────────────────────────────────────────────────────────────────────────────

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

	fn next_token(&mut self) -> Result<Token, ParseError>
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

impl<'config, T> Iterator for IterLexer<'config, T>
where
	T: Iterator<Item = Token> + Clone,
{
	type Item = Result<Token, ParseError>;

	fn next(&mut self) -> Option<Self::Item>
	{
		return self.tokens.next().map(|t| return Ok(t));
	}
}
