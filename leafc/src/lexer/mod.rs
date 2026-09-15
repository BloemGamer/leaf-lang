use std::collections::VecDeque;

use leaf_proc::generate_lexer;

use crate::{
	diagnostics::Diagnostic,
	source_map::SourceIndex,
	util::{
		backup::Backup,
		span::{Span, Spanned},
	},
};

#[cfg(test)]
#[path = "../../tests/lexer/mod.rs"]
mod lexer_tests;

pub const INTERNAL_CHAR: char = '#';

#[allow(unused)]
pub trait Lexer<'s>: Iterator<Item = Token<'s>> + Backup + From<LexerSlice<'s>> {}

#[derive(Clone)]
pub struct BasicLexer<'s>
{
	source: &'s str,
	file_id: SourceIndex,
	position: usize,
	current_char: Option<char>,
	line: usize,
	column: usize,
	diagnostics: VecDeque<TokenKind<'s>>,
	last_span: Span,
	reached_eof: bool,
}

#[derive(Clone, PartialEq)]
pub struct LexerSlice<'s>
{
	source: &'s str,
	file_id: SourceIndex,
	position: usize,
	line: usize,
	column: usize,
}

crate::bit_enum!(
	pub struct StringFlags: u64 {
		INVALID = 0b01,
		CSTRING = 0b10,
	}
);

impl StringFlags
{
	fn from_string(string: &str) -> Self
	{
		let f = || {
			let mut flags: StringFlags = StringFlags::default();
			for c in string.chars() {
				flags = match c {
					'c' => flags.add_flag(StringFlags::CSTRING)?,
					_ => return Err(()),
				};
			}
			return Ok(flags);
		};
		match f() {
			Ok(flags) => return flags,
			Err(()) => return Self::INVALID,
		}
	}
}

impl<'s> Lexer<'s> for BasicLexer<'s> {}

impl<'s> From<LexerSlice<'s>> for BasicLexer<'s>
{
	fn from(value: LexerSlice<'s>) -> Self
	{
		let mut lexer: Self = Self {
			source: value.source,
			file_id: value.file_id,
			position: value.position,
			current_char: None,
			line: value.line,
			column: value.column,
			diagnostics: VecDeque::new(),
			last_span: Span::DUMMY,
			reached_eof: false,
		};

		lexer.current_char = lexer.source.chars().next();

		return lexer;
	}
}

impl<'s> Iterator for BasicLexer<'s>
{
	type Item = Token<'s>;

	// for now, Eof is filtered, but if needed, Eof is able to be given to the next stages
	fn next(&mut self) -> Option<Self::Item>
	{
		return match self.next_token() {
			t @ Token {
				kind: TokenKind::Eof, ..
			} => {
				if self.reached_eof {
					return None;
				}
				self.reached_eof = true;
				Some(t)
			}
			t => Some(t),
		};
	}
}

#[derive(Debug, Clone)]
pub struct Token<'s>
{
	pub kind: TokenKind<'s>,
	pub span: Span,
}

impl Spanned for Token<'_>
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

#[derive(Debug, Clone, PartialEq)]
#[generate_lexer(BasicLexer<'s>)]
#[automatic_test(
 	mod(generated_tests),
 	function(
 		fn #test_name() {
 			let mut lexer: BasicLexer<'_> = BasicLexer::new(#token_str, SourceIndex::DUMMY);
 			println!("`{}` == `TokenKind::{:?}`",#token_str, TokenKind::#variant);
 			assert_eq!(lexer.next().unwrap().kind, TokenKind::#variant);
 		}
 	)
 )]
pub enum TokenKind<'s>
{
	// ===== Literals =====
	IntLiteral
	{
		value: &'s str,
		base: IntBase,
		ty: Option<IntType>,
	},

	FloatLiteral
	{
		value: &'s str,
		bits: Option<u16>,
	},

	CharLiteral(char),

	StringLiteral
	{
		string: &'s str,
		flags: StringFlags,
	},

	#[keyword("true")]
	True,

	#[keyword("false")]
	False,

	// ===== Identifiers =====
	Identifier(&'s str),

	Attribute(&'s str),

	Label(&'s str),

	#[keyword("_")]
	Underscore,

	#[keyword("self")]
	SelfKw,

	// ===== Keywords - Control Flow =====
	#[keyword("if")]
	If,

	#[keyword("else")]
	Else,

	#[keyword("while")]
	While,

	#[keyword("for")]
	For,

	#[keyword("loop")]
	Loop,

	#[keyword("switch")]
	Switch,

	#[keyword("return")]
	Return,

	#[keyword("break")]
	Break,

	#[keyword("continue")]
	Continue,

	#[keyword("in")]
	In,

	// ===== Keywords - Bindings =====
	#[keyword("let")]
	Let,

	// ===== Keywords - Declarations =====
	#[keyword("fn")]
	Fn,

	#[keyword("const")]
	Const,

	#[keyword("static")]
	Static,

	#[keyword("struct")]
	Struct,

	#[keyword("union")]
	Union,

	#[keyword("variant")]
	Variant,

	#[keyword("enum")]
	Enum,

	#[keyword("impl")]
	Impl,

	#[keyword("interface")]
	Interface,

	#[keyword("macro")]
	MacroDef,

	#[keyword("type")]
	Type,

	// ===== Keywords - Modifiers =====
	#[keyword("pub")]
	Pub,

	#[keyword("export")]
	Export,

	#[keyword("mut")]
	Mut,

	#[keyword("volatile")]
	Volatile,

	#[keyword("inline")]
	Inline,

	#[keyword("extern")]
	Extern,

	#[keyword("unsafe")]
	Unsafe,

	// ===== Keywords - Ownership (function-signature only) =====
	#[keyword("borrow")]
	Borrow,

	#[keyword("move")]
	Move,

	// ===== Keywords - Directive reserved words =====
	#[keyword("@use")]
	Use,

	#[keyword("@import")]
	Import,

	#[keyword("@module")]
	Module,

	// ===== Keywords - Other =====
	#[keyword("where")]
	Where,

	#[keyword("as")]
	As,

	#[keyword("default")]
	Default,

	/// Effect-list marker inside `unsafe(effect [E1, E2]) { .. }`.
	#[keyword("effect")]
	Effect,

	// ===== Arithmetic Operators =====
	#[operator("+")]
	Plus,

	#[operator("-")]
	Minus,

	#[operator("*")]
	Star,

	#[operator("/")]
	Slash,

	#[operator("%")]
	Mod,

	// ===== Bitwise Operators =====
	#[operator("|")]
	Pipe,

	#[operator("&")]
	Ampersand,

	#[operator("^")]
	Caret,

	#[operator("~")]
	Tilde,

	#[operator("<<")]
	LShift,

	#[operator(">>")]
	RShift,

	// ===== Logical Operators =====
	#[operator("!")]
	Bang,

	#[operator("&&")]
	And,

	#[operator("||")]
	Or,

	// ===== Comparison Operators =====
	#[operator("<")]
	LessThan,

	#[operator(">")]
	GreaterThan,

	#[operator("<=")]
	LessEquals,

	#[operator(">=")]
	GreaterEquals,

	#[operator("==")]
	EqualsEquals,

	#[operator("!=")]
	BangEquals,

	// ===== Assignment Operators =====
	#[operator("=")]
	Equals,

	#[operator("+=")]
	PlusEquals,

	#[operator("-=")]
	MinusEquals,

	#[operator("*=")]
	StarEquals,

	#[operator("/=")]
	SlashEquals,

	#[operator("%=")]
	ModEquals,

	#[operator("|=")]
	PipeEquals,

	#[operator("&=")]
	AmpersandEquals,

	#[operator("^=")]
	CaretEquals,

	#[operator("~=")]
	TildeEquals,

	#[operator("<<=")]
	LShiftEquals,

	#[operator(">>=")]
	RShiftEquals,

	// ===== Delimiters =====
	#[simple_token("(")]
	LeftParen,

	#[simple_token(")")]
	RightParen,

	#[simple_token("{")]
	LeftBrace,

	#[simple_token("}")]
	RightBrace,

	#[simple_token("[")]
	LeftBracket,

	#[simple_token("]")]
	RightBracket,

	// ===== Punctuation =====
	#[simple_token(";")]
	Semicolon,

	#[operator(":")]
	Colon,

	#[operator("::")]
	DoubleColon,

	#[simple_token(",")]
	Comma,

	#[operator(".")]
	Dot,

	#[operator("..")]
	DotDot,

	#[operator("...")]
	DotDotDot,

	#[operator("..=")]
	DotDotEquals,

	#[operator("->")]
	Arrow,

	#[operator("|>")]
	PipeGreater,

	#[operator(":>")]
	ColonGreater,

	#[operator(".>")]
	DotGreater,

	#[operator("=>")]
	FatArrow,

	#[simple_token("?")]
	QuestionMark,

	#[reserved]
	#[simple_token("\\")]
	Backslash,

	// ===== Special Tokens =====

	// ===== Comments =====
	LineComment(&'s str),
	BlockComment(&'s str),
	DocsComment(&'s str),
	EnclosedDocsComment(&'s str),

	// ===== End/Error =====
	Eof,
	Invalid,
	/// Used for warnings and errors, should be catched by the parser, and ignored
	Diag(Diagnostic),
	// ===== Reserved =====
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntBase
{
	Binary,
	Octal,
	Decimal,
	Hexadecimal,
}

impl std::fmt::Display for IntBase
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			IntBase::Binary => write!(f, "0b"),
			IntBase::Octal => write!(f, "0o"),
			IntBase::Decimal => Ok(()),
			IntBase::Hexadecimal => write!(f, "0x"),
		};
	}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum IntSign
{
	Signed,
	Unsigned,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntType
{
	pub bits: IntSize,
	pub sign: IntSign,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntSize
{
	Size,
	Fixed(u16),
}

impl std::fmt::Display for IntType
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match self.sign {
			IntSign::Signed => write!(f, "i{}", self.bits)?,
			IntSign::Unsigned => write!(f, "u{}", self.bits)?,
		}
		return Ok(());
	}
}

impl std::fmt::Display for IntSize
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			IntSize::Size => write!(f, "size"),
			IntSize::Fixed(bits) => write!(f, "{bits}"),
		};
	}
}

pub struct BasicLexerBackup
{
	position: usize,
	line: usize,
	current_char: Option<char>,
}

impl crate::util::backup::Backup for BasicLexer<'_>
{
	type Backup = BasicLexerBackup;

	fn make_backup(&self) -> Self::Backup
	{
		return BasicLexerBackup {
			position: self.position,
			line: self.line,
			current_char: self.current_char,
		};
	}

	fn load_backup(&mut self, backup: Self::Backup)
	{
		self.position = backup.position;
		self.line = backup.line;
		self.current_char = backup.current_char;
	}
}

impl<'s> BasicLexer<'s>
{
	fn next_token(&mut self) -> Token<'s>
	{
		if let Some(kind) = self.diagnostics.pop_front() {
			return Token {
				kind,
				span: self.last_span,
			};
		}

		self.skip_whitespace();

		let start: usize = self.position;
		let start_line: usize = self.line;

		let Some(ch) = self.current_char else {
			return Token {
				kind: TokenKind::Eof,
				span: Span {
					file: self.file_id,
					line: start_line,
					start,
					end: start,
				},
			};
		};

		let kind: TokenKind<'s> = match ch {
			'a'..='z' | 'A'..='Z' | '_' | INTERNAL_CHAR | '@' => self.lex_identifier_or_keyword(),
			'0'..='9' => self.lex_number(),
			'"' => self.lex_string_literal(None),
			'\'' => self.lex_char_or_label(),
			'/' => self.lex_slash_or_comment(),
			c => self.lex_char(c),
		};

		let end: usize = self.position;
		let span: Span = Span {
			file: self.file_id,
			line: start_line,
			start,
			end,
		};

		self.last_span = span;

		if let Some(qkind) = self.diagnostics.pop_front() {
			self.diagnostics.push_back(kind);
			return Token {
				kind: qkind,
				span: self.last_span,
			};
		}

		return Token { kind, span };
	}

	pub fn new(source: &'s str, file_id: SourceIndex) -> Self
	{
		let mut lexer: Self = Self {
			source,
			file_id,
			position: 0,
			current_char: None,
			line: 1,
			column: 1,
			diagnostics: VecDeque::new(),
			last_span: Span::DUMMY,
			reached_eof: false,
		};
		lexer.current_char = lexer.source.chars().next();
		return lexer;
	}

	fn skip_whitespace(&mut self)
	{
		while let Some(ch) = self.current_char {
			if ch.is_whitespace() {
				self.advance();
			} else {
				break;
			}
		}
	}

	fn advance(&mut self)
	{
		if let Some(ch) = self.current_char {
			if ch == '\n' {
				self.line += 1;
				self.column = 1;
			} else {
				self.column += 1;
			}

			self.position += ch.len_utf8();

			self.current_char = self.source[self.position..].chars().next();
		}
	}

	#[allow(unused)]
	fn peek(&self) -> Option<char>
	{
		if let Some(ch) = self.current_char {
			return self.source[self.position + ch.len_utf8()..].chars().next();
		}
		return None;
	}

	fn lex_identifier_or_keyword(&mut self) -> TokenKind<'s>
	{
		let start: usize = self.position;

		if let Some(ch) = self.current_char
			&& (ch.is_alphanumeric() || ch == '_' || ch == INTERNAL_CHAR || ch == '@')
		{
			self.advance();
		}
		while let Some(ch) = self.current_char {
			if ch.is_alphanumeric() || ch == '_' {
				self.advance();
			} else {
				break;
			}
		}

		let ident: &str = &self.source[start..self.position];
		debug_assert!(!ident.is_empty());

		if let Some(keyword) = Self::match_keyword(ident) {
			return keyword;
		}
		if ident.as_bytes().first().copied() == Some(b'@') {
			return TokenKind::Attribute(ident);
		}
		if self.current_char == Some('"') {
			return self.lex_string_literal(Some(ident));
		}
		return TokenKind::Identifier(ident);
	}

	fn lex_escape_sequence(&mut self) -> Option<char>
	{
		let escaped = match self.current_char? {
			'n' => '\n',
			't' => '\t',
			'r' => '\r',
			'0' => '\0',
			'\\' => '\\',
			'\'' => '\'',
			'"' => '"',
			'x' => {
				self.advance();
				let mut hex_str = String::new();

				for _ in 0..2 {
					if let Some(ch) = self.current_char {
						if ch.is_ascii_hexdigit() {
							hex_str.push(ch);
							self.advance();
						} else {
							break;
						}
					} else {
						break;
					}
				}

				if hex_str.is_empty() {
					return None;
				}

				if let Ok(value) = u8::from_str_radix(&hex_str, 16) {
					return Some(value as char);
				}
				return None;
			}
			'u' => {
				self.advance();

				if self.current_char != Some('{') {
					return None;
				}
				self.advance();

				let mut hex_str: String = String::new();

				while let Some(ch) = self.current_char {
					if ch == '}' {
						break;
					} else if ch.is_ascii_hexdigit() {
						hex_str.push(ch);
						self.advance();
					} else {
						return None;
					}
				}

				if self.current_char != Some('}') {
					return None;
				}
				self.advance();

				if hex_str.is_empty() || hex_str.len() > 6 {
					return None;
				}

				if let Ok(value) = u32::from_str_radix(&hex_str, 16) {
					return char::from_u32(value);
				}
				return None;
			}
			_ => return None,
		};

		if escaped != '\0' || self.current_char == Some('0') {
			self.advance();
		}

		return Some(escaped);
	}

	fn lex_string_literal(&mut self, flags_str: Option<&str>) -> TokenKind<'s>
	{
		let start: usize = self.position;
		let start_line: usize = self.line;

		self.advance(); // `"`

		let flags: StringFlags = flags_str.map_or_else(StringFlags::default, |str| StringFlags::from_string(&str));

		let start_string: usize = self.position;

		while let Some(ch) = self.current_char {
			match ch {
				'"' => {
					let end: usize = self.position;
					self.advance(); // `"`

					return TokenKind::StringLiteral {
						string: &self.source[start_string..end],
						flags,
					};
				}

				'\\' => {
					self.advance(); // `\`

					if self.lex_escape_sequence().is_none() {
						self.diagnostics.push_back(TokenKind::Diag(
							Diagnostic::error("invalid escape sequence").primary(
								Span {
									file: self.file_id,
									line: start_line,
									start,
									end: self.position,
								},
								Some("invalid escape sequence".to_string()),
							),
						));
						return TokenKind::Invalid;
					}
				}

				_ => self.advance(),
			}
		}

		// Unterminated string
		self.diagnostics
			.push_back(TokenKind::Diag(Diagnostic::error("Unterminated string").primary(
				Span {
					file: self.file_id,
					line: start_line,
					start,
					end: start + 1,
				},
				Some("Unterminated string".to_string()),
			)));
		return TokenKind::Invalid;
	}

	fn lex_char_or_label(&mut self) -> TokenKind<'s>
	{
		let backup: BasicLexerBackup = self.make_backup();
		let start: usize = self.position;

		self.advance(); // `'`

		match self.current_char {
			Some(ch) if ch.is_alphabetic() || ch == '_' => {
				self.advance();

				let is_multi_char: bool = self
					.current_char
					.is_some_and(|c| return c.is_alphanumeric() || c == '_');

				if is_multi_char {
					while let Some(c) = self.current_char {
						if c.is_alphanumeric() || c == '_' {
							self.advance();
						} else {
							break;
						}
					}
					return TokenKind::Label(&self.source[start..self.position]);
				} else if self.current_char == Some('\'') {
					self.load_backup(backup);
					return self.lex_char_literal();
				}
				return TokenKind::Label(&self.source[start..self.position]);
			}
			_ => {
				self.load_backup(backup);
				return self.lex_char_literal();
			}
		}
	}

	fn lex_char_literal(&mut self) -> TokenKind<'s>
	{
		let start: usize = self.position;
		let start_line: usize = self.line;

		self.advance(); // `'`

		let ch: char = if self.current_char == Some('\\') {
			self.advance();
			let Some(ch) = self.lex_escape_sequence() else {
				self.advance();
				self.diagnostics
					.push_back(TokenKind::Diag(Diagnostic::error("invalid escape sequence").primary(
						Span {
							file: self.file_id,
							line: start_line,
							start,
							end: self.position,
						},
						Some("invalid escape sequence".to_string()),
					)));
				if self.current_char == Some('\'') {
					self.advance();
				}
				return TokenKind::Invalid;
			};
			ch
		} else {
			let Some(ch) = self.current_char else {
				self.diagnostics
					.push_back(TokenKind::Diag(Diagnostic::error("reached EOF").primary(
						Span {
							file: self.file_id,
							line: start_line,
							start,
							end: self.position,
						},
						Some("reached EOF".to_string()),
					)));
				return TokenKind::Invalid;
			};
			self.advance();
			ch
		};

		if self.current_char == Some('\'') {
			self.advance(); // `'`
			return TokenKind::CharLiteral(ch);
		}
		self.diagnostics
			.push_back(TokenKind::Diag(Diagnostic::error("reached EOF").primary(
				Span {
					file: self.file_id,
					line: start_line,
					start,
					end: self.position,
				},
				Some("reached EOF".to_string()),
			)));
		return TokenKind::Invalid;
	}

	fn read_int_suffix(&mut self) -> Option<IntType>
	{
		let backup: BasicLexerBackup = self.make_backup();

		let result: Option<IntType> = (|| {
			let sign: IntSign = match self.current_char {
				Some('u') => {
					self.advance();
					IntSign::Unsigned
				}
				Some('i') => {
					self.advance();
					IntSign::Signed
				}
				_ => return None,
			};

			if self.current_char == Some('s') {
				let start: usize = self.position;

				for _ in 0..4
				/* size */
				{
					self.advance();
				}

				if self.source[start..self.position] != *"size" {
					return None;
				}

				return Some(IntType {
					bits: IntSize::Size,
					sign,
				});
			}

			let start: usize = self.position;
			while let Some(ch) = self.current_char {
				if ch.is_ascii_digit() {
					self.advance();
				} else {
					break;
				}
			}

			if start == self.position {
				return None;
			}

			let bits: u16 = self.source[start..self.position].parse::<u16>().ok()?;

			return Some(IntType {
				bits: IntSize::Fixed(bits),
				sign,
			});
		})();

		return result.map_or_else(
			|| {
				self.load_backup(backup);
				return None;
			},
			Some,
		);
	}

	fn read_float_suffix(&mut self) -> Option<u16>
	{
		if self.current_char != Some('f') {
			return None;
		}

		self.advance();

		let start: usize = self.position;
		while let Some(ch) = self.current_char {
			if ch.is_ascii_digit() {
				self.advance();
			} else {
				break;
			}
		}

		return self.source[start..self.position].parse::<u16>().ok();
	}

	fn read_radix_number<F>(&mut self, radix: u32, valid: F) -> TokenKind<'s>
	where
		F: Fn(char) -> bool,
	{
		let start: usize = self.position;
		let mut has_underscore: bool = false;
		let mut ty: Option<IntType> = None;
		let mut end: usize = start;

		while let Some(ch) = self.current_char {
			if valid(ch) || ch == '_' {
				self.advance();
			} else {
				end = self.position;
				ty = self.read_int_suffix();
				if let Some(c) = self.current_char
					&& c.is_ascii_alphanumeric()
				{
					let backup: BasicLexerBackup = self.make_backup();
					let first_span: Span = Span {
						file: self.file_id,
						line: self.line,
						start,
						end: self.position,
					};
					let second_span: Span = self.next_token().span;
					self.diagnostics.push_back(TokenKind::Diag(
						Diagnostic::warning("number is split") // TODO: make the error message better
							.primary(first_span, Some("first token".to_string()))
							.secondary(second_span, Some("second token".to_string())),
					));
					self.load_backup(backup);
				}
				break;
			}
		}

		let value: &str = &self.source[start..if end == start { self.position } else { end }];

		let base: IntBase = match radix {
			2 => IntBase::Binary,
			8 => IntBase::Octal,
			16 => IntBase::Hexadecimal,
			_ => IntBase::Decimal,
		};

		return TokenKind::IntLiteral { value, base, ty };
	}

	fn lex_number(&mut self) -> TokenKind<'s>
	{
		let start: usize = self.position;
		let mut has_underscore: bool = false;

		if self.current_char == Some('0') {
			match self.peek() {
				Some('x') => {
					self.advance(); // 0
					self.advance(); // x
					return self.read_radix_number(16, |c| return c.is_ascii_hexdigit());
				}
				Some('b') => {
					self.advance(); // 0
					self.advance(); // b
					return self.read_radix_number(2, |c| return c == '0' || c == '1');
				}
				Some('o') => {
					self.advance(); // 0
					self.advance(); // o
					return self.read_radix_number(8, |c| return ('0'..='7').contains(&c));
				}
				_ => {}
			}
		}
		let mut end: usize = start;
		let mut ty: Option<IntType> = None;

		while let Some(ch) = self.current_char {
			if ch.is_ascii_digit() || ch == '_' {
				self.advance();
			} else {
				if self.current_char == Some('.') {
					break;
				}
				end = self.position;
				ty = self.read_int_suffix();
				if let Some(c) = self.current_char
					&& c.is_ascii_alphanumeric()
				{
					let backup: BasicLexerBackup = self.make_backup();
					let first_span: Span = Span {
						file: self.file_id,
						line: self.line,
						start,
						end,
					};
					let second_span: Span = self.next_token().span;
					self.diagnostics.push_back(TokenKind::Diag(
						Diagnostic::warning("number is split") // TODO: make the error message better
							.primary(first_span, Some("first token".to_string()))
							.secondary(second_span, Some("second token".to_string())),
					));
					self.load_backup(backup);
				}
				break;
			}
		}

		if self.current_char == Some('.') && self.peek().is_some_and(|c| return c.is_ascii_digit()) {
			self.advance(); // .

			let mut bits: Option<u16> = None;
			let mut end: usize = start;

			while let Some(ch) = self.current_char {
				if ch.is_ascii_digit() || ch == '_' {
					if ch == '_' {
						has_underscore = true;
					}
					self.advance();
				} else {
					end = self.position;
					bits = self.read_float_suffix();
					if let Some(c) = self.current_char
						&& c.is_ascii_alphanumeric()
					{
						let backup: BasicLexerBackup = self.make_backup();
						let first_span: Span = Span {
							file: self.file_id,
							line: self.line,
							start,
							end: self.position,
						};
						let second_span: Span = self.next_token().span;
						self.diagnostics.push_back(TokenKind::Diag(
							Diagnostic::warning("number is split") // TODO: make the error message better
								.primary(first_span, Some("first token".to_string()))
								.secondary(second_span, Some("second token".to_string())),
						));
						self.load_backup(backup);
					}
					break;
				}
			}

			let value: &str = &self.source[start..if end == start { self.position } else { end }];

			return TokenKind::FloatLiteral { value, bits };
		}

		let value: &str = &self.source[start..if end == start { self.position } else { end }];

		return TokenKind::IntLiteral {
			value,
			base: IntBase::Decimal,
			ty,
		};
	}

	fn lex_slash_or_comment(&mut self) -> TokenKind<'s>
	{
		let backup: BasicLexerBackup = self.make_backup();
		self.advance(); // `/`
		return match self.current_char {
			Some('/') => {
				self.advance();
				self.lex_line_comment()
			}
			Some('*') => {
				self.advance();
				self.lex_block_comment()
			}
			_ => {
				self.load_backup(backup);
				self.lex_slash_family()
			}
		};
	}

	fn lex_line_comment(&mut self) -> TokenKind<'s>
	{
		let mut is_doc: bool = false;
		let mut is_encloded_doc: bool = false;
		if self.current_char == Some('/') {
			is_doc = true;
			self.advance();
		} else if self.current_char == Some('!') {
			is_encloded_doc = true;
			self.advance();
		}

		let start: usize = self.position;

		while let Some(ch) = self.current_char {
			if ch == '\n' {
				break;
			}
			self.advance();
		}

		if is_doc {
			return TokenKind::DocsComment(&self.source[start..self.position]);
		}
		if is_encloded_doc {
			return TokenKind::EnclosedDocsComment(&self.source[start..self.position]);
		}
		return TokenKind::LineComment(&self.source[start..self.position]);
	}

	fn lex_block_comment(&mut self) -> TokenKind<'s>
	{
		let is_doc: bool = self.current_char == Some('*') && self.peek() != Some('/');
		if is_doc {
			self.advance();
		}

		let start: usize = self.position;
		let mut end: usize = self.position;

		while let Some(ch) = self.current_char {
			if ch == '*' && self.peek() == Some('/') {
				end = self.position;
				self.advance(); // `*`
				self.advance(); // `/`
				break;
			}
			self.advance();
		}

		if is_doc {
			return TokenKind::DocsComment(&self.source[start..end]);
		}
		return TokenKind::BlockComment(&self.source[start..end]);
	}
}

// fn check_irregular_number_splitting<const FORWARDS: bool>(
// 	input: &str,
// 	span: Span,
// 	idx: Option<usize>,
// ) -> Option<Diagnostic>
// {
// 	let nidx: usize = idx.unwrap_or_else(|| {
// 		if FORWARDS {
// 			return input
// 				.bytes()
// 				.position(|ch| return ch == b'_')
// 				.expect("previous checks should have determined that there are underscores");
// 		}
// 		return input
// 			.bytes()
// 			.rev()
// 			.position(|ch| return ch == b'_')
// 			.expect("previous checks should have determined that there are underscores");
// 	});
//
// 	let valid: bool = if FORWARDS {
// 		input.bytes().enumerate().all(|(i, ch)| {
// 			if (i + 1) % (nidx + 1) == 0 {
// 				return ch == b'_';
// 			}
// 			return ch != b'_';
// 		})
// 	} else {
// 		input.bytes().rev().enumerate().all(|(i, ch)| {
// 			if (i + 1) % (nidx + 1) == 0 {
// 				return ch == b'_';
// 			}
// 			return ch != b'_';
// 		})
// 	};
// 	if !valid {
// 		return Some(
// 			Diagnostic::warning("irregular number splitting")
// 				.primary(span, Some("irregular number splitting".to_string())),
// 		);
// 	}
// 	return None;
// }
