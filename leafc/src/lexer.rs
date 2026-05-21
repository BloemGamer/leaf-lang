pub mod expander;

#[cfg(test)]
#[path = "../tests/lexer/tests.rs"]
mod tests;

use std::marker::PhantomData;
use std::path;

use leaf_proc::{Spanned, generate_lexer};

use crate::bit_enum;
use crate::config::Config;
use crate::diagnostics::DiagnosticBuilder;
use crate::parser::ParseError;
use crate::source_map::{SourceIndex, SourceMap};

pub trait LexerTrait<'source, 'config>: Iterator<Item = Result<Token, Box<DiagnosticBuilder>>> + Clone
{
	fn into_parts(self) -> (&'config Config, SourceIndex, Self);
	fn read_eof_returned(&self) -> bool;
	fn set_eof_returned(&mut self, eof_returned: bool);
	fn next_token(&mut self) -> Result<Token, Box<DiagnosticBuilder>>;
}

impl<'source, 'config> LexerTrait<'source, 'config> for Lexer<'source, 'config>
{
	/// Consumes the lexer and extracts its configuration reference.
	///
	/// This method is primarily used when converting a lexer into a parser,
	/// allowing the parser to store both the configuration reference and the
	/// lexer itself without violating Rust's borrowing rules.
	///
	/// # Returns
	/// A tuple containing:
	/// * `&'config Config` - Reference to the configuration object
	/// * `SourceIndex` - Index into the source map
	/// * `Lexer<'source, 'config>` - The lexer itself, moved out and ready to be consumed
	///
	/// # Example
	/// ```no_run
	/// # use crate::{Config, SourceIndex};
	/// # use crate::lexer::Lexer;
	/// let config = Config::default();
	/// let source_index = SourceIndex(0);
	/// let lexer = Lexer::new(&config, "fn main() {}", source_index);
	/// let (config_ref, idx, lexer) = lexer.into_parts();
	/// // Now config_ref, idx, and lexer can be used independently
	/// ```
	fn into_parts(self) -> (&'config Config, SourceIndex, Lexer<'source, 'config>)
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

	/// Retrieves the next token from the source code.
	///
	/// This is the main interface for the parser to consume tokens. It skips
	/// whitespace and returns the next meaningful token with its position information.
	///
	/// # Returns
	/// The next `Token` from the source, including its kind and span information.
	/// Returns a token with `TokenKind::Eof` when the end of the source is reached.
	///
	/// # Example
	/// ```no_run
	/// # use crate::{Config, SourceIndex};
	/// # use crate::lexer::Lexer;
	/// # let config = Config::default();
	/// # let source_index = SourceIndex(0);
	/// let mut lexer = Lexer::new(&config, "x + 42", source_index);
	/// let token = lexer.next_token(); // Returns Identifier("x")
	/// let token = lexer.next_token(); // Returns Plus
	/// let token = lexer.next_token(); // Returns IntLiteral(42)
	/// ```
	#[allow(unused)]
	fn next_token(&mut self) -> Result<Token, Box<DiagnosticBuilder>>
	{
		self.skip_whitespace();

		let start: usize = self.position;
		let start_line: usize = self.line;
		let start_col: usize = self.column;

		let Some(ch) = self.current_char else {
			return Ok(Token {
				kind: TokenKind::Eof,
				span: Span {
					start,
					end: start,
					start_line,
					start_col,
					end_line: start_line,
					end_col: start_col,
					source_index: self.source_index,
				},
			});
		};

		let kind: TokenKind = match ch {
			// Special case: / needs comment handling
			'/' => self.lex_slash_or_comment(),

			// Literals
			'"' => self.lex_string_literal(None),
			'\'' => self.lex_char_or_label(),
			'0'..='9' => self.lex_number(),

			// Identifiers and keywords
			'a'..='z' | 'A'..='Z' | '_' | '#' => self.lex_identifier_or_keyword(),

			// Directives
			'@' => self.lex_directive(),

			// Macros
			'$' => self.lex_macro(),

			// All operators and simple tokens - handled by generated lex_char
			_ => self.lex_char(ch),
		};

		let end = self.position;
		let end_line = self.line;
		let end_col = self.column;

		return Ok(Token {
			kind,
			span: Span {
				start,
				end,
				start_line,
				start_col,
				end_line,
				end_col,
				source_index: self.source_index,
			},
		});
	}
}

/// Lexical analyzer for tokenizing source code.
///
/// The lexer performs lexical analysis by scanning through source code character by character
/// and producing a stream of tokens. It maintains position information for error reporting
/// and handles various token types including literals, keywords, operators, and comments.
///
/// # Lifetimes
/// * `'source` - Lifetime of the source code string being tokenized
/// * `'config` - Lifetime of the configuration object
///
/// # Example
/// ```no_run
/// # use crate::{Config, SourceIndex};
/// # use crate::lexer::Lexer;
/// let config = Config::default();
/// let source = "fn main() { var x = 42; }";
/// let source_index = SourceIndex(0);
/// let mut lexer = Lexer::new(&config, source, source_index);
///
/// while let Some(token) = lexer.next() {
///     println!("{:?}", token);
/// }
/// ```
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Lexer<'source, 'config>
{
	source: &'source str,
	source_index: SourceIndex,
	config: &'config Config,
	position: usize,
	current_char: Option<char>,
	line: usize,
	column: usize,
	eof_returned: bool,
}

impl<'source, 'config> Lexer<'source, 'config>
{
	const fn make_checkpoint(&self) -> LexerCheckpoint<'source, 'config>
	{
		return LexerCheckpoint {
			position: self.position,
			current_char: self.current_char,
			line: self.line,
			column: self.column,
			eof_returned: self.eof_returned,
			_marker: PhantomData,
		};
	}

	#[allow(clippy::needless_pass_by_value)]
	const fn restore_checkpoint(&mut self, checkpoint: LexerCheckpoint<'source, 'config>)
	{
		self.position = checkpoint.position;
		self.current_char = checkpoint.current_char;
		self.line = checkpoint.line;
		self.column = checkpoint.column;
		self.eof_returned = checkpoint.eof_returned;
	}
}

struct LexerCheckpoint<'source, 'config>
{
	position: usize,
	current_char: Option<char>,
	line: usize,
	column: usize,
	eof_returned: bool,
	_marker: PhantomData<(&'source str, &'config Config)>,
}

/// A token produced by the lexer.
///
/// Represents a single lexical unit from the source code, containing both the
/// token's semantic meaning (kind) and its location in the source (span).
///
/// # Fields
/// * `kind` - The semantic type and value of the token
/// * `span` - Position information for error reporting and source mapping
///
/// # Example
/// ```no_run
/// # use crate::lexer::{Token, TokenKind, Span};
/// let token = Token {
///     kind: TokenKind::IntLiteral(42),
///     span: Span {
///         start: 10,
///         end: 12,
///         start_line: 1,
///         start_col: 11,
///         end_line: 1,
///         end_col: 13,
///     }
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Spanned)]
pub struct Token
{
	pub kind: TokenKind,
	pub span: Span,
}

impl Token
{
	pub fn check_reserved(&self) -> Result<(), ReservedError>
	{
		if self.kind.check_reserved().is_err() {
			return Err(ReservedError {
				token: self.kind.clone(),
			});
		}
		return Ok(());
	}
}

#[derive(Clone, Debug)]
pub struct ReservedError
{
	pub token: TokenKind,
}

#[allow(unused)]
pub trait Spanned
{
	fn span(&self) -> Span;
}

#[allow(unused)]
pub trait ErrorFromSpan
{
	fn from_span(span: impl Spanned, message: impl Into<String>) -> Self;
}

/// Source code position information for a token.
///
/// Tracks both byte offsets and line/column positions for a span of source code.
/// This information is used for error reporting and debugging.
///
/// # Fields
/// * `start` - Byte offset of the start of the span
/// * `end` - Byte offset of the end of the span (exclusive)
/// * `start_line` - Line number where the span starts (1-indexed)
/// * `start_col` - Column number where the span starts (1-indexed)
/// * `end_line` - Line number where the span ends (1-indexed)
/// * `end_col` - Column number where the span ends (1-indexed)
///
/// # Example
/// ```no_run
/// # use crate::lexer::Span;
/// let span = Span {
///     start: 0,
///     end: 5,
///     start_line: 1,
///     start_col: 1,
///     end_line: 1,
///     end_col: 6,
/// };
/// ```
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Span
{
	pub start: usize,
	pub end: usize,
	pub start_line: usize,
	pub start_col: usize,
	pub end_line: usize,
	pub end_col: usize,
	pub source_index: SourceIndex,
}

impl Default for Span
{
	fn default() -> Self
	{
		return Self {
			start: 0,
			end: 0,
			start_line: 0,
			start_col: 0,
			end_line: 0,
			end_col: 0,
			source_index: SourceIndex::new(usize::MAX),
		};
	}
}

impl Spanned for Span
{
	fn span(&self) -> Span
	{
		return *self;
	}
}

impl Span
{
	/// Merges two Spans together.
	///
	/// Creates a new span that encompasses both input spans, from the start of the
	/// earlier span to the end of the later span.
	///
	/// # Arguments
	/// * `self` - The first span
	/// * `other` - The second span to merge with
	///
	/// # Returns
	/// A new Span ranging from the start of the first span to the end of the second one
	///
	/// # Example
	/// ```no_run
	/// # use crate::lexer::Span;
	/// # let old_span1 = Span::default();
	/// # let old_span2 = Span::default();
	/// let new_span: Span = old_span1.merge(&old_span2);
	/// ```
	pub fn merge(&self, other: &Span) -> Self
	{
		assert!(
			self.source_index == other.source_index,
			"Bug: two spans of other sourcefiles can't be merged\nfirst: {self:?}\nsecond: {other:?}"
		);
		return Self {
			start: self.start.min(other.start),
			end: self.end.max(other.end),
			start_line: self.start_line.min(other.start_line),
			start_col: self.start_col.min(other.start_col),
			end_line: self.end_line.max(other.end_line),
			end_col: self.end_col.max(other.end_col),
			source_index: self.source_index,
		};
	}
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntType
{
	pub bits: IntSize,
	pub sign: IntSign,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntSize
{
	Size,
	Fixed(u16),
	Generic(String),
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
			IntSize::Generic(generic) => write!(f, "<{generic}>"),
		};
	}
}

bit_enum!(pub struct StringFlags : u8
	{
		INVALID = 0b01,
		CSTRING = 0b10,
	}
);

impl std::fmt::Display for StringFlags
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		if self.contains_single(StringFlags::INVALID) {
			return write!(f, "<INVALID FLAG>");
		}
		if self.contains_single(StringFlags::CSTRING) {
			write!(f, "c")?;
		}
		return Ok(());
	}
}

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

/// The semantic type and value of a token.
///
/// This enum represents all possible token types that can be produced by the lexer,
/// including literals, keywords, operators, punctuation, and special tokens.
///
/// # Categories
/// - **Literals**: Integer, float, char, string, and boolean values
/// - **Identifiers**: Variable and function names
/// - **Keywords**: Language keywords for control flow, declarations, and modifiers
/// - **Operators**: Arithmetic, bitwise, logical, comparison, and assignment operators
/// - **Delimiters**: Parentheses, braces, and brackets
/// - **Punctuation**: Semicolons, colons, commas, dots, arrows, etc.
/// - **Special**: Macros, directives, comments
/// - **End/Error**: EOF and invalid tokens
#[derive(Debug, Clone, PartialEq, Eq)]
#[generate_lexer]
pub enum TokenKind
{
	// ===== Literals =====
	/// Integer literal: `42`, `-10`, `0xFF`
	IntLiteral
	{
		value: String,
		base: IntBase,
		ty: Option<IntType>,
	},
	/// Floating point literal: `3.14`, `-0.5`, `1e10`
	FloatLiteral
	{
		value: String, bits: Option<u16>
	},
	/// Character literal: `'a'`, `'\n'`, `'\0'`
	CharLiteral(char),
	/// String literal: `"hello"`, `"world\n"`
	StringLiteral
	{
		string: String, flags: StringFlags
	},
	/// Boolean literal: `true`
	#[keyword("true")]
	True,
	/// Boolean literal: `false`
	#[keyword("false")]
	False,

	// ===== Identifiers =====
	/// Variable/function names: `foo`, `bar`, `my_var`
	Identifier(String),
	/// Wildcard pattern: `_`
	#[keyword("_")]
	Underscore,
	/// Self keyword: `self`
	#[keyword("self")]
	SelfKw,
	/// Default keyword: `default`
	#[keyword("default")]
	Default,
	/// Label: `'label`
	Label(String),

	// ===== Keywords - Control Flow =====
	/// Conditional: `if`
	#[keyword("if")]
	If,
	/// Conditional alternative: `else`
	#[keyword("else")]
	Else,
	/// Loop: `while`
	#[keyword("while")]
	While,
	/// Iterator loop: `for`
	#[keyword("for")]
	For,
	/// Loop: `loop`
	#[keyword("loop")]
	Loop,
	/// Pattern matching: `switch`
	#[keyword("switch")]
	Switch,
	/// Return from function: `return`
	#[keyword("return")]
	Return,
	/// Exit loop: `break`
	#[keyword("break")]
	Break,
	/// Skip to next iteration: `continue`
	#[keyword("continue")]
	Continue,
	/// Call the destructor for a type: `delete`
	#[keyword("delete")]
	Delete,

	// ===== Keywords - Declarations =====
	/// Function definition: `fn`
	#[keyword("fn")]
	FuncDef,
	/// Constant declaration: `const`
	#[keyword("const")]
	Const,
	/// Variable declaration: `var`
	#[keyword("var")]
	Var,
	/// Static variable: `static`
	#[keyword("static")]
	Static,
	/// Structure definition: `struct`
	#[keyword("struct")]
	Struct,
	/// Untagged union: `union`
	#[keyword("union")]
	Union,
	/// Tagged union: `variant`
	#[keyword("variant")]
	Variant,
	/// Enumeration definition: `enum`
	#[keyword("enum")]
	Enum,
	/// Implementation block: `impl`
	#[keyword("impl")]
	Impl,
	/// Trait definition: `trait`
	#[keyword("trait")]
	Trait,
	/// Macro definition: `macro`
	#[keyword("macro")]
	MacroDef,
	/// Namespace declaration: `module`
	#[keyword("module")]
	Module,
	/// Type alias: `type`
	#[keyword("type")]
	Type,
	#[keyword("assoc")]
	Assoc,

	// ===== Keywords - Modifiers =====
	/// Public visibility: `pub`
	#[keyword("pub")]
	Pub,
	#[keyword("export")]
	Export,
	/// Mutable binding: `mut`
	#[keyword("mut")]
	Mut,
	/// Unsafe block/function: `unsafe`
	#[keyword("unsafe")]
	Unsafe,
	/// Volatile memory access: `volatile`
	#[keyword("volatile")]
	Volatile,
	/// Inline function: `inline`
	#[keyword("inline")]
	Inline,
	#[keyword("extern")]
	Extern,

	// ===== Keywords - Other =====
	/// Iterator source: `in` (for x in iter)
	#[keyword("in")]
	In,
	/// Type casting: `as`
	#[keyword("as")]
	As,
	/// Generic constraints: `where`
	#[keyword("where")]
	Where,

	// ===== Arithmetic Operators =====
	/// Addition: `+`
	#[operator("+")]
	Plus,
	/// Subtraction or negation: `-`
	#[operator("-")]
	Minus,
	/// Multiplication or dereference: `*`
	#[operator("*")]
	Star,
	/// Division: `/`
	#[operator("/")]
	Slash,
	/// Modulo/remainder: `%`
	#[operator("%")]
	Mod,

	// ===== Bitwise Operators =====
	/// Bitwise OR: `|`
	#[operator("|")]
	Pipe,
	/// Bitwise AND or reference: `&`
	#[operator("&")]
	Ampersand,
	/// Bitwise XOR: `^`
	#[operator("^")]
	Caret,
	/// Bitwise NOT: `~`
	#[operator("~")]
	Tilde,
	/// Left shift: `<<`
	#[operator("<<")]
	LShift,
	/// Right shift: `>>`
	#[operator(">>")]
	RShift,

	// ===== Logical Operators =====
	/// Logical NOT: `!`
	#[operator("!")]
	Bang,
	/// Logical AND: `&&`
	#[operator("&&")]
	And,
	/// Logical OR: `||`
	#[operator("||")]
	Or,

	// ===== Comparison Operators =====
	/// Less than: `<`
	#[operator("<")]
	LessThan,
	/// Greater than: `>`
	#[operator(">")]
	GreaterThan,
	/// Less than or equal: `<=`
	#[operator("<=")]
	LessEquals,
	/// Greater than or equal: `>=`
	#[operator(">=")]
	GreaterEquals,
	/// Equality: `==`
	#[operator("==")]
	EqualsEquals,
	/// Inequality: `!=`
	#[operator("!=")]
	BangEquals,

	// ===== Assignment Operators =====
	/// Assignment: `=`
	#[operator("=")]
	Equals,
	/// Add and assign: `+=`
	#[operator("+=")]
	PlusEquals,
	/// Subtract and assign: `-=`
	#[operator("-=")]
	MinusEquals,
	/// Multiply and assign: `*=`
	#[operator("*=")]
	StarEquals,
	/// Divide and assign: `/=`
	#[operator("/=")]
	SlashEquals,
	/// Modulo and assign: `%=`
	#[operator("%=")]
	ModEquals,
	/// Bitwise OR and assign: `|=`
	#[operator("|=")]
	PipeEquals,
	/// Bitwise AND and assign: `&=`
	#[operator("&=")]
	AmpersandEquals,
	/// Bitwise XOR and assign: `^=`
	#[operator("^=")]
	CaretEquals,
	/// Bitwise NOT and assign: `~=`
	#[operator("~=")]
	TildeEquals,
	/// Left shift and assign: `<<=`
	#[operator("<<=")]
	LShiftEquals,
	/// Right shift and assign: `>>=`
	#[operator(">>=")]
	RShiftEquals,

	// ===== Delimiters =====
	/// Opening parenthesis: `(`
	#[simple_token("(")]
	LeftParen,
	/// Closing parenthesis: `)`
	#[simple_token(")")]
	RightParen,
	/// Opening brace: `{`
	#[simple_token("{")]
	LeftBrace,
	/// Closing brace: `}`
	#[simple_token("}")]
	RightBrace,
	/// Opening bracket: `[`
	#[simple_token("[")]
	LeftBracket,
	/// Closing bracket: `]`
	#[simple_token("]")]
	RightBracket,

	// ===== Punctuation =====
	/// Statement terminator: `;`
	#[simple_token(";")]
	Semicolon,
	/// Type annotation: `:`
	#[operator(":")]
	Colon,
	/// Path separator/module: `::`
	#[operator("::")]
	DoubleColon,
	/// List separator: `,`
	#[simple_token(",")]
	Comma,
	/// Member access: `.`
	#[operator(".")]
	Dot,
	/// Range: `..`
	#[operator("..")]
	DotDot,
	/// Inclusive range: `..=`
	#[operator("..=")]
	DotDotEquals,
	/// Variadic: `...`
	#[operator("...")]
	Ellipsis,
	/// Function return type: `->`
	#[operator("->")]
	Arrow,
	/// Switch arm: `=>`
	#[operator("=>")]
	FatArrow,
	#[simple_token("?")]
	QuestionMark,
	#[reserved]
	/// Escape character: `\`
	#[simple_token("\\")]
	Backslash,

	// ===== Special Tokens =====
	/// Macro invocation: `$identifier`
	Macro(String),
	/// Compiler directive: `@identifier`
	Directive(Directive),

	// ===== Comments =====
	/// Single-line comment: `// comment`
	LineComment(String),
	/// Multi-line comment: `/* comment */`
	BlockComment(String),
	/// Documentation comment: `///` or `/**`
	DocsComment(String),

	// ===== End/Error =====
	/// End of file
	Eof,
	/// Invalid/unrecognized token
	Invalid,

	// ===== Reserved =====
	/// Reserved `async`
	#[reserved]
	#[keyword("async")]
	Async,
	/// Reserved for iterator kind function `gen`
	#[reserved]
	#[keyword("gen")]
	Gen,
	/// Reserved `try`
	#[reserved]
	#[keyword("try")]
	Try,
	#[reserved]
	#[keyword("defer")]
	Defer,
	#[reserved]
	#[keyword("yield")]
	Yield,
}

/// Compiler directive types.
///
/// Represents the different kinds of compiler directives that can be parsed
/// from source code. Directives are prefixed with `@` in the source.
///
/// # Variants
/// * `Use` - Import directive: `@use`
/// * `Import` - Import directive: `@import`
/// * `Custom` - User-defined directive: `@custom_name`
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Directive
{
	Use,
	Import,
	Custom(String),
}

impl Iterator for Lexer<'_, '_>
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

impl<'source, 'config> Lexer<'source, 'config>
{
	/// Creates a new lexer instance.
	///
	/// Initializes the lexer with the provided configuration and source code,
	/// positioning it at the beginning of the source text.
	///
	/// # Arguments
	/// * `config` - Reference to the lexer configuration
	/// * `source` - Source code string to tokenize
	/// * `source_index` - Index into the source map
	///
	/// # Returns
	/// A new `Lexer` instance initialized at position 0, line 1, column 1
	///
	/// # Example
	/// ```no_run
	/// # use crate::{Config, SourceIndex};
	/// # use crate::lexer::Lexer;
	/// let config = Config::default();
	/// let source = "var x = 42;";
	/// let source_index = SourceIndex(0);
	/// let lexer = Lexer::new(&config, source, source_index);
	/// ```
	#[allow(unused)]
	pub fn new(config: &'config Config, source: &'source str, source_index: SourceIndex) -> Self
	{
		let mut lexer: Lexer<'_, '_> = Lexer {
			source,
			source_index,
			config,
			position: 0,
			current_char: None,
			line: 1,
			column: 1,
			eof_returned: false,
		};
		lexer.current_char = lexer.source.chars().next();
		return lexer;
	}

	/// Creates a new lexer and adds the source to the source map.
	///
	/// This is a convenience method that adds the source code to a source map
	/// and creates a lexer that references it.
	///
	/// # Arguments
	/// * `config` - Reference to the lexer configuration
	/// * `source` - Source code to tokenize (will be moved into the source map)
	/// * `file_name` - Name of the source file for error reporting
	/// * `source_map` - Mutable reference to the source map
	///
	/// # Returns
	/// A new `Lexer` instance
	///
	/// # Example
	/// ```no_run
	/// # use crate::{Config, SourceMap};
	/// # use crate::lexer::Lexer;
	/// let config = Config::default();
	/// let mut source_map = SourceMap::new();
	/// let lexer = Lexer::new_add_to_source_map(
	///     &config,
	///     "var x = 42;",
	///     "main.src",
	///     &mut source_map
	/// );
	/// ```
	#[allow(unused)]
	pub fn new_add_to_source_map(
		config: &'config Config,
		source: impl Into<String>,
		file_name: impl Into<path::PathBuf>,
		source_map: &'source mut SourceMap,
	) -> Self
	{
		let source_index: SourceIndex = source_map.add_file(file_name, source);
		let new_source: &String = &source_map
			.get(source_index)
			.expect("Bug: The sourcemap was not added")
			.src;
		let mut lexer: Lexer<'_, '_> = Lexer {
			source: new_source,
			source_index,
			config,
			position: 0,
			current_char: None,
			line: 1,
			column: 1,
			eof_returned: false,
		};
		lexer.current_char = lexer.source.chars().next();
		return lexer;
	}

	fn lex_slash_or_comment(&mut self) -> TokenKind
	{
		self.advance(); // consume '/'
		return match self.current_char {
			Some('=') => {
				self.advance();
				TokenKind::SlashEquals
			}
			Some('/') => {
				// Line comment
				self.advance();
				self.lex_line_comment()
			}
			Some('*') => {
				// Block comment
				self.advance();
				self.lex_block_comment()
			}
			_ => TokenKind::Slash,
		};
	}

	fn lex_line_comment(&mut self) -> TokenKind
	{
		let mut comment = String::new();

		let is_doc = self.current_char == Some('/');
		if is_doc {
			self.advance();
		}

		while let Some(ch) = self.current_char {
			if ch == '\n' {
				break;
			}
			comment.push(ch);
			self.advance();
		}

		if is_doc {
			return TokenKind::DocsComment(comment);
		}
		return TokenKind::LineComment(comment);
	}

	fn lex_block_comment(&mut self) -> TokenKind
	{
		let mut comment = String::new();

		let is_doc = self.current_char == Some('*') && self.peek() != Some('/');
		if is_doc {
			self.advance();
		}

		while let Some(ch) = self.current_char {
			if ch == '*' && self.peek() == Some('/') {
				self.advance(); // consume '*'
				self.advance(); // consume '/'
				break;
			}
			comment.push(ch);
			self.advance();
		}

		if is_doc {
			return TokenKind::DocsComment(comment);
		}
		return TokenKind::BlockComment(comment);
	}

	fn lex_string_literal(&mut self, flags_str: Option<String>) -> TokenKind
	{
		self.advance(); // consume opening '"'
		let mut string = String::new();

		let flags: StringFlags =
			flags_str.map_or_else(StringFlags::default, |str| return StringFlags::from_string(&str));

		while let Some(ch) = self.current_char {
			if ch == '"' {
				self.advance(); // consume closing '"'
				return TokenKind::StringLiteral { string, flags };
			} else if ch == '\\' {
				self.advance();
				if let Some(escaped) = self.lex_escape_sequence() {
					string.push(escaped);
				} else {
					// Invalid escape sequence
					return TokenKind::Invalid;
				}
			} else {
				string.push(ch);
				self.advance();
			}
		}

		// Unterminated string
		return TokenKind::Invalid;
	}

	fn lex_char_or_label(&mut self) -> TokenKind
	{
		let pos_backup = self.position;
		let char_backup = self.current_char;
		let line_backup = self.line;
		let col_backup = self.column;

		self.advance(); // '

		match self.current_char {
			Some(ch) if ch.is_alphabetic() || ch == '_' => {
				let mut label_name = String::new();
				label_name.push(ch);
				self.advance();

				let is_multi_char = self
					.current_char
					.is_some_and(|c| return c.is_alphanumeric() || c == '_');

				if is_multi_char {
					while let Some(c) = self.current_char {
						if c.is_alphanumeric() || c == '_' {
							label_name.push(c);
							self.advance();
						} else {
							break;
						}
					}
					return TokenKind::Label(label_name);
				} else if self.current_char == Some('\'') {
					self.position = pos_backup;
					self.current_char = char_backup;
					self.line = line_backup;
					self.column = col_backup;
					return self.lex_char_literal();
				}
				return TokenKind::Label(label_name);
			}
			_ => {
				self.position = pos_backup;
				self.current_char = char_backup;
				self.line = line_backup;
				self.column = col_backup;
				return self.lex_char_literal();
			}
		}
	}

	fn lex_char_literal(&mut self) -> TokenKind
	{
		self.advance(); // opening '\''

		let ch = if self.current_char == Some('\\') {
			self.advance();
			self.lex_escape_sequence()
		} else {
			let ch = self.current_char;
			self.advance();
			ch
		};

		if self.current_char == Some('\'') {
			self.advance(); // closing '\''
			if let Some(c) = ch {
				return TokenKind::CharLiteral(c);
			}
			return TokenKind::Invalid;
		}
		return TokenKind::Invalid;
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

	fn read_int_suffix(&mut self) -> Option<IntType>
	{
		let checkpoint: LexerCheckpoint<'_, '_> = self.make_checkpoint();

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
				self.advance();

				let mut s: String = String::from("s");

				for _ in 0..3 {
					let c = self.current_char?;
					self.advance();
					s.push(c);
				}

				if s != "size" {
					return None;
				}

				return Some(IntType {
					bits: IntSize::Size,
					sign,
				});
			}

			let mut digits = String::new();

			while let Some(ch) = self.current_char {
				if ch.is_ascii_digit() {
					digits.push(ch);
					self.advance();
				} else {
					break;
				}
			}

			if digits.is_empty() {
				return None;
			}

			let bits: u16 = digits.parse::<u16>().ok()?;

			return Some(IntType {
				bits: IntSize::Fixed(bits),
				sign,
			});
		})();

		return result.map_or_else(
			|| {
				self.restore_checkpoint(checkpoint);
				return None;
			},
			|v| return Some(v),
		);
	}

	fn read_float_suffix(&mut self) -> Option<u16>
	{
		if self.current_char != Some('f') {
			return None;
		}

		self.advance();

		let mut digits: String = String::new();

		while let Some(ch) = self.current_char {
			if ch.is_ascii_digit() {
				digits.push(ch);
				self.advance();
			} else {
				break;
			}
		}

		return digits.parse::<u16>().ok();
	}

	fn read_radix_number<F>(&mut self, radix: u32, valid: F) -> TokenKind
	where
		F: Fn(char) -> bool,
	{
		let mut num_str = String::new();

		while let Some(ch) = self.current_char {
			if valid(ch) || ch == '_' {
				if ch != '_' {
					num_str.push(ch);
				}
				self.advance();
			} else {
				break;
			}
		}

		let base = match radix {
			2 => IntBase::Binary,
			8 => IntBase::Octal,
			16 => IntBase::Hexadecimal,
			_ => IntBase::Decimal,
		};

		return TokenKind::IntLiteral {
			value: num_str,
			base,
			ty: self.read_int_suffix(),
		};
	}

	fn lex_number(&mut self) -> TokenKind
	{
		let mut num_str = String::new();

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

		while let Some(ch) = self.current_char {
			if ch.is_ascii_digit() || ch == '_' {
				if ch != '_' {
					num_str.push(ch);
				}
				self.advance();
			} else {
				break;
			}
		}

		if self.current_char == Some('.') && self.peek().is_some_and(|c| return c.is_ascii_digit()) {
			num_str.push('.');
			self.advance(); // .

			while let Some(ch) = self.current_char {
				if ch.is_ascii_digit() || ch == '_' {
					if ch != '_' {
						num_str.push(ch);
					}
					self.advance();
				} else {
					break;
				}
			}

			return TokenKind::FloatLiteral {
				value: num_str,
				bits: self.read_float_suffix(),
			};
		}

		return TokenKind::IntLiteral {
			value: num_str,
			base: IntBase::Decimal,
			ty: self.read_int_suffix(),
		};
	}

	fn lex_identifier_or_keyword(&mut self) -> TokenKind
	{
		let mut ident = String::new();

		while let Some(ch) = self.current_char {
			if ch.is_alphanumeric() || ch == '_' || ch == '#' {
				ident.push(ch);
				self.advance();
			} else {
				break;
			}
		}

		if let Some(keyword) = Self::match_keyword(&ident) {
			return keyword;
		}

		if self.current_char == Some('"') {
			return self.lex_string_literal(Some(ident));
		}
		return TokenKind::Identifier(ident);
	}

	fn lex_directive(&mut self) -> TokenKind
	{
		self.advance(); // @
		let mut directive: String = String::new();

		while let Some(ch) = self.current_char {
			if ch.is_alphanumeric() || ch == '_' {
				directive.push(ch);
				self.advance();
			} else {
				break;
			}
		}

		let dir: Directive = match directive.as_str() {
			"use" => Directive::Use,
			"import" => Directive::Import,
			_ => Directive::Custom(directive),
		};

		return TokenKind::Directive(dir);
	}

	fn lex_macro(&mut self) -> TokenKind
	{
		self.advance(); // consume '$'
		let mut macro_name: String = String::new();

		while let Some(ch) = self.current_char {
			if ch.is_alphanumeric() || ch == '_' {
				macro_name.push(ch);
				self.advance();
			} else {
				break;
			}
		}

		return TokenKind::Macro(macro_name);
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

	fn peek(&self) -> Option<char>
	{
		if let Some(ch) = self.current_char {
			return self.source[self.position + ch.len_utf8()..].chars().next();
		}
		return None;
	}
}
