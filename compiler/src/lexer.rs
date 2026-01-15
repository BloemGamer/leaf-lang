mod tests;

use leaf_proc::generate_lexer;

use crate::Config;
use crate::source_map::{SourceIndex, SourceMap};

impl<'source, 'config> Lexer<'source, 'config>
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
	pub const fn into_parts(self) -> (&'config Config, SourceIndex, Lexer<'source, 'config>)
	{
		let config: &'config Config = self.config;
		let source_index: SourceIndex = self.source_index;
		return (config, source_index, self);
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
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct Span
{
	pub start: usize,
	pub end: usize,
	pub start_line: usize,
	pub start_col: usize,
	pub end_line: usize,
	pub end_col: usize,
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
	/// Creates an error from this span.
	///
	/// # Arguments
	/// * `source` - The source code string
	/// * `message` - The error message
	///
	/// # Returns
	/// An error of type `E` constructed from the span and formatted message
	pub fn make_error<E: ErrorFromSpan>(self, source: &str, message: &str) -> E
	{
		return E::from_span(self, self.format_error(source, message));
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
		return Self {
			start: self.start.min(other.start),
			end: self.end.max(other.end),
			start_line: self.start_line.min(other.start_line),
			start_col: self.start_col.min(other.start_col),
			end_line: self.end_line.max(other.end_line),
			end_col: self.end_col.max(other.end_col),
		};
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
#[derive(Debug, Clone, PartialEq)]
#[generate_lexer]
pub enum TokenKind
{
	// ===== Literals =====
	/// Integer literal: `42`, `-10`, `0xFF`
	IntLiteral(i64),
	/// Floating point literal: `3.14`, `-0.5`, `1e10`
	FloatLiteral(f64),
	/// Character literal: `'a'`, `'\n'`, `'\0'`
	CharLiteral(char),
	/// String literal: `"hello"`, `"world\n"`
	StringLiteral(String),
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
	/// Namespace declaration: `namespace`
	#[keyword("namespace")]
	Namespace,
	/// Type alias: `type`
	#[keyword("type")]
	Type,

	// ===== Keywords - Modifiers =====
	/// Public visibility: `pub`
	#[keyword("pub")]
	Pub,
	/// Mutable binding: `mut`
	#[keyword("mut")]
	Mut,
	#[allow(unused)]
	/// Mutable reference: `mut` or `&mut`
	MutRef,
	#[allow(unused)]
	/// Immutable reference: `ref`
	Ref,
	/// Unsafe block/function: `unsafe`
	#[keyword("unsafe")]
	Unsafe,
	/// Volatile memory access: `volatile`
	#[keyword("volatile")]
	Volatile,
	/// Inline function: `inline`
	#[keyword("inline")]
	Inline,

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
	/// Path separator/namespaces: `::`
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
	/// Optional/error propagation: `?`
	#[simple_token("?")]
	QuestionMark,
	/// Attribute marker: `#`
	#[simple_token("#")]
	Hash,
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

impl<'source, 'config> Iterator for Lexer<'source, 'config>
{
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item>
	{
		let mut token: Token = self.next_token();
		while matches!(token.kind, TokenKind::LineComment(_) | TokenKind::BlockComment(_)) {
			token = self.next_token();
		}

		return if matches!(token.kind, TokenKind::Eof | TokenKind::Invalid) {
			if self.eof_returned {
				None
			} else {
				self.eof_returned = true;
				Some(token)
			}
		} else {
			Some(token)
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
		file_name: impl Into<String>,
		source_map: &'source mut SourceMap,
	) -> Self
	{
		let source_index: SourceIndex = source_map.add_file(file_name, source);
		let new_source: &String = &source_map.get(source_index).src;
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
	pub fn next_token(&mut self) -> Token
	{
		self.skip_whitespace();

		let start: usize = self.position;
		let start_line: usize = self.line;
		let start_col: usize = self.column;

		let Some(ch) = self.current_char else {
			return Token {
				kind: TokenKind::Eof,
				span: Span {
					start,
					end: start,
					start_line,
					start_col,
					end_line: start_line,
					end_col: start_col,
				},
			};
		};

		let kind: TokenKind = match ch {
			// Special case: / needs comment handling
			'/' => self.lex_slash_or_comment(),

			// Literals
			'"' => self.lex_string_literal(),
			'\'' => self.lex_char_or_label(),
			'0'..='9' => self.lex_number(),

			// Identifiers and keywords
			'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier_or_keyword(),

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

		return Token {
			kind,
			span: Span {
				start,
				end,
				start_line,
				start_col,
				end_line,
				end_col,
			},
		};
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
		} else {
			return TokenKind::LineComment(comment);
		}
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
		} else {
			return TokenKind::BlockComment(comment);
		}
	}

	fn lex_string_literal(&mut self) -> TokenKind
	{
		self.advance(); // consume opening '"'
		let mut string = String::new();

		while let Some(ch) = self.current_char {
			if ch == '"' {
				self.advance(); // consume closing '"'
				return TokenKind::StringLiteral(string);
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
				} else {
					return TokenKind::Label(label_name);
				}
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
			} else {
				return TokenKind::Invalid;
			}
		} else {
			return TokenKind::Invalid;
		}
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
				} else {
					return None;
				}
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
				} else {
					return None;
				}
			}
			_ => return None,
		};

		if escaped != '\0' || self.current_char == Some('0') {
			self.advance();
		}

		return Some(escaped);
	}

	fn read_radix_number(&mut self, radix: u32, is_valid_digit: impl Fn(char) -> bool) -> TokenKind
	{
		let mut num_str: String = String::new();

		while let Some(ch) = self.current_char {
			if is_valid_digit(ch) || ch == '_' {
				if ch != '_' {
					num_str.push(ch);
				}
				self.advance();
			} else {
				break;
			}
		}

		return i64::from_str_radix(&num_str, radix)
			.map(TokenKind::IntLiteral)
			.unwrap_or(TokenKind::Invalid);
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

			if let Ok(val) = num_str.parse::<f64>() {
				return TokenKind::FloatLiteral(val);
			} else {
				return TokenKind::Invalid;
			}
		} else {
			match num_str.parse::<i64>() {
				Ok(val) => return TokenKind::IntLiteral(val),
				Err(_) => return TokenKind::Invalid,
			}
		}
	}

	fn lex_identifier_or_keyword(&mut self) -> TokenKind
	{
		let mut ident = String::new();

		while let Some(ch) = self.current_char {
			if ch.is_alphanumeric() || ch == '_' {
				ident.push(ch);
				self.advance();
			} else {
				break;
			}
		}

		if let Some(keyword) = Self::match_keyword(&ident) {
			return keyword;
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
		} else {
			return None;
		}
	}
}

impl Token
{
	/// Formats an error message with source code context.
	///
	/// Generates a human-readable error message that includes the line number,
	/// column number, the relevant line of source code, and a visual indicator
	/// (caret) pointing to the location of the error.
	///
	/// # Arguments
	/// * `source_index` - Index into the source map
	/// * `source_map` - Reference to the source map containing all source files
	/// * `message` - The error message to display
	///
	/// # Returns
	/// A formatted string containing the error location, message, source line,
	/// and visual indicator pointing to the error position.
	///
	/// # Example
	/// ```no_run
	/// # use crate::lexer::Token;
	/// # use crate::{SourceIndex, SourceMap};
	/// # let token = Token { kind: crate::lexer::TokenKind::Invalid, span: crate::lexer::Span::default() };
	/// # let source_index = SourceIndex(0);
	/// # let source_map = SourceMap::new();
	/// let error = token.format_error(source_index, &source_map, "unexpected token");
	/// println!("{}", error);
	/// // Output:
	/// // Error at 1:20: unexpected token
	/// //   | var x = Vec<Vec<int>;
	/// //   |                     ^
	/// ```
	#[allow(unused)]
	pub fn format_error(&self, source_index: SourceIndex, source_map: &SourceMap, message: &str) -> String
	{
		let source: &str = &source_map.get(source_index).src;
		let line_start = source[..self.span.start].rfind('\n').map(|i| return i + 1).unwrap_or(0);
		let line_end = source[self.span.start..]
			.find('\n')
			.map(|i| return self.span.start + i)
			.unwrap_or(source.len());
		let line_text = &source[line_start..line_end];

		// Preserve tabs and spaces to maintain alignment
		let prefix = &source[line_start..self.span.start];
		let caret_indent: String = prefix
			.chars()
			.map(|c| if c == '\t' { return '\t' } else { return ' ' })
			.collect();

		let caret_length = (self.span.end - self.span.start).max(1);

		return format!(
			"Error at {}:{}: {}\n  | {}\n  | {}{}",
			self.span.start_line,
			self.span.start_col,
			message,
			line_text,
			caret_indent,
			"^".repeat(caret_length)
		);
	}
}

impl Span
{
	/// Formats an error message with source code context.
	///
	/// Generates a human-readable error message that includes the line number,
	/// column number, the relevant line of source code, and a visual indicator
	/// (caret) pointing to the location of the error.
	///
	/// # Arguments
	/// * `source` - The complete source code string
	/// * `message` - The error message to display
	///
	/// # Returns
	/// A formatted string containing the error location, message, source line,
	/// and visual indicator pointing to the error position.
	///
	/// # Example
	/// ```no_run
	/// # use crate::lexer::Span;
	/// # let span = Span::default();
	/// # let source = "var x = 42;";
	/// let error = span.format_error(source, "unexpected token");
	/// println!("{}", error);
	/// // Output:
	/// // Error at 1:20: unexpected token
	/// //   | var x = Vec<Vec<int>;
	/// //   |                     ^
	/// ```
	#[allow(unused)]
	pub fn format_error(&self, source: &str, message: &str) -> String
	{
		let line_start = source[..self.start].rfind('\n').map(|i| return i + 1).unwrap_or(0);
		let line_end = source[self.start..]
			.find('\n')
			.map(|i| return self.start + i)
			.unwrap_or(source.len());
		let line_text = &source[line_start..line_end];

		// Preserve tabs and spaces to maintain alignment
		let prefix = &source[line_start..self.start];
		let caret_indent: String = prefix
			.chars()
			.map(|c| if c == '\t' { return '\t' } else { return ' ' })
			.collect();

		let caret_length = (self.end - self.start).max(1);

		return format!(
			"Error at {}:{}: {}\n  | {}\n  | {}{}",
			self.start_line,
			self.start_col,
			message,
			line_text,
			caret_indent,
			"^".repeat(caret_length)
		);
	}
}
