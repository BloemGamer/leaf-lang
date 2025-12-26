use crate::Config;

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
	/// * `Lexer<'source, 'config>` - The lexer itself, moved out and ready to be consumed
	///
	/// # Example
	/// ```
	/// let config = Config::default();
	/// let lexer = Lexer::new(&config, "fn main() {}");
	/// let (config_ref, lexer) = lexer.into_parts();
	/// // Now both config_ref and lexer can be used independently
	/// ```
	pub fn into_parts(self) -> (&'config Config, &'source str, Lexer<'source, 'config>)
	{
		let config = self.config;
		let source = self.source;
		(config, source, self)
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
/// ```
/// let config = Config::default();
/// let source = "fn main() { let x = 42; }";
/// let mut lexer = Lexer::new(&config, source);
///
/// while let Some(token) = lexer.next() {
///     println!("{:?}", token);
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Lexer<'source, 'config>
{
	source: &'source str,
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
/// ```
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
/// ```
/// let span = Span {
///     start: 0,
///     end: 5,
///     start_line: 1,
///     start_col: 1,
///     end_line: 1,
///     end_col: 6,
/// };
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Span
{
	pub start: usize,
	pub end: usize,
	pub start_line: usize,
	pub start_col: usize,
	pub end_line: usize,
	pub end_col: usize,
}

impl Span
{
	/// Merges to Spans together
	///
	/// # Arguments
	/// * `self` - Self
	/// * `other` - Other Span
	///
	/// # Returns
	/// A new Span ranging from the start of the first span, and the end of the second one
	///
	///
	/// # Example
	/// ```
	/// let new_span: Span = old_span1.merge(old_span2);
	/// ```
	pub fn merge(&self, other: &Span) -> Self
	{
		Self {
			start: self.start.min(other.start),
			end: self.end.max(other.end),
			start_line: self.start_line.min(other.start_line),
			start_col: self.start_col.min(other.start_col),
			end_line: self.end_line.max(other.end_line),
			end_col: self.end_col.max(other.end_col),
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
///
/// # Example
/// ```
/// let kind = TokenKind::IntLiteral(42);
/// let kind = TokenKind::Identifier("my_var".to_string());
/// let kind = TokenKind::Plus;
/// ```
#[derive(Debug, Clone, PartialEq)]
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
	True,
	/// Boolean literal: `false`
	False,

	// ===== Identifiers =====
	/// Variable/function names: `foo`, `bar`, `my_var`
	Identifier(String),
	/// Wildcard pattern: `_`
	Underscore,
	/// Self keyword: `self`
	SelfKw,

	// ===== Keywords - Control Flow =====
	/// Conditional: `if`
	If,
	/// Conditional alternative: `else`
	Else,
	/// Loop: `while`
	While,
	/// Iterator loop: `for`
	For,
	/// Pattern matching: `match`
	Match,
	/// Return from function: `return`
	Return,
	/// Exit loop: `break`
	Break,
	/// Skip to next iteration: `continue`
	Continue,
	/// Call the constructor for a type: `new`
	New,
	/// Call the destructor for a type: `delete`
	Delete,

	// ===== Keywords - Declarations =====
	/// Function definition: `fn`
	FuncDef,
	/// Constant declaration: `const`
	Const,
	/// Variable declaration: `let`
	Let,
	/// Static variable: `static`
	Static,
	/// Structure definition: `struct`
	Struct,
	/// Untagged union: `union`
	Union,
	/// Tagged union: `tagged_union`
	TaggedUnion,
	/// Enumeration definition: `enum`
	Enum,
	/// Implementation block: `impl`
	Impl,
	/// Macro definition: `macro`
	MacroDef,
	/// Namespace declaration: `namespace`
	Namespace,

	// ===== Keywords - Modifiers =====
	/// Public visibility: `pub`
	Pub,
	/// Mutable binding: `mut`
	Mut,
	#[allow(unused)]
	/// Mutable reference: `mut` or `&mut`
	MutRef,
	#[allow(unused)]
	/// Immutable reference: `ref`
	Ref,
	/// Unsafe block/function: `unsafe`
	Unsafe,
	/// Volatile memory access: `volatile`
	Volatile,
	/// Inline function: `inline`
	Inline,

	// ===== Keywords - Other =====
	/// Iterator source: `in` (for x in iter)
	In,
	/// Type casting: `as`
	As,
	/// Generic constraints: `where`
	Where,

	// ===== Arithmetic Operators =====
	/// Addition: `+`
	Plus,
	/// Subtraction or negation: `-`
	Minus,
	/// Multiplication or dereference: `*`
	Star,
	/// Division: `/`
	Slash,
	/// Modulo/remainder: `%`
	Mod,

	// ===== Bitwise Operators =====
	/// Bitwise OR: `|`
	Pipe,
	/// Bitwise AND or reference: `&`
	Ampersand,
	/// Bitwise XOR: `^`
	Caret,
	/// Bitwise NOT: `~`
	Tilde,
	/// Left shift: `<<`
	LShift,
	/// Right shift: `>>`
	RShift,

	// ===== Logical Operators =====
	/// Logical NOT: `!`
	Bang,
	/// Logical AND: `&&`
	And,
	/// Logical OR: `||`
	Or,

	// ===== Comparison Operators =====
	/// Less than: `<`
	LessThan,
	/// Greater than: `>`
	GreaterThan,
	/// Less than or equal: `<=`
	LessEquals,
	/// Greater than or equal: `>=`
	GreaterEquals,
	/// Equality: `==`
	EqualsEquals,
	/// Inequality: `!=`
	BangEquals,

	// ===== Assignment Operators =====
	/// Assignment: `=`
	Equals,
	/// Add and assign: `+=`
	PlusEquals,
	/// Subtract and assign: `-=`
	MinusEquals,
	/// Multiply and assign: `*=`
	StarEquals,
	/// Divide and assign: `/=`
	SlashEquals,
	/// Modulo and assign: `%=`
	ModEquals,
	/// Bitwise OR and assign: `|=`
	PipeEquals,
	/// Bitwise AND and assign: `&=`
	AmpersandEquals,
	/// Bitwise XOR and assign: `^=`
	CaretEquals,
	/// Bitwise NOT and assign: `~=`
	TildeEquals,
	/// Left shift and assign: `<<=`
	LShiftEquals,
	/// Right shift and assign: `>>=`
	RShiftEquals,

	// ===== Delimiters =====
	/// Opening parenthesis: `(`
	LeftParen,
	/// Closing parenthesis: `)`
	RightParen,
	/// Opening brace: `{`
	LeftBrace,
	/// Closing brace: `}`
	RightBrace,
	/// Opening bracket: `[`
	LeftBracket,
	/// Closing bracket: `]`
	RightBracket,

	// ===== Punctuation =====
	/// Statement terminator: `;`
	Semicolon,
	/// To be determined: `:`
	Colon,
	/// Path separator/namespaces: `::`
	DoubleColon,
	/// List separator: `,`
	Comma,
	/// Member access: `.`
	Dot,
	/// Range: `..`
	DotDot,
	/// Range: `..=`
	DotDotEquals,
	/// Variadic: `...`
	Ellipsis,
	/// Function return type: `->`
	Arrow,
	/// Match arm: `=>`
	FatArrow,
	/// Optional/error propagation: `?`
	QuestionMark,
	/// To be determined: `#`
	Hash,
	/// Escape character: `\`
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
///
/// # Example
/// ```
/// let dir = Directive::Use;          // @use
/// let dir = Directive::Import;       // @import
/// let dir = Directive::Custom("cfg".to_string()); // @cfg
/// ```
#[derive(Debug, Clone, PartialEq)]
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
		let token = self.next_token();

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
	///
	/// # Returns
	/// A new `Lexer` instance initialized at position 0, line 1, column 1
	///
	/// # Example
	/// ```
	/// let config = Config::default();
	/// let source = "let x = 42;";
	/// let lexer = Lexer::new(&config, source);
	/// ```
	#[allow(unused)]
	pub fn new(config: &'config Config, source: &'source str) -> Self
	{
		let mut lexer = Lexer {
			source,
			config,
			position: 0,
			current_char: None,
			line: 1,
			column: 1,
			eof_returned: false,
		};
		lexer.current_char = lexer.source.chars().next();
		lexer
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
	/// ```
	/// let mut lexer = Lexer::new(&config, "x + 42");
	/// let token = lexer.next_token(); // Returns Identifier("x")
	/// let token = lexer.next_token(); // Returns Plus
	/// let token = lexer.next_token(); // Returns IntLiteral(42)
	/// ```
	#[allow(unused)]
	pub fn next_token(&mut self) -> Token
	{
		self.skip_whitespace();

		let start = self.position;
		let start_line = self.line;
		let start_col = self.column;

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

		let kind = match ch {
			// Operators that might be multi-character
			'+' => self.lex_plus_family(),
			'-' => self.lex_minus_family(),
			'*' => self.lex_star_family(),
			'/' => self.lex_slash_family(),
			'%' => self.lex_mod_family(),
			'<' => self.lex_less_family(),
			'>' => self.lex_greater_family(),
			'=' => self.lex_equals_family(),
			'&' => self.lex_ampersand_family(),
			'|' => self.lex_pipe_family(),
			'^' => self.lex_caret_family(),
			'~' => self.lex_tilde_family(),
			'!' => self.lex_bang_family(),
			'.' => self.lex_dot_family(),
			':' => self.lex_colon_family(),

			// Literals
			'"' => self.lex_string_literal(),
			'\'' => self.lex_char_literal(),
			'0'..='9' => self.lex_number(),

			// Identifiers and keywords
			'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier_or_keyword(),

			// Directives
			'@' => self.lex_directive(),

			// Macros
			'$' => self.lex_macro(),

			// Simple single-character tokens
			'(' => {
				self.advance();
				TokenKind::LeftParen
			}
			')' => {
				self.advance();
				TokenKind::RightParen
			}
			'{' => {
				self.advance();
				TokenKind::LeftBrace
			}
			'}' => {
				self.advance();
				TokenKind::RightBrace
			}
			'[' => {
				self.advance();
				TokenKind::LeftBracket
			}
			']' => {
				self.advance();
				TokenKind::RightBracket
			}
			';' => {
				self.advance();
				TokenKind::Semicolon
			}
			',' => {
				self.advance();
				TokenKind::Comma
			}
			'?' => {
				self.advance();
				TokenKind::QuestionMark
			}
			'#' => {
				self.advance();
				TokenKind::Hash
			}
			'\\' => {
				self.advance();
				TokenKind::Backslash
			}

			_ => {
				self.advance();
				TokenKind::Invalid
			}
		};

		let end = self.position;
		let end_line = self.line;
		let end_col = self.column;

		Token {
			kind,
			span: Span {
				start,
				end,
				start_line,
				start_col,
				end_line,
				end_col,
			},
		}
	}

	fn lex_plus_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '+'
		if self.current_char == Some('=') {
			self.advance();
			TokenKind::PlusEquals
		} else {
			TokenKind::Plus
		}
	}

	fn lex_minus_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '-'
		match self.current_char {
			Some('=') => {
				self.advance();
				TokenKind::MinusEquals
			}
			Some('>') => {
				self.advance();
				TokenKind::Arrow
			}
			_ => TokenKind::Minus,
		}
	}

	fn lex_star_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '*'
		if self.current_char == Some('=') {
			self.advance();
			TokenKind::StarEquals
		} else {
			TokenKind::Star
		}
	}

	fn lex_slash_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '/'
		match self.current_char {
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
		}
	}

	fn lex_mod_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '%'
		if self.current_char == Some('=') {
			self.advance();
			TokenKind::ModEquals
		} else {
			TokenKind::Mod
		}
	}

	fn lex_less_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '<'
		match self.current_char {
			Some('<') => {
				self.advance();
				if self.current_char == Some('=') {
					self.advance();
					TokenKind::LShiftEquals
				} else {
					TokenKind::LShift
				}
			}
			Some('=') => {
				self.advance();
				TokenKind::LessEquals
			}
			_ => TokenKind::LessThan,
		}
	}

	fn lex_greater_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '>'
		match self.current_char {
			Some('>') => {
				self.advance();
				if self.current_char == Some('=') {
					self.advance();
					TokenKind::RShiftEquals
				} else {
					TokenKind::RShift
				}
			}
			Some('=') => {
				self.advance();
				TokenKind::GreaterEquals
			}
			_ => TokenKind::GreaterThan,
		}
	}

	fn lex_equals_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '='
		match self.current_char {
			Some('=') => {
				self.advance();
				TokenKind::EqualsEquals
			}
			Some('>') => {
				self.advance();
				TokenKind::FatArrow
			}
			_ => TokenKind::Equals,
		}
	}

	fn lex_ampersand_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '&'
		match self.current_char {
			Some('&') => {
				self.advance();
				TokenKind::And
			}
			Some('=') => {
				self.advance();
				TokenKind::AmpersandEquals
			}
			_ => TokenKind::Ampersand,
		}
	}

	fn lex_pipe_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '|'
		match self.current_char {
			Some('|') => {
				self.advance();
				TokenKind::Or
			}
			Some('=') => {
				self.advance();
				TokenKind::PipeEquals
			}
			_ => TokenKind::Pipe,
		}
	}

	fn lex_caret_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '^'
		if self.current_char == Some('=') {
			self.advance();
			TokenKind::CaretEquals
		} else {
			TokenKind::Caret
		}
	}

	fn lex_tilde_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '~'
		if self.current_char == Some('=') {
			self.advance();
			TokenKind::TildeEquals
		} else {
			TokenKind::Tilde
		}
	}

	fn lex_bang_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '!'
		if self.current_char == Some('=') {
			self.advance();
			TokenKind::BangEquals
		} else {
			TokenKind::Bang
		}
	}

	fn lex_dot_family(&mut self) -> TokenKind
	{
		self.advance(); // consume '.'
		match self.current_char {
			Some('.') => {
				self.advance();
				match self.current_char {
					Some('.') => TokenKind::Ellipsis,
					Some('=') => TokenKind::DotDotEquals,
					_ => TokenKind::DotDot,
				}
			}
			_ => TokenKind::Dot,
		}
	}

	fn lex_colon_family(&mut self) -> TokenKind
	{
		self.advance(); // consume ':'
		if self.current_char == Some(':') {
			self.advance();
			TokenKind::DoubleColon
		} else {
			TokenKind::Colon
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
				}
			} else {
				string.push(ch);
				self.advance();
			}
		}

		// Unterminated string
		TokenKind::Invalid
	}

	fn lex_char_literal(&mut self) -> TokenKind
	{
		self.advance(); // consume opening '\''

		let ch = if self.current_char == Some('\\') {
			self.advance();
			self.lex_escape_sequence()
		} else {
			let ch = self.current_char;
			self.advance();
			ch
		};

		if self.current_char == Some('\'') {
			self.advance(); // consume closing '\''
			if let Some(c) = ch {
				TokenKind::CharLiteral(c)
			} else {
				TokenKind::Invalid
			}
		} else {
			TokenKind::Invalid
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
			_ => return None,
		};
		self.advance();
		Some(escaped)
	}

	fn read_radix_number(&mut self, radix: u32, is_valid_digit: impl Fn(char) -> bool) -> TokenKind
	{
		let mut num_str = String::new();

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

		i64::from_str_radix(&num_str, radix)
			.map(TokenKind::IntLiteral)
			.unwrap_or(TokenKind::Invalid)
	}

	fn lex_number(&mut self) -> TokenKind
	{
		let mut num_str = String::new();

		if self.current_char == Some('0') {
			match self.peek() {
				Some('x') => {
					self.advance(); // '0'
					self.advance(); // 'x'
					return self.read_radix_number(16, |c| c.is_ascii_hexdigit());
				}
				Some('b') => {
					self.advance(); // '0'
					self.advance(); // 'b'
					return self.read_radix_number(2, |c| c == '0' || c == '1');
				}
				Some('o') => {
					self.advance(); // '0'
					self.advance(); // 'o'
					return self.read_radix_number(8, |c| ('0'..='7').contains(&c));
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

		if self.current_char == Some('.') && self.peek().is_some_and(|c| c.is_ascii_digit()) {
			num_str.push('.');
			self.advance(); // consume '.'

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
				TokenKind::FloatLiteral(val)
			} else {
				TokenKind::Invalid
			}
		} else {
			match num_str.parse::<i64>() {
				Ok(val) => TokenKind::IntLiteral(val),
				Err(_) => TokenKind::Invalid,
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

		match ident.as_str() {
			"_" => TokenKind::Underscore,
			"self" => TokenKind::SelfKw,
			"if" => TokenKind::If,
			"else" => TokenKind::Else,
			"while" => TokenKind::While,
			"for" => TokenKind::For,
			"match" => TokenKind::Match,
			"return" => TokenKind::Return,
			"break" => TokenKind::Break,
			"continue" => TokenKind::Continue,
			"namespace" => TokenKind::Namespace,
			"fn" => TokenKind::FuncDef,
			"const" => TokenKind::Const,
			"let" => TokenKind::Let,
			"static" => TokenKind::Static,
			"struct" => TokenKind::Struct,
			"union" => TokenKind::Union,
			"tagged_union" => TokenKind::TaggedUnion,
			"enum" => TokenKind::Enum,
			"impl" => TokenKind::Impl,
			"macro" => TokenKind::MacroDef,
			"pub" => TokenKind::Pub,
			"inline" => TokenKind::Inline,
			"mut" => TokenKind::Mut,
			"unsafe" => TokenKind::Unsafe,
			"volatile" => TokenKind::Volatile,
			"in" => TokenKind::In,
			"as" => TokenKind::As,
			"where" => TokenKind::Where,
			"true" => TokenKind::True,
			"false" => TokenKind::False,
			"new" => TokenKind::New,
			"delete" => TokenKind::Delete,
			_ => TokenKind::Identifier(ident),
		}
	}

	fn lex_directive(&mut self) -> TokenKind
	{
		self.advance(); // consume '@'
		let mut directive = String::new();

		while let Some(ch) = self.current_char {
			if ch.is_alphanumeric() || ch == '_' {
				directive.push(ch);
				self.advance();
			} else {
				break;
			}
		}

		let dir = match directive.as_str() {
			"use" => Directive::Use,
			"import" => Directive::Import,
			_ => Directive::Custom(directive),
		};

		TokenKind::Directive(dir)
	}

	fn lex_macro(&mut self) -> TokenKind
	{
		self.advance(); // consume '$'
		let mut macro_name = String::new();

		while let Some(ch) = self.current_char {
			if ch.is_alphanumeric() || ch == '_' {
				macro_name.push(ch);
				self.advance();
			} else {
				break;
			}
		}

		TokenKind::Macro(macro_name)
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
			TokenKind::DocsComment(comment)
		} else {
			TokenKind::LineComment(comment)
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
			TokenKind::DocsComment(comment)
		} else {
			TokenKind::BlockComment(comment)
		}
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
			self.source[self.position + ch.len_utf8()..].chars().next()
		} else {
			None
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
	/// * `source` - The complete source code string
	/// * `message` - The error message to display
	///
	/// # Returns
	/// A formatted string containing the error location, message, source line,
	/// and visual indicator pointing to the error position.
	///
	/// # Example
	/// ```
	/// let token = Token { /* ... */ };
	/// let error = token.format_error(source, "unexpected token");
	/// println!("{}", error);
	/// // Output:
	/// // Error at 1:20: unexpected token
	/// //   | let x = Vec<Vec<int>>;
	/// //   |                    ^^
	/// ```
	#[allow(unused)]
	pub fn format_error(&self, source: &str, message: &str) -> String
	{
		let line_start = source[..self.span.start].rfind('\n').map(|i| i + 1).unwrap_or(0);
		let line_end = source[self.span.start..]
			.find('\n')
			.map(|i| self.span.start + i)
			.unwrap_or(source.len());
		let line_text = &source[line_start..line_end];

		let caret_offset = self.span.start_col - 1;
		let caret_length = (self.span.end - self.span.start).max(1);

		format!(
			"Error at {}:{}: {}\n  | {}\n  | {}{}",
			self.span.start_line,
			self.span.start_col,
			message,
			line_text,
			" ".repeat(caret_offset),
			"^".repeat(caret_length)
		)
	}
}

#[cfg(test)]
mod tests
{
	use super::*;

	/// Helper function to extract token kinds from source
	fn lex_kinds(source: &str) -> Vec<TokenKind>
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, source);
		lexer.map(|t| t.kind).collect()
	}

	/// Helper function to extract tokens from source
	fn lex_tokens(source: &str) -> Vec<Token>
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, source);
		lexer.collect()
	}

	// ===== Basic Token Tests =====

	#[test]
	fn test_basic_tokens()
	{
		let source = "let x = 42 + 3.14;";
		let config = Config::default();
		let lexer = Lexer::new(&config, source);

		let tokens: Vec<Token> = lexer.into_iter().collect();

		assert_eq!(tokens.len(), 8);
	}

	#[test]
	fn test_empty_source()
	{
		let kinds = lex_kinds("");
		assert_eq!(kinds, vec![TokenKind::Eof]);
	}

	#[test]
	fn test_whitespace_only()
	{
		let kinds = lex_kinds("   \n\t  \r\n  ");
		assert_eq!(kinds, vec![TokenKind::Eof]);
	}

	// ===== Literal Tests =====

	#[test]
	fn test_integer_literals()
	{
		let kinds = lex_kinds("0 42 123 1_000_000");
		assert_eq!(
			kinds,
			vec![
				TokenKind::IntLiteral(0),
				TokenKind::IntLiteral(42),
				TokenKind::IntLiteral(123),
				TokenKind::IntLiteral(1000000),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_hex_literals()
	{
		let kinds = lex_kinds("0x0 0xFF 0xDEADBEEF 0x1_2_3");
		assert_eq!(
			kinds,
			vec![
				TokenKind::IntLiteral(0),
				TokenKind::IntLiteral(255),
				TokenKind::IntLiteral(0xDEADBEEF),
				TokenKind::IntLiteral(0x123),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_binary_literals()
	{
		let kinds = lex_kinds("0b0 0b1010 0b1111_0000");
		assert_eq!(
			kinds,
			vec![
				TokenKind::IntLiteral(0),
				TokenKind::IntLiteral(0b1010),
				TokenKind::IntLiteral(0b11110000),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_octal_literals()
	{
		let kinds = lex_kinds("0o0 0o777 0o1_2_3");
		assert_eq!(
			kinds,
			vec![
				TokenKind::IntLiteral(0),
				TokenKind::IntLiteral(0o777),
				TokenKind::IntLiteral(0o123),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_float_literals()
	{
		let kinds = lex_kinds("3.12 0.5 123.456 1_000.5_0");
		assert_eq!(
			kinds,
			vec![
				TokenKind::FloatLiteral(3.12),
				TokenKind::FloatLiteral(0.5),
				TokenKind::FloatLiteral(123.456),
				TokenKind::FloatLiteral(1000.50),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_string_literals()
	{
		let kinds = lex_kinds(r#""hello" "world" "" "with spaces""#);
		assert_eq!(
			kinds,
			vec![
				TokenKind::StringLiteral("hello".to_string()),
				TokenKind::StringLiteral("world".to_string()),
				TokenKind::StringLiteral("".to_string()),
				TokenKind::StringLiteral("with spaces".to_string()),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_string_escape_sequences()
	{
		let kinds = lex_kinds(r#""hello\nworld" "tab\there" "quote\"" "backslash\\""#);
		assert_eq!(
			kinds,
			vec![
				TokenKind::StringLiteral("hello\nworld".to_string()),
				TokenKind::StringLiteral("tab\there".to_string()),
				TokenKind::StringLiteral("quote\"".to_string()),
				TokenKind::StringLiteral("backslash\\".to_string()),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_char_literals()
	{
		let kinds = lex_kinds(r"'a' 'Z' '0' ' '");
		assert_eq!(
			kinds,
			vec![
				TokenKind::CharLiteral('a'),
				TokenKind::CharLiteral('Z'),
				TokenKind::CharLiteral('0'),
				TokenKind::CharLiteral(' '),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_char_escape_sequences()
	{
		let kinds = lex_kinds(r"'\n' '\t' '\r' '\0' '\\' '\''");
		assert_eq!(
			kinds,
			vec![
				TokenKind::CharLiteral('\n'),
				TokenKind::CharLiteral('\t'),
				TokenKind::CharLiteral('\r'),
				TokenKind::CharLiteral('\0'),
				TokenKind::CharLiteral('\\'),
				TokenKind::CharLiteral('\''),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_boolean_literals()
	{
		let kinds = lex_kinds("true false");
		assert_eq!(kinds, vec![TokenKind::True, TokenKind::False, TokenKind::Eof]);
	}

	// ===== Identifier and Keyword Tests =====

	#[test]
	fn test_identifiers()
	{
		let kinds = lex_kinds("foo bar my_var _private MyType");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Identifier("foo".to_string()),
				TokenKind::Identifier("bar".to_string()),
				TokenKind::Identifier("my_var".to_string()),
				TokenKind::Identifier("_private".to_string()),
				TokenKind::Identifier("MyType".to_string()),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_underscore_wildcard()
	{
		let kinds = lex_kinds("_");
		assert_eq!(kinds, vec![TokenKind::Underscore, TokenKind::Eof]);
	}

	#[test]
	fn test_control_flow_keywords()
	{
		let kinds = lex_kinds("if else while for match return break continue");
		assert_eq!(
			kinds,
			vec![
				TokenKind::If,
				TokenKind::Else,
				TokenKind::While,
				TokenKind::For,
				TokenKind::Match,
				TokenKind::Return,
				TokenKind::Break,
				TokenKind::Continue,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_declaration_keywords()
	{
		let kinds = lex_kinds("fn const static struct union tagged_union enum impl macro");
		assert_eq!(
			kinds,
			vec![
				TokenKind::FuncDef,
				TokenKind::Const,
				TokenKind::Static,
				TokenKind::Struct,
				TokenKind::Union,
				TokenKind::TaggedUnion,
				TokenKind::Enum,
				TokenKind::Impl,
				TokenKind::MacroDef,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_modifier_keywords()
	{
		let kinds = lex_kinds("pub mut unsafe volatile");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Pub,
				TokenKind::Mut,
				TokenKind::Unsafe,
				TokenKind::Volatile,
				TokenKind::Eof
			]
		);
	}

	#[test]
	fn test_other_keywords()
	{
		let kinds = lex_kinds("in as where");
		assert_eq!(
			kinds,
			vec![TokenKind::In, TokenKind::As, TokenKind::Where, TokenKind::Eof]
		);
	}

	// ===== Operator Tests =====

	#[test]
	fn test_arithmetic_operators()
	{
		let kinds = lex_kinds("+ - * / %");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Plus,
				TokenKind::Minus,
				TokenKind::Star,
				TokenKind::Slash,
				TokenKind::Mod,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_bitwise_operators()
	{
		let kinds = lex_kinds("| & ^ ~ << >>");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Pipe,
				TokenKind::Ampersand,
				TokenKind::Caret,
				TokenKind::Tilde,
				TokenKind::LShift,
				TokenKind::RShift,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_logical_operators()
	{
		let kinds = lex_kinds("! && ||");
		assert_eq!(
			kinds,
			vec![TokenKind::Bang, TokenKind::And, TokenKind::Or, TokenKind::Eof]
		);
	}

	#[test]
	fn test_comparison_operators()
	{
		let kinds = lex_kinds("< > <= >= == !=");
		assert_eq!(
			kinds,
			vec![
				TokenKind::LessThan,
				TokenKind::GreaterThan,
				TokenKind::LessEquals,
				TokenKind::GreaterEquals,
				TokenKind::EqualsEquals,
				TokenKind::BangEquals,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_assignment_operators()
	{
		let kinds = lex_kinds("= += -= *= /= %= |= &= ^= ~= <<= >>=");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Equals,
				TokenKind::PlusEquals,
				TokenKind::MinusEquals,
				TokenKind::StarEquals,
				TokenKind::SlashEquals,
				TokenKind::ModEquals,
				TokenKind::PipeEquals,
				TokenKind::AmpersandEquals,
				TokenKind::CaretEquals,
				TokenKind::TildeEquals,
				TokenKind::LShiftEquals,
				TokenKind::RShiftEquals,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_arrow_operators()
	{
		let kinds = lex_kinds("-> =>");
		assert_eq!(kinds, vec![TokenKind::Arrow, TokenKind::FatArrow, TokenKind::Eof]);
	}

	// ===== Delimiter and Punctuation Tests =====

	#[test]
	fn test_delimiters()
	{
		let kinds = lex_kinds("( ) { } [ ]");
		assert_eq!(
			kinds,
			vec![
				TokenKind::LeftParen,
				TokenKind::RightParen,
				TokenKind::LeftBrace,
				TokenKind::RightBrace,
				TokenKind::LeftBracket,
				TokenKind::RightBracket,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_punctuation()
	{
		let kinds = lex_kinds("; : :: , . .. ... ? # \\");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Semicolon,
				TokenKind::Colon,
				TokenKind::DoubleColon,
				TokenKind::Comma,
				TokenKind::Dot,
				TokenKind::DotDot,
				TokenKind::Ellipsis,
				TokenKind::QuestionMark,
				TokenKind::Hash,
				TokenKind::Backslash,
				TokenKind::Eof,
			]
		);
	}

	// ===== Comment Tests =====

	#[test]
	fn test_line_comment()
	{
		let kinds = lex_kinds("// this is a comment\n42");
		assert_eq!(
			kinds,
			vec![
				TokenKind::LineComment(" this is a comment".to_string()),
				TokenKind::IntLiteral(42),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_doc_comment()
	{
		let kinds = lex_kinds("/// this is a doc comment\n42");
		assert_eq!(
			kinds,
			vec![
				TokenKind::DocsComment(" this is a doc comment".to_string()),
				TokenKind::IntLiteral(42),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_block_comment()
	{
		let kinds = lex_kinds("/* block comment */ 42");
		assert_eq!(
			kinds,
			vec![
				TokenKind::BlockComment(" block comment ".to_string()),
				TokenKind::IntLiteral(42),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_block_doc_comment()
	{
		let kinds = lex_kinds("/** doc block comment */ 42");
		assert_eq!(
			kinds,
			vec![
				TokenKind::DocsComment(" doc block comment ".to_string()),
				TokenKind::IntLiteral(42),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_multiline_block_comment()
	{
		let kinds = lex_kinds("/* line1\nline2\nline3 */ 42");
		assert_eq!(
			kinds,
			vec![
				TokenKind::BlockComment(" line1\nline2\nline3 ".to_string()),
				TokenKind::IntLiteral(42),
				TokenKind::Eof,
			]
		);
	}

	// ===== Special Token Tests =====

	#[test]
	fn test_macros()
	{
		let kinds = lex_kinds("$foo $bar_baz $MyMacro");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Macro("foo".to_string()),
				TokenKind::Macro("bar_baz".to_string()),
				TokenKind::Macro("MyMacro".to_string()),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_directives()
	{
		let kinds = lex_kinds("@use @import @custom");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Directive(Directive::Use),
				TokenKind::Directive(Directive::Import),
				TokenKind::Directive(Directive::Custom("custom".to_string())),
				TokenKind::Eof,
			]
		);
	}

	// ===== Error Cases =====

	#[test]
	fn test_unterminated_string()
	{
		let kinds = lex_kinds(r#""unterminated"#);
		assert_eq!(kinds, vec![TokenKind::Invalid]);
	}

	#[test]
	fn test_unterminated_char()
	{
		let kinds = lex_kinds("'a");
		assert_eq!(kinds, vec![TokenKind::Invalid]);
	}

	#[test]
	fn test_invalid_char()
	{
		let kinds = lex_kinds("''");
		assert_eq!(kinds, vec![TokenKind::Invalid]);
	}

	#[test]
	fn test_invalid_characters()
	{
		let kinds = lex_kinds("@ $ `");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Directive(Directive::Custom("".to_string())),
				TokenKind::Macro("".to_string()),
				TokenKind::Invalid,
			]
		);
	}

	// ===== Position Tracking Tests =====

	#[test]
	fn test_single_line_positions()
	{
		let tokens = lex_tokens("x + 42");

		assert_eq!(tokens[0].span.start_line, 1);
		assert_eq!(tokens[0].span.start_col, 1);
		assert_eq!(tokens[0].span.end_col, 2);

		assert_eq!(tokens[1].span.start_line, 1);
		assert_eq!(tokens[1].span.start_col, 3);
		assert_eq!(tokens[1].span.end_col, 4);

		assert_eq!(tokens[2].span.start_line, 1);
		assert_eq!(tokens[2].span.start_col, 5);
		assert_eq!(tokens[2].span.end_col, 7);
	}

	#[test]
	fn test_multiline_positions()
	{
		let tokens = lex_tokens("x\n+\n42");

		assert_eq!(tokens[0].span.start_line, 1);
		assert_eq!(tokens[1].span.start_line, 2);
		assert_eq!(tokens[2].span.start_line, 3);
	}

	#[test]
	fn test_byte_offsets()
	{
		let source = "x + 42";
		let tokens = lex_tokens(source);

		assert_eq!(&source[tokens[0].span.start..tokens[0].span.end], "x");
		assert_eq!(&source[tokens[1].span.start..tokens[1].span.end], "+");
		assert_eq!(&source[tokens[2].span.start..tokens[2].span.end], "42");
	}

	// ===== Integration Tests =====

	#[test]
	fn test_function_declaration()
	{
		let kinds = lex_kinds("fn add(x: i32, y: i32) -> i32 { x + y }");
		assert_eq!(
			kinds,
			vec![
				TokenKind::FuncDef,
				TokenKind::Identifier("add".to_string()),
				TokenKind::LeftParen,
				TokenKind::Identifier("x".to_string()),
				TokenKind::Colon,
				TokenKind::Identifier("i32".to_string()),
				TokenKind::Comma,
				TokenKind::Identifier("y".to_string()),
				TokenKind::Colon,
				TokenKind::Identifier("i32".to_string()),
				TokenKind::RightParen,
				TokenKind::Arrow,
				TokenKind::Identifier("i32".to_string()),
				TokenKind::LeftBrace,
				TokenKind::Identifier("x".to_string()),
				TokenKind::Plus,
				TokenKind::Identifier("y".to_string()),
				TokenKind::RightBrace,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_struct_definition()
	{
		let kinds = lex_kinds("struct Point { x: i32, y: i32 }");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Struct,
				TokenKind::Identifier("Point".to_string()),
				TokenKind::LeftBrace,
				TokenKind::Identifier("x".to_string()),
				TokenKind::Colon,
				TokenKind::Identifier("i32".to_string()),
				TokenKind::Comma,
				TokenKind::Identifier("y".to_string()),
				TokenKind::Colon,
				TokenKind::Identifier("i32".to_string()),
				TokenKind::RightBrace,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_if_statement()
	{
		let kinds = lex_kinds("if x > 0 { x } else { -x }");
		assert_eq!(
			kinds,
			vec![
				TokenKind::If,
				TokenKind::Identifier("x".to_string()),
				TokenKind::GreaterThan,
				TokenKind::IntLiteral(0),
				TokenKind::LeftBrace,
				TokenKind::Identifier("x".to_string()),
				TokenKind::RightBrace,
				TokenKind::Else,
				TokenKind::LeftBrace,
				TokenKind::Minus,
				TokenKind::Identifier("x".to_string()),
				TokenKind::RightBrace,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_for_loop()
	{
		let kinds = lex_kinds("for i in 0..10 { }");
		assert_eq!(
			kinds,
			vec![
				TokenKind::For,
				TokenKind::Identifier("i".to_string()),
				TokenKind::In,
				TokenKind::IntLiteral(0),
				TokenKind::DotDot,
				TokenKind::IntLiteral(10),
				TokenKind::LeftBrace,
				TokenKind::RightBrace,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_match_expression()
	{
		let kinds = lex_kinds("match x { 0 => true, _ => false }");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Match,
				TokenKind::Identifier("x".to_string()),
				TokenKind::LeftBrace,
				TokenKind::IntLiteral(0),
				TokenKind::FatArrow,
				TokenKind::True,
				TokenKind::Comma,
				TokenKind::Underscore,
				TokenKind::FatArrow,
				TokenKind::False,
				TokenKind::RightBrace,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_complex_expression()
	{
		let kinds = lex_kinds("(x + y) * z / (a - b)");
		assert_eq!(
			kinds,
			vec![
				TokenKind::LeftParen,
				TokenKind::Identifier("x".to_string()),
				TokenKind::Plus,
				TokenKind::Identifier("y".to_string()),
				TokenKind::RightParen,
				TokenKind::Star,
				TokenKind::Identifier("z".to_string()),
				TokenKind::Slash,
				TokenKind::LeftParen,
				TokenKind::Identifier("a".to_string()),
				TokenKind::Minus,
				TokenKind::Identifier("b".to_string()),
				TokenKind::RightParen,
				TokenKind::Eof,
			]
		);
	}

	// ===== Error Formatting Test =====

	#[test]
	fn test_format_error()
	{
		let source = "let x = 42;";
		let token = Token {
			kind: TokenKind::IntLiteral(42),
			span: Span {
				start: 8,
				end: 10,
				start_line: 1,
				start_col: 9,
				end_line: 1,
				end_col: 11,
			},
		};

		let error = token.format_error(source, "unexpected number");
		assert!(error.contains("Error at 1:9"));
		assert!(error.contains("unexpected number"));
		assert!(error.contains("let x = 42;"));
		assert!(error.contains("^^"));
	}

	#[test]
	fn test_format_error_multiline()
	{
		let source = "let x = 42;\nlet y = 100;";
		let token = Token {
			kind: TokenKind::IntLiteral(100),
			span: Span {
				start: 20,
				end: 23,
				start_line: 2,
				start_col: 9,
				end_line: 2,
				end_col: 12,
			},
		};

		let error = token.format_error(source, "value too large");
		assert!(error.contains("Error at 2:9"));
		assert!(error.contains("value too large"));
		assert!(error.contains("let y = 100;"));
		assert!(error.contains("^^^"));
	}
}
