pub struct Lexer<'source>
{
	source: &'source str,
	position: usize,
	current_char: Option<char>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token
{
	pub kind: TokenKind,
	pub span: Span,
	pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span
{
	pub start: usize,
	pub end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind
{
	// ===== Literals =====
	IntLiteral(i64),       // Integer literal: 42, -10, 0xFF
	FloatLiteral(f64),     // Floating point literal: 3.14, -0.5, 1e10
	CharLiteral(char),     // Character literal: 'a', '\n', '\0'
	StringLiteral(String), // String literal: "hello", "world\n"
	True,                  // Boolean literal: true
	False,                 // Boolean literal: false

	// ===== Identifiers =====
	Identifier(String), // Variable/function names: foo, bar, my_var
	Underscore,         // Wildcard pattern: _

	// ===== Keywords - Control Flow =====
	If,       // Conditional: if
	Else,     // Conditional alternative: else
	While,    // Loop: while
	For,      // Iterator loop: for
	Match,    // Pattern matching: match
	Return,   // Return from function: return
	Break,    // Exit loop: break
	Continue, // Skip to next iteration: continue

	// ===== Keywords - Declarations =====
	FuncDef,     // Function definition: fn
	Const,       // Constant declaration: const
	Static,      // Static variable: static
	Struct,      // Structure definition: struct
	Union,       // Untagged union: union
	TaggedUnion, // Tagged union: tagged_union
	Enum,        // Enumeration definition: enum
	Impl,        // Implementation block: impl
	Type,        // Type alias: type
	MacroDef,    // Macro definition: macro

	// ===== Keywords - Modifiers =====
	Pub,      // Public visibility: pub
	Mut,      // Mutable binding: mut
	MutRef,   // Mutable reference: mut_ref or &mut
	Ref,      // Immutable reference: ref
	Unsafe,   // Unsafe block/function: unsafe
	Volatile, // Volatile memory access: volatile

	// ===== Keywords - Other =====
	In,    // Iterator source: in (for x in iter)
	As,    // Type casting: as
	Where, // Generic constraints: where

	// ===== Arithmetic Operators =====
	Plus,  // Addition: +
	Minus, // Subtraction or negation: -
	Star,  // Multiplication or dereference: *
	Slash, // Division: /
	Mod,   // Modulo/remainder: %

	// ===== Bitwise Operators =====
	Pipe,      // Bitwise OR: |
	Ampersand, // Bitwise AND or reference: &
	Caret,     // Bitwise XOR: ^
	Tilde,     // Bitwise NOT: ~
	LShift,    // Left shift:
	RShift,    // Right shift: >>

	// ===== Logical Operators =====
	Bang, // Logical NOT: !
	And,  // Logical AND: &&
	Or,   // Logical OR: ||

	// ===== Comparison Operators =====
	LessThan,      // Less than:
	GreaterThan,   // Greater than: >
	LessEquals,    // Less than or equal: <=
	GreaterEquals, // Greater than or equal: >=
	EqualsEquals,  // Equality: ==
	BangEquals,    // Inequality: !=

	// ===== Assignment Operators =====
	Equals,          // Assignment: =
	PlusEquals,      // Add and assign: +=
	MinusEquals,     // Subtract and assign: -=
	StarEquals,      // Multiply and assign: *=
	SlashEquals,     // Divide and assign: /=
	ModEquals,       // Modulo and assign: %=
	PipeEquals,      // Bitwise OR and assign: |=
	AmpersandEquals, // Bitwise AND and assign: &=
	CaretEquals,     // Bitwise XOR and assign: ^=
	TildeEquals,     // Bitwise NOT and assign: ~=
	LShiftEquals,    // Left shift and assign: <<=
	RShiftEquals,    // Right shift and assign: >>=

	// ===== Delimiters =====
	LeftParen,    // Opening parenthesis: (
	RightParen,   // Closing parenthesis: )
	LeftBrace,    // Opening brace: {
	RightBrace,   // Closing brace: }
	LeftBracket,  // Opening bracket: [
	RightBracket, // Closing bracket: ]

	// ===== Punctuation =====
	Semicolon,    // Statement terminator: ;
	Colon,        // Type annotation separator: :
	DoubleColon,  // Path separator: ::
	Comma,        // List separator: ,
	Dot,          // Member access: .
	Ellipsis,     // Range or variadic: ...
	Arrow,        // Function return type: ->
	FatArrow,     // Match arm or closure: =>
	QuestionMark, // Optional/error propagation: ?
	Hash,         // Attribute prefix: #
	Backslash,    // Escape character: \

	// ===== Special Tokens =====
	Macro(String),        // Macro invocation: $identifier
	Directive(Directive), // Compiler directive: @identifier

	// ===== Comments =====
	LineComment(String),  // Single-line comment: // comment
	BlockComment(String), // Multi-line comment: /* comment */
	DocsComment(String),  // Documentation comment: /// or /**

	// ===== End/Error =====
	Eof,     // End of file
	Invalid, // Invalid/unrecognized token
}

#[derive(Debug, Clone, PartialEq)]
pub enum Directive
{
	Use,
	Import,
	Custom(String),
}
