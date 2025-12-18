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
	IntLiteral(i64),
	FloatLiteral(f64),
	CharLiteral(char),
	StringLiteral(String),
	Identifier(String),

	If,
	Else,
	While,
	For,
	Match,

	Plus,
	Minus,
	Star,
	Slash,
	Equals,
	Mod,
	Pipe,
	Anpersand,
	Bang,
	Caret,
	Tilda,
	EqualsEquals,
	EqualsPlus,
	EqualsMinus,
	EqualsStar,
	EqualsSlash,
	EqualsMod,

	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
	Semicolon,
	Comma,
	Dot,
	Questionmark,

	MacroDef,      // macro
	Macro(String), // $[identefier]
	FuncDef,       // fn

	DocsComment(String),  // "///" | "/**"
	Directive(Directive), //@[identefier]
	Eof,
	Invalid,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Directive
{
	Use,
	Import,
	Custom(String),
}
