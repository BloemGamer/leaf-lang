use std::iter::Peekable;

use crate::Config;
use crate::lexer::{self, Lexer, Span, Token, TokenKind};

pub struct Parser<'source, 'config>
{
	config: &'config Config,
	lexer: Peekable<Lexer<'source, 'config>>,
	last_span: Span,
}

impl<'s, 'c> From<Lexer<'s, 'c>> for Parser<'s, 'c>
{
	fn from(lexer: Lexer<'s, 'c>) -> Self
	{
		let (config, lexer) = lexer.into_parts();
		Self {
			config,
			lexer: lexer.peekable(),
			last_span: Span::default(),
		}
	}
}

pub type Ident = String;

#[derive(Debug, Clone)]
pub struct Spanned<T>
{
	pub node: T,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Program
{
	pub items: Vec<Spanned<TopLevelDecl>>,
}

#[derive(Debug, Clone)]
pub enum TopLevelDecl
{
	Function(FunctionDecl),
	Struct(StructDecl),
	Union(UnionDecl),
	Enum(EnumDecl),
	TaggedUnion(TaggedUnionDecl),
	TypeAlias(TypeAliasDecl),
	Trait(TraitDecl),
	Namespace(NamespaceDecl),
	Impl(ImplDecl),
	Directive(Directive),
}

#[derive(Debug, Clone)]
pub enum Modifier
{
	Pub,
	Unsafe,
	Static,
	Inline,
	Directive(Directive),
}

#[derive(Debug, Clone)]
pub enum Directive
{
	Import(String),
	Use(Vec<Ident>),
	Custom
	{
		name: Ident,
		args: Vec<Expr>,
		block: Option<Block>,
	},
}

#[derive(Debug, Clone)]
pub struct FunctionDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Ident,
	pub generics: Vec<Ident>,
	pub params: Vec<Param>,
	pub return_type: Option<Type>,
	pub where_clause: Option<Vec<WhereConstraint>>,
	pub body: Option<Block>, // Should be none for function prototyes, mostly used for external things
}

#[derive(Debug, Clone)]
pub struct Param
{
	pub ty: Type,
	pub name: Ident,
}

#[derive(Debug, Clone)]
pub struct Type
{
	pub modifiers: Vec<TypeModifier>,
	pub core: Box<TypeCore>,
}

#[derive(Debug, Clone)]
pub enum TypeModifier
{
	Volatile,
}

#[derive(Debug, Clone)]
pub enum TypeCore
{
	Base
	{
		path: Vec<Ident>,
		generics: Vec<Type>,
	},

	Reference
	{
		mutable: bool,
		inner: Box<TypeCore>,
	},

	Pointer
	{
		inner: Box<TypeCore>,
	},

	Array
	{
		inner: Box<TypeCore>,
		size: Box<Expr>,
	},

	Tuple(Vec<Type>),
}

#[derive(Debug, Clone)]
pub struct RangeExpr
{
	start: Option<Box<Expr>>,
	end: Option<Box<Expr>>,
	inclusive: bool,
}

#[derive(Debug, Clone)]
pub enum Expr
{
	Identifier(Vec<Ident>),

	Literal(Literal),

	Unary
	{
		op: UnaryOp,
		expr: Box<Expr>,
	},

	Binary
	{
		op: BinaryOp,
		lhs: Box<Expr>,
		rhs: Box<Expr>,
	},

	Cast
	{
		ty: Box<Type>,
		expr: Box<Expr>,
	},

	Call
	{
		callee: Box<Expr>,
		args: Vec<Expr>,
	},

	Field
	{
		base: Box<Expr>,
		name: Ident,
	},

	Index
	{
		base: Box<Expr>,
		index: Box<Expr>,
	},

	Range(RangeExpr),

	Tuple(Vec<Expr>),

	Array(ArrayLiteral),

	StructInit
	{
		path: Vec<Ident>,
		fields: Vec<(Ident, Expr)>,
	},

	Block(Box<Block>),

	Match
	{
		expr: Box<Expr>,
		arms: Vec<MatchArm>,
	},
}

#[derive(Debug, Clone)]
pub enum Literal
{
	Int(i64),
	Float(f64),
	Bool(bool),
	String(String),
	Char(char),
}

#[derive(Debug, Clone)]
pub enum ArrayLiteral
{
	List(Vec<Expr>),
	Repeat
	{
		value: Vec<Expr>,
		count: Box<Expr>,
	},
}

#[derive(Debug, Clone)]
pub enum UnaryOp
{
	Neg,
	Not,
	Deref,
	Addr
	{
		mutable: bool,
	},
}

#[derive(Debug, Clone)]
pub enum BinaryOp
{
	LogicalOr,
	LogicalAnd,
	Eq,
	Ne,
	Lt,
	Gt,
	Le,
	Ge,
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	BitAnd,
	BitOr,
	BitXor,
	Shl,
	Shr,
}

#[derive(Debug, Clone)]
pub enum AssignOp
{
	Assign,
	AddAssign,
	SubAssign,
	MulAssign,
	DivAssign,
	ModAssign,
	AndAssign,
	OrAssign,
	XorAssign,
	ShlAssign,
	ShrAssign,
}

#[derive(Debug, Clone)]
pub enum Stmt
{
	VariableDecl
	{
		modifiers: Vec<Modifier>,
		ty: Type,
		name: Ident,
		init: Option<Expr>,
	},

	Assignment
	{
		target: Expr,
		op: AssignOp,
		value: Expr,
	},

	Return(Option<Expr>),

	Expr(Expr),

	Break, // Maybe later it will have a value, to break a named loop
	Continue,

	If
	{
		cond: Expr,
		then_block: Block,
		else_branch: Option<Box<Stmt>>, // block or nested if
	},

	While
	{
		cond: Expr,
		body: Block,
	},

	For
	{
		name: Ident,
		iter: Expr,
		body: Block,
	},

	Unsafe(Block),
}

#[derive(Debug, Clone)]
pub struct Block
{
	pub stmts: Vec<Stmt>,
	pub tail_expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct MatchArm
{
	pub pattern: Pattern,
	pub body: MatchBody,
}

#[derive(Debug, Clone)]
pub enum MatchBody
{
	Expr(Expr),
	Block(Block),
}

#[derive(Debug, Clone)]
pub enum Pattern
{
	Wildcard,
	Literal(Literal),
	Variant
	{
		path: Vec<Ident>,
		args: Vec<Pattern>,
	},
	Range(RangeExpr),
}

#[derive(Debug, Clone)]
pub struct StructDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Ident,
	pub fields: Vec<(Type, Ident)>,
}

pub type UnionDecl = StructDecl;

#[derive(Debug, Clone)]
pub struct EnumDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Ident,
	pub variants: Vec<(Ident, Option<Expr>)>,
}

#[derive(Debug, Clone)]
pub struct TaggedUnionDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Ident,
	pub variants: Vec<(Ident, Vec<Type>)>,
}

#[derive(Debug, Clone)]
pub struct TraitDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Ident,
	pub generics: Vec<Ident>,
	pub items: Vec<FunctionSignature>,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature
{
	pub name: Ident,
	pub generics: Vec<Ident>,
	pub params: Vec<Param>,
	pub return_type: Option<Type>,
	pub where_clause: Vec<WhereConstraint>,
}

#[derive(Debug, Clone)]
pub struct ImplDecl
{
	pub modifiers: Vec<Modifier>,
	pub generics: Vec<Ident>,
	pub target: Vec<Ident>,
	pub trait_path: Option<Vec<Ident>>,
	pub where_clause: Vec<WhereConstraint>,
	pub body: Block,
}

#[derive(Debug, Clone)]
pub struct WhereConstraint
{
	pub ty: Ident,
	pub bounds: Vec<Vec<Ident>>,
}

#[derive(Debug, Clone)]
pub struct ParseError
{
	pub span: Span,
	pub message: String,
}

impl std::fmt::Display for ParseError
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "Parse error at {:?}: {}", self.span, self.message)
	}
}

impl std::error::Error for ParseError {}

#[derive(Debug, Clone)]
pub struct TypeAliasDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Ident,
	pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct NamespaceDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Ident,
	pub body: Block,
}

impl<'s, 'c> Parser<'s, 'c>
{
	/// Look at the next token without consuming it
	fn peek(&mut self) -> &Token
	{
		self.lexer.peek().expect("lexer exhausted unexpectedly")
	}

	/// Consume and return the next token
	fn next(&mut self) -> Token
	{
		let tok = self.lexer.next().expect("lexer exhausted unexpectedly");
		self.last_span = tok.span;
		tok
	}

	/// Returns the TokenKind of the next token for convenience
	fn peek_kind(&mut self) -> &TokenKind
	{
		&self.peek().kind
	}

	/// Check if the next token is the given kind
	fn at(&mut self, kind: &TokenKind) -> bool
	{
		self.peek_kind() == kind
	}

	/// Consume the next token if it matches the given kind
	fn consume(&mut self, kind: &TokenKind) -> bool
	{
		if self.at(kind) {
			self.next();
			true
		} else {
			false
		}
	}

	fn expect(&mut self, expected: &TokenKind) -> Result<Token, ParseError>
	{
		let tok = self.peek();

		if &tok.kind == expected {
			Ok(self.next())
		} else {
			Err(ParseError {
				span: tok.span,
				message: format!("expected {:?}, found {:?}", expected, tok.kind),
			})
		}
	}

	pub fn parse_program(&mut self) -> Result<Program, ParseError>
	{
		let mut items = Vec::new();

		while !matches!(self.peek().kind, TokenKind::Eof) {
			let decl = self.parse_top_level_decl()?;
			items.push(decl);
		}

		Ok(Program { items })
	}

	fn parse_top_level_decl(&mut self) -> Result<Spanned<TopLevelDecl>, ParseError>
	{
		let token = self.peek();
		let start_span = token.span;

		match &token.kind {
			// TokenKind::Directive() => {}
			other => {
				return Err(ParseError {
					span: token.span,
					message: format!("unexplected token at top level: {:?}", other),
				});
			}
		}
	}
}
