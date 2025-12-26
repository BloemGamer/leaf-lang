use std::iter::Peekable;

use crate::Config;
use crate::lexer::{self, Lexer, Span, Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Parser<'source, 'config>
{
	config: &'config Config,
	source: &'source str,
	lexer: Peekable<Lexer<'source, 'config>>,
	last_span: Span,
}

impl<'s, 'c> From<Lexer<'s, 'c>> for Parser<'s, 'c>
{
	fn from(lexer: Lexer<'s, 'c>) -> Self
	{
		let (config, source, lexer) = lexer.into_parts();
		Self {
			config,
			source,
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

impl<T> Spanned<T>
{
	fn unpack(self) -> (T, Span)
	{
		return (self.node, self.span);
	}
}

#[derive(Debug, Clone)]
pub struct Program
{
	pub items: Vec<Spanned<TopLevelDecl>>,
}

pub type TopLevelBlock = Program;

#[derive(Debug, Clone)]
pub enum TopLevelDecl
{
	Function(FunctionDecl),
	VariableDecl(VariableDecl),
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

#[derive(Debug, Clone, Copy)]
enum DeclKind
{
	Function,
	Struct,
	Union,
	Enum,
	Trait,
	Impl,
	TypeAlias,
	Namespace,
	Variable,
	Directive,
}

#[derive(Debug, Clone)]
pub enum Modifier
{
	Pub,
	Unsafe,
	Inline,
	Directive(Directive),
	Const,
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
		block: Option<BlockContent>,
	},
}

#[derive(Debug, Clone)]
pub struct FunctionDecl
{
	pub signature: FunctionSignature,
	pub body: Option<Block>, // Should be none for function prototypes, mostly used for external things
}

#[derive(Debug, Clone)]
pub struct FunctionSignature
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub generics: Vec<Ident>,
	pub params: Vec<Param>,
	pub return_type: Option<Type>,
	pub where_clause: Vec<WhereConstraint>,
	pub heap_func: bool,
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
	Directive(Directive),
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
struct VariableDecl
{
	ty: Type,
	name: Vec<Ident>,
	init: Option<Expr>,
	comp_const: bool,
}

#[derive(Debug, Clone)]
pub enum Stmt
{
	VariableDecl(VariableDecl),

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
pub enum BlockContent
{
	Block(Block),
	TopLevelBlock(TopLevelBlock),
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
	pub name: Vec<Ident>,
	pub fields: Vec<(Type, Ident)>,
}

pub type UnionDecl = StructDecl;

#[derive(Debug, Clone)]
pub struct EnumDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub variants: Vec<(Ident, Option<Expr>)>,
}

#[derive(Debug, Clone)]
pub struct TaggedUnionDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
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
pub struct ImplDecl
{
	pub modifiers: Vec<Modifier>,
	pub generics: Vec<Ident>,
	pub target: Vec<Ident>,
	pub trait_path: Option<Vec<Ident>>,
	pub where_clause: Vec<WhereConstraint>,
	pub body: Vec<Spanned<ImplItem>>,
}

#[derive(Debug, Clone)]
pub enum ImplItem
{
	Function(FunctionDecl),
	TypeAlias(TypeAliasDecl),
	Const(VariableDecl),
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
	pub body: TopLevelBlock,
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
		let tok: Token = self.lexer.next().expect("lexer exhausted unexpectedly");
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
		let tok: &Token = self.peek();

		if &tok.kind == expected {
			Ok(self.next())
		} else {
			let tok: Token = tok.clone();
			Err(ParseError {
				span: tok.span,
				message: tok.format_error(self.source, &format!("expected {:?}, found {:?}", expected, tok.kind)),
			})
		}
	}

	pub fn parse_program(&mut self) -> Result<Program, ParseError>
	{
		let mut items: Vec<Spanned<TopLevelDecl>> = Vec::new();

		while !matches!(self.peek().kind, TokenKind::Eof) {
			let decl = self.parse_top_level_decl()?;
			items.push(decl);
		}

		Ok(Program { items })
	}

	fn parse_top_level_decl(&mut self) -> Result<Spanned<TopLevelDecl>, ParseError>
	{
		let decl_kind = self.peek_declaration_kind()?;

		let (node, span) = match decl_kind {
			DeclKind::Function => {
				let (func_decl, span) = self.parse_function_decl()?.unpack();
				(TopLevelDecl::Function(func_decl), span)
			}
			DeclKind::Variable => {
				let (var_decl, span) = self.parse_var_decl()?.unpack();
				self.expect(&TokenKind::Semicolon)?;
				(TopLevelDecl::VariableDecl(var_decl), span)
			}
			DeclKind::Directive => {
				let (directive, span) = self.parse_directive()?.unpack();
				self.expect(&TokenKind::Semicolon)?;
				(TopLevelDecl::Directive(directive), span)
			}
			DeclKind::Struct => {
				let (struct_decl, span) = self.parse_struct()?.unpack();

				(TopLevelDecl::Struct(struct_decl), span)
			}
			DeclKind::Union => {
				let (union_decl, span) = self.parse_union()?.unpack();

				(TopLevelDecl::Union(union_decl), span)
			}
			other => todo!("not yet implemented: {:?}", other),
		};

		Ok(Spanned { node, span })
	}

	fn peek_declaration_kind(&mut self) -> Result<DeclKind, ParseError>
	{
		let checkpoint = self.lexer.clone();
		let checkpoint_span = self.last_span;

		loop {
			match self.peek_kind() {
				TokenKind::Pub | TokenKind::Unsafe | TokenKind::Inline | TokenKind::Directive(_) => {
					self.next();
				}
				TokenKind::Const => {
					self.next();
					if self.at(&TokenKind::FuncDef) {
						self.lexer = checkpoint;
						self.last_span = checkpoint_span;
						return Ok(DeclKind::Function);
					} else {
						self.lexer = checkpoint;
						self.last_span = checkpoint_span;
						return Ok(DeclKind::Variable);
					}
				}
				TokenKind::FuncDef => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Function);
				}
				TokenKind::Struct => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Struct);
				}
				TokenKind::Let => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Variable);
				}
				_ => {
					let tok = self.peek().clone();
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Err(ParseError {
						span: tok.span,
						message: format!("unexpected token in declaration: {:?}", tok.kind),
					});
				}
			}
		}
	}

	fn parse_directive(&mut self) -> Result<Spanned<Directive>, ParseError>
	{
		debug_assert!(matches!(self.peek().kind, TokenKind::Directive(_)));

		let tok: Token = self.next();
		let start: Span = tok.span;

		let node: Directive = match tok.kind {
			TokenKind::Directive(d) => self.parse_directive_kind(d)?,
			_ => unreachable!("{}", tok.format_error(self.source, "Bug: Token should be a directive")),
		};

		let end: Span = self.last_span;

		Ok(Spanned {
			node,
			span: start.merge(&end),
		})
	}

	fn parse_directive_kind(&mut self, direct: lexer::Directive) -> Result<Directive, ParseError>
	{
		return match direct {
			lexer::Directive::Use => {
				let ret: Directive = Directive::Use(self.get_path()?);
				Ok(ret)
			}
			lexer::Directive::Import => {
				let incl: Token = self.next();
				let ret: Directive = match &incl.kind {
					TokenKind::StringLiteral(str) => Directive::Import(str.to_string()),
					_ => {
						return Err(ParseError {
							span: incl.span,
							message: incl.format_error(self.source, "expected a string for @import"),
						});
					}
				};
				Ok(ret)
			}
			lexer::Directive::Custom(_name) => {
				todo!() // I have not yet decided how I want to do this one
			}
		};
	}

	fn parse_var_decl(&mut self) -> Result<Spanned<VariableDecl>, ParseError>
	{
		let tok: Token = self.next();
		let span: Span = tok.span;
		if !matches!(tok.kind, TokenKind::Const | TokenKind::Let) {
			unreachable!(
				"Bug: expected const or let for a variable declaration, got: {:?}",
				tok.kind
			);
		}
		let comp_const: bool = tok.kind == TokenKind::Const;

		let var_name: Vec<Ident> = if matches!(self.peek_kind(), TokenKind::Identifier(_)) {
			self.get_path()?
		} else {
			let tok: Token = self.next();
			return Err(ParseError {
				span: tok.span,
				message: tok.format_error(self.source, &format!("expected identifier, got: {:?}", tok.kind)),
			});
		};

		self.expect(&TokenKind::Colon)?;

		let ty = self.parse_type()?;

		let tok: &Token = self.peek();
		let init: Option<Expr> = if tok.kind == TokenKind::Equals {
			self.next();
			Some(self.parse_expr()?)
		} else {
			None
		};

		return Ok(Spanned {
			node: VariableDecl {
				ty,
				name: var_name,
				init,
				comp_const,
			},
			span: self.last_span.merge(&span),
		});
	}

	fn parse_type(&mut self) -> Result<Type, ParseError>
	{
		let modifiers: Vec<TypeModifier> = self.parse_type_modifiers()?;
		let core: TypeCore = self.parse_type_core()?;
		return Ok(Type {
			modifiers,
			core: Box::new(self.parse_type_suffix(core)?),
		});
	}

	fn parse_type_core(&mut self) -> Result<TypeCore, ParseError>
	{
		let tok: &Token = self.peek();
		match &tok.kind {
			TokenKind::Identifier(_) => {
				return Ok(TypeCore::Base {
					path: self.get_path()?,
					generics: Vec::new(),
				});
			}
			TokenKind::Ampersand => {
				self.next();
				let mutable: bool = self.at(&TokenKind::Mut);
				if mutable {
					self.next();
				}
				return Ok(TypeCore::Reference {
					mutable,
					inner: Box::new(self.parse_type_core()?),
				});
			}
			_ => {
				let tok = tok.clone();
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, "Expected a ampersand, mut or identefier"),
				});
			}
		}
	}

	fn parse_type_modifiers(&mut self) -> Result<Vec<TypeModifier>, ParseError>
	{
		let mut ret: Vec<TypeModifier> = Vec::new();

		loop {
			let tok: &Token = self.peek();
			match &tok.kind {
				TokenKind::Volatile => ret.push(TypeModifier::Volatile),
				TokenKind::Directive(d) => ret.push(TypeModifier::Directive(self.parse_directive()?.node)),
				_ => return Ok(ret),
			}
			self.next();
		}
	}

	fn parse_type_suffix(&mut self, mut base: TypeCore) -> Result<TypeCore, ParseError>
	{
		loop {
			match self.peek_kind() {
				TokenKind::Star => {
					self.next(); // consume '*'
					base = TypeCore::Pointer { inner: Box::new(base) };
				}
				TokenKind::LeftBracket => {
					self.next(); // consume '['
					// let size_expr = self.parse_expr()?; // TODO: Implement parse_expr
					self.expect(&TokenKind::RightBracket)?; // consume ']'
					// base = TypeCore::Array {
					// 	inner: Box::new(base),
					// 	size: Box::new(size_expr),
					// };
				}
				TokenKind::Comma | TokenKind::RightParen => {
					break;
				}
				_ => break,
			}
		}
		Ok(base)
	}

	fn get_path(&mut self) -> Result<Vec<Ident>, ParseError>
	{
		let mut path: Vec<Ident> = Vec::new();
		loop {
			let tok: Token = self.next();
			match &tok.kind {
				TokenKind::Identifier(s) => path.push(s.to_string()),
				_ => {
					return Err(ParseError {
						span: tok.span,
						message: tok.format_error(self.source, "expected identifier in @use path"),
					});
				}
			}

			if self.peek().kind != TokenKind::DoubleColon {
				return Ok(path);
			}

			self.next();
		}
	}

	fn get_generics(&mut self) -> Result<Vec<Ident>, ParseError>
	{
		assert_eq!(
			*self.peek_kind(),
			TokenKind::LessThan,
			"Generics are not yet implemented"
		);
		return Ok(Vec::new());
	}

	pub fn parse_expr(&mut self) -> Result<Expr, ParseError>
	{
		self.parse_logical_or()
	}

	fn parse_logical_or(&mut self) -> Result<Expr, ParseError>
	{
		let mut lhs: Expr = self.parse_logical_and()?;

		while self.consume(&TokenKind::Or) {
			let rhs: Expr = self.parse_logical_and()?;
			lhs = Expr::Binary {
				op: BinaryOp::LogicalOr,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			};
		}

		Ok(lhs)
	}

	fn parse_logical_and(&mut self) -> Result<Expr, ParseError>
	{
		let mut lhs: Expr = self.parse_equality()?;

		while self.consume(&TokenKind::And) {
			let rhs: Expr = self.parse_equality()?;
			lhs = Expr::Binary {
				op: BinaryOp::LogicalAnd,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			};
		}

		Ok(lhs)
	}

	fn parse_equality(&mut self) -> Result<Expr, ParseError>
	{
		let mut lhs: Expr = self.parse_relational()?;

		loop {
			let op: BinaryOp = match self.peek_kind() {
				TokenKind::EqualsEquals => BinaryOp::Eq,
				TokenKind::BangEquals => BinaryOp::Ne,
				_ => break,
			};

			self.next();
			let rhs: Expr = self.parse_relational()?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			};
		}

		Ok(lhs)
	}

	fn parse_relational(&mut self) -> Result<Expr, ParseError>
	{
		let mut lhs: Expr = self.parse_range()?;

		loop {
			let op: BinaryOp = match self.peek_kind() {
				TokenKind::LessThan => BinaryOp::Lt,
				TokenKind::GreaterThan => BinaryOp::Gt,
				TokenKind::LessEquals => BinaryOp::Le,
				TokenKind::GreaterEquals => BinaryOp::Ge,
				_ => break,
			};

			self.next();
			let rhs: Expr = self.parse_range()?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			};
		}

		Ok(lhs)
	}

	fn parse_range(&mut self) -> Result<Expr, ParseError>
	{
		let start: Expr = self.parse_bitwise()?;

		match self.peek_kind() {
			TokenKind::DotDot => {
				self.next();
				let end: Option<Box<Expr>> = if self.is_range_end() {
					None
				} else {
					Some(Box::new(self.parse_bitwise()?))
				};
				Ok(Expr::Range(RangeExpr {
					start: Some(Box::new(start)),
					end,
					inclusive: false,
				}))
			}
			TokenKind::DotDotEquals => {
				self.next();
				let end: Box<Expr> = Box::new(self.parse_bitwise()?);
				Ok(Expr::Range(RangeExpr {
					start: Some(Box::new(start)),
					end: Some(end),
					inclusive: true,
				}))
			}
			_ => Ok(start),
		}
	}

	fn is_range_end(&mut self) -> bool
	{
		matches!(
			self.peek_kind(),
			TokenKind::Comma
				| TokenKind::RightParen
				| TokenKind::RightBracket
				| TokenKind::RightBrace
				| TokenKind::Semicolon
		)
	}

	fn parse_bitwise(&mut self) -> Result<Expr, ParseError>
	{
		let mut lhs: Expr = self.parse_additive()?;

		loop {
			let op: BinaryOp = match self.peek_kind() {
				TokenKind::Ampersand => BinaryOp::BitAnd,
				TokenKind::Pipe => BinaryOp::BitOr,
				TokenKind::Caret => BinaryOp::BitXor,
				TokenKind::LShift => BinaryOp::Shl,
				TokenKind::RShift => BinaryOp::Shr,
				_ => break,
			};

			self.next();
			let rhs: Expr = self.parse_additive()?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			};
		}

		Ok(lhs)
	}

	fn parse_additive(&mut self) -> Result<Expr, ParseError>
	{
		let mut lhs: Expr = self.parse_multiplicative()?;

		loop {
			let op: BinaryOp = match self.peek_kind() {
				TokenKind::Plus => BinaryOp::Add,
				TokenKind::Minus => BinaryOp::Sub,
				_ => break,
			};

			self.next();
			let rhs: Expr = self.parse_multiplicative()?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			};
		}

		Ok(lhs)
	}

	fn parse_multiplicative(&mut self) -> Result<Expr, ParseError>
	{
		let mut lhs: Expr = self.parse_cast()?;

		loop {
			let op: BinaryOp = match self.peek_kind() {
				TokenKind::Star => BinaryOp::Mul,
				TokenKind::Slash => BinaryOp::Div,
				TokenKind::Mod => BinaryOp::Mod,
				_ => break,
			};

			self.next();
			let rhs: Expr = self.parse_cast()?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
			};
		}

		Ok(lhs)
	}

	fn parse_cast(&mut self) -> Result<Expr, ParseError>
	{
		if self.at(&TokenKind::LeftParen) {
			let checkpoint: Peekable<Lexer<'s, 'c>> = self.lexer.clone();
			self.next(); // consume '('

			if let Ok(ty) = self.parse_type()
				&& self.consume(&TokenKind::RightParen)
			{
				let expr: Expr = self.parse_cast()?;
				return Ok(Expr::Cast {
					ty: Box::new(ty),
					expr: Box::new(expr),
				});
			}

			self.lexer = checkpoint;
		}

		self.parse_unary()
	}

	fn parse_unary(&mut self) -> Result<Expr, ParseError>
	{
		let op: UnaryOp = match self.peek_kind() {
			TokenKind::Bang => {
				self.next();
				UnaryOp::Not
			}
			TokenKind::Minus => {
				self.next();
				UnaryOp::Neg
			}
			TokenKind::Star => {
				self.next();
				UnaryOp::Deref
			}
			TokenKind::Ampersand => {
				self.next();
				let mutable: bool = self.consume(&TokenKind::Mut);
				UnaryOp::Addr { mutable }
			}
			_ => return self.parse_postfix(),
		};

		let expr: Expr = self.parse_unary()?;
		Ok(Expr::Unary {
			op,
			expr: Box::new(expr),
		})
	}

	fn parse_postfix(&mut self) -> Result<Expr, ParseError>
	{
		let mut expr: Expr = self.parse_primary()?;

		loop {
			match self.peek_kind() {
				TokenKind::Dot => {
					self.next();
					let field_tok: Token = self.expect(&TokenKind::Identifier(String::new()))?;
					let field_name: Ident = if let TokenKind::Identifier(name) = field_tok.kind {
						name
					} else {
						unreachable!()
					};
					expr = Expr::Field {
						base: Box::new(expr),
						name: field_name,
					};
				}
				TokenKind::LeftBracket => {
					self.next();
					let index: Expr = self.parse_expr()?;
					self.expect(&TokenKind::RightBracket)?;
					expr = Expr::Index {
						base: Box::new(expr),
						index: Box::new(index),
					};
				}
				TokenKind::LeftParen => {
					self.next();
					let args: Vec<Expr> = self.parse_argument_list()?;
					self.expect(&TokenKind::RightParen)?;
					expr = Expr::Call {
						callee: Box::new(expr),
						args,
					};
				}
				_ => break,
			}
		}

		Ok(expr)
	}

	fn parse_primary(&mut self) -> Result<Expr, ParseError>
	{
		let tok: Token = self.peek().clone();

		match &tok.kind {
			TokenKind::IntLiteral(n) => {
				self.next();
				Ok(Expr::Literal(Literal::Int(*n)))
			}
			TokenKind::FloatLiteral(f) => {
				self.next();
				Ok(Expr::Literal(Literal::Float(*f)))
			}
			TokenKind::StringLiteral(s) => {
				self.next();
				Ok(Expr::Literal(Literal::String(s.clone())))
			}
			TokenKind::CharLiteral(c) => {
				self.next();
				Ok(Expr::Literal(Literal::Char(*c)))
			}
			TokenKind::True => {
				self.next();
				Ok(Expr::Literal(Literal::Bool(true)))
			}
			TokenKind::False => {
				self.next();
				Ok(Expr::Literal(Literal::Bool(false)))
			}

			TokenKind::Identifier(_) => {
				let path: Vec<String> = self.get_path()?;

				if self.at(&TokenKind::LeftBrace) {
					self.next();
					let fields: Vec<(String, Expr)> = self.parse_struct_fields()?;
					self.expect(&TokenKind::RightBrace)?;
					Ok(Expr::StructInit { path, fields })
				} else {
					Ok(Expr::Identifier(path))
				}
			}

			TokenKind::LeftParen => {
				self.next();

				if self.consume(&TokenKind::RightParen) {
					return Ok(Expr::Tuple(Vec::new()));
				}

				let first: Expr = self.parse_expr()?;

				if self.consume(&TokenKind::RightParen) {
					return Ok(first);
				}

				if self.consume(&TokenKind::Comma) {
					let mut elements = vec![first];

					if self.consume(&TokenKind::RightParen) {
						return Ok(Expr::Tuple(elements));
					}

					loop {
						elements.push(self.parse_expr()?);
						if !self.consume(&TokenKind::Comma) {
							break;
						}
						if self.at(&TokenKind::RightParen) {
							break;
						}
					}

					self.expect(&TokenKind::RightParen)?;
					return Ok(Expr::Tuple(elements));
				}

				Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, "expected ',' or ')' in tuple"),
				})
			}

			TokenKind::LeftBracket => {
				self.next();

				if self.consume(&TokenKind::RightBracket) {
					return Ok(Expr::Array(ArrayLiteral::List(Vec::new())));
				}

				let first: Expr = self.parse_expr()?;

				if self.consume(&TokenKind::Semicolon) {
					let count: Expr = self.parse_expr()?;
					self.expect(&TokenKind::RightBracket)?;
					return Ok(Expr::Array(ArrayLiteral::Repeat {
						value: vec![first],
						count: Box::new(count),
					}));
				}

				let mut elements: Vec<Expr> = vec![first];
				while self.consume(&TokenKind::Comma) {
					if self.at(&TokenKind::RightBracket) {
						break;
					}
					elements.push(self.parse_expr()?);
				}

				self.expect(&TokenKind::RightBracket)?;
				Ok(Expr::Array(ArrayLiteral::List(elements)))
			}

			TokenKind::LeftBrace => {
				let block: Block = self.parse_block()?;
				Ok(Expr::Block(Box::new(block)))
			}

			TokenKind::Match => {
				self.next();
				let expr: Expr = self.parse_expr()?;
				self.expect(&TokenKind::LeftBrace)?;

				let mut arms: Vec<MatchArm> = Vec::new();
				while !self.at(&TokenKind::RightBrace) {
					arms.push(self.parse_match_arm()?);
				}

				self.expect(&TokenKind::RightBrace)?;
				Ok(Expr::Match {
					expr: Box::new(expr),
					arms,
				})
			}

			_ => Err(ParseError {
				span: tok.span,
				message: tok.format_error(self.source, "expected expression"),
			}),
		}
	}

	fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError>
	{
		if self.at(&TokenKind::RightParen) {
			return Ok(Vec::new());
		}

		let mut args: Vec<Expr> = vec![self.parse_expr()?];

		while self.consume(&TokenKind::Comma) {
			if self.at(&TokenKind::RightParen) {
				break;
			}
			args.push(self.parse_expr()?);
		}

		Ok(args)
	}

	fn parse_struct_fields(&mut self) -> Result<Vec<(Ident, Expr)>, ParseError>
	{
		if self.at(&TokenKind::RightBrace) {
			return Ok(Vec::new());
		}

		let mut fields: Vec<(String, Expr)> = Vec::new();

		loop {
			let name_tok: Token = self.expect(&TokenKind::Identifier(String::new()))?;
			let name: Ident = if let TokenKind::Identifier(n) = name_tok.kind {
				n
			} else {
				unreachable!()
			};

			self.expect(&TokenKind::Equals)?;
			let value: Expr = self.parse_expr()?;

			fields.push((name, value));

			if !self.consume(&TokenKind::Comma) {
				break;
			}
			if self.at(&TokenKind::RightBrace) {
				break;
			}
		}

		Ok(fields)
	}

	fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError>
	{
		let pattern: Pattern = self.parse_pattern()?;
		self.expect(&TokenKind::FatArrow)?; // =>

		let body = if self.at(&TokenKind::LeftBrace) {
			MatchBody::Block(self.parse_block()?)
		} else {
			let expr = self.parse_expr()?;
			self.expect(&TokenKind::Comma)?;
			MatchBody::Expr(expr)
		};

		Ok(MatchArm { pattern, body })
	}

	fn parse_pattern(&mut self) -> Result<Pattern, ParseError>
	{
		let tok = self.peek().clone();

		match &tok.kind {
			TokenKind::Underscore => {
				self.next();
				Ok(Pattern::Wildcard)
			}
			TokenKind::Identifier(_) => {
				let path: Vec<String> = self.get_path()?;

				if self.consume(&TokenKind::LeftParen) {
					let mut args: Vec<Pattern> = Vec::new();

					if !self.at(&TokenKind::RightParen) {
						loop {
							args.push(self.parse_pattern()?);
							if !self.consume(&TokenKind::Comma) {
								break;
							}
							if self.at(&TokenKind::RightParen) {
								break;
							}
						}
					}

					self.expect(&TokenKind::RightParen)?;
					Ok(Pattern::Variant { path, args })
				} else {
					Ok(Pattern::Variant { path, args: Vec::new() })
				}
			}
			TokenKind::IntLiteral(n) => {
				self.next();
				Ok(Pattern::Literal(Literal::Int(*n)))
			}
			TokenKind::True => {
				self.next();
				Ok(Pattern::Literal(Literal::Bool(true)))
			}
			TokenKind::False => {
				self.next();
				Ok(Pattern::Literal(Literal::Bool(false)))
			}
			TokenKind::StringLiteral(s) => {
				self.next();
				Ok(Pattern::Literal(Literal::String(s.clone())))
			}
			TokenKind::CharLiteral(c) => {
				self.next();
				Ok(Pattern::Literal(Literal::Char(*c)))
			}
			_ => Err(ParseError {
				span: tok.span,
				message: tok.format_error(self.source, "expected pattern"),
			}),
		}
	}

	pub fn parse_block(&mut self) -> Result<Block, ParseError>
	{
		self.expect(&TokenKind::LeftBrace)?;

		let mut stmts: Vec<Stmt> = Vec::new();
		let mut tail_expr: Option<Box<Expr>> = None;

		while !self.at(&TokenKind::RightBrace) {
			if self.at(&TokenKind::Let) || self.at(&TokenKind::Const) {
				let var_decl = self.parse_var_decl()?;
				self.expect(&TokenKind::Semicolon)?;
				stmts.push(Stmt::VariableDecl(var_decl.node));
				continue;
			}

			if self.at(&TokenKind::Return) {
				self.next();
				let ret_expr = if self.at(&TokenKind::Semicolon) {
					None
				} else {
					Some(self.parse_expr()?)
				};
				self.expect(&TokenKind::Semicolon)?;
				stmts.push(Stmt::Return(ret_expr));
				continue;
			}

			if self.at(&TokenKind::Break) {
				self.next();
				self.expect(&TokenKind::Semicolon)?;
				stmts.push(Stmt::Break);
				continue;
			}

			if self.at(&TokenKind::Continue) {
				self.next();
				self.expect(&TokenKind::Semicolon)?;
				stmts.push(Stmt::Continue);
				continue;
			}

			if self.at(&TokenKind::While) {
				stmts.push(self.parse_while()?);
				continue;
			}

			if self.at(&TokenKind::For) {
				stmts.push(self.parse_for()?);
				continue;
			}

			if self.at(&TokenKind::If) {
				let if_stmt = self.parse_if()?;

				if self.consume(&TokenKind::Semicolon) {
					stmts.push(if_stmt);
				} else if self.at(&TokenKind::RightBrace) {
					tail_expr = Some(Box::new(self.stmt_if_to_expr(if_stmt)?));
					break;
				} else {
					stmts.push(if_stmt);
				}
				continue;
			}

			if self.at(&TokenKind::Unsafe) {
				self.next();
				let block = self.parse_block()?;
				if self.consume(&TokenKind::Semicolon) {
					stmts.push(Stmt::Unsafe(block));
				} else if self.at(&TokenKind::RightBrace) {
					tail_expr = Some(Box::new(Expr::Block(Box::new(block))));
					break;
				} else {
					stmts.push(Stmt::Unsafe(block));
				}
				continue;
			}

			let expr = self.parse_expr()?;

			if self.is_assignment_op() {
				let op = self.parse_assign_op()?;
				let value = self.parse_expr()?;
				self.expect(&TokenKind::Semicolon)?;
				stmts.push(Stmt::Assignment {
					target: expr,
					op,
					value,
				});
				continue;
			}

			let needs_semi = self.expr_needs_semicolon(&expr);

			if needs_semi {
				if self.consume(&TokenKind::Semicolon) {
					stmts.push(Stmt::Expr(expr));
				} else if self.at(&TokenKind::RightBrace) {
					tail_expr = Some(Box::new(expr));
					break;
				} else {
					let tok = self.peek().clone();
					return Err(ParseError {
						span: tok.span,
						message: tok.format_error(self.source, "expected `;` or `}` after expression"),
					});
				}
			} else if self.consume(&TokenKind::Semicolon) {
				stmts.push(Stmt::Expr(expr));
			} else if self.at(&TokenKind::RightBrace) {
				tail_expr = Some(Box::new(expr));
				break;
			} else {
				stmts.push(Stmt::Expr(expr));
			}
		}

		self.expect(&TokenKind::RightBrace)?;

		Ok(Block { stmts, tail_expr })
	}

	fn expr_needs_semicolon(&self, expr: &Expr) -> bool
	{
		!matches!(expr, Expr::Block(_) | Expr::Match { .. })
	}

	fn is_assignment_op(&mut self) -> bool
	{
		matches!(
			self.peek_kind(),
			TokenKind::Equals
				| TokenKind::PlusEquals
				| TokenKind::MinusEquals
				| TokenKind::StarEquals
				| TokenKind::SlashEquals
				| TokenKind::ModEquals
				| TokenKind::PipeEquals
				| TokenKind::AmpersandEquals
				| TokenKind::CaretEquals
				| TokenKind::TildeEquals
				| TokenKind::LShiftEquals
				| TokenKind::RShiftEquals
		)
	}

	fn parse_assign_op(&mut self) -> Result<AssignOp, ParseError>
	{
		let op = match self.peek_kind() {
			TokenKind::Equals => AssignOp::Assign,
			TokenKind::PlusEquals => AssignOp::AddAssign,
			TokenKind::MinusEquals => AssignOp::SubAssign,
			TokenKind::StarEquals => AssignOp::MulAssign,
			TokenKind::SlashEquals => AssignOp::DivAssign,
			TokenKind::ModEquals => AssignOp::ModAssign,
			TokenKind::PipeEquals => AssignOp::OrAssign,
			TokenKind::AmpersandEquals => AssignOp::AndAssign,
			TokenKind::CaretEquals => AssignOp::XorAssign,
			TokenKind::LShiftEquals => AssignOp::ShlAssign,
			TokenKind::RShiftEquals => AssignOp::ShrAssign,
			_ => {
				let tok = self.peek().clone();
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, "expected assignment operator"),
				});
			}
		};
		self.next();
		Ok(op)
	}

	fn stmt_if_to_expr(&self, stmt: Stmt) -> Result<Expr, ParseError>
	{
		match stmt {
			Stmt::If {
				cond,
				then_block,
				else_branch,
			} => Ok(Expr::Block(Box::new(Block {
				stmts: vec![Stmt::If {
					cond,
					then_block,
					else_branch,
				}],
				tail_expr: None,
			}))),
			_ => unreachable!("Expected if statement"),
		}
	}

	fn parse_if(&mut self) -> Result<Stmt, ParseError>
	{
		self.expect(&TokenKind::If)?;
		let cond = self.parse_expr()?;
		let then_block = self.parse_block()?;

		let else_branch = if self.consume(&TokenKind::Else) {
			if self.at(&TokenKind::If) {
				Some(Box::new(self.parse_if()?))
			} else {
				let else_block = self.parse_block()?;
				Some(Box::new(Stmt::If {
					cond: Expr::Literal(Literal::Bool(true)),
					then_block: else_block,
					else_branch: None,
				}))
			}
		} else {
			None
		};

		Ok(Stmt::If {
			cond,
			then_block,
			else_branch,
		})
	}

	fn parse_while(&mut self) -> Result<Stmt, ParseError>
	{
		self.expect(&TokenKind::While)?;
		let cond = self.parse_expr()?;
		let body = self.parse_block()?;
		Ok(Stmt::While { cond, body })
	}

	fn parse_for(&mut self) -> Result<Stmt, ParseError>
	{
		self.expect(&TokenKind::For)?;

		let name_tok = self.expect(&TokenKind::Identifier(String::new()))?;
		let name = if let TokenKind::Identifier(n) = name_tok.kind {
			n
		} else {
			unreachable!()
		};

		self.expect(&TokenKind::In)?;
		let iter = self.parse_expr()?;
		let body = self.parse_block()?;

		Ok(Stmt::For { name, iter, body })
	}

	fn parse_function_decl(&mut self) -> Result<Spanned<FunctionDecl>, ParseError>
	{
		let (signature, span): (FunctionSignature, Span) = self.parse_function_signature()?.unpack();
		let body: Option<Block> = if self.at(&TokenKind::Semicolon) {
			None
		} else {
			Some(self.parse_block()?)
		};
		let span: Span = span.merge(&self.last_span);
		return Ok(Spanned {
			node: FunctionDecl { signature, body },
			span,
		});
	}

	fn parse_function_signature(&mut self) -> Result<Spanned<FunctionSignature>, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;

		self.expect(&TokenKind::FuncDef)?;

		let heap_func = if self.at(&TokenKind::Bang) {
			self.next(); // !
			true
		} else {
			false
		};

		let name: Vec<Ident> = if matches!(self.peek_kind(), TokenKind::Identifier(_)) {
			self.get_path()?
		} else {
			let tok: Token = self.next();
			return Err(ParseError {
				span: tok.span,
				message: tok.format_error(self.source, &format!("Expected an identefier, got: {:?}", tok.kind)),
			});
		};

		let generics: Vec<Ident> = if self.at(&TokenKind::LessThan) {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let params: Vec<Param> = self.parse_function_arguments()?;

		let return_type = if self.at(&TokenKind::Arrow) {
			self.next(); // ->
			Some(self.parse_type()?)
		} else {
			None
		};

		let where_clause: Vec<WhereConstraint> = if self.at(&TokenKind::Where) {
			todo!("parse where constraint is not made, this does not work for now")
		} else {
			Vec::new()
		};

		return Ok(Spanned {
			span: span.merge(&self.last_span),
			node: FunctionSignature {
				modifiers,
				name,
				generics,
				params,
				return_type,
				where_clause,
				heap_func,
			},
		});
	}

	fn parse_function_arguments(&mut self) -> Result<Vec<Param>, ParseError>
	{
		self.expect(&TokenKind::LeftParen)?;

		let mut params: Vec<Param> = Vec::new();

		if self.at(&TokenKind::RightParen) {
			self.next();
			return Ok(params);
		}

		loop {
			if self.at(&TokenKind::Ampersand) {
				self.next(); // &

				let mutable = self.consume(&TokenKind::Mut);

				self.expect(&TokenKind::SelfKw)?;

				let self_type = Type {
					modifiers: Vec::new(),
					core: Box::new(TypeCore::Reference {
						mutable,
						inner: Box::new(TypeCore::Base {
							path: vec!["Self".to_string()],
							generics: Vec::new(),
						}),
					}),
				};

				params.push(Param {
					ty: self_type,
					name: "self".to_string(),
				});
			} else if self.at(&TokenKind::SelfKw) {
				self.next(); // self

				let self_type = Type {
					modifiers: Vec::new(),
					core: Box::new(TypeCore::Base {
						path: vec!["Self".to_string()],
						generics: Vec::new(),
					}),
				};

				params.push(Param {
					ty: self_type,
					name: "self".to_string(),
				});
			} else {
				let name_tok = self.next();
				let name = if let TokenKind::Identifier(str) = name_tok.kind {
					str
				} else {
					return Err(ParseError {
						span: name_tok.span,
						message: name_tok
							.format_error(self.source, &format!("expected identefier, got: {:?}", name_tok.kind)),
					});
				};

				self.expect(&TokenKind::Colon)?;

				let ty = self.parse_type()?;

				params.push(Param { ty, name });
			}

			if !self.consume(&TokenKind::Comma) {
				break;
			}

			if self.at(&TokenKind::RightParen) {
				break;
			}
		}

		self.expect(&TokenKind::RightParen)?;
		Ok(params)
	}

	fn parse_modifiers(&mut self) -> Result<Vec<Modifier>, ParseError>
	{
		let mut ret: Vec<Modifier> = Vec::new();

		loop {
			let tok: &Token = self.peek();
			match &tok.kind {
				TokenKind::Directive(_) => ret.push(Modifier::Directive(self.parse_directive()?.node)),
				TokenKind::Pub => ret.push(Modifier::Pub),
				TokenKind::Unsafe => ret.push(Modifier::Unsafe),
				TokenKind::Inline => ret.push(Modifier::Inline),
				TokenKind::Const => ret.push(Modifier::Const),
				_ => return Ok(ret),
			}
			self.next();
		}
	}

	fn parse_struct(&mut self) -> Result<Spanned<StructDecl>, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Struct)?;

		let name: Vec<Ident> = if matches!(self.peek_kind(), TokenKind::Identifier(_)) {
			self.get_path()?
		} else {
			let tok: Token = self.next();
			return Err(ParseError {
				span: tok.span,
				message: tok.format_error(self.source, &format!("expected identifier, got: {:?}", tok.kind)),
			});
		};

		self.expect(&TokenKind::LeftBrace)?;

		let mut fields: Vec<(Type, Ident)> = Vec::new();

		while !self.at(&TokenKind::RightBrace) {
			if *self.peek_kind() == TokenKind::RightBrace {
				break;
			}
			let field_name: Ident = if let TokenKind::Identifier(str) = self.next().kind {
				str
			} else {
				let tok: Token = self.next();
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, &format!("expected identifier, got: {:?}", tok.kind)),
				});
			};

			self.expect(&TokenKind::Colon)?;

			let field_type: Type = self.parse_type()?;

			fields.push((field_type, field_name));

			if *self.peek_kind() == TokenKind::RightBrace {
				break;
			}
			self.expect(&TokenKind::Comma)?;
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(Spanned {
			node: StructDecl {
				modifiers,
				name,
				fields,
			},
			span: span.merge(&self.last_span),
		});
	}

	fn parse_union(&mut self) -> Result<Spanned<UnionDecl>, ParseError>
	{
		let struct_decl = self.parse_struct()?;

		let union_decl: Spanned<UnionDecl> = struct_decl;

		return Ok(union_decl);
	}
}
