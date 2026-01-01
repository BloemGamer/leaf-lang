use std::iter::Peekable;

use crate::Config;
use crate::lexer::{self, Lexer, Span, Token, TokenKind};

/// Recursive descent parser for the programming language.
///
/// The parser performs syntactic analysis by consuming tokens from a lexer
/// and building an Abstract Syntax Tree (AST). It uses recursive descent parsing
/// with operator precedence climbing for expressions.
///
/// # Lifetimes
/// * `'source` - Lifetime of the source code string being parsed
/// * `'config` - Lifetime of the configuration object
///
/// # Example
/// ```
/// let config = Config::default();
/// let source = "fn main() { let x = 42; }";
/// let lexer = Lexer::new(&config, source);
/// let mut parser = Parser::from(lexer);
/// let program = parser.parse_program()?;
/// ```
#[derive(Debug, Clone)]
pub struct Parser<'source, 'config>
{
	#[allow(unused)]
	config: &'config Config,
	source: &'source str,
	lexer: Peekable<Lexer<'source, 'config>>,
	last_span: Span,
	buffered_token: Option<Token>,
}

impl<'s, 'c> From<Lexer<'s, 'c>> for Parser<'s, 'c>
{
	/// Creates a parser from a lexer.
	///
	/// This is the primary way to construct a parser. It extracts the configuration
	/// and source from the lexer and wraps the lexer in a peekable iterator for
	/// lookahead capabilities.
	///
	/// # Arguments
	/// * `lexer` - The lexer to consume tokens from
	///
	/// # Returns
	/// A new `Parser` instance ready to parse the token stream
	///
	/// # Example
	/// ```
	/// let lexer = Lexer::new(&config, source);
	/// let mut parser = Parser::from(lexer);
	/// ```
	fn from(lexer: Lexer<'s, 'c>) -> Self
	{
		let (config, source, lexer) = lexer.into_parts();
		Self {
			config,
			source,
			lexer: lexer.peekable(),
			last_span: Span::default(),
			buffered_token: None,
		}
	}
}

/// Identifier type alias for clearer code semantics.
///
/// Represents variable names, function names, type names, and other identifiers
/// throughout the AST.
pub type Ident = String;

/// A node in the AST with its source location information.
///
/// Wraps any AST node type with span information for error reporting and
/// source mapping.
///
/// # Type Parameters
/// * `T` - The type of the AST node being wrapped
///
/// # Fields
/// * `node` - The actual AST node
/// * `span` - Source location information
#[derive(Debug, Clone)]
pub struct Spanned<T>
{
	pub node: T,
	pub span: Span,
}

impl<T> Spanned<T>
{
	/// Unpacks a spanned node into its components.
	///
	/// # Returns
	/// A tuple containing the node and its span
	///
	/// # Example
	/// ```
	/// let (decl, span) = spanned_decl.unpack();
	/// ```
	fn unpack(self) -> (T, Span)
	{
		return (self.node, self.span);
	}
}

/// The root node of the Abstract Syntax Tree.
///
/// Represents a complete program or compilation unit as a sequence of
/// top-level declarations.
///
/// # Fields
/// * `items` - List of top-level declarations (functions, structs, traits, etc.)
#[derive(Debug, Clone)]
pub struct Program
{
	pub items: Vec<Spanned<TopLevelDecl>>,
}

// Add this implementation block after the existing From<Lexer> implementation
// in your parser.rs file (around line 68)

impl<'s, 'c> From<Parser<'s, 'c>> for Result<Program, ParseError>
{
	/// Converts a parser into a parsed program result.
	///
	/// This provides a convenient way to parse a complete program by consuming
	/// the parser. It calls `parse_program()` internally.
	///
	/// # Arguments
	/// * `parser` - The parser to consume
	///
	/// # Returns
	/// * `Ok(Program)` - The successfully parsed program AST
	/// * `Err(ParseError)` - If a syntax error is encountered during parsing
	///
	/// # Example
	/// ```
	/// use parser::{Parser, Program, ParseError};
	/// use lexer::Lexer;
	/// use config::Config;
	///
	/// let config = Config::default();
	/// let source = "fn main() { let x = 42; }";
	/// let lexer = Lexer::new(&config, source);
	/// let parser = Parser::from(lexer);
	///
	/// // Convert parser to Result<Program, ParseError>
	/// let program: Result<Program, ParseError> = parser.into();
	///
	/// // Or more idiomatically in a single chain:
	/// let program = Parser::from(Lexer::new(&config, source)).into();
	/// ```
	///
	/// # Usage Pattern
	/// This enables a clean one-shot parsing pattern:
	/// ```
	/// fn parse(config: &Config, source: &str) -> Result<Program, ParseError> {
	///     Parser::from(Lexer::new(config, source)).into()
	/// }
	/// ```
	fn from(mut parser: Parser<'s, 'c>) -> Self
	{
		parser.parse_program()
	}
}

impl<'s, 'c> From<Lexer<'s, 'c>> for Result<Program, ParseError>
{
	fn from(value: Lexer<'s, 'c>) -> Self
	{
		<Result<Program, ParseError>>::from(Parser::from(value))
	}
}

/// Type alias for top-level blocks (same structure as Program).
///
/// Used in contexts like namespaces where a block of top-level declarations
/// is needed.
pub type TopLevelBlock = Program;

/// Top-level declaration types.
///
/// Represents all possible declarations that can appear at the top level
/// of a source file or within a namespace.
///
/// # Variants
/// * `Function` - Function definition
/// * `VariableDecl` - Global variable declaration
/// * `Struct` - Structure type definition
/// * `Union` - Untagged union definition
/// * `Enum` - C-style enumeration
/// * `Variant` - Tagged union (Rust-style enum)
/// * `TypeAlias` - Type alias declaration
/// * `Trait` - Trait definition
/// * `Namespace` - Namespace/module declaration
/// * `Impl` - Implementation block
/// * `Directive` - Compiler directive
#[derive(Debug, Clone)]
pub enum TopLevelDecl
{
	Function(FunctionDecl),
	VariableDecl(VariableDecl),
	Struct(StructDecl),
	Union(UnionDecl),
	Enum(EnumDecl),
	Variant(VariantDecl),
	TypeAlias(TypeAliasDecl),
	Trait(TraitDecl),
	Namespace(NamespaceDecl),
	Impl(ImplDecl),
	Directive(Directive),
}

/// Internal enum for distinguishing declaration kinds during parsing.
///
/// Used by the parser to determine what kind of declaration to parse
/// based on lookahead tokens.
#[derive(Debug, Clone, Copy)]
enum DeclKind
{
	Function,
	Struct,
	Union,
	Enum,
	Variant,
	Trait,
	Impl,
	TypeAlias,
	Namespace,
	Variable,
	Directive,
}

/// Modifier keywords that can appear on declarations.
///
/// Represents visibility, safety, and optimization modifiers that can
/// be applied to various declarations.
///
/// # Variants
/// * `Pub` - Public visibility
/// * `Unsafe` - Unsafe code marker
/// * `Inline` - Inline optimization hint
/// * `Const` - Constant function (not used for variables)
/// * `Volatile` - Volatile memory access
/// * `Directive` - Custom compiler directive
#[derive(Debug, Clone)]
pub enum Modifier
{
	Pub,
	Unsafe,
	Inline,
	Const, // for variables this one is not used, for functions it is
	Volatile,
	Directive(Directive),
}

/// Compiler directive types.
///
/// Represents directives that provide instructions to the compiler,
/// such as imports and custom attributes.
///
/// # Variants
/// * `Import` - Import a file: `@import "file.rs"`
/// * `Use` - Use a module path: `@use std::vec`
/// * `Custom` - Custom directive with name and arguments
#[derive(Debug, Clone)]
pub enum Directive
{
	Import(String),
	Use(Vec<Ident>),
	Custom
	{
		name: Ident,
		args: Vec<Expr>,
	},
}

/// Function declaration.
///
/// Represents a complete function including its signature and optional body.
/// Functions without bodies are prototypes (typically for external functions).
///
/// # Fields
/// * `signature` - Function signature (name, parameters, return type, etc.)
/// * `body` - Optional function body (None for prototypes)
#[derive(Debug, Clone)]
pub struct FunctionDecl
{
	pub signature: FunctionSignature,
	pub body: Option<Block>, // Should be none for function prototypes, used for traits and external things
}

/// Function signature.
///
/// Contains all the metadata about a function except its body.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Qualified function name (can include namespace path)
/// * `generics` - Generic type parameters
/// * `params` - Function parameters
/// * `return_type` - Optional return type (None means void/unit)
/// * `where_clause` - Generic constraints
/// * `heap_func` - Whether this is a heap-allocated function (`fn!`)
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

/// Function parameter.
///
/// Represents a single parameter in a function signature.
///
/// # Fields
/// * `ty` - Parameter type
/// * `name` - Parameter name (can be qualified path)
#[derive(Debug, Clone)]
pub struct Param
{
	pub ty: Type,
	pub name: Vec<Ident>,
}

/// Type expression.
///
/// Represents a type in the type system, including modifiers.
///
/// # Fields
/// * `modifiers` - Type modifiers (const, volatile, etc.)
/// * `core` - The core type expression
#[derive(Debug, Clone)]
pub struct Type
{
	pub modifiers: Vec<Modifier>,
	pub core: Box<TypeCore>,
}

/// Core type expressions.
///
/// Represents the fundamental type constructs in the language.
///
/// # Variants
/// * `Base` - Named type with optional generic arguments
/// * `Reference` - Reference type (`&T` or `&mut T`)
/// * `Pointer` - Raw pointer type (`T*`)
/// * `Array` - Array type (`T[?]`)
/// * `Tuple` - Tuple type (`(T1, T2, ...)`)
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

/// Range expression representation.
///
/// Represents range literals like `1..10` or `1..=10`.
///
/// # Fields
/// * `start` - Optional start of range
/// * `end` - Optional end of range
/// * `inclusive` - Whether the range is inclusive (`..=`) or exclusive (`..`)
#[derive(Debug, Clone)]
pub struct RangeExpr
{
	start: Option<Box<Expr>>,
	end: Option<Box<Expr>>,
	inclusive: bool,
}

/// Expression node.
///
/// Represents all possible expression types in the language.
/// Expressions are constructs that evaluate to a value.
///
/// # Variants
/// * `Identifier` - Variable or constant reference
/// * `Literal` - Literal value (integer, string, etc.)
/// * `Unary` - Unary operation (negation, dereference, etc.)
/// * `Binary` - Binary operation (addition, comparison, etc.)
/// * `Cast` - Type cast expression
/// * `Call` - Function call
/// * `Field` - Field access
/// * `Index` - Array/slice indexing
/// * `Range` - Range expression
/// * `Tuple` - Tuple literal
/// * `Array` - Array literal
/// * `StructInit` - Struct initialization
/// * `Block` - Block expression
/// * `Match` - Pattern matching expression
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

/// Literal value types.
///
/// Represents constant literal values in the source code.
///
/// # Variants
/// * `Int` - Integer literal
/// * `Float` - Floating-point literal
/// * `Bool` - Boolean literal
/// * `String` - String literal
/// * `Char` - Character literal
#[derive(Debug, Clone)]
pub enum Literal
{
	Int(i64),
	Float(f64),
	Bool(bool),
	String(String),
	Char(char),
}

/// Array literal types.
///
/// Represents the two forms of array literals in the language.
///
/// # Variants
/// * `List` - Explicit element list: `[1, 2, 3]`
/// * `Repeat` - Repeated value: `[0; 10]`
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

/// Unary operator types.
///
/// Operators that take a single operand.
///
/// # Variants
/// * `Neg` - Numeric negation: `-x`
/// * `Not` - Logical/bitwise NOT: `!x`
/// * `Deref` - Pointer dereference: `*ptr`
/// * `Addr` - Address-of operator: `&x` or `&mut x`
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

/// Binary operator types.
///
/// Operators that take two operands.
///
/// # Variants
/// Logical operators:
/// * `LogicalOr` - `||`
/// * `LogicalAnd` - `&&`
///
/// Comparison operators:
/// * `Eq` - `==`
/// * `Ne` - `!=`
/// * `Lt` - `<`
/// * `Gt` - `>`
/// * `Le` - `<=`
/// * `Ge` - `>=`
///
/// Arithmetic operators:
/// * `Add` - `+`
/// * `Sub` - `-`
/// * `Mul` - `*`
/// * `Div` - `/`
/// * `Mod` - `%`
///
/// Bitwise operators:
/// * `BitAnd` - `&`
/// * `BitOr` - `|`
/// * `BitXor` - `^`
/// * `Shl` - `<<`
/// * `Shr` - `>>`
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

/// Assignment operator types.
///
/// Operators that perform assignment with optional operation.
///
/// # Variants
/// * `Assign` - Simple assignment: `=`
/// * `AddAssign` - Add and assign: `+=`
/// * `SubAssign` - Subtract and assign: `-=`
/// * `MulAssign` - Multiply and assign: `*=`
/// * `DivAssign` - Divide and assign: `/=`
/// * `ModAssign` - Modulo and assign: `%=`
/// * `AndAssign` - Bitwise AND and assign: `&=`
/// * `OrAssign` - Bitwise OR and assign: `|=`
/// * `XorAssign` - Bitwise XOR and assign: `^=`
/// * `ShlAssign` - Left shift and assign: `<<=`
/// * `ShrAssign` - Right shift and assign: `>>=`
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

/// Variable declaration.
///
/// Represents a variable binding, either mutable or immutable.
///
/// # Fields
/// * `ty` - Variable type
/// * `name` - Variable name (can be qualified path)
/// * `init` - Optional initializer expression
/// * `comp_const` - Whether this is a compile-time constant (`const` vs `let`)
#[derive(Debug, Clone)]
pub struct VariableDecl
{
	ty: Type,
	name: Vec<Ident>,
	init: Option<Expr>,
	comp_const: bool,
}

/// Statement types.
///
/// Represents executable statements that don't necessarily produce a value.
///
/// # Variants
/// * `VariableDecl` - Local variable declaration
/// * `Assignment` - Assignment to a variable or location
/// * `Return` - Return from function
/// * `Expr` - Expression statement
/// * `Break` - Break from loop
/// * `Continue` - Continue to next loop iteration
/// * `If` - Conditional statement
/// * `IfLet` - Pattern matching conditional statement
/// * `While` - While loop
/// * `WhileLetLoop` - Pattern matching while loop
/// * `For` - For-in loop
/// * `Unsafe` - Unsafe block
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

	Break,
	Continue,

	If
	{
		cond: Expr,
		then_block: Block,
		else_branch: Option<Box<Stmt>>,
	},

	IfLet
	{
		pattern: Pattern,
		expr: Expr,
		then_block: Block,
		else_branch: Option<Box<Stmt>>,
	},

	While
	{
		cond: Expr,
		body: Block,
	},

	WhileLetLoop
	{
		pattern: Pattern,
		expr: Expr,
		body: Block,
	},

	For
	{
		name: Vec<Ident>,
		iter: Expr,
		body: Block,
	},

	Delete(Vec<Ident>),

	Unsafe(Block),
}

/// Block of statements with optional tail expression.
///
/// Represents a sequence of statements that can optionally evaluate to a value
/// (the tail expression). This is the primary scoping construct.
///
/// # Fields
/// * `stmts` - List of statements in the block
/// * `tail_expr` - Optional final expression (the block's value)
///
/// # Example
/// ```
/// // Block with tail expression:
/// {
///     let x = 5;
///     x + 1  // tail expression, block evaluates to 6
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Block
{
	pub stmts: Vec<Stmt>,
	pub tail_expr: Option<Box<Expr>>,
}

/// Block content types.
///
/// Used to distinguish between regular blocks and top-level blocks.
///
/// # Variants
/// * `Block` - Regular block with statements
/// * `TopLevelBlock` - Top-level declarations block
#[derive(Debug, Clone)]
pub enum BlockContent
{
	Block(Block),
	TopLevelBlock(TopLevelBlock),
}

/// Match expression arm.
///
/// Represents a single arm in a match expression.
///
/// # Fields
/// * `pattern` - Pattern to match against
/// * `body` - Code to execute if pattern matches
#[derive(Debug, Clone)]
pub struct MatchArm
{
	pub pattern: Pattern,
	pub body: MatchBody,
}

/// Match arm body types.
///
/// The body of a match arm can be either a single expression or a block.
///
/// # Variants
/// * `Expr` - Single expression (requires comma)
/// * `Block` - Block of statements
#[derive(Debug, Clone)]
pub enum MatchBody
{
	Expr(Expr),
	Block(Block),
}

/// Pattern matching patterns.
///
/// Represents patterns that can appear in match expressions and if let/while let.
///
/// # Variants
/// * `Wildcard` - Catch-all pattern: `_`
/// * `Literal` - Literal value pattern
/// * `TypedIdentifier` - Bind to identifier with explicit type: `x: i32`
/// * `Variant` - Enum variant pattern with optional destructuring
/// * `Tuple` - Tuple pattern
/// * `Struct` - Struct pattern with field matching
/// * `Range` - Range pattern
/// * `Or` - Or pattern: `pat1 | pat2`
#[derive(Debug, Clone)]
pub enum Pattern
{
	Wildcard,
	Literal(Literal),
	TypedIdentifier
	{
		name: Ident,
		ty: Type,
	},
	Variant
	{
		path: Vec<Ident>,
		args: Vec<Pattern>,
	},
	Tuple(Vec<Pattern>),
	Struct
	{
		path: Vec<Ident>,
		fields: Vec<(Ident, Pattern)>,
	},
	Range(RangeExpr),
	Or(Vec<Pattern>),
}

/// Structure type declaration.
///
/// Represents a struct with named fields.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Struct name (can be qualified path)
/// * `fields` - List of (type, name) pairs for fields
#[derive(Debug, Clone)]
pub struct StructDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub fields: Vec<(Type, Ident)>,
}

/// Type alias for union declarations.
///
/// Unions have the same structure as structs but different semantics
pub type UnionDecl = StructDecl;

/// C-style enumeration declaration.
///
/// Represents an enum where variants are integer constants.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Enum name (can be qualified path)
/// * `variants` - List of (name, optional value) pairs
#[derive(Debug, Clone)]
pub struct EnumDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub variants: Vec<(Ident, Option<Expr>)>,
}

/// Tagged union (Rust-style enum) declaration.
///
/// Represents an enum where variants can carry data.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Variant name (can be qualified path)
/// * `variants` - List of (optional type, name) pairs for variants
#[derive(Debug, Clone)]
pub struct VariantDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub variants: Vec<(Option<Type>, Ident)>,
}

/// Trait declaration.
///
/// Represents a trait (interface) definition.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Trait name (can be qualified path)
/// * `generics` - Generic type parameters
/// * `super_traits` - Traits that this trait extends
/// * `items` - Associated items (functions, types, constants)
#[derive(Debug, Clone)]
pub struct TraitDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub generics: Vec<Ident>,
	pub super_traits: Vec<Vec<Ident>>,
	pub items: Vec<Spanned<TraitItem>>,
}

/// Trait item types.
///
/// Items that can appear in a trait definition.
///
/// # Variants
/// * `Function` - Method signature with optional default implementation
/// * `TypeAlias` - Associated type
/// * `Const` - Associated constant
#[derive(Debug, Clone)]
pub enum TraitItem
{
	Function(FunctionSignature, Option<Block>),
	TypeAlias(TypeAliasDecl),
	Const(VariableDecl),
}

/// Implementation block declaration.
///
/// Represents an `impl` block for either inherent implementations or
/// trait implementations.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `generics` - Generic type parameters
/// * `target` - Type being implemented for
/// * `trait_path` - Optional trait being implemented (None for inherent impl)
/// * `where_clause` - Generic constraints
/// * `body` - Implementation items
#[derive(Debug, Clone)]
pub struct ImplDecl
{
	pub modifiers: Vec<Modifier>,
	pub generics: Vec<Ident>,
	pub target: ImplTarget,
	pub trait_path: Option<ImplTarget>,
	pub where_clause: Vec<WhereConstraint>,
	pub body: Vec<Spanned<ImplItem>>,
}

/// Implementation target type.
///
/// Specifies what type an implementation applies to.
///
/// # Fields
/// * `path` - Type path
/// * `generics` - Generic arguments
#[derive(Debug, Clone)]
pub struct ImplTarget
{
	pub path: Vec<Ident>,
	pub generics: Vec<Type>,
}

/// Implementation block item types.
///
/// Items that can appear in an impl block.
///
/// # Variants
/// * `Function` - Method implementation
/// * `TypeAlias` - Associated type definition
/// * `Const` - Associated constant definition
#[derive(Debug, Clone)]
pub enum ImplItem
{
	Function(FunctionDecl),
	TypeAlias(TypeAliasDecl),
	Const(VariableDecl),
}

/// Generic type constraint (where clause).
///
/// Represents a constraint like `T: Trait1 + Trait2`.
///
/// # Fields
/// * `ty` - Type being constrained
/// * `bounds` - List of trait bounds
#[derive(Debug, Clone)]
pub struct WhereConstraint
{
	pub ty: Vec<Ident>,
	pub bounds: Vec<Vec<Ident>>,
}

/// Parse error type.
///
/// Represents an error encountered during parsing with location information.
///
/// # Fields
/// * `span` - Location of the error in the source
/// * `message` - Human-readable error message
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

/// Type alias declaration.
///
/// Represents a type alias like `type Int = i32;`.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Alias name (can be qualified path)
/// * `ty` - Type being aliased
#[derive(Debug, Clone)]
pub struct TypeAliasDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub ty: Type,
}

/// Namespace declaration.
///
/// Represents a namespace/module containing top-level declarations.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Namespace name (can be qualified path)
/// * `body` - Declarations within the namespace
#[derive(Debug, Clone)]
pub struct NamespaceDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub body: TopLevelBlock,
}

impl<'s, 'c> Parser<'s, 'c>
{
	fn peek(&mut self) -> &Token
	{
		if self.buffered_token.is_some() {
			return self.buffered_token.as_ref().unwrap();
		}
		self.lexer.peek().expect("lexer exhausted unexpectedly")
	}

	fn next(&mut self) -> Token
	{
		if let Some(tok) = self.buffered_token.take() {
			self.last_span = tok.span;
			return tok;
		}
		let tok: Token = self.lexer.next().expect("lexer exhausted unexpectedly");
		self.last_span = tok.span;
		tok
	}

	fn peek_kind(&mut self) -> &TokenKind
	{
		&self.peek().kind
	}

	fn at(&mut self, kind: &TokenKind) -> bool
	{
		self.peek_kind() == kind
	}

	fn consume(&mut self, kind: &TokenKind) -> bool
	{
		if self.at(kind) {
			self.next();
			true
		} else {
			false
		}
	}

	fn consume_greater_than(&mut self) -> bool
	{
		if self.at(&TokenKind::GreaterThan) {
			self.next();
			return true;
		}

		if self.at(&TokenKind::RShift) {
			let rshift_tok = self.next();

			let virtual_gt = Token {
				kind: TokenKind::GreaterThan,
				span: rshift_tok.span,
			};

			self.buffered_token = Some(virtual_gt);

			return true;
		}

		false
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

	/// Parse a complete program.
	///
	/// Entry point for parsing a source file. Parses all top-level declarations
	/// until EOF is reached.
	///
	/// # Returns
	/// * `Ok(Program)` - The parsed program AST
	/// * `Err(ParseError)` - If a syntax error is encountered
	///
	/// # Example
	/// ```
	/// let program = parser.parse_program()?;
	/// println!("Parsed {} items", program.items.len());
	/// ```
	pub fn parse_program(&mut self) -> Result<Program, ParseError>
	{
		let mut items: Vec<Spanned<TopLevelDecl>> = Vec::new();

		while !matches!(self.peek().kind, TokenKind::Eof | TokenKind::RightBrace) {
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
				let (struct_decl, span): (StructDecl, Span) = self.parse_struct()?.unpack();

				(TopLevelDecl::Struct(struct_decl), span)
			}
			DeclKind::Union => {
				let (union_decl, span): (UnionDecl, Span) = self.parse_union()?.unpack();

				(TopLevelDecl::Union(union_decl), span)
			}
			DeclKind::TypeAlias => {
				let (type_alias, span): (TypeAliasDecl, Span) = self.parse_type_alias()?.unpack();
				self.expect(&TokenKind::Semicolon)?;
				(TopLevelDecl::TypeAlias(type_alias), span)
			}
			DeclKind::Namespace => {
				let (namespace_decl, span): (NamespaceDecl, Span) = self.parse_namespace()?.unpack();

				(TopLevelDecl::Namespace(namespace_decl), span)
			}
			DeclKind::Impl => {
				let (impl_decl, span): (ImplDecl, Span) = self.parse_impl()?.unpack();

				(TopLevelDecl::Impl(impl_decl), span)
			}
			DeclKind::Trait => {
				let (trait_decl, span): (TraitDecl, Span) = self.parse_trait()?.unpack();

				(TopLevelDecl::Trait(trait_decl), span)
			}
			DeclKind::Enum => {
				let (enum_decl, span): (EnumDecl, Span) = self.parse_enum()?.unpack();

				(TopLevelDecl::Enum(enum_decl), span)
			}
			DeclKind::Variant => {
				let (tagged_union_decl, span): (VariantDecl, Span) = self.parse_variant()?.unpack();

				(TopLevelDecl::Variant(tagged_union_decl), span)
			} // other => todo!("not yet implemented: {:?}", other),
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
				TokenKind::Union => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Union);
				}
				TokenKind::Enum => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Enum);
				}
				TokenKind::Type => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::TypeAlias);
				}
				TokenKind::Variant => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Variant);
				}
				TokenKind::Let => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Variable);
				}
				TokenKind::Namespace => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Namespace);
				}
				TokenKind::Impl => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Impl);
				}
				TokenKind::Trait => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Trait);
				}
				_ => {
					let tok = self.peek().clone();
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Err(ParseError {
						span: tok.span,
						message: tok.format_error(
							self.source,
							&format!("unexpected token in declaration: got {:?}", tok.kind),
						),
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
			lexer::Directive::Custom(name) => {
				let args: Vec<Expr> = if self.at(&TokenKind::LeftParen) {
					let args: Vec<Expr> = self.parse_argument_list()?;
					self.expect(&TokenKind::RightParen)?;
					args
				} else {
					Vec::new()
				};
				Ok(Directive::Custom { name, args })
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
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
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
				let path = self.get_path()?;
				let generics = if self.at(&TokenKind::LessThan) {
					self.parse_type_generics()?
				} else {
					Vec::new()
				};

				return Ok(TypeCore::Base { path, generics });
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
					message: tok.format_error(self.source, "Expected an ampersand, mut or identifier"),
				});
			}
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
					let size_expr = self.parse_expr()?;
					self.expect(&TokenKind::RightBracket)?; // consume ']'
					base = TypeCore::Array {
						inner: Box::new(base),
						size: Box::new(size_expr),
					};
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
						message: tok.format_error(
							self.source,
							&format!("expected identifier in path, got: {:?}", tok.kind),
						),
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
		if !self.consume(&TokenKind::LessThan) {
			return Ok(Vec::new());
		}

		let mut generics: Vec<Ident> = Vec::new();

		if self.consume_greater_than() {
			return Ok(generics);
		}

		loop {
			let tok = self.next();
			match tok.kind {
				TokenKind::Identifier(name) => {
					generics.push(name);
				}
				_ => {
					return Err(ParseError {
						span: tok.span,
						message: tok.format_error(
							self.source,
							&format!("expected identifier in generic parameters, got: {:?}", tok.kind),
						),
					});
				}
			}

			if self.consume_greater_than() {
				break;
			}

			if !self.consume(&TokenKind::Comma) {
				let tok = self.peek().clone();
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, "expected ',' or '>' in generic parameters"),
				});
			}

			if self.consume_greater_than() {
				break;
			}
		}

		Ok(generics)
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
					let field_tok: Token = self.next();
					let field_name: Ident = if let TokenKind::Identifier(name) = field_tok.kind {
						name
					} else {
						return Err(ParseError {
							span: field_tok.span,
							message: field_tok
								.format_error(self.source, &format!("expected identifier, got: {:?}", field_tok.kind)),
						});
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
			TokenKind::SelfKw => {
				self.next();
				Ok(Expr::Identifier(vec!["self".to_string()]))
			}

			TokenKind::Identifier(_) => {
				let path: Vec<String> = self.get_path()?;

				if self.at(&TokenKind::LeftBrace) {
					let checkpoint = self.lexer.clone();
					let checkpoint_span = self.last_span;

					self.next(); // {

					let is_struct = self.at(&TokenKind::RightBrace)
						|| (matches!(self.peek_kind(), TokenKind::Identifier(_)) && self.lookahead_for_struct_field());

					self.lexer = checkpoint;
					self.last_span = checkpoint_span;

					if is_struct {
						self.next(); // {
						let fields: Vec<(String, Expr)> = self.parse_struct_fields()?;
						self.expect(&TokenKind::RightBrace)?;
						Ok(Expr::StructInit { path, fields })
					} else {
						Ok(Expr::Identifier(path))
					}
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
				self.next(); // match
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

	fn lookahead_for_struct_field(&mut self) -> bool
	{
		if let TokenKind::Identifier(_) = self.peek_kind() {
			let checkpoint = self.lexer.clone();
			self.next(); // identifier(_)
			let has_equals = self.at(&TokenKind::Equals);
			self.lexer = checkpoint;
			has_equals
		} else {
			false
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
			let name_tok: Token = self.next();
			let name: Ident = if let TokenKind::Identifier(str) = name_tok.kind {
				str
			} else {
				return Err(ParseError {
					span: name_tok.span,
					message: name_tok
						.format_error(self.source, &format!("expected identifier, got: {:?}", name_tok.kind)),
				});
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
		let mut patterns = vec![self.parse_pattern_no_or()?];

		while self.consume(&TokenKind::Pipe) {
			patterns.push(self.parse_pattern_no_or()?);
		}

		if patterns.len() == 1 {
			Ok(patterns.into_iter().next().unwrap())
		} else {
			Ok(Pattern::Or(patterns))
		}
	}

	fn parse_pattern_no_or(&mut self) -> Result<Pattern, ParseError>
	{
		let tok = self.peek().clone();

		match &tok.kind {
			TokenKind::Underscore => {
				self.next();
				if self.consume(&TokenKind::Colon) {
					let _ignored_type = self.parse_type()?;
				}
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
				} else if self.consume(&TokenKind::LeftBrace) {
					let mut fields: Vec<(Ident, Pattern)> = Vec::new();

					if !self.at(&TokenKind::RightBrace) {
						loop {
							let field_name = if let TokenKind::Identifier(name) = self.next().kind {
								name
							} else {
								return Err(ParseError {
									span: self.last_span,
									message: "expected field name in struct pattern".to_string(),
								});
							};

							self.expect(&TokenKind::Colon)?;

							let pattern = self.parse_pattern()?;

							fields.push((field_name, pattern));

							if !self.consume(&TokenKind::Comma) {
								break;
							}
							if self.at(&TokenKind::RightBrace) {
								break;
							}
						}
					}

					self.expect(&TokenKind::RightBrace)?;
					Ok(Pattern::Struct { path, fields })
				} else if self.at(&TokenKind::Colon) {
					if path.len() != 1 {
						return Err(ParseError {
							span: tok.span,
							message: tok
								.format_error(self.source, "binding patterns must be simple identifiers, not paths"),
						});
					}

					self.next(); // :
					let ty = self.parse_type()?;

					Ok(Pattern::TypedIdentifier {
						name: path[0].clone(),
						ty,
					})
				} else {
					Ok(Pattern::Variant { path, args: Vec::new() })
				}
			}

			TokenKind::LeftParen => {
				self.next(); // (

				if self.consume(&TokenKind::RightParen) {
					return Ok(Pattern::Tuple(Vec::new()));
				}

				let mut elements = vec![self.parse_pattern()?];

				if self.consume(&TokenKind::Comma) {
					if !self.at(&TokenKind::RightParen) {
						loop {
							elements.push(self.parse_pattern()?);
							if !self.consume(&TokenKind::Comma) {
								break;
							}
							if self.at(&TokenKind::RightParen) {
								break;
							}
						}
					}
					self.expect(&TokenKind::RightParen)?;
					Ok(Pattern::Tuple(elements))
				} else {
					self.expect(&TokenKind::RightParen)?;
					Ok(elements.into_iter().next().unwrap())
				}
			}

			TokenKind::IntLiteral(n) => {
				self.next();

				if self.at(&TokenKind::DotDot) || self.at(&TokenKind::DotDotEquals) {
					let inclusive = self.at(&TokenKind::DotDotEquals);
					self.next(); // .. | ..=

					let end = if self.is_range_end() {
						None
					} else {
						Some(Box::new(self.parse_expr()?))
					};

					Ok(Pattern::Range(RangeExpr {
						start: Some(Box::new(Expr::Literal(Literal::Int(*n)))),
						end,
						inclusive,
					}))
				} else {
					Ok(Pattern::Literal(Literal::Int(*n)))
				}
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
			let kind = self.peek_kind().clone();

			match kind {
				TokenKind::Let | TokenKind::Const => {
					let var_decl = self.parse_var_decl()?;
					self.expect(&TokenKind::Semicolon)?;
					stmts.push(Stmt::VariableDecl(var_decl.node));
				}

				TokenKind::Return => {
					self.next();
					let ret_expr = if self.at(&TokenKind::Semicolon) {
						None
					} else {
						Some(self.parse_expr()?)
					};
					self.expect(&TokenKind::Semicolon)?;
					stmts.push(Stmt::Return(ret_expr));
				}

				TokenKind::Break => {
					self.next();
					self.expect(&TokenKind::Semicolon)?;
					stmts.push(Stmt::Break);
				}

				TokenKind::Continue => {
					self.next();
					self.expect(&TokenKind::Semicolon)?;
					stmts.push(Stmt::Continue);
				}

				TokenKind::While => {
					let checkpoint: Peekable<Lexer<'s, 'c>> = self.lexer.clone();
					let checkpoint_span: Span = self.last_span;
					let checkpoint_buffered: Option<Token> = self.buffered_token.clone();

					self.next(); // while

					if self.consume(&TokenKind::Let) {
						let pattern: Pattern = self.parse_pattern()?;
						self.expect(&TokenKind::Equals)?;
						let expr: Expr = self.parse_expr()?;
						let body: Block = self.parse_block()?;
						stmts.push(Stmt::WhileLetLoop { pattern, expr, body });
					} else {
						self.lexer = checkpoint;
						self.last_span = checkpoint_span;
						self.buffered_token = checkpoint_buffered;
						stmts.push(self.parse_while()?);
					}
				}

				TokenKind::For => {
					stmts.push(self.parse_for()?);
				}

				TokenKind::If => {
					let checkpoint: Peekable<Lexer<'s, 'c>> = self.lexer.clone();
					let checkpoint_span: Span = self.last_span;
					let checkpoint_buffered: Option<Token> = self.buffered_token.clone();

					self.next(); // if

					if self.consume(&TokenKind::Let) {
						let pattern: Pattern = self.parse_pattern()?;
						self.expect(&TokenKind::Equals)?;
						let expr: Expr = self.parse_expr()?;
						let then_block: Block = self.parse_block()?;

						let else_branch: Option<Box<Stmt>> = if self.consume(&TokenKind::Else) {
							if self.at(&TokenKind::If) {
								Some(Box::new(self.parse_if_or_if_let()?))
							} else {
								let else_block: Block = self.parse_block()?;
								Some(Box::new(Stmt::If {
									cond: Expr::Literal(Literal::Bool(true)),
									then_block: else_block,
									else_branch: None,
								}))
							}
						} else {
							None
						};

						let if_let_stmt = Stmt::IfLet {
							pattern,
							expr,
							then_block,
							else_branch,
						};

						if self.consume(&TokenKind::Semicolon) {
							stmts.push(if_let_stmt);
						} else if self.at(&TokenKind::RightBrace) {
							tail_expr = Some(Box::new(self.stmt_if_to_expr_iflet(if_let_stmt)?));
							break;
						} else {
							stmts.push(if_let_stmt);
						}
					} else {
						self.lexer = checkpoint;
						self.last_span = checkpoint_span;
						self.buffered_token = checkpoint_buffered;

						let if_stmt: Stmt = self.parse_if()?;

						if self.consume(&TokenKind::Semicolon) {
							stmts.push(if_stmt);
						} else if self.at(&TokenKind::RightBrace) {
							tail_expr = Some(Box::new(self.stmt_if_to_expr(if_stmt)?));
							break;
						} else {
							stmts.push(if_stmt);
						}
					}
				}

				TokenKind::Delete => {
					stmts.push(Stmt::Delete(self.parse_delete()?));
				}

				TokenKind::Unsafe => {
					self.next();
					let block: Block = self.parse_block()?;

					if self.consume(&TokenKind::Semicolon) {
						stmts.push(Stmt::Unsafe(block));
					} else if self.at(&TokenKind::RightBrace) {
						tail_expr = Some(Box::new(Expr::Block(Box::new(block))));
						break;
					} else {
						stmts.push(Stmt::Unsafe(block));
					}
				}

				_ => {
					let expr: Expr = self.parse_expr()?;

					if self.is_assignment_op() {
						let op: AssignOp = self.parse_assign_op()?;
						let value: Expr = self.parse_expr()?;
						self.expect(&TokenKind::Semicolon)?;
						stmts.push(Stmt::Assignment {
							target: expr,
							op,
							value,
						});
					} else {
						let needs_semi: bool = self.expr_needs_semicolon(&expr);

						if needs_semi {
							if self.consume(&TokenKind::Semicolon) {
								stmts.push(Stmt::Expr(expr));
							} else if self.at(&TokenKind::RightBrace) {
								tail_expr = Some(Box::new(expr));
								break;
							} else {
								let tok: Token = self.peek().clone();
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
				}
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

	fn parse_if_or_if_let(&mut self) -> Result<Stmt, ParseError>
	{
		self.expect(&TokenKind::If)?;

		if self.consume(&TokenKind::Let) {
			let pattern = self.parse_pattern()?;
			self.expect(&TokenKind::Equals)?;
			let expr = self.parse_expr()?;
			let then_block = self.parse_block()?;

			let else_branch = if self.consume(&TokenKind::Else) {
				if self.at(&TokenKind::If) {
					Some(Box::new(self.parse_if_or_if_let()?))
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

			Ok(Stmt::IfLet {
				pattern,
				expr,
				then_block,
				else_branch,
			})
		} else {
			// Regular if
			self.parse_if()
		}
	}

	fn stmt_if_to_expr_iflet(&self, stmt: Stmt) -> Result<Expr, ParseError>
	{
		match stmt {
			Stmt::IfLet {
				pattern,
				expr,
				then_block,
				else_branch,
			} => Ok(Expr::Block(Box::new(Block {
				stmts: vec![Stmt::IfLet {
					pattern,
					expr,
					then_block,
					else_branch,
				}],
				tail_expr: None,
			}))),
			_ => unreachable!("Expected if let statement"),
		}
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

		let name: Vec<Ident> = self.get_path()?;

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
			match self.peek_kind() {
				TokenKind::Ampersand => {
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
						name: vec!["self".to_string()],
					});
				}
				TokenKind::SelfKw => {
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
						name: vec!["self".to_string()],
					});
				}
				_ => {
					let name = self.get_path()?;

					self.expect(&TokenKind::Colon)?;

					let ty = self.parse_type()?;

					params.push(Param { ty, name });
				}
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
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Union)?;

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
			node: UnionDecl {
				modifiers,
				name,
				fields,
			},
			span: span.merge(&self.last_span),
		});
	}

	fn parse_namespace(&mut self) -> Result<Spanned<NamespaceDecl>, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers = self.parse_modifiers()?;
		self.expect(&TokenKind::Namespace)?;
		let name: Vec<Ident> = self.get_path()?;
		self.expect(&TokenKind::LeftBrace)?;
		let body: TopLevelBlock = self.parse_program()?;
		self.expect(&TokenKind::RightBrace)?;
		return Ok(Spanned {
			node: NamespaceDecl { modifiers, name, body },
			span: span.merge(&self.last_span),
		});
	}

	fn parse_enum(&mut self) -> Result<Spanned<EnumDecl>, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Enum)?;

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

		let mut fields: Vec<(Ident, Option<Expr>)> = Vec::new();

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

			let field_type: Option<Expr> = if self.at(&TokenKind::Equals) {
				self.next();
				Some(self.parse_expr()?)
			} else {
				None
			};

			fields.push((field_name, field_type));

			if *self.peek_kind() == TokenKind::RightBrace {
				break;
			}
			self.expect(&TokenKind::Comma)?;
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(Spanned {
			node: EnumDecl {
				modifiers,
				name,
				variants: fields,
			},
			span: span.merge(&self.last_span),
		});
	}

	fn parse_variant(&mut self) -> Result<Spanned<VariantDecl>, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Variant)?;

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

		let mut fields: Vec<(Option<Type>, Ident)> = Vec::new();

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

			let field_type: Option<Type> = if self.at(&TokenKind::LeftParen) {
				self.next();
				let ty = Some(self.parse_type()?);
				self.expect(&TokenKind::RightParen)?;
				ty
			} else {
				None
			};

			fields.push((field_type, field_name));

			if *self.peek_kind() == TokenKind::RightBrace {
				break;
			}
			self.expect(&TokenKind::Comma)?;
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(Spanned {
			node: VariantDecl {
				modifiers,
				name,
				variants: fields,
			},
			span: span.merge(&self.last_span),
		});
	}

	fn parse_impl(&mut self) -> Result<Spanned<ImplDecl>, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Impl)?;

		let generics: Vec<Ident> = if self.at(&TokenKind::LessThan) {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let first_target: ImplTarget = self.parse_impl_target()?;

		let (trait_path, target) = if self.consume(&TokenKind::For) {
			let target = self.parse_impl_target()?;
			(Some(first_target), target)
		} else {
			(None, first_target)
		};

		let where_clause: Vec<WhereConstraint> = if self.at(&TokenKind::Where) {
			self.next();
			self.parse_where_clause()?
		} else {
			Vec::new()
		};

		self.expect(&TokenKind::LeftBrace)?;

		let mut body: Vec<Spanned<ImplItem>> = Vec::new();

		while !self.at(&TokenKind::RightBrace) {
			let item = self.parse_impl_item()?;
			body.push(item);
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(Spanned {
			node: ImplDecl {
				modifiers,
				generics,
				target,
				trait_path,
				where_clause,
				body,
			},
			span: span.merge(&self.last_span),
		});
	}

	fn parse_impl_target(&mut self) -> Result<ImplTarget, ParseError>
	{
		let path: Vec<Ident> = self.get_path()?;

		let generics: Vec<Type> = if self.at(&TokenKind::LessThan) {
			self.parse_type_generics()?
		} else {
			Vec::new()
		};

		Ok(ImplTarget { path, generics })
	}

	fn parse_type_generics(&mut self) -> Result<Vec<Type>, ParseError>
	{
		if !self.consume(&TokenKind::LessThan) {
			return Ok(Vec::new());
		}

		let mut generics: Vec<Type> = Vec::new();

		if self.consume_greater_than() {
			return Ok(generics);
		}

		loop {
			generics.push(self.parse_type()?);

			if self.consume_greater_than() {
				break;
			}

			if !self.consume(&TokenKind::Comma) {
				let tok = self.peek().clone();
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, "expected ',' or '>' in generic arguments"),
				});
			}

			if self.consume_greater_than() {
				break;
			}
		}

		Ok(generics)
	}

	fn parse_impl_item(&mut self) -> Result<Spanned<ImplItem>, ParseError>
	{
		let span: Span = self.peek().span;

		let decl_kind = self.peek_declaration_kind()?;

		let (node, end_span) = match decl_kind {
			DeclKind::Function => {
				let (func_decl, span) = self.parse_function_decl()?.unpack();
				(ImplItem::Function(func_decl), span)
			}
			DeclKind::TypeAlias => {
				let (type_alias, span) = self.parse_type_alias()?.unpack();
				self.expect(&TokenKind::Semicolon)?;
				(ImplItem::TypeAlias(type_alias), span)
			}
			DeclKind::Variable => {
				let (var_decl, span) = self.parse_var_decl()?.unpack();
				self.expect(&TokenKind::Semicolon)?;
				(ImplItem::Const(var_decl), span)
			}
			_ => {
				let tok = self.peek().clone();
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, &format!("unexpected item in impl block: {:?}", tok.kind)),
				});
			}
		};

		Ok(Spanned {
			node,
			span: span.merge(&end_span),
		})
	}

	fn parse_where_clause(&mut self) -> Result<Vec<WhereConstraint>, ParseError>
	{
		let mut constraints: Vec<WhereConstraint> = Vec::new();

		loop {
			let ty = self.get_path()?;

			self.expect(&TokenKind::Colon)?;

			let mut bounds: Vec<Vec<Ident>> = Vec::new();

			loop {
				let bound = self.get_path()?;
				bounds.push(bound);

				if !self.consume(&TokenKind::Plus) {
					break;
				}
			}

			constraints.push(WhereConstraint { ty, bounds });

			if !self.consume(&TokenKind::Comma) {
				break;
			}

			if self.at(&TokenKind::LeftBrace) {
				break;
			}
		}

		Ok(constraints)
	}

	fn parse_type_alias(&mut self) -> Result<Spanned<TypeAliasDecl>, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Type)?;

		let name = self.get_path()?;

		self.expect(&TokenKind::Equals)?;
		let ty = self.parse_type()?;

		Ok(Spanned {
			node: TypeAliasDecl { modifiers, name, ty },
			span: span.merge(&self.last_span),
		})
	}

	fn parse_trait(&mut self) -> Result<Spanned<TraitDecl>, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Trait)?;

		let name = self.get_path()?;

		let generics: Vec<Ident> = if self.at(&TokenKind::LessThan) {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let super_traits: Vec<Vec<Ident>> = if self.consume(&TokenKind::Colon) {
			self.parse_trait_bounds()?
		} else {
			Vec::new()
		};

		self.expect(&TokenKind::LeftBrace)?;

		let mut items: Vec<Spanned<TraitItem>> = Vec::new();

		while !self.at(&TokenKind::RightBrace) {
			let item = self.parse_trait_item()?;
			items.push(item);
		}

		self.expect(&TokenKind::RightBrace)?;

		Ok(Spanned {
			node: TraitDecl {
				modifiers,
				name,
				generics,
				super_traits,
				items,
			},
			span: span.merge(&self.last_span),
		})
	}

	fn parse_trait_bounds(&mut self) -> Result<Vec<Vec<Ident>>, ParseError>
	{
		let mut bounds: Vec<Vec<Ident>> = Vec::new();

		loop {
			let bound = self.get_path()?;
			bounds.push(bound);

			if !self.consume(&TokenKind::Plus) {
				break;
			}
		}

		Ok(bounds)
	}

	fn parse_trait_item(&mut self) -> Result<Spanned<TraitItem>, ParseError>
	{
		let span: Span = self.peek().span;

		let decl_kind = self.peek_declaration_kind()?;

		let (node, end_span) = match decl_kind {
			DeclKind::Function => {
				let (sig, sig_span) = self.parse_function_signature()?.unpack();

				let body = if self.at(&TokenKind::LeftBrace) {
					Some(self.parse_block()?)
				} else {
					self.expect(&TokenKind::Semicolon)?;
					None
				};

				(TraitItem::Function(sig, body), self.last_span.merge(&sig_span))
			}
			DeclKind::TypeAlias => {
				let (type_alias, span) = self.parse_trait_type_alias()?.unpack();
				self.expect(&TokenKind::Semicolon)?;
				(TraitItem::TypeAlias(type_alias), span)
			}
			DeclKind::Variable => {
				let (var_decl, span) = self.parse_var_decl()?.unpack();
				self.expect(&TokenKind::Semicolon)?;
				(TraitItem::Const(var_decl), span)
			}
			_ => {
				let tok = self.peek().clone();
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, &format!("unexpected item in trait block: {:?}", tok.kind)),
				});
			}
		};

		Ok(Spanned {
			node,
			span: span.merge(&end_span),
		})
	}

	fn parse_trait_type_alias(&mut self) -> Result<Spanned<TypeAliasDecl>, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Type)?;

		let name = self.get_path()?;

		let ty = if self.consume(&TokenKind::Equals) {
			self.parse_type()?
		} else {
			Type {
				modifiers: Vec::new(),
				core: Box::new(TypeCore::Base {
					path: vec!["_".to_string()],
					generics: Vec::new(),
				}),
			}
		};

		Ok(Spanned {
			node: TypeAliasDecl { modifiers, name, ty },
			span: span.merge(&self.last_span),
		})
	}

	fn parse_delete(&mut self) -> Result<Vec<Ident>, ParseError>
	{
		self.expect(&TokenKind::Delete)?;

		return self.get_path();
	}
}

#[cfg(test)]
mod parser_tests
{
	use super::*;
	use crate::Config;
	use crate::lexer::Lexer;

	fn parse_expr_from_str(input: &str) -> Result<Expr, ParseError>
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, input);
		let mut parser = Parser::from(lexer);
		parser.parse_expr()
	}

	fn parse_program_from_str(input: &str) -> Result<Program, ParseError>
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, input);
		let mut parser = Parser::from(lexer);
		parser.parse_program()
	}

	// ========== Literal Tests ==========

	#[test]
	fn test_parse_int_literal()
	{
		let result = parse_expr_from_str("42");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal(Literal::Int(42)) => (),
			_ => panic!("Expected Int literal"),
		}
	}

	#[test]
	fn test_parse_float_literal()
	{
		let result = parse_expr_from_str("3.16");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal(Literal::Float(f)) if (f - 3.16).abs() < 0.001 => (),
			_ => panic!("Expected Float literal"),
		}
	}

	#[test]
	fn test_parse_bool_literal_true()
	{
		let result = parse_expr_from_str("true");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal(Literal::Bool(true)) => (),
			_ => panic!("Expected Bool(true) literal"),
		}
	}

	#[test]
	fn test_parse_bool_literal_false()
	{
		let result = parse_expr_from_str("false");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal(Literal::Bool(false)) => (),
			_ => panic!("Expected Bool(false) literal"),
		}
	}

	#[test]
	fn test_parse_string_literal()
	{
		let result = parse_expr_from_str(r#""hello world""#);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal(Literal::String(s)) if s == "hello world" => (),
			_ => panic!("Expected String literal"),
		}
	}

	#[test]
	fn test_parse_char_literal()
	{
		let result = parse_expr_from_str("'a'");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal(Literal::Char('a')) => (),
			_ => panic!("Expected Char literal"),
		}
	}

	// ========== Identifier Tests ==========

	#[test]
	fn test_parse_simple_identifier()
	{
		let result = parse_expr_from_str("variable");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Identifier(path) if path == vec!["variable"] => (),
			_ => panic!("Expected simple identifier"),
		}
	}

	#[test]
	fn test_parse_path_identifier()
	{
		let result = parse_expr_from_str("std::vec::Vec");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Identifier(path) if path == vec!["std", "vec", "Vec"] => (),
			_ => panic!("Expected path identifier"),
		}
	}

	#[test]
	fn test_parse_self_keyword()
	{
		let result = parse_expr_from_str("self");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Identifier(path) if path == vec!["self"] => (),
			_ => panic!("Expected self identifier"),
		}
	}

	// ========== Binary Operation Tests ==========

	#[test]
	fn test_parse_addition()
	{
		let result = parse_expr_from_str("1 + 2");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Add, .. } => (),
			_ => panic!("Expected addition"),
		}
	}

	#[test]
	fn test_parse_subtraction()
	{
		let result = parse_expr_from_str("5 - 3");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Sub, .. } => (),
			_ => panic!("Expected subtraction"),
		}
	}

	#[test]
	fn test_parse_multiplication()
	{
		let result = parse_expr_from_str("4 * 3");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Mul, .. } => (),
			_ => panic!("Expected multiplication"),
		}
	}

	#[test]
	fn test_parse_division()
	{
		let result = parse_expr_from_str("10 / 2");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Div, .. } => (),
			_ => panic!("Expected division"),
		}
	}

	#[test]
	fn test_parse_modulo()
	{
		let result = parse_expr_from_str("10 % 3");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Mod, .. } => (),
			_ => panic!("Expected modulo"),
		}
	}

	#[test]
	fn test_parse_operator_precedence()
	{
		let result = parse_expr_from_str("1 + 2 * 3");
		assert!(result.is_ok());
		// Should parse as 1 + (2 * 3)
		match result.unwrap() {
			Expr::Binary {
				op: BinaryOp::Add,
				lhs,
				rhs,
			} => {
				match *lhs {
					Expr::Literal(Literal::Int(1)) => (),
					_ => panic!("Expected lhs to be 1"),
				}
				match *rhs {
					Expr::Binary { op: BinaryOp::Mul, .. } => (),
					_ => panic!("Expected rhs to be multiplication"),
				}
			}
			_ => panic!("Expected addition at top level"),
		}
	}

	#[test]
	fn test_parse_comparison_operators()
	{
		let ops = vec![
			("1 < 2", BinaryOp::Lt),
			("1 > 2", BinaryOp::Gt),
			("1 <= 2", BinaryOp::Le),
			("1 >= 2", BinaryOp::Ge),
			("1 == 2", BinaryOp::Eq),
			("1 != 2", BinaryOp::Ne),
		];

		for (input, expected_op) in ops {
			let result = parse_expr_from_str(input);
			assert!(result.is_ok(), "Failed to parse: {}", input);
			match result.unwrap() {
				Expr::Binary { op, .. } => {
					assert!(
						std::mem::discriminant(&op) == std::mem::discriminant(&expected_op),
						"Expected {:?} for input: {}",
						expected_op,
						input
					);
				}
				_ => panic!("Expected binary expression for: {}", input),
			}
		}
	}

	#[test]
	fn test_parse_logical_operators()
	{
		let result = parse_expr_from_str("true && false");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary {
				op: BinaryOp::LogicalAnd,
				..
			} => (),
			_ => panic!("Expected logical AND"),
		}

		let result = parse_expr_from_str("true || false");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary {
				op: BinaryOp::LogicalOr,
				..
			} => (),
			_ => panic!("Expected logical OR"),
		}
	}

	#[test]
	fn test_parse_bitwise_operators()
	{
		let ops = vec![
			("1 & 2", BinaryOp::BitAnd),
			("1 | 2", BinaryOp::BitOr),
			("1 ^ 2", BinaryOp::BitXor),
			("1 << 2", BinaryOp::Shl),
			("1 >> 2", BinaryOp::Shr),
		];

		for (input, expected_op) in ops {
			let result = parse_expr_from_str(input);
			assert!(result.is_ok(), "Failed to parse: {}", input);
			match result.unwrap() {
				Expr::Binary { op, .. } => {
					assert!(
						std::mem::discriminant(&op) == std::mem::discriminant(&expected_op),
						"Expected {:?} for input: {}",
						expected_op,
						input
					);
				}
				_ => panic!("Expected binary expression for: {}", input),
			}
		}
	}

	// ========== Unary Operation Tests ==========

	#[test]
	fn test_parse_negation()
	{
		let result = parse_expr_from_str("-5");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Unary { op: UnaryOp::Neg, .. } => (),
			_ => panic!("Expected negation"),
		}
	}

	#[test]
	fn test_parse_logical_not()
	{
		let result = parse_expr_from_str("!true");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Unary { op: UnaryOp::Not, .. } => (),
			_ => panic!("Expected logical NOT"),
		}
	}

	#[test]
	fn test_parse_dereference()
	{
		let result = parse_expr_from_str("*ptr");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Unary { op: UnaryOp::Deref, .. } => (),
			_ => panic!("Expected dereference"),
		}
	}

	#[test]
	fn test_parse_address_of()
	{
		let result = parse_expr_from_str("&x");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Unary {
				op: UnaryOp::Addr { mutable: false },
				..
			} => (),
			_ => panic!("Expected address-of"),
		}
	}

	#[test]
	fn test_parse_mutable_address_of()
	{
		let result = parse_expr_from_str("&mut x");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Unary {
				op: UnaryOp::Addr { mutable: true },
				..
			} => (),
			_ => panic!("Expected mutable address-of"),
		}
	}

	// ========== Cast Tests ==========

	#[test]
	fn test_parse_cast()
	{
		let result = parse_expr_from_str("(i32)42");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Cast { ty, expr } => {
				match *ty.core {
					TypeCore::Base { ref path, .. } if path == &vec!["i32"] => (),
					_ => panic!("Expected i32 type"),
				}
				match *expr {
					Expr::Literal(Literal::Int(42)) => (),
					_ => panic!("Expected 42 literal"),
				}
			}
			_ => panic!("Expected cast expression"),
		}
	}

	// ========== Range Tests ==========

	#[test]
	fn test_parse_exclusive_range()
	{
		let result = parse_expr_from_str("1..10");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: Some(_),
				end: Some(_),
				inclusive: false,
			}) => (),
			_ => panic!("Expected exclusive range"),
		}
	}

	#[test]
	fn test_parse_inclusive_range()
	{
		let result = parse_expr_from_str("1..=10");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: Some(_),
				end: Some(_),
				inclusive: true,
			}) => (),
			_ => panic!("Expected inclusive range"),
		}
	}

	#[test]
	fn test_parse_open_ended_range()
	{
		let result = parse_expr_from_str("1..;");

		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: Some(_),
				end: None,
				inclusive: false,
			}) => (),
			_ => panic!("Expected open-ended range"),
		}
	}

	// ========== Call Tests ==========

	#[test]
	fn test_parse_function_call_no_args()
	{
		let result = parse_expr_from_str("foo()");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { callee, args } => {
				match *callee {
					Expr::Identifier(ref path) if path == &vec!["foo"] => (),
					_ => panic!("Expected foo identifier"),
				}
				assert_eq!(args.len(), 0);
			}
			_ => panic!("Expected function call"),
		}
	}

	#[test]
	fn test_parse_function_call_with_args()
	{
		let result = parse_expr_from_str("foo(1, 2, 3)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { callee, args } => {
				match *callee {
					Expr::Identifier(ref path) if path == &vec!["foo"] => (),
					_ => panic!("Expected foo identifier"),
				}
				assert_eq!(args.len(), 3);
			}
			_ => panic!("Expected function call"),
		}
	}

	// ========== Field Access Tests ==========

	#[test]
	fn test_parse_field_access()
	{
		let result = parse_expr_from_str("obj.field");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Field { base, name } => {
				match *base {
					Expr::Identifier(ref path) if path == &vec!["obj"] => (),
					_ => panic!("Expected obj identifier"),
				}
				assert_eq!(name, "field");
			}
			_ => panic!("Expected field access"),
		}
	}

	#[test]
	fn test_parse_chained_field_access()
	{
		let result = parse_expr_from_str("obj.field1.field2");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Field { base, name } => {
				assert_eq!(name, "field2");
				match *base {
					Expr::Field {
						name: ref inner_name, ..
					} => {
						assert_eq!(inner_name, "field1");
					}
					_ => panic!("Expected nested field access"),
				}
			}
			_ => panic!("Expected field access"),
		}
	}

	// ========== Index Tests ==========

	#[test]
	fn test_parse_array_index()
	{
		let result = parse_expr_from_str("arr[0]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Index { base, index } => {
				match *base {
					Expr::Identifier(ref path) if path == &vec!["arr"] => (),
					_ => panic!("Expected arr identifier"),
				}
				match *index {
					Expr::Literal(Literal::Int(0)) => (),
					_ => panic!("Expected 0 index"),
				}
			}
			_ => panic!("Expected index expression"),
		}
	}

	// ========== Tuple Tests ==========

	#[test]
	fn test_parse_empty_tuple()
	{
		let result = parse_expr_from_str("()");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Tuple(elements) => assert_eq!(elements.len(), 0),
			_ => panic!("Expected empty tuple"),
		}
	}

	#[test]
	fn test_parse_single_element_tuple()
	{
		let result = parse_expr_from_str("(1,)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Tuple(elements) => assert_eq!(elements.len(), 1),
			_ => panic!("Expected single-element tuple"),
		}
	}

	#[test]
	fn test_parse_multi_element_tuple()
	{
		let result = parse_expr_from_str("(1, 2, 3)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Tuple(elements) => assert_eq!(elements.len(), 3),
			_ => panic!("Expected multi-element tuple"),
		}
	}

	#[test]
	fn test_parse_parenthesized_expr()
	{
		let result = parse_expr_from_str("(42)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal(Literal::Int(42)) => (),
			_ => panic!("Expected parenthesized expression to unwrap"),
		}
	}

	// ========== Array Tests ==========

	#[test]
	fn test_parse_empty_array()
	{
		let result = parse_expr_from_str("[]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::List(elements)) => assert_eq!(elements.len(), 0),
			_ => panic!("Expected empty array"),
		}
	}

	#[test]
	fn test_parse_array_list()
	{
		let result = parse_expr_from_str("[1, 2, 3]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::List(elements)) => assert_eq!(elements.len(), 3),
			_ => panic!("Expected array list"),
		}
	}

	#[test]
	fn test_parse_array_repeat()
	{
		let result = parse_expr_from_str("[0; 10]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::Repeat { value, count }) => {
				assert_eq!(value.len(), 1);
				match *count {
					Expr::Literal(Literal::Int(10)) => (),
					_ => panic!("Expected count of 10"),
				}
			}
			_ => panic!("Expected array repeat"),
		}
	}

	// ========== Struct Init Tests ==========

	#[test]
	fn test_parse_struct_init_empty()
	{
		let result = parse_expr_from_str("Point {}").inspect_err(|e| println!("{}", e));
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::StructInit { path, fields } => {
				assert_eq!(path, vec!["Point"]);
				assert_eq!(fields.len(), 0);
			}
			_ => panic!("Expected struct init"),
		}
	}

	#[test]
	fn test_parse_struct_init_with_fields()
	{
		let result = parse_expr_from_str("Point { x = 1, y = 2 }");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::StructInit { path, fields } => {
				assert_eq!(path, vec!["Point"]);
				assert_eq!(fields.len(), 2);
				assert_eq!(fields[0].0, "x");
				assert_eq!(fields[1].0, "y");
			}
			_ => panic!("Expected struct init"),
		}
	}

	// ========== Block Tests ==========

	#[test]
	fn test_parse_empty_block()
	{
		let result = parse_expr_from_str("{}");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Block(block) => {
				assert_eq!(block.stmts.len(), 0);
				assert!(block.tail_expr.is_none());
			}
			_ => panic!("Expected block expression"),
		}
	}

	#[test]
	fn test_parse_block_with_statements()
	{
		let input = "{ let x: i32 = 5; x + 1 }";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Block(block) => {
				assert_eq!(block.stmts.len(), 1);
				assert!(block.tail_expr.is_some());
			}
			_ => panic!("Expected block expression"),
		}
	}

	// ========== Match Tests ==========

	#[test]
	fn test_parse_match_expression()
	{
		let input = r#"match x {
            1 => 10,
            2 => 20,
            _ => 0,
        }"#;
		let result = parse_expr_from_str(input);
		match &result {
			Ok(program) => {
				println!("{:#?}", program);
			}
			Err(e) => {
				println!("{}", e);
			}
		}
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Match { expr: _, arms } => {
				assert_eq!(arms.len(), 3);
			}
			_ => panic!("Expected match expression"),
		}
	}

	// ========== Type Tests ==========

	#[test]
	fn test_parse_simple_type()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "i32");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Base { path, .. } if path == &vec!["i32"] => (),
			_ => panic!("Expected i32 type"),
		}
	}

	#[test]
	fn test_parse_generic_type()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "Vec<i32>");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Base { path, generics } => {
				assert_eq!(path, &vec!["Vec"]);
				assert_eq!(generics.len(), 1);
			}
			_ => panic!("Expected generic type"),
		}
	}

	#[test]
	fn test_parse_reference_type()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "&i32");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Reference { mutable: false, .. } => (),
			_ => panic!("Expected reference type"),
		}
	}

	#[test]
	fn test_parse_mutable_reference_type()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "&mut i32");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Reference { mutable: true, .. } => (),
			_ => panic!("Expected mutable reference type"),
		}
	}

	#[test]
	fn test_parse_pointer_type()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "i32*");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Pointer { .. } => (),
			_ => panic!("Expected pointer type"),
		}
	}

	#[test]
	fn test_parse_array_type()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "i32[10]");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Array { .. } => (),
			_ => panic!("Expected array type"),
		}
	}

	// ========== Variable Declaration Tests ==========

	#[test]
	fn test_parse_let_declaration()
	{
		let input = "let x: i32 = 5;";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		assert_eq!(program.items.len(), 1);
	}

	#[test]
	fn test_parse_const_declaration()
	{
		let input = "const X: i32 = 5;";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		assert_eq!(program.items.len(), 1);
	}

	// ========== Function Tests ==========

	#[test]
	fn test_parse_simple_function()
	{
		let input = r#"
            fn foo() {
                return 42;
            }
        "#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		assert_eq!(program.items.len(), 1);
		match &program.items[0].node {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.name, vec!["foo"]);
				assert_eq!(func.signature.params.len(), 0);
				assert!(func.body.is_some());
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_function_with_params()
	{
		let input = "fn add(x: i32, y: i32) -> i32 { x + y }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.params.len(), 2);
				assert!(func.signature.return_type.is_some());
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_function_with_generics()
	{
		let input = "fn identity<T>(x: T) -> T { x }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.generics.len(), 1);
				assert_eq!(func.signature.generics[0], "T");
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_function_with_modifiers()
	{
		let input = "pub unsafe fn dangerous() {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Function(func) => {
				assert!(func.signature.modifiers.len() >= 2);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_heap_function()
	{
		let input = "fn! heap_allocated() {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Function(func) => {
				assert!(func.signature.heap_func);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_method_with_self()
	{
		let input = "fn method(self) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.params.len(), 1);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_method_with_ref_self()
	{
		let input = "fn method(&self) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_method_with_mut_ref_self()
	{
		let input = "fn method(&mut self) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Struct Tests ==========

	#[test]
	fn test_parse_empty_struct()
	{
		let input = "struct Empty {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Struct(s) => {
				assert_eq!(s.name, vec!["Empty"]);
				assert_eq!(s.fields.len(), 0);
			}
			_ => panic!("Expected struct declaration"),
		}
	}

	#[test]
	fn test_parse_struct_with_fields()
	{
		let input = "struct Point { x: i32, y: i32 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Struct(s) => {
				assert_eq!(s.fields.len(), 2);
				assert_eq!(s.fields[0].1, "x");
				assert_eq!(s.fields[1].1, "y");
			}
			_ => panic!("Expected struct declaration"),
		}
	}

	#[test]
	fn test_parse_pub_struct()
	{
		let input = "pub struct Public {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Struct(s) => {
				assert!(!s.modifiers.is_empty());
			}
			_ => panic!("Expected struct declaration"),
		}
	}

	// ========== Union Tests ==========

	#[test]
	fn test_parse_union()
	{
		let input = "union Data { i: i32, f: f64 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Union(u) => {
				assert_eq!(u.name, vec!["Data"]);
				assert_eq!(u.fields.len(), 2);
			}
			_ => panic!("Expected union declaration"),
		}
	}

	// ========== Enum Tests ==========

	#[test]
	fn test_parse_simple_enum()
	{
		let input = "enum Color { Red, Green, Blue }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Enum(e) => {
				assert_eq!(e.name, vec!["Color"]);
				assert_eq!(e.variants.len(), 3);
			}
			_ => panic!("Expected enum declaration"),
		}
	}

	#[test]
	fn test_parse_enum_with_values()
	{
		let input = "enum Number { One = 1, Two = 2 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Enum(e) => {
				assert_eq!(e.variants.len(), 2);
				assert!(e.variants[0].1.is_some());
			}
			_ => panic!("Expected enum declaration"),
		}
	}

	// ========== Variant Tests ==========

	#[test]
	fn test_parse_variant()
	{
		let input = "variant Option { Some(i32), None }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Variant(v) => {
				assert_eq!(v.name, vec!["Option"]);
				assert_eq!(v.variants.len(), 2);
			}
			_ => panic!("Expected variant declaration"),
		}
	}

	// ========== Type Alias Tests ==========

	#[test]
	fn test_parse_type_alias()
	{
		let input = "type Int = i32;";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{}", e));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::TypeAlias(t) => {
				assert_eq!(t.name, vec!["Int"]);
			}
			_ => panic!("Expected type alias declaration"),
		}
	}

	// ========== Namespace Tests ==========

	#[test]
	fn test_parse_namespace()
	{
		let input = "namespace std { fn foo() {} }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Namespace(n) => {
				assert_eq!(n.name, vec!["std"]);
				assert_eq!(n.body.items.len(), 1);
			}
			_ => panic!("Expected namespace declaration"),
		}
	}

	// ========== Trait Tests ==========

	#[test]
	fn test_parse_simple_trait()
	{
		let input = "trait Display { fn fmt(&self); }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Trait(t) => {
				assert_eq!(t.name, vec!["Display"]);
				assert_eq!(t.items.len(), 1);
			}
			_ => panic!("Expected trait declaration"),
		}
	}

	#[test]
	fn test_parse_trait_with_generics()
	{
		let input = "trait Convert<T> { fn convert(self) -> T; }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Trait(t) => {
				assert_eq!(t.generics.len(), 1);
			}
			_ => panic!("Expected trait declaration"),
		}
	}

	#[test]
	fn test_parse_trait_with_supertraits()
	{
		let input = "trait Sub: Super { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Trait(t) => {
				assert_eq!(t.super_traits.len(), 1);
			}
			_ => panic!("Expected trait declaration"),
		}
	}

	// ========== Impl Tests ==========

	#[test]
	fn test_parse_inherent_impl()
	{
		let input = "impl MyStruct { fn method(&self) {} }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Impl(i) => {
				assert_eq!(i.target.path, vec!["MyStruct"]);
				assert!(i.trait_path.is_none());
				assert_eq!(i.body.len(), 1);
			}
			_ => panic!("Expected impl declaration"),
		}
	}

	#[test]
	fn test_parse_trait_impl()
	{
		let input = "impl Display for MyStruct { fn fmt(&self) {} }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Impl(i) => {
				assert!(i.trait_path.is_some());
				assert_eq!(i.target.path, vec!["MyStruct"]);
			}
			_ => panic!("Expected impl declaration"),
		}
	}

	#[test]
	fn test_parse_generic_impl()
	{
		let input = "impl<T> MyStruct<T> { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0].node {
			TopLevelDecl::Impl(i) => {
				assert_eq!(i.generics.len(), 1);
			}
			_ => panic!("Expected impl declaration"),
		}
	}

	// ========== Statement Tests ==========

	#[test]
	fn test_parse_if_statement()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{if true { 1 }}");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_else_statement()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{if true { 1 } else { 2 };}");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_while_statement()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ while true { break; } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_for_statement()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ for i in 0..10 { } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_return_statement()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ return 42; }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_statement()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ break; }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_continue_statement()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ continue; }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_assignment()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ x = 5; }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_compound_assignment()
	{
		let ops = vec!["+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="];
		for op in ops {
			let input = format!("{{ x {} 5; }}", op);
			let config = Config::default();
			let lexer = Lexer::new(&config, &input);
			let mut parser = Parser::from(lexer);
			let result = parser.parse_block();
			assert!(result.is_ok(), "Failed to parse compound assignment: {}", op);
		}
	}

	// ========== Complex Expression Tests ==========

	#[test]
	fn test_parse_nested_calls()
	{
		let result = parse_expr_from_str("foo(bar(baz()))");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_method_chain()
	{
		let result = parse_expr_from_str("obj.method1().method2().field");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_complex_expression()
	{
		let result = parse_expr_from_str("(a + b) * c - d / e");
		assert!(result.is_ok());
	}

	// ========== Error Tests ==========

	#[test]
	fn test_parse_error_unexpected_token()
	{
		let result = parse_expr_from_str("let");
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_error_unmatched_paren()
	{
		let result = parse_expr_from_str("(1 + 2");
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_error_invalid_type()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "123");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_if_let_basic()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let Some(x: i64) = opt { x } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		assert_eq!(block.stmts.len(), 0);
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_if_let_with_else()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let Some(x: i64) = opt { x } else { 0 }; }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_let_else_if_let()
	{
		let config = Config::default();
		let lexer = Lexer::new(
			&config,
			"{ if let Some(x: i32) = opt1 { x } else if let Some(y: i32) = opt2 { y } else { 0 }; }",
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_let_tuple_pattern()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let (x: i32, y: i32) = pair { x + y } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_let_wildcard()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let Some(_) = opt { true } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_let_wildcard_with_type()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let Some(_: i64) = opt { true } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	// ========== While Let Tests ==========

	#[test]
	fn test_parse_while_let()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ while let Some(x: i32) = iter.next() { process(x); } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::WhileLetLoop { .. } => (),
			_ => panic!("Expected while let loop"),
		}
	}

	#[test]
	fn test_parse_while_let_tuple()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ while let (a: i32, b: i32) = get_pair() { } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	// ========== Enhanced Pattern Tests ==========

	#[test]
	fn test_parse_struct_pattern()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let Point { x: i32, y: i32 } = pt { x } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_struct_pattern_nested()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let Point { x: a: i32, y: b: i32 } = pt { a } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_or_pattern()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let Some(1) | Some(2) | Some(3) = x { true } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_nested_variant_pattern()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let Some(Point { x: a: i32, y: b: i32 }) = opt { a } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_variant_with_tuple()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let Some((x: i32, y: i32)) = opt { x } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_unit_variant()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if let None = opt { true } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_match_with_typed_patterns()
	{
		let input = r#"
			match x {
				Some(val: i32) => val,
				None => 0,
			}
		"#;
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
	}
}
