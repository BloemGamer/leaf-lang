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
/// let source = "fn main() { var x = 42; }";
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
		let (config, source, lex) = lexer.into_parts();
		return Self {
			config,
			source,
			lexer: lex.peekable(),
			last_span: Span::default(),
			buffered_token: None,
		};
	}
}

/// Identifier type alias for clearer code semantics.
///
/// Represents variable names, function names, type names, and other identifiers
/// throughout the AST.
pub type Ident = String;

/// Trait for types that have source location information.
///
/// Provides a unified way to extract span information from various AST nodes.
pub trait Spanned
{
	fn span(&self) -> Span;
}

impl Spanned for Span
{
	fn span(&self) -> Span
	{
		return *self;
	}
}

impl Spanned for Token
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// The root node of the Abstract Syntax Tree.
///
/// Represents a complete program or compilation unit as a sequence of
/// top-level declarations.
///
/// # Fields
/// * `items` - List of top-level declarations (functions, structs, traits, etc.)
/// * `span` - Source location of the entire program
#[derive(Debug, Clone)]
pub struct Program
{
	pub items: Vec<TopLevelDecl>,
	pub span: Span,
}

impl Spanned for Program
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

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
	/// let source = "fn main() { var x = 42; }";
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
		return parser.parse_program();
	}
}

impl<'s, 'c> From<Lexer<'s, 'c>> for Result<Program, ParseError>
{
	fn from(value: Lexer<'s, 'c>) -> Self
	{
		return <Result<Program, ParseError>>::from(Parser::from(value));
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
	Directive(DirectiveNode),
}

impl Spanned for TopLevelDecl
{
	fn span(&self) -> Span
	{
		return match self {
			TopLevelDecl::Function(f) => f.span(),
			TopLevelDecl::VariableDecl(v) => v.span(),
			TopLevelDecl::Struct(s) => s.span(),
			TopLevelDecl::Union(u) => u.span(),
			TopLevelDecl::Enum(e) => e.span(),
			TopLevelDecl::Variant(v) => v.span(),
			TopLevelDecl::TypeAlias(t) => t.span(),
			TopLevelDecl::Trait(t) => t.span(),
			TopLevelDecl::Namespace(n) => n.span(),
			TopLevelDecl::Impl(i) => i.span(),
			TopLevelDecl::Directive(d) => d.span(),
		};
	}
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

/// Directive node with optional body.
///
/// Represents a directive that may have an associated block of code.
///
/// # Fields
/// * `directive` - The directive itself
/// * `body` - Optional block content associated with the directive
/// * `span` - Source location of the directive
#[derive(Debug, Clone)]
pub struct DirectiveNode
{
	pub directive: Directive,
	pub body: Option<BlockContent>,
	pub span: Span,
}

impl Spanned for DirectiveNode
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Function declaration.
///
/// Represents a complete function including its signature and optional body.
/// Functions without bodies are prototypes (typically for external functions).
///
/// # Fields
/// * `signature` - Function signature (name, parameters, return type, etc.)
/// * `body` - Optional function body (None for prototypes)
/// * `span` - Source location of the function
#[derive(Debug, Clone)]
pub struct FunctionDecl
{
	pub signature: FunctionSignature,
	pub body: Option<Block>,
	pub span: Span,
}

impl Spanned for FunctionDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
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
/// * `span` - Source location of the signature
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
	pub span: Span,
}

impl Spanned for FunctionSignature
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Function parameter.
///
/// Represents a single parameter in a function signature.
///
/// # Fields
/// * `ty` - Parameter type
/// * `name` - Parameter name (can be qualified path)
/// * `span` - Source location of the parameter
#[derive(Debug, Clone)]
pub struct Param
{
	pub ty: Type,
	pub name: Vec<Ident>,
	pub span: Span,
}

impl Spanned for Param
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Type expression.
///
/// Represents a type in the type system, including modifiers.
///
/// # Fields
/// * `modifiers` - Type modifiers (const, volatile, etc.)
/// * `core` - The core type expression
/// * `span` - Source location of the type
#[derive(Debug, Clone)]
pub struct Type
{
	pub modifiers: Vec<Modifier>,
	pub core: Box<TypeCore>,
	pub span: Span,
}

impl Spanned for Type
{
	fn span(&self) -> Span
	{
		return self.span;
	}
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
/// * `span` - Source location of the range
#[derive(Debug, Clone)]
pub struct RangeExpr
{
	pub start: Option<Box<Expr>>,
	pub end: Option<Box<Expr>>,
	pub inclusive: bool,
	pub span: Span,
}

impl Spanned for RangeExpr
{
	fn span(&self) -> Span
	{
		return self.span;
	}
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
/// * `UnsafeBlock` - Unsafe block expression
/// * `Switch` - Pattern matching expression
/// * `If` - Conditional expression
/// * `IfVar` - Pattern matching conditional expression
/// * `Loop` - Infinite loop expression
#[derive(Debug, Clone)]
pub enum Expr
{
	Identifier
	{
		path: Vec<Ident>,
		span: Span,
	},

	Literal
	{
		value: Literal,
		span: Span,
	},

	Default
	{
		span: Span,
	},

	Unary
	{
		op: UnaryOp,
		expr: Box<Expr>,
		span: Span,
	},

	Binary
	{
		op: BinaryOp,
		lhs: Box<Expr>,
		rhs: Box<Expr>,
		span: Span,
	},

	Cast
	{
		ty: Box<Type>,
		expr: Box<Expr>,
		span: Span,
	},

	Call
	{
		callee: Box<Expr>,
		args: Vec<Expr>,
		span: Span,
	},

	Field
	{
		base: Box<Expr>,
		name: Ident,
		span: Span,
	},

	Index
	{
		base: Box<Expr>,
		index: Box<Expr>,
		span: Span,
	},

	Range(RangeExpr),

	Tuple
	{
		elements: Vec<Expr>,
		span: Span,
	},

	Array(ArrayLiteral),

	StructInit
	{
		path: Vec<Ident>,
		fields: Vec<(Ident, Expr)>,
		span: Span,
	},

	Block(Box<Block>),

	UnsafeBlock(Box<Block>),

	Switch
	{
		expr: Box<Expr>,
		arms: Vec<SwitchArm>,
		span: Span,
	},

	If
	{
		cond: Box<Expr>,
		then_block: Block,
		else_branch: Option<Box<Expr>>,
		span: Span,
	},

	IfVar
	{
		pattern: Pattern,
		expr: Box<Expr>,
		then_block: Block,
		else_branch: Option<Box<Expr>>,
		span: Span,
	},

	Loop
	{
		label: Option<String>,
		body: Box<Block>,
		span: Span,
	},
}

impl Spanned for Expr
{
	fn span(&self) -> Span
	{
		#[allow(clippy::match_same_arms)]
		return match self {
			Expr::Identifier { span, .. } => *span,
			Expr::Literal { span, .. } => *span,
			Expr::Default { span } => *span,
			Expr::Unary { span, .. } => *span,
			Expr::Binary { span, .. } => *span,
			Expr::Cast { span, .. } => *span,
			Expr::Call { span, .. } => *span,
			Expr::Field { span, .. } => *span,
			Expr::Index { span, .. } => *span,
			Expr::Range(RangeExpr { span, .. }) => *span,
			Expr::Tuple { span, .. } => *span,
			Expr::Array(ArrayLiteral::List { span, .. }) => *span,
			Expr::Array(ArrayLiteral::Repeat { span, .. }) => *span,
			Expr::StructInit { span, .. } => *span,
			Expr::Block(block) => block.span(),
			Expr::UnsafeBlock(block) => block.span(),
			Expr::Switch { span, .. } => *span,
			Expr::If { span, .. } => *span,
			Expr::IfVar { span, .. } => *span,
			Expr::Loop { span, .. } => *span,
		};
	}
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
	List
	{
		elements: Vec<Expr>, span: Span
	},
	Repeat
	{
		value: Vec<Expr>,
		count: Box<Expr>,
		span: Span,
	},
}

impl Spanned for ArrayLiteral
{
	fn span(&self) -> Span
	{
		#[allow(clippy::match_same_arms)]
		return match self {
			ArrayLiteral::List { span, .. } => *span,
			ArrayLiteral::Repeat { span, .. } => *span,
		};
	}
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
/// * `pattern` - Pattern for destructuring
/// * `init` - Optional initializer expression
/// * `comp_const` - Whether this is a compile-time constant (`const` vs `var`)
/// * `span` - Source location of the declaration
#[derive(Debug, Clone)]
pub struct VariableDecl
{
	pub pattern: Pattern,
	pub init: Option<Expr>,
	pub comp_const: bool,
	pub span: Span,
}

impl Spanned for VariableDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
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
/// * `IfVar` - Pattern matching conditional statement
/// * `While` - While loop
/// * `Loop` - Infinite loop
/// * `WhileVarLoop` - Pattern matching while loop
/// * `For` - For-in loop
/// * `Delete` - Delete statement
/// * `Unsafe` - Unsafe block
/// * `Block` - Block statement
/// * `Directive` - Directive statement
#[derive(Debug, Clone)]
pub enum Stmt
{
	VariableDecl(VariableDecl),

	Assignment
	{
		target: Expr,
		op: AssignOp,
		value: Expr,
		span: Span,
	},

	Return
	{
		value: Option<Expr>,
		span: Span,
	},

	Expr(Expr),

	Break
	{
		label: Option<String>,
		value: Option<Expr>,
		span: Span,
	},

	Continue
	{
		label: Option<String>,
		span: Span,
	},

	If
	{
		cond: Expr,
		then_block: Block,
		else_branch: Option<Box<Stmt>>,
		span: Span,
	},

	IfVar
	{
		pattern: Pattern,
		expr: Expr,
		then_block: Block,
		else_branch: Option<Box<Stmt>>,
		span: Span,
	},

	While
	{
		label: Option<String>,
		cond: Expr,
		body: Block,
		span: Span,
	},

	Loop
	{
		label: Option<String>,
		body: Block,
		span: Span,
	},

	WhileVarLoop
	{
		label: Option<String>,
		pattern: Pattern,
		expr: Expr,
		body: Block,
		span: Span,
	},

	For
	{
		label: Option<String>,
		name: Vec<Ident>,
		iter: Expr,
		body: Block,
		span: Span,
	},

	Delete
	{
		path: Vec<Ident>,
		span: Span,
	},

	Unsafe(Block),

	Block(Block),

	Directive(DirectiveNode),
}

impl Spanned for Stmt
{
	fn span(&self) -> Span
	{
		#[allow(clippy::match_same_arms)]
		return match self {
			Stmt::VariableDecl(VariableDecl { span, .. }) => *span,
			Stmt::Assignment { span, .. } => *span,
			Stmt::Return { span, .. } => *span,
			Stmt::Expr(expr) => expr.span(),
			Stmt::Break { span, .. } => *span,
			Stmt::Continue { span, .. } => *span,
			Stmt::If { span, .. } => *span,
			Stmt::IfVar { span, .. } => *span,
			Stmt::While { span, .. } => *span,
			Stmt::Loop { span, .. } => *span,
			Stmt::WhileVarLoop { span, .. } => *span,
			Stmt::For { span, .. } => *span,
			Stmt::Delete { span, .. } => *span,
			Stmt::Unsafe(block) => block.span(),
			Stmt::Block(block) => block.span(),
			Stmt::Directive(DirectiveNode { span, .. }) => *span,
		};
	}
}

impl Stmt
{
	fn set_label(&mut self, label: String)
	{
		#[allow(clippy::match_same_arms)]
		match self {
			Stmt::While { label: l, .. } => *l = Some(label),
			Stmt::Loop { label: l, .. } => *l = Some(label),
			Stmt::For { label: l, .. } => *l = Some(label),
			Stmt::WhileVarLoop { label: l, .. } => *l = Some(label),
			_ => {}
		}
	}
}

/// Block of statements with optional tail expression.
///
/// Represents a sequence of statements that can optionally evaluate to a value
/// (the tail expression). This is the primary scoping construct.
///
/// # Fields
/// * `stmts` - List of statements in the block
/// * `tail_expr` - Optional final expression (the block's value)
/// * `span` - Source location of the block
///
/// # Example
/// ```
/// // Block with tail expression:
/// {
///     var x = 5;
///     x + 1  // tail expression, block evaluates to 6
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Block
{
	pub stmts: Vec<Stmt>,
	pub tail_expr: Option<Box<Expr>>,
	pub span: Span,
}

impl Spanned for Block
{
	fn span(&self) -> Span
	{
		return self.span;
	}
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

/// Switch expression arm.
///
/// Represents a single arm in a switch expression.
///
/// # Fields
/// * `pattern` - Pattern to match against
/// * `body` - Code to execute if pattern matches
/// * `span` - Source location of the arm
#[derive(Debug, Clone)]
pub struct SwitchArm
{
	pub pattern: Pattern,
	pub body: SwitchBody,
	pub span: Span,
}

impl Spanned for SwitchArm
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Switch arm body types.
///
/// The body of a match arm can be either a single expression or a block.
///
/// # Variants
/// * `Expr` - Single expression (requires comma)
/// * `Block` - Block of statements
#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum SwitchBody
{
	Expr(Expr),
	Block(Block),
}

impl Spanned for SwitchBody
{
	fn span(&self) -> Span
	{
		return match self {
			SwitchBody::Expr(expr) => expr.span(),
			SwitchBody::Block(Block { span, .. }) => *span,
		};
	}
}

/// Pattern matching patterns.
///
/// Represents patterns that can appear in switch expressions and if let/while let.
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
	Wildcard
	{
		span: Span,
	},
	Literal
	{
		value: Literal,
		span: Span,
	},
	TypedIdentifier
	{
		name: Ident,
		ty: Type,
		call_constructor: bool,
		span: Span,
	},
	Variant
	{
		path: Vec<Ident>,
		args: Vec<Pattern>,
		span: Span,
	},
	Tuple
	{
		patterns: Vec<Pattern>,
		span: Span,
	},
	Struct
	{
		path: Vec<Ident>,
		fields: Vec<(Ident, Pattern)>,
		span: Span,
	},
	Range(RangeExpr),
	Or
	{
		patterns: Vec<Pattern>,
		span: Span,
	},
}

impl Spanned for Pattern
{
	fn span(&self) -> Span
	{
		#[allow(clippy::match_same_arms)]
		return match self {
			Pattern::Wildcard { span } => *span,
			Pattern::Literal { span, .. } => *span,
			Pattern::TypedIdentifier { span, .. } => *span,
			Pattern::Variant { span, .. } => *span,
			Pattern::Tuple { span, .. } => *span,
			Pattern::Struct { span, .. } => *span,
			Pattern::Range(RangeExpr { span, .. }) => *span,
			Pattern::Or { span, .. } => *span,
		};
	}
}

/// Structure type declaration.
///
/// Represents a struct with named fields.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Struct name (can be qualified path)
/// * `fields` - List of (`type`, `name`, `Option<default_value>`) pairs for fields
/// * `span` - Source location of the struct
#[derive(Debug, Clone)]
pub struct StructDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub fields: Vec<(Type, Ident, Option<Expr>)>,
	pub span: Span,
}

impl Spanned for StructDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Structure type declaration.
///
/// Represents a union with named fields.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Struct name (can be qualified path)
/// * `fields` - List of (type, name) pairs for fields
/// * `span` - Source location of the struct
#[derive(Debug, Clone)]
pub struct UnionDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub fields: Vec<(Type, Ident)>,
	pub span: Span,
}

impl Spanned for UnionDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// C-style enumeration declaration.
///
/// Represents an enum where variants are integer constants.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Enum name (can be qualified path)
/// * `variants` - List of (name, optional value) pairs
/// * `span` - Source location of the enum
#[derive(Debug, Clone)]
pub struct EnumDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub variants: Vec<(Ident, Option<Expr>)>,
	pub span: Span,
}

impl Spanned for EnumDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Tagged union (Rust-style enum) declaration.
///
/// Represents an enum where variants can carry data.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Variant name (can be qualified path)
/// * `variants` - List of (`Option<type>`, `name`, `Option<value>`) pairs for variants
/// * `span` - Source location of the variant
#[derive(Debug, Clone)]
pub struct VariantDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub variants: Vec<(Option<Type>, Ident, Option<Expr>)>,
	pub span: Span,
}

impl Spanned for VariantDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
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
/// * `span` - Source location of the trait
#[derive(Debug, Clone)]
pub struct TraitDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub generics: Vec<Ident>,
	pub super_traits: Vec<Vec<Ident>>,
	pub items: Vec<TraitItem>,
	pub span: Span,
}

impl Spanned for TraitDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
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
	Function
	{
		signature: FunctionSignature,
		body: Option<Block>,
		span: Span,
	},
	TypeAlias(TypeAliasDecl),
	Const(VariableDecl),
}

impl Spanned for TraitItem
{
	fn span(&self) -> Span
	{
		#[allow(clippy::match_same_arms)]
		match self {
			TraitItem::Function { span, .. } => return *span,
			TraitItem::TypeAlias(TypeAliasDecl { span, .. }) => return *span,
			TraitItem::Const(VariableDecl { span, .. }) => return *span,
		}
	}
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
/// * `span` - Source location of the impl
#[derive(Debug, Clone)]
pub struct ImplDecl
{
	pub modifiers: Vec<Modifier>,
	pub generics: Vec<Ident>,
	pub target: ImplTarget,
	pub trait_path: Option<ImplTarget>,
	pub where_clause: Vec<WhereConstraint>,
	pub body: Vec<ImplItem>,
	pub span: Span,
}

impl Spanned for ImplDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Implementation target type.
///
/// Specifies what type an implementation applies to.
///
/// # Fields
/// * `path` - Type path
/// * `generics` - Generic arguments
/// * `span` - Source location of the target
#[derive(Debug, Clone)]
pub struct ImplTarget
{
	pub path: Vec<Ident>,
	pub generics: Vec<Type>,
	pub span: Span,
}

impl Spanned for ImplTarget
{
	fn span(&self) -> Span
	{
		return self.span;
	}
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

impl Spanned for ImplItem
{
	fn span(&self) -> Span
	{
		#[allow(clippy::match_same_arms)]
		match self {
			ImplItem::Function(FunctionDecl { span, .. }) => return *span,
			ImplItem::TypeAlias(TypeAliasDecl { span, .. }) => return *span,
			ImplItem::Const(VariableDecl { span, .. }) => return *span,
		}
	}
}

/// Generic type constraint (where clause).
///
/// Represents a constraint like `T: Trait1 + Trait2`.
///
/// # Fields
/// * `ty` - Type being constrained
/// * `bounds` - List of trait bounds
/// * `span` - Source location of the constraint
#[derive(Debug, Clone)]
pub struct WhereConstraint
{
	pub ty: Vec<Ident>,
	pub bounds: Vec<Vec<Ident>>,
	pub span: Span,
}

impl Spanned for WhereConstraint
{
	fn span(&self) -> Span
	{
		return self.span;
	}
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
		return write!(f, "Parse error at {:?}: {}", self.span, self.message);
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
/// * `span` - Source location of the type alias
#[derive(Debug, Clone)]
pub struct TypeAliasDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub ty: Type,
	pub span: Span,
}

impl Spanned for TypeAliasDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Namespace declaration.
///
/// Represents a namespace/module containing top-level declarations.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Namespace name (can be qualified path)
/// * `body` - Declarations within the namespace
/// * `span` - Source location of the namespace
#[derive(Debug, Clone)]
pub struct NamespaceDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Vec<Ident>,
	pub body: TopLevelBlock,
	pub span: Span,
}

impl Spanned for NamespaceDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

impl<'s, 'c> Parser<'s, 'c>
{
	fn peek(&mut self) -> &Token
	{
		return self
			.buffered_token
			.as_ref()
			.unwrap_or_else(|| return self.lexer.peek().expect("lexer exhausted unexpectedly"));
	}

	fn next(&mut self) -> Token
	{
		if let Some(tok) = self.buffered_token.take() {
			self.last_span = tok.span;
			return tok;
		}
		let tok: Token = self.lexer.next().expect("lexer exhausted unexpectedly");
		self.last_span = tok.span;
		return tok;
	}

	fn peek_kind(&mut self) -> &TokenKind
	{
		return &self.peek().kind;
	}

	fn at(&mut self, kind: &TokenKind) -> bool
	{
		return self.peek_kind() == kind;
	}

	fn consume(&mut self, kind: &TokenKind) -> bool
	{
		if self.at(kind) {
			self.next();
			return true;
		} else {
			return false;
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

		return false;
	}

	fn expect(&mut self, expected: &TokenKind) -> Result<Token, ParseError>
	{
		let tok: &Token = self.peek();

		if &tok.kind == expected {
			return Ok(self.next());
		} else {
			let err_tok: Token = tok.clone();
			return Err(ParseError {
				span: err_tok.span,
				message: err_tok.format_error(
					self.source,
					&format!("expected {:?}, found {:?}", expected, err_tok.kind),
				),
			});
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
		let mut items: Vec<TopLevelDecl> = Vec::new();

		while !matches!(self.peek().kind, TokenKind::Eof | TokenKind::RightBrace) {
			let decl = self.parse_top_level_decl()?;
			items.push(decl);
		}

		let span: Span = if items.is_empty() {
			Span::default()
		} else {
			items
				.first()
				.expect("at this point, items should have an item")
				.span()
				.merge(&items.last().expect("this should not be possible").span())
		};

		return Ok(Program { items, span });
	}

	fn parse_top_level_decl(&mut self) -> Result<TopLevelDecl, ParseError>
	{
		let decl_kind = self.peek_declaration_kind()?;

		let ret: TopLevelDecl = match decl_kind {
			DeclKind::Function => {
				let func_decl: FunctionDecl = self.parse_function_decl()?;
				TopLevelDecl::Function(func_decl)
			}
			DeclKind::Variable => {
				let var_decl: VariableDecl = self.parse_var_decl()?;
				self.expect(&TokenKind::Semicolon)?;
				TopLevelDecl::VariableDecl(var_decl)
			}
			DeclKind::Directive => {
				let directive_node: DirectiveNode = self.parse_directive_node()?;

				if directive_node.body.is_none() {
					self.expect(&TokenKind::Semicolon)?;
				}

				TopLevelDecl::Directive(directive_node)
			}
			DeclKind::Struct => {
				let struct_decl: StructDecl = self.parse_struct()?;

				TopLevelDecl::Struct(struct_decl)
			}
			DeclKind::Union => {
				let union_decl: UnionDecl = self.parse_union()?;

				TopLevelDecl::Union(union_decl)
			}
			DeclKind::TypeAlias => {
				let type_alias: TypeAliasDecl = self.parse_type_alias()?;
				self.expect(&TokenKind::Semicolon)?;
				TopLevelDecl::TypeAlias(type_alias)
			}
			DeclKind::Namespace => {
				let namespace_decl: NamespaceDecl = self.parse_namespace()?;

				TopLevelDecl::Namespace(namespace_decl)
			}
			DeclKind::Impl => {
				let impl_decl: ImplDecl = self.parse_impl()?;

				TopLevelDecl::Impl(impl_decl)
			}
			DeclKind::Trait => {
				let trait_decl: TraitDecl = self.parse_trait()?;

				TopLevelDecl::Trait(trait_decl)
			}
			DeclKind::Enum => {
				let enum_decl: EnumDecl = self.parse_enum()?;

				TopLevelDecl::Enum(enum_decl)
			}
			DeclKind::Variant => {
				let tagged_union_decl: VariantDecl = self.parse_variant()?;

				TopLevelDecl::Variant(tagged_union_decl)
			} // other => todo!("not yet implemented: {:?}", other),
		};

		return Ok(ret);
	}

	fn peek_declaration_kind(&mut self) -> Result<DeclKind, ParseError>
	{
		let checkpoint: Peekable<Lexer<'s, 'c>> = self.lexer.clone();
		let checkpoint_span: Span = self.last_span;

		loop {
			match self.peek_kind() {
				TokenKind::Pub | TokenKind::Unsafe | TokenKind::Inline => {
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
				TokenKind::Directive(_) => {
					self.next();

					if self.at(&TokenKind::LeftParen) {
						self.skip_until_balanced_paren()?;
					}

					loop {
						#[allow(clippy::match_same_arms)]
						match self.peek_kind() {
							TokenKind::Semicolon | TokenKind::LeftBrace => {
								self.lexer = checkpoint;
								self.last_span = checkpoint_span;
								return Ok(DeclKind::Directive);
							}
							TokenKind::FuncDef
							| TokenKind::Struct
							| TokenKind::Union
							| TokenKind::Enum
							| TokenKind::Variant
							| TokenKind::Type
							| TokenKind::Namespace
							| TokenKind::Impl
							| TokenKind::Trait
							| TokenKind::Var
							| TokenKind::Const => {
								break;
							}
							TokenKind::Pub | TokenKind::Unsafe | TokenKind::Inline | TokenKind::Directive(_) => {
								break;
							}
							_ => {
								self.next();
							}
						}
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
				TokenKind::Var => {
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

	fn skip_until_balanced_paren(&mut self) -> Result<(), ParseError>
	{
		if !self.at(&TokenKind::LeftParen) {
			return Ok(());
		}
		self.next(); // (

		let mut depth = 1;
		while depth > 0 {
			match self.peek_kind() {
				TokenKind::LeftParen => {
					depth += 1;
					self.next();
				}
				TokenKind::RightParen => {
					depth -= 1;
					self.next();
				}
				TokenKind::Eof => {
					return Err(ParseError {
						span: self.peek().span,
						message: "unexpected EOF while parsing attribute arguments".to_string(),
					});
				}
				_ => {
					self.next();
				}
			}
		}
		return Ok(());
	}

	fn parse_directive_node(&mut self) -> Result<DirectiveNode, ParseError>
	{
		#[allow(clippy::debug_assert_with_mut_call)]
		{
			debug_assert!(matches!(self.peek().kind, TokenKind::Directive(_)));
		}

		let tok: Token = self.next();
		let start: Span = tok.span;

		let directive: Directive = match tok.kind {
			TokenKind::Directive(d) => self.parse_directive_kind(d)?,
			_ => unreachable!("Bug: Token should be a directive"),
		};

		let body: Option<BlockContent> = if self.at(&TokenKind::LeftBrace) {
			self.next(); // {

			let content: BlockContent = if self.should_parse_as_top_level_block(&directive) {
				BlockContent::TopLevelBlock(self.parse_program()?)
			} else {
				BlockContent::Block(self.parse_block_content()?)
			};

			self.expect(&TokenKind::RightBrace)?;
			Some(content)
		} else {
			None
		};

		let end: Span = self.last_span;

		return Ok(DirectiveNode {
			directive,
			body,
			span: start.merge(&end),
		});
	}

	fn should_parse_as_top_level_block(&self, directive: &Directive) -> bool // TODO, find out why I have this funcition
	{
		match directive {
			Directive::Custom { name, .. } => {
				return matches!(name.as_str(), "extern" | "cfg" | "module" | "namespace");
			}
			_ => return false,
		}
	}

	fn parse_directive(&mut self) -> Result<Directive, ParseError>
	{
		#[allow(clippy::debug_assert_with_mut_call)]
		{
			debug_assert!(matches!(self.peek().kind, TokenKind::Directive(_)));
		}

		let tok: Token = self.next();
		// let start: Span = tok.span;

		let node: Directive = match tok.kind {
			TokenKind::Directive(d) => self.parse_directive_kind(d)?,
			_ => unreachable!("{}", tok.format_error(self.source, "Bug: Token should be a directive")),
		};

		// let end: Span = self.last_span;

		return Ok(node);
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
					TokenKind::StringLiteral(str) => Directive::Import(str.clone()),
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
					self.next(); // (
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

	fn parse_var_decl(&mut self) -> Result<VariableDecl, ParseError>
	{
		let tok: Token = self.next();
		let span: Span = tok.span;
		if !matches!(tok.kind, TokenKind::Const | TokenKind::Var) {
			unreachable!(
				"Bug: expected const or let for a variable declaration, got: {:?}",
				tok.kind
			);
		}
		let comp_const: bool = tok.kind == TokenKind::Const;

		let pattern: Pattern = self.parse_pattern()?;

		let init: Option<Expr> = if self.at(&TokenKind::Equals) {
			self.next();
			Some(self.parse_expr()?)
		} else {
			None
		};

		return Ok(VariableDecl {
			pattern,
			init,
			comp_const,
			span: self.last_span.merge(&span),
		});
	}

	fn parse_type(&mut self) -> Result<Type, ParseError>
	{
		let span: Span = self.peek().span();
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		let core: TypeCore = self.parse_type_core()?;
		return Ok(Type {
			modifiers,
			core: Box::new(self.parse_type_suffix(core)?),
			span: span.merge(&self.last_span),
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
			TokenKind::LeftParen => {
				self.next(); // (

				if self.consume(&TokenKind::RightParen) {
					return Ok(TypeCore::Tuple(Vec::new()));
				}

				let mut types: Vec<Type> = vec![self.parse_type()?];

				if self.consume(&TokenKind::Comma) {
					if !self.at(&TokenKind::RightParen) {
						loop {
							types.push(self.parse_type()?);
							if !self.consume(&TokenKind::Comma) {
								break;
							}
							if self.at(&TokenKind::RightParen) {
								break;
							}
						}
					}
					self.expect(&TokenKind::RightParen)?;
					return Ok(TypeCore::Tuple(types));
				} else {
					self.expect(&TokenKind::RightParen)?;
					let ty: Type = types.into_iter().next().expect(
						"this should already be cought in the code before, because of `vec![self.parse_type()?]",
					);
					return Ok(*ty.core);
				}
			}
			_ => {
				let err_tok: Token = tok.clone();
				return Err(ParseError {
					span: err_tok.span,
					message: err_tok
						.format_error(self.source, "Expected an ampersand, mut, identifier, or '(' for type"),
				});
			}
		}
	}

	fn parse_type_suffix(&mut self, mut base: TypeCore) -> Result<TypeCore, ParseError>
	{
		loop {
			match self.peek_kind() {
				TokenKind::Star => {
					self.next(); // *
					base = TypeCore::Pointer { inner: Box::new(base) };
				}
				TokenKind::LeftBracket => {
					self.next(); // [
					let size_expr: Expr = self.parse_expr()?;
					self.expect(&TokenKind::RightBracket)?; // ]
					base = TypeCore::Array {
						inner: Box::new(base),
						size: Box::new(size_expr),
					};
				}
				_ => break,
			}
		}
		return Ok(base);
	}

	fn get_path(&mut self) -> Result<Vec<Ident>, ParseError>
	{
		let mut path: Vec<Ident> = Vec::new();
		loop {
			let tok: Token = self.next();
			match &tok.kind {
				TokenKind::Identifier(s) => path.push(s.clone()),
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

		return Ok(generics);
	}

	pub fn parse_expr(&mut self) -> Result<Expr, ParseError>
	{
		return self.parse_expr_inner(true);
	}

	pub fn parse_expr_no_struct(&mut self) -> Result<Expr, ParseError>
	{
		return self.parse_expr_inner(false);
	}

	fn parse_expr_inner(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		return self.parse_logical_or(allow_struct_init);
	}

	fn parse_logical_or(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut lhs: Expr = self.parse_logical_and(allow_struct_init)?;

		while self.consume(&TokenKind::Or) {
			let rhs: Expr = self.parse_logical_and(allow_struct_init)?;
			lhs = Expr::Binary {
				op: BinaryOp::LogicalOr,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_logical_and(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut lhs: Expr = self.parse_bitwise_or(allow_struct_init)?;

		while self.consume(&TokenKind::And) {
			let rhs: Expr = self.parse_bitwise_or(allow_struct_init)?;
			lhs = Expr::Binary {
				op: BinaryOp::LogicalAnd,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_bitwise_or(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut lhs: Expr = self.parse_bitwise_xor(allow_struct_init)?;

		while self.at(&TokenKind::Pipe) {
			self.next();
			let rhs: Expr = self.parse_bitwise_xor(allow_struct_init)?;
			lhs = Expr::Binary {
				op: BinaryOp::BitOr,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_bitwise_xor(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut lhs: Expr = self.parse_bitwise_and(allow_struct_init)?;

		while self.at(&TokenKind::Caret) {
			self.next();
			let rhs: Expr = self.parse_bitwise_and(allow_struct_init)?;
			lhs = Expr::Binary {
				op: BinaryOp::BitXor,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_bitwise_and(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut lhs: Expr = self.parse_equality(allow_struct_init)?;

		while self.at(&TokenKind::Ampersand) {
			self.next();
			let rhs: Expr = self.parse_equality(allow_struct_init)?;
			lhs = Expr::Binary {
				op: BinaryOp::BitAnd,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_equality(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut lhs: Expr = self.parse_relational(allow_struct_init)?;

		loop {
			let op: BinaryOp = match self.peek_kind() {
				TokenKind::EqualsEquals => BinaryOp::Eq,
				TokenKind::BangEquals => BinaryOp::Ne,
				_ => break,
			};

			self.next();
			let rhs: Expr = self.parse_relational(allow_struct_init)?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_relational(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut lhs: Expr = self.parse_shift(allow_struct_init)?;

		loop {
			let op: BinaryOp = match self.peek_kind() {
				TokenKind::LessThan => BinaryOp::Lt,
				TokenKind::GreaterThan => BinaryOp::Gt,
				TokenKind::LessEquals => BinaryOp::Le,
				TokenKind::GreaterEquals => BinaryOp::Ge,
				_ => break,
			};

			self.next();
			let rhs: Expr = self.parse_shift(allow_struct_init)?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_shift(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut lhs: Expr = self.parse_range(allow_struct_init)?;

		loop {
			let op: BinaryOp = match self.peek_kind() {
				TokenKind::LShift => BinaryOp::Shl,
				TokenKind::RShift => BinaryOp::Shr,
				_ => break,
			};

			self.next();
			let rhs: Expr = self.parse_range(allow_struct_init)?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_range(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let start: Expr = self.parse_additive(allow_struct_init)?;

		match self.peek_kind() {
			TokenKind::DotDot => {
				self.next();
				let end: Option<Box<Expr>> = if self.is_range_end() {
					None
				} else {
					Some(Box::new(self.parse_additive(allow_struct_init)?))
				};
				return Ok(Expr::Range(RangeExpr {
					start: Some(Box::new(start)),
					end,
					inclusive: false,
					span: span.merge(&self.last_span),
				}));
			}
			TokenKind::DotDotEquals => {
				self.next();
				let end: Box<Expr> = Box::new(self.parse_additive(allow_struct_init)?);
				return Ok(Expr::Range(RangeExpr {
					start: Some(Box::new(start)),
					end: Some(end),
					inclusive: true,
					span: span.merge(&self.last_span),
				}));
			}
			_ => return Ok(start),
		}
	}

	fn is_range_end(&mut self) -> bool
	{
		return matches!(
			self.peek_kind(),
			TokenKind::Comma
				| TokenKind::RightParen
				| TokenKind::RightBracket
				| TokenKind::RightBrace
				| TokenKind::Semicolon
				| TokenKind::FatArrow
		);
	}

	fn parse_additive(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut lhs: Expr = self.parse_multiplicative(allow_struct_init)?;

		loop {
			let op: BinaryOp = match self.peek_kind() {
				TokenKind::Plus => BinaryOp::Add,
				TokenKind::Minus => BinaryOp::Sub,
				_ => break,
			};

			self.next();
			let rhs: Expr = self.parse_multiplicative(allow_struct_init)?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_multiplicative(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut lhs: Expr = self.parse_cast(allow_struct_init)?;

		loop {
			let op: BinaryOp = match self.peek_kind() {
				TokenKind::Star => BinaryOp::Mul,
				TokenKind::Slash => BinaryOp::Div,
				TokenKind::Mod => BinaryOp::Mod,
				_ => break,
			};

			self.next();
			let rhs: Expr = self.parse_cast(allow_struct_init)?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_cast(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		if self.at(&TokenKind::LeftParen) {
			let checkpoint: Peekable<Lexer<'s, 'c>> = self.lexer.clone();
			self.next(); // (

			if let Ok(ty) = self.parse_type()
				&& self.consume(&TokenKind::RightParen)
			{
				let expr: Expr = self.parse_cast(allow_struct_init)?;
				return Ok(Expr::Cast {
					ty: Box::new(ty),
					expr: Box::new(expr),
					span: span.merge(&self.last_span),
				});
			}

			self.lexer = checkpoint;
		}

		return self.parse_unary(allow_struct_init);
	}

	fn parse_unary(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
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
			_ => return self.parse_postfix(allow_struct_init),
		};

		let expr: Expr = self.parse_unary(allow_struct_init)?;
		return Ok(Expr::Unary {
			op,
			expr: Box::new(expr),
			span: span.merge(&self.last_span),
		});
	}

	fn parse_postfix(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek().span();
		let mut expr: Expr = self.parse_primary(allow_struct_init)?;

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
						span: span.merge(&self.last_span),
					};
				}
				TokenKind::LeftBracket => {
					self.next();
					let index: Expr = self.parse_expr()?; // Always allow struct init inside []
					self.expect(&TokenKind::RightBracket)?;
					expr = Expr::Index {
						base: Box::new(expr),
						index: Box::new(index),
						span: span.merge(&self.last_span),
					};
				}
				TokenKind::LeftParen => {
					self.next();
					let args: Vec<Expr> = self.parse_argument_list()?;
					self.expect(&TokenKind::RightParen)?;
					expr = Expr::Call {
						callee: Box::new(expr),
						args,
						span: span.merge(&self.last_span),
					};
				}
				_ => break,
			}
		}

		return Ok(expr);
	}

	fn parse_primary(&mut self, allow_struct_init: bool) -> Result<Expr, ParseError>
	{
		let tok: Token = self.peek().clone();
		let span: Span = tok.span();

		match &tok.kind {
			TokenKind::IntLiteral(n) => {
				self.next();
				return Ok(Expr::Literal {
					value: Literal::Int(*n),
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::FloatLiteral(f) => {
				self.next();
				return Ok(Expr::Literal {
					value: Literal::Float(*f),
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::StringLiteral(s) => {
				self.next();
				return Ok(Expr::Literal {
					value: Literal::String(s.clone()),
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::CharLiteral(c) => {
				self.next();
				return Ok(Expr::Literal {
					value: Literal::Char(*c),
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::True => {
				self.next();
				return Ok(Expr::Literal {
					value: Literal::Bool(true),
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::False => {
				self.next();
				return Ok(Expr::Literal {
					value: Literal::Bool(false),
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::Default => {
				self.next();
				return Ok(Expr::Default {
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::SelfKw => {
				self.next();
				return Ok(Expr::Identifier {
					path: vec!["self".to_string()],
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::Identifier(_) => {
				let path: Vec<String> = self.get_path()?;

				if allow_struct_init && self.at(&TokenKind::LeftBrace) {
					let checkpoint = self.lexer.clone();
					let checkpoint_span = self.last_span;
					let checkpoint_buffered = self.buffered_token.clone();

					self.next(); // {

					let is_struct = self.at(&TokenKind::RightBrace)
						|| (matches!(self.peek_kind(), TokenKind::Identifier(_)) && self.lookahead_for_struct_field());

					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					self.buffered_token = checkpoint_buffered;

					if is_struct {
						self.next(); // {
						let fields: Vec<(String, Expr)> = self.parse_struct_fields()?;
						self.expect(&TokenKind::RightBrace)?;
						return Ok(Expr::StructInit {
							path,
							fields,
							span: span.merge(&self.last_span),
						});
					} else {
						return Ok(Expr::Identifier {
							path,
							span: span.merge(&self.last_span),
						});
					}
				} else {
					return Ok(Expr::Identifier {
						path,
						span: span.merge(&self.last_span),
					});
				}
			}

			TokenKind::LeftParen => {
				self.next();

				if self.consume(&TokenKind::RightParen) {
					return Ok(Expr::Tuple {
						elements: Vec::new(),
						span: span.merge(&self.last_span),
					});
				}

				let first: Expr = self.parse_expr()?; // Always allow struct init inside ()

				if self.consume(&TokenKind::RightParen) {
					return Ok(first);
				}

				if self.consume(&TokenKind::Comma) {
					let mut elements = vec![first];

					if self.consume(&TokenKind::RightParen) {
						return Ok(Expr::Tuple {
							elements,
							span: span.merge(&self.last_span),
						});
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
					return Ok(Expr::Tuple {
						elements,
						span: span.merge(&self.last_span),
					});
				}

				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, "expected ',' or ')' in tuple"),
				});
			}

			TokenKind::LeftBracket => {
				self.next();

				if self.consume(&TokenKind::RightBracket) {
					return Ok(Expr::Array(ArrayLiteral::List {
						elements: Vec::new(),
						span: span.merge(&self.last_span),
					}));
				}

				let first: Expr = self.parse_expr()?; // Always allow struct init inside []

				if self.consume(&TokenKind::Semicolon) {
					let count: Expr = self.parse_expr()?;
					self.expect(&TokenKind::RightBracket)?;
					return Ok(Expr::Array(ArrayLiteral::Repeat {
						value: vec![first],
						count: Box::new(count),
						span: span.merge(&self.last_span),
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
				return Ok(Expr::Array(ArrayLiteral::List {
					elements,
					span: span.merge(&self.last_span),
				}));
			}

			TokenKind::LeftBrace => {
				let block: Block = self.parse_block()?;
				return Ok(Expr::Block(Box::new(block)));
			}

			TokenKind::Unsafe => {
				self.next(); // unsafe
				let block: Block = self.parse_block()?;
				return Ok(Expr::UnsafeBlock(Box::new(block)));
			}

			TokenKind::Switch => {
				self.next(); // switch
				let expr: Expr = self.parse_expr_no_struct()?; // Use no_struct for switch expression
				self.expect(&TokenKind::LeftBrace)?;

				let mut arms: Vec<SwitchArm> = Vec::new();
				while !self.at(&TokenKind::RightBrace) {
					arms.push(self.parse_switch_arm()?);
				}

				self.expect(&TokenKind::RightBrace)?;
				return Ok(Expr::Switch {
					expr: Box::new(expr),
					arms,
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::If => {
				let if_stmt: Stmt = self.parse_if()?;
				return self.stmt_if_to_expr_wrapper(if_stmt);
			}

			TokenKind::Loop => {
				let loop_stmt: Stmt = self.parse_loop()?;
				return Ok(self.stmt_loop_to_expr(loop_stmt));
			}

			TokenKind::Label(label) => {
				self.next(); // label
				self.expect(&TokenKind::Colon)?; // :

				if self.at(&TokenKind::Loop) {
					self.next(); // loop
					let body = self.parse_block()?;
					return Ok(Expr::Loop {
						label: Some(label.to_owned()),
						body: Box::new(body),
						span: span.merge(&self.last_span),
					});
				} else {
					let tok: Token = self.next();
					return Err(ParseError {
						span: tok.span,
						message: tok.format_error(
							self.source,
							&format!(
								"Expected a loop, because only a loop can return a value, and have a label, got: {:?}",
								tok.kind
							),
						),
					});
				}
			}

			_ => {
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, "expected expression"),
				});
			}
		}
	}

	fn stmt_if_to_expr_wrapper(&self, stmt: Stmt) -> Result<Expr, ParseError>
	{
		return match stmt {
			Stmt::If {
				cond,
				then_block,
				else_branch,
				span,
			} => Ok(Expr::If {
				cond: Box::new(cond),
				then_block,
				else_branch: match else_branch {
					Some(b) => match *b {
						Stmt::If { .. } | Stmt::IfVar { .. } => Some(Box::new(self.stmt_if_to_expr_wrapper(*b)?)),
						Stmt::Block(block) => Some(Box::new(Expr::Block(Box::new(block)))),
						Stmt::Expr(expr) => Some(Box::new(expr)),
						_ => {
							return Err(ParseError {
								span: b.span(),
								message: "Expected expression, block, or if statement in else branch".to_string(),
							});
						}
					},
					None => None,
				},
				span,
			}),
			Stmt::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
				span,
			} => Ok(Expr::IfVar {
				pattern,
				expr: Box::new(expr),
				then_block,
				else_branch: match else_branch {
					Some(b) => match *b {
						Stmt::If { .. } | Stmt::IfVar { .. } => Some(Box::new(self.stmt_if_to_expr_wrapper(*b)?)),
						Stmt::Block(block) => Some(Box::new(Expr::Block(Box::new(block)))),
						Stmt::Expr(expr) => Some(Box::new(expr)),
						_ => {
							return Err(ParseError {
								span: b.span(),
								message: "Expected expression, block, or if statement in else branch".to_string(),
							});
						}
					},
					None => None,
				},
				span,
			}),
			_ => unreachable!("Expected if or if var statement"),
		};
	}

	fn stmt_loop_to_expr(&self, stmt: Stmt) -> Expr
	{
		match stmt {
			Stmt::Loop { label, body, span } => {
				return Expr::Loop {
					label,
					body: Box::new(body),
					span,
				};
			}
			_ => unreachable!("Expected loop statement"),
		}
	}

	fn lookahead_for_struct_field(&mut self) -> bool
	{
		if let TokenKind::Identifier(_) = self.peek_kind() {
			let checkpoint: Peekable<Lexer<'s, 'c>> = self.lexer.clone();
			self.next(); // identifier(_)
			let has_equals = self.at(&TokenKind::Equals);
			self.lexer = checkpoint;
			return has_equals;
		} else {
			return false;
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

		return Ok(args);
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

		return Ok(fields);
	}

	fn parse_switch_arm(&mut self) -> Result<SwitchArm, ParseError>
	{
		let span: Span = self.peek().span();

		let pattern: Pattern = self.parse_pattern()?;
		self.expect(&TokenKind::FatArrow)?; // =>

		let body: SwitchBody = if self.at(&TokenKind::LeftBrace) {
			let switch: SwitchBody = SwitchBody::Block(self.parse_block()?);
			self.consume(&TokenKind::Comma);
			switch
		} else {
			let is_stmt = matches!(
				self.peek_kind(),
				TokenKind::Break | TokenKind::Continue | TokenKind::Return
			);

			if is_stmt {
				let stmt = match self.peek_kind() {
					TokenKind::Break => {
						self.next(); // break
						let label: Option<String> = if matches!(self.peek_kind(), TokenKind::Label(_)) {
							let tok: Token = self.next();
							if let TokenKind::Label(l) = tok.kind {
								Some(l)
							} else {
								None
							}
						} else {
							None
						};
						let value: Option<Expr> = if self.at(&TokenKind::Comma) {
							None
						} else {
							Some(self.parse_expr()?)
						};
						Stmt::Break {
							label,
							value,
							span: span.merge(&self.last_span),
						}
					}
					TokenKind::Continue => {
						self.next(); // continue
						let label = if matches!(self.peek_kind(), TokenKind::Label(_)) {
							let tok = self.next();
							if let TokenKind::Label(l) = tok.kind {
								Some(l)
							} else {
								None
							}
						} else {
							None
						};
						Stmt::Continue {
							label,
							span: span.merge(&self.last_span),
						}
					}
					TokenKind::Return => {
						self.next(); // return
						let ret_expr = if self.at(&TokenKind::Comma) {
							None
						} else {
							Some(self.parse_expr()?)
						};
						Stmt::Return {
							value: ret_expr,
							span: span.merge(&self.last_span),
						}
					}
					_ => unreachable!(),
				};

				self.expect(&TokenKind::Comma)?;

				SwitchBody::Block(Block {
					stmts: vec![stmt],
					tail_expr: None,
					span: span.merge(&self.last_span),
				})
			} else {
				let expr: Expr = self.parse_expr()?;
				self.expect(&TokenKind::Comma)?;
				SwitchBody::Expr(expr)
			}
		};

		return Ok(SwitchArm {
			pattern,
			body,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_pattern(&mut self) -> Result<Pattern, ParseError>
	{
		let span: Span = self.peek().span();
		let mut patterns: Vec<Pattern> = vec![self.parse_pattern_no_or()?];

		while self.consume(&TokenKind::Pipe) {
			patterns.push(self.parse_pattern_no_or()?);
		}

		if patterns.len() == 1 {
			return Ok(patterns.into_iter().next().expect("len == 1, so should not error"));
		} else {
			return Ok(Pattern::Or {
				patterns,
				span: span.merge(&self.last_span),
			});
		}
	}

	fn parse_pattern_no_or(&mut self) -> Result<Pattern, ParseError>
	{
		let span: Span = self.peek().span();
		let tok: Token = self.peek().clone();

		match &tok.kind {
			TokenKind::Underscore => {
				self.next();
				if self.consume(&TokenKind::Colon) {
					let _ignored_type = self.parse_type()?;
				}
				return Ok(Pattern::Wildcard { span });
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
					return Ok(Pattern::Variant {
						path,
						args,
						span: span.merge(&self.last_span),
					});
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
					return Ok(Pattern::Struct {
						path,
						fields,
						span: span.merge(&self.last_span),
					});
				} else if self.at(&TokenKind::Colon) {
					if path.len() != 1 {
						return Err(ParseError {
							span: tok.span,
							message: tok
								.format_error(self.source, "binding patterns must be simple identifiers, not paths"),
						});
					}

					self.next(); // :
					let ty: Type = self.parse_type()?;

					let call_constructor: bool = if self.consume(&TokenKind::LeftParen) {
						self.expect(&TokenKind::RightParen)?;
						true
					} else {
						false
					};

					return Ok(Pattern::TypedIdentifier {
						name: path[0].clone(),
						ty,
						call_constructor,
						span: span.merge(&self.last_span),
					});
				} else {
					return Ok(Pattern::Variant {
						path,
						args: Vec::new(),
						span: span.merge(&self.last_span),
					});
				}
			}

			TokenKind::LeftParen => {
				self.next(); // (

				if self.consume(&TokenKind::RightParen) {
					return Ok(Pattern::Tuple {
						patterns: Vec::new(),
						span: span.merge(&self.last_span),
					});
				}

				let mut patterns: Vec<Pattern> = vec![self.parse_pattern()?];

				if self.consume(&TokenKind::Comma) {
					if !self.at(&TokenKind::RightParen) {
						loop {
							patterns.push(self.parse_pattern()?);
							if !self.consume(&TokenKind::Comma) {
								break;
							}
							if self.at(&TokenKind::RightParen) {
								break;
							}
						}
					}
					self.expect(&TokenKind::RightParen)?;
					return Ok(Pattern::Tuple {
						patterns,
						span: span.merge(&self.last_span),
					});
				} else {
					self.expect(&TokenKind::RightParen)?;
					return Ok(patterns.into_iter().next().expect(
						"this should already be cought in the code before, because of `vec![self.parse_pattern()?]",
					));
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

					return Ok(Pattern::Range(RangeExpr {
						start: Some(Box::new(Expr::Literal {
							value: Literal::Int(*n),
							span: tok.span(),
						})),
						end,
						inclusive,
						span: span.merge(&self.last_span),
					}));
				} else {
					return Ok(Pattern::Literal {
						value: Literal::Int(*n),
						span: tok.span(),
					});
				}
			}

			TokenKind::True => {
				self.next();
				return Ok(Pattern::Literal {
					value: Literal::Bool(true),
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::False => {
				self.next();
				return Ok(Pattern::Literal {
					value: Literal::Bool(false),
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::StringLiteral(s) => {
				self.next();
				return Ok(Pattern::Literal {
					value: Literal::String(s.clone()),
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::CharLiteral(c) => {
				self.next();
				return Ok(Pattern::Literal {
					value: Literal::Char(*c),
					span: span.merge(&self.last_span),
				});
			}

			_ => {
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, "expected pattern"),
				});
			}
		}
	}

	pub fn parse_block(&mut self) -> Result<Block, ParseError>
	{
		self.expect(&TokenKind::LeftBrace)?;

		let ret = self.parse_block_content()?;

		self.expect(&TokenKind::RightBrace)?;
		return Ok(ret);
	}

	fn parse_block_content(&mut self) -> Result<Block, ParseError>
	{
		let span: Span = self.peek().span();
		let mut stmts: Vec<Stmt> = Vec::new();
		let mut tail_expr: Option<Box<Expr>> = None;

		while !self.at(&TokenKind::RightBrace) {
			let saved_label: Option<String> = if matches!(self.peek_kind(), TokenKind::Label(_)) {
				let tok = self.next();
				if let TokenKind::Label(l) = tok.kind {
					self.expect(&TokenKind::Colon)?;
					Some(l)
				} else {
					None
				}
			} else {
				None
			};

			let kind = self.peek_kind().clone();

			match kind {
				TokenKind::Var | TokenKind::Const => {
					let var_decl: VariableDecl = self.parse_var_decl()?;
					self.expect(&TokenKind::Semicolon)?;
					stmts.push(Stmt::VariableDecl(var_decl));
				}

				TokenKind::Return => {
					self.next();
					let ret_expr: Option<Expr> = if self.at(&TokenKind::Semicolon) {
						None
					} else {
						Some(self.parse_expr()?)
					};
					self.expect(&TokenKind::Semicolon)?;
					stmts.push(Stmt::Return {
						value: ret_expr,
						span: span.merge(&self.last_span),
					});
				}

				TokenKind::Break => {
					self.next(); // break

					let label: Option<String> = if matches!(self.peek_kind(), TokenKind::Label(_)) {
						let tok: Token = self.next();
						if let TokenKind::Label(l) = tok.kind {
							Some(l)
						} else {
							None
						}
					} else {
						None
					};

					let value = if self.at(&TokenKind::Semicolon) {
						None
					} else {
						Some(self.parse_expr()?)
					};

					self.expect(&TokenKind::Semicolon)?;
					stmts.push(Stmt::Break {
						label,
						value,
						span: span.merge(&self.last_span),
					});
				}

				TokenKind::Continue => {
					self.next(); // continue

					let label: Option<String> = if matches!(self.peek_kind(), TokenKind::Label(_)) {
						let tok: Token = self.next();
						if let TokenKind::Label(l) = tok.kind {
							Some(l)
						} else {
							None
						}
					} else {
						None
					};

					self.expect(&TokenKind::Semicolon)?;
					stmts.push(Stmt::Continue {
						label,
						span: span.merge(&self.last_span),
					});
				}

				TokenKind::While => {
					let mut while_stmt = self.parse_while()?;
					if let Some(lbl) = saved_label {
						while_stmt.set_label(lbl);
					}
					stmts.push(while_stmt);
				}

				TokenKind::For => {
					let mut for_stmt = self.parse_for()?;
					if let Some(lbl) = saved_label {
						for_stmt.set_label(lbl);
					}
					stmts.push(for_stmt);
				}

				TokenKind::Loop => {
					let mut loop_stmt: Stmt = self.parse_loop()?;
					if let Some(lbl) = saved_label {
						loop_stmt.set_label(lbl);
					}

					if self.at(&TokenKind::RightBrace) {
						tail_expr = Some(Box::new(self.stmt_loop_to_expr(loop_stmt)));
						break;
					} else {
						self.consume(&TokenKind::Semicolon);
						stmts.push(loop_stmt);
					}
				}

				TokenKind::If => {
					let checkpoint: Peekable<Lexer<'s, 'c>> = self.lexer.clone();
					let checkpoint_span: Span = self.last_span;
					let checkpoint_buffered: Option<Token> = self.buffered_token.clone();

					self.next(); // if

					if self.consume(&TokenKind::Var) {
						let pattern: Pattern = self.parse_pattern()?;
						self.expect(&TokenKind::Equals)?;
						let expr: Expr = self.parse_expr()?;
						let then_block: Block = self.parse_block()?;

						let else_branch: Option<Box<Stmt>> = if self.consume(&TokenKind::Else) {
							if self.at(&TokenKind::If) {
								Some(Box::new(self.parse_if()?))
							} else {
								let block: Block = self.parse_block()?;
								Some(Box::new(Stmt::Block(block)))
							}
						} else {
							None
						};

						let if_var_stmt: Stmt = Stmt::IfVar {
							pattern,
							expr,
							then_block,
							else_branch,
							span,
						};

						if self.consume(&TokenKind::Semicolon) {
							stmts.push(if_var_stmt);
						} else if self.at(&TokenKind::RightBrace) {
							tail_expr = Some(Box::new(self.stmt_if_to_expr_wrapper(if_var_stmt)?));
							break;
						} else {
							stmts.push(if_var_stmt);
						}
					} else {
						self.lexer = checkpoint;
						self.last_span = checkpoint_span;
						self.buffered_token = checkpoint_buffered;

						let if_stmt: Stmt = self.parse_if()?;

						if self.consume(&TokenKind::Semicolon) {
							stmts.push(if_stmt);
						} else if self.at(&TokenKind::RightBrace) {
							tail_expr = Some(Box::new(self.stmt_if_to_expr_wrapper(if_stmt)?));
							break;
						} else {
							stmts.push(if_stmt);
						}
					}
				}

				TokenKind::Delete => {
					stmts.push(Stmt::Delete {
						path: self.parse_delete()?,
						span: span.merge(&self.last_span),
					});
					self.expect(&TokenKind::Semicolon)?;
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

				TokenKind::Directive(_) => {
					let directive_node: DirectiveNode = self.parse_directive_node()?;

					if directive_node.body.is_none() {
						self.expect(&TokenKind::Semicolon)?;
					}

					stmts.push(Stmt::Directive(directive_node));
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
							span: span.merge(&self.last_span),
						});
					} else if let Expr::Block(block) = expr {
						if self.consume(&TokenKind::Semicolon) {
							stmts.push(Stmt::Block(*block));
						} else if self.at(&TokenKind::RightBrace) {
							tail_expr = Some(Box::new(Expr::Block(block)));
							break;
						} else {
							stmts.push(Stmt::Block(*block));
						}
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
							// TODO, check why 2 the same paths
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
		return Ok(Block {
			stmts,
			tail_expr,
			span: span.merge(&self.last_span),
		});
	}

	const fn expr_needs_semicolon(&self, expr: &Expr) -> bool
	{
		return !matches!(
			expr,
			Expr::Block { .. } | Expr::Switch { .. } | Expr::If { .. } | Expr::IfVar { .. } | Expr::Loop { .. }
		);
	}

	fn is_assignment_op(&mut self) -> bool
	{
		return matches!(
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
		);
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
		return Ok(op);
	}

	fn parse_if(&mut self) -> Result<Stmt, ParseError>
	{
		let span: Span = self.peek().span();
		self.expect(&TokenKind::If)?;

		return if self.consume(&TokenKind::Var) {
			let pattern: Pattern = self.parse_pattern()?;
			self.expect(&TokenKind::Equals)?;
			let expr: Expr = self.parse_expr()?;
			let then_block: Block = self.parse_block()?;

			let else_branch: Option<Box<Stmt>> = if self.consume(&TokenKind::Else) {
				if self.at(&TokenKind::If) {
					Some(Box::new(self.parse_if()?))
				} else {
					let block: Block = self.parse_block()?;
					Some(Box::new(Stmt::Block(block)))
				}
			} else {
				None
			};

			Ok(Stmt::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
				span: span.merge(&self.last_span),
			})
		} else {
			let cond: Expr = self.parse_expr_no_struct()?;
			let then_block: Block = self.parse_block()?;

			let else_branch: Option<Box<Stmt>> = if self.consume(&TokenKind::Else) {
				if self.at(&TokenKind::If) {
					Some(Box::new(self.parse_if()?))
				} else {
					let block: Block = self.parse_block()?;
					Some(Box::new(Stmt::Block(block)))
				}
			} else {
				None
			};

			Ok(Stmt::If {
				cond,
				then_block,
				else_branch,
				span: span.merge(&self.last_span),
			})
		};
	}

	fn parse_while(&mut self) -> Result<Stmt, ParseError>
	{
		let span: Span = self.peek().span();
		self.expect(&TokenKind::While)?;

		if self.consume(&TokenKind::Var) {
			let pattern: Pattern = self.parse_pattern()?;
			self.expect(&TokenKind::Equals)?;
			let expr: Expr = self.parse_expr()?;
			let body: Block = self.parse_block()?;

			return Ok(Stmt::WhileVarLoop {
				label: None,
				pattern,
				expr,
				body,
				span: span.merge(&self.last_span),
			});
		} else {
			let cond = self.parse_expr_no_struct()?;
			let body = self.parse_block()?;

			return Ok(Stmt::While {
				label: None,
				cond,
				body,
				span: span.merge(&self.last_span),
			});
		}
	}

	fn parse_for(&mut self) -> Result<Stmt, ParseError>
	{
		let span: Span = self.peek().span();
		self.expect(&TokenKind::For)?;
		let name: Vec<Ident> = self.get_path()?;
		self.expect(&TokenKind::In)?;
		let iter = self.parse_expr_no_struct()?;
		let body = self.parse_block()?;

		return Ok(Stmt::For {
			label: None,
			name,
			iter,
			body,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_loop(&mut self) -> Result<Stmt, ParseError>
	{
		let span: Span = self.peek().span();
		self.expect(&TokenKind::Loop)?;
		return Ok(Stmt::Loop {
			label: None,
			body: self.parse_block()?,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_function_decl(&mut self) -> Result<FunctionDecl, ParseError>
	{
		let mut span: Span = self.peek().span();
		let signature: FunctionSignature = self.parse_function_signature()?;
		let body: Option<Block> = if self.at(&TokenKind::Semicolon) {
			None
		} else {
			Some(self.parse_block()?)
		};
		span = span.merge(&self.last_span);
		return Ok(FunctionDecl {
			signature,
			body,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_function_signature(&mut self) -> Result<FunctionSignature, ParseError>
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

		return Ok(FunctionSignature {
			modifiers,
			name,
			generics,
			params,
			return_type,
			where_clause,
			heap_func,
			span: span.merge(&self.last_span),
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
			let loop_span: Span = self.peek().span();
			match self.peek_kind() {
				TokenKind::Ampersand => {
					self.next(); // &

					let mutable: bool = self.consume(&TokenKind::Mut);

					self.expect(&TokenKind::SelfKw)?;

					let self_type: Type = Type {
						modifiers: Vec::new(),
						core: Box::new(TypeCore::Reference {
							mutable,
							inner: Box::new(TypeCore::Base {
								path: vec!["Self".to_string()],
								generics: Vec::new(),
							}),
						}),
						span: loop_span.merge(&self.last_span),
					};

					params.push(Param {
						ty: self_type,
						name: vec!["self".to_string()],
						span: loop_span.merge(&self.last_span),
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
						span: loop_span.merge(&self.last_span),
					};

					params.push(Param {
						ty: self_type,
						name: vec!["self".to_string()],
						span: loop_span.merge(&self.last_span),
					});
				}
				_ => {
					let name: Vec<String> = self.get_path()?;

					self.expect(&TokenKind::Colon)?;

					let ty: Type = self.parse_type()?;

					params.push(Param {
						ty,
						name,
						span: loop_span.merge(&self.last_span),
					});
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
		return Ok(params);
	}

	fn parse_modifiers(&mut self) -> Result<Vec<Modifier>, ParseError>
	{
		let mut ret: Vec<Modifier> = Vec::new();

		loop {
			let tok: &Token = self.peek();
			match &tok.kind {
				TokenKind::Directive(_) => {
					ret.push(Modifier::Directive(self.parse_directive()?));
				}
				TokenKind::Pub => {
					ret.push(Modifier::Pub);
					self.next();
				}
				TokenKind::Unsafe => {
					ret.push(Modifier::Unsafe);
					self.next();
				}
				TokenKind::Inline => {
					ret.push(Modifier::Inline);
					self.next();
				}
				TokenKind::Const => {
					ret.push(Modifier::Const);
					self.next();
				}
				_ => return Ok(ret),
			}
		}
	}

	fn parse_struct(&mut self) -> Result<StructDecl, ParseError>
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

		let mut fields: Vec<(Type, Ident, Option<Expr>)> = Vec::new();

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

			let default_value: Option<Expr> = if self.consume(&TokenKind::Equals) {
				Some(self.parse_expr()?)
			} else {
				None
			};

			fields.push((field_type, field_name, default_value));

			if *self.peek_kind() == TokenKind::RightBrace {
				break;
			}
			self.expect(&TokenKind::Comma)?;
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(StructDecl {
			modifiers,
			name,
			fields,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_union(&mut self) -> Result<UnionDecl, ParseError>
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

		return Ok(UnionDecl {
			modifiers,
			name,
			fields,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_namespace(&mut self) -> Result<NamespaceDecl, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Namespace)?;
		let name: Vec<Ident> = self.get_path()?;
		self.expect(&TokenKind::LeftBrace)?;
		let body: TopLevelBlock = self.parse_program()?;
		self.expect(&TokenKind::RightBrace)?;
		return Ok(NamespaceDecl {
			modifiers,
			name,
			body,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_enum(&mut self) -> Result<EnumDecl, ParseError>
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

		return Ok(EnumDecl {
			modifiers,
			name,
			variants: fields,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_variant(&mut self) -> Result<VariantDecl, ParseError>
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

		let mut fields: Vec<(Option<Type>, Ident, Option<Expr>)> = Vec::new();

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
				let ty: Option<Type> = Some(self.parse_type()?);
				self.expect(&TokenKind::RightParen)?;
				ty
			} else {
				None
			};

			let field_value: Option<Expr> = if self.at(&TokenKind::Equals) {
				self.next();
				Some(self.parse_expr()?)
			} else {
				None
			};

			fields.push((field_type, field_name, field_value));

			if *self.peek_kind() == TokenKind::RightBrace {
				break;
			}
			self.expect(&TokenKind::Comma)?;
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(VariantDecl {
			modifiers,
			name,
			variants: fields,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_impl(&mut self) -> Result<ImplDecl, ParseError>
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

		let (trait_path, target): (Option<ImplTarget>, ImplTarget) = if self.consume(&TokenKind::For) {
			let target: ImplTarget = self.parse_impl_target()?;
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

		let mut body: Vec<ImplItem> = Vec::new();

		while !self.at(&TokenKind::RightBrace) {
			let item = self.parse_impl_item()?;
			body.push(item);
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(ImplDecl {
			modifiers,
			generics,
			target,
			trait_path,
			where_clause,
			body,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_impl_target(&mut self) -> Result<ImplTarget, ParseError>
	{
		let span: Span = self.peek().span();
		let path: Vec<Ident> = self.get_path()?;

		let generics: Vec<Type> = if self.at(&TokenKind::LessThan) {
			self.parse_type_generics()?
		} else {
			Vec::new()
		};

		return Ok(ImplTarget {
			path,
			generics,
			span: span.merge(&self.last_span),
		});
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

		return Ok(generics);
	}

	fn parse_impl_item(&mut self) -> Result<ImplItem, ParseError>
	{
		let decl_kind: DeclKind = self.peek_declaration_kind()?;

		let node: ImplItem = match decl_kind {
			DeclKind::Function => {
				let func_decl: FunctionDecl = self.parse_function_decl()?;
				ImplItem::Function(func_decl)
			}
			DeclKind::TypeAlias => {
				let type_alias: TypeAliasDecl = self.parse_type_alias()?;
				self.expect(&TokenKind::Semicolon)?;
				ImplItem::TypeAlias(type_alias)
			}
			DeclKind::Variable => {
				let var_decl = self.parse_var_decl()?;
				self.expect(&TokenKind::Semicolon)?;
				ImplItem::Const(var_decl)
			}
			_ => {
				let tok = self.peek().clone();
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, &format!("unexpected item in impl block: {:?}", tok.kind)),
				});
			}
		};

		return Ok(node);
	}

	fn parse_where_clause(&mut self) -> Result<Vec<WhereConstraint>, ParseError>
	{
		let mut constraints: Vec<WhereConstraint> = Vec::new();

		loop {
			let loop_span: Span = self.peek().span();
			let ty: Vec<Ident> = self.get_path()?;

			self.expect(&TokenKind::Colon)?;

			let mut bounds: Vec<Vec<Ident>> = Vec::new();

			loop {
				let bound: Vec<Ident> = self.get_path()?;
				bounds.push(bound);

				if !self.consume(&TokenKind::Plus) {
					break;
				}
			}

			constraints.push(WhereConstraint {
				ty,
				bounds,
				span: loop_span.merge(&self.last_span),
			});

			if !self.consume(&TokenKind::Comma) {
				break;
			}

			if self.at(&TokenKind::LeftBrace) {
				break;
			}
		}

		return Ok(constraints);
	}

	fn parse_type_alias(&mut self) -> Result<TypeAliasDecl, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Type)?;

		let name = self.get_path()?;

		self.expect(&TokenKind::Equals)?;
		let ty = self.parse_type()?;

		return Ok(TypeAliasDecl {
			modifiers,
			name,
			ty,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_trait(&mut self) -> Result<TraitDecl, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Trait)?;

		let name: Vec<Ident> = self.get_path()?;

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

		let mut items: Vec<TraitItem> = Vec::new();

		while !self.at(&TokenKind::RightBrace) {
			let item = self.parse_trait_item()?;
			items.push(item);
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(TraitDecl {
			modifiers,
			name,
			generics,
			super_traits,
			items,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_trait_bounds(&mut self) -> Result<Vec<Vec<Ident>>, ParseError>
	{
		let mut bounds: Vec<Vec<Ident>> = Vec::new();

		loop {
			let bound: Vec<Ident> = self.get_path()?;
			bounds.push(bound);

			if !self.consume(&TokenKind::Plus) {
				break;
			}
		}

		return Ok(bounds);
	}

	fn parse_trait_item(&mut self) -> Result<TraitItem, ParseError>
	{
		let span: Span = self.peek().span;

		let decl_kind: DeclKind = self.peek_declaration_kind()?;

		let node: TraitItem = match decl_kind {
			DeclKind::Function => {
				let signature: FunctionSignature = self.parse_function_signature()?;

				let body: Option<Block> = if self.at(&TokenKind::LeftBrace) {
					Some(self.parse_block()?)
				} else {
					self.expect(&TokenKind::Semicolon)?;
					None
				};

				TraitItem::Function {
					signature,
					body,
					span: span.merge(&self.last_span),
				}
			}
			DeclKind::TypeAlias => {
				let type_alias: TypeAliasDecl = self.parse_trait_type_alias()?;
				self.expect(&TokenKind::Semicolon)?;
				TraitItem::TypeAlias(type_alias)
			}
			DeclKind::Variable => {
				let var_decl: VariableDecl = self.parse_var_decl()?;
				self.expect(&TokenKind::Semicolon)?;
				TraitItem::Const(var_decl)
			}
			_ => {
				let tok: Token = self.next();
				return Err(ParseError {
					span: tok.span,
					message: tok.format_error(self.source, &format!("unexpected item in trait block: {:?}", tok.kind)),
				});
			}
		};

		return Ok(node);
	}

	fn parse_trait_type_alias(&mut self) -> Result<TypeAliasDecl, ParseError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Type)?;

		let name: Vec<String> = self.get_path()?;

		let ty: Type = if self.consume(&TokenKind::Equals) {
			self.parse_type()?
		} else {
			let tok: Token = self.next();
			return Err(ParseError {
				span: tok.span(),
				message: tok.format_error(self.source, &format!("Expected = for type alias, got {:?}", tok.kind)),
			});
		};

		return Ok(TypeAliasDecl {
			modifiers,
			name,
			ty,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_delete(&mut self) -> Result<Vec<Ident>, ParseError>
	{
		self.expect(&TokenKind::Delete)?;

		return self.get_path();
	}
}

use std::fmt;

struct IndentWriter
{
	indent_level: usize,
	indent_str: &'static str,
}

impl IndentWriter
{
	const fn new() -> Self
	{
		return Self {
			indent_level: 0,
			indent_str: "    ", // 4 spaces
		};
	}

	const fn indent(&mut self)
	{
		self.indent_level += 1;
	}

	fn dedent(&mut self)
	{
		debug_assert!(self.indent_level > 0);
		self.indent_level -= 1;
	}

	fn write_indent(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		for _ in 0..self.indent_level {
			write!(f, "{}", self.indent_str)?;
		}
		return Ok(());
	}
}

impl fmt::Display for Program
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		let mut writer: IndentWriter = IndentWriter::new();
		for item in &self.items {
			write_top_level_decl(f, &mut writer, item)?;
			writeln!(f)?; // Add blank line between top-level items
		}
		return Ok(());
	}
}

fn write_top_level_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, decl: &TopLevelDecl) -> fmt::Result
{
	match decl {
		TopLevelDecl::Function(func) => return write_function_decl(f, w, func),
		TopLevelDecl::VariableDecl(var) => {
			write_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		TopLevelDecl::Struct(s) => return write_struct_decl(f, w, s),
		TopLevelDecl::Union(u) => return write_union_decl(f, w, u),
		TopLevelDecl::Enum(e) => return write_enum_decl(f, w, e),
		TopLevelDecl::Variant(v) => return write_variant_decl(f, w, v),
		TopLevelDecl::TypeAlias(t) => {
			write_type_alias_decl(f, w, t)?;
			return write!(f, ";");
		}
		TopLevelDecl::Trait(t) => return write_trait_decl(f, w, t),
		TopLevelDecl::Namespace(n) => return write_namespace_decl(f, w, n),
		TopLevelDecl::Impl(i) => return write_impl_decl(f, w, i),
		TopLevelDecl::Directive(d) => return write!(f, "{};", d),
	}
}

impl fmt::Display for Modifier
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			Modifier::Pub => return write!(f, "pub"),
			Modifier::Unsafe => return write!(f, "unsafe"),
			Modifier::Inline => return write!(f, "inline"),
			Modifier::Const => return write!(f, "const"),
			Modifier::Volatile => return write!(f, "volatile"),
			Modifier::Directive(d) => return write!(f, "{}", d),
		}
	}
}

impl std::fmt::Display for DirectiveNode
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{}", self.directive)?;

		if let Some(body) = &self.body {
			write!(f, " ")?;
			match body {
				BlockContent::Block(_block) => {
					// Format block inline or with braces
					write!(f, "{{ ... }}")?;
				}
				BlockContent::TopLevelBlock(top_level) => {
					write!(f, "{{ {} items }}", top_level.items.len())?;
				}
			}
		}

		return Ok(());
	}
}

impl std::fmt::Display for Directive
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match self {
			Directive::Import(path) => return write!(f, "@import \"{}\"", path),
			Directive::Use(path) => {
				write!(f, "@use ")?;
				for (i, segment) in path.iter().enumerate() {
					if i > 0 {
						write!(f, "::")?;
					}
					write!(f, "{}", segment)?;
				}
				return Ok(());
			}
			Directive::Custom { name, args } => {
				write!(f, "@{}", name)?;
				if !args.is_empty() {
					write!(f, "(")?;
					for (i, arg) in args.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", arg)?;
					}
					write!(f, ")")?;
				}
				return Ok(());
			}
		}
	}
}

fn write_function_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, func: &FunctionDecl) -> fmt::Result
{
	write_function_signature(f, w, &func.signature)?;

	if let Some(body) = &func.body {
		write!(f, " ")?;
		write_block(f, w, body)?;
	} else {
		write!(f, ";")?;
	}

	return Ok(());
}

fn write_function_signature(f: &mut fmt::Formatter<'_>, _w: &mut IndentWriter, sig: &FunctionSignature) -> fmt::Result
{
	for modifier in &sig.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "fn")?;

	if sig.heap_func {
		write!(f, "!")?;
	}

	write!(f, " {}", sig.name.join("::"))?;

	if !sig.generics.is_empty() {
		write!(f, "<{}>", sig.generics.join(", "))?;
	}

	write!(f, "(")?;
	for (i, param) in sig.params.iter().enumerate() {
		if i > 0 {
			write!(f, ", ")?;
		}
		write!(f, "{}", param)?;
	}
	write!(f, ")")?;

	if let Some(ret_ty) = &sig.return_type {
		write!(f, " -> {}", ret_ty)?;
	}

	if !sig.where_clause.is_empty() {
		write!(f, " where ")?;
		for (i, constraint) in sig.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}

	return Ok(());
}

impl fmt::Display for Param
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "{}: {}", self.name.join("::"), self.ty);
	}
}

impl fmt::Display for Type
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		for modifier in &self.modifiers {
			write!(f, "{} ", modifier)?;
		}

		return write!(f, "{}", self.core);
	}
}

impl fmt::Display for TypeCore
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			TypeCore::Base { path, generics } => {
				write!(f, "{}", path.join("::"))?;
				if !generics.is_empty() {
					write!(f, "<")?;
					for (i, generic) in generics.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", generic)?;
					}
					write!(f, ">")?;
				}
				return Ok(());
			}
			TypeCore::Reference { mutable, inner } => {
				write!(f, "&")?;
				if *mutable {
					write!(f, "mut ")?;
				}
				return write!(f, "{}", inner);
			}
			TypeCore::Pointer { inner } => return write!(f, "{}*", inner),
			TypeCore::Array { inner, size } => return write!(f, "{}[{}]", inner, size),
			TypeCore::Tuple(types) => {
				write!(f, "(")?;
				for (i, ty) in types.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", ty)?;
				}
				return write!(f, ")");
			}
		}
	}
}

fn write_variable_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, var: &VariableDecl) -> fmt::Result
{
	if var.comp_const {
		write!(f, "const ")?;
	} else {
		write!(f, "var ")?;
	}

	write!(f, "{}", var.pattern)?;

	if let Some(init) = &var.init {
		write!(f, " = ")?;
		write_expr(f, w, init)?;
	}

	return Ok(());
}

impl fmt::Display for Pattern
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			Pattern::Wildcard { .. } => return write!(f, "_"),
			Pattern::Literal { value: lit, .. } => return write!(f, "{}", lit),
			Pattern::TypedIdentifier {
				name,
				ty,
				call_constructor,
				..
			} => {
				write!(f, "{}: {}", name, ty)?;
				if *call_constructor {
					write!(f, "()")?;
				}
				return Ok(());
			}
			Pattern::Variant { path, args, .. } => {
				write!(f, "{}", path.join("::"))?;
				if !args.is_empty() {
					write!(f, "(")?;
					for (i, arg) in args.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", arg)?;
					}
					write!(f, ")")?;
				}
				return Ok(());
			}
			Pattern::Tuple { patterns, .. } => {
				write!(f, "(")?;
				for (i, pat) in patterns.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", pat)?;
				}
				return write!(f, ")");
			}
			Pattern::Struct { path, fields, .. } => {
				write!(f, "{} {{", path.join("::"))?;
				for (i, (name, pat)) in fields.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}: {}", name, pat)?;
				}
				return write!(f, "}}");
			}
			Pattern::Range(range) => return write!(f, "{}", range),
			Pattern::Or { patterns, .. } => {
				for (i, pat) in patterns.iter().enumerate() {
					if i > 0 {
						write!(f, " | ")?;
					}
					write!(f, "{}", pat)?;
				}
				return Ok(());
			}
		}
	}
}

impl fmt::Display for Expr
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			Expr::Identifier { path, .. } => return write!(f, "{}", path.join("::")),
			Expr::Literal { value: lit, .. } => return write!(f, "{}", lit),
			Expr::Default { .. } => return write!(f, "default"),
			Expr::Unary { op, expr, .. } => match op {
				UnaryOp::Neg => return write!(f, "-{}", expr),
				UnaryOp::Not => return write!(f, "!{}", expr),
				UnaryOp::Deref => return write!(f, "*{}", expr),
				UnaryOp::Addr { mutable } => {
					if *mutable {
						return write!(f, "&mut {}", expr);
					} else {
						return write!(f, "&{}", expr);
					}
				}
			},
			Expr::Binary { op, lhs, rhs, .. } => return write!(f, "({} {} {})", lhs, op, rhs),
			Expr::Cast { ty, expr, .. } => return write!(f, "({}) {}", ty, expr),
			Expr::Call { callee, args, .. } => {
				write!(f, "{}(", callee)?;
				for (i, arg) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", arg)?;
				}
				return write!(f, ")");
			}
			Expr::Field { base, name, .. } => return write!(f, "{}.{}", base, name),
			Expr::Index { base, index, .. } => return write!(f, "{}[{}]", base, index),
			Expr::Range(range) => return write!(f, "{}", range),
			Expr::Tuple { elements: exprs, .. } => {
				write!(f, "(")?;
				for (i, expr) in exprs.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", expr)?;
				}
				return write!(f, ")");
			}
			Expr::Array(arr) => return write!(f, "{}", arr),
			Expr::StructInit { path, fields, .. } => {
				write!(f, "{} {{", path.join("::"))?;
				for (i, (name, expr)) in fields.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{} = {}", name, expr)?;
				}
				return write!(f, "}}");
			}
			Expr::Block(block) => {
				let mut w = IndentWriter::new();
				return write_block(f, &mut w, block);
			}
			Expr::UnsafeBlock(block) => {
				write!(f, "unsafe ")?;
				let mut w = IndentWriter::new();
				return write_block(f, &mut w, block);
			}
			Expr::Switch { expr, arms, .. } => {
				let mut w = IndentWriter::new();
				return write_switch(f, &mut w, expr, arms);
			}
			Expr::If {
				cond,
				then_block,
				else_branch,
				..
			} => {
				write!(f, "if {} ", cond)?;
				let mut w = IndentWriter::new();
				write_block(f, &mut w, then_block)?;
				if let Some(else_expr) = else_branch {
					write!(f, " else ")?;
					match else_expr.as_ref() {
						Expr::Block(b) => write_block(f, &mut w, b)?,
						_ => write!(f, "{}", else_expr)?,
					}
				}
				return Ok(());
			}

			Expr::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
				..
			} => {
				write!(f, "if var {} = {} ", pattern, expr)?;
				let mut w = IndentWriter::new();
				write_block(f, &mut w, then_block)?;
				if let Some(else_expr) = else_branch {
					write!(f, " else ")?;
					match else_expr.as_ref() {
						Expr::Block(b) => write_block(f, &mut w, b)?,
						_ => write!(f, "{}", else_expr)?,
					}
				}
				return Ok(());
			}
			Expr::Loop { label, body, .. } => {
				if let Some(lbl) = label {
					write!(f, "'{}: ", lbl)?;
				}
				write!(f, "loop ")?;
				let mut w = IndentWriter::new();
				return write_block(f, &mut w, body);
			}
		}
	}
}

fn write_switch(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, expr: &Expr, arms: &[SwitchArm]) -> fmt::Result
{
	write!(f, "switch ")?;
	write_expr(f, w, expr)?;
	writeln!(f, " {{")?;
	w.indent();

	for arm in arms {
		w.write_indent(f)?;
		write!(f, "{} => ", arm.pattern)?;
		match &arm.body {
			SwitchBody::Expr(expr) => {
				write_expr(f, w, expr)?;
				writeln!(f, ",")?;
			}
			SwitchBody::Block(b) => {
				write_block(f, w, b)?;
				writeln!(f, ",")?;
			}
		}
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

impl fmt::Display for Literal
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			Literal::Int(n) => return write!(f, "{}", n),
			Literal::Float(fl) => return write!(f, "{}", fl),
			Literal::Bool(b) => return write!(f, "{}", b),
			Literal::String(s) => return write!(f, "\"{}\"", s),
			Literal::Char(c) => return write!(f, "'{}'", c),
		}
	}
}

impl fmt::Display for ArrayLiteral
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			ArrayLiteral::List { elements, .. } => {
				write!(f, "[")?;
				for (i, expr) in elements.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", expr)?;
				}
				return write!(f, "]");
			}
			ArrayLiteral::Repeat { value, count, .. } => {
				write!(f, "[")?;
				for (i, expr) in value.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", expr)?;
				}
				return write!(f, "; {}]", count);
			}
		}
	}
}

impl fmt::Display for BinaryOp
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			BinaryOp::LogicalOr => return write!(f, "||"),
			BinaryOp::LogicalAnd => return write!(f, "&&"),
			BinaryOp::Eq => return write!(f, "=="),
			BinaryOp::Ne => return write!(f, "!="),
			BinaryOp::Lt => return write!(f, "<"),
			BinaryOp::Gt => return write!(f, ">"),
			BinaryOp::Le => return write!(f, "<="),
			BinaryOp::Ge => return write!(f, ">="),
			BinaryOp::Add => return write!(f, "+"),
			BinaryOp::Sub => return write!(f, "-"),
			BinaryOp::Mul => return write!(f, "*"),
			BinaryOp::Div => return write!(f, "/"),
			BinaryOp::Mod => return write!(f, "%"),
			BinaryOp::BitAnd => return write!(f, "&"),
			BinaryOp::BitOr => return write!(f, "|"),
			BinaryOp::BitXor => return write!(f, "^"),
			BinaryOp::Shl => return write!(f, "<<"),
			BinaryOp::Shr => return write!(f, ">>"),
		}
	}
}

impl fmt::Display for RangeExpr
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		if let Some(start) = &self.start {
			write!(f, "{}", start)?;
		}

		if self.inclusive {
			write!(f, "..=")?;
		} else {
			write!(f, "..")?;
		}

		if let Some(end) = &self.end {
			write!(f, "{}", end)?;
		}

		return Ok(());
	}
}

fn write_block(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, block: &Block) -> fmt::Result
{
	writeln!(f, "{{")?;
	w.indent();

	for stmt in &block.stmts {
		write_stmt(f, w, stmt)?;
		writeln!(f)?;
	}

	if let Some(tail) = &block.tail_expr {
		w.write_indent(f)?;
		write_expr(f, w, tail)?;
		writeln!(f)?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_expr(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, expr: &Expr) -> fmt::Result
{
	match expr {
		Expr::Switch {
			expr: switch_expr,
			arms,
			..
		} => return write_switch(f, w, switch_expr, arms),
		Expr::Block(block) => return write_block(f, w, block),
		Expr::UnsafeBlock(block) => {
			write!(f, "unsafe ")?;
			return write_block(f, w, block);
		}
		Expr::If {
			cond,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if ")?;
			write_expr(f, w, cond)?;
			write!(f, " ")?;
			write_block(f, w, then_block)?;
			if let Some(else_stmt) = else_branch {
				write!(f, " else ")?;
				write_expr(f, w, else_stmt)?;
			}
			return Ok(());
		}
		Expr::IfVar {
			pattern,
			expr,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if {} = ", pattern)?;

			write_expr(f, w, expr)?;
			write!(f, " ")?;
			write_block(f, w, then_block)?;
			if let Some(else_stmt) = else_branch {
				write!(f, " else ")?;
				write_expr(f, w, else_stmt)?;
			}
			return Ok(());
		}
		Expr::Loop { label, body, .. } => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "loop ")?;
			return write_block(f, w, body);
		}
		_ => return write!(f, "{}", expr),
	}
}

fn write_stmt(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, stmt: &Stmt) -> fmt::Result
{
	w.write_indent(f)?;
	return write_stmt_no_indent(f, w, stmt);
}

fn write_stmt_no_indent(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, stmt: &Stmt) -> fmt::Result
{
	match stmt {
		Stmt::VariableDecl(var) => {
			write_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		Stmt::Assignment { target, op, value, .. } => {
			write_expr(f, w, target)?;
			write!(f, " {} ", op)?;
			write_expr(f, w, value)?;
			return write!(f, ";");
		}
		Stmt::Return { value, .. } => {
			write!(f, "return")?;
			if let Some(e) = value {
				write!(f, " ")?;
				write_expr(f, w, e)?;
			}
			return write!(f, ";");
		}
		Stmt::Expr(expr) => match expr {
			Expr::Switch { expr, arms, .. } => {
				write_switch(f, w, expr, arms)?;
				return write!(f, ";");
			}
			Expr::Block(block) => {
				write_block(f, w, block)?;
				return write!(f, ";");
			}
			_ => {
				write_expr(f, w, expr)?;
				return write!(f, ";",);
			}
		},
		Stmt::Break { label, value, .. } => {
			write!(f, "break")?;
			if let Some(lbl) = label {
				write!(f, " '{}", lbl)?;
			}
			if let Some(val) = value {
				write!(f, " ")?;
				write_expr(f, w, val)?;
			}
			return write!(f, ";");
		}
		Stmt::Continue { label, .. } => {
			write!(f, "continue")?;
			if let Some(lbl) = label {
				write!(f, " '{}", lbl)?;
			}
			return write!(f, ";");
		}
		Stmt::If {
			cond,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if ")?;
			write_expr(f, w, cond)?;
			write!(f, " ")?;
			write_block(f, w, then_block)?;
			if let Some(else_stmt) = else_branch {
				write!(f, " else ")?;
				write_stmt_no_indent(f, w, else_stmt)?;
			}
			return Ok(());
		}
		Stmt::IfVar {
			pattern,
			expr,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if var {} = ", pattern)?;
			write_expr(f, w, expr)?;
			write!(f, " ")?;
			write_block(f, w, then_block)?;
			if let Some(else_stmt) = else_branch {
				write!(f, " else ")?;
				write_stmt_no_indent(f, w, else_stmt)?;
			}
			return Ok(());
		}
		Stmt::While { label, cond, body, .. } => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "while ")?;
			write_expr(f, w, cond)?;
			write!(f, " ")?;
			return write_block(f, w, body);
		}
		Stmt::Loop { label, body, .. } => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "loop ")?;
			return write_block(f, w, body);
		}
		Stmt::WhileVarLoop {
			label,
			pattern,
			expr,
			body,
			..
		} => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "while var {} = ", pattern)?;
			write_expr(f, w, expr)?;
			write!(f, " ")?;
			return write_block(f, w, body);
		}
		Stmt::For {
			label,
			name,
			iter,
			body,
			..
		} => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "for {} in ", name.join("::"))?;
			write_expr(f, w, iter)?;
			write!(f, " ")?;
			return write_block(f, w, body);
		}
		Stmt::Delete { path, .. } => return write!(f, "delete {};", path.join("::")),
		Stmt::Unsafe(block) => {
			write!(f, "unsafe ")?;
			return write_block(f, w, block);
		}
		Stmt::Block(block) => return write_block(f, w, block),
		Stmt::Directive(directive_node) => {
			w.indent();
			write!(f, "{}", directive_node)?;
			if directive_node.body.is_none() {
				write!(f, ";")?;
			}
			writeln!(f)?;
			return Ok(());
		}
	}
}

impl fmt::Display for AssignOp
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			AssignOp::Assign => return write!(f, "="),
			AssignOp::AddAssign => return write!(f, "+="),
			AssignOp::SubAssign => return write!(f, "-="),
			AssignOp::MulAssign => return write!(f, "*="),
			AssignOp::DivAssign => return write!(f, "/="),
			AssignOp::ModAssign => return write!(f, "%="),
			AssignOp::AndAssign => return write!(f, "&="),
			AssignOp::OrAssign => return write!(f, "|="),
			AssignOp::XorAssign => return write!(f, "^="),
			AssignOp::ShlAssign => return write!(f, "<<="),
			AssignOp::ShrAssign => return write!(f, ">>="),
		}
	}
}

fn write_struct_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, s: &StructDecl) -> fmt::Result
{
	for modifier in &s.modifiers {
		write!(f, "{} ", modifier)?;
	}

	writeln!(f, "struct {} {{", s.name.join("::"))?;
	w.indent();

	for (ty, name, default_value) in &s.fields {
		w.write_indent(f)?;
		write!(f, "{}: {}", name, ty)?;
		if let Some(default) = default_value {
			write!(f, " = {}", default)?;
		}
		writeln!(f, ",")?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_union_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, u: &UnionDecl) -> fmt::Result
{
	for modifier in &u.modifiers {
		write!(f, "{} ", modifier)?;
	}

	writeln!(f, "union {} {{", u.name.join("::"))?;
	w.indent();

	for (ty, name) in &u.fields {
		w.write_indent(f)?;
		writeln!(f, "{}: {},", name, ty)?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_enum_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, e: &EnumDecl) -> fmt::Result
{
	for modifier in &e.modifiers {
		write!(f, "{} ", modifier)?;
	}

	writeln!(f, "enum {} {{", e.name.join("::"))?;
	w.indent();

	for (name, value) in &e.variants {
		w.write_indent(f)?;
		if let Some(val) = value {
			writeln!(f, "{} = ,", name)?;
			write_expr(f, w, val)?;
		} else {
			writeln!(f, "{},", name)?;
		}
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_variant_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, v: &VariantDecl) -> fmt::Result
{
	for modifier in &v.modifiers {
		write!(f, "{} ", modifier)?;
	}

	writeln!(f, "variant {} {{", v.name.join("::"))?;
	w.indent();

	for (ty, name, value) in &v.variants {
		w.write_indent(f)?;
		write!(f, "{}", name)?;
		if let Some(t) = ty {
			write!(f, "({})", t)?;
		}
		if let Some(val) = value {
			write!(f, " = ")?;
			write_expr(f, w, val)?;
		}
		writeln!(f, ",")?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_type_alias_decl(f: &mut fmt::Formatter<'_>, _w: &mut IndentWriter, t: &TypeAliasDecl) -> fmt::Result
{
	for modifier in &t.modifiers {
		write!(f, "{} ", modifier)?;
	}

	return write!(f, "type {} = {}", t.name.join("::"), t.ty);
}

fn write_namespace_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, n: &NamespaceDecl) -> fmt::Result
{
	for modifier in &n.modifiers {
		write!(f, "{} ", modifier)?;
	}

	writeln!(f, "namespace {} {{", n.name.join("::"))?;
	w.indent();

	for item in &n.body.items {
		w.write_indent(f)?;
		write_top_level_decl(f, w, item)?;
		writeln!(f)?;
		writeln!(f)?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_trait_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, t: &TraitDecl) -> fmt::Result
{
	for modifier in &t.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "trait {}", t.name.join("::"))?;

	if !t.generics.is_empty() {
		write!(f, "<{}>", t.generics.join(", "))?;
	}

	if !t.super_traits.is_empty() {
		write!(f, ": ")?;
		for (i, st) in t.super_traits.iter().enumerate() {
			if i > 0 {
				write!(f, " + ")?;
			}
			write!(f, "{}", st.join("::"))?;
		}
	}

	writeln!(f, " {{")?;
	w.indent();

	for item in &t.items {
		w.write_indent(f)?;
		write_trait_item(f, w, item)?;
		writeln!(f)?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_trait_item(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, item: &TraitItem) -> fmt::Result
{
	match item {
		TraitItem::Function { signature, body, .. } => {
			write_function_signature(f, w, signature)?;
			if let Some(b) = body {
				write!(f, " ")?;
				return write_block(f, w, b);
			} else {
				return write!(f, ";");
			}
		}
		TraitItem::TypeAlias(ta) => {
			write_type_alias_decl(f, w, ta)?;
			return write!(f, ";");
		}
		TraitItem::Const(var) => {
			write_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
	}
}

fn write_impl_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, i: &ImplDecl) -> fmt::Result
{
	for modifier in &i.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "impl")?;

	if !i.generics.is_empty() {
		write!(f, "<{}>", i.generics.join(", "))?;
	}

	if let Some(trait_path) = &i.trait_path {
		write!(f, " {}", trait_path)?;
		write!(f, " for")?;
	}

	write!(f, " {}", i.target)?;

	if !i.where_clause.is_empty() {
		write!(f, " where ")?;
		for (i, constraint) in i.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}

	writeln!(f, " {{")?;
	w.indent();

	for item in &i.body {
		w.write_indent(f)?;
		write_impl_item(f, w, item)?;
		writeln!(f)?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_impl_item(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, item: &ImplItem) -> fmt::Result
{
	match item {
		ImplItem::Function(func) => return write_function_decl(f, w, func),
		ImplItem::TypeAlias(ta) => {
			write_type_alias_decl(f, w, ta)?;
			return write!(f, ";");
		}
		ImplItem::Const(var) => {
			write_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
	}
}

impl fmt::Display for ImplTarget
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.path.join("::"))?;

		if !self.generics.is_empty() {
			write!(f, "<")?;
			for (i, generic) in self.generics.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}", generic)?;
			}
			write!(f, ">")?;
		}

		return Ok(());
	}
}

impl fmt::Display for WhereConstraint
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}: ", self.ty.join("::"))?;
		for (i, bound) in self.bounds.iter().enumerate() {
			if i > 0 {
				write!(f, " + ")?;
			}
			write!(f, "{}", bound.join("::"))?;
		}
		return Ok(());
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
		return parser.parse_expr();
	}

	fn parse_program_from_str(input: &str) -> Result<Program, ParseError>
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, input);
		let mut parser = Parser::from(lexer);
		return parser.parse_program();
	}

	fn parse_block_from_str(input: &str) -> Result<Block, ParseError>
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, input);
		let mut parser = Parser::from(lexer);
		return parser.parse_block();
	}

	// ========== Literal Tests ==========

	#[test]
	fn test_parse_int_literal()
	{
		let result = parse_expr_from_str("42");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Int(42),
				..
			} => (),
			_ => panic!("Expected Int literal"),
		}
	}

	#[test]
	fn test_parse_float_literal()
	{
		let result = parse_expr_from_str("3.16");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Float(f),
				..
			} if (f - 3.16).abs() < 0.001 => (),
			_ => panic!("Expected Float literal"),
		}
	}

	#[test]
	fn test_parse_bool_literal_true()
	{
		let result = parse_expr_from_str("true");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Bool(true),
				..
			} => (),
			_ => panic!("Expected Bool(true) literal"),
		}
	}

	#[test]
	fn test_parse_bool_literal_false()
	{
		let result = parse_expr_from_str("false");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Bool(false),
				..
			} => (),
			_ => panic!("Expected Bool(false) literal"),
		}
	}

	#[test]
	fn test_parse_string_literal()
	{
		let result = parse_expr_from_str(r#""hello world""#);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::String(s),
				..
			} if s == "hello world" => (),
			_ => panic!("Expected String literal"),
		}
	}

	#[test]
	fn test_parse_char_literal()
	{
		let result = parse_expr_from_str("'a'");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Char('a'),
				..
			} => (),
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
			Expr::Identifier { path, .. } if path == vec!["variable"] => (),
			_ => panic!("Expected simple identifier"),
		}
	}

	#[test]
	fn test_parse_path_identifier()
	{
		let result = parse_expr_from_str("std::vec::Vec");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Identifier { path, .. } if path == vec!["std", "vec", "Vec"] => (),
			_ => panic!("Expected path identifier"),
		}
	}

	#[test]
	fn test_parse_self_keyword()
	{
		let result = parse_expr_from_str("self");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Identifier { path, .. } if path == vec!["self"] => (),
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
				..
			} => {
				match *lhs {
					Expr::Literal {
						value: Literal::Int(1), ..
					} => (),
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
			Expr::Cast { ty, expr, .. } => {
				match *ty.core {
					TypeCore::Base { ref path, .. } if path == &vec!["i32"] => (),
					_ => panic!("Expected i32 type"),
				}
				match *expr {
					Expr::Literal {
						value: Literal::Int(42),
						..
					} => (),
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
				..
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
				..
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
				..
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
			Expr::Call { callee, args, .. } => {
				match *callee {
					Expr::Identifier { ref path, .. } if path == &vec!["foo"] => (),
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
			Expr::Call { callee, args, .. } => {
				match *callee {
					Expr::Identifier { ref path, .. } if path == &vec!["foo"] => (),
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
			Expr::Field { base, name, .. } => {
				match *base {
					Expr::Identifier { ref path, .. } if path == &vec!["obj"] => (),
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
			Expr::Field { base, name, .. } => {
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
			Expr::Index { base, index, .. } => {
				match *base {
					Expr::Identifier { ref path, .. } if path == &vec!["arr"] => (),
					_ => panic!("Expected arr identifier"),
				}
				match *index {
					Expr::Literal {
						value: Literal::Int(0), ..
					} => (),
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
			Expr::Tuple { elements, .. } => assert_eq!(elements.len(), 0),
			_ => panic!("Expected empty tuple"),
		}
	}

	#[test]
	fn test_parse_single_element_tuple()
	{
		let result = parse_expr_from_str("(1,)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Tuple { elements, .. } => assert_eq!(elements.len(), 1),
			_ => panic!("Expected single-element tuple"),
		}
	}

	#[test]
	fn test_parse_multi_element_tuple()
	{
		let result = parse_expr_from_str("(1, 2, 3)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Tuple { elements, .. } => assert_eq!(elements.len(), 3),
			_ => panic!("Expected multi-element tuple"),
		}
	}

	#[test]
	fn test_parse_parenthesized_expr()
	{
		let result = parse_expr_from_str("(42)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Int(42),
				..
			} => (),
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
			Expr::Array(ArrayLiteral::List { elements, .. }) => assert_eq!(elements.len(), 0),
			_ => panic!("Expected empty array"),
		}
	}

	#[test]
	fn test_parse_array_list()
	{
		let result = parse_expr_from_str("[1, 2, 3]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::List { elements, .. }) => assert_eq!(elements.len(), 3),
			_ => panic!("Expected array list"),
		}
	}

	#[test]
	fn test_parse_array_repeat()
	{
		let result = parse_expr_from_str("[0; 10]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::Repeat { value, count, .. }) => {
				assert_eq!(value.len(), 1);
				match *count {
					Expr::Literal {
						value: Literal::Int(10),
						..
					} => (),
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
			Expr::StructInit { path, fields, .. } => {
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
			Expr::StructInit { path, fields, .. } => {
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
		let input = "{ var x: i32 = 5; x + 1 }";
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

	// ========== Switch Tests ==========

	#[test]
	fn test_parse_switch_expression()
	{
		let input = r#"switch x {
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
			Expr::Switch { expr: _, arms, .. } => {
				assert_eq!(arms.len(), 3);
			}
			_ => panic!("Expected switch expression"),
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
	fn test_parse_var_declaration()
	{
		let input = "var x: i32 = 5;";
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		match &program.items[0] {
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
		let result = parse_expr_from_str("var");
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
	fn test_parse_if_var_basic()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var Some(x: i64) = opt { x } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		assert_eq!(block.stmts.len(), 0);
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_if_var_with_else()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var Some(x: i64) = opt { x } else { 0 }; }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_var_else_if_var()
	{
		let config = Config::default();
		let lexer = Lexer::new(
			&config,
			"{ if var Some(x: i32) = opt1 { x } else if var Some(y: i32) = opt2 { y } else { 0 }; }",
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_var_tuple_pattern()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var (x: i32, y: i32) = pair { x + y } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_var_wildcard()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var Some(_) = opt { true } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_var_wildcard_with_type()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var Some(_: i64) = opt { true } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	// ========== While Let Tests ==========

	#[test]
	fn test_parse_while_let()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ while var Some(x: i32) = iter.next() { process(x); } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::WhileVarLoop { .. } => (),
			_ => panic!("Expected while var loop"),
		}
	}

	#[test]
	fn test_parse_while_let_tuple()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ while var (a: i32, b: i32) = get_pair() { } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	// ========== Enhanced Pattern Tests ==========

	#[test]
	fn test_parse_struct_pattern()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var Point { x: i32, y: i32 } = pt { x } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_struct_pattern_nested()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var Point { x: a: i32, y: b: i32 } = pt { a } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_or_pattern()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var Some(1) | Some(2) | Some(3) = x { true } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_nested_variant_pattern()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var Some(Point { x: a: i32, y: b: i32 }) = opt { a } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_variant_with_tuple()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var Some((x: i32, y: i32)) = opt { x } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_unit_variant()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ if var None = opt { true } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_switch_with_typed_patterns()
	{
		let input = r#"
			switch x {
				Some(val: i32) => val,
				None => 0,
			}
		"#;
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_loop_basic()
	{
		let result = parse_block_from_str("{ loop { break; }; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		assert_eq!(block.stmts.len(), 1);
		match &block.stmts[0] {
			Stmt::Loop { .. } => (),
			_ => panic!("Expected loop statement"),
		}
	}

	#[test]
	fn test_parse_loop_with_continue()
	{
		let result = parse_block_from_str("{ loop { if x { continue; } break; } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_nested_loops()
	{
		let result = parse_block_from_str("{ loop { loop { break; } break; } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	// ========== Delete Statement Tests ==========

	#[test]
	fn test_parse_delete_simple()
	{
		let result = parse_block_from_str("{ delete ptr; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Delete { path, .. } => assert_eq!(path, &vec!["ptr"]),
			_ => panic!("Expected delete statement"),
		}
	}

	#[test]
	fn test_parse_delete_qualified_path()
	{
		let result = parse_block_from_str("{ delete std::ptr; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Delete { path, .. } => assert_eq!(path, &vec!["std", "ptr"]),
			_ => panic!("Expected delete statement"),
		}
	}

	// ========== Directive Tests ==========

	#[test]
	fn test_parse_import_directive()
	{
		let input = r#"@import "file.rs";"#;
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Directive(DirectiveNode {
				directive: Directive::Import(path),
				..
			}) => {
				assert_eq!(path, "file.rs");
			}
			_ => panic!("Expected import directive"),
		}
	}

	#[test]
	fn test_parse_use_directive()
	{
		let input = "@use std::vec::Vec;";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Directive(DirectiveNode {
				directive: Directive::Use(path),
				..
			}) => {
				assert_eq!(path, &vec!["std", "vec", "Vec"]);
			}
			_ => panic!("Expected use directive"),
		}
	}

	#[test]
	fn test_parse_custom_directive_no_args()
	{
		let input = "@custom;";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_custom_directive_with_args()
	{
		let input = "@custom(1, 2, 3);";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Directive(DirectiveNode {
				directive: Directive::Custom { name, args },
				..
			}) => {
				assert_eq!(name, "custom");
				assert_eq!(args.len(), 3);
			}
			_ => panic!("Expected custom directive"),
		}
	}

	// ========== Modifier Tests ==========

	#[test]
	fn test_parse_function_with_inline_modifier()
	{
		let input = "inline fn fast() {}";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert!(func.signature.modifiers.iter().any(|m| matches!(m, Modifier::Inline)));
			}
			_ => panic!("Expected function"),
		}
	}

	#[test]
	fn test_parse_function_with_const_modifier()
	{
		let input = "const fn compile_time() {}";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert!(func.signature.modifiers.iter().any(|m| matches!(m, Modifier::Const)));
			}
			_ => panic!("Expected function"),
		}
	}

	#[test]
	fn test_parse_function_with_multiple_modifiers()
	{
		let input = "pub unsafe inline fn complex() {}";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert!(func.signature.modifiers.len() >= 3);
			}
			_ => panic!("Expected function"),
		}
	}

	// ========== Type System Tests ==========

	#[test]
	fn test_parse_nested_pointer_type()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "i32**");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Pointer { inner } => match inner.as_ref() {
				TypeCore::Pointer { .. } => (),
				_ => panic!("Expected nested pointer"),
			},
			_ => panic!("Expected pointer type"),
		}
	}

	#[test]
	fn test_parse_array_of_pointers()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "i32*[10]");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Array { inner, .. } => match inner.as_ref() {
				TypeCore::Pointer { .. } => (),
				_ => panic!("Expected pointer element type"),
			},
			_ => panic!("Expected array type"),
		}
	}

	#[test]
	fn test_parse_pointer_to_array()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "i32[10]*");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_tuple_type_single_element()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "(i32,)");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Tuple(types) => assert_eq!(types.len(), 1),
			_ => panic!("Expected tuple type"),
		}
	}

	#[test]
	fn test_parse_tuple_type_empty()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "()");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Tuple(types) => assert_eq!(types.len(), 0),
			_ => panic!("Expected empty tuple type"),
		}
	}

	#[test]
	fn test_parse_nested_generic_types()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "Vec<Vec<i32>>");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
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
	fn test_parse_multiple_generic_arguments()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "Map<String, i32>");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Base { generics, .. } => {
				assert_eq!(generics.len(), 2);
			}
			_ => panic!("Expected generic type"),
		}
	}

	#[test]
	fn test_parse_qualified_type_path()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "std::vec::Vec<i32>");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Base { path, .. } => {
				assert_eq!(path, &vec!["std", "vec", "Vec"]);
			}
			_ => panic!("Expected qualified type"),
		}
	}

	#[test]
	fn test_parse_reference_to_tuple()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "&(i32, i32)");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	// ========== Pattern Tests ==========

	#[test]
	fn test_parse_pattern_range_inclusive()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ switch x { 1..=10 => true, } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_range_exclusive()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ switch x { 1..10 => true, } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_range_open_ended()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ switch x { 1.. => true, } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_char_literal()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ switch x { 'a' => true, } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_string_literal()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, r#"{ switch x { "hello" => true, } }"#);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_bool_literals()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ switch x { true => 1, false => 0, } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_nested_tuple()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ switch x { ((a: i32, b: i32), c: i32) => a, } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_variant_multiple_args()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ switch x { Some(a: i32, b: i32, c: i32) => a, } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_or_with_wildcards()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "{ switch x { None | Some(_) => true, } }");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	// ========== Expression Edge Switchs ==========

	#[test]
	fn test_parse_nested_struct_init()
	{
		let result = parse_expr_from_str("Outer { inner = Inner { x = 1 } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_array_of_tuples()
	{
		let result = parse_expr_from_str("[(1, 2), (3, 4), (5, 6)]").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_tuple_of_arrays()
	{
		let result = parse_expr_from_str("([1, 2], [3, 4])").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_array_repeat_with_expression()
	{
		let result = parse_expr_from_str("[x + 1; 10]").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_struct_init_with_computed_fields()
	{
		let result = parse_expr_from_str("Point { x = a + b, y = c * d }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_call_with_struct_init()
	{
		let result = parse_expr_from_str("foo(Point { x = 1, y = 2 })").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_index_chain()
	{
		let result = parse_expr_from_str("arr[0][1][2]").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_mixed_postfix_operations()
	{
		let result = parse_expr_from_str("obj.method()[0].field").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_cast_in_expression()
	{
		let result = parse_expr_from_str("(i64)x + (i64)y").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_address_of_field()
	{
		let result = parse_expr_from_str("&obj.field").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_dereference_chain()
	{
		let result = parse_expr_from_str("***ptr").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	// ========== Block and Tail Expression Tests ==========

	#[test]
	fn test_parse_block_with_only_tail_expr()
	{
		let result = parse_block_from_str("{ 42 }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		assert_eq!(block.stmts.len(), 0);
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_block_tail_expr_after_semicolons()
	{
		let result = parse_block_from_str("{ var x: i32 = 1; var y: i32 = 2; x + y }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		assert_eq!(block.stmts.len(), 2);
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_nested_blocks_with_tail()
	{
		let result = parse_block_from_str("{ { { 42 } } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_as_tail_expr()
	{
		let result = parse_block_from_str("{ if true { 1 } else { 2 } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_switch_as_tail_expr()
	{
		let result = parse_block_from_str("{ switch x { 1 => 10, _ => 0, } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_function_default()
	{
		let input = "fn default() -> Self { Self::new() }";
		let result = parse_program_from_str(input);
		assert!(result.is_err()); // should fail, no function default is allowed, because of the default keyword
	}

	#[test]
	fn test_parse_function_new()
	{
		let input = "fn new() -> Self { Self::new() }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_trait_with_associated_type()
	{
		let input = "trait Iterator { type Item; }";
		let result = parse_program_from_str(input);
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_trait_with_associated_const()
	{
		let input = "trait HasMax { const MAX: i32; }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_trait_with_multiple_supertraits()
	{
		let input = "trait Sub: Super1 + Super2 + Super3 { }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Trait(t) => {
				assert_eq!(t.super_traits.len(), 3);
			}
			_ => panic!("Expected trait"),
		}
	}

	// ========== Impl Block Tests ==========

	#[test]
	fn test_parse_impl_with_associated_type()
	{
		let input = "impl Iterator for Counter { type Item = i32; }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_impl_with_associated_const()
	{
		let input = "impl HasMax for MyType { const MAX: i32 = 100; }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_impl_with_where_clause()
	{
		let input = "impl<T> MyTrait for MyType<T> where T: Clone { }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				assert_eq!(i.where_clause.len(), 1);
			}
			_ => panic!("Expected impl"),
		}
	}

	#[test]
	fn test_parse_impl_with_multiple_where_constraints()
	{
		let input = "impl<T, U> Trait for Type<T, U> where T: Clone, U: Debug { }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				assert_eq!(i.where_clause.len(), 2);
			}
			_ => panic!("Expected impl"),
		}
	}

	#[test]
	fn test_parse_impl_with_multiple_bounds()
	{
		let input = "impl<T> Trait for Type<T> where T: Clone + Debug + Display { }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				assert_eq!(i.where_clause[0].bounds.len(), 3);
			}
			_ => panic!("Expected impl"),
		}
	}

	// ========== Namespace Tests ==========

	#[test]
	fn test_parse_nested_namespace()
	{
		let input = "namespace outer { namespace inner { fn foo() {} } }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Namespace(n) => {
				assert_eq!(n.body.items.len(), 1);
				match &n.body.items[0] {
					TopLevelDecl::Namespace(_) => (),
					_ => panic!("Expected nested namespace"),
				}
			}
			_ => panic!("Expected namespace"),
		}
	}

	#[test]
	fn test_parse_qualified_namespace_name()
	{
		let input = "namespace std::vec { }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Namespace(n) => {
				assert_eq!(n.name, vec!["std", "vec"]);
			}
			_ => panic!("Expected namespace"),
		}
	}

	// ========== Statement Combination Tests ==========

	#[test]
	fn test_parse_if_else_if_chain()
	{
		let result = parse_block_from_str("{ if a { 1 } else if b { 2 } else if c { 3 } else { 4 }; }")
			.inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_var_else_if_chain()
	{
		let result =
			parse_block_from_str("{ if var Some(x: i32) = a { x } else if var Some(y: i32) = b { y } else { 0 }; }")
				.inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_mixed_if_if_var_chain()
	{
		let result =
			parse_block_from_str("{ if a { 1 } else if var Some(x: i32) = b { x } else if c { 3 } else { 4 }; }")
				.inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_nested_if_statements()
	{
		let result =
			parse_block_from_str("{ if a { if b { 1 } else { 2 } } else { 3 }; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	// ========== Variable Declaration Tests ==========

	#[test]
	fn test_parse_var_without_init()
	{
		let result = parse_block_from_str("{ var x: i32; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_const_with_complex_expr()
	{
		let result = parse_block_from_str("{ const X: i32 = 2 + 3 * 4; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	// ========== Function Parameter Tests ==========

	#[test]
	fn test_parse_function_with_qualified_param_name()
	{
		let input = "fn foo(ns::name: i32) {}";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.params[0].name, vec!["ns", "name"]);
			}
			_ => panic!("Expected function"),
		}
	}

	#[test]
	fn test_parse_function_with_trailing_comma()
	{
		let input = "fn foo(x: i32, y: i32,) {}";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	// ========== Switch Expression Tests ==========

	#[test]
	fn test_parse_switch_with_block_arms()
	{
		let input = "switch x { 1 => { println(1); }, 2 => { println(2); }, }";
		let result = parse_expr_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Switch { arms, .. } => {
				for arm in arms {
					match arm.body {
						SwitchBody::Block(_) => (),
						SwitchBody::Expr(_) => panic!("Expected block arm"),
					}
				}
			}
			_ => panic!("Expected switch expression"),
		}
	}

	#[test]
	fn test_parse_switch_mixed_arms()
	{
		let input = "switch x { 1 => 10, 2 => { println(2); 20 }, }";
		let result = parse_expr_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_nested_switch()
	{
		let input = "switch x { 1 => switch y { 2 => 3, _ => 4, }, _ => 0, }";
		let result = parse_expr_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	// ========== Unsafe Block Tests ==========

	#[test]
	fn test_parse_unsafe_block_as_statement()
	{
		let result = parse_block_from_str("{ unsafe { ptr.write(42); } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_unsafe_block_as_expr()
	{
		let result = parse_block_from_str("{ var x: i32 = unsafe { *ptr }; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_unsafe_block_as_tail()
	{
		let result = parse_block_from_str("{ unsafe { *ptr } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		assert!(block.tail_expr.is_some());
	}

	// ========== Complex Struct/Variant Tests ==========

	#[test]
	fn test_parse_struct_with_qualified_name()
	{
		let input = "struct ns::Name { }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Struct(s) => {
				assert_eq!(s.name, vec!["ns", "Name"]);
			}
			_ => panic!("Expected struct"),
		}
	}

	#[test]
	fn test_parse_struct_with_trailing_comma()
	{
		let input = "struct Point { x: i32, y: i32, }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_variant_with_qualified_types()
	{
		let input = "variant Result { Ok(std::string::String), Err(std::error::Error) }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	// ========== Expression Precedence Tests ==========

	#[test]
	fn test_parse_precedence_bitwise_vs_comparison()
	{
		let result = parse_expr_from_str("a & b == c").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		// & has lower precedence than ==, so should parse as (a & (b == c))
		match result.unwrap() {
			Expr::Binary {
				op: BinaryOp::BitAnd, ..
			} => (),
			_ => panic!("Expected bitwise AND at top level"),
		}
	}

	#[test]
	fn test_parse_precedence_shift_vs_addition()
	{
		let result = parse_expr_from_str("a + b << c").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		// << has lower precedence than +, so should parse as ((a + b) << c)
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Shl, .. } => (),
			_ => panic!("Expected shift at top level"),
		}
	}

	#[test]
	fn test_parse_precedence_logical_and_or()
	{
		let result = parse_expr_from_str("a || b && c").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		// && has higher precedence than ||, so should parse as (a || (b && c))
		match result.unwrap() {
			Expr::Binary {
				op: BinaryOp::LogicalOr,
				rhs,
				..
			} => match *rhs {
				Expr::Binary {
					op: BinaryOp::LogicalAnd,
					..
				} => (),
				_ => panic!("Expected logical AND on right"),
			},
			_ => panic!("Expected logical OR at top level"),
		}
	}

	// ========== Display/Formatting Tests ==========

	#[test]
	fn test_display_program()
	{
		let input = "fn foo() { return 42; }";
		let program = parse_program_from_str(input).inspect_err(|e| println!("{e}")).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("fn"));
		assert!(output.contains("foo"));
	}

	#[test]
	fn test_display_preserves_structure()
	{
		let input = "struct Point { x: i32, y: i32 }";
		let program = parse_program_from_str(input).inspect_err(|e| println!("{e}")).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("Point"));
		assert!(output.contains("x"));
		assert!(output.contains("y"));
	}

	// ========== Error Recovery Tests ==========

	#[test]
	fn test_error_missing_semicolon()
	{
		let result = parse_block_from_str("{ var x: i32 = 5 var y: i32 = 10; }");
		assert!(result.is_err());
	}

	#[test]
	fn test_error_unmatched_brace()
	{
		let result = parse_block_from_str("{ var x: i32 = 5;");
		assert!(result.is_err());
	}

	#[test]
	fn test_error_invalid_pattern()
	{
		let result = parse_block_from_str("{ switch x { 1 + 2 => 3, } }");
		assert!(result.is_err());
	}

	#[test]
	fn test_error_missing_arrow_in_switch()
	{
		let result = parse_block_from_str("{ switch x { 1 3, } }");
		assert!(result.is_err());
	}

	// ========== Edge Switch Tests ==========

	#[test]
	fn test_parse_empty_program()
	{
		let result = parse_program_from_str("").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		assert_eq!(program.items.len(), 0);
	}

	#[test]
	fn test_parse_deeply_nested_expressions()
	{
		let result = parse_expr_from_str("((((((((42))))))))").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_long_operator_chain()
	{
		let result =
			parse_expr_from_str("a + b - c * d / e % f & g | h ^ i << j >> k").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_struct_init_ambiguity()
	{
		// Should parse as struct init, not block
		let result = parse_expr_from_str("Foo {}").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::StructInit { .. } => (),
			_ => panic!("Expected struct init"),
		}
	}

	#[test]
	fn test_parse_identifier_vs_struct_init()
	{
		// Should parse as identifier when no brace follows
		let result = parse_expr_from_str("Foo").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Identifier { .. } => (),
			_ => panic!("Expected identifier"),
		}
	}

	// ========== Right Shift Generic Handling ==========

	#[test]
	fn test_parse_nested_generics_with_rshift()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "Vec<Vec<i32>>");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_triple_nested_generics()
	{
		let config = Config::default();
		let lexer = Lexer::new(&config, "Box<Vec<Option<i32>>>");
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type().inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	// ========== Loop Label and Break Value Tests ==========

	#[test]
	fn test_parse_break_with_value()
	{
		let result = parse_block_from_str("{ loop { break 42; }; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { body, .. } => match &body.stmts[0] {
				Stmt::Break {
					label: None,
					value: Some(_),
					..
				} => (),
				_ => panic!("Expected break with value"),
			},
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_parse_break_without_value()
	{
		let result = parse_block_from_str("{ loop { break; }; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { body, .. } => match &body.stmts[0] {
				Stmt::Break {
					label: None,
					value: None,
					..
				} => (),
				_ => panic!("Expected break without value"),
			},
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_parse_labeled_loop()
	{
		let result = parse_block_from_str("{ 'outer: loop { break; }; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { label: Some(lbl), .. } => {
				assert_eq!(lbl, "outer");
			}
			_ => panic!("Expected labeled loop"),
		}
	}

	#[test]
	fn test_parse_break_with_label()
	{
		let result = parse_block_from_str("{ 'outer: loop { break 'outer; }; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop {
				label: Some(_), body, ..
			} => match &body.stmts[0] {
				Stmt::Break {
					label: Some(lbl),
					value: None,
					..
				} => {
					assert_eq!(lbl, "outer");
				}
				_ => panic!("Expected break with label"),
			},
			_ => panic!("Expected labeled loop"),
		}
	}

	#[test]
	fn test_parse_break_with_label_and_value()
	{
		let result = parse_block_from_str("{ 'outer: loop { break 'outer 42; }; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { body, .. } => match &body.stmts[0] {
				Stmt::Break {
					label: Some(lbl),
					value: Some(_),
					..
				} => {
					assert_eq!(lbl, "outer");
				}
				_ => panic!("Expected break with label and value"),
			},
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_parse_continue_with_label()
	{
		let result = parse_block_from_str("{ 'outer: loop { continue 'outer; }; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { body, .. } => match &body.stmts[0] {
				Stmt::Continue { label: Some(lbl), .. } => {
					assert_eq!(lbl, "outer");
				}
				_ => panic!("Expected continue with label"),
			},
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_parse_continue_without_label()
	{
		let result = parse_block_from_str("{ loop { continue; }; }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { body, .. } => match &body.stmts[0] {
				Stmt::Continue { label: None, .. } => (),
				_ => panic!("Expected continue without label"),
			},
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_parse_labeled_while_loop()
	{
		let result = parse_block_from_str("{ 'outer: while true { break 'outer; } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::While { label: Some(lbl), .. } => {
				assert_eq!(lbl, "outer");
			}
			_ => panic!("Expected labeled while loop"),
		}
	}

	#[test]
	fn test_parse_labeled_for_loop()
	{
		let result =
			parse_block_from_str("{ 'outer: for i in 0..10 { break 'outer; } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::For { label: Some(lbl), .. } => {
				assert_eq!(lbl, "outer");
			}
			_ => panic!("Expected labeled for loop"),
		}
	}

	#[test]
	fn test_parse_labeled_while_var_loop()
	{
		let result = parse_block_from_str("{ 'outer: while var Some(x: i32) = opt { break 'outer; } }")
			.inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::WhileVarLoop { label: Some(lbl), .. } => {
				assert_eq!(lbl, "outer");
			}
			_ => panic!("Expected labeled while var loop"),
		}
	}

	#[test]
	fn test_parse_nested_labeled_loops()
	{
		let input = r#"{
		'outer: loop {
			'inner: loop {
				break 'outer;
			};
		};
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop {
				label: Some(outer_lbl),
				body,
				..
			} => {
				assert_eq!(outer_lbl, "outer");
				match &body.stmts[0] {
					Stmt::Loop {
						label: Some(inner_lbl), ..
					} => {
						assert_eq!(inner_lbl, "inner");
					}
					_ => panic!("Expected inner labeled loop"),
				}
			}
			_ => panic!("Expected outer labeled loop"),
		}
	}

	#[test]
	fn test_parse_break_to_outer_loop()
	{
		let input = r#"{
		'outer: loop {
			'inner: loop {
				if condition {
					break 'outer;
				}
				break 'inner;
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_continue_to_outer_loop()
	{
		let input = r#"{
		'outer: for i in 0..10 {
			'inner: for j in 0..10 {
				if j == 5 {
					continue 'outer;
				}
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_loop_with_break_value_as_expression()
	{
		let input = r#"{
		var result: i32 = loop {
			if condition {
				break 42;
			}
		};
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_labeled_loop_with_break_value()
	{
		let input = r#"{
		var result: i32 = 'search: loop {
			if found {
				break 'search 100;
			}
		};
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_with_complex_expression()
	{
		let input = r#"{
		loop {
			break x * 2 + y;
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_with_function_call()
	{
		let input = r#"{
		loop {
			break compute_result();
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_label_with_underscore()
	{
		let result =
			parse_block_from_str("{ 'outer_loop: loop { break 'outer_loop; } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_label_with_numbers()
	{
		let result = parse_block_from_str("{ 'loop1: loop { break 'loop1; } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_multiple_labeled_loops_same_level()
	{
		let input = r#"{
		'first: loop {
			break 'first;
		}
		'second: loop {
			break 'second;
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_deeply_nested_labeled_loops()
	{
		let input = r#"{
		'level1: loop {
			'level2: while true {
				'level3: for i in 0..10 {
					'level4: loop {
						break 'level1;
					}
				}
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_block_expression()
	{
		let input = r#"{
		loop {
			break {
				var x: i32 = 10;
				x + 5
			};
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_if_expression()
	{
		let input = r#"{
		loop {
			break if condition { 1 } else { 2 };
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_match()
	{
		let input = r#"{
		loop {
			break switch x {
				1 => 10,
				2 => 20,
				_ => 0,
			};
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_label_similar_to_lifetime()
	{
		let result = parse_block_from_str("{ 'a: loop { break 'a; } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_char_literal_still_works()
	{
		let result = parse_expr_from_str("'x'").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Char('x'),
				..
			} => (),
			_ => panic!("Expected char literal"),
		}
	}

	#[test]
	fn test_parse_char_literal_and_label_in_same_block()
	{
		let input = r#"{
		var ch: char = 'a';
		'outer: loop {
			if ch == 'b' {
				break 'outer;
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_single_char_label()
	{
		let result = parse_block_from_str("{ 'a: loop { break 'a; } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_while_with_break_value()
	{
		let input = r#"{
		while true {
			break 42;
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_for_with_break_value()
	{
		let input = r#"{
		for i in 0..10 {
			if i == 5 {
				break i;
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_labeled_for_with_continue_and_break()
	{
		let input = r#"{
		'outer: for i in 0..10 {
			if i == 3 {
				continue 'outer;
			}
			if i == 7 {
				break 'outer i;
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_multiple_breaks_different_labels()
	{
		let input = r#"{
		'outer: loop {
			'inner: loop {
				if a {
					break 'outer 1;
				}
				if b {
					break 'inner 2;
				}
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_display_labeled_loop()
	{
		let input = "'outer: loop { break 'outer; }";
		let program = parse_program_from_str(&format!("fn test() {{{}}}", input))
			.inspect_err(|e| println!("{e}"))
			.unwrap();
		let output = format!("{}", program);
		assert!(output.contains("'outer"));
	}

	#[test]
	fn test_display_break_with_value()
	{
		let input = "loop { break 42; }";
		let program = parse_program_from_str(&format!("fn test() {{{}}}", input))
			.inspect_err(|e| println!("{e}"))
			.unwrap();
		let output = format!("{}", program);
		assert!(output.contains("break"));
		assert!(output.contains("42"));
	}

	#[test]
	fn test_display_break_with_label_and_value()
	{
		let input = "'outer: loop { break 'outer 42; }";
		let program = parse_program_from_str(&format!("fn test() {{{}}}", input))
			.inspect_err(|e| println!("{e}"))
			.unwrap();
		let output = format!("{}", program);
		assert!(output.contains("'outer"));
		assert!(output.contains("break"));
	}

	#[test]
	fn test_parse_label_without_colon_error()
	{
		let result = parse_block_from_str("{ 'outer loop { break; } }");
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_colon_without_label()
	{
		let result = parse_block_from_str("{ : loop { break; } }");
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_real_world_example_search()
	{
		let input = r#"{
		'search: for item in collection {
			if item.matches(target) {
				break 'search item.value;
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_real_world_example_nested_iteration()
	{
		let input = r#"{
		'outer: for row in 0..height {
			'inner: for col in 0..width {
				if grid[row][col] == target {
					break 'outer (row, col);
				}
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_real_world_example_state_machine()
	{
		let input = r#"{
		'state_machine: loop {
			switch current_state {
				State::Init => {
					if init_success() {
						break 'state_machine Result::Ok;
					}
					current_state = State::Retry;
					continue 'state_machine;
				},
				State::Retry => {
					if retry_count > 3 {
						break 'state_machine Result::Error;
					}
					continue 'state_machine;
				},
				_ => break 'state_machine Result::Error,
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_in_nested_if_in_loop()
	{
		let input = r#"{
		'outer: loop {
			if condition1 {
				if condition2 {
					break 'outer 42;
				}
			}
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_label_name_like_keyword()
	{
		// Labels named after keywords should work
		let result = parse_block_from_str("{ 'while: loop { break 'while; } }").inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_very_long_label_name()
	{
		let result = parse_block_from_str(
			"{ 'this_is_a_very_long_label_name_for_testing: loop { break 'this_is_a_very_long_label_name_for_testing; } }",
		)
		.inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_tuple()
	{
		let input = r#"{
		loop {
			break (1, 2, 3);
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_array()
	{
		let input = r#"{
		loop {
			break [1, 2, 3];
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_struct_init()
	{
		let input = r#"{
		loop {
			break Point { x = 1, y = 2 };
		}
	}"#;
		let result = parse_block_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_struct_with_default_values()
	{
		let input = "struct Point { x: i32 = 0, y: i32 = 0 }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Struct(s) => {
				assert_eq!(s.fields.len(), 2);
				assert!(s.fields[0].2.is_some()); // x has default value
				assert!(s.fields[1].2.is_some()); // y has default value
			}
			_ => panic!("Expected struct declaration"),
		}
	}

	#[test]
	fn test_parse_struct_mixed_defaults()
	{
		let input = "struct Person { name: String, age: i32 = 0 }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Struct(s) => {
				assert_eq!(s.fields.len(), 2);
				assert!(s.fields[0].2.is_none()); // name has no default
				assert!(s.fields[1].2.is_some()); // age has default
			}
			_ => panic!("Expected struct declaration"),
		}
	}

	#[test]
	fn test_parse_struct_default_complex_expr()
	{
		let input = "struct Config { timeout: i32 = 30 * 1000 }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_variant_with_values()
	{
		let input = "variant Status { Success = 0, Error = 1, Pending = 2 }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Variant(v) => {
				assert_eq!(v.variants.len(), 3);
				assert!(v.variants[0].2.is_some()); // Success has value
				assert!(v.variants[1].2.is_some()); // Error has value
				assert!(v.variants[2].2.is_some()); // Pending has value
			}
			_ => panic!("Expected variant declaration"),
		}
	}

	#[test]
	fn test_parse_variant_mixed_types_and_values()
	{
		let input = "variant Mixed { Unit = 0, WithData(i32) = 1, Other }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Variant(v) => {
				assert_eq!(v.variants.len(), 3);
				// Unit: no type, has value
				assert!(v.variants[0].0.is_none());
				assert!(v.variants[0].2.is_some());
				// WithData: has type, has value
				assert!(v.variants[1].0.is_some());
				assert!(v.variants[1].2.is_some());
				// Other: no type, no value
				assert!(v.variants[2].0.is_none());
				assert!(v.variants[2].2.is_none());
			}
			_ => panic!("Expected variant declaration"),
		}
	}

	#[test]
	fn test_parse_variant_with_complex_values()
	{
		let input = "variant Flags { A = 1 << 0, B = 1 << 1, C = 1 << 2 }";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
	}
}
