mod tests;

use std::{cmp::Ordering, convert::TryFrom, iter::Peekable, marker::PhantomData};

use ignorable::PartialEq;

use crate::{
	Config, diagnostics::{CompileDiagnostic, CompileError, Diagnostic, Severity}, lexer::{self, IntBase, IntType, Lexer, LexerTrait, ReservedError, Span, Spanned, Token, TokenKind}, source_map::SourceIndex, symbol_collection::Visibility
};
use leaf_proc::Spanned;

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
/// ```ignore
/// # use crate::{Config, Parser, SourceIndex};
/// # use crate::lexer::Lexer;
/// let config = Config::default();
/// let source = "fn main() { var x = 42; }";
/// let source_index = SourceIndex(0);
/// let lexer = Lexer::new(&config, source, source_index);
/// let mut parser = Parser::from(lexer);
/// let program = parser.parse_program().unwrap();
/// ```
#[derive(Debug, Clone)]
pub struct Parser<'source, 'config, T>
where
	T: LexerTrait<'source, 'config>,
{
	_config: &'config Config,
	source_index: SourceIndex,
	lexer: Peekable<T>,
	last_span: Span,
	buffered_token: Option<Token>,

	_marker: PhantomData<&'source ()>,
}

impl<'s, 'c, T> From<T> for Parser<'s, 'c, T>
where
	T: LexerTrait<'s, 'c>,
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
	/// ```ignore
	/// # use crate::{Parser, Config, SourceIndex};
	/// # use crate::lexer::Lexer;
	/// # let config = Config::default();
	/// # let source = "fn main() {}";
	/// # let source_index = SourceIndex(0);
	/// let lexer = Lexer::new(&config, source, source_index);
	/// let mut parser = Parser::from(lexer);
	/// ```
	fn from(lexer: T) -> Self
	{
		let (config, source_index, lex) = lexer.into_parts();
		return Self {
			_config: config,
			source_index,
			lexer: lex.peekable(),
			last_span: Span::default(),
			buffered_token: None,
			_marker: PhantomData,
		};
	}
}

/// Identifier type alias for clearer code semantics.
///
/// Represents variable names, function names, type names, and other identifiers
/// throughout the AST.
pub type Ident = String;

/// Represents a complete program or compilation unit as a sequence of
/// top-level declarations.
///
/// # Fields
/// * `items` - List of top-level declarations (functions, structs, traits, etc.)
/// * `span` - Source location of the entire program
#[derive(Default, Debug, Clone, PartialEq, Spanned)]
pub struct TopLevelBlock
{
	pub items: Vec<TopLevelDecl>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// The root node of the Abstract Syntax Tree.
///
/// # Fields
/// * `top_level_block` - The real programm
/// * `source_index` - The source index of the file
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, PartialEq)]
pub struct AST
{
	pub top_level_block: TopLevelBlock,
	pub source_index: SourceIndex,
}

impl Spanned for AST
{
	fn span(&self) -> Span
	{
		return self.top_level_block.span();
	}
}

impl<'s, 'c, T> TryFrom<Parser<'s, 'c, T>> for AST
where
	T: LexerTrait<'s, 'c>,
{
	type Error = ParseError;

	/// Converts a parser into a parsed program result.
	///
	/// This provides a convenient way to parse a complete program by consuming
	/// the parser. It calls `parse_program()` internally.
	///
	/// # Arguments
	/// * `parser` - The parser to consume
	///
	/// # Returns
	/// * `Ok(AST)` - The successfully parsed program AST
	/// * `Err(ParseError)` - If a syntax error is encountered during parsing
	///
	/// # Example
	/// ```ignore
	/// # use crate::{Parser, AST, ParseError, Config, SourceIndex};
	/// # use crate::lexer::Lexer;
	/// # fn main() -> Result<(), ParseError> {
	/// let config = Config::default();
	/// let source = "fn main() { var x = 42; }";
	/// let source_index = SourceIndex(0);
	/// let lexer = Lexer::new(&config, source, source_index);
	/// let parser = Parser::from(lexer);
	///
	/// // Convert parser to AST using TryFrom
	/// let program = AST::try_from(parser)?;
	///
	/// // Or more idiomatically in a single chain:
	/// let program = AST::try_from(Parser::from(Lexer::new(&config, source, source_index)))?;
	/// # Ok(())
	/// # }
	/// ```
	fn try_from(mut parser: Parser<'s, 'c, T>) -> Result<Self, Self::Error>
	{
		return parser.parse_program();
	}
}

impl<'s, 'c> TryFrom<Lexer<'s, 'c>> for TopLevelBlock
{
	type Error = ParseError;

	/// Converts a lexer into a parsed program.
	///
	/// This provides a convenient way to parse a complete program directly from a lexer.
	/// Internally, it first converts the lexer into a parser, then parses the program.
	///
	/// # Arguments
	/// * `lexer` - The lexer to consume
	///
	/// # Returns
	/// * `Ok(AST)` - The successfully parsed program AST
	/// * `Err(ParseError)` - If a syntax error is encountered during parsing
	///
	/// # Example
	/// ```ignore
	/// # use crate::lexer::Lexer;
	/// # use crate::{Parser, AST, Config, ParseError, SourceIndex};
	/// # fn main() -> Result<(), ParseError> {
	/// let config = Config::default();
	/// let source = "fn main() { var x = 42; }";
	/// let source_index = SourceIndex(0);
	/// let lexer = Lexer::new(&config, source, source_index);
	///
	/// // Convert lexer directly to a AST
	/// let program = AST::try_from(lexer)?;
	///
	/// // Or in a single chain:
	/// let program = AST::try_from(Lexer::new(&config, source, source_index))?;
	/// # Ok(())
	/// # }
	/// ```
	fn try_from(lexer: Lexer<'s, 'c>) -> Result<Self, Self::Error>
	{
		let mut parser: Parser<'_, '_, _> = Parser::from(lexer);

		return parser.parse_top_level_block();
	}
}

/// Top-level declaration types.
///
/// Represents all possible declarations that can appear at the top level
/// of a source file or within a module.
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
/// * `Module` - Module declaration
/// * `Impl` - Implementation block
/// * `Directive` - Compiler directive
#[derive(Debug, Clone, PartialEq, Spanned)]
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
	Module(ModuleDecl),
	Impl(ImplDecl),
	Directive(DirectiveNode),
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
	AssocType,
	Module,
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
/// * `Mut` Mutable variable
/// * `Directive` - Custom compiler directive
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum Modifier
{
	Mut, // only used for the parse modifier function and should not be seed anywere else, if it's anywere else it should return an error
	Pub,
	Export,
	Unsafe,
	Inline,
	Const, // for variables this one is not used, for functions it is
	Volatile,
	Directive(Directive),
}

impl PartialOrd for Modifier
{
	fn partial_cmp(&self, other: &Self) -> Option<Ordering>
	{
		const fn rank(m: &Modifier) -> u8
		{
			// ordering:
			// @directive pub export const unsafe inline volatile mut
			return match m {
				Modifier::Directive(_) => 0,
				Modifier::Pub => 1,
				Modifier::Export => 2,
				Modifier::Const => 3,
				Modifier::Unsafe => 4,
				Modifier::Inline => 5,
				Modifier::Volatile => 6,
				Modifier::Mut => 7,
			};
		}

		return rank(self).partial_cmp(&rank(other));
	}
}

pub fn get_visibility(modifiers: &Vec<Modifier>) -> Visibility
{
	for m in modifiers {
		match m {
			Modifier::Directive(_) => {}
			Modifier::Pub => return Visibility::Public,
			Modifier::Export => return Visibility::Export,
			Modifier::Mut | Modifier::Unsafe | Modifier::Inline | Modifier::Const | Modifier::Volatile => {
				return Visibility::Private;
			}
		}
	}
	return Visibility::Private;
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
/// * `ValidateStructPattern` Internal for validating if a struct pattern is valid
/// * `ValidateType` Internal for validating if a variable is a certain type
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum Directive
{
	Import
	{
		modifers: Vec<Modifier>,
		import: String,
		visibility: Visibility,
	},
	Use
	{
		modifers: Vec<Modifier>,
		use_path: Path,
		visibility: Visibility,
	},
	Custom
	{
		name: Ident, params: Vec<DirectiveParam>
	},
	ValidateStructPattern
	{
		struct_path: Path,
		pattern_fields: Vec<String>,
		has_rest: bool,
	},
	ValidateType
	{
		ty: Type, expr: Expr
	},
}

/// Parameter types for custom compiler directives.
///
/// Directives can accept various types of parameters including named arguments,
/// identifiers, and literal values. This enum represents all possible parameter
/// types that can be passed to a directive.
///
/// # Variants
///
/// * `Named { name: String, arg: Literal }` - A named parameter with a literal value.
/// * `Identifier(String)` - A standalone identifier parameter.
/// * `Literal(Literal)` - A literal value parameter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectiveParam
{
	Named
	{
		name: String,
		arg: Literal,
	},
	Identifier(String),
	Literal(Literal),
}

/// Directive node with optional body.
///
/// Represents a directive that may have an associated block of code.
///
/// # Fields
/// * `directive` - The directive itself
/// * `body` - Optional block content associated with the directive
/// * `span` - Source location of the directive
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct DirectiveNode
{
	pub directive: Directive,
	pub body: Option<BlockContent>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Qualified path representing a sequence of identifiers separated by `::`.
///
/// A path is used to reference items across module boundaries and can include
/// generic type arguments at any segment. Paths are fundamental to the type system.
///
/// # Structure
///
/// A path consists of segments, where each segment can optionally have generic arguments:
/// - `Vec::<i32>::new` - generics on first segment
/// - `std::vec::Vec::<i32>` - generics on last segment
/// - `Foo::<T>::bar::<U>::baz` - generics on multiple segments
///
/// # Fields
///
/// * `segments` - The path segments, each potentially with generic arguments
/// * `glob` - If the last element is `*`, just used for the `@use` statement
/// * `global` - If the path starts with `::`, means no module, so ignore all the `@use` statements
/// * `span` - Source location information for error reporting and debugging
#[derive(Debug, Clone, PartialEq, Default, Spanned)]
pub struct Path
{
	pub segments: Vec<PathSegment>,
	pub glob: bool,
	pub global: bool,
	#[allow(dead_code)]
	#[ignored(PartialEq)]
	pub span: Span,
}

/// A single segment in a path, optionally with generic arguments
///
/// # Fields
///
/// * `name` - Name of the segment
/// * `generics` - The generics that are with the name -> `Vec::<i64>`
/// * `span` - Source location information for error reporting and debugging
#[derive(Debug, Clone, PartialEq)]
pub struct PathSegment
{
	pub name: Ident,
	pub generics: Vec<Type>,
	#[ignored(PartialEq)]
	#[allow(dead_code)]
	pub span: Span,
}

impl Path
{
	/// Creates a simple path without generic arguments.
	///
	/// # Arguments
	/// * `segments` - The identifier segments
	/// * `span` - Source location
	#[allow(dead_code)]
	pub fn simple(segments: Vec<Ident>, span: Span) -> Self
	{
		return Self {
			segments: segments
				.into_iter()
				.map(|name| {
					return PathSegment {
						span,
						name,
						generics: Vec::new(),
					};
				})
				.collect(),
			glob: false,
			global: false,
			span,
		};
	}

	/// Checks if this path has any generic type arguments.
	///
	/// # Returns
	/// `true` if any segment has generic arguments, `false` otherwise
	#[allow(dead_code)]
	pub fn has_generics(&self) -> bool
	{
		return self.segments.iter().any(|seg| return !seg.generics.is_empty());
	}

	/// Checks if this path is empty (no segments).
	///
	/// # Returns
	/// `true` if the path is empty, `false` otherwise
	#[allow(dead_code)]
	pub const fn is_empty(&self) -> bool
	{
		return self.segments.is_empty();
	}

	/// Returns the number of segments.
	///
	/// # Returns
	/// The number of path segments
	#[allow(dead_code)]
	pub const fn len(&self) -> usize
	{
		return self.segments.len();
	}
}

impl Path
{
	/// Returns an iterator over the path's components.
	///
	/// # Returns
	/// An iterator that yields `PathComponent` items representing
	/// segments and generic type arguments in order.
	#[allow(dead_code)]
	pub fn iter(&self) -> std::slice::Iter<'_, PathSegment>
	{
		return self.segments.iter();
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
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the function
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct FunctionDecl
{
	pub signature: FunctionSignature,
	pub body: Option<Block>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Function signature.
///
/// Contains all the metadata about a function except its body.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Qualified function name (can include module path)
/// * `generics` - Generic type parameters
/// * `params` - Function parameters
/// * `return_type` - Optional return type (None means void/unit)
/// * `where_clause` - Generic constraints
/// * `heap_func` - Whether this is a heap-allocated function (`fn!`)
/// * `span` - Source location of the signature
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct FunctionSignature
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub generics: Vec<GenericParam>,
	pub params: Vec<Param>,
	pub return_type: Option<Type>,
	pub where_clause: Vec<WhereConstraint>,
	pub call_type: CallType,
	pub heap_generics: Vec<GenericParam>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Generic parameter with optional trait bounds.
///
/// Represents a generic type parameter that can optionally have trait bounds
/// specified inline using the `:` syntax.
///
/// # Fields
/// * `name` - The name of the generic
/// * `bounds` - A vec of all the bounds of the generic
/// * `span` - Source location of the parameter
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct GenericParam
{
	pub name: Ident,
	pub bounds: Vec<WhereBound>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(dead_code)]
impl GenericParam
{
	/// Creates a generic parameter without bounds
	pub const fn simple(name: Ident, span: Span) -> Self
	{
		return Self {
			name,
			bounds: Vec::new(),
			span,
		};
	}

	/// Returns true if this parameter has trait bounds
	pub const fn has_bounds(&self) -> bool
	{
		return !self.bounds.is_empty();
	}
}

/// Function parameter.
///
/// Represents a single parameter in a function signature.
///
/// # Fields
/// * `ty` - Parameter type
/// * `pattern` - Pattern for destructuring (can be identifier, tuple, etc.)
/// * `variadic` - Variadic param (`...`)
/// * `span` - Source location of the parameter
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct Param
{
	pub ty: Type,
	pub pattern: Pattern,
	pub variadic: bool,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Type expression.
///
/// Represents a type in the type system, including modifiers.
///
/// # Fields
/// * `modifiers` - Type modifiers (const, volatile, etc.)
/// * `core` - The core type expression
/// * `span` - Source location of the type
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct Type
{
	pub core: Box<TypeCore>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Core type expressions.
///
/// Represents the fundamental type constructs in the language.
///
/// # Variants
/// * `Base` - Named type with optional generic arguments
/// * `Reference` - Reference type (`&T` or `&mut T`)
/// * `Mutable` - Mutable type wrapper
/// * `Pointer` - Raw pointer type (`T*`)
/// * `Array` - Array type with size expression (`[T;size]`)
/// * `Tuple` - Tuple type (`(T1, T2, ...)`)
/// * `ImplTrait` - Impl trait type (`impl Trait`)
#[derive(Debug, Clone, PartialEq)]
pub enum TypeCore
{
	Base
	{
		path: Path,
		generics: Vec<Type>,
	},

	Reference
	{
		mutable: bool,
		inner: Box<TypeCore>,
	},

	Mutable
	{
		inner: Box<TypeCore>,
	},

	Pointer
	{
		inner: Box<TypeCore>,
	},

	Array
	{
		inner: Box<TypeCore>,
		size: Option<Box<Expr>>,
	},

	Tuple(Vec<Type>),

	ImplTrait
	{
		bounds: Vec<WhereBound>,
	},
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
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct RangeExpr
{
	pub start: Option<Box<Expr>>,
	pub end: Option<Box<Expr>>,
	pub inclusive: bool,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Expression node.
///
/// Represents all possible expression types in the language.
/// Expressions are constructs that evaluate to a value.
///
/// # Variants
/// * `Identifier` - Variable or constant reference
/// * `Literal` - Literal value (integer, string, etc.)
/// * `Default` - Default value constructor
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
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum Expr
{
	Identifier
	{
		path: Path,
		#[ignored(PartialEq)]
		span: Span,
	},

	Literal
	{
		value: Literal,
		#[ignored(PartialEq)]
		span: Span,
	},

	Default
	{
		heap_call: CallType,
		#[ignored(PartialEq)]
		span: Span,
	},

	Unary
	{
		op: UnaryOp,
		expr: Box<Expr>,
		#[ignored(PartialEq)]
		span: Span,
	},

	Binary
	{
		op: BinaryOp,
		lhs: Box<Expr>,
		rhs: Box<Expr>,
		#[ignored(PartialEq)]
		span: Span,
	},

	Cast
	{
		ty: Box<Type>,
		expr: Box<Expr>,
		#[ignored(PartialEq)]
		span: Span,
	},

	Call
	{
		callee: Box<Expr>,
		call_type: CallType,
		named_generics: Vec<(Ident, Type)>, // used for heap and allocators
		args: Vec<Expr>,
		#[ignored(PartialEq)]
		span: Span,
	},

	Field
	{
		base: Box<Expr>,
		name: Path,
		#[ignored(PartialEq)]
		span: Span,
	},

	Index
	{
		base: Box<Expr>,
		index: Box<Expr>,
		#[ignored(PartialEq)]
		span: Span,
	},

	Range(RangeExpr),

	Tuple
	{
		elements: Vec<Expr>,
		#[ignored(PartialEq)]
		span: Span,
	},

	Array(ArrayLiteral),

	StructInit
	{
		path: Path,
		fields: Vec<(Ident, Expr)>,
		base: Option<Box<Expr>>,
		has_rest: bool,
		#[ignored(PartialEq)]
		span: Span,
	},

	Block(Box<Block>),

	UnsafeBlock(Box<Block>),

	Switch
	{
		expr: Box<Expr>,
		arms: Vec<SwitchArm>,
		#[ignored(PartialEq)]
		span: Span,
	},

	If
	{
		cond: Box<Expr>,
		then_block: Block,
		else_branch: Option<Box<Expr>>,
		#[ignored(PartialEq)]
		span: Span,
	},

	IfVar
	{
		pattern: Pattern,
		expr: Box<Expr>,
		then_block: Block,
		else_branch: Option<Box<Expr>>,
		#[ignored(PartialEq)]
		span: Span,
	},

	Loop
	{
		label: Option<String>,
		body: Box<Block>,
		#[ignored(PartialEq)]
		span: Span,
	},
}

#[derive(Clone, Debug)]
pub enum ExprEnum
{
	Int(i128),
	Bool(bool),
	#[allow(unused)]
	String(String),
}

fn read_radix_number(lit: &Literal) -> Result<i128, ParseError>
{
	let Literal::Int { value, base, ty, span } = lit else {
		unreachable!("Called `read_radix_number` with a not `Literal::Int`: {:?}", lit);
	};
	if !ty.is_some() {
		return Err(ParseError {
			span: *span,
			kind: ParseErrorKind::CompileExprError {
				reason: "typed integers for comptime expressions is not allowed".to_string(),
			},
			context: Vec::new(),
			severity: Severity::Error
		});
	}

	return i128::from_str_radix(
		value,
		match base {
			IntBase::Binary => 2,
			IntBase::Octal => 8,
			IntBase::Decimal => 10,
			IntBase::Hexadecimal => 16,
		},
	)
	.map_err(|err| -> ParseError {
		match err.kind() {
			std::num::IntErrorKind::PosOverflow => {
				return ParseError {
					span: *span,
					kind: ParseErrorKind::CompileExprError {
						reason: "IntergetOverflow".to_string(),
					},
					context: Vec::new(),
					severity: Severity::Error
				};
			}
			std::num::IntErrorKind::NegOverflow => {
				return ParseError {
					span: *span,
					kind: ParseErrorKind::CompileExprError {
						reason: "IntergetUnderflow".to_string(),
					},
					context: Vec::new(),
					severity: Severity::Error
				};
			}
			_ => unreachable!("somthing went wrong during parsing the number"),
		}
	});
}

impl Expr
{
	pub fn comp_time_check(&self, config: &Config, source_index: SourceIndex) -> Result<bool, ParseError>
	{
		match self.eval(config, source_index)? {
			ExprEnum::Bool(b) => return Ok(b),
			_ => {
				return Err(ParseError {
					span: self.span(),
					kind: ParseErrorKind::NoCompileExpr {
						reason: "Compile-time expression must evaluate to a boolean".to_string(),
					},
					context: Vec::new(),
					severity: Severity::Error
				});
			}
		}
	}

	fn eval(&self, config: &Config, source_index: SourceIndex) -> Result<ExprEnum, ParseError>
	{
		match self {
			Expr::Literal { value, span: _, .. } => {
				return Ok(match value {
					lit @ Literal::Int { .. } => ExprEnum::Int(read_radix_number(lit)?),
					Literal::Float { span, .. } | Literal::Char { value: _, span } => {
						return Err(type_err(*span));
					}
					Literal::Bool { value, span: _ } => ExprEnum::Bool(*value),
					Literal::String { value, span: _ } => ExprEnum::String(value.clone()),
				});
			}

			Expr::Identifier { path, span } => {
				if path.has_generics() || path.glob || path.global {
					return Err(type_err(*span));
				}
				if matches!(&path.segments[0], PathSegment { name, generics, span, } if name == "cfg" && generics.is_empty())
				{
					let p = Path {
						segments: path.segments[1..].into(),
						glob: false,
						global: false,
						span: path.span,
					};
					return config.lookup(&p).map_err(|err| {
						return ParseError {
							span: *span,
							kind: ParseErrorKind::CompileExprError { reason: err },
							context: Vec::new(),
							severity: Severity::Error
						};
					});
				}
				return Err(type_err(*span));
			}

			Expr::Unary { op, expr, span } => {
				let v = expr.eval(config, source_index)?;

				match (op, v) {
					(UnaryOp::Neg, ExprEnum::Int(i)) => return Ok(ExprEnum::Int(-i)),
					(UnaryOp::Not, ExprEnum::Bool(b)) => return Ok(ExprEnum::Bool(!b)),

					_ => {
						return Err(ParseError {
							span: *span,
							kind: ParseErrorKind::NoCompileExpr {
								reason: "Invalid unary operation for given type".to_string(),
							},
							context: Vec::new(),
							severity: Severity::Error
						});
					}
				}
			}

			Expr::Binary { op, lhs, rhs, span } => {
				match op {
					BinaryOp::LogicalAnd => {
						let l = lhs.eval(config, source_index)?;
						match l {
							ExprEnum::Bool(false) => return Ok(ExprEnum::Bool(false)),
							ExprEnum::Bool(true) => {
								let r = rhs.eval(config, source_index)?;
								return match r {
									ExprEnum::Bool(b) => Ok(ExprEnum::Bool(b)),
									_ => Err(type_err(*span)),
								};
							}
							_ => return Err(type_err(*span)),
						}
					}

					BinaryOp::LogicalOr => {
						let l = lhs.eval(config, source_index)?;
						match l {
							ExprEnum::Bool(true) => return Ok(ExprEnum::Bool(true)),
							ExprEnum::Bool(false) => {
								let r = rhs.eval(config, source_index)?;
								return match r {
									ExprEnum::Bool(b) => Ok(ExprEnum::Bool(b)),
									_ => Err(type_err(*span)),
								};
							}
							_ => return Err(type_err(*span)),
						}
					}

					_ => {}
				}

				let l = lhs.eval(config, source_index)?;
				let r = rhs.eval(config, source_index)?;

				match (op, l, r) {
					(BinaryOp::Add, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Int(a + b)),
					(BinaryOp::Sub, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Int(a - b)),
					(BinaryOp::Mul, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Int(a * b)),
					(BinaryOp::Div, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Int(a / b)),
					(BinaryOp::Mod, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Int(a % b)),

					(BinaryOp::Eq, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Bool(a == b)),
					(BinaryOp::Ne, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Bool(a != b)),
					(BinaryOp::Lt, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Bool(a < b)),
					(BinaryOp::Gt, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Bool(a > b)),
					(BinaryOp::Le, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Bool(a <= b)),
					(BinaryOp::Ge, ExprEnum::Int(a), ExprEnum::Int(b)) => return Ok(ExprEnum::Bool(a >= b)),

					(BinaryOp::Eq, ExprEnum::Bool(a), ExprEnum::Bool(b)) => return Ok(ExprEnum::Bool(a == b)),
					(BinaryOp::Ne, ExprEnum::Bool(a), ExprEnum::Bool(b)) => return Ok(ExprEnum::Bool(a != b)),

					(BinaryOp::Eq, ExprEnum::String(a), ExprEnum::String(b)) => return Ok(ExprEnum::Bool(a == b)),
					(BinaryOp::Ne, ExprEnum::String(a), ExprEnum::String(b)) => return Ok(ExprEnum::Bool(a != b)),

					_ => return Err(type_err(*span)),
				}
			}

			Expr::Default { span, .. } => {
				return Err(ParseError {
					span: *span,
					kind: ParseErrorKind::NoCompileExpr {
						reason: "Can't use `default()` in an `@if` block".to_string(),
					},
					context: Vec::new(),
					severity: Severity::Error
				});
			}

			_ => {
				return Err(ParseError {
					span: self.span(),
					kind: ParseErrorKind::NoCompileExpr {
						reason: "Expression not allowed in compile-time condition".to_string(),
					},
					context: Vec::new(),
					severity: Severity::Error
				});
			}
		}
	}
}

fn type_err(span: Span) -> ParseError
{
	return ParseError {
		span,
		kind: ParseErrorKind::NoCompileExpr {
			reason: "Type mismatch in compile-time expression".to_string(),
		},
		context: Vec::new(),
		severity: Severity::Error
	};
}

/// Type of function call
///
/// # Variants
/// * `Regular` - Regular function call: `func()`
/// * `UserHeap` - User-written heap call: `func!<IO: x>()`
/// * `UserMaybeHeap` - User-written for templates call: `func?<IO: x>()`
/// * `CompilerHeap` - Compiler-generated call: `func?<IO: x>()`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum CallType
{
	/// Regular function call: `func()`
	Regular,
	/// User-written heap call: `func!<IO: x>()`
	UserHeap,
	/// User-written for templates call: `func?<IO: x>()`
	UserMaybeHeap,
	/// Compiler-generated call: `func?<IO: x>()`
	CompilerHeap,
}

impl CallType
{
	/// Returns true if this is a heap call (either user or compiler-generated)
	#[allow(dead_code)]
	pub const fn is_heap_call(self) -> bool
	{
		return matches!(
			self,
			CallType::UserHeap | CallType::CompilerHeap | CallType::UserMaybeHeap
		);
	}

	/// Returns true if this is a compiler-generated call
	#[allow(dead_code)]
	pub const fn is_compiler_generated(self) -> bool
	{
		return matches!(self, CallType::CompilerHeap);
	}

	/// Returns true if this is a user-generated maybe call
	#[allow(dead_code)]
	pub const fn is_user_maybe_call(self) -> bool
	{
		return matches!(self, CallType::UserMaybeHeap);
	}

	/// Returns true if this is a user- or compiler-generated maybe call
	#[allow(dead_code)]
	pub const fn is_maybe_call(self) -> bool
	{
		return matches!(self, CallType::UserMaybeHeap | CallType::CompilerHeap);
	}

	/// Returns true if this is a user-written call
	#[allow(dead_code)]
	pub const fn is_user_call(self) -> bool
	{
		return !matches!(self, CallType::CompilerHeap);
	}

	/// Returns true if this is a regular (non-heap) call
	#[allow(dead_code)]
	pub const fn is_regular(self) -> bool
	{
		return matches!(self, CallType::Regular);
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
#[derive(Debug, Clone, PartialEq, Eq, Spanned)]
pub enum Literal
{
	Int
	{
		value: String,
		base: IntBase,
		ty: Option<IntType>,
		span: Span,
	},
	Float
	{
		value: String,
		bits: Option<u16>,
		span: Span,
	},
	Bool
	{
		value: bool, span: Span
	},
	String
	{
		value: String, span: Span
	},
	Char
	{
		value: char, span: Span
	},
}

/// Array literal types.
///
/// Represents the two forms of array literals in the language.
///
/// # Variants
/// * `List` - Explicit element list: `[1, 2, 3]`
/// * `Repeat` - Repeated value: `[0; 10]`
#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum ArrayLiteral
{
	List
	{
		elements: Vec<Expr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Repeat
	{
		value: Box<Expr>,
		count: Box<Expr>,
		#[ignored(PartialEq)]
		span: Span,
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp
{
	/// `||`
	LogicalOr,
	/// `&&`
	LogicalAnd,
	/// `==`
	Eq,
	/// `!=`
	Ne,
	/// `<`
	Lt,
	/// `>`
	Gt,
	/// `<=`
	Le,
	/// `>=`
	Ge,
	/// `+`
	Add,
	/// `-`
	Sub,
	/// `*`
	Mul,
	/// `/`
	Div,
	/// `%`
	Mod,
	/// `&`
	BitAnd,
	/// `|`
	BitOr,
	/// `^`
	BitXor,
	/// `<<`
	Shl,
	/// `>>`
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignOp
{
	/// `=`
	Assign,
	/// `+=`
	AddAssign,
	/// `-=`
	SubAssign,
	/// `*=`
	MulAssign,
	/// `/=`
	DivAssign,
	/// `%=`
	ModAssign,
	/// `&=`
	AndAssign,
	/// `|=`
	OrAssign,
	/// `^=`
	XorAssign,
	/// `<<=`
	ShlAssign,
	/// `>>=`
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
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the declaration
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct VariableDecl
{
	pub pattern: Pattern,
	pub init: Option<Expr>,
	pub comp_const: bool,
	#[ignored(PartialEq)]
	#[allow(unused)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
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
#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum Stmt
{
	VariableDecl(VariableDecl),

	Assignment
	{
		target: Expr,
		op: AssignOp,
		value: Expr,
		#[ignored(PartialEq)]
		span: Span,
	},

	Return
	{
		value: Option<Expr>,
		#[ignored(PartialEq)]
		span: Span,
	},

	Expr(Expr),

	Break
	{
		label: Option<String>,
		value: Option<Expr>,
		#[ignored(PartialEq)]
		span: Span,
	},

	Continue
	{
		label: Option<String>,
		#[ignored(PartialEq)]
		span: Span,
	},

	If
	{
		cond: Expr,
		then_block: Block,
		else_branch: Option<Box<Stmt>>,
		#[ignored(PartialEq)]
		span: Span,
	},

	IfVar
	{
		pattern: Pattern,
		expr: Expr,
		then_block: Block,
		else_branch: Option<Box<Stmt>>,
		#[ignored(PartialEq)]
		span: Span,
	},

	While
	{
		label: Option<String>,
		cond: Expr,
		body: Block,
		#[ignored(PartialEq)]
		span: Span,
	},

	Loop
	{
		label: Option<String>,
		body: Block,
		#[ignored(PartialEq)]
		span: Span,
	},

	WhileVarLoop
	{
		label: Option<String>,
		pattern: Pattern,
		expr: Expr,
		body: Block,
		#[ignored(PartialEq)]
		span: Span,
	},

	For
	{
		label: Option<String>,
		pattern: Pattern,
		iter: Expr,
		body: Block,
		#[ignored(PartialEq)]
		span: Span,
	},

	Delete
	{
		expr: Expr,
		#[ignored(PartialEq)]
		span: Span,
	},

	Unsafe(Block),

	Block(Block),

	Directive(DirectiveNode),
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
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct Block
{
	pub stmts: Vec<Stmt>,
	pub tail_expr: Option<Box<Expr>>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Block content types.
///
/// Used to distinguish between regular blocks and top-level blocks.
///
/// # Variants
/// * `Block` - Regular block with statements
/// * `TopLevelBlock` - Top-level declarations block
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct SwitchArm
{
	pub pattern: Pattern,
	pub body: SwitchBody,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Switch arm body types.
///
/// The body of a match arm can be either a single expression or a block.
///
/// # Variants
/// * `Expr` - Single expression (requires comma)
/// * `Block` - Block of statements
#[derive(Debug, Clone, PartialEq, Spanned)]
#[allow(clippy::large_enum_variant)]
pub enum SwitchBody
{
	Expr(Expr),
	Block(Block),
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
#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum Pattern
{
	Wildcard
	{
		#[ignored(PartialEq)]
		span: Span,
		ty: Option<Type>,
	},
	Literal
	{
		value: Literal,
		#[ignored(PartialEq)]
		span: Span,
	},
	TypedIdentifier
	{
		path: Path,
		modifiers: Vec<Modifier>,
		ty: Type,
		call_constructor: Option<CallType>,
		#[ignored(PartialEq)]
		span: Span,
		mutable: bool,
	},
	Variant
	{
		path: Path,
		args: Vec<Pattern>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Tuple
	{
		patterns: Vec<Pattern>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Struct
	{
		path: Path,
		fields: Vec<(Ident, Pattern)>,
		has_rest: bool,
		#[ignored(PartialEq)]
		span: Span,
	},
	Range(RangeExpr),
	Or
	{
		patterns: Vec<Pattern>,
		#[ignored(PartialEq)]
		span: Span,
	},
}

/// Structure type declaration.
///
/// Represents a struct with named fields.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Struct name (can be qualified path)
/// * `fields` - List of (`type`, `name`, `Option<default_value>`) tuples for fields
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the struct
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct StructDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub generics: Vec<GenericParam>,
	pub where_clause: Vec<WhereConstraint>,
	pub fields: Vec<StructField>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Union type declaration.
///
/// Represents a union with named fields.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Union name (can be qualified path)
/// * `fields` - List of (type, name) tuples for fields
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the union
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct UnionDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub generics: Vec<GenericParam>,
	pub where_clause: Vec<WhereConstraint>,
	pub fields: Vec<UnionField>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Enumeration declaration.
///
/// Represents an enum where variants are integer constants.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Enum name (can be qualified path)
/// * `variants` - List of (`name`, `Option<value>`) tuples
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the enum
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct EnumDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub generics: Vec<GenericParam>,
	pub where_clause: Vec<WhereConstraint>,
	pub variants: Vec<EnumVariant>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Tagged union (Rust-style enum) declaration.
///
/// Represents an enum where variants can carry data.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Variant name (can be qualified path)
/// * `variants` - List of (`Option<type>`, `name`, `Option<value>`) tuples for variants
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the variant
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct VariantDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub generics: Vec<GenericParam>,
	pub where_clause: Vec<WhereConstraint>,
	pub variants: Vec<VariantMember>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
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
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the trait
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TraitDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub generics: Vec<GenericParam>,
	pub super_traits: Vec<WhereBound>,
	pub items: Vec<TraitItem>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Trait item types.
///
/// Items that can appear in a trait definition.
///
/// # Variants
/// * `Function` - Method signature with optional default implementation
/// * `TypeAlias` - Associated type
/// * `Const` - Associated constant
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum TraitItem
{
	Function(FunctionDecl),
	TypeAlias(TypeAliasDecl),
	Const(VariableDecl),
	AssocType(AssocTypeDecl),
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
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the impl
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct ImplDecl
{
	pub modifiers: Vec<Modifier>,
	pub generics: Vec<GenericParam>,
	pub target: ImplTarget,
	pub trait_path: Option<ImplTarget>,
	pub where_clause: Vec<WhereConstraint>,
	pub body: Vec<ImplItem>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Implementation target type.
///
/// Specifies what type an implementation applies to.
///
/// # Fields
/// * `path` - Type path
/// * `generics` - Generic arguments
/// * `span` - Source location of the target
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct ImplTarget
{
	pub path: Path,
	pub generics: Vec<Type>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Implementation block item types.
///
/// Items that can appear in an impl block.
///
/// # Variants
/// * `Function` - Method implementation
/// * `TypeAlias` - Associated type definition
/// * `Const` - Associated constant definition
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum ImplItem
{
	Function(FunctionDecl),
	TypeAlias(TypeAliasDecl),
	Const(VariableDecl),
	AssocType(AssocTypeDecl),
}

/// Generic type constraint (where clause).
///
/// Represents a constraint like `T: Trait1 + Trait2`.
///
/// # Fields
/// * `ty` - Type being constrained
/// * `bounds` - List of trait bounds
/// * `type_args` - Type arguments for the constraint
/// * `span` - Source location of the constraint
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct WhereConstraint
{
	pub ty: Path,
	pub bounds: Vec<WhereBound>,
	pub type_args: Vec<Type>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Trait bound in a where clause or generic parameter.
///
/// Represents the different types of bounds that can be specified for generic
/// parameters, either as trait paths or function trait bounds.
/// These bounds constrain what types can be used as generic arguments.
///
/// # Variants
///
/// * `Path(Path)` - A trait path bound.
/// * `Func(FuncBound)` - A function trait bound.
#[derive(Debug, Clone, PartialEq)]
pub enum WhereBound
{
	Path
	{
		path: Path,
		args: Vec<GenericArg>,
	},
	Func(FuncBound),
}

/// Generic argument in angle brackets - can be a type or an associated type binding
///
/// # Variants
/// * `Type` - A type
/// * `Binding` - A struct with name and type, like `<Item = i64>`
#[derive(Debug, Clone, PartialEq)]
pub enum GenericArg
{
	Type(Type),
	Binding
	{
		name: Ident,
		ty: Type,
		#[ignored(PartialEq)]
		#[allow(dead_code)]
		span: Span,
	},
}

/// Function trait bounds (`Fn`).
///
/// Represents bounds that specify a type must implement a function trait,
/// including the argument types and optional return type. Currently only
/// supports `Fn` but the enum structure allows for future expansion to
/// support `FnMut` and `FnOnce`.
///
/// # Variants
///
/// * `Fn { args: Vec<Type>, ret: Option<Type> }` - An `Fn` trait bound.
///
///   Fields:
///   - `args`: Vector of parameter types the function must accept
///   - `ret`: Optional return type. `None` indicates `()` (unit/void) return type
#[derive(Debug, Clone, PartialEq)]
pub enum FuncBound
{
	Fn
	{
		args: Vec<Type>, ret: Option<Type>
	},
}

/// Kinds of parse errors that can occur.
///
/// Categorizes different types of syntax errors encountered during parsing.
///
/// # Variants
/// * `UnexpectedToken` - Expected one token, found another
/// * `UnexpectedEof` - Unexpected end of file
/// * `InvalidPattern` - Pattern syntax error
/// * `UnbalancedDelimiter` - Mismatched brackets/parens/braces
/// * `InvalidType` - Type syntax error
/// * `InvalidDeclaration` - Declaration syntax error
/// * `UnexpectedItem` - Item in wrong context
/// * `Generic` - Generic error with custom message
/// * `ReservedToken` - When a reserved token is given
#[derive(Debug, Clone)]
pub enum ParseErrorKind
{
	UnexpectedToken
	{
		expected: Expected,
		found: TokenKind,
	},
	UnexpectedEof,
	InvalidPattern
	{
		reason: String,
	},
	UnbalancedDelimiter
	{
		delimiter: char,
	},
	InvalidType
	{
		reason: String,
	},
	InvalidDeclaration
	{
		reason: String,
	},
	UnexpectedItem
	{
		context: String,
		found: TokenKind,
	},
	Generic
	{
		message: String,
	},
	NoCompileExpr
	{
		reason: String,
	},
	CompileExprError
	{
		reason: String,
	},
	ReservedToken(ReservedError),
	UseOfNotAllowedInternal
	{
		reason: String,
	},
}

/// Expected token or construct description.
///
/// Describes what the parser expected to find at a given position.
///
/// # Variants
/// * `Token` - Expected a specific token
/// * `Identifier` - Expected an identifier
/// * `Type` - Expected a type expression
/// * `Pattern` - Expected a pattern
/// * `Expression` - Expected an expression
/// * `OneOf` - Expected one of several tokens
/// * `Description` - Custom expectation description
#[derive(Debug, Clone)]
pub enum Expected
{
	Token(TokenKind),
	Identifier,
	Type,
	Pattern,
	Expression,
	OneOf(Vec<TokenKind>),
	Description(String),
}

/// Parse error with location and context information.
///
/// Contains detailed information about a syntax error including its location,
/// kind, and contextual information about what was being parsed.
///
/// # Fields
/// * `span` - Source location of the error
/// * `kind` - The kind of parse error
/// * `context` - Stack of parsing contexts (e.g., "while parsing function declaration")
/// * `source_index` - Index into the source map
// #[derive(Debug, Clone, Spanned)]
pub type ParseError = Diagnostic<ParseErrorKind>;
// pub struct ParseError
// {
// 	pub span: Span,
// 	pub kind: ParseErrorKind,
// 	pub context: Vec<String>,
// 	pub source_index: SourceIndex,
// }

impl ParseError
{
	/// Creates a new parse error.
	///
	/// # Arguments
	/// * `span` - Source location of the error
	/// * `kind` - The kind of parse error
	/// * `source_index` - Index into the source map
	pub const fn new(span: Span, kind: ParseErrorKind) -> Self
	{
		return Self {
			span,
			kind,
			context: Vec::new(),
			severity: Severity::Error
		};
	}

	/// Adds context information to the error.
	///
	/// # Arguments
	/// * `ctx` - Context description
	///
	/// # Returns
	/// The error with added context
	pub fn with_context(mut self, ctx: impl Into<String>) -> Self
	{
		self.context.push(ctx.into());
		return self;
	}

	/// Creates an unexpected token error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `expected` - What was expected
	/// * `found` - What was actually found
	/// * `source_index` - Index into the source map
	pub const fn unexpected_token(span: Span, expected: Expected, found: TokenKind) -> Self
	{
		return Self::new(span, ParseErrorKind::UnexpectedToken { expected, found });
	}

	/// Creates an unexpected EOF error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `source_index` - Index into the source map
	pub const fn unexpected_eof(span: Span) -> Self
	{
		return Self::new(span, ParseErrorKind::UnexpectedEof);
	}

	/// Creates an invalid pattern error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `reason` - Why the pattern is invalid
	/// * `source_index` - Index into the source map
	pub fn invalid_pattern(span: Span, reason: impl Into<String>) -> Self
	{
		return Self::new(
			span,
			ParseErrorKind::InvalidPattern { reason: reason.into() },
		);
	}

	/// Creates an unbalanced delimiter error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `delimiter` - The unbalanced delimiter character
	/// * `source_index` - Index into the source map
	pub const fn unbalanced_delimiter(span: Span, delimiter: char) -> Self
	{
		return Self::new(span, ParseErrorKind::UnbalancedDelimiter { delimiter });
	}

	/// Creates an invalid type error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `reason` - Why the type is invalid
	/// * `source_index` - Index into the source map
	pub fn invalid_type(span: Span, reason: impl Into<String>) -> Self
	{
		return Self::new(
			span,
			ParseErrorKind::InvalidType { reason: reason.into() },
		);
	}

	/// Creates an invalid declaration error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `reason` - Why the declaration is invalid
	/// * `source_index` - Index into the source map
	pub fn invalid_declaration(span: Span, reason: impl Into<String>) -> Self
	{
		return Self::new(
			span,
			ParseErrorKind::InvalidDeclaration { reason: reason.into() },
		);
	}

	/// Creates an unexpected item error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `context` - Parsing context where item was unexpected
	/// * `found` - The unexpected token
	/// * `source_index` - Index into the source map
	pub fn unexpected_item(span: Span, context: impl Into<String>, found: TokenKind)
	-> Self
	{
		return Self::new(
			span,
			ParseErrorKind::UnexpectedItem {
				context: context.into(),
				found,
			},
		);
	}

	/// Creates a generic error with custom message.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `message` - Error message
	/// * `source_index` - Index into the source map
	pub fn generic(span: Span, message: impl Into<String>) -> Self
	{
		return Self::new(
			span,
			ParseErrorKind::Generic {
				message: message.into(),
			},
		);
	}
}

impl std::fmt::Display for ParseError
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "Parse error at {:?}: ", self.span)?;

		match &self.kind {
			ParseErrorKind::UnexpectedToken { expected, found } => {
				write!(f, "expected {}, found {:?}", expected, found)?;
			}
			ParseErrorKind::UnexpectedEof => {
				write!(f, "unexpected end of file")?;
			}
			ParseErrorKind::InvalidPattern { reason } => {
				write!(f, "invalid pattern: {}", reason)?;
			}
			ParseErrorKind::UnbalancedDelimiter { delimiter } => {
				write!(f, "unbalanced delimiter '{}'", delimiter)?;
			}
			ParseErrorKind::InvalidType { reason } => {
				write!(f, "invalid type: {}", reason)?;
			}
			ParseErrorKind::InvalidDeclaration { reason } => {
				write!(f, "invalid declaration: {}", reason)?;
			}
			ParseErrorKind::UnexpectedItem { context, found } => {
				write!(f, "unexpected item in {}: found {:?}", context, found)?;
			}
			ParseErrorKind::ReservedToken(e) => {
				write!(f, "reserved token: {:?}", e.token)?;
			}
			ParseErrorKind::Generic { message } => {
				write!(f, "{}", message)?;
			}
			ParseErrorKind::CompileExprError { reason } => {
				write!(f, "compile time expr error: {}", reason)?;
			}
			ParseErrorKind::NoCompileExpr { reason } => {
				write!(f, "no compile time expr: {}", reason)?;
			}
			ParseErrorKind::UseOfNotAllowedInternal { reason } => {
				write!(f, "{}", reason)?; //TODO
			}
		}

		if !self.context.is_empty() {
			write!(f, "\n  while parsing: {}", self.context.join(" → "))?;
		}

		return Ok(());
	}
}

impl From<ParseError> for CompileError
{
	fn from(value: ParseError) -> Self
	{
		return CompileError::Parse(value);
	}
}

impl CompileDiagnostic for ParseError
{
	fn fmt_with_source(&self, f: &mut impl std::fmt::Write, sm: &crate::source_map::SourceMap) -> std::fmt::Result
	{
		return write!(
			f,
			"{}",
			self.span.format_error(self.span.source_index, sm, &format!("{self}"))
		);
	}
}

impl std::fmt::Display for Expected
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			Expected::Token(tk) => write!(f, "{:?}", tk),
			Expected::Identifier => write!(f, "identifier"),
			Expected::Type => write!(f, "type"),
			Expected::Pattern => write!(f, "pattern"),
			Expected::Expression => write!(f, "expression"),
			Expected::OneOf(tokens) => {
				write!(f, "one of: ")?;
				for (i, tk) in tokens.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{:?}", tk)?;
				}
				Ok(())
			}
			Expected::Description(s) => write!(f, "{}", s),
		};
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
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the type alias
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypeAliasDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub generics: Vec<GenericParam>,
	pub ty: Type,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Assoc type declaration.
///
/// Represents a type alias like `assoc Int = i32;`.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Alias name (can be qualified path)
/// * `ty` - Type being aliased
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the type alias
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct AssocTypeDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub generics: Vec<GenericParam>,
	pub ty: Option<Type>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Module declaration.
///
/// Represents a module containing top-level declarations.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Module name (can be qualified path)
/// * `body` - Declarations within the module
/// * `kind` - The module kind, Inline or External
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location of the module
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct ModuleDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub kind: ModuleKind,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// Module kind.
///
/// # Fields
/// * `Inline` - A `TopLevelBlock` that represents if the module is defined inline (`module name { /* code */ }`)
/// * `External` - The module is just `module name;`
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleKind
{
	Inline(TopLevelBlock),
	External,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Restrictions
{
	no_struct_literal: bool,
}

impl Restrictions
{
	const NONE: Self = Self {
		no_struct_literal: false,
	};

	const NO_STRUCT_LITERAL: Self = Self {
		no_struct_literal: true,
	};
}

/// Docs comment
///
/// # Fields
/// * `content` - The docs comment
/// * `span` - Source location of the docs comment
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct DocsComment
{
	pub content: String,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// A field in a struct declaration
///
/// # Fields
/// * `ty` - The type of the field
/// * `name` - The name of the field
/// * `default_value` - Optinal default value `member: i64 = 0`
/// * `modifiers` - Visibility and other modifiers
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location information of the field
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct StructField
{
	pub ty: Type,
	pub name: Ident,
	pub default_value: Option<Expr>,
	pub modifiers: Vec<Modifier>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// A field in a union declaration
///
/// # Fields
/// * `ty` - The type of the field
/// * `name` - The name of the field
/// * `modifiers` - Visibility and other modifiers
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location information of the field
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct UnionField
{
	pub ty: Type,
	pub name: Ident,
	pub modifiers: Vec<Modifier>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// A variant in an enum declaration
///
/// # Fields
/// * `name` - The name of the field
/// * `value` - Optional falue of the field `Member = 0`
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location information of the field
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct EnumVariant
{
	pub name: Ident,
	pub value: Option<Expr>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

/// A member in a variant declaration
///
/// # Fields
/// * `ty` - The type of the field
/// * `name` - The name of the field
/// * `value` - Optinal value `Member = 0`
/// * `docs` - Optional docs comments, mostly for lsp and library exports
/// * `span` - Source location information of the field
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct VariantMember
{
	pub ty: Option<Type>,
	pub name: Ident,
	pub value: Option<Expr>,
	#[ignored(PartialEq)]
	pub docs: Option<DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

pub const ALLOWED_HEAP_GENERICS: [&str; 2] = ["IO", "Alloc"];

impl<'s, 'c, T> Parser<'s, 'c, T>
where
	T: LexerTrait<'s, 'c>,
{
	fn peek(&mut self) -> Result<&Token, ParseError>
	{
		if let Some(token) = self.buffered_token.as_ref() {
			return Ok(token);
		}

		let token: Result<&Token, ParseError> = self
			.lexer
			.peek()
			.map(|x| return x.as_ref())
			.transpose()
			.map_err(|err| return err.clone())?
			.ok_or_else(|| {
				return ParseError::unexpected_eof(self.last_span);
			});

		let Ok(tok) = token else {
			return token;
		};
		match tok.check_reserved() {
			Ok(()) => return Ok(tok),
			Err(e) => {
				return Err(ParseError {
					span: tok.span(),
					kind: ParseErrorKind::ReservedToken(e),
					context: Vec::new(),
					severity: Severity::Error
				});
			}
		}
	}

	fn next(&mut self) -> Result<Token, ParseError>
	{
		if let Some(tok) = self.buffered_token.take() {
			self.last_span = tok.span;
			return Ok(tok);
		}

		let tok: Token = self.lexer.next().transpose()?.ok_or_else(|| {
			return ParseError::unexpected_eof(self.last_span);
		})?;

		self.last_span = tok.span;
		match tok.check_reserved() {
			Ok(()) => return Ok(tok),
			Err(e) => {
				return Err(ParseError {
					span: tok.span(),
					kind: ParseErrorKind::ReservedToken(e),
					context: Vec::new(),
					severity: Severity::Error
				});
			}
		}
	}

	fn make_checkpoint(&self) -> (Peekable<T>, Span, Option<Token>)
	{
		return (self.lexer.clone(), self.last_span, self.buffered_token.clone());
	}

	fn load_checkpoint(&mut self, checkpoint: (Peekable<T>, Span, Option<Token>))
	{
		let (lexer, last_span, buffered_token) = checkpoint;
		self.lexer = lexer;
		self.last_span = last_span;
		self.buffered_token = buffered_token;
	}

	fn peek_kind(&mut self) -> Result<&TokenKind, ParseError>
	{
		return Ok(&self.peek()?.kind);
	}

	fn at(&mut self, kind: &TokenKind) -> Result<bool, ParseError>
	{
		return Ok(self.peek_kind()? == kind);
	}

	fn consume(&mut self, kind: &TokenKind) -> Result<bool, ParseError>
	{
		if self.at(kind)? {
			self.next()?;
			return Ok(true);
		}
		return Ok(false);
	}

	fn consume_greater_than(&mut self) -> Result<bool, ParseError>
	{
		if self.at(&TokenKind::GreaterThan)? {
			self.next()?;
			return Ok(true);
		}

		if self.at(&TokenKind::RShift)? {
			let rshift_tok: Token = self.next()?;

			let virtual_gt: Token = Token {
				kind: TokenKind::GreaterThan,
				span: rshift_tok.span,
			};

			self.buffered_token = Some(virtual_gt);

			return Ok(true);
		}

		return Ok(false);
	}

	fn expect(&mut self, expected: &TokenKind) -> Result<Token, ParseError>
	{
		let tok: &Token = self.peek()?;

		if &tok.kind == expected {
			return self.next();
		}
		let err_tok: Token = tok.clone();
		return Err(ParseError::unexpected_token(
			err_tok.span,
			Expected::Token(expected.clone()),
			err_tok.kind,
		));
	}

	/// Parse a complete program.
	///
	/// Entry point for parsing a source file. Parses all top-level declarations
	/// until EOF is reached.
	///
	/// # Returns
	/// * `Ok(AST)` - The parsed program AST
	/// * `Err(ParseError)` - If a syntax error is encountered
	///
	/// # Example
	/// ```ignore
	/// # use crate::{Parser, AST, ParseError};
	/// # fn example(parser: &mut Parser) -> Result<(), ParseError> {
	/// let program = parser.parse_program()?;
	/// println!("Parsed {} items", program.items.len());
	/// # Ok(())
	/// # }
	/// ```
	pub fn parse_program(&mut self) -> Result<AST, ParseError>
	{
		let top_level_block: TopLevelBlock = self.parse_top_level_block()?;

		return Ok(AST {
			top_level_block,
			source_index: self.source_index,
		});
	}

	fn parse_top_level_block(&mut self) -> Result<TopLevelBlock, ParseError>
	{
		let mut items: Vec<TopLevelDecl> = Vec::new();

		while !matches!(self.peek()?.kind, TokenKind::Eof | TokenKind::RightBrace) {
			if self.consume(&TokenKind::Semicolon)? {
				continue;
			}
			let decl: TopLevelDecl = self.parse_top_level_decl()?;
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

		return Ok(TopLevelBlock { items, span });
	}

	fn parse_top_level_decl(&mut self) -> Result<TopLevelDecl, ParseError>
	{
		let decl_kind: DeclKind = self.peek_declaration_kind()?;

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
			DeclKind::Module => {
				let module_decl: ModuleDecl = self.parse_module()?;
				TopLevelDecl::Module(module_decl)
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
				let variant_decl: VariantDecl = self.parse_variant()?;
				TopLevelDecl::Variant(variant_decl)
			}
			DeclKind::AssocType => {
				todo!("make error or something")
			}
		};

		return Ok(ret);
	}

	fn peek_declaration_kind(&mut self) -> Result<DeclKind, ParseError>
	{
		let checkpoint: Peekable<T> = self.lexer.clone();
		let checkpoint_span: Span = self.last_span;

		loop {
			match self.peek_kind()? {
				TokenKind::Pub
				| TokenKind::Export
				| TokenKind::Unsafe
				| TokenKind::Inline
				| TokenKind::Volatile
				| TokenKind::DocsComment(..) => {
					self.next()?;
				}
				TokenKind::Const => {
					self.next()?;
					if self.at(&TokenKind::FuncDef)? {
						self.lexer = checkpoint;
						self.last_span = checkpoint_span;
						return Ok(DeclKind::Function);
					}
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Variable);
				}
				TokenKind::Directive(_) => {
					self.next()?;

					if self.at(&TokenKind::LeftParen)? {
						self.skip_until_balanced_paren()?;
					}

					loop {
						#[allow(clippy::match_same_arms)]
						match self.peek_kind()? {
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
							| TokenKind::Module
							| TokenKind::Impl
							| TokenKind::Trait
							| TokenKind::Var
							| TokenKind::Const => {
								break;
							}
							TokenKind::Pub
							| TokenKind::Export
							| TokenKind::Unsafe
							| TokenKind::Inline
							| TokenKind::Volatile
							| TokenKind::Directive(_) => {
								break;
							}
							_ => {
								self.next()?;
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
				TokenKind::Assoc => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::AssocType);
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
				TokenKind::Module => {
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Ok(DeclKind::Module);
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
					let tok: Token = self.peek()?.clone();
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Err(ParseError::unexpected_item(
						tok.span,
						"declaration",
						tok.kind,
					));
				}
			}
		}
	}

	fn skip_until_balanced_paren(&mut self) -> Result<(), ParseError>
	{
		if !self.at(&TokenKind::LeftParen)? {
			return Ok(());
		}
		self.next()?; // (

		let mut depth = 1;
		while depth > 0 {
			match self.peek_kind()? {
				TokenKind::LeftParen => {
					depth += 1;
					self.next()?;
				}
				TokenKind::RightParen => {
					depth -= 1;
					self.next()?;
				}
				TokenKind::Eof => {
					return Err(ParseError::unexpected_eof(self.peek()?.span));
				}
				_ => {
					self.next()?;
				}
			}
		}
		return Ok(());
	}

	fn parse_directive_node(&mut self) -> Result<DirectiveNode, ParseError>
	{
		let start: Span = self.peek()?.span;

		let modifiers: Vec<Modifier> = self.parse_modifiers()?;

		let directive: Directive = if modifiers.len() == 1 {
			if let Some(Modifier::Directive(d)) = modifiers.into_iter().next() {
				d
			} else {
				let tok: Token = self.next()?;
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::Description("directive".to_string()),
					tok.kind,
				));
			}
		} else if modifiers.is_empty() {
			let tok: Token = self.next()?;
			match tok.kind {
				TokenKind::Directive(d) => self.parse_directive_kind(d, Vec::new())?,
				_ => unreachable!("Bug: Token should be a directive"),
			}
		} else {
			let tok: Token = self.next()?;
			return Err(ParseError::unexpected_token(
				tok.span,
				Expected::Description("directive".to_string()),
				tok.kind,
			));
		};

		let body: Option<BlockContent> = if self.at(&TokenKind::LeftBrace)? {
			self.next()?;
			let content: BlockContent = if self.should_parse_as_top_level_block(&directive) {
				BlockContent::TopLevelBlock(self.parse_top_level_block()?)
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

	fn should_parse_as_top_level_block(&self, _directive: &Directive) -> bool
	{
		todo!("Directives with blocks are not supported yet")
	}

	fn parse_directive_kind(
		&mut self,
		direct: lexer::Directive,
		modifiers: Vec<Modifier>,
	) -> Result<Directive, ParseError>
	{
		return match direct {
			lexer::Directive::Use => Ok(Directive::Use {
				visibility: get_visibility(&modifiers),
				modifers: modifiers,
				use_path: self.get_path_with_glob()?,
			}),
			lexer::Directive::Import => {
				let incl: Token = self.next()?;
				let ret = match &incl.kind {
					TokenKind::StringLiteral(str) => Directive::Import {
						visibility: get_visibility(&modifiers),
						modifers: modifiers,
						import: str.clone(),
					},
					_ => {
						return Err(ParseError::unexpected_token(
							incl.span,
							Expected::Token(TokenKind::StringLiteral(String::new())),
							incl.kind,
						));
					}
				};
				Ok(ret)
			}
			lexer::Directive::Custom(name) => {
				let params = if self.at(&TokenKind::LeftParen)? {
					self.parse_directive_params()?
				} else {
					Vec::new()
				};
				Ok(Directive::Custom { name, params })
			}
		};
	}

	#[allow(clippy::needless_pass_by_ref_mut)]
	fn parse_directive_params(&mut self) -> Result<Vec<DirectiveParam>, ParseError>
	{
		unimplemented!("don't know what to do for this yet");
		// if !self.consume(&TokenKind::LeftParen)? {
		// 	return Ok(Vec::new());
		// }
		// let mut params: Vec<DirectiveParam> = Vec::new();
		// loop {
		// 	if self.at(&TokenKind::RightParen)? {
		// 		break;
		// 	}
		//
		// 	let is_negative: bool = self.consume(&TokenKind::Minus)?;
		//
		// 	let tok_span: Span = self.peek()?.span();
		// 	let tok_kind: TokenKind = self.peek_kind()?.clone();
		//
		// 	let tok: Token = self.next()?;
		//
		// 	let arg: DirectiveParam = match tok.kind {
		// 		TokenKind::StringLiteral(s) => {
		// 			if is_negative {
		// 				return Err(ParseError::invalid_pattern(
		// 					tok_span,
		// 					"A string can't be negative",
		// 					self.source_index,
		// 				));
		// 			}
		// 			DirectiveParam::Literal(Literal::String {
		// 				value: s,
		// 				span: tok_span,
		// 			})
		// 		}
		// 		TokenKind::IntLiteral(i) => {
		// 			let value: i64 = if is_negative { -i } else { i };
		// 			DirectiveParam::Literal(Literal::Int { value, span: tok_span })
		// 		}
		// 		TokenKind::FloatLiteral(f) => {
		// 			let value: f64 = if is_negative { -f } else { f };
		// 			DirectiveParam::Literal(Literal::Float { value, span: tok_span })
		// 		}
		// 		TokenKind::CharLiteral(c) => {
		// 			if is_negative {
		// 				return Err(ParseError::invalid_pattern(
		// 					tok_span,
		// 					"A character can't be negative",
		// 					self.source_index,
		// 				));
		// 			}
		// 			DirectiveParam::Literal(Literal::Char {
		// 				value: c,
		// 				span: tok_span,
		// 			})
		// 		}
		// 		TokenKind::True => {
		// 			if is_negative {
		// 				return Err(ParseError::invalid_pattern(
		// 					tok_span,
		// 					"A bool can't be negative",
		// 					self.source_index,
		// 				));
		// 			}
		// 			DirectiveParam::Literal(Literal::Bool {
		// 				value: true,
		// 				span: tok_span,
		// 			})
		// 		}
		// 		TokenKind::False => {
		// 			if is_negative {
		// 				return Err(ParseError::invalid_pattern(
		// 					tok_span,
		// 					"A bool can't be negative",
		// 					self.source_index,
		// 				));
		// 			}
		// 			DirectiveParam::Literal(Literal::Bool {
		// 				value: false,
		// 				span: tok_span,
		// 			})
		// 		}
		// 		TokenKind::Identifier(ident) => {
		// 			if is_negative {
		// 				return Err(ParseError::invalid_pattern(
		// 					tok_span,
		// 					"An identifier can't be negative",
		// 					self.source_index,
		// 				));
		// 			}
		//
		// 			match self.peek_kind()? {
		// 				TokenKind::Equals => {
		// 					self.next()?; // =
		//
		// 					let value_is_negative = self.consume(&TokenKind::Minus)?;
		//
		// 					let token_span = self.peek()?.span();
		// 					let token = self.next()?;
		//
		// 					let lit: Literal = match token.kind {
		// 						TokenKind::StringLiteral(s) => {
		// 							if value_is_negative {
		// 								return Err(ParseError::invalid_pattern(
		// 									token_span,
		// 									"Cannot negate a string literal",
		// 									self.source_index,
		// 								));
		// 							}
		// 							Literal::String {
		// 								value: s,
		// 								span: token_span,
		// 							}
		// 						}
		// 						TokenKind::IntLiteral(i) => {
		// 							let value: i64 = if value_is_negative { -i } else { i };
		// 							Literal::Int {
		// 								value,
		// 								span: token_span,
		// 							}
		// 						}
		// 						TokenKind::FloatLiteral(f) => {
		// 							let value: f64 = if value_is_negative { -f } else { f };
		// 							Literal::Float {
		// 								value,
		// 								span: token_span,
		// 							}
		// 						}
		// 						TokenKind::CharLiteral(c) => {
		// 							if value_is_negative {
		// 								return Err(ParseError::invalid_pattern(
		// 									token_span,
		// 									"Cannot negate a character literal",
		// 									self.source_index,
		// 								));
		// 							}
		// 							Literal::Char {
		// 								value: c,
		// 								span: token_span,
		// 							}
		// 						}
		// 						TokenKind::True => {
		// 							if value_is_negative {
		// 								return Err(ParseError::invalid_pattern(
		// 									token_span,
		// 									"Cannot negate a boolean literal",
		// 									self.source_index,
		// 								));
		// 							}
		// 							Literal::Bool {
		// 								value: true,
		// 								span: token_span,
		// 							}
		// 						}
		// 						TokenKind::False => {
		// 							if value_is_negative {
		// 								return Err(ParseError::invalid_pattern(
		// 									token_span,
		// 									"Cannot negate a boolean literal",
		// 									self.source_index,
		// 								));
		// 							}
		// 							Literal::Bool {
		// 								value: false,
		// 								span: token_span,
		// 							}
		// 						}
		// 						_ => {
		// 							return Err(ParseError::invalid_pattern(
		// 								token_span,
		// 								format!("Expected an identifier or a literal, got {:?}", tok_kind),
		// 								self.source_index,
		// 							));
		// 						}
		// 					};
		// 					DirectiveParam::Named { name: ident, arg: lit }
		// 				}
		// 				_ => DirectiveParam::Identifier(ident),
		// 			}
		// 		}
		// 		_ => {
		// 			return Err(ParseError::invalid_pattern(
		// 				tok_span,
		// 				format!("Expected an identifier or a literal, got {:?}", tok_kind),
		// 				self.source_index,
		// 			));
		// 		}
		// 	};
		// 	params.push(arg);
		// 	if self.at(&TokenKind::RightParen)? {
		// 		break;
		// 	}
		// 	if !self.consume(&TokenKind::Comma)? {
		// 		break;
		// 	}
		// }
		// self.expect(&TokenKind::RightParen)?;
		// return Ok(params);
	}

	fn parse_var_decl(&mut self) -> Result<VariableDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let tok: Token = self.next()?;
		let span: Span = tok.span;
		if !matches!(tok.kind, TokenKind::Const | TokenKind::Var) {
			unreachable!(
				"Bug: expected const or let for a variable declaration, got: {:?}",
				tok.kind
			);
		}
		let comp_const: bool = tok.kind == TokenKind::Const;

		let pattern: Pattern = self.parse_pattern()?;

		let init: Option<Expr> = if self.at(&TokenKind::Equals)? {
			self.next()?;
			Some(self.parse_expr()?)
		} else {
			None
		};

		return Ok(VariableDecl {
			pattern,
			init,
			docs,
			comp_const,
			span: self.last_span.merge(&span),
		});
	}

	fn parse_type(&mut self) -> Result<Type, ParseError>
	{
		let span: Span = self.peek()?.span();
		let core: TypeCore = self.parse_type_core()?;
		return Ok(Type {
			core: Box::new(self.parse_type_suffix(core)?),
			span: span.merge(&self.last_span),
		});
	}

	fn parse_type_core(&mut self) -> Result<TypeCore, ParseError>
	{
		let tok: &Token = self.peek()?;
		match &tok.kind {
			TokenKind::Impl => {
				self.next()?; // impl
				let bounds: Vec<WhereBound> = self.parse_trait_bounds()?;
				return Ok(TypeCore::ImplTrait { bounds });
			}
			TokenKind::Mut => {
				self.next()?; // mut
				let inner: Box<TypeCore> = Box::new(self.parse_type_core()?);
				return Ok(TypeCore::Mutable { inner });
			}
			TokenKind::Identifier(_) | TokenKind::DoubleColon => {
				let path: Path = self.get_path()?;
				let generics: Vec<Type> = if self.at(&TokenKind::LessThan)? {
					self.parse_type_generics()?
				} else {
					Vec::new()
				};

				return Ok(TypeCore::Base { path, generics });
			}
			TokenKind::Ampersand => {
				self.next()?;
				let mutable: bool = self.at(&TokenKind::Mut)?;
				if mutable {
					self.next()?;
				}
				return Ok(TypeCore::Reference {
					mutable,
					inner: Box::new(self.parse_type_core()?),
				});
			}
			TokenKind::LeftParen => {
				self.next()?; // (

				if self.consume(&TokenKind::RightParen)? {
					return Ok(TypeCore::Tuple(Vec::new()));
				}

				let mut types: Vec<Type> = vec![self.parse_type()?];

				if self.consume(&TokenKind::Comma)? {
					if !self.at(&TokenKind::RightParen)? {
						loop {
							types.push(self.parse_type()?);
							if !self.consume(&TokenKind::Comma)? {
								break;
							}
							if self.at(&TokenKind::RightParen)? {
								break;
							}
						}
					}
					self.expect(&TokenKind::RightParen)?;
					return Ok(TypeCore::Tuple(types));
				}
				self.expect(&TokenKind::RightParen)?;
				let ty: Type = types
					.into_iter()
					.next()
					.expect("this should already be cought in the code before, because of `vec![self.parse_type()?]");
				return Ok(*ty.core);
			}
			TokenKind::LeftBracket => {
				self.next()?; // [

				let base_type: TypeCore = self.parse_type_core()?;

				let complete_type: TypeCore = self.parse_type_suffix(base_type)?;

				let size: Option<Box<Expr>> = if self.consume(&TokenKind::Semicolon)? {
					Some(Box::new(self.parse_expr()?))
				} else {
					None
				};

				self.expect(&TokenKind::RightBracket)?;

				return Ok(TypeCore::Array {
					inner: Box::new(complete_type),
					size,
				});
			}
			_ => {
				let err_tok: Token = tok.clone();
				return Err(ParseError::invalid_type(
					err_tok.span,
					"expected '&', 'mut', identifier, '[' or '(' to start a type",
				));
			}
		}
	}

	fn parse_type_suffix(&mut self, mut base: TypeCore) -> Result<TypeCore, ParseError>
	{
		while matches!(self.peek_kind()?, TokenKind::Star) {
			self.next()?; // *
			base = TypeCore::Pointer { inner: Box::new(base) };
		}
		return Ok(base);
	}

	fn get_path(&mut self) -> Result<Path, ParseError>
	{
		let path: Path = self.get_path_allow_internals()?;

		for p in &path.segments {
			if p.name.contains('#') {
				return Err(ParseError {
					span: p.span,
					kind: ParseErrorKind::UseOfNotAllowedInternal {
						reason: "Internal name is not allowed at this place (the use of `#`)".to_string(),
					},
					context: Vec::new(),
					severity: Severity::Error
				});
			}
		}

		return Ok(path);
	}

	fn get_path_allow_internals(&mut self) -> Result<Path, ParseError>
	{
		let start_span: Span = self.peek()?.span();
		let mut segments: Vec<PathSegment> = Vec::new();

		let global: bool = self.consume(&TokenKind::DoubleColon)?;

		loop {
			let tok: Token = self.next()?;
			let segment_start = tok.span;
			let name: Ident = match tok.kind {
				TokenKind::Identifier(s) => s,
				_ => {
					return Err(ParseError::unexpected_token(
						tok.span,
						Expected::Identifier,
						tok.kind,
					));
				}
			};

			let generics: Vec<Type> = if self.peek()?.kind == TokenKind::DoubleColon {
				let checkpoint: (Peekable<T>, Span, Option<Token>) = self.make_checkpoint();
				self.next()?; // ::

				if self.peek()?.kind == TokenKind::LessThan {
					self.parse_type_generics()?
				} else {
					self.load_checkpoint(checkpoint);
					Vec::new()
				}
			} else {
				Vec::new()
			};

			segments.push(PathSegment {
				name,
				generics,
				span: segment_start.merge(&self.last_span),
			});

			if self.peek()?.kind != TokenKind::DoubleColon {
				break;
			}

			let checkpoint: (Peekable<T>, Span, Option<Token>) = self.make_checkpoint();
			self.next()?; // ::

			if !matches!(self.peek()?.kind, TokenKind::Identifier(_)) {
				self.load_checkpoint(checkpoint);
				break;
			}
		}

		if self.peek()?.kind == TokenKind::DoubleColon {
			let checkpoint = self.make_checkpoint();
			self.next()?; // ::
			if self.at(&TokenKind::Star)? {
				// Someone wrote foo::* outside of @use — give a clear error
				return Err(ParseError::generic(
					self.peek()?.span(),
					"glob imports (`::*`) are only allowed in `@use` directives",
				));
			}
			self.load_checkpoint(checkpoint);
		}

		return Ok(Path {
			segments,
			glob: false,
			global,
			span: start_span.merge(&self.last_span),
		});
	}

	fn get_path_with_glob(&mut self) -> Result<Path, ParseError>
	{
		let start_span: Span = self.peek()?.span();
		let mut segments: Vec<PathSegment> = Vec::new();

		let global: bool = self.consume(&TokenKind::DoubleColon)?;

		loop {
			let tok: Token = self.next()?;
			let segment_start = tok.span;
			let name: Ident = match tok.kind {
				TokenKind::Identifier(s) => s,
				_ => {
					return Err(ParseError::unexpected_token(
						tok.span,
						Expected::Identifier,
						tok.kind,
					));
				}
			};

			let generics: Vec<Type> = if self.peek()?.kind == TokenKind::DoubleColon {
				let checkpoint: (Peekable<T>, Span, Option<Token>) = self.make_checkpoint();
				self.next()?; // ::

				if self.peek()?.kind == TokenKind::LessThan {
					self.parse_type_generics()?
				} else {
					self.load_checkpoint(checkpoint);
					Vec::new()
				}
			} else {
				Vec::new()
			};

			segments.push(PathSegment {
				name,
				generics,
				span: segment_start.merge(&self.last_span),
			});

			if self.peek()?.kind != TokenKind::DoubleColon {
				break;
			}

			let checkpoint: (Peekable<T>, Span, Option<Token>) = self.make_checkpoint();
			self.next()?; // ::

			if !matches!(self.peek()?.kind, TokenKind::Identifier(_)) {
				self.load_checkpoint(checkpoint);
				break;
			}
		}

		let glob: bool = if self.peek()?.kind == TokenKind::DoubleColon {
			let checkpoint: (Peekable<T>, Span, Option<Token>) = self.make_checkpoint();
			self.next()?; // ::
			if self.at(&TokenKind::Star)? {
				self.next()?; // *
				true
			} else {
				self.load_checkpoint(checkpoint);
				false
			}
		} else {
			false
		};

		return Ok(Path {
			segments,
			glob,
			global,
			span: start_span.merge(&self.last_span),
		});
	}

	fn get_generics(&mut self) -> Result<Vec<GenericParam>, ParseError>
	{
		if !self.consume(&TokenKind::LessThan)? {
			return Ok(Vec::new());
		}

		let mut generics: Vec<GenericParam> = Vec::new();

		if self.consume_greater_than()? {
			return Ok(generics);
		}

		loop {
			let start_span: Span = self.peek()?.span;
			let tok: Token = self.next()?;

			let name: Ident = match tok.kind {
				TokenKind::Identifier(name) => name,
				_ => {
					return Err(ParseError::unexpected_token(
						tok.span,
						Expected::Identifier,
						tok.kind,
					));
				}
			};

			let bounds: Vec<WhereBound> = if self.consume(&TokenKind::Colon)? {
				self.parse_trait_bounds()?
			} else {
				Vec::new()
			};

			generics.push(GenericParam {
				name,
				bounds,
				span: start_span.merge(&self.last_span),
			});

			if self.consume_greater_than()? {
				break;
			}

			if !self.consume(&TokenKind::Comma)? {
				let tok: Token = self.next()?;
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::OneOf(vec![TokenKind::Comma, TokenKind::GreaterThan]),
					tok.kind,
				));
			}

			if self.consume_greater_than()? {
				break;
			}
		}

		return Ok(generics);
	}

	fn parse_expr(&mut self) -> Result<Expr, ParseError>
	{
		return self.parse_expr_with_restrictions(Restrictions::NONE);
	}

	pub fn parse_expr_no_struct(&mut self) -> Result<Expr, ParseError>
	{
		return self.parse_expr_with_restrictions(Restrictions::NO_STRUCT_LITERAL);
	}

	fn parse_expr_with_restrictions(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		return self.parse_logical_or(restrictions);
	}

	fn parse_logical_or(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut lhs: Expr = self.parse_logical_and(restrictions)?;

		while self.consume(&TokenKind::Or)? {
			let rhs: Expr = self.parse_logical_and(restrictions)?;
			lhs = Expr::Binary {
				op: BinaryOp::LogicalOr,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_logical_and(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut lhs: Expr = self.parse_bitwise_or(restrictions)?;

		while self.consume(&TokenKind::And)? {
			let rhs: Expr = self.parse_bitwise_or(restrictions)?;
			lhs = Expr::Binary {
				op: BinaryOp::LogicalAnd,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_bitwise_or(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut lhs: Expr = self.parse_bitwise_xor(restrictions)?;

		while self.at(&TokenKind::Pipe)? {
			self.next()?;
			let rhs: Expr = self.parse_bitwise_xor(restrictions)?;
			lhs = Expr::Binary {
				op: BinaryOp::BitOr,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_bitwise_xor(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut lhs: Expr = self.parse_bitwise_and(restrictions)?;

		while self.at(&TokenKind::Caret)? {
			self.next()?;
			let rhs: Expr = self.parse_bitwise_and(restrictions)?;
			lhs = Expr::Binary {
				op: BinaryOp::BitXor,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_bitwise_and(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut lhs: Expr = self.parse_equality(restrictions)?;

		while self.at(&TokenKind::Ampersand)? {
			self.next()?;
			let rhs: Expr = self.parse_equality(restrictions)?;
			lhs = Expr::Binary {
				op: BinaryOp::BitAnd,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_equality(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut lhs: Expr = self.parse_relational(restrictions)?;

		loop {
			let op: BinaryOp = match self.peek_kind()? {
				TokenKind::EqualsEquals => BinaryOp::Eq,
				TokenKind::BangEquals => BinaryOp::Ne,
				_ => break,
			};

			self.next()?;
			let rhs: Expr = self.parse_relational(restrictions)?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_relational(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut lhs: Expr = self.parse_shift(restrictions)?;

		loop {
			let op: BinaryOp = match self.peek_kind()? {
				TokenKind::LessThan => BinaryOp::Lt,
				TokenKind::GreaterThan => BinaryOp::Gt,
				TokenKind::LessEquals => BinaryOp::Le,
				TokenKind::GreaterEquals => BinaryOp::Ge,
				_ => break,
			};

			self.next()?;
			let rhs: Expr = self.parse_shift(restrictions)?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_shift(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut lhs: Expr = self.parse_range(restrictions)?;

		loop {
			let op: BinaryOp = match self.peek_kind()? {
				TokenKind::LShift => BinaryOp::Shl,
				TokenKind::RShift => BinaryOp::Shr,
				_ => break,
			};

			self.next()?;
			let rhs: Expr = self.parse_range(restrictions)?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_range(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();

		if self.at(&TokenKind::DotDot)? || self.at(&TokenKind::DotDotEquals)? {
			let inclusive = self.at(&TokenKind::DotDotEquals)?;
			self.next()?; // .. | ..=

			let end: Option<Box<Expr>> = if self.is_range_end() {
				None
			} else {
				Some(Box::new(self.parse_additive(restrictions)?))
			};

			return Ok(Expr::Range(RangeExpr {
				start: None,
				end,
				inclusive,
				span: span.merge(&self.last_span),
			}));
		}

		let start: Expr = self.parse_additive(restrictions)?;

		match self.peek_kind()? {
			TokenKind::DotDot => {
				self.next()?;
				let end: Option<Box<Expr>> = if self.is_range_end() {
					None
				} else {
					Some(Box::new(self.parse_additive(restrictions)?))
				};
				return Ok(Expr::Range(RangeExpr {
					start: Some(Box::new(start)),
					end,
					inclusive: false,
					span: span.merge(&self.last_span),
				}));
			}
			TokenKind::DotDotEquals => {
				self.next()?;
				let end: Box<Expr> = Box::new(self.parse_additive(restrictions)?);
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
			Ok(TokenKind::Comma
				| TokenKind::RightParen
				| TokenKind::RightBracket
				| TokenKind::RightBrace
				| TokenKind::Semicolon
				| TokenKind::FatArrow)
		);
	}

	fn parse_additive(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut lhs: Expr = self.parse_multiplicative(restrictions)?;

		loop {
			let op: BinaryOp = match self.peek_kind()? {
				TokenKind::Plus => BinaryOp::Add,
				TokenKind::Minus => BinaryOp::Sub,
				_ => break,
			};

			self.next()?;
			let rhs: Expr = self.parse_multiplicative(restrictions)?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_multiplicative(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut lhs: Expr = self.parse_cast(restrictions)?;

		loop {
			let op: BinaryOp = match self.peek_kind()? {
				TokenKind::Star => BinaryOp::Mul,
				TokenKind::Slash => BinaryOp::Div,
				TokenKind::Mod => BinaryOp::Mod,
				_ => break,
			};

			self.next()?;
			let rhs: Expr = self.parse_cast(restrictions)?;
			lhs = Expr::Binary {
				op,
				lhs: Box::new(lhs),
				rhs: Box::new(rhs),
				span: span.merge(&self.last_span),
			};
		}

		return Ok(lhs);
	}

	fn parse_cast(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		if self.at(&TokenKind::LeftParen)? {
			let checkpoint: (Peekable<T>, Span, Option<Token>) = self.make_checkpoint();
			self.next()?; // (

			if let Ok(ty) = self.parse_type()
				&& self.consume(&TokenKind::RightParen)?
			{
				let next_tok: &TokenKind = self.peek_kind()?;

				if matches!(next_tok, TokenKind::DotDot | TokenKind::DotDotEquals) {
					self.load_checkpoint(checkpoint);
					return self.parse_unary(restrictions);
				}

				let expr: Expr = self.parse_cast(restrictions)?;
				return Ok(Expr::Cast {
					ty: Box::new(ty),
					expr: Box::new(expr),
					span: span.merge(&self.last_span),
				});
			}

			self.load_checkpoint(checkpoint);
		}

		return self.parse_unary(restrictions);
	}

	fn parse_unary(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let op: UnaryOp = match self.peek_kind()? {
			TokenKind::Bang => {
				self.next()?;
				UnaryOp::Not
			}
			TokenKind::Minus => {
				self.next()?;
				UnaryOp::Neg
			}
			TokenKind::Star => {
				self.next()?;
				UnaryOp::Deref
			}
			TokenKind::Ampersand => {
				self.next()?;
				let mutable: bool = self.consume(&TokenKind::Mut)?;
				UnaryOp::Addr { mutable }
			}
			_ => return self.parse_postfix(restrictions),
		};

		let expr: Expr = self.parse_unary(restrictions)?;
		return Ok(Expr::Unary {
			op,
			expr: Box::new(expr),
			span: span.merge(&self.last_span),
		});
	}

	fn parse_postfix(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut expr: Expr = self.parse_primary(restrictions)?;

		loop {
			match self.peek_kind()? {
				TokenKind::Dot => {
					self.next()?;
					let field_name: Path = self.get_path_allow_internals()?;
					expr = Expr::Field {
						base: Box::new(expr),
						name: field_name,
						span: span.merge(&self.last_span),
					};
				}
				TokenKind::LeftBracket => {
					self.next()?;
					let index: Expr = self.parse_expr()?; // Always allow struct init inside []
					self.expect(&TokenKind::RightBracket)?;
					expr = Expr::Index {
						base: Box::new(expr),
						index: Box::new(index),
						span: span.merge(&self.last_span),
					};
				}
				TokenKind::Bang | TokenKind::QuestionMark => {
					let call_type: CallType = if self.consume(&TokenKind::Bang)? {
						CallType::UserHeap
					} else if self.consume(&TokenKind::QuestionMark)? {
						CallType::UserMaybeHeap
					} else {
						unreachable!()
					};

					let named_generics: Vec<(Ident, Type)> = if self.at(&TokenKind::LessThan)? {
						self.parse_named_generics()?
					} else {
						Vec::new()
					};

					self.expect(&TokenKind::LeftParen)?;
					let args: Vec<Expr> = self.parse_argument_list()?;
					self.expect(&TokenKind::RightParen)?;

					expr = Expr::Call {
						callee: Box::new(expr),
						call_type,
						named_generics,
						args,
						span: span.merge(&self.last_span),
					};
				}
				TokenKind::LeftParen => {
					self.next()?;
					let args: Vec<Expr> = self.parse_argument_list()?;
					self.expect(&TokenKind::RightParen)?;
					expr = Expr::Call {
						callee: Box::new(expr),
						call_type: CallType::Regular,
						named_generics: Vec::new(),
						args,
						span: span.merge(&self.last_span),
					};
				}
				_ => break,
			}
		}

		return Ok(expr);
	}

	fn parse_primary(&mut self, restrictions: Restrictions) -> Result<Expr, ParseError>
	{
		let tok: Token = self.peek()?.clone();
		let span: Span = tok.span();

		match &tok.kind {
			TokenKind::IntLiteral { value, base, ty } => {
				self.next()?;
				return Ok(Expr::Literal {
					value: Literal::Int {
						value: value.clone(),
						base: *base,
						ty: *ty,
						span,
					},
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::FloatLiteral { value, bits } => {
				self.next()?;
				return Ok(Expr::Literal {
					value: Literal::Float {
						value: value.clone(),
						bits: *bits,
						span,
					},
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::StringLiteral(s) => {
				self.next()?;
				return Ok(Expr::Literal {
					value: Literal::String { value: s.clone(), span },
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::CharLiteral(c) => {
				self.next()?;
				return Ok(Expr::Literal {
					value: Literal::Char { value: *c, span },
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::True => {
				self.next()?;
				return Ok(Expr::Literal {
					value: Literal::Bool { value: true, span },
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::False => {
				self.next()?;
				return Ok(Expr::Literal {
					value: Literal::Bool { value: false, span },
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::Default => {
				self.next()?; // default

				let call_type = if self.consume(&TokenKind::Bang)? {
					CallType::UserHeap
				} else if self.consume(&TokenKind::QuestionMark)? {
					CallType::UserMaybeHeap
				} else {
					CallType::Regular
				};

				self.expect(&TokenKind::LeftParen)?;
				self.expect(&TokenKind::RightParen)?;

				return Ok(Expr::Default {
					heap_call: call_type,
					span: span.merge(&self.last_span),
				});
			}
			TokenKind::SelfKw => {
				self.next()?;
				return Ok(Expr::Identifier {
					path: Path::simple(vec!["self".to_string()], tok.span()),
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::Identifier(_) | TokenKind::DoubleColon => {
				let path: Path = self.get_path_allow_internals()?;

				let call_type = if self.consume(&TokenKind::Bang)? {
					Some(CallType::UserHeap)
				} else if self.consume(&TokenKind::QuestionMark)? {
					Some(CallType::UserMaybeHeap)
				} else {
					None
				};

				if let Some(ct) = call_type {
					let named_generics: Vec<(Ident, Type)> = if self.at(&TokenKind::LessThan)? {
						self.parse_named_generics()?
					} else {
						Vec::new()
					};

					self.expect(&TokenKind::LeftParen)?;
					let args: Vec<Expr> = self.parse_argument_list()?;
					self.expect(&TokenKind::RightParen)?;

					return Ok(Expr::Call {
						callee: Box::new(Expr::Identifier {
							path,
							span: span.merge(&self.last_span),
						}),
						call_type: ct,
						named_generics,
						args,
						span: span.merge(&self.last_span),
					});
				}

				if !restrictions.no_struct_literal && self.at(&TokenKind::LeftBrace)? {
					let checkpoint = self.lexer.clone();
					let checkpoint_span = self.last_span;
					let checkpoint_buffered = self.buffered_token.clone();

					self.next()?; // {

					let is_struct: bool = self.at(&TokenKind::RightBrace)?
						|| self.at(&TokenKind::DotDot)?
						|| (matches!(self.peek_kind()?, TokenKind::Identifier(_))
							&& self.lookahead_for_struct_field()?);

					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					self.buffered_token = checkpoint_buffered;

					if is_struct {
						self.next()?; // {

						let fields: Vec<(String, Expr)> = self.parse_struct_fields()?;

						let (base, has_rest) = if self.consume(&TokenKind::DotDot)? {
							if self.at(&TokenKind::RightBrace)? {
								(None, true)
							} else {
								(Some(Box::new(self.parse_expr()?)), false)
							}
						} else {
							(None, false)
						};

						self.expect(&TokenKind::RightBrace)?;
						return Ok(Expr::StructInit {
							path,
							fields,
							base,
							has_rest,
							span: span.merge(&self.last_span),
						});
					}
					return Ok(Expr::Identifier {
						path,
						span: span.merge(&self.last_span),
					});
				}
				return Ok(Expr::Identifier {
					path,
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::LeftParen => {
				self.next()?;

				if self.consume(&TokenKind::RightParen)? {
					return Ok(Expr::Tuple {
						elements: Vec::new(),
						span: span.merge(&self.last_span),
					});
				}

				let first: Expr = self.parse_expr()?; // Always allow struct init inside ()

				if self.consume(&TokenKind::RightParen)? {
					return Ok(first);
				}

				if self.consume(&TokenKind::Comma)? {
					let mut elements = vec![first];

					if self.consume(&TokenKind::RightParen)? {
						return Ok(Expr::Tuple {
							elements,
							span: span.merge(&self.last_span),
						});
					}

					loop {
						elements.push(self.parse_expr()?);
						if !self.consume(&TokenKind::Comma)? {
							break;
						}
						if self.at(&TokenKind::RightParen)? {
							break;
						}
					}

					self.expect(&TokenKind::RightParen)?;
					return Ok(Expr::Tuple {
						elements,
						span: span.merge(&self.last_span),
					});
				}

				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::OneOf(vec![TokenKind::Comma, TokenKind::RightParen]),
					tok.kind,
				));
			}

			TokenKind::LeftBracket => {
				self.next()?;

				if self.consume(&TokenKind::RightBracket)? {
					return Ok(Expr::Array(ArrayLiteral::List {
						elements: Vec::new(),
						span: span.merge(&self.last_span),
					}));
				}

				let first: Expr = self.parse_expr()?;

				if self.consume(&TokenKind::Semicolon)? {
					let count: Expr = self.parse_expr()?;
					self.expect(&TokenKind::RightBracket)?;
					return Ok(Expr::Array(ArrayLiteral::Repeat {
						value: Box::new(first),
						count: Box::new(count),
						span: span.merge(&self.last_span),
					}));
				}

				let mut elements: Vec<Expr> = vec![first];

				while self.consume(&TokenKind::Comma)? {
					if self.at(&TokenKind::RightBracket)? {
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
				self.next()?; // unsafe
				let block: Block = self.parse_block()?;
				return Ok(Expr::UnsafeBlock(Box::new(block)));
			}

			TokenKind::Switch => {
				self.next()?; // switch
				let expr: Expr = self.parse_expr_no_struct()?; // Use no_struct for switch expression
				self.expect(&TokenKind::LeftBrace)?;

				let mut arms: Vec<SwitchArm> = Vec::new();
				while !self.at(&TokenKind::RightBrace)? {
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
				return Ok(Self::stmt_loop_to_expr(loop_stmt));
			}

			TokenKind::Label(label) => {
				self.next()?; // label
				self.expect(&TokenKind::Colon)?; // :

				if self.at(&TokenKind::Loop)? {
					self.next()?; // loop
					let body = self.parse_block()?;
					return Ok(Expr::Loop {
						label: Some(label.to_owned()),
						body: Box::new(body),
						span: span.merge(&self.last_span),
					});
				}
				let tok: Token = self.next()?;
				return Err(ParseError::unexpected_item(
					tok.span,
					"Expected a loop, only a loop can have a label and return a value",
					tok.kind,
				));
			}

			_ => {
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::Expression,
					tok.kind,
				));
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
							return Err(ParseError::generic(
								b.span(),
								"expected expression, block, or if statement in else branch",
							));
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
							return Err(ParseError::generic(
								b.span(),
								"expected expression, block, or if statement in else branch",
							));
						}
					},
					None => None,
				},
				span,
			}),
			_ => unreachable!("Expected if or if var statement"),
		};
	}

	fn stmt_loop_to_expr(stmt: Stmt) -> Expr
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

	fn lookahead_for_struct_field(&mut self) -> Result<bool, ParseError>
	{
		if let TokenKind::Identifier(_) = self.peek_kind()? {
			let checkpoint: (Peekable<T>, Span, Option<Token>) = self.make_checkpoint();
			self.next()?; // identifier

			let is_struct_field: bool = self.at(&TokenKind::Arrow)?
				|| self.at(&TokenKind::Comma)?
				|| self.at(&TokenKind::RightBrace)?
				|| self.at(&TokenKind::DotDot)?;

			self.load_checkpoint(checkpoint);
			return Ok(is_struct_field);
		}
		return Ok(false);
	}

	fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError>
	{
		if self.at(&TokenKind::RightParen)? {
			return Ok(Vec::new());
		}

		let mut args: Vec<Expr> = vec![self.parse_expr()?];

		while self.consume(&TokenKind::Comma)? {
			if self.at(&TokenKind::RightParen)? {
				break;
			}
			args.push(self.parse_expr()?);
		}

		return Ok(args);
	}

	fn parse_struct_fields(&mut self) -> Result<Vec<(Ident, Expr)>, ParseError>
	{
		if self.at(&TokenKind::RightBrace)? || self.at(&TokenKind::DotDot)? {
			return Ok(Vec::new());
		}

		let mut fields: Vec<(String, Expr)> = Vec::new();

		loop {
			if self.at(&TokenKind::DotDot)? {
				break;
			}

			let name_tok: Token = self.next()?;
			let name: Ident = if let TokenKind::Identifier(str) = name_tok.kind {
				str
			} else {
				return Err(ParseError::unexpected_token(
					name_tok.span,
					Expected::Identifier,
					name_tok.kind,
				));
			};

			let value: Expr = if self.consume(&TokenKind::Arrow)? {
				self.parse_expr()?
			} else {
				Expr::Identifier {
					path: Path::simple(vec![name.clone()], name_tok.span),
					span: name_tok.span,
				}
			};

			fields.push((name, value));

			if !self.consume(&TokenKind::Comma)? {
				break;
			}
			if self.at(&TokenKind::RightBrace)? || self.at(&TokenKind::DotDot)? {
				break;
			}
		}

		return Ok(fields);
	}

	fn parse_switch_arm(&mut self) -> Result<SwitchArm, ParseError>
	{
		let span: Span = self.peek()?.span();

		let pattern: Pattern = self.parse_pattern()?;
		self.expect(&TokenKind::FatArrow)?; // =>

		let body: SwitchBody = if self.at(&TokenKind::LeftBrace)? {
			let switch: SwitchBody = SwitchBody::Block(self.parse_block()?);
			self.consume(&TokenKind::Comma)?;
			switch
		} else {
			let is_stmt = matches!(
				self.peek_kind()?,
				TokenKind::Break | TokenKind::Continue | TokenKind::Return
			);

			if is_stmt {
				let stmt = match self.peek_kind()? {
					TokenKind::Break => {
						self.next()?; // break
						let label: Option<String> = if matches!(self.peek_kind()?, TokenKind::Label(_)) {
							let tok: Token = self.next()?;
							if let TokenKind::Label(l) = tok.kind {
								Some(l)
							} else {
								None
							}
						} else {
							None
						};
						let value: Option<Expr> = if self.at(&TokenKind::Comma)? {
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
						self.next()?; // continue
						let label: Option<Ident> = if matches!(self.peek_kind()?, TokenKind::Label(_)) {
							let tok = self.next()?;
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
						self.next()?; // return
						let ret_expr = if self.at(&TokenKind::Comma)? {
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
		let span: Span = self.peek()?.span();
		let mut patterns: Vec<Pattern> = vec![self.parse_pattern_no_or()?];

		while self.consume(&TokenKind::Pipe)? {
			patterns.push(self.parse_pattern_no_or()?);
		}

		if patterns.len() == 1 {
			return Ok(patterns.into_iter().next().expect("len == 1, so should not error"));
		}
		return Ok(Pattern::Or {
			patterns,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_pattern_no_or(&mut self) -> Result<Pattern, ParseError>
	{
		let span: Span = self.peek()?.span();

		let mut modifiers: Vec<Modifier> = self.parse_modifiers()?;
		let mutable: bool = modifiers.pop_if(|m| return *m == Modifier::Mut).is_some();

		let tok: Token = self.peek()?.clone();

		match &tok.kind {
			TokenKind::Underscore => {
				self.next()?;
				let ty: Option<Type> = if self.consume(&TokenKind::Colon)? {
					Some(self.parse_type()?)
				} else {
					None
				};
				return Ok(Pattern::Wildcard { span, ty });
			}

			TokenKind::DotDot | TokenKind::DotDotEquals => {
				if !modifiers.is_empty() {
					return Err(ParseError::invalid_pattern(
						span,
						"modifiers not allowed on range patterns",

					));
				}

				let inclusive: bool = self.at(&TokenKind::DotDotEquals)?;
				self.next()?; // .. | ..=

				let end: Option<Box<Expr>> = if self.is_range_end() {
					None
				} else {
					Some(Box::new(self.parse_expr()?))
				};

				return Ok(Pattern::Range(RangeExpr {
					start: None,
					end,
					inclusive,
					span: span.merge(&self.last_span),
				}));
			}

			TokenKind::Identifier(_) | TokenKind::DoubleColon => {
				let path: Path = self.get_path()?;

				if self.consume(&TokenKind::LeftParen)? {
					if !modifiers.is_empty() {
						return Err(ParseError::invalid_pattern(
							span,
							"modifiers not allowed on variant patterns",
						));
					}

					let mut args: Vec<Pattern> = Vec::new();
					if !self.at(&TokenKind::RightParen)? {
						loop {
							args.push(self.parse_pattern()?);
							if !self.consume(&TokenKind::Comma)? {
								break;
							}
							if self.at(&TokenKind::RightParen)? {
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
				} else if self.consume(&TokenKind::LeftBrace)? {
					if !modifiers.is_empty() {
						return Err(ParseError::invalid_pattern(
							span,
							"modifiers not allowed on variant patterns",
						));
					}

					let mut fields: Vec<(Ident, Pattern)> = Vec::new();
					let mut has_rest: bool = false;

					if !self.at(&TokenKind::RightBrace)? {
						loop {
							if self.consume(&TokenKind::DotDot)? {
								has_rest = true;
								if !self.at(&TokenKind::RightBrace)? {
									return Err(ParseError::invalid_pattern(
										self.peek()?.span(),
										".. must be the last element in a struct pattern",
									));
								}
								break;
							}

							let field_modifiers: Vec<Modifier> = self.parse_modifiers()?;
							let field_tok: Token = self.next()?;
							let field_name: Ident = if let TokenKind::Identifier(name) = field_tok.kind {
								name
							} else {
								return Err(ParseError::unexpected_token(
									field_tok.span,
									Expected::Identifier,
									field_tok.kind,
								));
							};

							let pattern: Pattern = if self.consume(&TokenKind::Arrow)? {
								self.parse_pattern()?
							} else if self.consume(&TokenKind::Colon)? {
								let ty: Type = self.parse_type()?;

								let call_constructor = if self.consume(&TokenKind::Bang)? {
									self.expect(&TokenKind::LeftParen)?;
									self.expect(&TokenKind::RightParen)?;
									Some(CallType::UserHeap)
								} else if self.consume(&TokenKind::QuestionMark)? {
									self.expect(&TokenKind::LeftParen)?;
									self.expect(&TokenKind::RightParen)?;
									Some(CallType::UserMaybeHeap)
								} else if self.consume(&TokenKind::LeftParen)? {
									self.expect(&TokenKind::RightParen)?;
									Some(CallType::Regular)
								} else {
									None
								};

								Pattern::TypedIdentifier {
									path: Path::simple(vec![field_name.clone()], field_tok.span),
									ty,
									mutable: true, // this one just is just ignored, because the mutable on a structfield is not usefull, but maybe will later be used for pub/priv
									modifiers: field_modifiers,
									call_constructor,
									span: field_tok.span.merge(&self.last_span),
								}
							} else {
								if !modifiers.is_empty() {
									return Err(ParseError::invalid_pattern(
										span,
										"modifiers require type annotation (use `: Type` after identifier)",
									));
								}
								Pattern::Variant {
									path: Path::simple(vec![field_name.clone()], field_tok.span),
									args: Vec::new(),
									span: field_tok.span,
								}
							};

							fields.push((field_name, pattern));

							if !self.consume(&TokenKind::Comma)? {
								break;
							}
							if self.at(&TokenKind::RightBrace)? {
								break;
							}
						}
					}

					self.expect(&TokenKind::RightBrace)?;
					return Ok(Pattern::Struct {
						path,
						fields,
						has_rest,
						span: span.merge(&self.last_span),
					});
				} else if self.consume(&TokenKind::Colon)? {
					if path.len() != 1 {
						return Err(ParseError::invalid_pattern(
							tok.span,
							"binding patterns must be simple identifiers, not paths",
						));
					}

					let ty: Type = self.parse_type()?;

					let call_constructor: Option<CallType> = if self.consume(&TokenKind::Bang)? {
						self.expect(&TokenKind::LeftParen)?;
						self.expect(&TokenKind::RightParen)?;
						Some(CallType::UserHeap)
					} else if self.consume(&TokenKind::QuestionMark)? {
						self.expect(&TokenKind::LeftParen)?;
						self.expect(&TokenKind::RightParen)?;
						Some(CallType::UserMaybeHeap)
					} else if self.consume(&TokenKind::LeftParen)? {
						self.expect(&TokenKind::RightParen)?;
						Some(CallType::Regular)
					} else {
						None
					};

					return Ok(Pattern::TypedIdentifier {
						path,
						ty,
						modifiers,
						call_constructor,
						span: span.merge(&self.last_span),
						mutable,
					});
				}
				return Ok(Pattern::Variant {
					path,
					args: Vec::new(),
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::LeftParen => {
				if !modifiers.is_empty() {
					return Err(ParseError::invalid_pattern(
						span,
						"modifiers not allowed on tuple patterns",
					));
				}
				self.next()?; // (

				if self.consume(&TokenKind::RightParen)? {
					return Ok(Pattern::Tuple {
						patterns: Vec::new(),
						span: span.merge(&self.last_span),
					});
				}

				let mut patterns: Vec<Pattern> = vec![self.parse_pattern()?];

				if self.consume(&TokenKind::Comma)? {
					if !self.at(&TokenKind::RightParen)? {
						loop {
							patterns.push(self.parse_pattern()?);
							if !self.consume(&TokenKind::Comma)? {
								break;
							}
							if self.at(&TokenKind::RightParen)? {
								break;
							}
						}
					}
					self.expect(&TokenKind::RightParen)?;
					return Ok(Pattern::Tuple {
						patterns,
						span: span.merge(&self.last_span),
					});
				}
				self.expect(&TokenKind::RightParen)?;
				return Ok(patterns.into_iter().next().expect(
					"this should already be cought in the code before, because of `vec![self.parse_pattern()?]",
				));
			}

			TokenKind::IntLiteral { value, base, ty } => {
				self.next()?;

				if self.at(&TokenKind::DotDot)? || self.at(&TokenKind::DotDotEquals)? {
					let inclusive = self.at(&TokenKind::DotDotEquals)?;
					self.next()?; // .. | ..=

					let end = if self.is_range_end() {
						None
					} else {
						Some(Box::new(self.parse_expr()?))
					};

					return Ok(Pattern::Range(RangeExpr {
						start: Some(Box::new(Expr::Literal {
							value: Literal::Int {
								value: value.clone(),
								base: *base,
								ty: *ty,
								span: tok.span(),
							},
							span: tok.span(),
						})),
						end,
						inclusive,
						span: span.merge(&self.last_span),
					}));
				}
				return Ok(Pattern::Literal {
					value: Literal::Int {
						value: value.clone(),
						base: *base,
						ty: *ty,
						span: tok.span(),
					},
					span: tok.span(),
				});
			}

			TokenKind::True => {
				if !modifiers.is_empty() {
					return Err(ParseError::invalid_pattern(
						span,
						"modifiers not allowed on literal patterns",
					));
				}
				self.next()?;
				return Ok(Pattern::Literal {
					value: Literal::Bool {
						value: true,
						span: span.merge(&self.last_span),
					},
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::False => {
				if !modifiers.is_empty() {
					return Err(ParseError::invalid_pattern(
						span,
						"modifiers not allowed on literal patterns",
					));
				}
				self.next()?;
				return Ok(Pattern::Literal {
					value: Literal::Bool {
						value: false,
						span: span.merge(&self.last_span),
					},
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::StringLiteral(s) => {
				if !modifiers.is_empty() {
					return Err(ParseError::invalid_pattern(
						span,
						"modifiers not allowed on literal patterns",
					));
				}
				self.next()?;
				return Ok(Pattern::Literal {
					value: Literal::String {
						value: s.clone(),
						span: span.merge(&self.last_span),
					},
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::CharLiteral(c) => {
				if !modifiers.is_empty() {
					return Err(ParseError::invalid_pattern(
						span,
						"modifiers not allowed on literal patterns",
					));
				}
				self.next()?;
				return Ok(Pattern::Literal {
					value: Literal::Char {
						value: *c,
						span: span.merge(&self.last_span),
					},
					span: span.merge(&self.last_span),
				});
			}

			_ => {
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::Pattern,
					tok.kind,
				));
			}
		}
	}

	fn parse_block(&mut self) -> Result<Block, ParseError>
	{
		self.expect(&TokenKind::LeftBrace)?;

		let ret: Block = self.parse_block_content()?;

		self.expect(&TokenKind::RightBrace)?;
		return Ok(ret);
	}

	fn parse_block_content(&mut self) -> Result<Block, ParseError>
	{
		let span: Span = self.peek()?.span();
		let mut stmts: Vec<Stmt> = Vec::new();
		let mut tail_expr: Option<Box<Expr>> = None;

		while !self.at(&TokenKind::RightBrace)? {
			let saved_label: Option<String> = if matches!(self.peek_kind()?, TokenKind::Label(_)) {
				let tok = self.next()?;
				if let TokenKind::Label(l) = tok.kind {
					self.expect(&TokenKind::Colon)?;
					Some(l)
				} else {
					None
				}
			} else {
				None
			};

			let kind: TokenKind = self.peek_kind()?.clone();

			match kind {
				TokenKind::Semicolon => {
					self.next()?; // ;
				}
				TokenKind::Var | TokenKind::Const => {
					let var_decl: VariableDecl = self.parse_var_decl()?;
					self.expect(&TokenKind::Semicolon)?;
					stmts.push(Stmt::VariableDecl(var_decl));
				}

				TokenKind::Return => {
					self.next()?;
					let ret_expr: Option<Expr> = if self.at(&TokenKind::Semicolon)? {
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
					self.next()?; // break

					let label: Option<String> = if matches!(self.peek_kind()?, TokenKind::Label(_)) {
						let tok: Token = self.next()?;
						if let TokenKind::Label(l) = tok.kind {
							Some(l)
						} else {
							None
						}
					} else {
						None
					};

					let value = if self.at(&TokenKind::Semicolon)? {
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
					self.next()?; // continue

					let label: Option<String> = if matches!(self.peek_kind()?, TokenKind::Label(_)) {
						let tok: Token = self.next()?;
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

					if self.at(&TokenKind::RightBrace)? {
						tail_expr = Some(Box::new(Self::stmt_loop_to_expr(loop_stmt)));
						break;
					}
					self.consume(&TokenKind::Semicolon)?;
					stmts.push(loop_stmt);
				}

				TokenKind::If => {
					let checkpoint: (Peekable<T>, Span, Option<Token>) = self.make_checkpoint();

					self.next()?; // if

					if self.consume(&TokenKind::Var)? {
						let pattern: Pattern = self.parse_pattern()?;
						self.expect(&TokenKind::Equals)?;
						let expr: Expr = self.parse_expr_no_struct()?;
						let then_block: Block = self.parse_block()?;

						let else_branch: Option<Box<Stmt>> = if self.consume(&TokenKind::Else)? {
							if self.at(&TokenKind::If)? {
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

						if self.consume(&TokenKind::Semicolon)? {
							stmts.push(if_var_stmt);
						} else if self.at(&TokenKind::RightBrace)? {
							tail_expr = Some(Box::new(self.stmt_if_to_expr_wrapper(if_var_stmt)?));
							break;
						} else {
							stmts.push(if_var_stmt);
						}
					} else {
						self.load_checkpoint(checkpoint);

						let if_stmt: Stmt = self.parse_if()?;

						if self.consume(&TokenKind::Semicolon)? {
							stmts.push(if_stmt);
						} else if self.at(&TokenKind::RightBrace)? {
							tail_expr = Some(Box::new(self.stmt_if_to_expr_wrapper(if_stmt)?));
							break;
						} else {
							stmts.push(if_stmt);
						}
					}
				}

				TokenKind::Delete => {
					let expr = self.parse_delete()?;
					stmts.push(Stmt::Delete {
						expr,
						span: span.merge(&self.last_span),
					});
					self.expect(&TokenKind::Semicolon)?;
				}

				TokenKind::Unsafe => {
					self.next()?;
					let block: Block = self.parse_block()?;

					if self.consume(&TokenKind::Semicolon)? {
						stmts.push(Stmt::Unsafe(block));
					} else if self.at(&TokenKind::RightBrace)? {
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
						if self.consume(&TokenKind::Semicolon)? {
							stmts.push(Stmt::Block(*block));
						} else if self.at(&TokenKind::RightBrace)? {
							tail_expr = Some(Box::new(Expr::Block(block)));
							break;
						} else {
							stmts.push(Stmt::Block(*block));
						}
					} else {
						let needs_semi: bool = Self::expr_needs_semicolon(&expr);

						if needs_semi {
							if self.consume(&TokenKind::Semicolon)? {
								stmts.push(Stmt::Expr(expr));
							} else if self.at(&TokenKind::RightBrace)? {
								tail_expr = Some(Box::new(expr));
								break;
							} else {
								let tok: Token = self.next()?;
								return Err(ParseError::unexpected_token(
									tok.span,
									Expected::OneOf(vec![TokenKind::Semicolon, TokenKind::RightBrace]),
									tok.kind,
								));
							}
						} else if self.at(&TokenKind::RightBrace)? {
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

	const fn expr_needs_semicolon(expr: &Expr) -> bool
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
			Ok(TokenKind::Equals
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
				| TokenKind::RShiftEquals)
		);
	}

	fn parse_assign_op(&mut self) -> Result<AssignOp, ParseError>
	{
		let op: AssignOp = match self.peek_kind()? {
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
				let tok: Token = self.next()?;
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::Description("assignment operator".to_string()),
					tok.kind,
				));
			}
		};
		self.next()?;
		return Ok(op);
	}

	fn parse_if(&mut self) -> Result<Stmt, ParseError>
	{
		let span: Span = self.peek()?.span();
		self.expect(&TokenKind::If)?;

		return if self.consume(&TokenKind::Var)? {
			let pattern: Pattern = self.parse_pattern()?;
			self.expect(&TokenKind::Equals)?;
			let expr: Expr = self.parse_expr_no_struct()?;
			let then_block: Block = self.parse_block()?;

			let else_branch: Option<Box<Stmt>> = if self.consume(&TokenKind::Else)? {
				if self.at(&TokenKind::If)? {
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

			let else_branch: Option<Box<Stmt>> = if self.consume(&TokenKind::Else)? {
				if self.at(&TokenKind::If)? {
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
		let span: Span = self.peek()?.span();
		self.expect(&TokenKind::While)?;

		if self.consume(&TokenKind::Var)? {
			let pattern: Pattern = self.parse_pattern()?;
			self.expect(&TokenKind::Equals)?;
			let expr: Expr = self.parse_expr_no_struct()?;
			let body: Block = self.parse_block()?;

			return Ok(Stmt::WhileVarLoop {
				label: None,
				pattern,
				expr,
				body,
				span: span.merge(&self.last_span),
			});
		}
		let cond: Expr = self.parse_expr_no_struct()?;
		let body: Block = self.parse_block()?;

		return Ok(Stmt::While {
			label: None,
			cond,
			body,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_for(&mut self) -> Result<Stmt, ParseError>
	{
		let span: Span = self.peek()?.span();
		self.expect(&TokenKind::For)?;
		let pattern: Pattern = self.parse_pattern()?;
		self.expect(&TokenKind::In)?;
		let iter = self.parse_expr_no_struct()?;
		let body = self.parse_block()?;

		return Ok(Stmt::For {
			label: None,
			pattern,
			iter,
			body,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_loop(&mut self) -> Result<Stmt, ParseError>
	{
		let span: Span = self.peek()?.span();
		self.expect(&TokenKind::Loop)?;
		return Ok(Stmt::Loop {
			label: None,
			body: self.parse_block()?,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_function_decl(&mut self) -> Result<FunctionDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let mut span: Span = self.peek()?.span();
		let signature: FunctionSignature = self.parse_function_signature()?;
		let body: Option<Block> = if self.consume(&TokenKind::Semicolon)? {
			None
		} else {
			Some(self.parse_block()?)
		};
		span = span.merge(&self.last_span);
		return Ok(FunctionDecl {
			signature,
			body,
			docs,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_function_signature(&mut self) -> Result<FunctionSignature, ParseError>
	{
		let span: Span = self.peek()?.span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;

		self.expect(&TokenKind::FuncDef)?;

		let call_type = if self.consume(&TokenKind::Bang)? {
			CallType::UserHeap
		} else if self.consume(&TokenKind::QuestionMark)? {
			CallType::UserMaybeHeap
		} else {
			CallType::Regular
		};

		let heap_generics: Vec<GenericParam> = if call_type.is_heap_call() && self.at(&TokenKind::LessThan)? {
			let generic_pars: Vec<GenericParam> = self.get_generics()?;
			generic_pars
				.iter()
				.all(|v| return ALLOWED_HEAP_GENERICS.iter().any(|&n| return v.name == n));
			generic_pars
		} else {
			Vec::new()
		};

		let name: Path = if matches!(self.peek_kind()?, TokenKind::Identifier(_) | TokenKind::DoubleColon) {
			self.get_path()?
		} else if self.at(&TokenKind::Delete)? {
			let tok = self.next()?;
			Path::simple(vec!["delete".to_string()], tok.span())
		} else {
			let tok: Token = self.next()?;
			return Err(ParseError::unexpected_token(
				tok.span,
				Expected::Identifier,
				tok.kind,
			));
		};

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan)? {
			self.get_generics()?
		} else {
			Vec::new()
		};
		let params: Vec<Param> = self.parse_function_arguments()?;

		let return_type: Option<Type> = if self.at(&TokenKind::Arrow)? {
			self.next()?; // ->
			Some(self.parse_type()?)
		} else {
			None
		};

		let where_clause: Vec<WhereConstraint> = if self.at(&TokenKind::Where)? {
			self.next()?; // where
			if !matches!(self.peek_kind()?, TokenKind::Identifier(_)) {
				return Err(ParseError::unexpected_token(
					self.peek()?.span(),
					Expected::Identifier,
					self.next()?.kind,
				));
			}
			self.parse_where_clause()?
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
			call_type,
			heap_generics,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_function_arguments(&mut self) -> Result<Vec<Param>, ParseError>
	{
		self.expect(&TokenKind::LeftParen)?;

		let mut params: Vec<Param> = Vec::new();

		if self.at(&TokenKind::RightParen)? {
			self.next()?;
			return Ok(params);
		}

		loop {
			let loop_span: Span = self.peek()?.span();

			let checkpoint: (Peekable<T>, Span, Option<Token>) = self.make_checkpoint();

			let mut modifiers: Vec<Modifier> = self.parse_modifiers()?;
			let mutable: bool = modifiers.pop_if(|m| return *m == Modifier::Mut).is_some();

			match self.peek_kind()? {
				TokenKind::Ampersand => {
					self.next()?; // &

					let mutable: bool = self.consume(&TokenKind::Mut)?;

					let self_span: Span = self.expect(&TokenKind::SelfKw)?.span();

					let base: TypeCore = TypeCore::Base {
						path: Path::simple(vec!["Self".to_string()], self_span),
						generics: Vec::new(),
					};
					let self_type: Type = Type {
						core: Box::new(TypeCore::Reference {
							mutable,
							inner: Box::new(base),
						}),
						span: loop_span.merge(&self.last_span),
					};

					let self_pattern = Pattern::TypedIdentifier {
						path: Path::simple(vec!["self".to_string()], self_span),
						modifiers,
						ty: self_type.clone(),
						call_constructor: None,
						span: self_span,
						mutable,
					};

					params.push(Param {
						ty: self_type,
						variadic: false,
						pattern: self_pattern,
						span: loop_span.merge(&self.last_span),
					});
				}
				TokenKind::SelfKw => {
					let self_span: Span = self.next()?.span(); // self

					let self_type: Type = Type {
						core: Box::new(TypeCore::Base {
							path: Path::simple(vec!["Self".to_string()], self_span),
							generics: Vec::new(),
						}),
						span: loop_span.merge(&self.last_span),
					};

					let self_pattern: Pattern = Pattern::TypedIdentifier {
						path: Path::simple(vec!["self".to_string()], self_span),
						modifiers,
						ty: self_type.clone(),
						call_constructor: None,
						span: self_span,
						mutable,
					};

					params.push(Param {
						ty: self_type,
						variadic: false,
						pattern: self_pattern,
						span: loop_span.merge(&self.last_span),
					});
				}
				// TokenKind::Mut => {
				// 	let self_span: Span = self.next()?.span(); // mut
				// 	self.expect(&TokenKind::SelfKw)?;
				//
				// 	let self_type = Type {
				// 		core: Box::new(TypeCore::Mutable {
				// 			inner: Box::new(TypeCore::Base {
				// 				path: Path::simple(vec!["Self".to_string()], self_span),
				// 				generics: Vec::new(),
				// 			}),
				// 		}),
				// 		span: loop_span.merge(&self.last_span),
				// 	};
				//
				// 	let self_pattern = Pattern::TypedIdentifier {
				// 		path: Path::simple(vec!["self".to_string()], self_span),
				// 		modifiers,
				// 		ty: self_type.clone(),
				// 		call_constructor: None,
				// 		span: self_span,
				// 	};
				//
				// 	params.push(Param {
				// 		ty: self_type,
				// 		variadic: false,
				// 		pattern: self_pattern,
				// 		span: loop_span.merge(&self.last_span),
				// 	});
				// }
				TokenKind::Ellipsis => {
					let span: Span = self.next()?.span(); // ...

					if !self.at(&TokenKind::RightParen)? {
						return Err(ParseError::generic(
							loop_span,
							"variadic parameter (...) must be the last parameter",
						));
					}

					params.push(Param {
						ty: Type {
							core: Box::new(TypeCore::Base {
								path: Path::simple(vec!["_".to_string()], span),
								generics: vec![],
							}),
							span,
						},
						pattern: Pattern::Wildcard {
							span: loop_span,
							ty: None,
						},
						variadic: true,
						span: loop_span.merge(&self.last_span),
					});

					break;
				}

				_ => {
					self.load_checkpoint(checkpoint);
					let pattern: Pattern = self.parse_pattern()?;

					let ty: Type = if let Some(extracted_ty) = extract_type_from_pattern(&pattern) {
						extracted_ty
					} else {
						match &pattern {
							Pattern::Variant { args, .. } if args.is_empty() => {
								return Err(ParseError::invalid_pattern(
									pattern.span(),
									"tuple patterns in parameters must have type annotations for each element (e.g., (a: i64, b: i64))",
								));
							}
							Pattern::Tuple { .. } => {
								return Err(ParseError::invalid_pattern(
									pattern.span(),
									"tuple patterns in parameters must have type annotations for each element (e.g., (a: i64, b: i64))",
								));
							}
							_ => {
								return Err(ParseError::invalid_pattern(
									pattern.span(),
									"parameter patterns must either be simple identifiers or tuples with type annotations",
								));
							}
						}
					};

					params.push(Param {
						ty,
						variadic: false,
						pattern,
						span: loop_span.merge(&self.last_span),
					});
				}
			}

			if !self.consume(&TokenKind::Comma)? {
				break;
			}

			if self.at(&TokenKind::RightParen)? {
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
			let tok: &Token = self.peek()?;
			match &tok.kind {
				TokenKind::Directive(_) => {
					let tok = self.next()?;
					match tok.kind {
						TokenKind::Directive(d @ (lexer::Directive::Use | lexer::Directive::Import)) => {
							let collected: Vec<Modifier> = std::mem::take(&mut ret);
							let dir: Directive = self.parse_directive_kind(d, collected)?;
							ret.push(Modifier::Directive(dir));
							break;
						}
						TokenKind::Directive(d) => {
							let dir: Directive = self.parse_directive_kind(d, Vec::new())?;
							ret.push(Modifier::Directive(dir));
						}
						_ => unreachable!(),
					}
				}
				TokenKind::Pub => {
					ret.push(Modifier::Pub);
					self.next()?;
				}
				TokenKind::Export => {
					ret.push(Modifier::Export);
					self.next()?;
				}
				TokenKind::Unsafe => {
					ret.push(Modifier::Unsafe);
					self.next()?;
				}
				TokenKind::Inline => {
					ret.push(Modifier::Inline);
					self.next()?;
				}
				TokenKind::Volatile => {
					ret.push(Modifier::Volatile);
					self.next()?;
				}
				TokenKind::Mut => {
					ret.push(Modifier::Mut);
					self.next()?;
				}
				TokenKind::Const => {
					ret.push(Modifier::Const);
					self.next()?;
				}
				_ => break,
			}
		}

		if !ret.is_sorted() {
			todo!("return ordering error")
		}
		for r in ret.windows(2) {
			if matches!(r[0], Modifier::Directive(_)) {
				continue;
			}
			if r[0] == r[1] {
				todo!("return error of 2 the same modifiers")
			}
			if r[0] == Modifier::Pub && r[1] == Modifier::Export {
				todo!("return error of 2 differen visibility tokens")
			}
		}

		return Ok(ret);
	}

	fn parse_struct(&mut self) -> Result<StructDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let span: Span = self.peek()?.span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Struct)?;

		let name: Path = if matches!(self.peek_kind()?, TokenKind::Identifier(_) | TokenKind::DoubleColon) {
			self.get_path()?
		} else {
			let tok: Token = self.next()?;
			return Err(ParseError::unexpected_token(
				tok.span,
				Expected::Identifier,
				tok.kind,
			));
		};

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan)? {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let where_clause: Vec<WhereConstraint> = if self.at(&TokenKind::Where)? {
			self.next()?;
			self.parse_where_clause()?
		} else {
			Vec::new()
		};

		self.expect(&TokenKind::LeftBrace)?;

		let mut fields: Vec<StructField> = Vec::new();

		while !self.at(&TokenKind::RightBrace)? {
			if *self.peek_kind()? == TokenKind::RightBrace {
				break;
			}
			let field_start = self.peek()?.span();
			let field_docs: Option<DocsComment> = self.parse_docs()?;

			let modifiers: Vec<Modifier> = self.parse_modifiers()?;

			let field_name: Ident = if let TokenKind::Identifier(str) = self.next()?.kind {
				str
			} else {
				let tok: Token = self.next()?;
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::Identifier,
					tok.kind,

				));
			};

			self.expect(&TokenKind::Colon)?;

			let field_type: Type = self.parse_type()?;

			let default_value: Option<Expr> = if self.consume(&TokenKind::Equals)? {
				Some(self.parse_expr()?)
			} else {
				None
			};

			fields.push(StructField {
				docs: field_docs,
				ty: field_type,
				modifiers,
				name: field_name,
				default_value,
				span: field_start.merge(&self.last_span),
			});

			if *self.peek_kind()? == TokenKind::RightBrace {
				break;
			}
			self.expect(&TokenKind::Comma)?;
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(StructDecl {
			modifiers,
			name,
			generics,
			where_clause,
			fields,
			docs,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_union(&mut self) -> Result<UnionDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let span: Span = self.peek()?.span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Union)?;

		let name: Path = if matches!(self.peek_kind()?, TokenKind::Identifier(_) | TokenKind::DoubleColon) {
			self.get_path()?
		} else {
			let tok: Token = self.next()?;
			return Err(ParseError::unexpected_token(
				tok.span,
				Expected::Identifier,
				tok.kind,
			));
		};

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan)? {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let where_clause: Vec<WhereConstraint> = if self.at(&TokenKind::Where)? {
			self.next()?;
			self.parse_where_clause()?
		} else {
			Vec::new()
		};

		self.expect(&TokenKind::LeftBrace)?;

		let mut fields: Vec<UnionField> = Vec::new();

		while !self.at(&TokenKind::RightBrace)? {
			if *self.peek_kind()? == TokenKind::RightBrace {
				break;
			}
			let field_start = self.peek()?.span();
			let field_docs: Option<DocsComment> = self.parse_docs()?;

			let modifiers: Vec<Modifier> = self.parse_modifiers()?;

			let field_name: Ident = if let TokenKind::Identifier(str) = self.next()?.kind {
				str
			} else {
				let tok: Token = self.next()?;
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::Identifier,
					tok.kind,
				));
			};

			self.expect(&TokenKind::Colon)?;

			let field_type: Type = self.parse_type()?;

			fields.push(UnionField {
				docs: field_docs,
				ty: field_type,
				modifiers,
				name: field_name,
				span: field_start.merge(&self.last_span),
			});

			if *self.peek_kind()? == TokenKind::RightBrace {
				break;
			}
			self.expect(&TokenKind::Comma)?;
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(UnionDecl {
			modifiers,
			name,
			generics,
			where_clause,
			fields,
			docs,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_module(&mut self) -> Result<ModuleDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let span: Span = self.peek()?.span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Module)?;
		let name: Path = self.get_path()?;
		let kind = if self.consume(&TokenKind::LeftBrace)? {
			let body: TopLevelBlock = self.parse_top_level_block()?;
			self.expect(&TokenKind::RightBrace)?;
			ModuleKind::Inline(body)
		} else {
			self.expect(&TokenKind::Semicolon)?;
			ModuleKind::External
		};
		return Ok(ModuleDecl {
			modifiers,
			name,
			kind,
			docs,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_enum(&mut self) -> Result<EnumDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let span: Span = self.peek()?.span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Enum)?;

		let name: Path = if matches!(self.peek_kind()?, TokenKind::Identifier(_) | TokenKind::DoubleColon) {
			self.get_path()?
		} else {
			let tok: Token = self.next()?;
			return Err(ParseError::unexpected_token(
				tok.span,
				Expected::Identifier,
				tok.kind,
			));
		};

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan)? {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let where_clause: Vec<WhereConstraint> = if self.at(&TokenKind::Where)? {
			self.next()?;
			self.parse_where_clause()?
		} else {
			Vec::new()
		};

		self.expect(&TokenKind::LeftBrace)?;

		let mut variants: Vec<EnumVariant> = Vec::new();

		while !self.at(&TokenKind::RightBrace)? {
			if *self.peek_kind()? == TokenKind::RightBrace {
				break;
			}
			let variant_start = self.peek()?.span();
			let variant_docs: Option<DocsComment> = self.parse_docs()?;

			let variant_name: Ident = if let TokenKind::Identifier(str) = self.next()?.kind {
				str
			} else {
				let tok: Token = self.next()?;
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::Identifier,
					tok.kind,
				));
			};

			let variant_value: Option<Expr> = if self.at(&TokenKind::Equals)? {
				self.next()?;
				Some(self.parse_expr()?)
			} else {
				None
			};

			variants.push(EnumVariant {
				docs: variant_docs,
				name: variant_name,
				value: variant_value,
				span: variant_start.merge(&self.last_span),
			});

			if *self.peek_kind()? == TokenKind::RightBrace {
				break;
			}
			self.expect(&TokenKind::Comma)?;
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(EnumDecl {
			modifiers,
			name,
			generics,
			where_clause,
			variants,
			docs,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_variant(&mut self) -> Result<VariantDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let span: Span = self.peek()?.span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Variant)?;

		let name: Path = if matches!(self.peek_kind()?, TokenKind::Identifier(_) | TokenKind::DoubleColon) {
			self.get_path()?
		} else {
			let tok: Token = self.next()?;
			return Err(ParseError::unexpected_token(
				tok.span,
				Expected::Identifier,
				tok.kind,
			));
		};

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan)? {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let where_clause: Vec<WhereConstraint> = if self.at(&TokenKind::Where)? {
			self.next()?;
			self.parse_where_clause()?
		} else {
			Vec::new()
		};

		self.expect(&TokenKind::LeftBrace)?;

		let mut variants: Vec<VariantMember> = Vec::new();

		while !self.at(&TokenKind::RightBrace)? {
			if *self.peek_kind()? == TokenKind::RightBrace {
				break;
			}
			let member_start: Span = self.peek()?.span();
			let member_docs: Option<DocsComment> = self.parse_docs()?;

			let member_name: Ident = if let TokenKind::Identifier(str) = self.next()?.kind {
				str
			} else {
				let tok: Token = self.next()?;
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::Identifier,
					tok.kind,
				));
			};

			let member_type: Option<Type> = if self.at(&TokenKind::LeftParen)? {
				self.next()?;
				let ty: Option<Type> = Some(self.parse_type()?);
				self.expect(&TokenKind::RightParen)?;
				ty
			} else {
				None
			};

			let member_value: Option<Expr> = if self.at(&TokenKind::Equals)? {
				self.next()?;
				Some(self.parse_expr()?)
			} else {
				None
			};

			variants.push(VariantMember {
				docs: member_docs,
				ty: member_type,
				name: member_name,
				value: member_value,
				span: member_start.merge(&self.last_span),
			});

			if *self.peek_kind()? == TokenKind::RightBrace {
				break;
			}
			self.expect(&TokenKind::Comma)?;
		}

		self.expect(&TokenKind::RightBrace)?;

		return Ok(VariantDecl {
			modifiers,
			name,
			generics,
			where_clause,
			variants,
			docs,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_impl(&mut self) -> Result<ImplDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let span: Span = self.peek()?.span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Impl)?;

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan)? {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let first_target: ImplTarget = self.parse_impl_target()?;

		let (trait_path, target): (Option<ImplTarget>, ImplTarget) = if self.consume(&TokenKind::For)? {
			let target: ImplTarget = self.parse_impl_target()?;
			(Some(first_target), target)
		} else {
			(None, first_target)
		};

		let where_clause: Vec<WhereConstraint> = if self.at(&TokenKind::Where)? {
			self.next()?;
			self.parse_where_clause()?
		} else {
			Vec::new()
		};

		self.expect(&TokenKind::LeftBrace)?;

		let mut body: Vec<ImplItem> = Vec::new();

		while !self.at(&TokenKind::RightBrace)? {
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
			docs,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_impl_target(&mut self) -> Result<ImplTarget, ParseError>
	{
		let span: Span = self.peek()?.span();
		let path: Path = self.get_path()?;

		let generics: Vec<Type> = if self.at(&TokenKind::LessThan)? {
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
		if !self.consume(&TokenKind::LessThan)? {
			return Ok(Vec::new());
		}

		let mut generics: Vec<Type> = Vec::new();

		if self.consume_greater_than()? {
			return Ok(generics);
		}

		loop {
			generics.push(self.parse_type()?);

			if self.consume_greater_than()? {
				break;
			}

			if !self.consume(&TokenKind::Comma)? {
				let tok = self.peek()?.clone();
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::OneOf(vec![TokenKind::Comma, TokenKind::GreaterThan]),
					tok.kind,
				));
			}

			if self.consume_greater_than()? {
				break;
			}
		}

		return Ok(generics);
	}

	fn parse_named_generics(&mut self) -> Result<Vec<(Ident, Type)>, ParseError>
	{
		if !self.consume(&TokenKind::LessThan)? {
			return Ok(Vec::new());
		}

		let mut named_generics: Vec<(Ident, Type)> = Vec::new();

		if self.consume_greater_than()? {
			return Ok(named_generics);
		}

		loop {
			let TokenKind::Identifier(name) = self.next()?.kind else {
				let tok: Token = self.peek()?.clone();
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::Identifier,
					tok.kind,
				));
			};

			self.expect(&TokenKind::Colon)?;

			let ty: Type = self.parse_type()?;

			named_generics.push((name, ty));

			if self.consume_greater_than()? {
				break;
			}

			if !self.consume(&TokenKind::Comma)? {
				let tok = self.peek()?.clone();
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::OneOf(vec![TokenKind::Comma, TokenKind::GreaterThan]),
					tok.kind,
				));
			}

			if self.consume_greater_than()? {
				break;
			}
		}

		return Ok(named_generics);
	}

	fn parse_generic_args(&mut self) -> Result<Vec<GenericArg>, ParseError>
	{
		if !self.consume(&TokenKind::LessThan)? {
			return Ok(Vec::new());
		}

		let mut args: Vec<GenericArg> = Vec::new();

		if self.consume_greater_than()? {
			return Ok(args);
		}

		loop {
			let checkpoint: (Peekable<T>, Span, Option<Token>) = self.make_checkpoint();

			if let Ok(TokenKind::Identifier(name)) = self.peek_kind().cloned() {
				self.next()?; // identifier

				if self.consume(&TokenKind::Equals)? {
					let ty: Type = self.parse_type()?;
					let span: Span = self.last_span;
					args.push(GenericArg::Binding { name, ty, span });
				} else {
					self.load_checkpoint(checkpoint);
					let ty: Type = self.parse_type()?;
					args.push(GenericArg::Type(ty));
				}
			} else {
				let ty: Type = self.parse_type()?;
				args.push(GenericArg::Type(ty));
			}

			if self.consume_greater_than()? {
				break;
			}

			if !self.consume(&TokenKind::Comma)? {
				let tok: Token = self.peek()?.clone();
				return Err(ParseError::unexpected_token(
					tok.span,
					Expected::OneOf(vec![TokenKind::Comma, TokenKind::GreaterThan]),
					tok.kind,
				));
			}

			if self.consume_greater_than()? {
				break;
			}
		}

		return Ok(args);
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
				let var_decl: VariableDecl = self.parse_var_decl()?;
				self.expect(&TokenKind::Semicolon)?;
				ImplItem::Const(var_decl)
			}
			DeclKind::AssocType => {
				let assoc_type: AssocTypeDecl = self.parse_assoc_type()?;
				self.expect(&TokenKind::Semicolon)?;
				ImplItem::AssocType(assoc_type)
			}
			_ => {
				let tok = self.peek()?.clone();
				return Err(ParseError::unexpected_item(
					tok.span,
					"impl block",
					tok.kind,
				));
			}
		};

		return Ok(node);
	}

	fn parse_where_clause(&mut self) -> Result<Vec<WhereConstraint>, ParseError>
	{
		let mut constraints: Vec<WhereConstraint> = Vec::new();

		loop {
			let loop_span: Span = self.peek()?.span();

			let ty: Path = self.get_path()?;

			let type_args: Vec<Type> = if self.at(&TokenKind::LessThan)? {
				self.parse_type_generics()?
			} else {
				Vec::new()
			};

			self.expect(&TokenKind::Colon)?;

			let mut bounds: Vec<WhereBound> = Vec::new();

			loop {
				bounds.push(self.parse_where_bound()?);

				if !self.consume(&TokenKind::Plus)? {
					break;
				}
			}

			constraints.push(WhereConstraint {
				ty,
				type_args,
				bounds,
				span: loop_span.merge(&self.last_span),
			});

			if !self.consume(&TokenKind::Comma)? {
				break;
			}

			if self.at(&TokenKind::LeftBrace)? {
				break;
			}
		}

		return Ok(constraints);
	}

	fn parse_type_alias(&mut self) -> Result<TypeAliasDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let span: Span = self.peek()?.span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Type)?;

		let name: Path = self.get_path()?;

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan)? {
			self.get_generics()?
		} else {
			Vec::new()
		};

		self.expect(&TokenKind::Equals)?;
		let ty: Type = self.parse_type()?;

		return Ok(TypeAliasDecl {
			modifiers,
			name,
			generics,
			ty,
			docs,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_assoc_type(&mut self) -> Result<AssocTypeDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let span: Span = self.peek()?.span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Assoc)?;

		let name: Path = self.get_path()?;

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan)? {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let ty = if self.consume(&TokenKind::Equals)? {
			Some(self.parse_type()?)
		} else {
			None
		};

		return Ok(AssocTypeDecl {
			modifiers,
			name,
			generics,
			ty,
			docs,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_trait(&mut self) -> Result<TraitDecl, ParseError>
	{
		let docs: Option<DocsComment> = self.parse_docs()?;
		let span: Span = self.peek()?.span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Trait)?;

		let name: Path = self.get_path()?;

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan)? {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let super_traits: Vec<WhereBound> = if self.consume(&TokenKind::Colon)? {
			self.parse_trait_bounds()?
		} else {
			Vec::new()
		};

		self.expect(&TokenKind::LeftBrace)?;

		let mut items: Vec<TraitItem> = Vec::new();

		while !self.at(&TokenKind::RightBrace)? {
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
			docs,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_trait_bounds(&mut self) -> Result<Vec<WhereBound>, ParseError>
	{
		let mut bounds: Vec<WhereBound> = Vec::new();

		loop {
			let bound: WhereBound = self.parse_where_bound()?;
			bounds.push(bound);

			if !self.consume(&TokenKind::Plus)? {
				break;
			}
		}

		return Ok(bounds);
	}

	fn parse_where_bound(&mut self) -> Result<WhereBound, ParseError>
	{
		let bound: WhereBound = if matches!(self.peek_kind()?, TokenKind::Identifier(s) if *s == "Fn") {
			self.next()?; // Fn
			let mut params: Vec<Type> = Vec::new();

			self.expect(&TokenKind::LeftParen)?;
			loop {
				if self.at(&TokenKind::RightParen)? {
					break;
				}
				params.push(self.parse_type()?);
				if !self.consume(&TokenKind::Comma)? {
					break;
				}
				if self.at(&TokenKind::RightParen)? {
					break;
				}
			}
			self.expect(&TokenKind::RightParen)?;

			let return_type: Option<Type> = if self.at(&TokenKind::Arrow)? {
				self.next()?; // ->
				Some(self.parse_type()?)
			} else {
				None
			};
			WhereBound::Func(FuncBound::Fn {
				args: params,
				ret: return_type,
			})
		} else {
			let bound_path: Path = self.get_path()?;
			let args: Vec<GenericArg> = if self.at(&TokenKind::LessThan)? {
				self.parse_generic_args()?
			} else {
				Vec::new()
			};
			WhereBound::Path { path: bound_path, args }
		};

		return Ok(bound);
	}

	fn parse_trait_item(&mut self) -> Result<TraitItem, ParseError>
	{
		let decl_kind: DeclKind = self.peek_declaration_kind()?;

		let node: TraitItem = match decl_kind {
			DeclKind::Function => {
				let func: FunctionDecl = self.parse_function_decl()?;

				TraitItem::Function(func)
			}
			DeclKind::TypeAlias => {
				let type_alias: TypeAliasDecl = self.parse_type_alias()?;
				self.expect(&TokenKind::Semicolon)?;
				TraitItem::TypeAlias(type_alias)
			}
			DeclKind::AssocType => {
				let assoc_type: AssocTypeDecl = self.parse_assoc_type()?;
				self.expect(&TokenKind::Semicolon)?;
				TraitItem::AssocType(assoc_type)
			}
			DeclKind::Variable => {
				let var_decl: VariableDecl = self.parse_var_decl()?;
				self.expect(&TokenKind::Semicolon)?;
				TraitItem::Const(var_decl)
			}
			_ => {
				let tok: Token = self.next()?;
				return Err(ParseError::unexpected_item(
					tok.span,
					"trait block",
					tok.kind,
				));
			}
		};

		return Ok(node);
	}

	fn parse_delete(&mut self) -> Result<Expr, ParseError>
	{
		self.expect(&TokenKind::Delete)?;

		return self.parse_expr();
	}

	fn parse_docs(&mut self) -> Result<Option<DocsComment>, ParseError>
	{
		let mut combined_content = String::new();
		let mut start_span: Option<Span> = None;
		let mut end_span: Span = Span::default();

		while let Ok(TokenKind::DocsComment(content)) = self.peek_kind().cloned() {
			let span: Span = self.peek()?.span();
			self.next()?; // DocsComment

			if start_span.is_none() {
				start_span = Some(span);
			}

			if !combined_content.is_empty() {
				combined_content.push('\n');
			}
			combined_content.push_str(&content);
			end_span = span;
		}

		if let Some(start) = start_span {
			return Ok(Some(DocsComment {
				content: combined_content,
				span: start.merge(&end_span),
			}));
		}
		return Ok(None);
	}
}

pub fn extract_type_from_pattern(pattern: &Pattern) -> Option<Type>
{
	match pattern {
		Pattern::Wildcard { ty, .. } => return ty.clone(),
		Pattern::TypedIdentifier { ty, .. } => {
			return Some(ty.clone());
		}
		Pattern::Tuple { patterns, span } => {
			let mut types: Vec<Type> = Vec::new();
			for p in patterns {
				if let Some(ty) = extract_type_from_pattern(p) {
					types.push(ty);
				} else {
					return None;
				}
			}
			return Some(Type {
				core: Box::new(TypeCore::Tuple(types)),
				span: *span,
			});
		}
		Pattern::Struct { path, span, .. } => {
			return Some(Type {
				core: Box::new(TypeCore::Base {
					path: path.clone(),
					generics: vec![],
				}),
				span: *span,
			});
		}
		_ => return None,
	}
}

use std::fmt;

pub struct IndentWriter
{
	indent_level: usize,
	indent_str: &'static str,
}

impl IndentWriter
{
	pub const fn new() -> Self
	{
		return Self {
			indent_level: 0,
			indent_str: "    ", // 4 spaces
		};
	}

	pub const fn indent(&mut self)
	{
		self.indent_level += 1;
	}

	pub fn dedent(&mut self)
	{
		debug_assert!(self.indent_level > 0);
		self.indent_level -= 1;
	}

	pub fn write_indent(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		for _ in 0..self.indent_level {
			write!(f, "{}", self.indent_str)?;
		}
		return Ok(());
	}
}

impl fmt::Display for AST
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "{}", self.top_level_block);
	}
}

impl fmt::Display for TopLevelBlock
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

pub fn write_top_level_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, decl: &TopLevelDecl) -> fmt::Result
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
		TopLevelDecl::Module(n) => return write_module_decl(f, w, n),
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
			Modifier::Export => return write!(f, "export"),
			Modifier::Unsafe => return write!(f, "unsafe"),
			Modifier::Inline => return write!(f, "inline"),
			Modifier::Const => return write!(f, "const"),
			Modifier::Volatile => return write!(f, "volatile"),
			Modifier::Mut => return write!(f, "mut"),
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
					todo!()
				}
				BlockContent::TopLevelBlock(top_level) => {
					write!(f, "{{ {} items }}", top_level.items.len())?;
				}
			}
		}

		return Ok(());
	}
}

impl std::fmt::Display for Path
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		if self.global {
			write!(f, "::")?;
		}
		for (i, segment) in self.segments.iter().enumerate() {
			if i > 0 {
				write!(f, "::")?;
			}
			write!(f, "{}", segment.name)?;
			if !segment.generics.is_empty() {
				write!(f, "::<")?;
				for (i, g) in segment.generics.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", g)?;
				}
				write!(f, ">")?;
			}
		}
		if self.glob {
			if !self.segments.is_empty() {
				write!(f, "::")?;
			}
			write!(f, "*")?;
		}
		return Ok(());
	}
}

impl std::fmt::Display for PathSegment
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.name)?;

		if !self.generics.is_empty() {
			write!(f, "::<")?;
			for (i, g) in self.generics.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}", g)?;
			}
			write!(f, ">")?;
		}
		return Ok(());
	}
}

impl std::fmt::Display for Directive
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match self {
			Directive::Import {
				modifers,
				import,
				visibility: _,
			} => {
				for m in modifers {
					write!(f, "{} ", m)?;
				}
				return write!(f, "@import \"{}\"", import);
			}
			Directive::Use {
				modifers,
				use_path,
				visibility: _,
			} => {
				for m in modifers {
					write!(f, "{} ", m)?;
				}
				write!(f, "@use ")?;
				write!(f, "{}", use_path)?;
				return Ok(());
			}
			Directive::Custom { name, params } => {
				write!(f, "@{}", name)?;
				if !params.is_empty() {
					write!(f, "(")?;
					for (i, arg) in params.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", arg)?;
					}
					write!(f, ")")?;
				}
				return Ok(());
			}
			Directive::ValidateStructPattern {
				struct_path,
				pattern_fields,
				has_rest,
			} => {
				write!(f, "@#validate_struct_pattern({struct_path}{{")?;
				for (i, p) in pattern_fields.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", p)?;
				}
				if *has_rest {
					if !pattern_fields.is_empty() {
						write!(f, ", ")?;
					}
					write!(f, "..")?;
				}
				return write!(f, "}})");
			}
			Directive::ValidateType { ty, expr } => {
				return write!(f, "@#validate_type({} == #typeof({}))", ty, expr);
			}
		}
	}
}

impl fmt::Display for DirectiveParam
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			DirectiveParam::Literal(lit) => write!(f, "{}", lit),
			DirectiveParam::Identifier(ident) => write!(f, "{}", ident),
			DirectiveParam::Named { name, arg } => write!(f, "{} = {}", name, arg),
		};
	}
}

pub fn write_function_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, func: &FunctionDecl) -> fmt::Result
{
	write_docs(f, w, &func.docs)?;
	write_function_signature(f, w, &func.signature)?;

	if let Some(body) = &func.body {
		write!(f, " ")?;
		write_block(f, w, body)?;
	} else {
		write!(f, ";")?;
	}

	return Ok(());
}

pub fn write_function_signature(
	f: &mut fmt::Formatter<'_>,
	_w: &mut IndentWriter,
	sig: &FunctionSignature,
) -> fmt::Result
{
	for modifier in &sig.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "fn")?;

	match sig.call_type {
		CallType::UserHeap => write!(f, "!")?,
		CallType::UserMaybeHeap | CallType::CompilerHeap => write!(f, "?")?,
		CallType::Regular => {}
	}

	if !sig.heap_generics.is_empty() {
		write!(f, "<")?;
		for (i, generic_param) in sig.heap_generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic_param)?;
		}
		write!(f, ">")?;
	}

	write!(f, " {}", sig.name)?;

	if !sig.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in sig.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
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
		if self.variadic {
			return write!(f, "...");
		}

		return write!(f, "{}", self.pattern);
	}
}

impl fmt::Display for GenericParam
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.name)?;
		if !self.bounds.is_empty() {
			write!(f, ": ")?;
			for (i, bound) in self.bounds.iter().enumerate() {
				if i > 0 {
					write!(f, " + ")?;
				}
				write!(f, "{}", bound)?;
			}
		}
		return Ok(());
	}
}

impl fmt::Display for Type
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "{}", self.core);
	}
}

impl fmt::Display for TypeCore
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			TypeCore::Base { path, generics } => {
				write!(f, "{}", path)?;
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
			TypeCore::Mutable { inner } => {
				return write!(f, "mut {}", inner);
			}
			TypeCore::Pointer { inner } => return write!(f, "{}*", inner),
			TypeCore::Array { inner, size } => {
				write!(f, "[")?;
				write!(f, "{}", inner)?;
				if let Some(s) = size {
					write!(f, "; {}", s)?;
				}
				return write!(f, "]");
			}
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
			TypeCore::ImplTrait { bounds } => {
				write!(f, "impl ")?;
				for (i, bound) in bounds.iter().enumerate() {
					if i > 0 {
						write!(f, " + ")?;
					}
					write!(f, "{}", bound)?;
				}
				return Ok(());
			}
		}
	}
}

pub fn write_variable_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, var: &VariableDecl) -> fmt::Result
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
			Pattern::Wildcard { ty, .. } => {
				write!(f, "_")?;
				if let Some(t) = ty {
					write!(f, ": {}", t)?;
				}
				return Ok(());
			}
			Pattern::Literal { value: lit, .. } => return write!(f, "{}", lit),
			Pattern::TypedIdentifier {
				path,
				modifiers,
				ty,
				call_constructor,
				mutable,
				..
			} => {
				for modifier in modifiers {
					write!(f, "{} ", modifier)?;
				}
				if *mutable {
					write!(f, "mut ")?;
				}
				write!(f, "{}: {}", path, ty)?;
				if let Some(ct) = call_constructor {
					match ct {
						CallType::Regular => write!(f, "()")?,
						CallType::UserHeap => write!(f, "!()")?,
						CallType::UserMaybeHeap | CallType::CompilerHeap => write!(f, "?()")?,
					}
				}
				return Ok(());
			}
			Pattern::Variant { path, args, .. } => {
				write!(f, "{}", path)?;
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
			Pattern::Struct {
				path, fields, has_rest, ..
			} => {
				write!(f, "{} {{", path)?;
				for (i, (name, pat)) in fields.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{} -> {}", name, pat)?;
				}
				if *has_rest {
					if !fields.is_empty() {
						write!(f, ", ")?;
					}
					write!(f, "..")?;
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
			Expr::Identifier { path, .. } => return write!(f, "{}", path),
			Expr::Literal { value: lit, .. } => return write!(f, "{}", lit),
			Expr::Default { heap_call, .. } => {
				return {
					write!(f, "default")?;
					match *heap_call {
						CallType::Regular => write!(f, "()"),
						CallType::UserHeap => write!(f, "!()"),
						CallType::UserMaybeHeap | CallType::CompilerHeap => write!(f, "?()"),
					}
				};
			}
			Expr::Unary { op, expr, .. } => match op {
				UnaryOp::Neg => return write!(f, "-{}", expr),
				UnaryOp::Not => return write!(f, "!{}", expr),
				UnaryOp::Deref => return write!(f, "*{}", expr),
				UnaryOp::Addr { mutable } => {
					if *mutable {
						return write!(f, "&mut {}", expr);
					}
					return write!(f, "&{}", expr);
				}
			},
			Expr::Binary { op, lhs, rhs, .. } => return write!(f, "({} {} {})", lhs, op, rhs),
			Expr::Cast { ty, expr, .. } => return write!(f, "({}) {}", ty, expr),
			Expr::Call {
				callee,
				call_type,
				named_generics,
				args,
				..
			} => {
				write!(f, "{}", callee)?;

				match call_type {
					CallType::UserHeap => write!(f, "!")?,
					CallType::CompilerHeap | CallType::UserMaybeHeap => write!(f, "?")?,
					CallType::Regular => {}
				}

				if !named_generics.is_empty() {
					write!(f, "<")?;
					for (i, (name, ty)) in named_generics.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}: {}", name, ty)?;
					}
					write!(f, ">")?;
				}

				write!(f, "(")?;
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
			Expr::StructInit {
				path,
				fields,
				base,
				has_rest,
				..
			} => {
				write!(f, "{} {{", path)?;
				for (i, (name, expr)) in fields.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{} -> {}", name, expr)?;
				}
				if let Some(base_expr) = base {
					if !fields.is_empty() {
						write!(f, ", ")?;
					}
					write!(f, "..{}", base_expr)?;
				} else if *has_rest {
					if !fields.is_empty() {
						write!(f, ", ")?;
					}
					write!(f, "..")?;
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

pub fn write_switch(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, expr: &Expr, arms: &[SwitchArm]) -> fmt::Result
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
			Literal::Int {
				value,
				base,
				ty,
				span: _,
			} => {
				write!(f, "{}{}", base, value)?;
				if let Some(t) = ty {
					write!(f, "{}", t)?;
				}
				return Ok(());
			}
			Literal::Float { value, bits, span: _ } => {
				write!(f, "{}", value)?;
				if let Some(b) = bits {
					write!(f, "{}", b)?;
				}
				return Ok(());
			}
			Literal::Bool { value: b, span: _ } => return write!(f, "{}", b),
			Literal::String { value: s, span: _ } => return write!(f, "{:?}", s),
			Literal::Char { value: c, span: _ } => return write!(f, "{:?}", c),
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
				write!(f, "{}", value)?;
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

pub fn write_block(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, block: &Block) -> fmt::Result
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

pub fn write_expr(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, expr: &Expr) -> fmt::Result
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
			write!(f, "if var {} = ", pattern)?;

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

pub fn write_stmt(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, stmt: &Stmt) -> fmt::Result
{
	w.write_indent(f)?;
	return write_stmt_no_indent(f, w, stmt);
}

pub fn write_stmt_no_indent(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, stmt: &Stmt) -> fmt::Result
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
				return write!(f, ";");
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
			pattern,
			iter,
			body,
			..
		} => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "for {} in ", pattern)?;
			write_expr(f, w, iter)?;
			write!(f, " ")?;
			return write_block(f, w, body);
		}
		Stmt::Delete { expr, .. } => {
			write!(f, "delete ")?;
			write_expr(f, w, expr)?;
			return write!(f, ";");
		}
		Stmt::Unsafe(block) => {
			write!(f, "unsafe ")?;
			return write_block(f, w, block);
		}
		Stmt::Block(block) => return write_block(f, w, block),
		Stmt::Directive(directive_node) => {
			write!(f, "{}", directive_node)?;
			if directive_node.body.is_none() {
				write!(f, ";")?;
			}
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

pub fn write_struct_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, s: &StructDecl) -> fmt::Result
{
	write_docs(f, w, &s.docs)?;
	for modifier in &s.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "struct {}", s.name)?;
	if !s.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in s.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if !s.where_clause.is_empty() {
		write!(f, "\nwhere ")?;
		for (i, constraint) in s.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}

	writeln!(f, " {{")?;
	w.indent();

	for field in &s.fields {
		write_docs(f, w, &field.docs)?;
		w.write_indent(f)?;
		write!(f, "{}: {}", field.name, field.ty)?;
		if let Some(default) = &field.default_value {
			write!(f, " = {}", default)?;
		}
		writeln!(f, ",")?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_union_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, u: &UnionDecl) -> fmt::Result
{
	write_docs(f, w, &u.docs)?;
	for modifier in &u.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "union {}", u.name)?;
	if !u.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in u.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if !u.where_clause.is_empty() {
		write!(f, "\nwhere ")?;
		for (i, constraint) in u.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}
	writeln!(f, " {{")?;

	w.indent();

	for field in &u.fields {
		write_docs(f, w, &field.docs)?;
		w.write_indent(f)?;
		writeln!(f, "{}: {},", field.name, field.ty)?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_enum_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, e: &EnumDecl) -> fmt::Result
{
	write_docs(f, w, &e.docs)?;
	for modifier in &e.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "enum {}", e.name)?;
	if !e.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in e.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if !e.where_clause.is_empty() {
		write!(f, "\nwhere ")?;
		for (i, constraint) in e.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}

	writeln!(f, " {{")?;
	w.indent();

	for variant in &e.variants {
		write_docs(f, w, &variant.docs)?;
		w.write_indent(f)?;
		if let Some(val) = &variant.value {
			write!(f, "{} = ", variant.name)?;
			write_expr(f, w, val)?;
		} else {
			writeln!(f, "{},", variant.name)?;
		}
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_variant_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, v: &VariantDecl) -> fmt::Result
{
	write_docs(f, w, &v.docs)?;
	for modifier in &v.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "variant {}", v.name)?;
	if !v.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in v.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if !v.where_clause.is_empty() {
		write!(f, "\nwhere\n")?;
		for (i, constraint) in v.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}

	writeln!(f, " {{")?;
	w.indent();

	for member in &v.variants {
		write_docs(f, w, &member.docs)?;
		w.write_indent(f)?;
		write!(f, "{}", member.name)?;
		if let Some(t) = &member.ty {
			write!(f, "({})", t)?;
		}
		if let Some(val) = &member.value {
			write!(f, " = ")?;
			write_expr(f, w, val)?;
		}
		writeln!(f, ",")?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_type_alias_decl(f: &mut fmt::Formatter<'_>, w: &IndentWriter, t: &TypeAliasDecl) -> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for modifier in &t.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "type {}", t.name)?;

	if !t.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in t.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	return write!(f, " = {}", t.ty);
}

pub fn write_type_assoc_type(f: &mut fmt::Formatter<'_>, w: &IndentWriter, t: &AssocTypeDecl) -> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for modifier in &t.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "type {}", t.name)?;

	if !t.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in t.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if let Some(ty) = &t.ty {
		write!(f, " = {}", ty)?;
	}
	return Ok(());
}

pub fn write_module_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, n: &ModuleDecl) -> fmt::Result
{
	write_docs(f, w, &n.docs)?;
	for modifier in &n.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "module {}", n.name)?;

	match &n.kind {
		ModuleKind::Inline(inline) => {
			writeln!(f, " {{")?;
			w.indent();
			for item in &inline.items {
				w.write_indent(f)?;
				write_top_level_decl(f, w, item)?;
				writeln!(f)?;
				writeln!(f)?;
			}
			w.dedent();
			w.write_indent(f)?;
			write!(f, "}}")?;
		}
		ModuleKind::External => {
			write!(f, ";")?;
		}
	}
	return Ok(());
}

pub fn write_trait_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, t: &TraitDecl) -> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for modifier in &t.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "trait {}", t.name)?;

	if !t.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in t.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if !t.super_traits.is_empty() {
		write!(f, ": ")?;
		for (i, st) in t.super_traits.iter().enumerate() {
			if i > 0 {
				write!(f, " + ")?;
			}
			write!(f, "{}", st)?;
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

pub fn write_trait_item(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, item: &TraitItem) -> fmt::Result
{
	match item {
		TraitItem::Function(func) => {
			return write_function_decl(f, w, func);
		}
		TraitItem::TypeAlias(ta) => {
			write_type_alias_decl(f, w, ta)?;
			return write!(f, ";");
		}
		TraitItem::Const(var) => {
			write_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		TraitItem::AssocType(ty) => {
			write_type_assoc_type(f, w, ty)?;
			return write!(f, ";");
		}
	}
}

pub fn write_impl_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, i: &ImplDecl) -> fmt::Result
{
	write_docs(f, w, &i.docs)?;
	for modifier in &i.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "impl")?;

	if !i.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in i.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
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

pub fn write_impl_item(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, item: &ImplItem) -> fmt::Result
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
		ImplItem::AssocType(ty) => {
			write_type_assoc_type(f, w, ty)?;
			return write!(f, ";");
		}
	}
}

impl fmt::Display for ImplTarget
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.path)?;

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
		write!(f, "{}", self.ty)?;

		if !self.type_args.is_empty() {
			write!(f, "<")?;
			for (i, arg) in self.type_args.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}", arg)?;
			}
			write!(f, ">")?;
		}

		write!(f, ": ")?;
		for (i, bound) in self.bounds.iter().enumerate() {
			if i > 0 {
				write!(f, " + ")?;
			}
			write!(f, "{}", bound)?;
		}
		return Ok(());
	}
}

impl fmt::Display for WhereBound
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			WhereBound::Path { path, args } => {
				write!(f, "{}", path)?;
				if !args.is_empty() {
					write!(f, "<")?;
					for (i, arg) in args.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", arg)?;
					}
					write!(f, ">")?;
				}
				Ok(())
			}
			WhereBound::Func(func_bound) => {
				write!(f, "{}", func_bound)
			}
		};
	}
}

impl fmt::Display for FuncBound
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			FuncBound::Fn { args, ret } => {
				write!(f, "Fn(")?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{},", a)?;
					write!(f, ")")?;
				}
				if let Some(ty) = ret {
					write!(f, "-> {}", ty)?;
				}
				Ok(())
			}
		};
	}
}

impl fmt::Display for GenericArg
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			GenericArg::Type(ty) => write!(f, "{}", ty),
			GenericArg::Binding { name, ty, .. } => write!(f, "{} = {}", name, ty),
		};
	}
}

#[allow(clippy::ref_option)]
pub fn write_docs(f: &mut fmt::Formatter<'_>, w: &IndentWriter, docs: &Option<DocsComment>) -> fmt::Result
{
	if let Some(doc) = docs {
		for line in doc.content.lines() {
			w.write_indent(f)?;
			writeln!(f, "///{}", line)?;
		}
	}
	return Ok(());
}

impl fmt::Display for DocsComment
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "///{}", self.content);
	}
}
