mod tests;

use std::{convert::TryFrom, iter::Peekable};

use ignorable::PartialEq;

use crate::{
	CompileError, Config,
	lexer::{self, Lexer, Span, Token, TokenKind},
	source_map::SourceIndex,
};

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
/// ```no_run
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
pub struct Parser<'source, 'config>
{
	#[allow(unused)]
	config: &'config Config,
	#[allow(unused)]
	source_index: SourceIndex,
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
	/// ```no_run
	/// # use crate::{Parser, Config, SourceIndex};
	/// # use crate::lexer::Lexer;
	/// # let config = Config::default();
	/// # let source = "fn main() {}";
	/// # let source_index = SourceIndex(0);
	/// let lexer = Lexer::new(&config, source, source_index);
	/// let mut parser = Parser::from(lexer);
	/// ```
	fn from(lexer: Lexer<'s, 'c>) -> Self
	{
		let (config, source_index, lex) = lexer.into_parts();
		return Self {
			config,
			source_index,
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
pub trait Spanned // TODO: Why do I have this also defined here????
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
#[derive(Debug, Clone, PartialEq)]
pub struct Program
{
	pub items: Vec<TopLevelDecl>,
	#[ignored(PartialEq)]
	pub span: Span,
}

impl Spanned for Program
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

impl<'s, 'c> TryFrom<Parser<'s, 'c>> for Program
{
	type Error = CompileError;

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
	/// * `Err(CompileError)` - If a syntax error is encountered during parsing
	///
	/// # Example
	/// ```no_run
	/// # use crate::{Parser, Program, CompileError, Config, SourceIndex};
	/// # use crate::lexer::Lexer;
	/// # fn main() -> Result<(), CompileError> {
	/// let config = Config::default();
	/// let source = "fn main() { var x = 42; }";
	/// let source_index = SourceIndex(0);
	/// let lexer = Lexer::new(&config, source, source_index);
	/// let parser = Parser::from(lexer);
	///
	/// // Convert parser to Program using TryFrom
	/// let program = Program::try_from(parser)?;
	///
	/// // Or more idiomatically in a single chain:
	/// let program = Program::try_from(Parser::from(Lexer::new(&config, source, source_index)))?;
	/// # Ok(())
	/// # }
	/// ```
	fn try_from(mut parser: Parser<'s, 'c>) -> Result<Self, Self::Error>
	{
		return parser.parse_program();
	}
}

impl<'s, 'c> TryFrom<Lexer<'s, 'c>> for Program
{
	type Error = CompileError;

	/// Converts a lexer into a parsed program.
	///
	/// This provides a convenient way to parse a complete program directly from a lexer.
	/// Internally, it first converts the lexer into a parser, then parses the program.
	///
	/// # Arguments
	/// * `lexer` - The lexer to consume
	///
	/// # Returns
	/// * `Ok(Program)` - The successfully parsed program AST
	/// * `Err(CompileError)` - If a syntax error is encountered during parsing
	///
	/// # Example
	/// ```no_run
	/// # use crate::lexer::Lexer;
	/// # use crate::{Parser, Program, Config, CompileError, SourceIndex};
	/// # fn main() -> Result<(), CompileError> {
	/// let config = Config::default();
	/// let source = "fn main() { var x = 42; }";
	/// let source_index = SourceIndex(0);
	/// let lexer = Lexer::new(&config, source, source_index);
	///
	/// // Convert lexer directly to a Program
	/// let program = Program::try_from(lexer)?;
	///
	/// // Or in a single chain:
	/// let program = Program::try_from(Lexer::new(&config, source, source_index))?;
	/// # Ok(())
	/// # }
	/// ```
	fn try_from(lexer: Lexer<'s, 'c>) -> Result<Self, Self::Error>
	{
		let mut parser: Parser<'_, '_> = Parser::from(lexer);

		return parser.parse_program();
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
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub enum Directive
{
	Import(String),
	Use(Path),
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
#[derive(Debug, Clone, PartialEq)]
pub struct DirectiveNode
{
	pub directive: Directive,
	pub body: Option<BlockContent>,
	#[ignored(PartialEq)]
	pub span: Span,
}

impl Spanned for DirectiveNode
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Qualified path representing a sequence of identifiers separated by `::`.
///
/// A path is used to reference items across module boundaries and can include
/// generic type arguments. Paths are fundamental to the type system and are used
/// for type names, function names, trait references, and more.
///
/// # Structure
///
/// A path consists of two parts:
/// - **Segments**: A sequence of identifiers separated by `::` (e.g., `std::collections::Vec`)
/// - **Generics**: Optional generic type arguments applied to the path (e.g., `::<i32, String>`)
///
/// # Fields
///
/// * `segments` - The identifier segments making up the path. Each segment is a `String`.
///   Examples: `["std", "collections", "Vec"]` or `["MyStruct"]`
/// * `generics` - Generic type arguments applied to the path. Empty if no generics are used.
///   These are applied to the entire path, not individual segments.
/// * `span` - Source location information for error reporting and debugging
///
/// # Syntax Examples
///
/// ```text
/// std::io::File              // 3 segments, 0 generics
/// Vec                        // 1 segment, 0 generics
/// HashMap::<String, i32>     // 1 segment, 2 generics
/// std::collections::Vec::<T> // 3 segments, 1 generic
/// ```
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Path
{
	pub segments: Vec<Ident>,
	pub generics: Vec<Type>,
	#[allow(dead_code)]
	#[ignored(PartialEq)]
	pub span: Span,
}

impl Path
{
	/// Creates a simple path without generic arguments.
	///
	/// # Arguments
	/// * `segments` - The identifier segments
	/// * `span` - Source location
	///
	/// # Example
	/// # Example
	/// ```no_run
	/// # use crate::parser::{Path, Span};
	/// let path = Path::simple(vec!["std".to_string(), "io".to_string()], Span::default());
	/// ```
	#[allow(dead_code)]
	pub const fn simple(segments: Vec<Ident>, span: Span) -> Self
	{
		return Self {
			segments,
			generics: Vec::new(),
			span,
		};
	}

	/// Checks if this path has generic type arguments.
	///
	/// # Returns
	/// `true` if the path has generic arguments, `false` otherwise
	#[allow(dead_code)]
	pub const fn has_generics(&self) -> bool
	{
		return !self.generics.is_empty();
	}

	/// Checks if this path is empty (no segments or generics).
	///
	/// # Returns
	/// `true` if the path is empty, `false` otherwise
	#[allow(dead_code)]
	pub const fn is_empty(&self) -> bool
	{
		return !(self.segments.is_empty() && self.generics.is_empty());
	}

	/// Returns the total length (segments + generics).
	///
	/// # Returns
	/// The sum of segment count and generic count
	#[allow(dead_code)]
	pub const fn len(&self) -> usize
	{
		return self.segments.len() + self.generics.len();
	}
}

#[allow(dead_code)]
pub enum PathComponent<'a>
{
	Segment(&'a Ident),
	Generic(&'a Type),
}

/// Iterator over path components (segments and generics).
///
/// `PathIter` is primarily used as the item type for iterating over a `Path`.
/// When iterating over a path, you get a sequence of components that can be either
/// segments or generics, distinguished by the `PathComponent` enum.
///
/// # Purpose
///
/// `PathComponent` is primarily used as the item type for `PathIter`. When
/// iterating over a path, you get a sequence of components that can be either
/// segments or generics, and this enum lets you distinguish between them.
///
/// # Variants
///
/// * `Segment(&'a Ident)` - A reference to an identifier segment.
///   This represents one piece of the `::` separated path.
///
///   Examples: `"std"`, `"Vec"`, `"io"`, `"MyType"`
///
/// * `Generic(&'a Type)` - A reference to a generic type argument.
///   This represents one type in the `::<...>` type arguments.
///
///   Examples: The `i32` in `Vec::<i32>`, or the `String` in `Option::<String>`
///
/// # Lifetime
///
/// The `'a` lifetime ties the component references back to the original `Path`
/// that owns the data. Components are borrowed, not owned, so they're only
/// valid as long as the source `Path` exists.
pub struct PathIter<'a>
{
	segments: std::slice::Iter<'a, Ident>,
	generics: std::slice::Iter<'a, Type>,
}

impl<'a> Iterator for PathIter<'a>
{
	type Item = PathComponent<'a>;

	fn next(&mut self) -> Option<Self::Item>
	{
		if let Some(segment) = self.segments.next() {
			return Some(PathComponent::Segment(segment));
		} else {
			return self
				.generics
				.next()
				.map(|generic| -> PathComponent<'_> { return PathComponent::Generic(generic) });
		}
	}

	fn size_hint(&self) -> (usize, Option<usize>)
	{
		let len = self.segments.len() + self.generics.len();
		return (len, Some(len));
	}
}

impl<'a> ExactSizeIterator for PathIter<'a>
{
	fn len(&self) -> usize
	{
		return self.segments.len() + self.generics.len();
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
	pub fn iter(&'_ self) -> PathIter<'_>
	{
		return PathIter {
			segments: self.segments.iter(),
			generics: self.generics.iter(),
		};
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
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl
{
	pub signature: FunctionSignature,
	pub body: Option<Block>,
	#[ignored(PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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

impl Spanned for FunctionSignature
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Generic parameter with optional trait bounds.
///
/// Represents a generic type parameter that can optionally have trait bounds
/// specified inline using the `:` syntax.
///
/// # Examples
/// ```text
/// T              // No bounds
/// T: Clone       // Single bound
/// T: Clone + Debug  // Multiple bounds
/// ```
#[derive(Debug, Clone, PartialEq)]
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

impl Spanned for GenericParam
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
/// * `mutable` - Whether the parameter is mutable
/// * `span` - Source location of the parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Param
{
	pub ty: Type,
	pub name: Path,
	pub mutable: bool,
	#[ignored(PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub struct Type
{
	pub modifiers: Vec<Modifier>,
	pub core: Box<TypeCore>,
	#[ignored(PartialEq)]
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
/// * `Mutable` - Mutable type wrapper
/// * `Pointer` - Raw pointer type (`T*`)
/// * `Array` - Array type with size expression (`T[size]`)
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
#[derive(Debug, Clone, PartialEq)]
pub struct RangeExpr
{
	pub start: Option<Box<Expr>>,
	pub end: Option<Box<Expr>>,
	pub inclusive: bool,
	#[ignored(PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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
		name: Ident,
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

impl Spanned for Expr
{
	fn span(&self) -> Span
	{
		#[allow(clippy::match_same_arms)]
		return match self {
			Expr::Identifier { span, .. } => *span,
			Expr::Literal { span, .. } => *span,
			Expr::Default { span, .. } => *span,
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

/// Type of function call
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
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDecl
{
	pub pattern: Pattern,
	pub init: Option<Expr>,
	pub comp_const: bool,
	#[ignored(PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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
		name: Path,
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
/// ```text
/// // Block with tail expression:
/// {
///     var x = 5;
///     x + 1  // tail expression, block evaluates to 6
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Block
{
	pub stmts: Vec<Stmt>,
	pub tail_expr: Option<Box<Expr>>,
	#[ignored(PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub struct SwitchArm
{
	pub pattern: Pattern,
	pub body: SwitchBody,
	#[ignored(PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern
{
	Wildcard
	{
		#[ignored(PartialEq)]
		span: Span,
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
		ty: Type,
		call_constructor: Option<CallType>,
		#[ignored(PartialEq)]
		span: Span,
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
/// * `fields` - List of (`type`, `name`, `Option<default_value>`) tuples for fields
/// * `span` - Source location of the struct
#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub fields: Vec<(Type, Ident, Option<Expr>)>,
	#[ignored(PartialEq)]
	pub span: Span,
}

impl Spanned for StructDecl
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

/// Union type declaration.
///
/// Represents a union with named fields.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Union name (can be qualified path)
/// * `fields` - List of (type, name) tuples for fields
/// * `span` - Source location of the union
#[derive(Debug, Clone, PartialEq)]
pub struct UnionDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub fields: Vec<(Type, Ident)>,
	#[ignored(PartialEq)]
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
/// * `variants` - List of (`name`, `Option<value>`) tuples
/// * `span` - Source location of the enum
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub variants: Vec<(Ident, Option<Expr>)>,
	#[ignored(PartialEq)]
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
/// * `variants` - List of (`Option<type>`, `name`, `Option<value>`) tuples for variants
/// * `span` - Source location of the variant
#[derive(Debug, Clone, PartialEq)]
pub struct VariantDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub variants: Vec<(Option<Type>, Ident, Option<Expr>)>,
	#[ignored(PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub struct TraitDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub generics: Vec<GenericParam>,
	pub super_traits: Vec<WhereBound>,
	pub items: Vec<TraitItem>,
	#[ignored(PartialEq)]
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
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
pub enum TraitItem
{
	Function
	{
		signature: FunctionSignature,
		body: Option<Block>,
		#[ignored(PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub struct ImplDecl
{
	pub modifiers: Vec<Modifier>,
	pub generics: Vec<GenericParam>,
	pub target: ImplTarget,
	pub trait_path: Option<ImplTarget>,
	pub where_clause: Vec<WhereConstraint>,
	pub body: Vec<ImplItem>,
	#[ignored(PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub struct ImplTarget
{
	pub path: Path,
	pub generics: Vec<Type>,
	#[ignored(PartialEq)]
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
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
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
/// * `type_args` - Type arguments for the constraint
/// * `span` - Source location of the constraint
#[derive(Debug, Clone, PartialEq)]
pub struct WhereConstraint
{
	pub ty: Path,
	pub bounds: Vec<WhereBound>,
	pub type_args: Vec<Type>,
	#[ignored(PartialEq)]
	pub span: Span,
}

impl Spanned for WhereConstraint
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum WhereBound
{
	Path(Path),
	Func(FuncBound),
}

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
#[derive(Debug, Clone)]
pub struct ParseError
{
	pub span: Span,
	pub kind: ParseErrorKind,
	pub context: Vec<String>,
	pub source_index: SourceIndex,
}

impl ParseError
{
	/// Creates a new parse error.
	///
	/// # Arguments
	/// * `span` - Source location of the error
	/// * `kind` - The kind of parse error
	/// * `source_index` - Index into the source map
	pub const fn new(span: Span, kind: ParseErrorKind, source_index: SourceIndex) -> Self
	{
		return Self {
			span,
			kind,
			context: Vec::new(),
			source_index,
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
	pub const fn unexpected_token(span: Span, expected: Expected, found: TokenKind, source_index: SourceIndex) -> Self
	{
		return Self::new(span, ParseErrorKind::UnexpectedToken { expected, found }, source_index);
	}

	/// Creates an unexpected EOF error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `source_index` - Index into the source map
	pub const fn unexpected_eof(span: Span, source_index: SourceIndex) -> Self
	{
		return Self::new(span, ParseErrorKind::UnexpectedEof, source_index);
	}

	/// Creates an invalid pattern error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `reason` - Why the pattern is invalid
	/// * `source_index` - Index into the source map
	pub fn invalid_pattern(span: Span, reason: impl Into<String>, source_index: SourceIndex) -> Self
	{
		return Self::new(
			span,
			ParseErrorKind::InvalidPattern { reason: reason.into() },
			source_index,
		);
	}

	/// Creates an unbalanced delimiter error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `delimiter` - The unbalanced delimiter character
	/// * `source_index` - Index into the source map
	pub const fn unbalanced_delimiter(span: Span, delimiter: char, source_index: SourceIndex) -> Self
	{
		return Self::new(span, ParseErrorKind::UnbalancedDelimiter { delimiter }, source_index);
	}

	/// Creates an invalid type error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `reason` - Why the type is invalid
	/// * `source_index` - Index into the source map
	pub fn invalid_type(span: Span, reason: impl Into<String>, source_index: SourceIndex) -> Self
	{
		return Self::new(
			span,
			ParseErrorKind::InvalidType { reason: reason.into() },
			source_index,
		);
	}

	/// Creates an invalid declaration error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `reason` - Why the declaration is invalid
	/// * `source_index` - Index into the source map
	pub fn invalid_declaration(span: Span, reason: impl Into<String>, source_index: SourceIndex) -> Self
	{
		return Self::new(
			span,
			ParseErrorKind::InvalidDeclaration { reason: reason.into() },
			source_index,
		);
	}

	/// Creates an unexpected item error.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `context` - Parsing context where item was unexpected
	/// * `found` - The unexpected token
	/// * `source_index` - Index into the source map
	pub fn unexpected_item(span: Span, context: impl Into<String>, found: TokenKind, source_index: SourceIndex)
	-> Self
	{
		return Self::new(
			span,
			ParseErrorKind::UnexpectedItem {
				context: context.into(),
				found,
			},
			source_index,
		);
	}

	/// Creates a generic error with custom message.
	///
	/// # Arguments
	/// * `span` - Source location
	/// * `message` - Error message
	/// * `source_index` - Index into the source map
	pub fn generic(span: Span, message: impl Into<String>, source_index: SourceIndex) -> Self
	{
		return Self::new(
			span,
			ParseErrorKind::Generic {
				message: message.into(),
			},
			source_index,
		);
	}

	/// Writes the error to a formatter with source context.
	///
	/// # Arguments
	/// * `f` - The formatter to write to
	/// * `source_map` - Source map for looking up source text
	///
	/// # Returns
	/// Result of the formatting operation
	pub fn write(&self, f: &mut impl std::fmt::Write, source_map: &crate::source_map::SourceMap) -> std::fmt::Result
	{
		return write!(
			f,
			"{}",
			self.span
				.format_error(&source_map.get(self.source_index).src, &format!("{self}"))
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
			ParseErrorKind::Generic { message } => {
				write!(f, "{}", message)?;
			}
		}

		if !self.context.is_empty() {
			write!(f, "\n  while parsing: {}", self.context.join(" → "))?;
		}

		return Ok(());
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

// impl ErrorFromSpan for ParseError
// {
// 	fn from_span(span: impl lexer::Spanned, message: impl Into<String>) -> Self
// 	{
// 		return Self {
// 			span: span.span(),
// 			message: message.into(),
// 		};
// 	}
// }

/// Type alias declaration.
///
/// Represents a type alias like `type Int = i32;`.
///
/// # Fields
/// * `modifiers` - Visibility and other modifiers
/// * `name` - Alias name (can be qualified path)
/// * `ty` - Type being aliased
/// * `span` - Source location of the type alias
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub ty: Type,
	#[ignored(PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
pub struct NamespaceDecl
{
	pub modifiers: Vec<Modifier>,
	pub name: Path,
	pub body: TopLevelBlock,
	#[ignored(PartialEq)]
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

	fn expect(&mut self, expected: &TokenKind) -> Result<Token, CompileError>
	{
		let tok: &Token = self.peek();

		if &tok.kind == expected {
			return Ok(self.next());
		} else {
			let err_tok: Token = tok.clone();
			return Err(CompileError::ParseError(ParseError::unexpected_token(
				err_tok.span,
				Expected::Token(expected.clone()),
				err_tok.kind,
				self.source_index,
			)));
		}
	}

	/// Parse a complete program.
	///
	/// Entry point for parsing a source file. Parses all top-level declarations
	/// until EOF is reached.
	///
	/// # Returns
	/// * `Ok(Program)` - The parsed program AST
	/// * `Err(CompileError)` - If a syntax error is encountered
	///
	/// # Example
	/// ```no_run
	/// # use crate::{Parser, Program, CompileError};
	/// # fn example(parser: &mut Parser) -> Result<(), CompileError> {
	/// let program = parser.parse_program()?;
	/// println!("Parsed {} items", program.items.len());
	/// # Ok(())
	/// # }
	/// ```
	pub fn parse_program(&mut self) -> Result<Program, CompileError>
	{
		let mut items: Vec<TopLevelDecl> = Vec::new();

		while !matches!(self.peek().kind, TokenKind::Eof | TokenKind::RightBrace) {
			if self.consume(&TokenKind::Semicolon) {
				continue;
			}
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

	fn parse_top_level_decl(&mut self) -> Result<TopLevelDecl, CompileError>
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

	fn peek_declaration_kind(&mut self) -> Result<DeclKind, CompileError>
	{
		let checkpoint: Peekable<Lexer<'s, 'c>> = self.lexer.clone();
		let checkpoint_span: Span = self.last_span;

		loop {
			match self.peek_kind() {
				TokenKind::Pub | TokenKind::Unsafe | TokenKind::Inline | TokenKind::Volatile => {
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
							TokenKind::Pub
							| TokenKind::Unsafe
							| TokenKind::Inline
							| TokenKind::Volatile
							| TokenKind::Directive(_) => {
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
					let tok: Token = self.peek().clone();
					self.lexer = checkpoint;
					self.last_span = checkpoint_span;
					return Err(CompileError::ParseError(ParseError::unexpected_item(
						tok.span,
						"declaration",
						tok.kind,
						self.source_index,
					)));
				}
			}
		}
	}

	fn skip_until_balanced_paren(&mut self) -> Result<(), CompileError>
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
					return Err(CompileError::ParseError(ParseError::unexpected_eof(
						self.peek().span,
						self.source_index,
					)));
				}
				_ => {
					self.next();
				}
			}
		}
		return Ok(());
	}

	fn parse_directive_node(&mut self) -> Result<DirectiveNode, CompileError>
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

	fn parse_directive(&mut self) -> Result<Directive, CompileError>
	{
		#[allow(clippy::debug_assert_with_mut_call)]
		{
			debug_assert!(matches!(self.peek().kind, TokenKind::Directive(_)));
		}

		let tok: Token = self.next();
		// let start: Span = tok.span;

		let node: Directive = match tok.kind {
			TokenKind::Directive(d) => self.parse_directive_kind(d)?,
			_ => unreachable!("Bug: Token should be a directive"),
		};

		// let end: Span = self.last_span;

		return Ok(node);
	}

	fn parse_directive_kind(&mut self, direct: lexer::Directive) -> Result<Directive, CompileError>
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
						return Err(CompileError::ParseError(ParseError::unexpected_token(
							incl.span,
							Expected::Token(TokenKind::StringLiteral(String::new())),
							incl.kind,
							self.source_index,
						)));
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

	fn parse_var_decl(&mut self) -> Result<VariableDecl, CompileError>
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

	fn parse_type(&mut self) -> Result<Type, CompileError>
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

	fn parse_type_core(&mut self) -> Result<TypeCore, CompileError>
	{
		let tok: &Token = self.peek();
		match &tok.kind {
			TokenKind::Impl => {
				self.next(); // impl
				let bounds: Vec<WhereBound> = self.parse_trait_bounds()?;
				return Ok(TypeCore::ImplTrait { bounds });
			}
			TokenKind::Mut => {
				self.next(); // mut
				let inner: Box<TypeCore> = Box::new(self.parse_type_core()?);
				return Ok(TypeCore::Mutable { inner });
			}
			TokenKind::Identifier(_) => {
				let path: Path = self.get_path()?;
				let generics: Vec<Type> = if self.at(&TokenKind::LessThan) {
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
				return Err(CompileError::ParseError(ParseError::invalid_type(
					err_tok.span,
					"expected '&', 'mut', identifier, or '(' to start a type",
					self.source_index,
				)));
			}
		}
	}

	fn parse_type_suffix(&mut self, mut base: TypeCore) -> Result<TypeCore, CompileError>
	{
		loop {
			match self.peek_kind() {
				TokenKind::Star => {
					self.next(); // *
					base = TypeCore::Pointer { inner: Box::new(base) };
				}
				TokenKind::LeftBracket => {
					self.next(); // [
					let size_expr = if !self.at(&TokenKind::RightBracket) {
						Some(Box::new(self.parse_expr()?))
					} else {
						None
					};
					self.expect(&TokenKind::RightBracket)?; // ]
					base = TypeCore::Array {
						inner: Box::new(base),
						size: size_expr,
					};
				}
				_ => break,
			}
		}
		return Ok(base);
	}

	fn get_path(&mut self) -> Result<Path, CompileError>
	{
		let start_span: Span = self.peek().span();
		let mut segments: Vec<Ident> = Vec::new();

		loop {
			let tok: Token = self.next();
			match &tok.kind {
				TokenKind::Identifier(s) => segments.push(s.clone()),
				_ => {
					return Err(CompileError::ParseError(ParseError::unexpected_token(
						tok.span,
						Expected::Identifier,
						tok.kind,
						self.source_index,
					)));
				}
			}

			if self.peek().kind != TokenKind::DoubleColon {
				break;
			}

			let double_colon = self.next(); // ::
			if !matches!(self.peek().kind, TokenKind::Identifier(_)) {
				self.buffered_token = Some(double_colon);
				break;
			}
		}

		let generics: Vec<Type> = if self.peek().kind == TokenKind::DoubleColon {
			self.next(); // ::

			if self.peek().kind == TokenKind::LessThan {
				self.parse_type_generics()?
			} else {
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					self.peek().span(),
					Expected::Token(TokenKind::LessThan),
					self.peek().kind.clone(),
					self.source_index,
				)));
			}
		} else {
			Vec::new()
		};

		return Ok(Path {
			segments,
			generics,
			span: start_span.merge(&self.last_span),
		});
	}

	fn get_generics(&mut self) -> Result<Vec<GenericParam>, CompileError>
	{
		if !self.consume(&TokenKind::LessThan) {
			return Ok(Vec::new());
		}

		let mut generics: Vec<GenericParam> = Vec::new();

		if self.consume_greater_than() {
			return Ok(generics);
		}

		loop {
			let start_span: Span = self.peek().span;
			let tok: Token = self.next();

			let name: Ident = match tok.kind {
				TokenKind::Identifier(name) => name,
				_ => {
					return Err(CompileError::ParseError(ParseError::unexpected_token(
						tok.span,
						Expected::Identifier,
						tok.kind,
						self.source_index,
					)));
				}
			};

			let bounds: Vec<WhereBound> = if self.consume(&TokenKind::Colon) {
				self.parse_trait_bounds()?
			} else {
				Vec::new()
			};

			generics.push(GenericParam {
				name,
				bounds,
				span: start_span.merge(&self.last_span),
			});

			if self.consume_greater_than() {
				break;
			}

			if !self.consume(&TokenKind::Comma) {
				let tok: Token = self.next();
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::OneOf(vec![TokenKind::Comma, TokenKind::GreaterThan]),
					tok.kind,
					self.source_index,
				)));
			}

			if self.consume_greater_than() {
				break;
			}
		}

		return Ok(generics);
	}

	fn parse_expr(&mut self) -> Result<Expr, CompileError>
	{
		return self.parse_expr_inner(true);
	}

	fn parse_expr_no_struct(&mut self) -> Result<Expr, CompileError>
	{
		return self.parse_expr_inner(false);
	}

	fn parse_expr_inner(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
	{
		return self.parse_logical_or(allow_struct_init);
	}

	fn parse_logical_or(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_logical_and(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_bitwise_or(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_bitwise_xor(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_bitwise_and(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_equality(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_relational(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_shift(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_range(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
	{
		let span: Span = self.peek().span();

		if self.at(&TokenKind::DotDot) || self.at(&TokenKind::DotDotEquals) {
			let inclusive = self.at(&TokenKind::DotDotEquals);
			self.next(); // .. | ..=

			let end: Option<Box<Expr>> = if self.is_range_end() {
				None
			} else {
				Some(Box::new(self.parse_additive(allow_struct_init)?))
			};

			return Ok(Expr::Range(RangeExpr {
				start: None,
				end,
				inclusive,
				span: span.merge(&self.last_span),
			}));
		}

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

	fn parse_additive(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_multiplicative(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_cast(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
	{
		let span: Span = self.peek().span();
		if self.at(&TokenKind::LeftParen) {
			let checkpoint: Peekable<Lexer<'s, 'c>> = self.lexer.clone();
			let checkpoint_buffered = self.buffered_token.clone();
			self.next(); // (

			if let Ok(ty) = self.parse_type()
				&& self.consume(&TokenKind::RightParen)
			{
				let next_tok = self.peek_kind();

				if matches!(next_tok, TokenKind::DotDot | TokenKind::DotDotEquals) {
					self.lexer = checkpoint;
					self.buffered_token = checkpoint_buffered;
					return self.parse_unary(allow_struct_init);
				}

				let expr: Expr = self.parse_cast(allow_struct_init)?;
				return Ok(Expr::Cast {
					ty: Box::new(ty),
					expr: Box::new(expr),
					span: span.merge(&self.last_span),
				});
			}

			self.lexer = checkpoint;
			self.buffered_token = checkpoint_buffered;
		}

		return self.parse_unary(allow_struct_init);
	}

	fn parse_unary(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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

	fn parse_postfix(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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
						return Err(CompileError::ParseError(ParseError::unexpected_token(
							field_tok.span,
							Expected::Identifier,
							field_tok.kind,
							self.source_index,
						)));
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
						call_type: CallType::Regular,
						named_generics: Vec::new(),
						args,
						span: span.merge(&self.last_span),
					};
				}
				TokenKind::Bang | TokenKind::QuestionMark => {
					let call_type = if self.consume(&TokenKind::Bang) {
						CallType::UserHeap
					} else if self.consume(&TokenKind::QuestionMark) {
						CallType::UserMaybeHeap
					} else {
						unreachable!()
					};

					let named_generics: Vec<(Ident, Type)> = if self.at(&TokenKind::LessThan) {
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
				_ => break,
			}
		}

		return Ok(expr);
	}

	fn parse_primary(&mut self, allow_struct_init: bool) -> Result<Expr, CompileError>
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
				self.next(); // default

				let call_type = if self.consume(&TokenKind::Bang) {
					CallType::UserHeap
				} else if self.consume(&TokenKind::QuestionMark) {
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
				self.next();
				return Ok(Expr::Identifier {
					path: Path::simple(vec!["self".to_string()], tok.span()),
					span: span.merge(&self.last_span),
				});
			}

			TokenKind::Identifier(_) => {
				let path: Path = self.get_path()?;

				let call_type = if self.consume(&TokenKind::Bang) {
					Some(CallType::UserHeap)
				} else if self.consume(&TokenKind::QuestionMark) {
					Some(CallType::UserMaybeHeap)
				} else {
					None
				};

				if let Some(ct) = call_type {
					let named_generics: Vec<(Ident, Type)> = if self.at(&TokenKind::LessThan) {
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

				if allow_struct_init && self.at(&TokenKind::LeftBrace) {
					let checkpoint = self.lexer.clone();
					let checkpoint_span = self.last_span;
					let checkpoint_buffered = self.buffered_token.clone();

					self.next(); // {

					let is_struct: bool = self.at(&TokenKind::RightBrace)
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

				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::OneOf(vec![TokenKind::Comma, TokenKind::RightParen]),
					tok.kind,
					self.source_index,
				)));
			}

			TokenKind::LeftBracket => {
				self.next();

				if self.consume(&TokenKind::RightBracket) {
					return Ok(Expr::Array(ArrayLiteral::List {
						elements: Vec::new(),
						span: span.merge(&self.last_span),
					}));
				}

				let first: Expr = self.parse_expr()?;

				if self.consume(&TokenKind::Semicolon) {
					let count: Expr = self.parse_expr()?;
					self.expect(&TokenKind::RightBracket)?;
					return Ok(Expr::Array(ArrayLiteral::Repeat {
						value: Box::new(first),
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
					return Err(CompileError::ParseError(ParseError::unexpected_item(
						tok.span,
						"Expected a loop, only a loop can have a label and return a value",
						tok.kind,
						self.source_index,
					)));
				}
			}

			_ => {
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::Expression,
					tok.kind,
					self.source_index,
				)));
			}
		}
	}

	fn stmt_if_to_expr_wrapper(&self, stmt: Stmt) -> Result<Expr, CompileError>
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
							return Err(CompileError::ParseError(ParseError::generic(
								b.span(),
								"expected expression, block, or if statement in else branch",
								self.source_index,
							)));
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
							return Err(CompileError::ParseError(ParseError::generic(
								b.span(),
								"expected expression, block, or if statement in else branch",
								self.source_index,
							)));
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

	fn parse_argument_list(&mut self) -> Result<Vec<Expr>, CompileError>
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

	fn parse_struct_fields(&mut self) -> Result<Vec<(Ident, Expr)>, CompileError>
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
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					name_tok.span,
					Expected::Identifier,
					name_tok.kind,
					self.source_index,
				)));
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

	fn parse_switch_arm(&mut self) -> Result<SwitchArm, CompileError>
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

	fn parse_pattern(&mut self) -> Result<Pattern, CompileError>
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

	fn parse_pattern_no_or(&mut self) -> Result<Pattern, CompileError>
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

			TokenKind::DotDot | TokenKind::DotDotEquals => {
				let inclusive = self.at(&TokenKind::DotDotEquals);
				self.next(); // .. | ..=

				let end = if self.is_range_end() {
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

			TokenKind::Identifier(_) => {
				let path: Path = self.get_path()?;

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
								return Err(CompileError::ParseError(ParseError::unexpected_token(
									self.last_span,
									Expected::Identifier,
									self.peek().kind.clone(),
									self.source_index,
								)));
							};

							let pattern: Pattern = if self.consume(&TokenKind::Equals) {
								self.parse_pattern()?
							} else if self.consume(&TokenKind::Colon) {
								let ty: Type = self.parse_type()?;

								let call_constructor: Option<CallType> = if self.consume(&TokenKind::Bang) {
									self.expect(&TokenKind::LeftParen)?;
									self.expect(&TokenKind::RightParen)?;
									Some(CallType::UserHeap)
								} else if self.consume(&TokenKind::QuestionMark) {
									self.expect(&TokenKind::LeftParen)?;
									self.expect(&TokenKind::RightParen)?;
									Some(CallType::UserMaybeHeap)
								} else if self.consume(&TokenKind::LeftParen) {
									self.expect(&TokenKind::RightParen)?;
									Some(CallType::Regular)
								} else {
									None
								};

								Pattern::TypedIdentifier {
									path: Path::simple(vec![field_name.clone()], self.last_span),
									ty,
									call_constructor,
									span: self.last_span,
								}
							} else {
								return Err(CompileError::ParseError(ParseError::unexpected_token(
									self.last_span,
									Expected::OneOf(vec![TokenKind::Equals, TokenKind::Colon]),
									self.peek().kind.clone(),
									self.source_index,
								)));
							};

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
						return Err(CompileError::ParseError(ParseError::invalid_pattern(
							tok.span,
							"binding patterns must be simple identifiers, not paths",
							self.source_index,
						)));
					}

					self.next(); // :
					let ty: Type = self.parse_type()?;

					let call_constructor: Option<CallType> = if self.consume(&TokenKind::Bang) {
						self.expect(&TokenKind::LeftParen)?;
						self.expect(&TokenKind::RightParen)?;
						Some(CallType::UserHeap)
					} else if self.consume(&TokenKind::QuestionMark) {
						self.expect(&TokenKind::LeftParen)?;
						self.expect(&TokenKind::RightParen)?;
						Some(CallType::UserMaybeHeap)
					} else if self.consume(&TokenKind::LeftParen) {
						self.expect(&TokenKind::RightParen)?;
						Some(CallType::Regular)
					} else {
						None
					};

					return Ok(Pattern::TypedIdentifier {
						path,
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
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::Pattern,
					tok.kind,
					self.source_index,
				)));
			}
		}
	}

	fn parse_block(&mut self) -> Result<Block, CompileError>
	{
		self.expect(&TokenKind::LeftBrace)?;

		let ret: Block = self.parse_block_content()?;

		self.expect(&TokenKind::RightBrace)?;
		return Ok(ret);
	}

	fn parse_block_content(&mut self) -> Result<Block, CompileError>
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

			let kind: TokenKind = self.peek_kind().clone();

			match kind {
				TokenKind::Semicolon => {
					self.next(); // ;
				}
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
					let expr = self.parse_delete()?;
					stmts.push(Stmt::Delete {
						expr,
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
								let tok: Token = self.next();
								return Err(CompileError::ParseError(ParseError::unexpected_token(
									tok.span,
									Expected::OneOf(vec![TokenKind::Semicolon, TokenKind::RightBrace]),
									tok.kind,
									self.source_index,
								)));
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

	fn parse_assign_op(&mut self) -> Result<AssignOp, CompileError>
	{
		let op: AssignOp = match self.peek_kind() {
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
				let tok: Token = self.next();
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::Description("assignment operator".to_string()),
					tok.kind,
					self.source_index,
				)));
			}
		};
		self.next();
		return Ok(op);
	}

	fn parse_if(&mut self) -> Result<Stmt, CompileError>
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

	fn parse_while(&mut self) -> Result<Stmt, CompileError>
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

	fn parse_for(&mut self) -> Result<Stmt, CompileError>
	{
		let span: Span = self.peek().span();
		self.expect(&TokenKind::For)?;
		let name: Path = self.get_path()?;
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

	fn parse_loop(&mut self) -> Result<Stmt, CompileError>
	{
		let span: Span = self.peek().span();
		self.expect(&TokenKind::Loop)?;
		return Ok(Stmt::Loop {
			label: None,
			body: self.parse_block()?,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_function_decl(&mut self) -> Result<FunctionDecl, CompileError>
	{
		let mut span: Span = self.peek().span();
		let signature: FunctionSignature = self.parse_function_signature()?;
		let body: Option<Block> = if self.consume(&TokenKind::Semicolon) {
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

	fn parse_function_signature(&mut self) -> Result<FunctionSignature, CompileError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;

		self.expect(&TokenKind::FuncDef)?;

		let call_type = if self.consume(&TokenKind::Bang) {
			CallType::UserHeap
		} else if self.consume(&TokenKind::QuestionMark) {
			CallType::UserMaybeHeap
		} else {
			CallType::Regular
		};

		let heap_generics: Vec<GenericParam> = if call_type.is_heap_call() && self.at(&TokenKind::LessThan) {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let name: Path = if matches!(self.peek_kind(), TokenKind::Identifier(_)) {
			self.get_path()?
		} else if self.at(&TokenKind::Delete) {
			let tok = self.next();
			Path::simple(vec!["delete".to_string()], tok.span())
		} else {
			let tok: Token = self.next();
			return Err(CompileError::ParseError(ParseError::unexpected_token(
				tok.span,
				Expected::Identifier,
				tok.kind,
				self.source_index,
			)));
		};

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan) {
			self.get_generics()?
		} else {
			Vec::new()
		};
		let params: Vec<Param> = self.parse_function_arguments()?;

		let return_type: Option<Type> = if self.at(&TokenKind::Arrow) {
			self.next(); // ->
			Some(self.parse_type()?)
		} else {
			None
		};

		let where_clause: Vec<WhereConstraint> = if self.at(&TokenKind::Where) {
			self.next(); // where
			if !matches!(self.peek_kind(), TokenKind::Identifier(_)) {
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					self.peek().span(),
					Expected::Identifier,
					self.next().kind,
					self.source_index,
				)));
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

	fn parse_function_arguments(&mut self) -> Result<Vec<Param>, CompileError>
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

					let self_span: Span = self.expect(&TokenKind::SelfKw)?.span();

					let self_type: Type = Type {
						modifiers: Vec::new(),
						core: Box::new(TypeCore::Reference {
							mutable,
							inner: Box::new(TypeCore::Base {
								path: Path::simple(vec!["Self".to_string()], self_span),
								generics: Vec::new(),
							}),
						}),
						span: loop_span.merge(&self.last_span),
					};

					params.push(Param {
						ty: self_type,
						mutable,
						name: Path::simple(vec!["self".to_string()], self_span),
						span: loop_span.merge(&self.last_span),
					});
				}
				TokenKind::SelfKw => {
					let self_span: Span = self.next().span(); // self

					let self_type = Type {
						modifiers: Vec::new(),
						core: Box::new(TypeCore::Base {
							path: Path::simple(vec!["Self".to_string()], self_span),
							generics: Vec::new(),
						}),
						span: loop_span.merge(&self.last_span),
					};

					params.push(Param {
						ty: self_type,
						mutable: false,
						name: Path::simple(vec!["self".to_string()], self_span),
						span: loop_span.merge(&self.last_span),
					});
				}
				TokenKind::Mut => {
					let self_span: Span = self.next().span(); // mut
					self.expect(&TokenKind::SelfKw)?;

					let self_type = Type {
						modifiers: Vec::new(),
						core: Box::new(TypeCore::Base {
							path: Path::simple(vec!["Self".to_string()], self_span),
							generics: Vec::new(),
						}),
						span: loop_span.merge(&self.last_span),
					};

					params.push(Param {
						ty: self_type,
						mutable: false,
						name: Path::simple(vec!["self".to_string()], self_span),
						span: loop_span.merge(&self.last_span),
					});
				}

				_ => {
					let name: Path = self.get_path()?;

					self.expect(&TokenKind::Colon)?;

					let ty: Type = self.parse_type()?;

					params.push(Param {
						mutable: false,
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

	fn parse_modifiers(&mut self) -> Result<Vec<Modifier>, CompileError>
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
				TokenKind::Volatile => {
					ret.push(Modifier::Volatile);
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

	fn parse_struct(&mut self) -> Result<StructDecl, CompileError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Struct)?;

		let name: Path = if matches!(self.peek_kind(), TokenKind::Identifier(_)) {
			self.get_path()?
		} else {
			let tok: Token = self.next();
			return Err(CompileError::ParseError(ParseError::unexpected_token(
				tok.span,
				Expected::Identifier,
				tok.kind,
				self.source_index,
			)));
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
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::Identifier,
					tok.kind,
					self.source_index,
				)));
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

	fn parse_union(&mut self) -> Result<UnionDecl, CompileError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Union)?;

		let name: Path = if matches!(self.peek_kind(), TokenKind::Identifier(_)) {
			self.get_path()?
		} else {
			let tok: Token = self.next();
			return Err(CompileError::ParseError(ParseError::unexpected_token(
				tok.span,
				Expected::Identifier,
				tok.kind,
				self.source_index,
			)));
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
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::Identifier,
					tok.kind,
					self.source_index,
				)));
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

	fn parse_namespace(&mut self) -> Result<NamespaceDecl, CompileError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Namespace)?;
		let name: Path = self.get_path()?;
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

	fn parse_enum(&mut self) -> Result<EnumDecl, CompileError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Enum)?;

		let name: Path = if matches!(self.peek_kind(), TokenKind::Identifier(_)) {
			self.get_path()?
		} else {
			let tok: Token = self.next();
			return Err(CompileError::ParseError(ParseError::unexpected_token(
				tok.span,
				Expected::Identifier,
				tok.kind,
				self.source_index,
			)));
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
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::Identifier,
					tok.kind,
					self.source_index,
				)));
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

	fn parse_variant(&mut self) -> Result<VariantDecl, CompileError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Variant)?;

		let name: Path = if matches!(self.peek_kind(), TokenKind::Identifier(_)) {
			self.get_path()?
		} else {
			let tok: Token = self.next();
			return Err(CompileError::ParseError(ParseError::unexpected_token(
				tok.span,
				Expected::Identifier,
				tok.kind,
				self.source_index,
			)));
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
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::Identifier,
					tok.kind,
					self.source_index,
				)));
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

	fn parse_impl(&mut self) -> Result<ImplDecl, CompileError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Impl)?;

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan) {
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

	fn parse_impl_target(&mut self) -> Result<ImplTarget, CompileError>
	{
		let span: Span = self.peek().span();
		let path: Path = self.get_path()?;

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

	fn parse_type_generics(&mut self) -> Result<Vec<Type>, CompileError>
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
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::OneOf(vec![TokenKind::Comma, TokenKind::GreaterThan]),
					tok.kind,
					self.source_index,
				)));
			}

			if self.consume_greater_than() {
				break;
			}
		}

		return Ok(generics);
	}

	fn parse_named_generics(&mut self) -> Result<Vec<(Ident, Type)>, CompileError>
	{
		if !self.consume(&TokenKind::LessThan) {
			return Ok(Vec::new());
		}

		let mut named_generics: Vec<(Ident, Type)> = Vec::new();

		if self.consume_greater_than() {
			return Ok(named_generics);
		}

		loop {
			let name = if let TokenKind::Identifier(name) = self.next().kind {
				name
			} else {
				let tok: Token = self.peek().clone();
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::Identifier,
					tok.kind,
					self.source_index,
				)));
			};

			self.expect(&TokenKind::Colon)?;

			let ty: Type = self.parse_type()?;

			named_generics.push((name, ty));

			if self.consume_greater_than() {
				break;
			}

			if !self.consume(&TokenKind::Comma) {
				let tok = self.peek().clone();
				return Err(CompileError::ParseError(ParseError::unexpected_token(
					tok.span,
					Expected::OneOf(vec![TokenKind::Comma, TokenKind::GreaterThan]),
					tok.kind,
					self.source_index,
				)));
			}

			if self.consume_greater_than() {
				break;
			}
		}

		return Ok(named_generics);
	}

	fn parse_impl_item(&mut self) -> Result<ImplItem, CompileError>
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
				return Err(CompileError::ParseError(ParseError::unexpected_item(
					tok.span,
					"impl block",
					tok.kind,
					self.source_index,
				)));
			}
		};

		return Ok(node);
	}

	fn parse_where_clause(&mut self) -> Result<Vec<WhereConstraint>, CompileError>
	{
		let mut constraints: Vec<WhereConstraint> = Vec::new();

		loop {
			let loop_span: Span = self.peek().span();

			let ty: Path = self.get_path()?;

			let type_args: Vec<Type> = if self.at(&TokenKind::LessThan) {
				self.parse_type_generics()?
			} else {
				Vec::new()
			};

			self.expect(&TokenKind::Colon)?;

			let mut bounds: Vec<WhereBound> = Vec::new();

			loop {
				bounds.push(self.parse_where_bound()?);

				if !self.consume(&TokenKind::Plus) {
					break;
				}
			}

			constraints.push(WhereConstraint {
				ty,
				type_args,
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

	fn parse_type_alias(&mut self) -> Result<TypeAliasDecl, CompileError>
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

	fn parse_trait(&mut self) -> Result<TraitDecl, CompileError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Trait)?;

		let name: Path = self.get_path()?;

		let generics: Vec<GenericParam> = if self.at(&TokenKind::LessThan) {
			self.get_generics()?
		} else {
			Vec::new()
		};

		let super_traits: Vec<WhereBound> = if self.consume(&TokenKind::Colon) {
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

	fn parse_trait_bounds(&mut self) -> Result<Vec<WhereBound>, CompileError>
	{
		let mut bounds: Vec<WhereBound> = Vec::new();

		loop {
			let bound: WhereBound = self.parse_where_bound()?;
			bounds.push(bound);

			if !self.consume(&TokenKind::Plus) {
				break;
			}
		}

		return Ok(bounds);
	}

	fn parse_where_bound(&mut self) -> Result<WhereBound, CompileError>
	{
		let bound: WhereBound = if matches!(self.peek_kind(), TokenKind::Identifier(s) if *s == "Fn") {
			self.next(); // Fn
			let mut params: Vec<Type> = Vec::new();

			self.expect(&TokenKind::LeftParen)?;
			loop {
				if self.at(&TokenKind::RightParen) {
					break;
				}
				params.push(self.parse_type()?);
				if !self.consume(&TokenKind::Comma) {
					break;
				}
				if self.at(&TokenKind::RightParen) {
					break;
				}
			}
			self.expect(&TokenKind::RightParen)?;

			let return_type: Option<Type> = if self.at(&TokenKind::Arrow) {
				self.next(); // ->
				Some(self.parse_type()?)
			} else {
				None
			};
			WhereBound::Func(FuncBound::Fn {
				args: params,
				ret: return_type,
			})
		} else {
			let mut bound_path: Path = self.get_path()?;

			if self.at(&TokenKind::LessThan) {
				bound_path.generics.extend(self.parse_type_generics()?);
			}
			WhereBound::Path(bound_path)
		};

		return Ok(bound);
	}

	fn parse_trait_item(&mut self) -> Result<TraitItem, CompileError>
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
				return Err(CompileError::ParseError(ParseError::unexpected_item(
					tok.span,
					"trait block",
					tok.kind,
					self.source_index,
				)));
			}
		};

		return Ok(node);
	}

	fn parse_trait_type_alias(&mut self) -> Result<TypeAliasDecl, CompileError>
	{
		let span: Span = self.peek().span;
		let modifiers: Vec<Modifier> = self.parse_modifiers()?;
		self.expect(&TokenKind::Type)?;

		let name: Path = self.get_path()?;

		let ty: Type = if self.consume(&TokenKind::Equals) {
			self.parse_type()?
		} else {
			let tok: Token = self.next();
			return Err(CompileError::ParseError(ParseError::unexpected_token(
				tok.span(),
				Expected::Token(TokenKind::Equals),
				tok.kind,
				self.source_index,
			)));
		};

		return Ok(TypeAliasDecl {
			modifiers,
			name,
			ty,
			span: span.merge(&self.last_span),
		});
	}

	fn parse_delete(&mut self) -> Result<Expr, CompileError>
	{
		self.expect(&TokenKind::Delete)?;

		return self.parse_expr();
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

impl std::fmt::Display for Path
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		for (i, segment) in self.segments.iter().enumerate() {
			if i > 0 {
				write!(f, "::")?;
			}
			write!(f, "{}", segment)?;
		}

		if !self.generics.is_empty() {
			write!(f, "::<")?;
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

impl std::fmt::Display for Directive
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match self {
			Directive::Import(path) => return write!(f, "@import \"{}\"", path),
			Directive::Use(path) => {
				write!(f, "@use ")?;
				write!(f, "{}", path)?;
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
		if self.mutable {
			write!(f, "mut ")?;
		}
		return write!(f, "{}: {}", self.name, self.ty);
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
				return {
					write!(f, "{}[", inner)?;
					if let Some(s) = size {
						write!(f, "{}", s)?;
					}
					write!(f, "]")
				};
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
				path,
				ty,
				call_constructor,
				..
			} => {
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
			Pattern::Struct { path, fields, .. } => {
				write!(f, "{} {{", path)?;
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
					} else {
						return write!(f, "&{}", expr);
					}
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
			Expr::StructInit { path, fields, .. } => {
				write!(f, "{} {{", path)?;
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
			write!(f, "for {} in ", name)?;
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

	writeln!(f, "struct {} {{", s.name)?;
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

	writeln!(f, "union {} {{", u.name)?;
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

	writeln!(f, "enum {} {{", e.name)?;
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

	writeln!(f, "variant {} {{", v.name)?;
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

	return write!(f, "type {} = {}", t.name, t.ty);
}

fn write_namespace_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, n: &NamespaceDecl) -> fmt::Result
{
	for modifier in &n.modifiers {
		write!(f, "{} ", modifier)?;
	}

	writeln!(f, "namespace {} {{", n.name)?;
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
			WhereBound::Path(path) => {
				write!(f, "{}", path)
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
