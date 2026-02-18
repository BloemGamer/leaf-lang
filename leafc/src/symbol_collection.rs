mod tests;

use crate::{
	CompileDiagnostic, CompileError,
	desugar::DesugaredAST,
	lexer::{Span, Spanned},
	parser::{
		self, ArrayLiteral, Block, CallType, Directive, DirectiveNode, Expr, FunctionDecl, FunctionSignature, Ident,
		ImplDecl, ImplItem, Modifier, ModuleDecl, ModuleKind, Path, Pattern, RangeExpr, Stmt, StructDecl, SwitchBody,
		TopLevelBlock, TopLevelDecl, TraitDecl, TraitItem, VariableDecl,
	},
	source_map::SourceIndex,
};

/// Unique identifier for a scope in the symbol table.
///
/// Scopes represent lexical regions where symbols can be defined and looked up.
/// Each scope has a unique ID that can be used to index into the symbol table's
/// scope storage.
///
/// # Examples
/// ```ignore
/// let scope_id = ScopeId(0); // Root scope
/// let scope = symbol_table.scope(scope_id);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

/// Unique identifier for a symbol in the symbol table.
///
/// Symbols represent named entities like variables, functions, types, etc.
/// Each symbol has a unique ID that can be used to index into the symbol table's
/// symbol storage.
///
/// # Examples
/// ```ignore
/// let symbol_id = SymbolId(5);
/// let symbol = symbol_table.symbol(symbol_id);
/// println!("Symbol name: {}", symbol.name);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub usize);

/// The kind of symbol and its associated metadata.
///
/// Different symbol kinds represent different language constructs and may have
/// specific attributes. For example, variables track mutability, while functions
/// track whether they're compile-time constant.
///
/// # Variants
///
/// * `Variable { mutability }` - A variable binding (local or global)
/// * `Function { comp_const }` - A function declaration
/// * `Struct` - A structure type definition
/// * `Union` - An untagged union type
/// * `Enum` - A C-style enumeration
/// * `Variant` - A tagged union (Rust-style enum)
/// * `VariantMember` - A member of a tagged union variant
/// * `EnumVariant` - A variant in a C-style enum
/// * `TypeAlias` - A type alias declaration
/// * `GenericParam` - A generic type parameter
/// * `Trait` - A trait definition
/// * `Module` - A module declaration
/// * `Field` - A field in a struct or union
/// * `Label` - A loop label
///
/// # Examples
/// ```ignore
/// match symbol.kind {
///     SymbolKind::Variable { mutability: Mutability::Mutable } => {
///         println!("Mutable variable: {}", symbol.name);
///     }
///     SymbolKind::Function { comp_const: true } => {
///         println!("Compile-time constant function: {}", symbol.name);
///     }
///     _ => {}
/// }
/// ```
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind
{
	Variable
	{
		mutability: Mutability,
	},
	Function
	{
		comp_const: bool,
	},
	Struct,
	Union,
	Enum,
	Variant,
	VariantMember,
	EnumVariant,
	TypeAlias,
	GenericParam,
	Trait,
	Module,
	Field,
	Label,
}

/// A symbol in the program with its metadata.
///
/// Symbols represent named entities in the source code. Each symbol knows its name,
/// what kind of entity it is, where it's defined, which scope it belongs to, and
/// its visibility.
///
/// # Fields
///
/// * `name` - The identifier name of the symbol
/// * `kind` - What kind of symbol this is (variable, function, type, etc.)
/// * `def_span` - Source location where the symbol is defined
/// * `scope` - The scope this symbol belongs to
/// * `visibility` - Whether the symbol is public or private
///
/// # Examples
/// ```ignore
/// let symbol = symbol_table.symbol(symbol_id);
/// println!("Found {} at {:?}", symbol.name, symbol.def_span);
///
/// if symbol.visibility == Visibility::Public {
///     println!("Symbol is public");
/// }
/// ```
#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Symbol
{
	pub name: Ident,
	pub kind: SymbolKind,
	pub def_span: Span,
	pub scope: ScopeId,
	pub visibility: Visibility,
}

/// The kind of lexical scope.
///
/// Different scope kinds represent different language constructs that create
/// new scoping regions. Each scope kind has specific semantics about what
/// symbols it can contain and how name resolution works.
///
/// # Variants
///
/// * `ModuleImport` - Scope created by an import statement
/// * `ModuleInline` - Scope for an inline module definition
/// * `FunctionBody` - Scope for the body of a function
/// * `StructFields` - Scope containing struct field definitions
/// * `UnionFields` - Scope containing union field definitions
/// * `EnumVariants` - Scope containing enum variant definitions
/// * `VariantMembers` - Scope containing tagged union variant members
/// * `TraitBody` - Scope for trait items
/// * `ImplBody` - Scope for implementation block items
/// * `Block` - Scope for a general code block `{ }`
/// * `SwitchArm` - Scope for a single switch/match arm
/// * `IfThen` - Scope for the then-branch of an if statement
/// * `ElseBlock` - Scope for the else-branch of an if statement
/// * `LoopBody` - Scope for the body of an infinite loop
///
/// # Examples
/// ```ignore
/// let scope = symbol_table.scope(scope_id);
/// match scope.kind {
///     ScopeKind::FunctionBody => println!("In function body"),
///     ScopeKind::Block => println!("In regular block"),
///     _ => {}
/// }
/// ```
#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind
{
	ModuleImport,
	ModuleInline,
	FunctionBody,
	StructFields,
	UnionFields,
	EnumVariants,
	VariantMembers,
	TraitBody,
	ImplBody,
	Block,
	SwitchArm,
	IfThen,
	ElseBlock,
	LoopBody,
}

/// A lexical scope containing symbols and child scopes.
///
/// Scopes form a tree structure representing the lexical nesting of the program.
/// Each scope knows its parent scope, the symbols defined directly in it, and
/// any child scopes nested within it.
///
/// # Fields
///
/// * `kind` - What kind of scope this is
/// * `parent` - Parent scope ID (None for the root scope)
/// * `symbols` - Symbols defined directly in this scope
/// * `children` - Child scopes nested within this scope
/// * `span` - Source location covered by this scope
///
/// # Examples
/// ```ignore
/// let scope = symbol_table.scope(scope_id);
///
/// // Iterate through symbols in this scope
/// for &symbol_id in &scope.symbols {
///     let symbol = symbol_table.symbol(symbol_id);
///     println!("  {}", symbol.name);
/// }
///
/// // Check parent scope
/// if let Some(parent_id) = scope.parent {
///     let parent = symbol_table.scope(parent_id);
///     println!("Parent scope: {:?}", parent.kind);
/// }
/// ```
#[derive(Debug, Clone)]
#[allow(unused)]
pub struct Scope
{
	pub kind: ScopeKind,
	pub parent: Option<ScopeId>,
	pub symbols: Vec<SymbolId>,
	pub children: Vec<ScopeId>,
	pub span: Span,
}

/// The complete symbol table for a program.
///
/// Contains all scopes and symbols collected from the program's AST.
/// Provides methods to look up scopes and symbols by their IDs.
/// The symbol table forms a tree structure with scopes as nodes.
///
/// # Fields
///
/// * `scopes` - All scopes in the program
/// * `symbols` - All symbols in the program
/// * `root` - The root scope ID (typically the module scope)
///
/// # Examples
/// ```ignore
/// use crate::symbol_collection::collect_symbols;
///
/// let symbol_table = collect_symbols(&program, source_index)?;
///
/// // Access root scope
/// let root = symbol_table.scope(symbol_table.root);
/// println!("Root has {} symbols", root.symbols.len());
///
/// // Look up a symbol
/// for &symbol_id in &root.symbols {
///     let symbol = symbol_table.symbol(symbol_id);
///     println!("Top-level symbol: {}", symbol.name);
/// }
/// ```
#[derive(Debug, Clone)]
pub struct SymbolTable
{
	pub scopes: Vec<Scope>,
	pub symbols: Vec<Symbol>,
	pub root: ScopeId,
}

impl SymbolTable
{
	/// Gets a reference to a scope by its ID.
	///
	/// # Arguments
	/// * `id` - The scope ID to look up
	///
	/// # Returns
	/// A reference to the scope
	///
	/// # Panics
	/// Panics if the scope ID is invalid
	///
	/// # Examples
	/// ```ignore
	/// let scope = symbol_table.scope(ScopeId(0));
	/// println!("Scope kind: {:?}", scope.kind);
	/// ```
	#[allow(unused)]
	pub fn scope(&self, id: ScopeId) -> &Scope
	{
		return &self.scopes[id.0];
	}

	/// Gets a reference to a symbol by its ID.
	///
	/// # Arguments
	/// * `id` - The symbol ID to look up
	///
	/// # Returns
	/// A reference to the symbol
	///
	/// # Panics
	/// Panics if the symbol ID is invalid
	///
	/// # Examples
	/// ```ignore
	/// let symbol = symbol_table.symbol(SymbolId(5));
	/// println!("Symbol: {} ({:?})", symbol.name, symbol.kind);
	/// ```
	#[allow(unused)]
	pub fn symbol(&self, id: SymbolId) -> &Symbol
	{
		return &self.symbols[id.0];
	}
}

/// Symbol visibility level.
///
/// Determines whether a symbol can be accessed from outside its defining module.
/// This is typically controlled by the `pub` keyword in the source code.
///
/// # Variants
///
/// * `Public` - Symbol is visible outside its module (marked with `pub`)
/// * `Private` - Symbol is only visible within its module (default)
///
/// # Examples
/// ```ignore
/// if symbol.visibility == Visibility::Public {
///     println!("{} is exported from this module", symbol.name);
/// } else {
///     println!("{} is internal to this module", symbol.name);
/// }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility
{
	Public,
	Private,
}

/// Mutability level of a variable.
///
/// Variables can be immutable (default), mutable (can be reassigned), or
/// compile-time constant (evaluated at compile time).
///
/// # Variants
///
/// * `Mutable` - Variable can be reassigned (`var mut x`)
/// * `Const` - Compile-time constant (`const X`)
/// * `Immutable` - Cannot be reassigned (`var x`)
///
/// # Examples
/// ```ignore
/// match symbol.kind {
///     SymbolKind::Variable { mutability: Mutability::Mutable } => {
///         println!("Can reassign {}", symbol.name);
///     }
///     SymbolKind::Variable { mutability: Mutability::Const } => {
///         println!("{} is a compile-time constant", symbol.name);
///     }
///     SymbolKind::Variable { mutability: Mutability::Immutable } => {
///         println!("{} cannot be reassigned", symbol.name);
///     }
///     _ => {}
/// }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability
{
	Mutable,
	Const,
	Immutable,
}

/// Error that occurred during symbol collection.
///
/// Contains information about what went wrong, where it happened, and context
/// about what was being processed at the time.
///
/// # Fields
///
/// * `span` - Source location where the error occurred
/// * `kind` - The specific kind of error
/// * `context` - Stack of processing contexts
/// * `source_index` - Index into the source map
/// * `scope` - The scope being processed when the error occurred
///
/// # Examples
/// ```ignore
/// match collect_symbols(&program, source_index) {
///     Ok(table) => { /* use table */ }
///     Err(error) => {
///         eprintln!("Symbol collection failed: {}", error);
///         eprintln!("At: {:?}", error.span);
///     }
/// }
/// ```
#[allow(unused)]
#[derive(Debug, Clone)]
pub struct SymbolCollectionError
{
	pub span: Span,
	pub kind: SymbolCollectionErrorKind,
	pub context: Vec<String>,
	pub source_index: SourceIndex,
	pub scope: ScopeId,
}

impl Spanned for SymbolCollectionError
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

impl From<SymbolCollectionError> for CompileError
{
	fn from(val: SymbolCollectionError) -> Self
	{
		return CompileError::SymbolCollectionError(val);
	}
}

impl CompileDiagnostic for SymbolCollectionError
{
	fn fmt_with_source(&self, f: &mut impl std::fmt::Write, sm: &crate::source_map::SourceMap) -> std::fmt::Result
	{
		return write!(
			f,
			"{}",
			self.span
				.format_error(&sm.get(self.source_index).src, &format!("{self}"))
		);
	}
}

/// The specific kind of symbol collection error.
///
/// Categorizes different types of errors that can occur during symbol collection,
/// such as duplicate definitions or invalid paths in declarations.
///
/// # Variants
///
/// * `DuplicateDefinition { name, first_definition }` - A symbol is defined twice in the same scope
/// * `InvalidPath { declaration_type, reason }` - A declaration uses an invalid path
/// * `Generic { message }` - A generic error with a custom message
///
/// # Examples
/// ```ignore
/// match error.kind {
///     SymbolCollectionErrorKind::DuplicateDefinition { ref name, first_definition } => {
///         eprintln!("Symbol '{}' is already defined at {:?}", name, first_definition);
///     }
///     SymbolCollectionErrorKind::InvalidPath { ref declaration_type, .. } => {
///         eprintln!("Invalid path in {} declaration", declaration_type);
///     }
///     SymbolCollectionErrorKind::Generic { ref message } => {
///         eprintln!("Error: {}", message);
///     }
/// }
/// ```
#[derive(Debug, Clone)]
pub enum SymbolCollectionErrorKind
{
	DuplicateDefinition
	{
		name: String, first_definition: Span
	},
	InvalidPath
	{
		declaration_type: String,
		reason: PathErrorReason,
	},
	Generic
	{
		message: String
	},
}

/// Reason why a path is invalid in a declaration.
///
/// Some declarations (like functions and structs) must use simple single-segment
/// paths without generics. This enum explains why a given path is invalid.
///
/// # Variants
///
/// * `MultipleSegments` - Path has multiple segments (e.g., `foo::bar`) where only one is allowed
/// * `HasGenerics` - Path has generic arguments (e.g., `Foo<T>`) where none are allowed
///
/// # Examples
/// ```ignore
/// match reason {
///     PathErrorReason::MultipleSegments => {
///         eprintln!("Declaration must use a simple name, not a path");
///     }
///     PathErrorReason::HasGenerics => {
///         eprintln!("Declaration name cannot have generic arguments");
///     }
/// }
/// ```
#[derive(Debug, Clone)]
pub enum PathErrorReason
{
	MultipleSegments,
	HasGenerics,
}

impl std::fmt::Display for SymbolCollectionError
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match &self.kind {
			SymbolCollectionErrorKind::DuplicateDefinition { name, first_definition } => {
				return write!(
					f,
					"duplicate definition of `{}`: first at {:?}, again at {:?}",
					name,
					first_definition,
					self.span()
				);
			}
			SymbolCollectionErrorKind::InvalidPath {
				declaration_type,
				reason,
			} => {
				let reason_str = match reason {
					PathErrorReason::MultipleSegments => {
						format!("A {} can't be a path, and only can have one segment", declaration_type)
					}
					PathErrorReason::HasGenerics => {
						format!("A {} can't have a generic in the path", declaration_type)
					}
				};
				return write!(f, "{}", reason_str);
			}
			SymbolCollectionErrorKind::Generic { message } => {
				return write!(f, "{}", message);
			}
		}
	}
}

impl std::error::Error for SymbolCollectionError {}

struct Collector
{
	table: SymbolTable,
	current_scope: ScopeId,
	source_index: SourceIndex,
}

impl Collector
{
	fn new(source_index: SourceIndex) -> Self
	{
		let root = Scope {
			kind: ScopeKind::ModuleInline,
			parent: None,
			symbols: Vec::new(),
			children: Vec::new(),
			span: Span::default(),
		};

		return Self {
			table: SymbolTable {
				scopes: vec![root],
				symbols: Vec::new(),
				root: ScopeId(0),
			},
			current_scope: ScopeId(0),
			source_index,
		};
	}

	fn alloc_scope(&mut self, kind: ScopeKind, span: Span) -> ScopeId
	{
		let id: ScopeId = ScopeId(self.table.scopes.len());
		let parent: ScopeId = self.current_scope;

		self.table.scopes.push(Scope {
			kind,
			parent: Some(parent),
			symbols: Vec::new(),
			children: Vec::new(),
			span,
		});
		self.table.scopes[parent.0].children.push(id);
		return id;
	}

	#[allow(unused)]
	fn alloc_scope_under(&mut self, parent: ScopeId, kind: ScopeKind, span: Span) -> ScopeId
	{
		let id: ScopeId = ScopeId(self.table.scopes.len());

		self.table.scopes.push(Scope {
			kind,
			parent: Some(parent),
			symbols: Vec::new(),
			children: Vec::new(),
			span,
		});
		self.table.scopes[parent.0].children.push(id);
		return id;
	}

	fn insert_symbol(
		&mut self,
		scope: ScopeId,
		name: Ident,
		kind: SymbolKind,
		def_span: Span,
		visibility: Visibility,
	) -> Result<SymbolId, SymbolCollectionError>
	{
		if !matches!(kind, SymbolKind::Label) {
			for &sid in &self.table.scopes[scope.0].symbols {
				let existing: &Symbol = &self.table.symbols[sid.0];
				if existing.name == name && !matches!(existing.kind, SymbolKind::Label) {
					return Err(SymbolCollectionError {
						span: def_span,
						source_index: self.source_index,
						context: Vec::new(),
						kind: SymbolCollectionErrorKind::DuplicateDefinition {
							name,
							first_definition: existing.def_span,
						},
						scope,
					});
				}
			}
		}

		let id: SymbolId = SymbolId(self.table.symbols.len());
		self.table.symbols.push(Symbol {
			name,
			kind,
			def_span,
			scope,
			visibility,
		});
		self.table.scopes[scope.0].symbols.push(id);
		return Ok(id);
	}

	fn define(
		&mut self,
		name: Ident,
		kind: SymbolKind,
		def_span: Span,
		visibility: Visibility,
	) -> Result<SymbolId, SymbolCollectionError>
	{
		let scope = self.current_scope;
		return self.insert_symbol(scope, name, kind, def_span, visibility);
	}

	fn in_scope<F, R>(&mut self, scope: ScopeId, f: F) -> Result<R, SymbolCollectionError>
	where
		F: FnOnce(&mut Self) -> Result<R, SymbolCollectionError>,
	{
		let prev: ScopeId = self.current_scope;
		self.current_scope = scope;
		let result: Result<R, SymbolCollectionError> = f(self);
		self.current_scope = prev;
		return result;
	}

	fn validate_simple_path(&self, path: &Path, declaration_type: &str) -> Result<(), SymbolCollectionError>
	{
		if path.len() != 1 {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::InvalidPath {
					declaration_type: declaration_type.to_string(),
					reason: PathErrorReason::MultipleSegments,
				},
			});
		}
		if !path.segments[0].generics.is_empty() {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::InvalidPath {
					declaration_type: declaration_type.to_string(),
					reason: PathErrorReason::HasGenerics,
				},
			});
		}
		return Ok(());
	}

	fn collect_program(&mut self, program: &DesugaredAST) -> Result<(), SymbolCollectionError>
	{
		self.collect_top_level_block(&program.top_level_block)?;
		return Ok(());
	}

	fn collect_top_level_block(&mut self, top_level_block: &TopLevelBlock) -> Result<(), SymbolCollectionError>
	{
		for item in &top_level_block.items {
			self.collect_top_level_decl(item)?;
		}
		return Ok(());
	}

	fn collect_top_level_decl(&mut self, decl: &TopLevelDecl) -> Result<(), SymbolCollectionError>
	{
		return match decl {
			TopLevelDecl::Function(f) => self.collect_function_decl(f),
			TopLevelDecl::VariableDecl(v) => self.collect_variable_decl(v),
			TopLevelDecl::Struct(s) => self.collect_struct_decl(s),
			TopLevelDecl::Union(u) => self.collect_union_decl(u),
			TopLevelDecl::Enum(e) => self.collect_enum_decl(e),
			TopLevelDecl::Variant(v) => self.collect_variant_decl(v),
			TopLevelDecl::TypeAlias(t) => self.collect_type_alias_decl(t),
			TopLevelDecl::Trait(t) => self.collect_trait_decl(t),
			TopLevelDecl::Module(n) => self.collect_module_decl(n),
			TopLevelDecl::Impl(i) => self.collect_impl_decl(i),
			TopLevelDecl::Directive(d) => self.collect_directive(d),
		};
	}

	fn collect_block(&mut self, block: &Block) -> Result<(), SymbolCollectionError>
	{
		for item in &block.stmts {
			self.collect_stmt(item)?;
		}
		return Ok(());
	}

	fn collect_function_decl(&mut self, func: &FunctionDecl) -> Result<(), SymbolCollectionError>
	{
		let sig: &FunctionSignature = &func.signature;

		if sig.name.segments.len() != 1 {
			return Err(SymbolCollectionError {
				span: sig.name.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A function signature can't be a path, and only can have one segment".to_string(),
				},
			});
		}
		if let Some(name) = sig.name.segments.first() {
			self.define(
				name.name.clone(),
				SymbolKind::Function {
					comp_const: sig.modifiers.iter().any(|m| matches!(m, Modifier::Const)),
				},
				sig.span(),
				get_visability(&func.signature.modifiers),
			)?;
		} else {
			unreachable!("A signature should always have a segment, otherwise the parser did not do his job right");
		}

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::FunctionBody, func.span());

		self.in_scope(body_scope, |c| {
			for generic in &sig.generics {
				c.define(
					generic.name.clone(),
					SymbolKind::GenericParam,
					generic.span(),
					Visibility::Private,
				)?;
			}

			if sig.call_type != CallType::Regular {
				for ge in ["IO", "Alloc"].iter() {
					// TODO: maybe extract this one to a global variable, but not for now
					c.define(
						ge.to_string(),
						SymbolKind::GenericParam,
						sig.heap_generics
							.iter()
							.find(|g| return g.name == *ge)
							.map_or_else(|| return sig.span(), |g| return g.span()),
						Visibility::Private,
					)?;
				}
			} else {
				debug_assert!(
					sig.heap_generics.is_empty(),
					"If the calltype is regular, there should not be any heap_generics",
				);
			}

			for param in &sig.params {
				let Pattern::TypedIdentifier { path, span, .. } = &param.pattern else {
					unreachable!("Desugarer should have handled this");
				};
				c.validate_simple_path(path, "function")?;

				let Pattern::TypedIdentifier { mutable, .. } = param.pattern else {
					unreachable!("Should be handeled by the desugarer");
				};
				c.define(
					path.segments[0].name.clone(),
					SymbolKind::Variable {
						mutability: (if mutable {
							Mutability::Mutable
						} else {
							Mutability::Immutable
						}),
					},
					*span,
					Visibility::Private,
				)?;
			}

			if let Some(body) = &func.body {
				c.collect_block(body)?;
			}

			return Ok(());
		})?;

		return Ok(());
	}

	fn collect_variable_decl(&mut self, var: &VariableDecl) -> Result<(), SymbolCollectionError>
	{
		let Pattern::TypedIdentifier {
			path, span, modifiers, ..
		} = &var.pattern
		else {
			unreachable!("Desugarer should have handled this");
		};

		self.validate_simple_path(path, "variable")?;

		let Pattern::TypedIdentifier { mutable, .. } = var.pattern else {
			unreachable!("Should be handeled by the desugarer");
		};
		self.define(
			path.segments[0].name.clone(),
			SymbolKind::Variable {
				mutability: if var.comp_const {
					if mutable {
						todo!("const can't be mixed with const")
					}
					Mutability::Const
				} else if mutable {
					Mutability::Mutable
				} else {
					Mutability::Immutable
				},
			},
			*span,
			get_visability(modifiers),
		)?;

		return Ok(());
	}

	fn collect_struct_decl(&mut self, s: &StructDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &s.name;
		self.validate_simple_path(path, "struct")?;

		self.define(
			path.segments[0].name.clone(),
			SymbolKind::Struct,
			s.span(),
			get_visability(&s.modifiers),
		)?;

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::StructFields, s.span());

		self.in_scope(body_scope, |c| {
			for f in &s.fields {
				c.define(
					f.name.clone(),
					SymbolKind::Field,
					f.span(),
					get_visability(&f.modifiers),
				)?;
			}
			return Ok(());
		})?;
		return Ok(());
	}

	fn collect_union_decl(&mut self, u: &parser::UnionDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &u.name;
		self.validate_simple_path(path, "union")?;

		self.define(
			path.segments[0].name.clone(),
			SymbolKind::Union,
			u.span(),
			get_visability(&u.modifiers),
		)?;

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::UnionFields, u.span());

		self.in_scope(body_scope, |c| {
			for f in &u.fields {
				c.define(
					f.name.clone(),
					SymbolKind::Field,
					f.span(),
					get_visability(&f.modifiers),
				)?;
			}
			return Ok(());
		})?;
		return Ok(());
	}

	fn collect_enum_decl(&mut self, e: &parser::EnumDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &e.name;
		self.validate_simple_path(path, "enum")?;

		let visibility: Visibility = get_visability(&e.modifiers);
		self.define(path.segments[0].name.clone(), SymbolKind::Enum, e.span(), visibility)?;

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::EnumVariants, e.span());

		self.in_scope(body_scope, |c| {
			for f in &e.variants {
				c.define(f.name.clone(), SymbolKind::EnumVariant, f.span(), visibility)?;
			}
			return Ok(());
		})?;
		return Ok(());
	}

	fn collect_variant_decl(&mut self, v: &parser::VariantDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &v.name;
		self.validate_simple_path(path, "variant")?;

		let visibility: Visibility = get_visability(&v.modifiers);
		self.define(path.segments[0].name.clone(), SymbolKind::Variant, v.span(), visibility)?;

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::VariantMembers, v.span());

		self.in_scope(body_scope, |c| {
			for f in &v.variants {
				c.define(f.name.clone(), SymbolKind::VariantMember, f.span(), visibility)?;
			}
			return Ok(());
		})?;
		return Ok(());
	}

	fn collect_type_alias_decl(&mut self, t: &parser::TypeAliasDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &t.name;
		self.validate_simple_path(path, "type")?;

		self.define(
			path.segments[0].name.clone(),
			SymbolKind::TypeAlias,
			path.span(),
			get_visability(&t.modifiers),
		)?;

		return Ok(());
	}

	fn collect_trait_decl(&mut self, t: &TraitDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &t.name;
		self.validate_simple_path(path, "trait")?;

		self.define(
			path.segments[0].name.clone(),
			SymbolKind::Trait,
			path.span(),
			get_visability(&t.modifiers),
		)?;

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::TraitBody, t.span());
		self.in_scope(body_scope, |c| {
			for generic in &t.generics {
				c.define(
					generic.name.clone(),
					SymbolKind::GenericParam,
					generic.span(),
					Visibility::Private,
				)?;
			}

			for item in &t.items {
				match item {
					TraitItem::TypeAlias(ty) => c.collect_type_alias_decl(ty),
					TraitItem::Function(func) => c.collect_function_decl(func),
					TraitItem::Const(var) => c.collect_variable_decl(var),
				}?;
			}

			return Ok(());
		})?;
		return Ok(());
	}

	fn collect_module_decl(&mut self, m: &ModuleDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &m.name;
		self.validate_simple_path(path, "module")?;

		self.define(
			path.segments[0].name.clone(),
			SymbolKind::Module,
			path.span(),
			get_visability(&m.modifiers),
		)?;

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::ModuleInline, m.span());
		self.in_scope(body_scope, |c| {
			match &m.kind {
				ModuleKind::Inline(body) => c.collect_top_level_block(body)?,
				ModuleKind::External => {}
			}
			return Ok(());
		})?;
		return Ok(());
	}

	fn collect_impl_decl(&mut self, i: &ImplDecl) -> Result<(), SymbolCollectionError>
	{
		let body_scope: ScopeId = self.alloc_scope(ScopeKind::ImplBody, i.span());

		self.in_scope(body_scope, |c| {
			for generic in &i.generics {
				c.define(
					generic.name.clone(),
					SymbolKind::GenericParam,
					generic.span(),
					Visibility::Private,
				)?;
			}

			for item in &i.body {
				match item {
					ImplItem::TypeAlias(ty) => c.collect_type_alias_decl(ty),
					ImplItem::Function(func) => c.collect_function_decl(func),
					ImplItem::Const(var) => c.collect_variable_decl(var),
				}?;
			}

			return Ok(());
		})?;

		return Ok(());
	}

	fn collect_stmt(&mut self, stmt: &Stmt) -> Result<(), SymbolCollectionError>
	{
		match stmt {
			Stmt::For { .. } | Stmt::While { .. } | Stmt::WhileVarLoop { .. } | Stmt::IfVar { .. } => {
				unreachable!("this should be filtered out by the desugarer")
			}
			Stmt::Directive(d) => {
				self.collect_directive(d)?;
			}
			Stmt::Continue { .. } => {}
			Stmt::Loop {
				label: labelv,
				body,
				span,
			} => {
				let Some(label): Option<&String> = labelv.as_ref() else {
					unreachable!("desugarer should have added a label")
				};
				self.define(label.clone(), SymbolKind::Label, *span, Visibility::Private)?;

				let body_scope: ScopeId = self.alloc_scope(ScopeKind::LoopBody, *span);

				self.in_scope(body_scope, |c| {
					for item in &body.stmts {
						c.collect_stmt(item)?;
					}
					if let Some(expr) = &body.tail_expr {
						c.collect_expr(expr)?;
					}

					return Ok(());
				})?;
			}
			Stmt::Block(block) | Stmt::Unsafe(block) => {
				let body_scope: ScopeId = self.alloc_scope(ScopeKind::Block, block.span());
				self.in_scope(body_scope, |c| {
					for item in &block.stmts {
						c.collect_stmt(item)?;
					}
					if let Some(expr) = &block.tail_expr {
						c.collect_expr(expr)?;
					}

					return Ok(());
				})?;
			}
			Stmt::VariableDecl(var) => self.collect_variable_decl(var)?,
			Stmt::Assignment {
				target,
				op: _,
				value,
				span: _,
			} => {
				self.collect_expr(target)?;
				self.collect_expr(value)?;
			}
			Stmt::Delete { expr, span: _ } | Stmt::Expr(expr) => {
				self.collect_expr(expr)?;
			}
			Stmt::Return { value, span: _ } => {
				if let Some(expr) = value {
					self.collect_expr(expr)?;
				}
			}
			Stmt::Break { label, value, span: _ } => {
				debug_assert!(label.is_some());

				if let Some(expr) = value {
					self.collect_expr(expr)?;
				}
			}
			Stmt::If {
				cond,
				then_block,
				else_branch,
				span: _,
			} => {
				self.collect_expr(cond)?;

				let then_scope: ScopeId = self.alloc_scope(ScopeKind::IfThen, then_block.span());
				self.in_scope(then_scope, |c| {
					for item in &then_block.stmts {
						c.collect_stmt(item)?;
					}
					if let Some(expr) = &then_block.tail_expr {
						c.collect_expr(expr)?;
					}
					return Ok(());
				})?;

				if let Some(el) = else_branch {
					let else_scope: ScopeId = self.alloc_scope(ScopeKind::ElseBlock, el.span());
					match &**el {
						Stmt::Block(block) => {
							self.in_scope(else_scope, |c| {
								for item in &block.stmts {
									c.collect_stmt(item)?;
								}
								if let Some(expr) = &block.tail_expr {
									c.collect_expr(expr)?;
								}
								return Ok(());
							})?;
						}
						Stmt::If { .. } => self.in_scope(else_scope, |c| return c.collect_stmt(el))?,

						_ => unreachable!(),
					}
				}
			}
		}
		return Ok(());
	}

	fn collect_expr(&mut self, expr: &Expr) -> Result<(), SymbolCollectionError>
	{
		match expr {
			Expr::Identifier { .. } | Expr::Literal { .. } | Expr::Default { .. } => {}

			Expr::Unary { expr, .. } | Expr::Cast { expr, .. } => {
				self.collect_expr(expr)?;
			}

			Expr::Binary { lhs, rhs, .. } => {
				self.collect_expr(lhs)?;
				self.collect_expr(rhs)?;
			}

			Expr::Call { callee, args, .. } => {
				self.collect_expr(callee)?;
				for arg in args {
					self.collect_expr(arg)?;
				}
			}

			Expr::Field { base, .. } => {
				self.collect_expr(base)?;
			}

			Expr::Index { base, index, .. } => {
				self.collect_expr(base)?;
				self.collect_expr(index)?;
			}

			Expr::Range(RangeExpr { start, end, .. }) => {
				if let Some(s) = start {
					self.collect_expr(s)?;
				}
				if let Some(e) = end {
					self.collect_expr(e)?;
				}
			}

			Expr::Tuple { elements, .. } => {
				for elem in elements {
					self.collect_expr(elem)?;
				}
			}

			Expr::Array(array_lit) => match array_lit {
				ArrayLiteral::List { elements, .. } => {
					for elem in elements {
						self.collect_expr(elem)?;
					}
				}
				ArrayLiteral::Repeat { value, count, .. } => {
					self.collect_expr(value)?;
					self.collect_expr(count)?;
				}
			},

			Expr::StructInit { fields, base, .. } => {
				for (_, field_expr) in fields {
					self.collect_expr(field_expr)?;
				}
				if let Some(base_expr) = base {
					self.collect_expr(base_expr)?;
				}
			}

			Expr::Block(block) | Expr::UnsafeBlock(block) => {
				let block_scope = self.alloc_scope(ScopeKind::Block, block.span());
				self.in_scope(block_scope, |c| {
					for stmt in &block.stmts {
						c.collect_stmt(stmt)?;
					}
					if let Some(tail) = &block.tail_expr {
						c.collect_expr(tail)?;
					}
					return Ok(());
				})?;
			}

			Expr::Switch { expr, arms, .. } => {
				self.collect_expr(expr)?;

				for arm in arms {
					let arm_scope = self.alloc_scope(ScopeKind::SwitchArm, arm.span());
					self.in_scope(arm_scope, |c| {
						c.collect_pattern_bindings(&arm.pattern)?;

						match &arm.body {
							SwitchBody::Expr(e) => c.collect_expr(e)?,
							SwitchBody::Block(block) => {
								for stmt in &block.stmts {
									c.collect_stmt(stmt)?;
								}
								if let Some(tail) = &block.tail_expr {
									c.collect_expr(tail)?;
								}
							}
						}
						return Ok(());
					})?;
				}
			}

			Expr::If {
				cond,
				then_block,
				else_branch,
				..
			} => {
				self.collect_expr(cond)?;

				let then_scope = self.alloc_scope(ScopeKind::IfThen, then_block.span());
				self.in_scope(then_scope, |c| {
					for stmt in &then_block.stmts {
						c.collect_stmt(stmt)?;
					}
					if let Some(tail) = &then_block.tail_expr {
						c.collect_expr(tail)?;
					}
					return Ok(());
				})?;

				if let Some(else_expr) = else_branch {
					let else_scope = self.alloc_scope(ScopeKind::ElseBlock, else_expr.span());
					self.in_scope(else_scope, |c| return c.collect_expr(else_expr))?;
				}
			}

			Expr::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
				..
			} => {
				self.collect_expr(expr)?;

				let then_scope = self.alloc_scope(ScopeKind::IfThen, then_block.span());
				self.in_scope(then_scope, |c| {
					c.collect_pattern_bindings(pattern)?;

					for stmt in &then_block.stmts {
						c.collect_stmt(stmt)?;
					}
					if let Some(tail) = &then_block.tail_expr {
						c.collect_expr(tail)?;
					}
					return Ok(());
				})?;

				if let Some(else_expr) = else_branch {
					let else_scope = self.alloc_scope(ScopeKind::ElseBlock, else_expr.span());
					self.in_scope(else_scope, |c| return c.collect_expr(else_expr))?;
				}
			}

			Expr::Loop { label, body, span } => {
				let Some(label_str) = label else {
					unreachable!("desugarer should have added a label")
				};

				self.define(label_str.clone(), SymbolKind::Label, *span, Visibility::Private)?;

				let loop_scope = self.alloc_scope(ScopeKind::LoopBody, body.span());
				self.in_scope(loop_scope, |c| {
					for stmt in &body.stmts {
						c.collect_stmt(stmt)?;
					}
					if let Some(tail) = &body.tail_expr {
						c.collect_expr(tail)?;
					}
					return Ok(());
				})?;
			}
		}

		return Ok(());
	}

	fn collect_pattern_bindings(&mut self, pattern: &parser::Pattern) -> Result<(), SymbolCollectionError>
	{
		match pattern {
			Pattern::Wildcard { .. } | Pattern::Literal { .. } => {}

			Pattern::TypedIdentifier {
				path,
				mutable,
				span,
				modifiers,
				ty: _,
				call_constructor: _,
			} => {
				self.validate_simple_path(path, "pattern")?;

				self.define(
					path.segments[0].name.clone(),
					SymbolKind::Variable {
						mutability: if *mutable {
							Mutability::Mutable
						} else {
							Mutability::Immutable
						},
					},
					*span,
					get_visability(modifiers),
				)?;
			}

			Pattern::Variant { args, path: _, span: _ } => {
				for arg_pattern in args {
					self.collect_pattern_bindings(arg_pattern)?;
				}
			}

			Pattern::Tuple { patterns, span: _ } | Pattern::Or { patterns, span: _ } => {
				for pat in patterns {
					self.collect_pattern_bindings(pat)?;
				}
			}

			Pattern::Struct {
				fields,
				span: _,
				path: _,
				has_rest: _,
			} => {
				for (_, field_pattern) in fields {
					self.collect_pattern_bindings(field_pattern)?;
				}
			}

			Pattern::Range(RangeExpr {
				start,
				end,
				span: _,
				inclusive: _,
			}) => {
				if let Some(start_expr) = start {
					self.collect_expr(start_expr)?;
				}
				if let Some(end_expr) = end {
					self.collect_expr(end_expr)?;
				}
			}
		}

		return Ok(());
	}

	fn collect_directive(&mut self, directive: &DirectiveNode) -> Result<(), SymbolCollectionError>
	{
		let dir = &directive.directive;
		match dir {
			Directive::Use { .. }
			| Directive::Import { .. }
			| Directive::ValidateStructPattern {
				struct_path: _,
				pattern_fields: _,
				has_rest: _,
			} => {}
			Directive::ValidateType { ty: _, expr } => {
				self.collect_expr(expr)?;
			}
			Directive::Custom { .. } => unimplemented!("For now, directives are not stable for standalone things"),
		}
		return Ok(());
	}
}

/// Collects all symbols from a program AST into a symbol table.
///
/// This is the main entry point for symbol collection. It traverses the entire
/// program AST and builds a complete symbol table containing all scopes and
/// symbols, along with their relationships.
///
/// The symbol table provides:
/// - All named entities (functions, variables, types, etc.)
/// - Scope hierarchy (which scopes contain which symbols)
/// - Symbol metadata (visibility, mutability, etc.)
/// - Source locations for all definitions
///
/// # Arguments
///
/// * `program` - The program AST to collect symbols from
/// * `source_index` - Index into the source map for error reporting
///
/// # Returns
///
/// * `Ok(SymbolTable)` - Complete symbol table for the program
/// * `Err(SymbolCollectionError)` - If an error occurs (e.g., duplicate definitions)
///
/// # Errors
///
/// Returns an error if:
/// - A symbol is defined multiple times in the same scope
/// - A declaration uses an invalid path (multiple segments or generics where not allowed)
/// - Other structural issues are detected
///
/// # Examples
///
/// ```ignore
/// use crate::symbol_collection::collect_symbols;
/// use crate::source_map::SourceIndex;
///
/// // After parsing and desugaring
/// let symbol_table = collect_symbols(&program, SourceIndex(0))?;
///
/// // Access root scope
/// let root = symbol_table.scope(symbol_table.root);
/// println!("Found {} top-level symbols", root.symbols.len());
///
/// // Iterate through all symbols
/// for (i, symbol) in symbol_table.symbols.iter().enumerate() {
///     println!("Symbol {}: {} ({:?})", i, symbol.name, symbol.kind);
/// }
/// ```
pub fn collect_symbols(program: &DesugaredAST, source_index: SourceIndex)
-> Result<SymbolTable, SymbolCollectionError>
{
	let mut collector: Collector = Collector::new(source_index);
	collector.table.scopes[collector.table.root.0].span = program.span();
	collector.collect_program(program)?;
	return Ok(collector.table);
}

fn get_visability(mods: &[Modifier]) -> Visibility
{
	if mods.iter().any(|m| matches!(m, Modifier::Pub)) {
		return Visibility::Public;
	} else {
		return Visibility::Private;
	}
}
