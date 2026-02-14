use crate::{
	lexer::{Span, Spanned},
	parser::{
		self, Block, CallType, FunctionDecl, FunctionSignature, Ident, Path, Pattern, Program, Stmt, StructDecl,
		TopLevelDecl, VariableDecl,
	},
	source_map::SourceIndex,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub usize);

#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind
{
	Variable
	{
		mutable: bool,
	},
	Function
	{
		signature_span: Span,
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
	AssociatedType,
	AssociatedConst,
	Module,
	Field,
	Label,
}

#[derive(Debug, Clone)]
pub struct Symbol
{
	pub name: Ident,
	pub kind: SymbolKind,
	pub def_span: Span,
	pub scope: ScopeId,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeKind
{
	ModuleImport,
	ModuleInline,
	Generics
	{
		body_scope: ScopeId,
	},
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
	WhileBody,
	ForBody,
	LoopBody,
}

#[derive(Debug, Clone)]
pub struct Scope
{
	pub kind: ScopeKind,
	pub parent: Option<ScopeId>,
	pub symbols: Vec<SymbolId>,
	pub children: Vec<ScopeId>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SymbolTable
{
	pub scopes: Vec<Scope>,
	pub symbols: Vec<Symbol>,
	pub root: ScopeId,
}

impl SymbolTable
{
	pub fn scope(&self, id: ScopeId) -> &Scope
	{
		return &self.scopes[id.0];
	}
	pub fn symbol(&self, id: SymbolId) -> &Symbol
	{
		return &self.symbols[id.0];
	}
}

#[derive(Debug, Clone)]
pub struct SymbolCollectionError
{
	pub span: Span,
	pub kind: SymbolCollectionErrorKind,
	pub context: Vec<String>,
	pub source_index: SourceIndex,
	scope: ScopeId,
}

impl Spanned for SymbolCollectionError
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

#[derive(Debug, Clone)]
pub enum SymbolCollectionErrorKind
{
	DuplicateDefinition
	{
		name: String, first_definition: Span
	},
	Generic
	{
		message: String
	},
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
		});
		self.table.scopes[scope.0].symbols.push(id);
		return Ok(id);
	}

	fn define(&mut self, name: Ident, kind: SymbolKind, def_span: Span) -> Result<SymbolId, SymbolCollectionError>
	{
		let scope = self.current_scope;
		return self.insert_symbol(scope, name, kind, def_span);
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

	fn collect_program(&mut self, program: &Program) -> Result<(), SymbolCollectionError>
	{
		for item in &program.items {
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
			TopLevelDecl::Directive(_) => Ok(()),
		};
	}

	fn collect_block(&mut self, block: &Block) -> Result<(), SymbolCollectionError>
	{
		for item in &block.stmts {
			self.collect_block_stmt(item)?;
		}
		return Ok(());
	}

	fn collect_block_stmt(&mut self, stmt: &Stmt) -> Result<(), SymbolCollectionError>
	{
		todo!()
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
					message: "A fucntion signature can't be a path, and only can have one segment".to_string(),
				},
			});
		}
		if let Some(name) = sig.name.segments.first() {
			self.define(
				name.name.clone(),
				SymbolKind::Function {
					signature_span: sig.span(),
				},
				sig.span(),
			)?;
		} else {
			unreachable!("A signature should always have a segment, otherwise the parser did not do his job right");
		}

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::FunctionBody, func.span());

		self.in_scope(body_scope, |c| {
			for generic in &sig.generics {
				c.define(generic.name.clone(), SymbolKind::GenericParam, generic.span())?;
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
				if path.len() != 1 {
					return Err(SymbolCollectionError {
						span: path.span(),
						context: Vec::new(),
						source_index: c.source_index,
						scope: c.current_scope,
						kind: SymbolCollectionErrorKind::Generic {
							message: "A function parameter can't be a path, and only can have one segment".to_string(),
						},
					});
				}
				if !path.segments[0].generics.is_empty() {
					return Err(SymbolCollectionError {
						span: path.span(),
						context: Vec::new(),
						source_index: c.source_index,
						scope: c.current_scope,
						kind: SymbolCollectionErrorKind::Generic {
							message: "A function parameter can't have a generic in the path".to_string(),
						},
					});
				}
				let Pattern::TypedIdentifier { mutable, .. } = param.pattern else {
					unreachable!("Should be handeled by the desugarer");
				};
				c.define(path.segments[0].name.clone(), SymbolKind::Variable { mutable }, *span)?;
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
		let Pattern::TypedIdentifier { path, span, .. } = &var.pattern else {
			unreachable!("Desugarer should have handled this");
		};
		if path.len() != 1 {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A variable can't be a path, and only can have one segment".to_string(),
				},
			});
		}
		if !path.segments[0].generics.is_empty() {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A variable can't have a generic in the path".to_string(),
				},
			});
		}
		let Pattern::TypedIdentifier { mutable, .. } = var.pattern else {
			unreachable!("Should be handeled by the desugarer");
		};
		self.define(path.segments[0].name.clone(), SymbolKind::Variable { mutable }, *span)?;

		return Ok(());
	}

	fn collect_struct_decl(&mut self, s: &StructDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &s.name;
		if path.len() != 1 {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A struct can't be a path, and only can have one segment".to_string(),
				},
			});
		}
		if !path.segments[0].generics.is_empty() {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A struct can't have a generic in the path".to_string(),
				},
			});
		}

		self.define(path.segments[0].name.clone(), SymbolKind::Struct, s.span())?;

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::StructFields, s.span());

		self.in_scope(body_scope, |c| {
			for f in &s.fields {
				c.define(f.name.clone(), SymbolKind::Field, f.span())?;
			}
			return Ok(());
		})?;
		return Ok(());
	}

	fn collect_union_decl(&mut self, u: &parser::UnionDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &u.name;
		if path.len() != 1 {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A union can't be a path, and only can have one segment".to_string(),
				},
			});
		}
		if !path.segments[0].generics.is_empty() {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A union can't have a generic in the path".to_string(),
				},
			});
		}

		self.define(path.segments[0].name.clone(), SymbolKind::Union, u.span())?;

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::UnionFields, u.span());

		self.in_scope(body_scope, |c| {
			for f in &u.fields {
				c.define(f.name.clone(), SymbolKind::Field, f.span())?;
			}
			return Ok(());
		})?;
		return Ok(());
	}

	fn collect_enum_decl(&mut self, e: &parser::EnumDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &e.name;
		if path.len() != 1 {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A enum can't be a path, and only can have one segment".to_string(),
				},
			});
		}
		if !path.segments[0].generics.is_empty() {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A enum can't have a generic in the path".to_string(),
				},
			});
		}

		self.define(path.segments[0].name.clone(), SymbolKind::Enum, e.span())?;

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::EnumVariants, e.span());

		self.in_scope(body_scope, |c| {
			for f in &e.variants {
				c.define(f.name.clone(), SymbolKind::EnumVariant, f.span())?;
			}
			return Ok(());
		})?;
		return Ok(());
	}

	fn collect_variant_decl(&mut self, v: &parser::VariantDecl) -> Result<(), SymbolCollectionError>
	{
		let path: &Path = &v.name;
		if path.len() != 1 {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A variant can't be a path, and only can have one segment".to_string(),
				},
			});
		}
		if !path.segments[0].generics.is_empty() {
			return Err(SymbolCollectionError {
				span: path.span(),
				context: Vec::new(),
				source_index: self.source_index,
				scope: self.current_scope,
				kind: SymbolCollectionErrorKind::Generic {
					message: "A variant can't have a generic in the path".to_string(),
				},
			});
		}

		self.define(path.segments[0].name.clone(), SymbolKind::Variant, v.span())?;

		let body_scope: ScopeId = self.alloc_scope(ScopeKind::VariantMembers, v.span());

		self.in_scope(body_scope, |c| {
			for f in &v.variants {
				c.define(f.name.clone(), SymbolKind::VariantMember, f.span())?;
			}
			return Ok(());
		})?;
		return Ok(());
	}

	fn collect_type_alias_decl(&mut self, t: &parser::TypeAliasDecl) -> Result<(), SymbolCollectionError>
	{
		todo!()
	}

	fn collect_trait_decl(&mut self, t: &parser::TraitDecl) -> Result<(), SymbolCollectionError>
	{
		todo!()
	}

	fn collect_module_decl(&mut self, n: &parser::ModuleDecl) -> Result<(), SymbolCollectionError>
	{
		todo!()
	}

	fn collect_impl_decl(&mut self, i: &parser::ImplDecl) -> Result<(), SymbolCollectionError>
	{
		todo!()
	}

	fn collect_stmt(&mut self, stmt: &parser::Stmt) -> Result<(), SymbolCollectionError>
	{
		todo!()
	}

	fn collect_expr(&mut self, expr: &parser::Expr) -> Result<(), SymbolCollectionError>
	{
		todo!()
	}

	fn collect_pattern_bindings(
		&mut self,
		pattern: &parser::Pattern,
		mutable: bool,
	) -> Result<(), SymbolCollectionError>
	{
		todo!()
	}
}

pub fn collect_symbols(program: &Program, source_index: SourceIndex) -> Result<SymbolTable, SymbolCollectionError>
{
	let mut collector: Collector = Collector::new(source_index);
	collector.table.scopes[collector.table.root.0].span = program.span();
	collector.collect_program(program)?;
	return Ok(collector.table);
}
