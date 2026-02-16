use crate::{
	lexer::{Span, Spanned},
	parser::{
		self, ArrayLiteral, Block, CallType, Expr, FunctionDecl, FunctionSignature, Ident, ImplDecl, ImplItem,
		Modifier, ModuleDecl, Path, Pattern, Program, RangeExpr, Stmt, StructDecl, SwitchBody, TopLevelDecl, TraitDecl,
		TraitItem, VariableDecl,
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

#[derive(Debug, Clone)]
pub struct Symbol
{
	pub name: Ident,
	pub kind: SymbolKind,
	pub def_span: Span,
	pub scope: ScopeId,
	pub visibility: Visibility,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility
{
	Public,
	Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability
{
	Mutable,
	Const,
	Immutable,
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
			return c.collect_program(&m.body);
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
			Stmt::Directive(_) => {
				unimplemented!("For now, directives are not stable for standalone things")
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
					self.in_scope(else_scope, |c| return c.collect_stmt(el))?;
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
						c.collect_pattern_bindings(&arm.pattern, false)?;

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
					c.collect_pattern_bindings(pattern, false)?;

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

fn get_visability(mods: &Vec<Modifier>) -> Visibility
{
	if mods.iter().any(|m| matches!(m, Modifier::Pub)) {
		return Visibility::Public;
	} else {
		return Visibility::Private;
	}
}
