// TODO:
// - add a proper print function to the ResolvedAST (fmt::Display)
// - maybe make a tree structure for the modules instead of an un organised array

#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::needless_pass_by_ref_mut)]
#![allow(clippy::unused_self)]

use std::fmt;

use crate::{
	CompileDiagnostic, CompileError,
	desugar::DesugaredAST,
	lexer::{Span, Spanned},
	parser::{Path, TopLevelDecl},
	source_map::SourceIndex,
	symbol_collection::{Scope, ScopeId, Symbol, SymbolId, SymbolTable, Visibility},
};

#[allow(unused)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone)]
pub struct ResolvedAST
{
	pub top_level_block: ResolvedTopLevelBlock,
	pub source_index: SourceIndex,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct ResolvedTopLevelBlock
{
	pub items: Vec<ResolvedTopLevelDecl>,
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum ResolvedTopLevelDecl
{
	// TODO: add variants for each AST member
	#[allow(dead_code)]
	Unresolved(TopLevelDecl),
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct ResolvedModule
{
	pub path: Vec<String>,
	pub ast: ResolvedAST,
	pub symbols: SymbolTable,
}

#[allow(unused)]
struct Resolver<'a>
{
	modules: &'a [(Vec<String>, DesugaredAST, SymbolTable)],
	symbols: &'a SymbolTable,
	current_scope: ScopeId,
	use_imports: Vec<UseImport>,

	source_index: SourceIndex,
}

#[allow(unused)]
#[derive(Debug, Clone)]
struct UseImport
{
	alias: Vec<String>,
	target_path: Vec<String>,
	glob: bool,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct NameResolutionError
{
	pub span: Span,
	pub kind: NameResolutionErrorKind,
	pub source_index: SourceIndex,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum NameResolutionErrorKind
{
	UnresolvedPath
	{
		path: Path
	},
	PrivateSymbol
	{
		path: Path
	},
	UnresolvedUseTarget
	{
		path: Path
	},
	AmbiguousName
	{
		name: String, candidates: Vec<Path>
	},
}

impl Spanned for NameResolutionError
{
	fn span(&self) -> Span
	{
		return self.span;
	}
}

impl fmt::Display for NameResolutionError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match &self.kind {
			NameResolutionErrorKind::UnresolvedPath { path } => {
				return write!(
					f,
					"unresolved path `{}`",
					path.segments
						.iter()
						.map(|s| return s.name.as_str())
						.collect::<Vec<_>>()
						.join("::")
				);
			}
			NameResolutionErrorKind::PrivateSymbol { path } => {
				return write!(
					f,
					"symbol `{}` is private",
					path.segments
						.iter()
						.map(|s| return s.name.as_str())
						.collect::<Vec<_>>()
						.join("::")
				);
			}
			NameResolutionErrorKind::UnresolvedUseTarget { path } => {
				return write!(
					f,
					"`@use` target `{}` does not exist",
					path.segments
						.iter()
						.map(|s| return s.name.as_str())
						.collect::<Vec<_>>()
						.join("::")
				);
			}
			NameResolutionErrorKind::AmbiguousName { name, .. } => return write!(f, "ambiguous name `{}`", name),
		}
	}
}

impl std::error::Error for NameResolutionError {}

impl From<NameResolutionError> for CompileError
{
	fn from(e: NameResolutionError) -> Self
	{
		return CompileError::NameResolutionError(e);
	}
}

impl CompileDiagnostic for NameResolutionError
{
	fn fmt_with_source(&self, f: &mut impl fmt::Write, sm: &crate::source_map::SourceMap) -> fmt::Result
	{
		return write!(
			f,
			"{}",
			self.span
				.format_error(&sm.get(self.source_index).src, &format!("{self}"))
		);
	}
}

#[allow(unused)]
impl<'a> Resolver<'a>
{
	const fn new(
		modules: &'a [(Vec<String>, DesugaredAST, SymbolTable)],
		symbols: &'a SymbolTable,
		source_index: SourceIndex,
	) -> Self
	{
		return Self {
			modules,
			symbols,
			current_scope: symbols.root,
			use_imports: Vec::new(),
			source_index,
		};
	}

	const fn enter_scope(&mut self, scope: ScopeId)
	{
		return self.current_scope = scope;
	}

	fn exit_scope(&mut self)
	{
		if let Some(parent) = self.symbols.scope(self.current_scope).parent {
			self.current_scope = parent;
		}
	}

	fn lookup_in_scope(&self, name: &str) -> Option<SymbolId>
	{
		let mut scope_id: ScopeId = self.current_scope;
		loop {
			let scope: &Scope = self.symbols.scope(scope_id);
			for &sym_id in &scope.symbols {
				if self.symbols.symbol(sym_id).name == name {
					return Some(sym_id);
				}
			}
			match scope.parent {
				Some(parent) => scope_id = parent,
				None => break,
			}
		}
		return None;
	}

	fn resolve_path(&self, path: &Path, span: Span) -> Result<SymbolId, NameResolutionError>
	{
		let segments: &Vec<crate::parser::PathSegment> = &path.segments;
		if segments.is_empty() {
			return Err(NameResolutionError {
				span,
				kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
				source_index: self.source_index,
			});
		}

		let first_name: &String = &segments[0].name;

		let (mut current_sym_id, mut current_scope) = if path.global {
			let sym: SymbolId = self.find_in_scope(self.symbols.root, first_name).ok_or_else(|| {
				return NameResolutionError {
					span,
					kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
					source_index: self.source_index,
				};
			})?;
			(sym, self.symbols.root)
		} else if let Some(sym_id) = self.resolve_first_via_use(first_name, span, path)? {
			if segments.len() == 1 {
				return Ok(sym_id);
			}
			let introduced: Option<ScopeId> = self.find_introduced_scope(sym_id);
			match introduced {
				Some(sc) => (sym_id, sc),
				None => {
					return Err(NameResolutionError {
						span,
						kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
						source_index: self.source_index,
					});
				}
			}
		} else {
			self.find_in_scope_chain(self.current_scope, first_name)
				.ok_or_else(|| {
					return NameResolutionError {
						span,
						kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
						source_index: self.source_index,
					};
				})?
		};

		for seg in &segments[1..] {
			let name: &String = &seg.name;

			let search_scope: ScopeId = self.find_introduced_scope(current_sym_id).ok_or_else(|| {
				return NameResolutionError {
					span,
					kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
					source_index: self.source_index,
				};
			})?;

			let sym_id: SymbolId = self.find_in_scope(search_scope, name).ok_or_else(|| {
				return NameResolutionError {
					span,
					kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
					source_index: self.source_index,
				};
			})?;

			let sym: &Symbol = self.symbols.symbol(sym_id);
			if sym.visibility == Visibility::Private && !self.is_descendant_of(self.current_scope, search_scope) {
				return Err(NameResolutionError {
					span,
					kind: NameResolutionErrorKind::PrivateSymbol { path: path.clone() },
					source_index: self.source_index,
				});
			}

			current_sym_id = sym_id;
			current_scope = search_scope;
		}

		return Ok(current_sym_id);
	}

	fn resolve_top_level_block(
		&mut self,
		block: &crate::parser::TopLevelBlock,
		source_index: SourceIndex,
	) -> Result<ResolvedTopLevelBlock, NameResolutionError>
	{
		let mut items: Vec<ResolvedTopLevelDecl> = Vec::new();
		for decl in &block.items {
			items.push(self.resolve_top_level_decl(decl)?);
		}
		return Ok(ResolvedTopLevelBlock {
			items,
			span: block.span,
		});
	}

	#[allow(unused)]
	fn resolve_top_level_decl(&mut self, decl: &TopLevelDecl) -> Result<ResolvedTopLevelDecl, NameResolutionError>
	{
		// TODO: match on each variant and produce the resolved form
		return Ok(ResolvedTopLevelDecl::Unresolved(decl.clone()));
	}

	fn find_in_scope(&self, scope_id: ScopeId, name: &str) -> Option<SymbolId>
	{
		return self
			.symbols
			.scope(scope_id)
			.symbols
			.iter()
			.find(|&&sym_id| return self.symbols.symbol(sym_id).name == name)
			.copied();
	}

	fn find_in_scope_chain(&self, start: ScopeId, name: &str) -> Option<(SymbolId, ScopeId)>
	{
		let mut scope_id: ScopeId = start;
		loop {
			let scope: &Scope = self.symbols.scope(scope_id);
			for &sym_id in &scope.symbols {
				if self.symbols.symbol(sym_id).name == name {
					return Some((sym_id, scope_id));
				}
			}
			match scope.parent {
				Some(parent) => scope_id = parent,
				None => return None,
			}
		}
	}

	fn find_introduced_scope(&self, sym_id: SymbolId) -> Option<ScopeId>
	{
		return self.symbols.symbol(sym_id).introduced_scope;
	}

	fn is_descendant_of(&self, mut scope: ScopeId, ancestor: ScopeId) -> bool
	{
		loop {
			if scope == ancestor {
				return true;
			}
			match self.symbols.scope(scope).parent {
				Some(parent) => scope = parent,
				None => return false,
			}
		}
	}

	fn resolve_first_via_use(
		&self,
		name: &str,
		span: Span,
		original_path: &Path,
	) -> Result<Option<SymbolId>, NameResolutionError>
	{
		for import in &self.use_imports {
			if import.glob {
				if let Some(sym_id) = self.resolve_use_glob(import, name, span, original_path)? {
					return Ok(Some(sym_id));
				}
				continue;
			}

			let alias_name: &str = match import.alias.last() {
				Some(a) => a.as_str(),
				None => continue,
			};

			if alias_name != name {
				continue;
			}

			let sym_id: SymbolId = self.resolve_absolute_path(&import.target_path, span, original_path)?;
			return Ok(Some(sym_id));
		}

		return Ok(None);
	}

	fn resolve_use_glob(
		&self,
		import: &UseImport,
		name: &str,
		span: Span,
		original_path: &Path,
	) -> Result<Option<SymbolId>, NameResolutionError>
	{
		let mut scope = self.symbols.root;

		for seg in &import.target_path {
			let sym_id: SymbolId = self.find_in_scope(scope, seg).ok_or_else(|| {
				return NameResolutionError {
					span,
					kind: NameResolutionErrorKind::UnresolvedUseTarget {
						path: original_path.clone(),
					},
					source_index: self.source_index,
				};
			})?;
			scope = self.find_introduced_scope(sym_id).unwrap_or(scope);
		}

		if let Some(sym_id) = self.find_in_scope(scope, name) {
			return Ok(Some(sym_id));
		}

		if let Some(sym_id) = self.resolve_cross_module_glob(&import.target_path, name) {
			return Ok(Some(sym_id));
		}

		return Ok(None);
	}

	fn resolve_absolute_path(
		&self,
		segments: &[String],
		span: Span,
		original_path: &Path,
	) -> Result<SymbolId, NameResolutionError>
	{
		let mut scope: ScopeId = self.symbols.root;
		let mut last_sym: Option<SymbolId> = None;

		for (i, seg) in segments.iter().enumerate() {
			if let Some(sym_id) = self.find_in_scope(scope, seg) {
				last_sym = Some(sym_id);
				if i + 1 < segments.len() {
					scope = self.find_introduced_scope(sym_id).unwrap_or(scope);
				}
			} else if let Some((sym_id, new_scope)) = self.resolve_cross_module_segment(segments, i, scope) {
				last_sym = Some(sym_id);
				scope = new_scope;
			} else {
				return Err(NameResolutionError {
					span,
					kind: NameResolutionErrorKind::UnresolvedUseTarget {
						path: original_path.clone(),
					},
					source_index: self.source_index,
				});
			}
		}

		return last_sym.ok_or_else(|| {
			return NameResolutionError {
				span,
				kind: NameResolutionErrorKind::UnresolvedUseTarget {
					path: original_path.clone(),
				},
				source_index: self.source_index,
			};
		});
	}

	fn resolve_cross_module_segment(
		&self,
		segments: &[String],
		at: usize,
		_current_scope: ScopeId,
	) -> Option<(SymbolId, ScopeId)>
	{
		let prefix: &[String] = &segments[..=at];
		for (mod_path, _ast, mod_symbols) in self.modules {
			if mod_path.len() >= prefix.len() && &mod_path[..prefix.len()] == prefix {
				let name: &String = &segments[at];
				if let Some(sym_id) = self.find_in_scope(self.symbols.root, name) {
					return Some((sym_id, mod_symbols.root));
				}
			}
		}
		return None;
	}

	fn resolve_cross_module_glob(&self, target_path: &[String], name: &str) -> Option<SymbolId>
	{
		for (mod_path, _ast, mod_symbols) in self.modules {
			if mod_path == target_path {
				for &sym_id in &mod_symbols.scope(mod_symbols.root).symbols {
					let sym: &Symbol = mod_symbols.symbol(sym_id);
					if sym.name == name && sym.visibility == Visibility::Public {
						// TODO: unify cross-module symbol IDs
						break;
					}
				}
			}
		}
		return None;
	}
}

pub fn resolve_names(
	logical_path: &[String],
	ast: &DesugaredAST,
	symbols: &SymbolTable,
	modules: &[(Vec<String>, DesugaredAST, SymbolTable)],
) -> Result<ResolvedModule, CompileError>
{
	let mut resolver: Resolver<'_> = Resolver::new(modules, symbols, ast.source_index);

	let resolved_block: ResolvedTopLevelBlock = resolver
		.resolve_top_level_block(&ast.top_level_block, ast.source_index)
		.map_err(CompileError::NameResolutionError)?;

	return Ok(ResolvedModule {
		path: logical_path.to_vec(),
		ast: ResolvedAST {
			top_level_block: resolved_block,
			source_index: ast.source_index,
		},
		symbols: symbols.clone(),
	});
}
