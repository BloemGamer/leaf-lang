// TODO:
// - add a proper print function to the ResolvedAST (fmt::Display)
// - maybe make a tree structure for the modules instead of an un organised array

use std::fmt;

use crate::{
	CompileDiagnostic, CompileError,
	desugar::DesugaredAST,
	lexer::{Span, Spanned},
	parser::{Path, TopLevelDecl},
	source_map::SourceIndex,
	symbol_collection::{Scope, ScopeId, SymbolId, SymbolTable},
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
		// TODO: implement full resolution logic
		let _: (&Path, Span) = (path, span);
		return Err(NameResolutionError {
			span,
			kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
			source_index: self.source_index,
		});
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
