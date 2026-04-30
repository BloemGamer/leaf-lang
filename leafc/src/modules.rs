mod tests;

use std::path::{self, PathBuf};

use crate::{
	diagnostics::{CompileDiagnostic, CompileError},
	lexer::Span,
	parser::{ModuleKind, TopLevelBlock, TopLevelDecl, AST},
	source_map::{SourceIndex, SourceMap},
};

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct PendingModule
{
	pub logical_path: Vec<String>,
	pub file_path: PathBuf,
	pub declared_at_span: Span,
	pub declared_at_source: SourceIndex,
}

#[derive(Debug, Clone)]
pub struct ModuleError
{
	pub logical_path: Vec<String>,
	pub span: Span,
	pub source_index: SourceIndex,
	pub kind: ModuleErrorKind,
}

#[derive(Debug, Clone)]
pub enum ModuleErrorKind
{
	FileNotFound(PathBuf),
	IoError(String),
	Cycle(Vec<Vec<String>>),
}

impl std::fmt::Display for ModuleError
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		let path = self.logical_path.join("::");
		return match &self.kind {
			ModuleErrorKind::FileNotFound(p) => {
				write!(f, "module '{}': file not found at '{}'", path, p.display())
			}
			ModuleErrorKind::IoError(e) => write!(f, "module '{}': io error: {}", path, e),
			ModuleErrorKind::Cycle(chain) => {
				let chain_str: Vec<String> = chain.iter().map(|p| return p.join("::")).collect();
				write!(f, "module cycle detected: {}", chain_str.join(" -> "))
			}
		};
	}
}

impl std::error::Error for ModuleError {}

impl From<ModuleError> for CompileError
{
	fn from(e: ModuleError) -> Self
	{
		return CompileError::ModuleError(e);
	}
}

impl CompileDiagnostic for ModuleError
{
	fn fmt_with_source(&self, f: &mut impl std::fmt::Write, sm: &SourceMap) -> std::fmt::Result
	{
		let msg = self.to_string();
		return write!(f, "{}", self.span.format_error(self.source_index, sm, &msg));
	}
}

pub fn collect_pending(ast: &AST, declaring_file: &path::Path, current_modue: &[String]) -> Vec<PendingModule>
{
	let mut pending: Vec<PendingModule> = Vec::new();
	collect_from_block(
		&ast.top_level_block,
		current_modue,
		&[],
		declaring_file,
		ast.source_index,
		&mut pending,
	);
	return pending;
}

fn collect_from_block(
	block: &TopLevelBlock,
	parent_path: &[String],
	file_path_segments: &[String],
	declaring_file: &path::Path,
	declaring_source: SourceIndex,
	pending: &mut Vec<PendingModule>,
)
{
	for item in &block.items {
		if let TopLevelDecl::Module(module_decl) = item {
			let name: Vec<String> = module_decl
				.name
				.segments
				.iter()
				.map(|s| return s.name.clone())
				.collect();
			let mut full_path: Vec<String> = parent_path.to_vec();
			full_path.extend(name.clone());
			let mut file_segments: Vec<String> = file_path_segments.to_vec();
			file_segments.extend(name);
			match &module_decl.kind {
				ModuleKind::External => {
					pending.push(PendingModule {
						file_path: resolve_file(&file_segments, declaring_file),
						logical_path: full_path,
						declared_at_span: module_decl.span,
						declared_at_source: declaring_source,
					});
				}
				ModuleKind::Inline(body) => {
					collect_from_block(
						body,
						&full_path,
						file_path_segments,
						declaring_file,
						declaring_source,
						pending,
					);
				}
			}
		}
	}
}

fn resolve_file(logical_path: &[String], declared_in: &path::Path) -> PathBuf
{
	let base_dir: &path::Path = declared_in.parent().unwrap_or_else(|| return path::Path::new("."));
	let mut path: PathBuf = base_dir.to_path_buf();
	for segment in logical_path {
		path.push(segment);
	}
	path.set_extension("leaf");
	return path;
}
