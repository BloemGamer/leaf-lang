mod tests;

use std::{
	fmt,
	path::{self, PathBuf},
};

use leaf_proc::Spanned;

use crate::{
	diagnostics::{CompileDiagnostic, CompileError, DiagnosticBuilder, ErrorCode},
	lexer::{Span, Spanned},
	parser::{AST, ModuleKind, TopLevelBlock, TopLevelDecl},
};

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct PendingModule
{
	pub logical_path: Vec<String>,
	pub file_path: PathBuf,
	pub declared_at_span: Span,
}

#[derive(Debug, Clone)]
pub enum ModuleErrorKind
{
	FileNotFound(PathBuf),
	NoFileOrDirectory(PathBuf),
	IoError(String),
	#[allow(unused)]
	Cycle(Vec<Vec<String>>),
}

#[derive(Debug, Clone, Spanned)]
pub struct ModuleError
{
	pub logical_path: Vec<String>,
	pub span: Span,
	pub kind: ModuleErrorKind,
	pub context: Vec<String>,
}

#[allow(unused)]
impl ModuleError
{
	pub const fn new(logical_path: Vec<String>, span: Span, kind: ModuleErrorKind) -> Self
	{
		return Self {
			logical_path,
			span,
			kind,
			context: Vec::new(),
		};
	}

	pub fn with_context(mut self, ctx: impl Into<String>) -> Self
	{
		self.context.push(ctx.into());
		return self;
	}
}

impl fmt::Display for ModuleError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		let path = self.logical_path.join("::");

		return match &self.kind {
			ModuleErrorKind::FileNotFound(p) => {
				write!(f, "module `{path}`: file not found at `{}`", p.display())
			}

			ModuleErrorKind::NoFileOrDirectory(p) => {
				let mut file_path: PathBuf = p.clone();
				file_path.set_extension("leaf");
				let mut dir_path: PathBuf = p.clone();
				dir_path.push("module");
				dir_path.set_extension("leaf");
				write!(
					f,
					"module `{path}`: files not found at `{}` or `{}`",
					dir_path.display(),
					file_path.display()
				)
			}

			ModuleErrorKind::IoError(e) => {
				write!(f, "module `{path}`: io error: {e}")
			}

			ModuleErrorKind::Cycle(chain) => {
				let chain_str: Vec<String> = chain.iter().map(|p| return p.join("::")).collect();
				write!(f, "module cycle detected: {}", chain_str.join(" -> "))
			}
		};
	}
}

impl std::error::Error for ModuleError {}

impl CompileDiagnostic for ModuleError
{
	fn build(&self) -> DiagnosticBuilder<'_>
	{
		let module_path = self.logical_path.join("::");

		let mut diag = match &self.kind {
			ModuleErrorKind::FileNotFound(p) => {
				DiagnosticBuilder::error(format!("module `{module_path}`: file not found at `{}`", p.display()))
					.code(ErrorCode::ModuleFileNotFound)
					.primary(self.span, None)
			}

			ModuleErrorKind::NoFileOrDirectory(p) => {
				let mut file_path: PathBuf = p.clone();
				file_path.set_extension("leaf");
				let mut dir_path: PathBuf = p.clone();
				dir_path.push("module");
				dir_path.set_extension("leaf");
				DiagnosticBuilder::error(format!(
					"module `{module_path}`: files not found at `{}` or `{}`",
					dir_path.display(),
					file_path.display(),
				))
				.code(ErrorCode::ModuleFileNotFound)
				.primary(self.span, None)
			}

			ModuleErrorKind::IoError(e) => DiagnosticBuilder::error(format!("module `{module_path}`: io error: {e}"))
				.code(ErrorCode::ModuleIoError)
				.primary(self.span, None),

			ModuleErrorKind::Cycle(chain) => {
				let mut d = DiagnosticBuilder::error("module cycle detected")
					.code(ErrorCode::ModuleCycle)
					.primary(self.span, None);

				for path in chain {
					d = d.note(format!("in cycle: {}", path.join("::")));
				}

				d
			}
		};

		// Add context stack
		for ctx in &self.context {
			diag = diag.note(format!("while loading module: {ctx}"));
		}

		return diag;
	}
}

impl From<ModuleError> for CompileError
{
	fn from(e: ModuleError) -> Self
	{
		return CompileError::Module(e);
	}
}

pub fn collect_pending(
	ast: &AST,
	declaring_file: &path::Path,
	current_modue: &[String],
) -> Result<Vec<PendingModule>, ModuleError>
{
	let mut pending: Vec<PendingModule> = Vec::new();
	collect_from_block(&ast.top_level_block, current_modue, &[], declaring_file, &mut pending)?;
	return Ok(pending);
}

fn collect_from_block(
	block: &TopLevelBlock,
	parent_path: &[String],
	file_path_segments: &[String],
	declaring_file: &path::Path,
	pending: &mut Vec<PendingModule>,
) -> Result<(), ModuleError>
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
						file_path: resolve_file(&file_segments, declaring_file, module_decl.span)?,
						logical_path: full_path,
						declared_at_span: module_decl.span,
					});
				}
				ModuleKind::Inline(body) => {
					collect_from_block(body, &full_path, file_path_segments, declaring_file, pending)?;
				}
			}
		}
	}
	return Ok(());
}

fn resolve_file(logical_path: &[String], declared_in: &path::Path, span: Span) -> Result<PathBuf, ModuleError>
{
	let base_dir: &path::Path = declared_in.parent().unwrap_or_else(|| return path::Path::new("."));
	let mut path: PathBuf = base_dir.to_path_buf();
	for segment in logical_path {
		path.push(segment);
	}
	// first check if `path/mudule.leaf` exsists, otherwise, check if `path.leaf` exsists
	if !path.exists() {
		path.set_extension("leaf");
		if path.exists() {
			return Ok(path);
		}

		return Err(ModuleError {
			logical_path: logical_path.to_vec(),
			span,
			kind: ModuleErrorKind::NoFileOrDirectory(path),
			context: Vec::new(),
		});
	}
	path.push("module");
	path.set_extension("leaf");
	if !path.exists() {
		return Err(ModuleError {
			logical_path: logical_path.to_vec(),
			span,
			kind: ModuleErrorKind::FileNotFound(path),
			context: Vec::new(),
		});
	}
	return Ok(path);
}
