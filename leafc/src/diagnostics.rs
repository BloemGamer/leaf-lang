use crate::{source_map::SourceIndex, Span};
use leaf_proc::Spanned;

use crate::{
	desugar::DesugarError, lexer::Spanned, modules::ModuleError, name_resolution::NameResolutionError,
	parser::ParseError, symbol_collection::SymbolCollectionError, type_analysis::TypeError,
};

pub trait CompileDiagnostic: std::fmt::Display + std::fmt::Debug + Spanned
{
	#[allow(clippy::missing_errors_doc)]
	fn fmt_with_source(&self, f: &mut impl std::fmt::Write, sm: &crate::source_map::SourceMap) -> std::fmt::Result;
	#[allow(clippy::missing_errors_doc)]
	fn to_string_with_source(&self, sm: &crate::source_map::SourceMap) -> Result<String, std::fmt::Error>
	{
		let mut out: String = String::new();
		self.fmt_with_source(&mut out, sm)?;
		return Ok(out);
	}
}

#[derive(Debug, Clone, Spanned)]
pub enum CompileError
{
	Parse(ParseError),
	Desugar(DesugarError),
	Module(ModuleError),
	SymbolCollection(SymbolCollectionError),
	NameResolution(NameResolutionError),
	Type(TypeError),
}

impl std::fmt::Display for CompileError
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			CompileError::Parse(error) => {
				write!(f, "{}", error)
			}
			CompileError::Desugar(error) => {
				write!(f, "{}", error)
			}
			CompileError::Module(error) => {
				write!(f, "{}", error)
			}
			CompileError::SymbolCollection(error) => {
				write!(f, "{}", error)
			}
			CompileError::NameResolution(error) => {
				write!(f, "{}", error)
			}
			CompileError::Type(error) => {
				write!(f, "{}", error)
			}
		};
	}
}

impl std::error::Error for CompileError {}

impl CompileDiagnostic for CompileError
{
	#[allow(clippy::missing_errors_doc)]
	fn fmt_with_source(&self, f: &mut impl std::fmt::Write, sm: &crate::source_map::SourceMap) -> std::fmt::Result
	{
		return match self {
			CompileError::Parse(err) => err.fmt_with_source(f, sm),
			CompileError::Desugar(err) => err.fmt_with_source(f, sm),
			CompileError::Module(err) => err.fmt_with_source(f, sm),
			CompileError::SymbolCollection(err) => err.fmt_with_source(f, sm),
			CompileError::NameResolution(err) => err.fmt_with_source(f, sm),
			CompileError::Type(err) => err.fmt_with_source(f, sm),
		};
	}
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum CompileWarning {
	// TODO: add warnings for each module
}

impl Spanned for CompileWarning
{
	fn span(&self) -> Span
	{
		todo!(
			"implement this one if `CompileWarning` is not empy, CompileWarning is empyt: {}",
			std::mem::size_of::<Self>() == 0
		)
	}
}

impl std::fmt::Display for CompileWarning
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match self {
			_ => {
				todo!(
					"implement this one if `CompileWarning` is not empy, CompileWarning is empyt: {}",
					std::mem::size_of::<Self>() == 0
				)
			}
		}
	}
}

impl CompileDiagnostic for CompileWarning
{
	fn fmt_with_source(&self, f: &mut impl std::fmt::Write, sm: &crate::source_map::SourceMap) -> std::fmt::Result
	{
		match self {
			_ => {
				todo!(
					"implement this one if `CompileWarning` is not empy, CompileWarning is empyt: {}",
					std::mem::size_of::<Self>() == 0
				)
			}
		}
	}
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned)]
pub struct Diagnostic<K>
{
	pub span: Span,
	pub kind: K,
	pub context: Vec<String>,
	pub source_index: SourceIndex,
	pub severity: Severity,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Severity
{
	Error,
	Warning,
}
