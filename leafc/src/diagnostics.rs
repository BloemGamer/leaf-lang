use crate::{
	desugar::DesugarError, modules::ModuleError, name_resolution::NameResolutionError, parser::ParseError,
	symbol_collection::SymbolCollectionError, type_analysis::TypeError,
};

pub trait CompileDiagnostic: std::fmt::Display + std::fmt::Debug
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

#[derive(Debug, Clone)]
pub enum CompileError
{
	ParseError(ParseError),
	DesugarError(DesugarError),
	ModuleError(ModuleError),
	SymbolCollectionError(SymbolCollectionError),
	NameResolutionError(NameResolutionError),
	TypeError(TypeError),
}

impl std::fmt::Display for CompileError
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			CompileError::ParseError(error) => {
				write!(f, "{}", error)
			}
			CompileError::DesugarError(error) => {
				write!(f, "{}", error)
			}
			CompileError::ModuleError(error) => {
				write!(f, "{}", error)
			}
			CompileError::SymbolCollectionError(error) => {
				write!(f, "{}", error)
			}
			CompileError::NameResolutionError(error) => {
				write!(f, "{}", error)
			}
			CompileError::TypeError(error) => {
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
			CompileError::ParseError(err) => err.fmt_with_source(f, sm),
			CompileError::DesugarError(err) => err.fmt_with_source(f, sm),
			CompileError::ModuleError(err) => err.fmt_with_source(f, sm),
			CompileError::SymbolCollectionError(err) => err.fmt_with_source(f, sm),
			CompileError::NameResolutionError(err) => err.fmt_with_source(f, sm),
			CompileError::TypeError(err) => err.fmt_with_source(f, sm),
		};
	}
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum CompileWarning {
	// TODO: add warnings for each module
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
