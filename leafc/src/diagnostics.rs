use std::borrow::Cow;

use leaf_proc::Spanned;

use crate::config::{ColourConf, Config};
use crate::utils;
use crate::{Span, lexer::Spanned, source_map::SourceMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorCode
{
	// Parser
	ParseUnexpectedToken,
	ParseUnexpectedEof,
	ParseInvalidPattern,
	ParseInvalidType,
	ParseInvalidDeclaration,
	ParseUnexpectedItem,
	ParseGeneric,
	ParseNoCompileExpr,
	ParseCompileExprError,
	ParseReservedToken,
	ParseUseOfNotAllowedInternal,
	ParseUndefinedStringFlags,

	// Desugarer
	DesugarInvalidConstructorType,
	DesugarInvalidPattern,
	DesugarGeneric,

	// Symbol collection
	SymbolDuplicateDefinition,
	SymbolInvalidPath,
	SymbolGeneric,

	// Name resolution
	NameResolutionUnresolvedPath,
	NameResolutionShadowedVariable,
	NameResolutionPrivateSymbol,
	NameResolutionUnresolvedUseTarget,
	NameResolutionAmbiguousName,

	// Type resolution
	TypeCannotInfer,
	TypeMismatch,
	TypeUnknownSymbol,
	TypeNotCallable,
	TypeFieldAccessOnNonStruct,
	TypeUnknownField,
	TypeIndexOnNonArray,
	TypeReturnMismatch,
	TypeBreakMismatch,
	TypeArgCountMismatch,
	TypeInvalidUnaryOp,
	TypeInvalidBinaryOp,
	TypeInvalidCast,
	TypeIfBranchMismatch,
	TypeSwitchArmMismatch,
	TypeStructUnknownField,
	TypeStructFieldMismatch,
	TypeUnknownType,
	TypeUnresolvedIdentifier,
	TypeUnresolvedAssocPath,
	TypeUnresolvedSelf,
	TypeInvalidPrimitivePosition,

	// Module
	ModuleFileNotFound,
	ModuleIoError,
	ModuleCycle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity
{
	Error,
	#[allow(unused)]
	Warning,
}

#[derive(Debug, Clone, Spanned)]
pub struct Label
{
	pub span: Span,
	pub kind: LabelKind,
	pub message: Option<String>,
}

#[derive(Debug, Clone, Copy)]
pub enum LabelKind
{
	Primary,
	Secondary,
}

#[derive(Debug, Clone)]
pub struct Suggestion
{
	pub edits: Vec<(Span, String)>,
	pub message: String,
	#[allow(unused)]
	pub applicability: Applicability,
}

#[allow(unused)]
#[derive(Debug, Clone, Copy)]
pub enum Applicability
{
	MachineApplicable,
	MaybeIncorrect,
	HasPlaceholders,
}

#[derive(Debug)]
pub struct DiagnosticBuilder
{
	pub code: Option<ErrorCode>,
	pub severity: Severity,
	pub message: Cow<'static, str>,
	pub labels: Vec<Label>,
	pub notes: Vec<Cow<'static, str>>,
	pub helps: Vec<Cow<'static, str>>,
	pub suggestions: Vec<Suggestion>,
	pub related: Vec<Diagnostic>,
}

impl DiagnosticBuilder
{
	pub fn error<M: Into<Cow<'static, str>>>(msg: M) -> Self
	{
		return Self {
			code: None,
			severity: Severity::Error,
			message: msg.into(),
			labels: vec![],
			notes: vec![],
			helps: vec![],
			suggestions: vec![],
			related: vec![],
		};
	}

	#[allow(unused)]
	pub fn warning<M: Into<Cow<'static, str>>>(msg: M) -> Self
	{
		return Self {
			severity: Severity::Warning,
			..Self::error(msg)
		};
	}

	pub const fn code(mut self, code: ErrorCode) -> Self
	{
		self.code = Some(code);
		return self;
	}

	pub fn primary(mut self, span: Span, msg: impl Into<Option<String>>) -> Self
	{
		self.labels.push(Label {
			span,
			kind: LabelKind::Primary,
			message: msg.into(),
		});
		return self;
	}

	pub fn secondary(mut self, span: Span, msg: impl Into<Option<String>>) -> Self
	{
		self.labels.push(Label {
			span,
			kind: LabelKind::Secondary,
			message: msg.into(),
		});
		return self;
	}

	pub fn note(mut self, msg: impl Into<Cow<'static, str>>) -> Self
	{
		self.notes.push(msg.into());
		return self;
	}

	pub fn help(mut self, msg: impl Into<Cow<'static, str>>) -> Self
	{
		self.helps.push(msg.into());
		return self;
	}

	#[allow(unused)]
	pub fn suggestion(mut self, suggestion: Suggestion) -> Self
	{
		self.suggestions.push(suggestion);
		return self;
	}

	#[allow(unused)]
	pub fn related(mut self, diag: Diagnostic) -> Self
	{
		self.related.push(diag);
		return self;
	}

	pub fn finish(self) -> Diagnostic
	{
		return Diagnostic {
			code: self.code,
			severity: self.severity,
			message: self.message.into_owned(),
			labels: self.labels,
			notes: self.notes.into_iter().map(|c| return c.into_owned()).collect(),
			helps: self.helps.into_iter().map(|c| return c.into_owned()).collect(),
			suggestions: self.suggestions,
			related: self.related,
		};
	}
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Diagnostic
{
	pub code: Option<ErrorCode>,
	pub severity: Severity,
	pub message: String,
	pub labels: Vec<Label>,
	pub notes: Vec<String>,
	pub helps: Vec<String>,
	pub suggestions: Vec<Suggestion>,
	pub related: Vec<Diagnostic>,
}

pub trait CompileDiagnostic: std::fmt::Debug + Spanned
{
	fn build(&self) -> DiagnosticBuilder;

	fn to_diagnostic(&self) -> Diagnostic
	{
		return self.build().finish();
	}
}

pub trait CompileDiagnosticRenderer<'a>: std::fmt::Display
{
	fn new(diag: &'a Diagnostic, source_map: &'a SourceMap, config: &'a Config) -> Self;
}

#[derive(Debug, Clone, Spanned)]
pub enum CompileError
{
	Parse(crate::parser::ParseError),
	Desugar(crate::desugar::DesugarError),
	Module(crate::modules::ModuleError),
	SymbolCollection(crate::symbol_collection::SymbolCollectionError),
	NameResolution(crate::name_resolution::NameResolutionError),
	Type(crate::type_analysis::TypeError),
}

macro_rules! delegate {
	($self:ident, $method:ident) => {
		match $self {
			Self::Parse(e) => e.$method(),
			Self::Desugar(e) => e.$method(),
			Self::Module(e) => e.$method(),
			Self::SymbolCollection(e) => e.$method(),
			Self::NameResolution(e) => e.$method(),
			Self::Type(e) => e.$method(),
		}
	};
}

impl std::fmt::Display for CompileError
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			Self::Parse(e) => write!(f, "{e}"),
			Self::Desugar(e) => write!(f, "{e}"),
			Self::Module(e) => write!(f, "{e}"),
			Self::SymbolCollection(e) => write!(f, "{e}"),
			Self::NameResolution(e) => write!(f, "{e}"),
			Self::Type(e) => write!(f, "{e}"),
		};
	}
}

impl CompileDiagnostic for CompileError
{
	fn build(&self) -> DiagnosticBuilder
	{
		let mut diag = delegate!(self, build);

		match self {
			CompileError::Parse(_) => {
				diag = diag
					.note("while parsing the source file")
					.help("check for missing semicolons, braces, or invalid tokens");
			}
			CompileError::Desugar(_) => {
				diag = diag
					.note("during AST desugaring")
					.help("this usually means a syntactic construct was not lowered correctly");
			}
			CompileError::Module(_) => {
				diag = diag
					.note("while loading a module")
					.help("verify that the file exists and the module path is correct");
			}
			CompileError::SymbolCollection(_) => {
				diag = diag
					.note("during symbol collection")
					.help("check for duplicate definitions or invalid identifiers");
			}
			CompileError::NameResolution(_) => {
				diag = diag
					.note("during name resolution")
					.help("ensure all referenced names are defined and imported");
			}
			CompileError::Type(_) => {
				diag = diag
					.note("during type checking")
					.help("verify that expressions match expected types");
			}
		}

		return diag;
	}
}

pub struct OldStyleRenderer<'a>
{
	diag: &'a Diagnostic,
	source_map: &'a SourceMap,
	config: &'a Config,
}

#[allow(unused)]
pub const RESET: &str = "\x1b[0m";
#[allow(unused)]
pub const BOLD: &str = "\x1b[1m";
#[allow(unused)]
pub const DIM: &str = "\x1b[2m";
#[allow(unused)]
pub const ITALIC: &str = "\x1b[3m";
#[allow(unused)]
pub const UNDERLINE: &str = "\x1b[4m";
#[allow(unused)]
pub const BLINK: &str = "\x1b[5m";
#[allow(unused)]
pub const REVERSE: &str = "\x1b[7m";
#[allow(unused)]
pub const HIDDEN: &str = "\x1b[8m";
#[allow(unused)]
pub const STRIKETHROUGH: &str = "\x1b[9m";

#[allow(unused)]
pub const BLACK: &str = "\x1b[30m";
#[allow(unused)]
pub const RED: &str = "\x1b[31m";
#[allow(unused)]
pub const GREEN: &str = "\x1b[32m";
#[allow(unused)]
pub const YELLOW: &str = "\x1b[33m";
#[allow(unused)]
pub const BLUE: &str = "\x1b[34m";
#[allow(unused)]
pub const MAGENTA: &str = "\x1b[35m";
#[allow(unused)]
pub const CYAN: &str = "\x1b[36m";
#[allow(unused)]
pub const WHITE: &str = "\x1b[37m";

#[allow(unused)]
pub const BRIGHT_BLACK: &str = "\x1b[90m";
#[allow(unused)]
pub const BRIGHT_RED: &str = "\x1b[91m";
#[allow(unused)]
pub const BRIGHT_GREEN: &str = "\x1b[92m";
#[allow(unused)]
pub const BRIGHT_YELLOW: &str = "\x1b[93m";
#[allow(unused)]
pub const BRIGHT_BLUE: &str = "\x1b[94m";
#[allow(unused)]
pub const BRIGHT_MAGENTA: &str = "\x1b[95m";
#[allow(unused)]
pub const BRIGHT_CYAN: &str = "\x1b[96m";
#[allow(unused)]
pub const BRIGHT_WHITE: &str = "\x1b[97m";

#[allow(unused)]
pub const BG_BRIGHT_BLACK: &str = "\x1b[100m";
#[allow(unused)]
pub const BG_BRIGHT_RED: &str = "\x1b[101m";
#[allow(unused)]
pub const BG_BRIGHT_GREEN: &str = "\x1b[102m";
#[allow(unused)]
pub const BG_BRIGHT_YELLOW: &str = "\x1b[103m";
#[allow(unused)]
pub const BG_BRIGHT_BLUE: &str = "\x1b[104m";
#[allow(unused)]
pub const BG_BRIGHT_MAGENTA: &str = "\x1b[105m";
#[allow(unused)]
pub const BG_BRIGHT_CYAN: &str = "\x1b[106m";
#[allow(unused)]
pub const BG_BRIGHT_WHITE: &str = "\x1b[107m";

#[allow(unused)]
pub const CLEAR_SCREEN: &str = "\x1b[2J";
#[allow(unused)]
pub const CLEAR_LINE: &str = "\x1b[2K";
#[allow(unused)]
pub const CURSOR_HOME: &str = "\x1b[H";

pub fn use_colour_conf(config: &Config) -> bool
{
	return use_colour(config.colour);
}

pub fn use_colour(colour_conf: ColourConf) -> bool
{
	match colour_conf {
		crate::config::ColourConf::Always => return true,
		crate::config::ColourConf::Auto => {
			#[cfg(target_os = "linux")]
			{
				unsafe {
					// SAFETY: need to check if `stdin` is a terminal, and is using FFI
					return utils::libc::isatty(1) == 1; // `fd` = 1 -> `stdin`
				}
			}
			#[allow(unreachable_code)]
			return true;
		} // TODO
		crate::config::ColourConf::Never => return false,
	}
}

impl<'a> CompileDiagnosticRenderer<'a> for OldStyleRenderer<'a>
{
	fn new(diag: &'a Diagnostic, source_map: &'a SourceMap, config: &'a Config) -> Self
	{
		return Self {
			diag,
			source_map,
			config,
		};
	}
}

impl std::fmt::Display for OldStyleRenderer<'_>
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		let red = |s: &str, config: &Config| {
			return if use_colour_conf(config) {
				format!("{RED}{BOLD}{s}{RESET}")
			} else {
				s.to_string()
			};
		};
		let yellow = |s: &str, config: &Config| {
			return if use_colour_conf(config) {
				format!("{YELLOW}{BOLD}{s}{RESET}")
			} else {
				s.to_string()
			};
		};
		let blue = |s: &str, config: &Config| {
			return if use_colour_conf(config) {
				format!("{BLUE}{s}{RESET}")
			} else {
				s.to_string()
			};
		};
		let cyan = |s: &str, config: &Config| {
			return if use_colour_conf(config) {
				format!("{CYAN}{s}{RESET}")
			} else {
				s.to_string()
			};
		};
		let bold = |s: &str, config: &Config| {
			return if use_colour_conf(config) {
				format!("{BOLD}{s}{RESET}")
			} else {
				s.to_string()
			};
		};

		let severity_label: String = match self.diag.severity {
			Severity::Error => red("error", self.config),
			Severity::Warning => yellow("warning", self.config),
		};

		let Some(primary): Option<&Label> = self
			.diag
			.labels
			.iter()
			.find(|l| matches!(l.kind, LabelKind::Primary))
			.or_else(|| return self.diag.labels.first())
		else {
			writeln!(f, "{severity_label}: {}", self.diag.message)?;
			return Ok(());
		};

		let span: Span = primary.span;

		let file: &crate::source_map::SourceFile = self
			.source_map
			.get(span.source_index)
			.expect("Bug: asked a file that does not exsist");
		let source: &str = &file.src;
		let filename: &std::path::PathBuf = &file.path;

		let location: String = cyan(
			&format!("{}:{}:{}", filename.display(), span.start_line, span.start_col),
			self.config,
		);
		let gutter: String = blue("|", self.config);

		writeln!(
			f,
			"{severity_label}: {}\n  --> {}\n   {}",
			bold(&self.diag.message, self.config),
			location,
			gutter
		)?;

		let mut current_index = 0;

		for (line_number, line) in (1..).zip(source.lines()) {
			let line_start = current_index;
			let line_end = current_index + line.len();

			let labels_on_line: Vec<_> = self
				.diag
				.labels
				.iter()
				.filter(|l| {
					let s = l.span.start;
					let e = l.span.end;
					return line_end >= s && line_start <= e;
				})
				.collect();

			if !labels_on_line.is_empty() {
				writeln!(f, "{:>3} {} {}", line_number, gutter, line)?;

				for label in labels_on_line {
					let caret_start: usize = label.span.start.max(line_start) - line_start;
					let caret_end: usize = label.span.end.min(line_end) - line_start;
					let caret_len: usize = (caret_end.saturating_sub(caret_start)).max(1);

					let prefix: &str = &line[..caret_start];
					let caret_indent: String = prefix
						.chars()
						.map(|c| return if c == '\t' { '\t' } else { ' ' })
						.collect();

					let caret_color = match label.kind {
						LabelKind::Primary => red,
						LabelKind::Secondary => blue,
					};

					let caret: String = caret_color(&"^".repeat(caret_len), self.config);

					writeln!(f, "    {} {}{}", gutter, caret_indent, caret)?;

					if let Some(msg) = &label.message {
						writeln!(f, "    {} {}{}", gutter, caret_indent, caret_color(msg, self.config))?;
					}
				}
			}

			current_index = line_end + 1;
		}

		for note in &self.diag.notes {
			writeln!(f, "note: {}", note)?;
		}

		for help in &self.diag.helps {
			writeln!(f, "help: {}", help)?;
		}

		for sug in &self.diag.suggestions {
			writeln!(f, "suggestion: {}", sug.message)?;
			for (span, replacement) in &sug.edits {
				writeln!(f, "  replace {}..{} with `{}`", span.start, span.end, replacement)?;
			}
		}

		for related in &self.diag.related {
			writeln!(f)?;
			writeln!(f, "related:")?;
			let r: OldStyleRenderer = Self::new(related, self.source_map, self.config);
			writeln!(f, "{}", r)?;
		}
		return Ok(());
	}
}
