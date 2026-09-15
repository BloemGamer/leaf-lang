use std::{fmt, panic::Location};

use leaf_proc::Spanned;

use crate::{
	source_map::SourceMap,
	util::span::{Span, Spanned},
};

pub trait DiagnosticRenderer<'s>: fmt::Display
{
	fn new(diagnostic: &'s Diagnostic, source_map: &'s SourceMap) -> Self;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel
{
	Error,
	Warning,
	Note,
	Help,
	Bug,
	Unimplemented,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode
{
	E0000,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnnotationLevel
{
	Primary,
	Secondary,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct Annotation
{
	level: AnnotationLevel,
	span: Span,
	msg: Option<String>,
}

impl Annotation
{
	pub fn src<'s>(&self, source_map: &'s SourceMap) -> &'s str
	{
		return &source_map
			.get(self.span().file)
			.expect("when having a Span, the SourceMap should have the file")
			.src;
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic
{
	level: DiagnosticLevel,
	msg: String,
	code: Option<ErrorCode>,
	annotations: Vec<Annotation>,
	childeren: Vec<Diagnostic>,
}

impl Diagnostic
{
	#[inline]
	#[track_caller]
	pub fn at(level: DiagnosticLevel, msg: impl Into<String>) -> Self
	{
		let mut diag: Diagnostic = Diagnostic {
			level,
			msg: msg.into(),
			code: None,
			annotations: Vec::new(),
			childeren: Vec::new(),
		};
		if cfg!(feature = "diag_source") {
			diag.childeren.push(Diagnostic {
				level: DiagnosticLevel::Note,
				msg: format!("[{}]", Location::caller()),
				code: None,
				annotations: Vec::new(),
				childeren: Vec::new(),
			});
		}
		return diag;
	}

	#[inline]
	#[track_caller]
	pub fn error(msg: impl Into<String>) -> Self
	{
		return Diagnostic::at(DiagnosticLevel::Error, msg);
	}

	#[inline]
	#[track_caller]
	pub fn warning(msg: impl Into<String>) -> Self
	{
		return Diagnostic::at(DiagnosticLevel::Warning, msg);
	}

	#[inline]
	#[track_caller]
	pub fn note(msg: impl Into<String>) -> Self
	{
		return Diagnostic::at(DiagnosticLevel::Note, msg);
	}

	#[inline]
	#[track_caller]
	pub fn help(msg: impl Into<String>) -> Self
	{
		return Diagnostic::at(DiagnosticLevel::Help, msg);
	}

	#[inline]
	#[track_caller]
	pub fn bug(msg: impl Into<String>) -> Self
	{
		return Diagnostic::at(DiagnosticLevel::Bug, msg);
	}

	#[inline]
	#[track_caller]
	pub fn unimplemented(msg: impl Into<String>) -> Self
	{
		return Diagnostic::at(DiagnosticLevel::Unimplemented, msg);
	}

	pub fn primary(mut self, span: Span, msg: Option<impl Into<String>>) -> Self
	{
		self.annotations.push(Annotation {
			level: AnnotationLevel::Primary,
			span,
			msg: msg.map(|m| m.into()),
		});
		return self;
	}

	pub fn secondary(mut self, span: Span, msg: Option<impl Into<String>>) -> Self
	{
		self.annotations.push(Annotation {
			level: AnnotationLevel::Secondary,
			span,
			msg: msg.map(|m| m.into()),
		});
		return self;
	}

	pub const fn severity(&self) -> DiagnosticLevel
	{
		return self.level;
	}
}

pub struct DiagnosticPrettyRenderer<'s>
{
	diagnostic: &'s Diagnostic,
	source_map: &'s SourceMap,
}

impl<'s> DiagnosticRenderer<'s> for DiagnosticPrettyRenderer<'s>
{
	fn new(diagnostic: &'s Diagnostic, source_map: &'s SourceMap) -> Self
	{
		return DiagnosticPrettyRenderer { diagnostic, source_map };
	}
}

impl fmt::Display for DiagnosticPrettyRenderer<'_>
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		writeln!(f, "{:?}: {}", self.diagnostic.level, self.diagnostic.msg)?;
		for ann in &self.diagnostic.annotations {
			write!(f, "{:?}", ann.level)?;
			if let Some(m) = &ann.msg {
				write!(f, ": {}", m)?;
			}
			writeln!(f)?;
		}
		for child in &self.diagnostic.childeren {
			writeln!(f, "{}", Self::new(child, self.source_map))?;
		}
		return Ok(());
	}
}
