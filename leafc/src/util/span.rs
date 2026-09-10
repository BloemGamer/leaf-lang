use std::ops::{Deref, DerefMut, Range};

use crate::source_map::SourceIndex;

/// A region of source text.
///
/// `line` is the line `start` falls on, cached here because the lexer already
/// knows it. It lets the renderer print a line number without building a
/// line-start index for the whole file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span
{
	pub file: SourceIndex,
	pub line: usize,
	pub start: usize,
	pub end: usize,
}

/// A wrapper around the `Span` struct, so it can be ignored in the automatic `PartialEq` functions
#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
#[allow(clippy::module_name_repetitions)]
pub struct SpanNoEq(Span);

impl PartialEq for SpanNoEq
{
	fn eq(&self, _other: &Self) -> bool
	{
		return true;
	}
}

impl Span
{
	pub const DUMMY: Span = Span {
		file: SourceIndex::DUMMY,
		line: usize::MAX,
		start: usize::MAX,
		end: usize::MAX,
	};

	pub const fn new(file: SourceIndex, line: usize, start: usize, end: usize) -> Self
	{
		return Span { file, line, start, end };
	}

	pub const fn len(&self) -> usize
	{
		return self.end - self.start;
	}

	pub const fn is_empty(&self) -> bool
	{
		return self.start == self.end;
	}

	/// Merges two spans into one covering both.
	///
	/// `line` is taken from whichever span starts first, keeping the
	/// invariant that `line` is the line of `start`.
	///
	/// # Panics
	///
	/// Panics if the spans are in different files.
	pub fn to(self, other: Self) -> Self
	{
		assert!(self.file == other.file, "cannot merge spans across files");

		let (first, last) = if self.start <= other.start {
			(self, other)
		} else {
			(other, self)
		};

		return Span {
			file: first.file,
			line: first.line,
			start: first.start,
			end: last.end.max(first.end),
		};
	}
}

impl SpanNoEq
{
	pub const DUMMY: SpanNoEq = SpanNoEq(Span::DUMMY);
}

impl From<Span> for Range<usize>
{
	fn from(s: Span) -> Range<usize>
	{
		return s.start..s.end;
	}
}

/// A type that has an associated source [`Span`].
pub trait Spanned
{
	/// Returns the span covering this value.
	fn span(&self) -> Span;
}

impl Spanned for Span
{
	fn span(&self) -> Span
	{
		return *self;
	}
}

impl Spanned for SpanNoEq
{
	fn span(&self) -> Span
	{
		return self.0;
	}
}

impl From<Span> for SpanNoEq
{
	fn from(value: Span) -> Self
	{
		return SpanNoEq(value);
	}
}

impl From<SpanNoEq> for Span
{
	fn from(value: SpanNoEq) -> Self
	{
		return value.0;
	}
}

impl DerefMut for SpanNoEq
{
	fn deref_mut(&mut self) -> &mut Self::Target
	{
		return &mut self.0;
	}
}

impl Deref for SpanNoEq
{
	type Target = Span;

	fn deref(&self) -> &Self::Target
	{
		return &self.0;
	}
}
