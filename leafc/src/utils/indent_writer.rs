use std::fmt;

pub struct IndentWriter
{
	indent_level: usize,
	indent_str: &'static str,
}

impl IndentWriter
{
	pub const fn new() -> Self
	{
		return Self {
			indent_level: 0,
			indent_str: "    ", // 4 spaces
		};
	}

	pub const fn indent(&mut self)
	{
		self.indent_level += 1;
	}

	pub fn dedent(&mut self)
	{
		debug_assert!(self.indent_level > 0);
		self.indent_level -= 1;
	}

	pub fn write_indent(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		for _ in 0..self.indent_level {
			write!(f, "{}", self.indent_str)?;
		}
		return Ok(());
	}
}
