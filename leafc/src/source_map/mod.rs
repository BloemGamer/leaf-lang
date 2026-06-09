use std::path;

#[derive(Debug, Clone)]
pub struct SourceFile
{
	pub path: path::PathBuf,
	pub src: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceIndex(usize);

impl SourceIndex
{
	pub const fn new(index: usize) -> SourceIndex
	{
		return SourceIndex(index);
	}
}

#[derive(Debug, Default)]
pub struct SourceMap
{
	files: Vec<SourceFile>,
}

impl SourceMap
{
	pub fn new() -> SourceMap
	{
		return SourceMap::default();
	}
	pub fn add_file(&mut self, path: impl Into<path::PathBuf>, src: impl Into<String>) -> SourceIndex
	{
		let file: SourceFile = SourceFile {
			path: path.into(),
			src: src.into(),
		};
		self.files.push(file);
		return SourceIndex(self.files.len() - 1);
	}

	pub fn get(&self, source_index: SourceIndex) -> Option<&SourceFile>
	{
		return self.files.get(source_index.0);
	}
}
