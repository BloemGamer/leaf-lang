#[derive(Debug, Clone)]
pub struct SourceFile
{
	pub path: String,
	pub src: String,
}

pub type SourceIndex = usize;

#[derive(Debug, Default)]
pub struct SourceMap
{
	files: Vec<SourceFile>,
}

impl SourceMap
{
	pub fn add_file(&mut self, path: impl Into<String>, src: impl Into<String>) -> SourceIndex
	{
		let file: SourceFile = SourceFile {
			path: path.into(),
			src: src.into(),
		};
		self.files.push(file);
		return self.files.len() - 1;
	}

	pub fn get(&self, index: usize) -> &SourceFile
	{
		return &self.files[index];
	}
}
