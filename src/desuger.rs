use crate::parser::{Ident, Program};

#[derive(Debug, Default)]
pub struct Desugarer
{
	tmp_counter: usize,
}

impl Desugarer
{
	pub fn new() -> Self
	{
		return Default::default();
	}

	fn gen_temp(&mut self, name: &str) -> Ident
	{
		let name = format!("#__tmp_{}_{}", self.tmp_counter, name); // # only internals can use it, only at the start,
		// and this will be changed in compiletime, so the users can use `__` at the start fo their variables
		self.tmp_counter += 1;
		name
	}

	pub fn desugar_program(&mut self, program: Program) -> Program
	{
		todo!()
		// Program {
		// 	items: program
		// 		.items
		// 		.into_iter()
		// 		.map(|item| self.desugar_top_level(item))
		// 		.collect(),
		// }
	}
}
