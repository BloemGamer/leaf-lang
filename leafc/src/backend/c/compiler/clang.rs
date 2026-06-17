use leaf_proc::CCompiler;

use crate::backend::c::compiler::gcc_like::GCCLike;

#[derive(Clone, Debug, CCompiler)]
#[name(Clang)]
pub struct ClangCompiler {}
impl GCCLike for ClangCompiler
{
	fn driver_for_target(&self, _target: &crate::config::Target) -> String
	{
		return "clang".to_string();
	}
}
