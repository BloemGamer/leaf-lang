use leaf_proc::CCompiler;

use crate::backend::c::compiler::gcc_like::GCCLike;

#[derive(Clone, Debug, CCompiler)]
#[name(GCC)]
pub struct GCCCompiler {}
impl GCCLike for GCCCompiler {}
