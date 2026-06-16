#![allow(clippy::too_many_arguments)]
#![allow(clippy::module_inception)]
#![allow(clippy::module_name_repetitions)]

use std::fmt::Write;

use crate::{
	CBackend,
	backend::{
		BackendInput,
		c::{mono_ty_to_string, tuple_type_name},
	},
	diagnostics::DiagnosticBuilder,
	monomorphization::{MonoFunction, MonoOperand, MonoTy},
	type_analysis::{Primitive, intrinsics::Intrinsic},
};

pub mod gcc;
pub mod gcc_like;

#[derive(Clone, Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum CCompilers
{
	GCC(gcc::GCCCompiler),
}

macro_rules! delegate {
    ($self:ident, $method:ident ( $($args:expr),* $(,)? )) => {
        match $self {
            Self::GCC(e) => e.$method($($args),*),
        }
    };

    ($self:ident, $method:ident) => {
        match $self {
            Self::GCC(e) => e.$method(),
        }
    };
}

impl CCompilers
{
	pub fn write_intrinsic(
		&mut self,
		intr: &Intrinsic,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return delegate!(self, write_intrinsic(intr, args, result_ty, f, input, backend, out,));
	}
}

impl CompilerToolChain for CCompilers
{
	fn build_executable(
		&mut self,
		c_source_path: &std::path::Path,
		final_path: &std::path::Path,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>
	{
		return delegate!(self, build_executable(c_source_path, final_path, input, backend));
	}

	fn build_object(
		&mut self,
		c_source_path: &std::path::Path,
		final_path: &std::path::Path,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>
	{
		return delegate!(self, build_object(c_source_path, final_path, input, backend));
	}

	fn build_static_lib(
		&mut self,
		c_source_path: &std::path::Path,
		final_path: &std::path::Path,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>
	{
		return delegate!(self, build_static_lib(c_source_path, final_path, input, backend));
	}

	fn build_dynamic_lib(
		&mut self,
		c_source_path: &std::path::Path,
		final_path: &std::path::Path,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>
	{
		return delegate!(self, build_dynamic_lib(c_source_path, final_path, input, backend));
	}

	fn build_asm(
		&mut self,
		c_source_path: &std::path::Path,
		final_path: &std::path::Path,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>
	{
		return delegate!(self, build_asm(c_source_path, final_path, input, backend));
	}
}

pub trait CCompiler: IntrinsicWriter + CompilerToolChain {}

pub trait IntrinsicWriter
{
	fn write_intrinsic(
		&mut self,
		intr: &Intrinsic,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return match intr {
			// arithmetic
			Intrinsic::AddUnchecked | Intrinsic::WrappingAdd => self.write_intr_add(args, f, input, backend, out),
			Intrinsic::SubUnchecked | Intrinsic::WrappingSub => self.write_intr_sub(args, f, input, backend, out),
			Intrinsic::MulUnchecked | Intrinsic::WrappingMul => self.write_intr_mul(args, f, input, backend, out),
			Intrinsic::Div | Intrinsic::DivUnchecked => self.write_intr_div(args, f, input, backend, out),
			Intrinsic::Rem | Intrinsic::RemUnchecked => self.write_intr_rem(args, f, input, backend, out),

			// checked arithmetic
			Intrinsic::AddChecked => self.write_intr_add_checked(args, result_ty, f, input, backend, out),
			Intrinsic::SubChecked => self.write_intr_sub_checked(args, result_ty, f, input, backend, out),
			Intrinsic::MulChecked => self.write_intr_mul_checked(args, result_ty, f, input, backend, out),

			// saturating
			Intrinsic::SaturatingAdd => self.write_intr_saturating_add(args, f, input, backend, out),
			Intrinsic::SaturatingSub => self.write_intr_saturating_sub(args, f, input, backend, out),

			// shifts
			Intrinsic::Shl => self.write_intr_shl(args, f, input, backend, out),
			Intrinsic::Shr => self.write_intr_shr(args, f, input, backend, out),
			Intrinsic::UShr => self.write_intr_ushr(args, f, input, backend, out),

			// integer comparisons
			Intrinsic::IntEq => self.write_intr_int_eq(args, f, input, backend, out),
			Intrinsic::IntNe => self.write_intr_int_ne(args, f, input, backend, out),
			Intrinsic::IntLt => self.write_intr_int_lt(args, f, input, backend, out),
			Intrinsic::IntLe => self.write_intr_int_le(args, f, input, backend, out),
			Intrinsic::IntGt => self.write_intr_int_gt(args, f, input, backend, out),
			Intrinsic::IntGe => self.write_intr_int_ge(args, f, input, backend, out),

			// float arithmetic
			Intrinsic::FAdd => self.write_intr_fadd(args, f, input, backend, out),
			Intrinsic::FSub => self.write_intr_fsub(args, f, input, backend, out),
			Intrinsic::FMul => self.write_intr_fmul(args, f, input, backend, out),
			Intrinsic::FDiv => self.write_intr_fdiv(args, f, input, backend, out),
			Intrinsic::FRem => self.write_intr_frem(args, f, input, backend, out),
			Intrinsic::FNeg => self.write_intr_fneg(args, f, input, backend, out),
			Intrinsic::Fma => self.write_intr_fma(args, f, input, backend, out),

			// float math
			Intrinsic::Sqrt => self.write_intr_sqrt(args, f, input, backend, out),
			Intrinsic::FAbs => self.write_intr_fabs(args, f, input, backend, out),
			Intrinsic::FMin => self.write_intr_fmin(args, f, input, backend, out),
			Intrinsic::FMax => self.write_intr_fmax(args, f, input, backend, out),
			Intrinsic::Floor => self.write_intr_floor(args, f, input, backend, out),
			Intrinsic::Ceil => self.write_intr_ceil(args, f, input, backend, out),
			Intrinsic::FRound => self.write_intr_fround(args, f, input, backend, out),
			Intrinsic::FTrunc => self.write_intr_ftrunc(args, f, input, backend, out),

			// bit manipulation
			Intrinsic::Ctz => self.write_intr_ctz(args, f, input, backend, out),
			Intrinsic::Clz => self.write_intr_clz(args, f, input, backend, out),
			Intrinsic::Popcount => self.write_intr_popcount(args, f, input, backend, out),
			Intrinsic::Bswap => self.write_intr_bswap(args, f, input, backend, out),
			Intrinsic::BitReverse => self.write_intr_bit_reverse(args, f, input, backend, out),

			// deref
			Intrinsic::RefDeref | Intrinsic::PtrDeref => self.write_intr_deref(args, f, input, backend, out),

			// type queries / transmute
			Intrinsic::SizeOf => self.write_intr_size_of(args, f, input, backend, out),
			Intrinsic::AlignOf => self.write_intr_align_of(args, f, input, backend, out),
			Intrinsic::Transmute => self.write_intr_transmute(args, result_ty, f, input, backend, out),

			// memory
			Intrinsic::Memcpy => self.write_intr_memcpy(args, f, input, backend, out),
			Intrinsic::Memmove => self.write_intr_memmove(args, f, input, backend, out),
			Intrinsic::Memset => self.write_intr_memset(args, f, input, backend, out),

			// atomics
			Intrinsic::AtomicLoad => self.write_intr_atomic_load(args, f, input, backend, out),
			Intrinsic::AtomicStore => self.write_intr_atomic_store(args, f, input, backend, out),
			Intrinsic::AtomicSwap => self.write_intr_atomic_swap(args, f, input, backend, out),
			Intrinsic::AtomicAdd => self.write_intr_atomic_add(args, f, input, backend, out),
			Intrinsic::AtomicSub => self.write_intr_atomic_sub(args, f, input, backend, out),
			Intrinsic::AtomicAnd => self.write_intr_atomic_and(args, f, input, backend, out),
			Intrinsic::AtomicOr => self.write_intr_atomic_or(args, f, input, backend, out),
			Intrinsic::AtomicXor => self.write_intr_atomic_xor(args, f, input, backend, out),
			Intrinsic::AtomicCas => self.write_intr_atomic_cas(args, result_ty, f, input, backend, out),
			Intrinsic::Fence => self.write_intr_fence(args, f, input, backend, out),

			// volatile
			Intrinsic::VolatileLoad => self.write_intr_volatile_load(args, f, input, backend, out),
			Intrinsic::VolatileStore => self.write_intr_volatile_store(args, f, input, backend, out),

			// pointer arithmetic
			Intrinsic::PtrOffset => self.write_intr_ptr_offset(args, f, input, backend, out),

			// control flow
			Intrinsic::Unreachable => self.write_intr_unreachable(backend, out),
			Intrinsic::Panic => self.write_intr_panic(args, f, input, backend, out),
		};
	}

	fn write_operand(
		&mut self,
		op: &MonoOperand,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return backend.write_operand(op, f, input, out);
	}

	fn write_intr_add(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("+", args, f, input, backend, out);
	}

	fn write_intr_sub(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("-", args, f, input, backend, out);
	}

	fn write_intr_mul(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("*", args, f, input, backend, out);
	}

	fn write_intr_div(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("/", args, f, input, backend, out);
	}

	fn write_intr_rem(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("%", args, f, input, backend, out);
	}

	fn write_intr_add_checked(
		&mut self,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_sub_checked(
		&mut self,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_mul_checked(
		&mut self,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_saturating_add(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_saturating_sub(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_shl(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("<<", args, f, input, backend, out);
	}

	fn write_intr_shr(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop(">>", args, f, input, backend, out);
	}

	fn write_intr_ushr(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		let t = mono_ty_to_string(args[0].ty());
		write!(out, "(({t}) ((u{t}) (")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, ") >> ")?;
		self.write_operand(&args[1], f, input, backend, out)?;
		return write!(out, "))");
	}

	fn write_intr_int_eq(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("==", args, f, input, backend, out);
	}
	fn write_intr_int_ne(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("!=", args, f, input, backend, out);
	}
	fn write_intr_int_lt(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("<", args, f, input, backend, out);
	}
	fn write_intr_int_le(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("<=", args, f, input, backend, out);
	}
	fn write_intr_int_gt(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop(">", args, f, input, backend, out);
	}
	fn write_intr_int_ge(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop(">=", args, f, input, backend, out);
	}

	fn write_intr_fadd(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("+", args, f, input, backend, out);
	}
	fn write_intr_fsub(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("-", args, f, input, backend, out);
	}
	fn write_intr_fmul(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("*", args, f, input, backend, out);
	}
	fn write_intr_fdiv(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_binop("/", args, f, input, backend, out);
	}

	fn write_intr_frem(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_libm_call(f32_or_f64(args[0].ty(), "fmodf", "fmod"), args, f, input, backend, out);
	}

	fn write_intr_fneg(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		write!(out, "(-")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		return write!(out, ")");
	}

	fn write_intr_fma(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_libm_call(f32_or_f64(args[0].ty(), "fmaf", "fma"), args, f, input, backend, out);
	}

	fn write_intr_sqrt(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_libm_call(f32_or_f64(args[0].ty(), "sqrtf", "sqrt"), args, f, input, backend, out);
	}

	fn write_intr_fabs(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_libm_call(f32_or_f64(args[0].ty(), "fabsf", "fabs"), args, f, input, backend, out);
	}

	fn write_intr_fmin(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_libm_call(f32_or_f64(args[0].ty(), "fminf", "fmin"), args, f, input, backend, out);
	}

	fn write_intr_fmax(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_libm_call(f32_or_f64(args[0].ty(), "fmaxf", "fmax"), args, f, input, backend, out);
	}

	fn write_intr_floor(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_libm_call(
			f32_or_f64(args[0].ty(), "floorf", "floor"),
			args,
			f,
			input,
			backend,
			out,
		);
	}

	fn write_intr_ceil(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_libm_call(f32_or_f64(args[0].ty(), "ceilf", "ceil"), args, f, input, backend, out);
	}

	fn write_intr_fround(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_libm_call(f32_or_f64(args[0].ty(), "rintf", "rint"), args, f, input, backend, out);
	}

	fn write_intr_ftrunc(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_libm_call(
			f32_or_f64(args[0].ty(), "truncf", "trunc"),
			args,
			f,
			input,
			backend,
			out,
		);
	}

	fn write_intr_ctz(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_clz(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_popcount(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_bswap(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_bit_reverse(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_deref(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;

	fn write_intr_size_of(
		&mut self,
		args: &[MonoOperand],
		_f: &MonoFunction,
		_input: &BackendInput<'_>,
		_backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return write!(out, "sizeof({})", mono_ty_to_string(args[0].ty()));
	}

	fn write_intr_align_of(
		&mut self,
		args: &[MonoOperand],
		_f: &MonoFunction,
		_input: &BackendInput<'_>,
		_backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return write!(out, "alignof({})", mono_ty_to_string(args[0].ty()));
	}

	fn write_intr_transmute(
		&mut self,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		let from_ty = mono_ty_to_string(args[0].ty());
		let to_ty = result_ty.map_or_else(|| return "/* unknown */".to_string(), mono_ty_to_string);
		write!(out, "({{ union {{ {from_ty} _f; {to_ty} _t; }} _u; _u._f = ")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		return write!(out, "; _u._t; }})");
	}

	fn write_intr_memcpy(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_named_call("memcpy", args, f, input, backend, out);
	}

	fn write_intr_memmove(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_named_call("memmove", args, f, input, backend, out);
	}

	fn write_intr_memset(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_named_call("memset", args, f, input, backend, out);
	}

	fn write_intr_atomic_load(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		write!(out, "atomic_load_explicit(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, ", ")?;
		self.write_operand(&args[1], f, input, backend, out)?;
		return write!(out, ")");
	}

	fn write_intr_atomic_store(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		write!(out, "atomic_store_explicit(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, ", ")?;
		self.write_operand(&args[1], f, input, backend, out)?;
		write!(out, ", ")?;
		self.write_operand(&args[2], f, input, backend, out)?;
		return write!(out, ")");
	}

	fn write_intr_atomic_swap(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_atomic_rmw("atomic_exchange_explicit", args, f, input, backend, out);
	}

	fn write_intr_atomic_add(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_atomic_rmw("atomic_fetch_add_explicit", args, f, input, backend, out);
	}

	fn write_intr_atomic_sub(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_atomic_rmw("atomic_fetch_sub_explicit", args, f, input, backend, out);
	}

	fn write_intr_atomic_and(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_atomic_rmw("atomic_fetch_and_explicit", args, f, input, backend, out);
	}

	fn write_intr_atomic_or(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_atomic_rmw("atomic_fetch_or_explicit", args, f, input, backend, out);
	}

	fn write_intr_atomic_xor(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_atomic_rmw("atomic_fetch_xor_explicit", args, f, input, backend, out);
	}

	fn write_intr_atomic_cas(
		&mut self,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		let val_ty = mono_ty_to_string(args[1].ty());
		let tuple_name = match result_ty {
			Some(MonoTy::Tuple(elems)) => tuple_type_name(elems),
			_ => "/* unknown cas return */".to_string(),
		};
		write!(out, "({{ {val_ty} _exp = ")?;
		self.write_operand(&args[1], f, input, backend, out)?;
		write!(out, "; bool _ok = atomic_compare_exchange_strong_explicit(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, ", &_exp, ")?;
		self.write_operand(&args[2], f, input, backend, out)?;
		write!(out, ", false, ")?;
		self.write_operand(&args[3], f, input, backend, out)?;
		write!(out, ", ")?;
		self.write_operand(&args[4], f, input, backend, out)?;
		return write!(out, "); ({tuple_name}){{ ._0 = _exp, ._1 = _ok }}; }})");
	}

	fn write_intr_fence(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		write!(out, "atomic_thread_fence(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		return write!(out, ")");
	}

	fn write_intr_volatile_load(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		let inner_ty = volatile_inner_ty(args[0].ty());
		write!(out, "(*(volatile {inner_ty} *)(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		return write!(out, "))");
	}

	fn write_intr_volatile_store(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		let inner_ty = volatile_inner_ty(args[0].ty());
		write!(out, "(*(volatile {inner_ty} *)(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, ") = ")?;
		self.write_operand(&args[1], f, input, backend, out)?;
		return write!(out, ")");
	}

	fn write_intr_ptr_offset(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		write!(out, "(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, " + ")?;
		self.write_operand(&args[1], f, input, backend, out)?;
		return write!(out, ")");
	}

	fn write_intr_unreachable(&mut self, _backend: &mut CBackend, out: &mut impl Write) -> std::fmt::Result;

	fn write_intr_panic(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		write!(out, r#"(fprintf(stderr, "%.*s\n", (int)"#)?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, ".len, ")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		return write!(out, ".data), abort(), 0)");
	}

	fn write_binop(
		&mut self,
		op: &str,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		write!(out, "(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, " {op} ")?;
		self.write_operand(&args[1], f, input, backend, out)?;
		return write!(out, ")");
	}

	fn write_named_call(
		&mut self,
		name: &str,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		write!(out, "{name}(")?;
		for (i, a) in args.iter().enumerate() {
			if i > 0 {
				write!(out, ", ")?;
			}
			self.write_operand(a, f, input, backend, out)?;
		}
		return write!(out, ")");
	}

	fn write_libm_call(
		&mut self,
		name: &str,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return self.write_named_call(name, args, f, input, backend, out);
	}

	fn write_atomic_rmw(
		&mut self,
		builtin: &str,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		write!(out, "{builtin}(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, ", ")?;
		self.write_operand(&args[1], f, input, backend, out)?;
		write!(out, ", ")?;
		self.write_operand(&args[2], f, input, backend, out)?;
		return write!(out, ")");
	}

	fn write_checked_arith(
		&mut self,
		builtin: &str,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		let elem_ty = mono_ty_to_string(args[0].ty());
		let tuple_name = match result_ty {
			Some(MonoTy::Tuple(elems)) => tuple_type_name(elems),
			_ => "/* unknown checked return */".to_string(),
		};
		write!(out, "({{ {elem_ty} _r; bool _o = {builtin}(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, ", ")?;
		self.write_operand(&args[1], f, input, backend, out)?;
		return write!(out, ", &_r); ({tuple_name}){{ ._0 = _r, ._1 = _o }}; }})");
	}

	fn write_saturating_arith(
		&mut self,
		op: &str,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut CBackend,
		out: &mut impl Write,
	) -> std::fmt::Result;
}

const fn f32_or_f64<'a>(ty: &MonoTy, f32_name: &'a str, f64_name: &'a str) -> &'a str
{
	match ty {
		MonoTy::Primitive(Primitive::F32) => return f32_name,
		_ => return f64_name,
	}
}

fn volatile_inner_ty(ty: &MonoTy) -> String
{
	match ty {
		MonoTy::Pointer { inner, .. } | MonoTy::Reference { inner, .. } => return mono_ty_to_string(inner),
		_ => return "/* not a pointer */".to_string(),
	}
}

pub trait CompilerToolChain
{
	fn build_executable(
		&mut self,
		c_source_path: &std::path::Path,
		final_path: &std::path::Path,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>;

	fn build_object(
		&mut self,
		c_source_path: &std::path::Path,
		final_path: &std::path::Path,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>;

	fn build_static_lib(
		&mut self,
		c_source_path: &std::path::Path,
		final_path: &std::path::Path,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>;

	fn build_dynamic_lib(
		&mut self,
		c_source_path: &std::path::Path,
		final_path: &std::path::Path,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>;

	fn build_asm(
		&mut self,
		c_source_path: &std::path::Path,
		final_path: &std::path::Path,
		input: &BackendInput<'_>,
		backend: &mut CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>;
}
