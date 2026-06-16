use leaf_proc::compiler_unable_intrinsic;

use super::{CCompiler, IntrinsicWriter};
use crate::{
	Span,
	backend::{BackendInput, c::mono_ty_to_string},
	diagnostics::DiagnosticBuilder,
	lexer::{IntSign, IntSize, IntType},
	monomorphization::{MonoFunction, MonoOperand, MonoTy},
	type_analysis::Primitive,
};

pub trait GCCLike {}

impl<T: GCCLike> CCompiler for T {}
impl<T: GCCLike> IntrinsicWriter for T
{
	fn write_intr_add_checked(
		&mut self,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		return self.write_checked_arith("__builtin_add_overflow", args, result_ty, f, input, backend, out);
	}

	fn write_intr_sub_checked(
		&mut self,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		return self.write_checked_arith("__builtin_sub_overflow", args, result_ty, f, input, backend, out);
	}

	fn write_intr_mul_checked(
		&mut self,
		args: &[MonoOperand],
		result_ty: Option<&MonoTy>,
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		return self.write_checked_arith("__builtin_mul_overflow", args, result_ty, f, input, backend, out);
	}

	fn write_intr_saturating_add(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		return self.write_saturating_arith("__builtin_add_overflow", args, f, input, backend, out);
	}

	fn write_intr_saturating_sub(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		return self.write_saturating_arith("__builtin_sub_overflow", args, f, input, backend, out);
	}

	fn write_intr_ctz(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		let name = gcc_int_builtin("__builtin_ctz", args[0].ty());
		return self.write_named_call(name, args, f, input, backend, out);
	}

	fn write_intr_clz(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		let name = gcc_int_builtin("__builtin_clz", args[0].ty());
		return self.write_named_call(name, args, f, input, backend, out);
	}

	fn write_intr_popcount(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		let name = gcc_int_builtin("__builtin_popcount", args[0].ty());
		return self.write_named_call(name, args, f, input, backend, out);
	}

	fn write_intr_bswap(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		let name = gcc_bswap_builtin(args[0].ty());
		return self.write_named_call(name, args, f, input, backend, out);
	}

	fn write_intr_bit_reverse(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		backend.diagnostics.push(compiler_unable_intrinsic!(
			Span::default(),
			"bit_reverse: GCC has no __builtin_bitreverse; emitting portable C23 fallback loop",
		));

		let ty = mono_ty_to_string(args[0].ty());
		write!(out, "(({{ {ty} _v = ")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		return write!(
			out,
			"; {ty} _r = 0; \
			 for (int _i = 0; _i < (int)(sizeof({ty})*8); _i++, _v >>= 1) \
			     _r = (_r << 1) | (_v & 1); \
			 _r; }}))"
		);
	}

	fn write_intr_deref(
		&mut self,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		write!(out, "(*(")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		return write!(out, "))");
	}

	fn write_intr_unreachable(
		&mut self,
		_backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		return write!(out, "__builtin_unreachable()");
	}

	fn write_saturating_arith(
		&mut self,
		op: &str,
		args: &[MonoOperand],
		f: &MonoFunction,
		input: &BackendInput<'_>,

		backend: &mut crate::backend::c::CBackend,
		out: &mut impl std::fmt::Write,
	) -> std::fmt::Result
	{
		let ty = mono_ty_to_string(args[0].ty());
		let (min_lit, max_lit) = c23_min_max_literals(args[0].ty());

		let clamp_expr = if is_signed_int(args[0].ty()) {
			format!("(_a0 < 0 ? ({ty}){min_lit} : ({ty}){max_lit})")
		} else {
			format!("({ty}){max_lit}")
		};

		write!(out, "({{ {ty} _a0 = ")?;
		self.write_operand(&args[0], f, input, backend, out)?;
		write!(out, "; {ty} _r; bool _o = {op}(_a0, ")?;
		self.write_operand(&args[1], f, input, backend, out)?;
		return write!(out, ", &_r); _o ? {clamp_expr} : _r; }})");
	}
}

fn gcc_int_builtin(base: &'static str, ty: &MonoTy) -> &'static str
{
	return match int_size_bits(ty) {
		Some(64 | 128) => match base {
			"__builtin_ctz" => "__builtin_ctzll",
			"__builtin_clz" => "__builtin_clzll",
			"__builtin_popcount" => "__builtin_popcountll",
			_ => base,
		},

		_ => base,
	};
}

fn gcc_bswap_builtin(ty: &MonoTy) -> &'static str
{
	return match int_size_bits(ty) {
		Some(16) => "__builtin_bswap16",
		Some(64) => "__builtin_bswap64",
		Some(128) => "__builtin_bswap128",
		_ => "__builtin_bswap32",
	};
}

const fn c23_min_max_literals(ty: &MonoTy) -> (&'static str, &'static str)
{
	return match ty {
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Signed,
			bits: IntSize::Fixed(8),
		})) => ("INT8_MIN", "INT8_MAX"),
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Signed,
			bits: IntSize::Fixed(16),
		})) => ("INT16_MIN", "INT16_MAX"),
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Signed,
			bits: IntSize::Fixed(32),
		})) => ("INT32_MIN", "INT32_MAX"),
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Signed,
			bits: IntSize::Fixed(64),
		})) => ("INT64_MIN", "INT64_MAX"),
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Signed,
			bits: IntSize::Fixed(128),
		})) => ("INT128_MIN", "INT128_MAX"),
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Unsigned,
			bits: IntSize::Fixed(8),
		})) => ("0", "UINT8_MAX"),
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Unsigned,
			bits: IntSize::Fixed(16),
		})) => ("0", "UINT16_MAX"),
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Unsigned,
			bits: IntSize::Fixed(32),
		})) => ("0", "UINT32_MAX"),
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Unsigned,
			bits: IntSize::Fixed(64),
		})) => ("0", "UINT64_MAX"),
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Unsigned,
			bits: IntSize::Fixed(128),
		})) => ("0", "UINT128_MAX"),
		_ => ("0", "0"),
	};
}

const fn is_signed_int(ty: &MonoTy) -> bool
{
	return matches!(
		ty,
		MonoTy::Primitive(Primitive::Int(IntType {
			sign: IntSign::Signed,
			..
		}))
	);
}

const fn int_size_bits(ty: &MonoTy) -> Option<u32>
{
	return match ty {
		MonoTy::Primitive(Primitive::Int(IntType {
			bits: IntSize::Fixed(size),
			..
		})) => Some(*size as u32),
		_ => None,
	};
}
