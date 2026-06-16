use std::process::{Command, Output};

use leaf_proc::{compiler_bug, compiler_unable_intrinsic};

use super::{CCompiler, CompilerToolChain, IntrinsicWriter};
use crate::{
	Span,
	backend::{BackendInput, c::mono_ty_to_string},
	config::{Architecture, Environment, OperatingSystem, Optimization, Target},
	diagnostics::DiagnosticBuilder,
	lexer::{IntSign, IntSize, IntType},
	monomorphization::{MonoFunction, MonoOperand, MonoTy},
	type_analysis::Primitive,
};

pub trait GCCLike
{
	fn driver_for_target(&self, target: &Target) -> String;

	fn optimization_flags(&self, opt: &Optimization) -> Vec<String>
	{
		return match opt {
			Optimization::Debug => vec!["-O0".to_string()],
			Optimization::Release => vec!["-O3".to_string()],
		};
	}

	fn target_flags(&self, target: &Target) -> Vec<String>
	{
		let mut out: Vec<String> = Vec::new();

		// Arch / bitness.
		match target.arch {
			Architecture::X86_64 => out.push("-m64".into()),
			Architecture::X86 => out.push("-m32".into()),
			Architecture::Aarch64 => { /* gcc default on aarch64 hosts */ }
			Architecture::Arm => {
				// Default to a reasonable hard-float ARMv7 baseline; users
				// can override via `cpu`/`features`.
				out.push("-march=armv7-a".into());
			}
			Architecture::RiscV64 => {
				out.push("-march=rv64gc".into());
				out.push("-mabi=lp64d".into());
			}
			Architecture::Wasm32 => {
				// Plain gcc can't target wasm; the driver lookup should
				// have picked `emcc` or similar. Nothing to add here.
			}
			Architecture::RiscV32 => todo!(),
			Architecture::Unknown => {}
		}

		// OS / ABI quirks.
		match target.os {
			OperatingSystem::Windows => {
				// MinGW: make sure we don't accidentally link MSVC-style.
				if matches!(target.env, Environment::Gnu) {
					out.push("-mthreads".into());
				}
			}
			OperatingSystem::MacOS => {
				// TODO: I don't have a mac
			}
			OperatingSystem::Linux => {
				out.push("-fPIC".into());
			}
			OperatingSystem::Unknown => {
				// Freestanding: no libc, no startup files.
				out.push("-ffreestanding".into());
				out.push("-nostdlib".into());
			}
		}

		// Environment / ABI.
		match target.env {
			Environment::Musl => out.push("-D_GNU_SOURCE".into()),
			Environment::Gnu | Environment::Msvc | Environment::None => {}
			Environment::Gnueabi => todo!(),
			Environment::Gnueabihf => todo!(),
		}

		return out;
	}

	/// For now, returns nothing, untill features are given
	fn feature_flags(&self, _features: &str) -> Vec<String>
	{
		return Vec::new();
	}
}

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

impl<T: GCCLike> CompilerToolChain for T
{
	fn build_executable(
		&mut self,
		c_source_path: &std::path::PathBuf,
		final_path: &std::path::PathBuf,
		input: &BackendInput<'_>,
		backend: &mut crate::backend::c::CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>
	{
		let program: String = self.driver_for_target(&input.options.target);

		let mut cmd: Command = Command::new(&program);

		// Input + output.
		cmd.arg(c_source_path);
		cmd.arg("-o").arg(final_path);

		// Language: we always feed it C23
		cmd.arg("-std=c23");

		// Optimisation level.
		cmd.args(self.optimization_flags(&input.options.optimization));

		// Debug info.
		if input.options.debug_info {
			cmd.arg("-g");
		}

		// Target triple / arch tuning (cross compiles, -march, -mcpu, ...).
		cmd.args(self.target_flags(&input.options.target));
		if let Some(cpu) = &input.options.cpu {
			cmd.arg(format!("-mcpu={cpu}"));
		}
		if let Some(features) = &input.options.features {
			cmd.args(self.feature_flags(features));
		}

		// Warnings/diagnostics knobs we always want on generated C so the
		// user sees codegen bugs rather than silent UB.
		cmd.args([
			// "-Wall",
			// "-Wextra",
			// "-Wno-unused-parameter",
			// "-Wno-unused-variable",
			// "-Wno-unused-but-set-variable",
			// "-Wno-builtin-declaration-mismatch",
			"-fno-strict-aliasing", // we punt on TBAA via union puns
		]);

		cmd.arg("-lm");
		cmd.arg("-lpthread");

		let output: Output = match cmd.output() {
			Ok(o) => o,
			Err(e) => {
				return Err(vec![compiler_bug!(Span::default(), "failed to spawn `{program}`: {e}")]);
			}
		};

		if !output.status.success() {
			let stderr: String = String::from_utf8_lossy(&output.stderr).into_owned();
			let stdout: String = String::from_utf8_lossy(&output.stdout).into_owned();
			let code: String = output
				.status
				.code()
				.map_or_else(|| return "<signal>".to_string(), |c| return c.to_string());
			return Err(vec![compiler_bug!(
				Span::default(),
				"`{program}` exited with status {code}\n\
					 --- stderr ---\n{stderr}\n\
					 --- stdout ---\n{stdout}"
			)]);
		}

		if !output.stderr.is_empty() {
			let stderr: String = String::from_utf8_lossy(&output.stderr).into_owned();
			backend
				.diagnostics
				.push(compiler_bug!(Span::default(), "`{program}` warnings:\n{stderr}"));
		}

		let mut extras: Vec<std::path::PathBuf> = Vec::new();
		if input.options.debug_info {
			let dwo: std::path::PathBuf = final_path.with_extension("dwo");
			if dwo.exists() {
				extras.push(dwo);
			}
		}
		return Ok(extras);
	}

	fn build_object(
		&mut self,
		c_source_path: &std::path::PathBuf,
		final_path: &std::path::PathBuf,
		input: &BackendInput<'_>,
		backend: &mut crate::backend::c::CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>
	{
		todo!()
	}

	fn build_static_lib(
		&mut self,
		c_source_path: &std::path::PathBuf,
		final_path: &std::path::PathBuf,
		input: &BackendInput<'_>,
		backend: &mut crate::backend::c::CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>
	{
		todo!()
	}

	fn build_dynamic_lib(
		&mut self,
		c_source_path: &std::path::PathBuf,
		final_path: &std::path::PathBuf,
		input: &BackendInput<'_>,
		backend: &mut crate::backend::c::CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>
	{
		todo!()
	}

	fn build_asm(
		&mut self,
		c_source_path: &std::path::PathBuf,
		final_path: &std::path::PathBuf,
		input: &BackendInput<'_>,
		backend: &mut crate::backend::c::CBackend,
	) -> Result<Vec<std::path::PathBuf>, Vec<DiagnosticBuilder>>
	{
		todo!()
	}
}
