//! Built-in compiler intrinsics (`#name`).
//!
//! Every intrinsic has a known arity and a `check` method that validates
//! argument types and returns the intrinsic's concrete return type.
//! This lets the type inference system work normally through intrinsic calls.

use crate::{
	lexer::{IntSign, IntSize, IntType, Span},
	type_analysis::{Primitive, Ty, TypeError, TypeErrorKind},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Intrinsic
{
	/// `#add_checked(a: T, b: T) -> (T, bool)`
	AddChecked,
	/// `#sub_checked(a: T, b: T) -> (T, bool)`
	SubChecked,
	/// `#mul_checked(a: T, b: T) -> (T, bool)`
	MulChecked,

	/// `#add_unchecked(a: T, b: T) -> T`
	AddUnchecked,
	/// `#sub_unchecked(a: T, b: T) -> T`
	SubUnchecked,
	/// `#mul_unchecked(a: T, b: T) -> T`
	MulUnchecked,

	/// `#div(a: T, b: T) -> T`
	Div,
	/// `#rem(a: T, b: T) -> T`
	Rem,
	/// `#div_unchecked(a: T, b: T) -> T`  (UB on div-by-zero)
	DivUnchecked,
	/// `#rem_unchecked(a: T, b: T) -> T`  (UB on div-by-zero)
	RemUnchecked,

	/// `#wrapping_add(a: T, b: T) -> T`
	WrappingAdd,
	/// `#wrapping_sub(a: T, b: T) -> T`
	WrappingSub,
	/// `#wrapping_mul(a: T, b: T) -> T`
	WrappingMul,

	/// `#saturating_add(a: T, b: T) -> T`
	SaturatingAdd,
	/// `#saturating_sub(a: T, b: T) -> T`
	SaturatingSub,

	/// `#shl(val: T, shift: u32) -> T`
	Shl,
	/// `#shr(val: T, shift: u32) -> T`  - arithmetic (sign-extending for signed T)
	Shr,
	/// `#ushr(val: T, shift: u32) -> T` - logical (zero-filling regardless of sign)
	UShr,

	/// `#int_eq(a: T, b: T) -> bool`
	IntEq,
	/// `#int_ne(a: T, b: T) -> bool`
	IntNe,
	/// `#int_lt(a: T, b: T) -> bool`
	IntLt,
	/// `#int_le(a: T, b: T) -> bool`
	IntLe,
	/// `#int_gt(a: T, b: T) -> bool`
	IntGt,
	/// `#int_ge(a: T, b: T) -> bool`
	IntGe,

	/// `#fadd(a: T, b: T) -> T`
	FAdd,
	/// `#fsub(a: T, b: T) -> T`
	FSub,
	/// `#fmul(a: T, b: T) -> T`
	FMul,
	/// `#fdiv(a: T, b: T) -> T`
	FDiv,
	/// `#frem(a: T, b: T) -> T`
	FRem,
	/// `#fneg(val: T) -> T`
	FNeg,
	/// `#fma(a: T, b: T, c: T) -> T`  - fused multiply-add (a*b + c), single rounding
	Fma,
	/// `#sqrt(val: T) -> T`
	Sqrt,
	/// `#fabs(val: T) -> T`
	FAbs,
	/// `#fmin(a: T, b: T) -> T`  - propagates NaN correctly
	FMin,
	/// `#fmax(a: T, b: T) -> T`
	FMax,
	/// `#floor(val: T) -> T`
	Floor,
	/// `#ceil(val: T) -> T`
	Ceil,
	/// `#fround(val: T) -> T`  - round to nearest, ties to even
	FRound,
	/// `#ftrunc(val: T) -> T`  - round toward zero
	FTrunc,

	/// `#ctz(val: T) -> u32`   - count trailing zeros
	Ctz,
	/// `#clz(val: T) -> u32`   - count leading zeros
	Clz,
	/// `#popcount(val: T) -> u32`
	Popcount,
	/// `#bswap(val: T) -> T`   - byte-swap
	Bswap,
	/// `#bit_reverse(val: T) -> T`
	BitReverse,

	/// `#ref_deref(r: &T) -> T`
	RefDeref,
	/// `#ptr_deref(p: *T) -> T`
	PtrDeref,

	/// `#size_of(val: T) -> usize`
	/// Takes a *value* (zero-cost at runtime); backend uses the type only.
	SizeOf,
	/// `#align_of(val: T) -> usize`
	AlignOf,
	/// `#transmute(val: T) -> U`   - reinterpret bits; requires same size (checked by backend)
	/// Return type is always `Ty::Infer` here; call-site annotation required.
	Transmute,
	/// `#memcpy(dst: *mut u8, src: *u8, count: usize)`  - non-overlapping regions
	Memcpy,
	/// `#memmove(dst: *mut u8, src: *u8, count: usize)` - handles overlapping regions
	Memmove,
	/// `#memset(dst: *mut u8, val: u8, count: usize)`
	Memset,

	/// `#atomic_load(ptr: *T, ordering: u32) -> T`
	AtomicLoad,
	/// `#atomic_store(ptr: *mut T, val: T, ordering: u32)`
	AtomicStore,
	/// `#atomic_swap(ptr: *mut T, val: T, ordering: u32) -> T`
	AtomicSwap,
	/// `#atomic_cas(ptr: *mut T, expected: T, desired: T, success_ord: u32, fail_ord: u32) -> (T, bool)`
	AtomicCas,
	/// `#atomic_add(ptr: *mut T, val: T, ordering: u32) -> T`  - returns old value
	AtomicAdd,
	/// `#atomic_sub(ptr: *mut T, val: T, ordering: u32) -> T`  - returns old value
	AtomicSub,
	/// `#atomic_and(ptr: *mut T, val: T, ordering: u32) -> T`  - returns old value
	AtomicAnd,
	/// `#atomic_or(ptr: *mut T, val: T, ordering: u32) -> T`   - returns old value
	AtomicOr,
	/// `#atomic_xor(ptr: *mut T, val: T, ordering: u32) -> T`  - returns old value
	AtomicXor,
	/// `#fence(ordering: u32)`  - memory barrier without an associated memory location
	Fence,

	/// `#volatile_load(ptr: *T) -> T`
	VolatileLoad,
	/// `#volatile_store(ptr: *mut T, val: T)`
	VolatileStore,

	/// `#ptr_offset(ptr: *T, offset: isize) -> *T`
	PtrOffset,

	/// `#unreachable() -> !`
	Unreachable,
	/// `#panic(msg: &str) -> !`
	Panic,
}

impl std::fmt::Display for Intrinsic
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return write!(f, "{}", self.name());
	}
}

impl Intrinsic
{
	pub fn from_name(name: &str) -> Option<Self>
	{
		return Some(match name {
			// checked arithmetic
			"#add_checked" => Intrinsic::AddChecked,
			"#sub_checked" => Intrinsic::SubChecked,
			"#mul_checked" => Intrinsic::MulChecked,
			// unchecked arithmetic
			"#add_unchecked" => Intrinsic::AddUnchecked,
			"#sub_unchecked" => Intrinsic::SubUnchecked,
			"#mul_unchecked" => Intrinsic::MulUnchecked,
			// div / rem
			"#div" => Intrinsic::Div,
			"#rem" => Intrinsic::Rem,
			"#div_unchecked" => Intrinsic::DivUnchecked,
			"#rem_unchecked" => Intrinsic::RemUnchecked,
			// wrapping
			"#wrapping_add" => Intrinsic::WrappingAdd,
			"#wrapping_sub" => Intrinsic::WrappingSub,
			"#wrapping_mul" => Intrinsic::WrappingMul,
			// saturating
			"#saturating_add" => Intrinsic::SaturatingAdd,
			"#saturating_sub" => Intrinsic::SaturatingSub,
			// shifts
			"#shl" => Intrinsic::Shl,
			"#shr" => Intrinsic::Shr,
			"#ushr" => Intrinsic::UShr,
			// comparason
			"#int_eq" => Intrinsic::IntEq,
			"#int_ne" => Intrinsic::IntNe,
			"#int_lt" => Intrinsic::IntLt,
			"#int_le" => Intrinsic::IntLe,
			"#int_gt" => Intrinsic::IntGt,
			"#int_ge" => Intrinsic::IntGe,
			// float arithmetic
			"#fadd" => Intrinsic::FAdd,
			"#fsub" => Intrinsic::FSub,
			"#fmul" => Intrinsic::FMul,
			"#fdiv" => Intrinsic::FDiv,
			"#frem" => Intrinsic::FRem,
			"#fneg" => Intrinsic::FNeg,
			"#fma" => Intrinsic::Fma,
			"#sqrt" => Intrinsic::Sqrt,
			"#fabs" => Intrinsic::FAbs,
			"#fmin" => Intrinsic::FMin,
			"#fmax" => Intrinsic::FMax,
			"#floor" => Intrinsic::Floor,
			"#ceil" => Intrinsic::Ceil,
			"#fround" => Intrinsic::FRound,
			"#ftrunc" => Intrinsic::FTrunc,
			// bit manipulation
			"#ctz" => Intrinsic::Ctz,
			"#clz" => Intrinsic::Clz,
			"#popcount" => Intrinsic::Popcount,
			"#bswap" => Intrinsic::Bswap,
			"#bit_reverse" => Intrinsic::BitReverse,
			// deref
			"#ref_deref" => Intrinsic::RefDeref,
			"#ptr_deref" => Intrinsic::PtrDeref,
			// memory
			"#size_of" => Intrinsic::SizeOf,
			"#align_of" => Intrinsic::AlignOf,
			"#transmute" => Intrinsic::Transmute,
			"#memcpy" => Intrinsic::Memcpy,
			"#memmove" => Intrinsic::Memmove,
			"#memset" => Intrinsic::Memset,
			// atomics
			"#atomic_load" => Intrinsic::AtomicLoad,
			"#atomic_store" => Intrinsic::AtomicStore,
			"#atomic_swap" => Intrinsic::AtomicSwap,
			"#atomic_cas" => Intrinsic::AtomicCas,
			"#atomic_add" => Intrinsic::AtomicAdd,
			"#atomic_sub" => Intrinsic::AtomicSub,
			"#atomic_and" => Intrinsic::AtomicAnd,
			"#atomic_or" => Intrinsic::AtomicOr,
			"#atomic_xor" => Intrinsic::AtomicXor,
			"#fence" => Intrinsic::Fence,
			// volatile
			"#volatile_load" => Intrinsic::VolatileLoad,
			"#volatile_store" => Intrinsic::VolatileStore,
			// pointer
			"#ptr_offset" => Intrinsic::PtrOffset,
			// control flow
			"#unreachable" => Intrinsic::Unreachable,
			"#panic" => Intrinsic::Panic,
			_ => return None,
		});
	}

	pub const fn name(&self) -> &'static str
	{
		return match self {
			Intrinsic::AddChecked => "#add_checked",
			Intrinsic::SubChecked => "#sub_checked",
			Intrinsic::MulChecked => "#mul_checked",
			Intrinsic::AddUnchecked => "#add_unchecked",
			Intrinsic::SubUnchecked => "#sub_unchecked",
			Intrinsic::MulUnchecked => "#mul_unchecked",
			Intrinsic::Div => "#div",
			Intrinsic::Rem => "#rem",
			Intrinsic::DivUnchecked => "#div_unchecked",
			Intrinsic::RemUnchecked => "#rem_unchecked",
			Intrinsic::WrappingAdd => "#wrapping_add",
			Intrinsic::WrappingSub => "#wrapping_sub",
			Intrinsic::WrappingMul => "#wrapping_mul",
			Intrinsic::SaturatingAdd => "#saturating_add",
			Intrinsic::SaturatingSub => "#saturating_sub",
			Intrinsic::Shl => "#shl",
			Intrinsic::Shr => "#shr",
			Intrinsic::IntEq => "#int_eq",
			Intrinsic::IntNe => "#int_ne",
			Intrinsic::IntLt => "#int_lt",
			Intrinsic::IntLe => "#int_le",
			Intrinsic::IntGt => "#int_gt",
			Intrinsic::IntGe => "#int_ge",
			Intrinsic::UShr => "#ushr",
			Intrinsic::FAdd => "#fadd",
			Intrinsic::FSub => "#fsub",
			Intrinsic::FMul => "#fmul",
			Intrinsic::FDiv => "#fdiv",
			Intrinsic::FRem => "#frem",
			Intrinsic::FNeg => "#fneg",
			Intrinsic::Fma => "#fma",
			Intrinsic::Sqrt => "#sqrt",
			Intrinsic::FAbs => "#fabs",
			Intrinsic::FMin => "#fmin",
			Intrinsic::FMax => "#fmax",
			Intrinsic::Floor => "#floor",
			Intrinsic::Ceil => "#ceil",
			Intrinsic::FRound => "#fround",
			Intrinsic::FTrunc => "#ftrunc",
			Intrinsic::Ctz => "#ctz",
			Intrinsic::Clz => "#clz",
			Intrinsic::Popcount => "#popcount",
			Intrinsic::Bswap => "#bswap",
			Intrinsic::BitReverse => "#bit_reverse",
			Intrinsic::RefDeref => "#ref_deref",
			Intrinsic::PtrDeref => "#ptr_deref",
			Intrinsic::SizeOf => "#size_of",
			Intrinsic::AlignOf => "#align_of",
			Intrinsic::Transmute => "#transmute",
			Intrinsic::Memcpy => "#memcpy",
			Intrinsic::Memmove => "#memmove",
			Intrinsic::Memset => "#memset",
			Intrinsic::AtomicLoad => "#atomic_load",
			Intrinsic::AtomicStore => "#atomic_store",
			Intrinsic::AtomicSwap => "#atomic_swap",
			Intrinsic::AtomicCas => "#atomic_cas",
			Intrinsic::AtomicAdd => "#atomic_add",
			Intrinsic::AtomicSub => "#atomic_sub",
			Intrinsic::AtomicAnd => "#atomic_and",
			Intrinsic::AtomicOr => "#atomic_or",
			Intrinsic::AtomicXor => "#atomic_xor",
			Intrinsic::Fence => "#fence",
			Intrinsic::VolatileLoad => "#volatile_load",
			Intrinsic::VolatileStore => "#volatile_store",
			Intrinsic::PtrOffset => "#ptr_offset",
			Intrinsic::Unreachable => "#unreachable",
			Intrinsic::Panic => "#panic",
		};
	}

	#[allow(clippy::unnecessary_wraps)]
	pub const fn arity(&self) -> Option<usize>
	{
		#[allow(clippy::match_same_arms)]
		return Some(match self {
			// (a, b)
			Intrinsic::AddChecked
			| Intrinsic::SubChecked
			| Intrinsic::MulChecked
			| Intrinsic::AddUnchecked
			| Intrinsic::SubUnchecked
			| Intrinsic::MulUnchecked
			| Intrinsic::Div
			| Intrinsic::Rem
			| Intrinsic::DivUnchecked
			| Intrinsic::RemUnchecked
			| Intrinsic::WrappingAdd
			| Intrinsic::WrappingSub
			| Intrinsic::WrappingMul
			| Intrinsic::SaturatingAdd
			| Intrinsic::SaturatingSub => 2,
			// (val, shift: u32)
			Intrinsic::Shl | Intrinsic::Shr | Intrinsic::UShr => 2,
			// (a, b)
			Intrinsic::FAdd
			| Intrinsic::FSub
			| Intrinsic::FMul
			| Intrinsic::FDiv
			| Intrinsic::FRem
			| Intrinsic::FMin
			| Intrinsic::FMax => 2,
			// (a, b, c)
			Intrinsic::Fma => 3,
			// (val)
			Intrinsic::FNeg
			| Intrinsic::Sqrt
			| Intrinsic::FAbs
			| Intrinsic::Floor
			| Intrinsic::Ceil
			| Intrinsic::FRound
			| Intrinsic::FTrunc => 1,
			// (val)
			Intrinsic::Ctz
			| Intrinsic::Clz
			| Intrinsic::Popcount
			| Intrinsic::Bswap
			| Intrinsic::BitReverse
			| Intrinsic::SizeOf
			| Intrinsic::AlignOf
			| Intrinsic::Transmute => 1,
			// (dst, src, count)
			Intrinsic::Memcpy | Intrinsic::Memmove | Intrinsic::Memset => 3,
			// (ptr, ordering)
			Intrinsic::AtomicLoad => 2,
			// (ptr, val, ordering)
			Intrinsic::AtomicStore
			| Intrinsic::AtomicSwap
			| Intrinsic::AtomicAdd
			| Intrinsic::AtomicSub
			| Intrinsic::AtomicAnd
			| Intrinsic::AtomicOr
			| Intrinsic::AtomicXor => 3,
			// (ptr, expected, desired, success_ord, fail_ord)
			Intrinsic::AtomicCas => 5,
			// (ordering)
			Intrinsic::Fence => 1,
			// (ptr)
			Intrinsic::VolatileLoad => 1,
			// (ptr, val)
			Intrinsic::VolatileStore => 2,
			// (ptr, offset)
			Intrinsic::PtrOffset => 2,
			// ()
			Intrinsic::Unreachable => 0,
			// (msg)
			Intrinsic::Panic => 1,
			Intrinsic::IntEq
			| Intrinsic::IntNe
			| Intrinsic::IntLt
			| Intrinsic::IntLe
			| Intrinsic::IntGt
			| Intrinsic::IntGe => 2,
			Intrinsic::RefDeref | Intrinsic::PtrDeref => 1,
		});
	}

	pub fn param_hint(&self, index: usize, unified_int: Option<&Ty>) -> Option<Ty>
	{
		let int = || return unified_int.cloned().unwrap_or(Ty::Infer);
		let float = || return unified_int.cloned().unwrap_or(Ty::Infer);
		let u8_ty = || {
			return Ty::Primitive(Primitive::Int(IntType {
				bits: IntSize::Fixed(8),
				sign: IntSign::Unsigned,
			}));
		};
		let u32_ty = || {
			return Ty::Primitive(Primitive::Int(IntType {
				bits: IntSize::Fixed(32),
				sign: IntSign::Unsigned,
			}));
		};
		let usize_ty = || {
			return Ty::Primitive(Primitive::Int(IntType {
				bits: IntSize::Fixed(32),
				sign: IntSign::Unsigned,
			}));
		};
		let isize_ty = || {
			return Ty::Primitive(Primitive::Int(IntType {
				bits: IntSize::Size,
				sign: IntSign::Signed,
			}));
		};
		let u8_ptr = || {
			return Ty::Pointer {
				mutable: true,
				inner: Box::new(u8_ty()),
			};
		};
		let str_ref = || {
			return Ty::Reference {
				mutable: false,
				inner: Box::new(Ty::Primitive(crate::type_analysis::Primitive::Str)),
			};
		};

		return Some(match self {
			// (a: T, b: T)
			Intrinsic::AddChecked
			| Intrinsic::SubChecked
			| Intrinsic::MulChecked
			| Intrinsic::AddUnchecked
			| Intrinsic::SubUnchecked
			| Intrinsic::MulUnchecked
			| Intrinsic::Div
			| Intrinsic::Rem
			| Intrinsic::DivUnchecked
			| Intrinsic::RemUnchecked
			| Intrinsic::WrappingAdd
			| Intrinsic::WrappingSub
			| Intrinsic::WrappingMul
			| Intrinsic::SaturatingAdd
			| Intrinsic::SaturatingSub => match index {
				0 | 1 => int(),
				_ => return None,
			},
			// (val: T, shift: u32)
			Intrinsic::Shl | Intrinsic::Shr | Intrinsic::UShr => match index {
				0 => int(),
				1 => u32_ty(),
				_ => return None,
			},
			// (a: T, b: T)
			Intrinsic::FAdd
			| Intrinsic::FSub
			| Intrinsic::FMul
			| Intrinsic::FDiv
			| Intrinsic::FRem
			| Intrinsic::FMin
			| Intrinsic::FMax => match index {
				0 | 1 => float(),
				_ => return None,
			},
			// (a: T, b: T, c: T)
			Intrinsic::Fma => match index {
				0..=2 => float(),
				_ => return None,
			},
			// (val: T)
			Intrinsic::FNeg
			| Intrinsic::Sqrt
			| Intrinsic::FAbs
			| Intrinsic::Floor
			| Intrinsic::Ceil
			| Intrinsic::FRound
			| Intrinsic::FTrunc => match index {
				0 => float(),
				_ => return None,
			},
			// (val: T)
			Intrinsic::Ctz | Intrinsic::Clz | Intrinsic::Popcount | Intrinsic::Bswap | Intrinsic::BitReverse => {
				match index {
					0 => int(),
					_ => return None,
				}
			}
			// (val: T) - type inferred from call site
			Intrinsic::SizeOf
			| Intrinsic::AlignOf
			| Intrinsic::Transmute
			| Intrinsic::VolatileLoad
			| Intrinsic::RefDeref
			| Intrinsic::PtrDeref => match index {
				0 => Ty::Infer,
				_ => return None,
			},
			// (dst: *mut u8, src: *u8, count: usize)
			Intrinsic::Memcpy | Intrinsic::Memmove => match index {
				0 | 1 => u8_ptr(),
				2 => usize_ty(),
				_ => return None,
			},
			// (dst: *mut u8, val: u8, count: usize)
			Intrinsic::Memset => match index {
				0 => u8_ptr(),
				1 => u8_ty(),
				2 => usize_ty(),
				_ => return None,
			},
			// (ptr: *T, ordering: u32)
			Intrinsic::AtomicLoad => match index {
				0 => Ty::Infer,
				1 => u32_ty(),
				_ => return None,
			},
			// (ptr: *mut T, val: T, ordering: u32)
			Intrinsic::AtomicStore
			| Intrinsic::AtomicSwap
			| Intrinsic::AtomicAdd
			| Intrinsic::AtomicSub
			| Intrinsic::AtomicAnd
			| Intrinsic::AtomicOr
			| Intrinsic::AtomicXor => match index {
				0 | 1 => Ty::Infer,
				2 => u32_ty(),
				_ => return None,
			},
			// (ptr: *mut T, expected: T, desired: T, success_ord: u32, fail_ord: u32)
			Intrinsic::AtomicCas => match index {
				0..=2 => Ty::Infer,
				3 | 4 => u32_ty(),
				_ => return None,
			},
			// (ordering: u32)
			Intrinsic::Fence => match index {
				0 => u32_ty(),
				_ => return None,
			},
			// (ptr: *T)
			// (ptr: *mut T, val: T)
			Intrinsic::VolatileStore => match index {
				0 | 1 => Ty::Infer,
				_ => return None,
			},
			// (ptr: T*, offset: isize)
			Intrinsic::PtrOffset => match index {
				0 => Ty::Infer,
				1 => isize_ty(),
				_ => return None,
			},
			// ()
			Intrinsic::Unreachable => return None,
			// (msg: &str)
			Intrinsic::Panic => match index {
				0 => str_ref(),
				_ => return None,
			},
			Intrinsic::IntEq
			| Intrinsic::IntNe
			| Intrinsic::IntLt
			| Intrinsic::IntLe
			| Intrinsic::IntGt
			| Intrinsic::IntGe => match index {
				0 | 1 => int(),
				_ => return None,
			},
		});
	}

	pub fn check(&self, args: &[Ty], span: Span) -> Result<Ty, TypeError>
	{
		let err = |kind| return TypeError::new(span, kind);

		if let Some(expected) = self.arity()
			&& args.len() != expected
		{
			return Err(err(TypeErrorKind::ArgCountMismatch {
				expected,
				found: args.len(),
			}));
		}

		let is_int = |ty: &Ty| return ty.is_integer() || matches!(ty, Ty::Generic { .. } | Ty::Infer);
		let is_float = |ty: &Ty| return ty.is_float() || matches!(ty, Ty::Generic { .. } | Ty::Infer);

		let u8_ty = Ty::Primitive(Primitive::Int(IntType {
			bits: IntSize::Fixed(8),
			sign: IntSign::Unsigned,
		}));
		let u8_ptr = Ty::Pointer {
			mutable: true,
			inner: Box::new(u8_ty.clone()),
		};
		let u32_ty = Ty::Primitive(Primitive::Int(IntType {
			bits: IntSize::Fixed(32),
			sign: IntSign::Unsigned,
		}));
		let usize_ty = Ty::Primitive(Primitive::Int(IntType {
			bits: IntSize::Size,
			sign: IntSign::Unsigned,
		}));
		let isize_ty = Ty::Primitive(Primitive::Int(IntType {
			bits: IntSize::Size,
			sign: IntSign::Signed,
		}));

		let types_compat = |a: &Ty, b: &Ty| -> bool {
			return a == b
				|| matches!(a, Ty::Infer | Ty::Generic { .. })
				|| matches!(b, Ty::Infer | Ty::Generic { .. });
		};

		let checked_ret = |t: &Ty| {
			return Ty::Tuple(vec![t.clone(), Ty::Primitive(Primitive::Bool)]);
		};

		return Ok(match self {
			Intrinsic::AddChecked | Intrinsic::SubChecked | Intrinsic::MulChecked => {
				let t = &args[0];
				if !is_int(t) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "integer".to_string(),
						found: format!("{t}"),
					}));
				}
				if !types_compat(t, &args[1]) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{t}"),
						found: format!("{}", &args[1]),
					}));
				}
				checked_ret(t)
			}
			Intrinsic::IntEq
			| Intrinsic::IntNe
			| Intrinsic::IntLt
			| Intrinsic::IntLe
			| Intrinsic::IntGt
			| Intrinsic::IntGe => {
				let t = &args[0];
				if !is_int(t) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "integer".to_string(),
						found: format!("{t}"),
					}));
				}
				if !types_compat(t, &args[1]) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{t}"),
						found: format!("{}", &args[1]),
					}));
				}
				Ty::Primitive(Primitive::Bool)
			}

			Intrinsic::RefDeref => match &args[0] {
				Ty::Reference { inner, .. } | Ty::Mutable { inner } => *inner.clone(),
				Ty::Infer | Ty::Generic { .. } => Ty::Infer,
				other => {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "reference".to_string(),
						found: format!("{other}"),
					}));
				}
			},

			Intrinsic::PtrDeref => match &args[0] {
				Ty::Pointer { inner, .. } => *inner.clone(),
				Ty::Infer | Ty::Generic { .. } => Ty::Infer,
				other => {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "pointer".to_string(),
						found: format!("{other}"),
					}));
				}
			},

			Intrinsic::AddUnchecked
			| Intrinsic::SubUnchecked
			| Intrinsic::MulUnchecked
			| Intrinsic::Div
			| Intrinsic::Rem
			| Intrinsic::DivUnchecked
			| Intrinsic::RemUnchecked
			| Intrinsic::WrappingAdd
			| Intrinsic::WrappingSub
			| Intrinsic::WrappingMul
			| Intrinsic::SaturatingAdd
			| Intrinsic::SaturatingSub => {
				let t = &args[0];
				if !is_int(t) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "integer".to_string(),
						found: format!("{t}"),
					}));
				}
				if !types_compat(t, &args[1]) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{t}"),
						found: format!("{}", &args[1]),
					}));
				}
				t.clone()
			}

			Intrinsic::Shl | Intrinsic::Shr | Intrinsic::UShr => {
				let t = &args[0];
				if !is_int(t) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "integer".to_string(),
						found: format!("{t}"),
					}));
				}
				if !types_compat(&args[1], &u32_ty) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{u32_ty}"),
						found: format!("{}", &args[1]),
					}));
				}
				t.clone()
			}

			Intrinsic::FAdd
			| Intrinsic::FSub
			| Intrinsic::FMul
			| Intrinsic::FDiv
			| Intrinsic::FRem
			| Intrinsic::FMin
			| Intrinsic::FMax => {
				let t = &args[0];
				if !is_float(t) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "float".to_string(),
						found: format!("{t}"),
					}));
				}
				if !types_compat(t, &args[1]) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{t}"),
						found: format!("{}", &args[1]),
					}));
				}
				t.clone()
			}

			Intrinsic::Fma => {
				let t = &args[0];
				if !is_float(t) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "float".to_string(),
						found: format!("{t}"),
					}));
				}
				for i in 1..3 {
					if !types_compat(t, &args[i]) {
						return Err(err(TypeErrorKind::TypeMismatch {
							expected: format!("{t}"),
							found: format!("{}", &args[i]),
						}));
					}
				}
				t.clone()
			}

			Intrinsic::FNeg
			| Intrinsic::Sqrt
			| Intrinsic::FAbs
			| Intrinsic::Floor
			| Intrinsic::Ceil
			| Intrinsic::FRound
			| Intrinsic::FTrunc => {
				let t = &args[0];
				if !is_float(t) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "float".to_string(),
						found: format!("{t}"),
					}));
				}
				t.clone()
			}

			Intrinsic::Ctz | Intrinsic::Clz | Intrinsic::Popcount => {
				if !is_int(&args[0]) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "integer".to_string(),
						found: format!("{}", &args[0]),
					}));
				}
				u32_ty
			}

			Intrinsic::Bswap | Intrinsic::BitReverse => {
				let t = &args[0];
				if !is_int(t) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "integer".to_string(),
						found: format!("{t}"),
					}));
				}
				t.clone()
			}

			Intrinsic::SizeOf | Intrinsic::AlignOf => usize_ty,

			Intrinsic::Transmute => Ty::Infer,

			Intrinsic::Memcpy | Intrinsic::Memmove => {
				for (i, expected) in [&u8_ptr, &u8_ptr, &usize_ty].iter().enumerate() {
					if !types_compat(&args[i], expected) {
						return Err(err(TypeErrorKind::TypeMismatch {
							expected: format!("{expected}"),
							found: format!("{}", &args[i]),
						}));
					}
				}
				Ty::Unit
			}

			Intrinsic::Memset => {
				for (i, expected) in [&u8_ptr, &u8_ty, &usize_ty].iter().enumerate() {
					if !types_compat(&args[i], expected) {
						return Err(err(TypeErrorKind::TypeMismatch {
							expected: format!("{expected}"),
							found: format!("{}", &args[i]),
						}));
					}
				}
				Ty::Unit
			}

			Intrinsic::AtomicLoad => {
				let ptr_ty = &args[0];
				if !matches!(ptr_ty, Ty::Pointer { .. } | Ty::Infer | Ty::Generic { .. }) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "pointer".to_string(),
						found: format!("{ptr_ty}"),
					}));
				}
				if !types_compat(&args[1], &u32_ty) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{u32_ty}"),
						found: format!("{}", &args[1]),
					}));
				}
				match ptr_ty {
					Ty::Pointer { inner, .. } => *inner.clone(),
					_ => Ty::Infer,
				}
			}

			Intrinsic::AtomicStore => {
				if !matches!(
					&args[0],
					Ty::Pointer { mutable: true, .. } | Ty::Infer | Ty::Generic { .. }
				) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "*mut T".to_string(),
						found: format!("{}", &args[0]),
					}));
				}
				if !types_compat(&args[2], &u32_ty) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{u32_ty}"),
						found: format!("{}", &args[2]),
					}));
				}
				Ty::Unit
			}

			Intrinsic::AtomicSwap
			| Intrinsic::AtomicAdd
			| Intrinsic::AtomicSub
			| Intrinsic::AtomicAnd
			| Intrinsic::AtomicOr
			| Intrinsic::AtomicXor => {
				if !matches!(
					&args[0],
					Ty::Pointer { mutable: true, .. } | Ty::Infer | Ty::Generic { .. }
				) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "*mut T".to_string(),
						found: format!("{}", &args[0]),
					}));
				}
				if !types_compat(&args[2], &u32_ty) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{u32_ty}"),
						found: format!("{}", &args[2]),
					}));
				}
				match &args[0] {
					Ty::Pointer { inner, .. } => *inner.clone(),
					_ => args[1].clone(),
				}
			}

			Intrinsic::AtomicCas => {
				if !matches!(
					&args[0],
					Ty::Pointer { mutable: true, .. } | Ty::Infer | Ty::Generic { .. }
				) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "*mut T".to_string(),
						found: format!("{}", &args[0]),
					}));
				}
				if !types_compat(&args[3], &u32_ty) || !types_compat(&args[4], &u32_ty) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{u32_ty}"),
						found: "ordering".to_string(),
					}));
				}
				let val_ty = match &args[0] {
					Ty::Pointer { inner, .. } => *inner.clone(),
					_ => args[1].clone(),
				};
				checked_ret(&val_ty)
			}

			Intrinsic::Fence => {
				if !types_compat(&args[0], &u32_ty) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{u32_ty}"),
						found: format!("{}", &args[0]),
					}));
				}
				Ty::Unit
			}

			Intrinsic::VolatileLoad => {
				let ptr_ty = &args[0];
				if !matches!(ptr_ty, Ty::Pointer { .. } | Ty::Infer | Ty::Generic { .. }) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "pointer".to_string(),
						found: format!("{ptr_ty}"),
					}));
				}
				match ptr_ty {
					Ty::Pointer { inner, .. } => *inner.clone(),
					_ => Ty::Infer,
				}
			}

			Intrinsic::VolatileStore => {
				if !matches!(
					&args[0],
					Ty::Pointer { mutable: true, .. } | Ty::Infer | Ty::Generic { .. }
				) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "*mut T".to_string(),
						found: format!("{}", &args[0]),
					}));
				}
				Ty::Unit
			}

			Intrinsic::PtrOffset => {
				let ptr_ty = &args[0];
				if !matches!(ptr_ty, Ty::Pointer { .. } | Ty::Infer | Ty::Generic { .. }) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: "pointer".to_string(),
						found: format!("{ptr_ty}"),
					}));
				}
				if !types_compat(&args[1], &isize_ty) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{isize_ty}"),
						found: format!("{}", &args[1]),
					}));
				}
				ptr_ty.clone()
			}

			Intrinsic::Unreachable => Ty::Never,

			Intrinsic::Panic => {
				let str_ref = Ty::Reference {
					mutable: false,
					inner: Box::new(Ty::Primitive(Primitive::Str)),
				};
				if !types_compat(&args[0], &str_ref) {
					return Err(err(TypeErrorKind::TypeMismatch {
						expected: format!("{str_ref}"),
						found: format!("{}", &args[0]),
					}));
				}
				Ty::Never
			}
		});
	}
}
