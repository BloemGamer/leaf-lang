#![allow(clippy::module_name_repetitions)]

// TODO: change to the new error system
// TODO: rewrite this probably, I there are a lot of weird things in here, and the reflection system probs needs to be at least partly in here
// TODO: runtime const/mut is not yet checked

#[cfg(test)]
#[path = "../../tests/type_analysis/tests.rs"]
mod tests;

pub mod display;
pub mod intrinsics;

use std::{
	collections::{HashMap, HashSet},
	fmt,
};

use leaf_proc::Spanned;

use crate::{
	diagnostics::{CompileDiagnostic, CompileError, DiagnosticBuilder, ErrorCode},
	lexer::{IntSign, IntSize, IntType, Span, Spanned, StringFlags},
	name_resolution::{
		ResolvedArrayLiteral, ResolvedAssocTypeDecl, ResolvedBlock, ResolvedDirective, ResolvedDirectiveNode,
		ResolvedEnumDecl, ResolvedExpr, ResolvedFuncBound, ResolvedFunctionDecl, ResolvedFunctionSignature,
		ResolvedGenericArg, ResolvedGenericHeapKind, ResolvedGenericHeapParam, ResolvedImplDecl, ResolvedImplItem,
		ResolvedModule, ResolvedModuleDecl, ResolvedPath, ResolvedPathKind, ResolvedPattern, ResolvedRangeExpr,
		ResolvedStmt, ResolvedStructDecl, ResolvedSwitchBody, ResolvedTopLevelBlock, ResolvedTopLevelDecl,
		ResolvedTraitDecl, ResolvedTraitItem, ResolvedType, ResolvedTypeAliasDecl, ResolvedTypeCore, ResolvedUnionDecl,
		ResolvedVariableDecl, ResolvedVariantDecl, ResolvedWhereBound, ResolvedWhereConstraint,
	},
	parser::{self, AssignOp, BinaryOp, CallType, Literal, PathSegment, UnaryOp, read_radix_number},
	source_map::SourceIndex,
	symbol_collection::{GlobalSymbolTable, Scope, Symbol, SymbolId, SymbolKind},
	type_analysis::intrinsics::Intrinsic,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primitive
{
	Bool,
	Char,
	Int(IntType),
	F32,
	F64,
	Str,
	CStr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty
{
	Primitive(Primitive),

	Unit,

	Never,

	Named
	{
		symbol: SymbolId,
		generics: Vec<Ty>,
	},

	Reference
	{
		mutable: bool,
		inner: Box<Ty>,
	},
	Mutable
	{
		inner: Box<Ty>,
	},
	Pointer
	{
		mutable: bool,
		inner: Box<Ty>,
	},

	Array
	{
		inner: Box<Ty>,
		size: Option<u64>,
	},
	Tuple(Vec<Ty>),

	Generic
	{
		name: String,
		bounds: Vec<TyBound>,
	},

	ImplTrait
	{
		bounds: Vec<TyBound>,
		concrete: Option<Box<Ty>>,
	},

	Infer,

	#[allow(clippy::enum_variant_names)]
	SelfTy,
}

impl Ty
{
	pub fn implements_copy(&self, trait_impls: &HashMap<TyKey, HashSet<SymbolId>>, copy_sym: SymbolId) -> bool
	{
		return match self {
			Ty::Primitive(_) | Ty::Unit | Ty::Never | Ty::Pointer { .. } => true,

			Ty::Mutable { .. } | Ty::Infer | Ty::SelfTy => false,

			Ty::Reference { mutable, .. } => !mutable,

			Ty::Tuple(elems) => elems.iter().all(|t| return t.implements_copy(trait_impls, copy_sym)),

			Ty::Array { inner, size } => size.is_some() && inner.implements_copy(trait_impls, copy_sym),

			Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => bounds.iter().any(|b| {
				return matches!(b, TyBound::Trait { symbol, .. } if *symbol == copy_sym);
			}),

			Ty::Named { .. } => {
				let Some(key) = TyKey::of(self) else { return false };
				trait_impls.get(&key).is_some_and(|set| return set.contains(&copy_sym))
			}
		};
	}
}

impl Primitive
{
	const fn int_from_int_type(int_type: IntType) -> Primitive
	{
		return Primitive::Int(int_type);
	}
}

impl Ty
{
	pub const fn is_integer(&self) -> bool
	{
		return matches!(self, Ty::Primitive(Primitive::Int(_)));
	}

	pub const fn is_float(&self) -> bool
	{
		return matches!(self, Ty::Primitive(Primitive::F32 | Primitive::F64));
	}

	pub const fn is_numeric(&self) -> bool
	{
		return self.is_integer() || self.is_float();
	}

	pub fn is_assignable_to(&self, expected: &Ty) -> bool
	{
		if *self == Ty::Never {
			return true;
		}
		if *self == Ty::Infer || matches!(expected, Ty::Infer) {
			return true;
		}
		if *self == Ty::SelfTy || matches!(expected, Ty::SelfTy) {
			return true;
		}
		if matches!(expected, Ty::Generic { .. }) {
			return true;
		}

		if let (Ty::Generic { name: n1, .. }, Ty::Generic { name: n2, .. }) = (self, expected) {
			return n1 == n2;
		}

		return match (self, expected) {
			(Ty::Primitive(Primitive::Int(a)), Ty::Primitive(Primitive::Int(b))) => a == b,

			(
				Ty::Named {
					symbol: s1,
					generics: g1,
				},
				Ty::Named {
					symbol: s2,
					generics: g2,
				},
			) => {
				if s1 != s2 {
					return false;
				}
				match (g1.is_empty(), g2.is_empty()) {
					(false, false) => {
						g1.len() == g2.len() && g1.iter().zip(g2).all(|(a, b)| return a.is_assignable_to(b))
					}
					_ => true,
				}
			}

			(Ty::Reference { mutable: m1, inner: i1 }, Ty::Reference { mutable: m2, inner: i2 })
			| (Ty::Pointer { mutable: m1, inner: i1 }, Ty::Pointer { mutable: m2, inner: i2 }) => {
				(*m1 || !m2) && i1.is_assignable_to(i2)
			}

			(Ty::Mutable { inner: i1 }, Ty::Mutable { inner: i2 }) => i1.is_assignable_to(i2),

			(Ty::Array { inner: i1, size: s1 }, Ty::Array { inner: i2, size: s2 }) => {
				i1.is_assignable_to(i2) && (s2.is_none() || s1 == s2)
			}

			(Ty::Tuple(ts1), Ty::Tuple(ts2)) => {
				ts1.len() == ts2.len() && ts1.iter().zip(ts2).all(|(a, b)| return a.is_assignable_to(b))
			}
			(Ty::Unit, Ty::Tuple(e)) if e.is_empty() => true,
			(Ty::Tuple(e), Ty::Unit) if e.is_empty() => true,
			(Ty::Unit, Ty::Unit) => true,

			_ => self == expected,
		};
	}

	pub fn impl_trait_binding_hints(&self) -> Vec<&Ty>
	{
		let Ty::ImplTrait { bounds, .. } = self else {
			return Vec::new();
		};
		return bounds
			.iter()
			.flat_map(|b| {
				let TyBound::Trait { args, .. } = b else {
					return [].iter();
				};
				return args.iter();
			})
			.filter_map(|a| {
				let TyGenericArg::Binding { ty, .. } = a else {
					return None;
				};
				return Some(ty);
			})
			.collect();
	}

	pub const fn named(symbol: SymbolId) -> Ty
	{
		return Ty::Named {
			symbol,
			generics: Vec::new(),
		};
	}

	pub fn from_primitive_name(name: &str) -> Option<Ty>
	{
		return Some(match name {
			"bool" => Ty::Primitive(Primitive::Bool),
			"char" => Ty::Primitive(Primitive::Char),
			"f32" => Ty::Primitive(Primitive::F32),
			"f64" => Ty::Primitive(Primitive::F64),
			"str" => Ty::Primitive(Primitive::Str),
			"cstr" => Ty::Primitive(Primitive::CStr),
			"!" => Ty::Never,
			"_" => Ty::Infer,
			"Self" => Ty::SelfTy,
			_ => return lower_int_ty(name, &GenericScope::default()),
		});
	}

	pub const fn as_primitive(&self) -> Option<&Primitive>
	{
		return if let Ty::Primitive(p) = self { Some(p) } else { None };
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKey
{
	Symbol(SymbolId),
	Prim(Primitive),
}

impl TyKey
{
	pub fn of(ty: &Ty) -> Option<TyKey>
	{
		return match ty {
			Ty::Named { symbol, .. } => Some(TyKey::Symbol(*symbol)),
			Ty::Primitive(p) => Some(TyKey::Prim(p.clone())),
			_ => None,
		};
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyBound
{
	Trait
	{
		symbol: SymbolId, args: Vec<TyGenericArg>
	},
	Fn
	{
		args: Vec<Ty>, ret: Box<Ty>
	},
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyGenericArg
{
	Type(Ty),
	Binding
	{
		name: String,
		ty: Ty,
	},
}

#[derive(Debug, Clone, Default)]
pub struct TypeEnv
{
	map: HashMap<SymbolId, Ty>,
}

impl TypeEnv
{
	pub fn insert(&mut self, id: SymbolId, ty: Ty)
	{
		self.map.insert(id, ty);
	}

	pub fn get(&self, id: SymbolId) -> Option<&Ty>
	{
		return self.map.get(&id);
	}

	#[allow(unused)]
	fn require(&self, id: SymbolId, span: Span) -> Result<Ty, TypeError>
	{
		return self
			.map
			.get(&id)
			.cloned()
			.ok_or_else(|| return TypeError::new(span, TypeErrorKind::UnknownSymbol { id }));
	}
}

#[derive(Debug, Clone, Default)]
pub struct ParamTypeCache
{
	map: HashMap<(SymbolId, usize), Ty>,
}

impl ParamTypeCache
{
	pub fn insert(&mut self, fn_sym: SymbolId, index: usize, ty: Ty)
	{
		self.map.insert((fn_sym, index), ty);
	}

	pub fn get(&self, fn_sym: SymbolId, index: usize) -> Option<&Ty>
	{
		return self.map.get(&(fn_sym, index));
	}

	pub fn find_fn_sym_by_name(global: &GlobalSymbolTable, fn_name: &str) -> Option<SymbolId>
	{
		return global.symbols.iter().enumerate().find_map(|(i, sym)| {
			if sym.name == fn_name && matches!(sym.kind, SymbolKind::Function { .. }) {
				return Some(SymbolId(i));
			}
			return None;
		});
	}
}

#[derive(Debug, Clone, Default)]
pub struct MethodTypeCache
{
	map: HashMap<(TyKey, String), Ty>,
}

impl MethodTypeCache
{
	pub fn insert(&mut self, key: TyKey, method_name: impl Into<String>, return_ty: Ty)
	{
		self.map.insert((key, method_name.into()), return_ty);
	}

	pub fn insert_sym(&mut self, type_sym: SymbolId, method_name: impl Into<String>, return_ty: Ty)
	{
		self.insert(TyKey::Symbol(type_sym), method_name, return_ty);
	}

	pub fn get(&self, key: &TyKey, method_name: &str) -> Option<&Ty>
	{
		return self.map.get(&(key.clone(), method_name.to_owned()));
	}

	pub fn get_sym(&self, type_sym: SymbolId, method_name: &str) -> Option<&Ty>
	{
		return self.get(&TyKey::Symbol(type_sym), method_name);
	}

	pub fn iter(&self) -> impl Iterator<Item = (&(TyKey, String), &Ty)>
	{
		return self.map.iter();
	}
}

#[derive(Debug, Clone, Default)]
pub struct MethodFnCache
{
	map: HashMap<(TyKey, String), SymbolId>,
}

impl MethodFnCache
{
	pub fn insert(&mut self, key: TyKey, method_name: impl Into<String>, fn_sym: SymbolId)
	{
		self.map.insert((key, method_name.into()), fn_sym);
	}

	pub fn insert_sym(&mut self, type_sym: SymbolId, method_name: impl Into<String>, fn_sym: SymbolId)
	{
		self.insert(TyKey::Symbol(type_sym), method_name, fn_sym);
	}

	pub fn get(&self, key: &TyKey, method_name: &str) -> Option<&SymbolId>
	{
		return self.map.get(&(key.clone(), method_name.to_owned()));
	}

	pub fn get_sym(&self, type_sym: SymbolId, method_name: &str) -> Option<&SymbolId>
	{
		return self.get(&TyKey::Symbol(type_sym), method_name);
	}

	pub fn iter(&self) -> impl Iterator<Item = (&(TyKey, String), &SymbolId)>
	{
		return self.map.iter();
	}
}

#[derive(Debug, Clone, Default)]
pub struct FieldTypeCache
{
	map: HashMap<(SymbolId, String), Ty>,
}

impl FieldTypeCache
{
	pub fn insert(&mut self, parent: SymbolId, field_name: impl Into<String>, ty: Ty)
	{
		self.map.insert((parent, field_name.into()), ty);
	}

	pub fn get(&self, parent: SymbolId, field_name: &str) -> Option<&Ty>
	{
		return self.map.get(&(parent, field_name.to_owned()));
	}

	#[allow(unused)]
	pub fn fields_of(&self, parent: SymbolId) -> Vec<(String, Ty)>
	{
		return self
			.map
			.iter()
			.filter_map(|((sid, name), ty)| {
				if *sid == parent {
					return Some((name.clone(), ty.clone()));
				}
				return None;
			})
			.collect();
	}
}

#[derive(Debug, Clone)]
pub struct TypedModule
{
	pub path: Vec<String>,
	pub ast: TypedAST,
	pub caches: TypeCaches,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedAST
{
	pub top_level_block: TypedTopLevelBlock,
	pub source_index: SourceIndex,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedTopLevelBlock
{
	pub items: Vec<TypedTopLevelDecl>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum TypedTopLevelDecl
{
	Function(TypedFunctionDecl),
	VariableDecl(TypedVariableDecl),
	Struct(TypedStructDecl),
	Union(TypedUnionDecl),
	Enum(TypedEnumDecl),
	Variant(TypedVariantDecl),
	TypeAlias(TypedTypeAliasDecl),
	Trait(TypedTraitDecl),
	Module(TypedModuleDecl),
	Impl(TypedImplDecl),
	Directive(TypedDirectiveNode),
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedFunctionDecl
{
	pub resolved_name: SymbolId,
	pub signature: TypedFunctionSignature,
	pub body: Option<TypedBlock>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedFunctionSignature
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<(parser::Ident, Span)>,
	pub heap_generics: Vec<ResolvedGenericHeapParam>,
	pub call_type: CallType,
	pub params: Vec<TypedParam>,
	pub return_type: Ty,
	pub where_clause: Vec<TypedWhereConstraint>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Spanned)]
pub struct TypedParam
{
	pub symbol: SymbolId,
	pub name: String,
	pub ty: Ty,
	pub mutable: bool,
	pub variadic: bool,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedBlock
{
	pub stmts: Vec<TypedStmt>,
	pub tail_expr: Option<Box<TypedExpr>>,
	pub ty: Ty,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum TypedStmt
{
	VariableDecl(TypedVariableDecl),
	Assignment
	{
		target: TypedExpr,
		op: AssignOp,
		value: TypedExpr,
		span: Span,
	},
	Return
	{
		value: Option<TypedExpr>,
		span: Span,
	},
	Expr(TypedExpr),
	Break
	{
		label: String,
		value: Option<TypedExpr>,
		span: Span,
	},
	Continue
	{
		label: String,
		span: Span,
	},
	If
	{
		cond: TypedExpr,
		then_block: TypedBlock,
		else_branch: Option<Box<TypedStmt>>,
		span: Span,
	},
	Loop
	{
		label: String,
		body: TypedBlock,
		span: Span,
	},
	Delete
	{
		expr: TypedExpr,
		span: Span,
	},
	Unsafe(TypedBlock),
	Block(TypedBlock),
	Directive(TypedDirectiveNode),
	Pending(Span),
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedExpr
{
	pub kind: TypedExprKind,
	pub ty: Ty,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExprKind
{
	Identifier
	{
		path: ResolvedPath,
	},
	Literal
	{
		value: Literal,
	},
	Default
	{
		heap_call: CallType,
	},
	Unary
	{
		op: UnaryOp,
		expr: Box<TypedExpr>,
	},
	Binary
	{
		op: BinaryOp,
		lhs: Box<TypedExpr>,
		rhs: Box<TypedExpr>,
	},
	Cast
	{
		ty: Ty,
		expr: Box<TypedExpr>,
	},
	Call
	{
		callee: Box<TypedExpr>,
		call_type: CallType,
		named_generics: Vec<(String, TypedExpr)>,
		args: Vec<TypedExpr>,
	},
	InternalCall
	{
		intrinsic: Intrinsic,
	},
	Field
	{
		base: Box<TypedExpr>,
		name: String,
	},
	Index
	{
		base: Box<TypedExpr>,
		index: Box<TypedExpr>,
	},
	Range(TypedRangeExpr),
	Tuple
	{
		elements: Vec<TypedExpr>,
	},
	Array(TypedArrayLiteral),
	StructInit
	{
		path: ResolvedPath,
		fields: Vec<(String, TypedExpr)>,
		base: Option<Box<TypedExpr>>,
		has_rest: bool,
	},
	Block(Box<TypedBlock>),
	UnsafeBlock(Box<TypedBlock>),
	Switch
	{
		expr: Box<TypedExpr>,
		arms: Vec<TypedSwitchArm>,
	},
	If
	{
		cond: Box<TypedExpr>,
		then_block: TypedBlock,
		else_branch: Option<Box<TypedExpr>>,
	},
	Loop
	{
		label: String,
		body: Box<TypedBlock>,
	},
}

macro_rules! binary_op_trait_method {
	($op:expr) => {
		match $op {
			BinaryOp::Add => Some("Add"),
			BinaryOp::Sub => Some("Sub"),
			BinaryOp::Mul => Some("Mul"),
			BinaryOp::Div => Some("Div"),
			BinaryOp::Mod => Some("Rem"),
			BinaryOp::BitAnd => Some("BitAnd"),
			BinaryOp::BitOr => Some("BitOr"),
			BinaryOp::BitXor => Some("BitXor"),
			BinaryOp::Shl => Some("Shl"),
			BinaryOp::Shr => Some("Shr"),
			BinaryOp::Eq | BinaryOp::Ne => Some("Eq"),
			BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => Some("Cmp"),
			BinaryOp::LogicalAnd | BinaryOp::LogicalOr => None,
		}
	};
}

macro_rules! assign_op_trait_method {
	($op:expr) => {
		match $op {
			AssignOp::AddAssign => Some("AssignAdd"),
			AssignOp::SubAssign => Some("AssignSub"),
			AssignOp::MulAssign => Some("AssignMul"),
			AssignOp::DivAssign => Some("AssignDiv"),
			AssignOp::ModAssign => Some("AssignRem"),
			AssignOp::AndAssign => Some("AssignBitAnd"),
			AssignOp::OrAssign => Some("AssignBitOr"),
			AssignOp::XorAssign => Some("AssignBitXor"),
			AssignOp::ShlAssign => Some("AssignShl"),
			AssignOp::ShrAssign => Some("AssignShr"),
			AssignOp::Assign => None,
		}
	};
}

macro_rules! unary_op_trait_method {
	($op:expr) => {
		match $op {
			UnaryOp::Neg => Some("Neg"),
			UnaryOp::Not => Some("Not"),
			UnaryOp::Deref => Some("Deref"),
			UnaryOp::Addr { .. } => None,
		}
	};
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedRangeExpr
{
	pub start: Option<Box<TypedExpr>>,
	pub end: Option<Box<TypedExpr>>,
	pub inclusive: bool,
	pub ty: Ty,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum TypedArrayLiteral
{
	List
	{
		elements: Vec<TypedExpr>, span: Span
	},
	Repeat
	{
		value: Box<TypedExpr>,
		count: Box<TypedExpr>,
		span: Span,
	},
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedSwitchArm
{
	pub pattern: TypedPattern,
	pub body: TypedSwitchBody,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum TypedSwitchBody
{
	Expr(TypedExpr),
	Block(TypedBlock),
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum TypedPattern
{
	Wildcard
	{
		ty: Ty,
		span: Span,
	},
	Literal
	{
		value: Literal,
		ty: Ty,
		span: Span,
	},
	TypedIdentifier
	{
		symbol: SymbolId,
		name: String,
		ty: Ty,
		mutable: bool,
		span: Span,
	},
	Variant
	{
		path: ResolvedPath,
		args: Vec<TypedPattern>,
		ty: Ty,
		span: Span,
	},
	Tuple
	{
		patterns: Vec<TypedPattern>,
		ty: Ty,
		span: Span,
	},
	Struct
	{
		path: ResolvedPath,
		fields: Vec<(String, TypedPattern)>,
		has_rest: bool,
		ty: Ty,
		span: Span,
	},
	Range(TypedRangeExpr),
	Or
	{
		patterns: Vec<TypedPattern>,
		ty: Ty,
		span: Span,
	},
}

impl TypedPattern
{
	pub const fn ty(&self) -> &Ty
	{
		match self {
			TypedPattern::Wildcard { ty, .. }
			| TypedPattern::Literal { ty, .. }
			| TypedPattern::TypedIdentifier { ty, .. }
			| TypedPattern::Variant { ty, .. }
			| TypedPattern::Tuple { ty, .. }
			| TypedPattern::Struct { ty, .. }
			| TypedPattern::Or { ty, .. } => return ty,
			TypedPattern::Range(re) => return &re.ty,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedVariableDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub ty: Ty,
	pub init: Option<TypedExpr>,
	pub comp_const: bool,
	pub mutable: bool,
	pub modifiers: Vec<parser::Modifier>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedStructField
{
	pub name: String,
	pub ty: Ty,
	pub default_value: Option<TypedExpr>,
	pub modifiers: Vec<parser::Modifier>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedStructDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub fields: Vec<TypedStructField>,
	pub where_clause: Vec<TypedWhereConstraint>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedUnionField
{
	pub name: String,
	pub ty: Ty,
	pub modifiers: Vec<parser::Modifier>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedUnionDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub fields: Vec<TypedUnionField>,
	pub where_clause: Vec<TypedWhereConstraint>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedEnumVariant
{
	pub name: String,
	pub value: Option<TypedExpr>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedEnumDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub variants: Vec<TypedEnumVariant>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedVariantMember
{
	pub name: String,
	pub ty: Option<Ty>,
	pub value: Option<TypedExpr>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedVariantDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub variants: Vec<TypedVariantMember>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedTypeAliasDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub ty: Ty,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedAssocTypeDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub ty: Option<Ty>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum TypedTraitItem
{
	Function(TypedFunctionDecl),
	TypeAlias(TypedTypeAliasDecl),
	AssocType(TypedAssocTypeDecl),
	Const(TypedVariableDecl),
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedTraitDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub super_traits: Vec<TyBound>,
	pub items: Vec<TypedTraitItem>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedModuleDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub resolved_body: Option<TypedTopLevelBlock>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum TypedImplItem
{
	Function(TypedFunctionDecl),
	TypeAlias(TypedTypeAliasDecl),
	AssocType(TypedAssocTypeDecl),
	Const(TypedVariableDecl),
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedImplDecl
{
	pub resolved_target: ResolvedPath,
	pub resolved_trait: Option<ResolvedPath>,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub where_clause: Vec<TypedWhereConstraint>,
	pub items: Vec<TypedImplItem>,
	pub docs: Option<parser::DocsComment>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Spanned)]
pub struct TypedWhereConstraint
{
	pub ty: String,
	pub bounds: Vec<TyBound>,
	pub type_args: Vec<Ty>,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct TypedDirectiveNode
{
	pub directive: TypedDirective,
	pub body: Option<TypedBlock>,
	pub span: Span,
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
pub enum TypedDirective
{
	Import
	{
		import: String,
		visibility: crate::symbol_collection::Visibility,
	},
	Use
	{
		use_path: parser::Path,
		visibility: crate::symbol_collection::Visibility,
	},
	Custom
	{
		name: String,
		params: Vec<parser::DirectiveParam>,
	},
	ValidateStructPattern
	{
		struct_path: ResolvedPath,
		pattern_fields: Vec<String>,
		has_rest: bool,
	},
	ValidateType
	{
		ty: Ty, expr: TypedExpr
	},
}

#[derive(Debug, Clone)]
pub enum TypeErrorKind
{
	CannotInferType,

	TypeMismatch
	{
		expected: String,
		found: String,
	},

	UnknownSymbol
	{
		id: SymbolId,
	},

	NotCallable
	{
		ty: String,
	},

	FieldAccessOnNonStruct
	{
		ty: String,
	},

	UnknownField
	{
		ty: String,
		field: String,
	},

	UnresolvedIdentifier
	{
		path: String,
	},

	UnresolvedAssocPath
	{
		path: String,
	},

	UnresolvedSelf
	{
		member: String,
	},

	IndexOnNonArray
	{
		ty: String,
	},

	ReturnTypeMismatch
	{
		expected: String,
		found: String,
	},

	#[allow(unused)]
	BreakTypeMismatch
	{
		expected: String,
		found: String,
	},

	ArgCountMismatch
	{
		expected: usize,
		found: usize,
	},

	InvalidUnaryOp
	{
		op: UnaryOp,
		ty: String,
	},

	InvalidBinaryOp
	{
		op: BinaryOp,
		lhs: String,
		rhs: String,
	},

	InvalidCast
	{
		from: String,
		to: String,
	},

	IfBranchTypeMismatch
	{
		then_ty: String,
		else_ty: String,
	},

	SwitchArmTypeMismatch
	{
		first: String,
		found: String,
	},

	StructUnknownField
	{
		struct_ty: String,
		field: String,
	},

	StructFieldTypeMismatch
	{
		field: String,
		expected: String,
		found: String,
	},

	UnknownType
	{
		name: String,
	},

	InvalidPrimitivePosition
	{
		ty: String,
		position: &'static str,
	},
}

#[derive(Debug, Clone, Spanned)]
pub struct TypeError
{
	pub span: Span,
	pub kind: TypeErrorKind,
	pub context: Vec<String>,
}

impl TypeError
{
	pub const fn new(span: Span, kind: TypeErrorKind) -> Self
	{
		return Self {
			span,
			kind,
			context: Vec::new(),
		};
	}

	#[allow(unused)]
	pub fn with_context(mut self, ctx: impl Into<String>) -> Self
	{
		self.context.push(ctx.into());
		return self;
	}
}

impl fmt::Display for TypeError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match &self.kind {
			TypeErrorKind::CannotInferType => write!(
				f,
				"cannot infer type: `_` is only allowed in local variable declarations"
			),

			TypeErrorKind::TypeMismatch { expected, found } => {
				write!(f, "type mismatch: expected `{expected}`, found `{found}`")
			}

			TypeErrorKind::UnknownSymbol { id } => write!(f, "internal: no type registered for symbol #{}", id.0),

			TypeErrorKind::NotCallable { ty } => write!(f, "type `{ty}` is not callable"),

			TypeErrorKind::FieldAccessOnNonStruct { ty } => {
				write!(f, "cannot access field on non-struct/union type `{ty}`")
			}

			TypeErrorKind::UnknownField { ty, field } => write!(f, "type `{ty}` has no field `{field}`"),

			TypeErrorKind::IndexOnNonArray { ty } => write!(f, "cannot index into type `{ty}`"),

			TypeErrorKind::ReturnTypeMismatch { expected, found } => {
				write!(f, "return type mismatch: expected `{expected}`, found `{found}`")
			}

			TypeErrorKind::BreakTypeMismatch { expected, found } => {
				write!(f, "break type mismatch: expected `{expected}`, found `{found}`")
			}

			TypeErrorKind::ArgCountMismatch { expected, found } => {
				write!(f, "wrong number of arguments: expected {expected}, found {found}")
			}

			TypeErrorKind::InvalidUnaryOp { op, ty } => write!(f, "operator `{op:?}` cannot be applied to type `{ty}`"),

			TypeErrorKind::InvalidBinaryOp { op, lhs, rhs } => {
				write!(f, "operator `{op:?}` cannot be applied to `{lhs}` and `{rhs}`")
			}

			TypeErrorKind::InvalidCast { from, to } => write!(f, "cannot cast `{from}` to `{to}`"),

			TypeErrorKind::IfBranchTypeMismatch { then_ty, else_ty } => {
				write!(f, "if-else branches have different types: `{then_ty}` vs `{else_ty}`")
			}

			TypeErrorKind::SwitchArmTypeMismatch { first, found } => {
				write!(f, "switch arms have different types: `{first}` vs `{found}`")
			}

			TypeErrorKind::StructUnknownField { struct_ty, field } => {
				write!(f, "struct `{struct_ty}` has no field `{field}`")
			}

			TypeErrorKind::StructFieldTypeMismatch { field, expected, found } => {
				write!(f, "field `{field}`: expected `{expected}`, found `{found}`")
			}

			TypeErrorKind::UnknownType { name } => write!(f, "unknown type `{name}`"),

			TypeErrorKind::UnresolvedIdentifier { path } => write!(f, "unresolved identifier `{path}`"),

			TypeErrorKind::UnresolvedAssocPath { path } => write!(f, "unresolved associated path `{path}`"),

			TypeErrorKind::UnresolvedSelf { member } => write!(f, "unresolved `Self::{member}`"),

			TypeErrorKind::InvalidPrimitivePosition { ty, position } => {
				write!(f, "primitive type `{ty}` cannot be used as {position}")
			}
		};
	}
}

impl std::error::Error for TypeError {}

impl CompileDiagnostic for TypeError
{
	fn build(&self) -> DiagnosticBuilder
	{
		let mut diag = match &self.kind {
			TypeErrorKind::CannotInferType => {
				DiagnosticBuilder::error("cannot infer type: `_` is only allowed in local variable declarations")
					.code(ErrorCode::TypeCannotInfer)
					.primary(self.span, None)
			}

			TypeErrorKind::TypeMismatch { expected, found } => {
				DiagnosticBuilder::error(format!("type mismatch: expected `{expected}`, found `{found}`"))
					.code(ErrorCode::TypeMismatch)
					.primary(self.span, None)
			}

			TypeErrorKind::UnknownSymbol { id } => {
				DiagnosticBuilder::error(format!("internal: no type registered for symbol #{}", id.0))
					.code(ErrorCode::TypeUnknownSymbol)
					.primary(self.span, None)
			}

			TypeErrorKind::NotCallable { ty } => DiagnosticBuilder::error(format!("type `{ty}` is not callable"))
				.code(ErrorCode::TypeNotCallable)
				.primary(self.span, None),

			TypeErrorKind::FieldAccessOnNonStruct { ty } => {
				DiagnosticBuilder::error(format!("cannot access field on non-struct/union type `{ty}`"))
					.code(ErrorCode::TypeFieldAccessOnNonStruct)
					.primary(self.span, None)
			}

			TypeErrorKind::UnknownField { ty, field } => {
				DiagnosticBuilder::error(format!("type `{ty}` has no field `{field}`"))
					.code(ErrorCode::TypeUnknownField)
					.primary(self.span, None)
			}

			TypeErrorKind::IndexOnNonArray { ty } => DiagnosticBuilder::error(format!("cannot index into type `{ty}`"))
				.code(ErrorCode::TypeIndexOnNonArray)
				.primary(self.span, None),

			TypeErrorKind::ReturnTypeMismatch { expected, found } => {
				DiagnosticBuilder::error(format!("return type mismatch: expected `{expected}`, found `{found}`"))
					.code(ErrorCode::TypeReturnMismatch)
					.primary(self.span, None)
			}

			TypeErrorKind::BreakTypeMismatch { expected, found } => {
				DiagnosticBuilder::error(format!("break type mismatch: expected `{expected}`, found `{found}`"))
					.code(ErrorCode::TypeBreakMismatch)
					.primary(self.span, None)
			}

			TypeErrorKind::ArgCountMismatch { expected, found } => {
				DiagnosticBuilder::error(format!("wrong number of arguments: expected {expected}, found {found}"))
					.code(ErrorCode::TypeArgCountMismatch)
					.primary(self.span, None)
			}

			TypeErrorKind::InvalidUnaryOp { op, ty } => {
				DiagnosticBuilder::error(format!("operator `{op:?}` cannot be applied to type `{ty}`"))
					.code(ErrorCode::TypeInvalidUnaryOp)
					.primary(self.span, None)
			}

			TypeErrorKind::InvalidBinaryOp { op, lhs, rhs } => {
				DiagnosticBuilder::error(format!("operator `{op:?}` cannot be applied to `{lhs}` and `{rhs}`"))
					.code(ErrorCode::TypeInvalidBinaryOp)
					.primary(self.span, None)
			}

			TypeErrorKind::InvalidCast { from, to } => {
				DiagnosticBuilder::error(format!("cannot cast `{from}` to `{to}`"))
					.code(ErrorCode::TypeInvalidCast)
					.primary(self.span, None)
			}

			TypeErrorKind::IfBranchTypeMismatch { then_ty, else_ty } => DiagnosticBuilder::error(format!(
				"if-else branches have different types: `{then_ty}` vs `{else_ty}`"
			))
			.code(ErrorCode::TypeIfBranchMismatch)
			.primary(self.span, None),

			TypeErrorKind::SwitchArmTypeMismatch { first, found } => {
				DiagnosticBuilder::error(format!("switch arms have different types: `{first}` vs `{found}`"))
					.code(ErrorCode::TypeSwitchArmMismatch)
					.primary(self.span, None)
			}

			TypeErrorKind::StructUnknownField { struct_ty, field } => {
				DiagnosticBuilder::error(format!("struct `{struct_ty}` has no field `{field}`"))
					.code(ErrorCode::TypeStructUnknownField)
					.primary(self.span, None)
			}

			TypeErrorKind::StructFieldTypeMismatch { field, expected, found } => {
				DiagnosticBuilder::error(format!("field `{field}`: expected `{expected}`, found `{found}`"))
					.code(ErrorCode::TypeStructFieldMismatch)
					.primary(self.span, None)
			}

			TypeErrorKind::UnknownType { name } => DiagnosticBuilder::error(format!("unknown type `{name}`"))
				.code(ErrorCode::TypeUnknownType)
				.primary(self.span, None),

			TypeErrorKind::UnresolvedIdentifier { path } => {
				DiagnosticBuilder::error(format!("unresolved identifier `{path}`"))
					.code(ErrorCode::TypeUnresolvedIdentifier)
					.primary(self.span, None)
			}

			TypeErrorKind::UnresolvedAssocPath { path } => {
				DiagnosticBuilder::error(format!("unresolved associated path `{path}`"))
					.code(ErrorCode::TypeUnresolvedAssocPath)
					.primary(self.span, None)
			}

			TypeErrorKind::UnresolvedSelf { member } => {
				DiagnosticBuilder::error(format!("unresolved `Self::{member}`"))
					.code(ErrorCode::TypeUnresolvedSelf)
					.primary(self.span, None)
			}

			TypeErrorKind::InvalidPrimitivePosition { ty, position } => {
				DiagnosticBuilder::error(format!("primitive type `{ty}` cannot be used as {position}"))
					.code(ErrorCode::TypeInvalidPrimitivePosition)
					.primary(self.span, None)
			}
		};

		for ctx in &self.context {
			diag = diag.note(format!("while checking types: {ctx}"));
		}

		return diag;
	}
}

impl From<TypeError> for CompileError
{
	fn from(e: TypeError) -> Self
	{
		return CompileError::Type(e);
	}
}

pub struct TyDisplay<'a>
{
	ty: &'a Ty,
	global: &'a GlobalSymbolTable,
}

impl TyDisplay<'_>
{
	fn fmt_ty(&self, f: &mut fmt::Formatter<'_>, ty: &Ty) -> fmt::Result
	{
		return match ty {
			Ty::Named { symbol, generics } => {
				write!(f, "{}", self.global.symbol(*symbol).name)?;
				if !generics.is_empty() {
					write!(f, "<")?;
					for (i, g) in generics.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						self.fmt_ty(f, g)?;
					}
					write!(f, ">")?;
				}
				Ok(())
			}
			Ty::Reference { mutable, inner } => {
				write!(f, "&")?;
				if *mutable {
					write!(f, "mut ")?;
				}
				self.fmt_ty(f, inner)
			}
			Ty::Mutable { inner } => {
				write!(f, "mut ")?;
				self.fmt_ty(f, inner)
			}
			Ty::Pointer { mutable, inner } => {
				write!(f, "*")?;
				if *mutable {
					write!(f, "mut ")?;
				}
				self.fmt_ty(f, inner)
			}
			Ty::Array { inner, size } => {
				write!(f, "[")?;
				self.fmt_ty(f, inner)?;
				if let Some(n) = size {
					write!(f, "; {n}")?;
				}
				write!(f, "]")
			}
			Ty::Tuple(ts) => {
				write!(f, "(")?;
				for (i, t) in ts.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					self.fmt_ty(f, t)?;
				}
				write!(f, ")")
			}
			Ty::Generic { name, bounds } => {
				write!(f, "{name}")?;
				if !bounds.is_empty() {
					write!(f, ": ")?;
					for (i, b) in bounds.iter().enumerate() {
						if i > 0 {
							write!(f, " + ")?;
						}
						self.fmt_bound(f, b)?;
					}
				}
				Ok(())
			}
			Ty::ImplTrait { bounds, .. } => {
				write!(f, "impl ")?;
				for (i, b) in bounds.iter().enumerate() {
					if i > 0 {
						write!(f, " + ")?;
					}
					self.fmt_bound(f, b)?;
				}
				Ok(())
			}
			other => write!(f, "{other}"),
		};
	}

	fn fmt_bound(&self, f: &mut fmt::Formatter<'_>, bound: &TyBound) -> fmt::Result
	{
		return match bound {
			TyBound::Trait { symbol, args } => {
				write!(f, "{}", self.global.symbol(*symbol).name)?;
				if !args.is_empty() {
					write!(f, "<")?;
					for (i, a) in args.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						match a {
							TyGenericArg::Type(ty) => self.fmt_ty(f, ty)?,
							TyGenericArg::Binding { name, ty } => {
								write!(f, "{name} = ")?;
								self.fmt_ty(f, ty)?;
							}
						}
					}
					write!(f, ">")?;
				}
				Ok(())
			}
			TyBound::Fn { args, ret } => {
				write!(f, "Fn(")?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					self.fmt_ty(f, a)?;
				}
				write!(f, ") -> ")?;
				self.fmt_ty(f, ret)
			}
		};
	}
}

#[derive(Debug, Default)]
struct GenericFrame(HashMap<String, Ty>);

#[derive(Debug, Default)]
struct GenericScope(Vec<GenericFrame>);

impl GenericScope
{
	fn push(&mut self, frame: GenericFrame)
	{
		self.0.push(frame);
	}

	fn pop(&mut self)
	{
		self.0.pop();
	}

	fn lookup(&self, name: &str) -> Option<&Ty>
	{
		for frame in self.0.iter().rev() {
			if let Some(ty) = frame.0.get(name) {
				return Some(ty);
			}
		}
		return None;
	}

	fn push_params(&mut self, params: &[(parser::Ident, Span)], where_clause: &[TypedWhereConstraint])
	{
		let mut frame: GenericFrame = GenericFrame::default();
		for (name, _) in params {
			let bounds: Vec<TyBound> = where_clause
				.iter()
				.filter(|c| return &c.ty == name)
				.flat_map(|c| return c.bounds.iter().cloned())
				.collect();
			frame.0.insert(
				name.clone(),
				Ty::Generic {
					name: name.clone(),
					bounds,
				},
			);
		}
		self.push(frame);
	}

	fn push_generic_params(&mut self, params: &[parser::GenericParam], where_clause: &[TypedWhereConstraint])
	{
		let pairs: Vec<(parser::Ident, Span)> = params.iter().map(|g| return (g.name.clone(), g.span)).collect();
		self.push_params(&pairs, where_clause);
	}
}

#[derive(Debug)]
struct PendingDecl<'src>
{
	resolved: &'src ResolvedVariableDecl,
	stmt_index: usize,
}

enum InferVarResult
{
	Unresolvable(Vec<SymbolId>),
	HardError(TypeError),
}

#[derive(Debug, Default)]
struct BackpatchState<'src>
{
	pending: HashMap<SymbolId, PendingDecl<'src>>,
	dependents: HashMap<SymbolId, Vec<SymbolId>>,
	worklist: std::collections::VecDeque<SymbolId>,
}

impl BackpatchState<'_>
{
	fn add_dependency(&mut self, dependency: SymbolId, dependent: SymbolId)
	{
		self.dependents.entry(dependency).or_default().push(dependent);
	}

	fn mark_resolved(&mut self, sym: SymbolId)
	{
		self.worklist.push_back(sym);
	}
}

#[derive(Debug, Clone)]
struct BlanketMethod
{
	name: String,
	param_tys: Vec<Ty>,
	return_ty: Ty,
	fn_sym: SymbolId,
	has_self_param: bool,
}

#[derive(Debug, Clone)]
struct BlanketImpl
{
	required_builtin: SymbolId,

	granted_trait: SymbolId,
	methods: Vec<BlanketMethod>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InferExprId(u32);

#[derive(Debug, Clone)]
enum PatchTarget
{
	StmtExpr
	{
		stmt_index: usize
	},

	VarDeclInit
	{
		stmt_index: usize, sym: SymbolId
	},

	#[allow(unused)]
	BlockTail,
}

struct PendingExpr<'src>
{
	expr: &'src ResolvedExpr,

	hint: Option<Ty>,

	target: PatchTarget,

	sym_deps: Vec<SymbolId>,
}

#[derive(Default)]
struct BlockInferState<'src>
{
	pending: HashMap<InferExprId, PendingExpr<'src>>,

	sym_to_exprs: HashMap<SymbolId, Vec<InferExprId>>,

	worklist: std::collections::VecDeque<InferExprId>,

	next_id: u32,
}

impl<'src> BlockInferState<'src>
{
	const fn fresh_id(&mut self) -> InferExprId
	{
		let id = InferExprId(self.next_id);
		self.next_id += 1;
		return id;
	}

	fn register(
		&mut self,
		expr: &'src ResolvedExpr,
		hint: Option<Ty>,
		target: PatchTarget,
		sym_deps: Vec<SymbolId>,
	) -> InferExprId
	{
		let id = self.fresh_id();
		for &sym in &sym_deps {
			self.sym_to_exprs.entry(sym).or_default().push(id);
		}
		self.pending.insert(
			id,
			PendingExpr {
				expr,
				hint,
				target,
				sym_deps,
			},
		);
		return id;
	}

	fn notify_sym(&mut self, sym: SymbolId)
	{
		if let Some(deps) = self.sym_to_exprs.remove(&sym) {
			for id in deps {
				if self.pending.contains_key(&id) {
					self.worklist.push_back(id);
				}
			}
		}
	}

	fn refine_hint(&mut self, id: InferExprId, new_hint: Ty)
	{
		if let Some(p) = self.pending.get_mut(&id) {
			match &p.hint {
				None | Some(Ty::Infer) => p.hint = Some(new_hint),
				_ => {}
			}
		}
	}

	fn is_empty(&self) -> bool
	{
		return self.pending.is_empty();
	}

	fn first_span(&self) -> Option<Span>
	{
		return self.pending.values().next().map(|p| return p.expr.span());
	}
}

const ALLOC_TRAIT_PATH: &[&str] = &["std", "Alloc"];
const IO_TRAIT_PATH: &[&str] = &["std", "IO"];
const COPY_TRAIT_PATH: &[&str] = &["std", "Copy"];

#[derive(Debug, Clone, Default)]
struct HeapTraitSyms
{
	alloc: Option<SymbolId>,
	io: Option<SymbolId>,
}

#[derive(Debug, Clone)]
pub struct TypeCaches
{
	pub env: TypeEnv,
	pub field: FieldTypeCache,
	pub field_default: HashMap<(SymbolId, String), TypedExpr>,
	pub method: MethodTypeCache,
	pub method_fn: MethodFnCache,
	pub param: ParamTypeCache,
	pub impl_assoc: HashMap<(TyKey, SymbolId, String), Ty>,
	pub impl_assoc_generic_params: HashMap<(TyKey, SymbolId, String), Vec<String>>,
	pub trait_impls: HashMap<TyKey, HashSet<SymbolId>>,
	pub variant_generics: HashMap<SymbolId, Vec<String>>,
	pub copy_sym: SymbolId,
}

impl Default for TypeCaches
{
	fn default() -> Self
	{
		return Self {
			env: TypeEnv::default(),
			field: FieldTypeCache::default(),
			field_default: HashMap::default(),
			method: MethodTypeCache::default(),
			method_fn: MethodFnCache::default(),
			param: ParamTypeCache::default(),
			impl_assoc: HashMap::default(),
			impl_assoc_generic_params: HashMap::default(),
			trait_impls: HashMap::default(),
			variant_generics: HashMap::default(),
			copy_sym: SymbolId(usize::MAX), // This one is always overwritten, and so can be safely be written to a nonsence value
		};
	}
}

#[derive(Debug, Default)]
struct TraitData<'a>
{
	decls: HashMap<SymbolId, &'a ResolvedTraitDecl>,
	impls: HashMap<TyKey, HashSet<SymbolId>>,
	builtins: HashSet<SymbolId>,
	blanket_impls: Vec<BlanketImpl>,
	op_symbols: HashMap<String, SymbolId>,
	heap_syms: HeapTraitSyms,
}

#[derive(Debug, Default)]
struct ConstraintData
{
	struct_where_bounds: HashMap<SymbolId, Vec<(usize, TyBound)>>,
	fn_where_bounds: HashMap<SymbolId, Vec<(String, TyBound)>>,
	variadic_fns: HashSet<SymbolId>,
	fn_call_type_cache: HashMap<SymbolId, CallType>,
	blanket_fn_has_self: HashMap<SymbolId, bool>,
}

#[derive(Debug, Clone)]
struct FnCtx
{
	return_ty: Option<Ty>,
	self_ty: Option<Ty>,
	call_type: CallType,
	heap_params: HashMap<String, Ty>,
	assoc_types: HashMap<String, Ty>,
}

impl Default for FnCtx
{
	fn default() -> Self
	{
		return Self {
			return_ty: None,
			self_ty: None,
			call_type: CallType::Regular,
			heap_params: HashMap::new(),
			assoc_types: HashMap::new(),
		};
	}
}

struct Checker<'a>
{
	global: &'a GlobalSymbolTable,
	caches: TypeCaches,
	traits: TraitData<'a>,
	constraints: ConstraintData,
	fn_ctx: FnCtx,
	generic_scope: GenericScope,
	source_index: SourceIndex,
	infer_syms: HashSet<SymbolId>,
	newly_pinned: Vec<SymbolId>,
}

impl<'a> Checker<'a>
{
	fn fmt_ty(&self, ty: &Ty) -> String
	{
		return TyDisplay {
			ty,
			global: self.global,
		}
		.to_string();
	}

	fn new(global: &'a GlobalSymbolTable, source_index: SourceIndex) -> Self
	{
		return Self {
			global,
			caches: TypeCaches::default(),
			traits: TraitData::default(),
			constraints: ConstraintData::default(),
			fn_ctx: FnCtx::default(),
			generic_scope: GenericScope::default(),
			source_index,
			infer_syms: HashSet::new(),
			newly_pinned: Vec::new(),
		};
	}

	const fn err(span: Span, kind: TypeErrorKind) -> TypeError
	{
		return TypeError::new(span, kind);
	}

	fn mismatch(&self, span: Span, expected: &Ty, found: &Ty) -> TypeError
	{
		return Self::err(
			span,
			TypeErrorKind::TypeMismatch {
				expected: self.fmt_ty(expected),
				found: self.fmt_ty(found),
			},
		);
	}

	fn expect_ty(&mut self, found: &Ty, expected: &Ty, span: Span) -> Result<(), TypeError>
	{
		if let Ty::ImplTrait { bounds, .. } = expected {
			self.apply_blanket_impls_for_ty(found);
			for bound in bounds {
				if !self.ty_satisfies_bound(found, bound) {
					return Err(self.mismatch(span, expected, found));
				}
			}
			return Ok(());
		}
		if found.is_assignable_to(expected) {
			return Ok(());
		}
		return Err(self.mismatch(span, expected, found));
	}

	fn heap_param_symbol(&self, name: &str) -> Option<SymbolId>
	{
		return self.global.symbols.iter().enumerate().find_map(|(i, s)| {
			return if s.name == name && matches!(s.kind, SymbolKind::GenericParam) {
				Some(SymbolId(i))
			} else {
				None
			};
		});
	}

	fn ty_of_symbol(&self, id: SymbolId, span: Span) -> Result<Ty, TypeError>
	{
		if let Some(ty) = self.caches.env.get(id) {
			return Ok(ty.clone());
		}

		if id.0 < self.global.symbols.len() {
			let name = &self.global.symbol(id).name;
			if let Some(ty) = self.fn_ctx.heap_params.get(name.as_str()) {
				return Ok(ty.clone());
			}
		}

		if id.0 < self.global.symbols.len() {
			let sym = self.global.symbol(id);
			return Ok(match &sym.kind {
				SymbolKind::Struct
				| SymbolKind::Union
				| SymbolKind::Enum
				| SymbolKind::Variant
				| SymbolKind::Trait
				| SymbolKind::Module => Ty::named(id),
				SymbolKind::Function { .. } => Ty::Unit,
				SymbolKind::EnumVariant => {
					let parent = self.global.symbols.iter().enumerate().find_map(|(i, s)| {
						if !matches!(s.kind, SymbolKind::Enum) {
							return None;
						}
						let scope_id = s.introduced_scope?;
						if self.global.scope(scope_id).symbols.contains(&id) {
							return Some(SymbolId(i));
						}
						return None;
					});
					match parent {
						Some(parent_id) => Ty::named(parent_id),
						None => return Err(Self::err(span, TypeErrorKind::UnknownSymbol { id })),
					}
				}
				SymbolKind::VariantMember => {
					let parent = self.global.symbols.iter().enumerate().find_map(|(i, s)| {
						if !matches!(s.kind, SymbolKind::Variant) {
							return None;
						}
						let scope_id = s.introduced_scope?;
						let scope = self.global.scope(scope_id);
						if scope.symbols.contains(&id) {
							return Some(SymbolId(i));
						}
						return None;
					});
					match parent {
						Some(parent_id) => Ty::named(parent_id),
						None => return Err(Self::err(span, TypeErrorKind::UnknownSymbol { id })),
					}
				}
				_ => return Err(Self::err(span, TypeErrorKind::UnknownSymbol { id })),
			});
		}

		return Err(Self::err(span, TypeErrorKind::UnknownSymbol { id }));
	}

	fn resolve_to_struct_sym(&self, sym: SymbolId) -> SymbolId
	{
		if matches!(self.global.symbol(sym).kind, SymbolKind::TypeAlias)
			&& let Some(Ty::Named { symbol, .. }) = self.caches.env.get(sym)
		{
			return self.resolve_to_struct_sym(*symbol);
		}
		return sym;
	}

	fn resolve_assoc_types_in_ty(&self, ty: &Ty) -> Ty
	{
		return match ty {
			Ty::Named { symbol, generics } => {
				let sym_name = &self.global.symbol(*symbol).name;
				if let Some(concrete) = self.fn_ctx.assoc_types.get(sym_name) {
					return concrete.clone();
				}
				Ty::Named {
					symbol: *symbol,
					generics: generics
						.iter()
						.map(|g| return self.resolve_assoc_types_in_ty(g))
						.collect(),
				}
			}
			Ty::Reference { mutable, inner } => Ty::Reference {
				mutable: *mutable,
				inner: Box::new(self.resolve_assoc_types_in_ty(inner)),
			},
			Ty::Mutable { inner } => Ty::Mutable {
				inner: Box::new(self.resolve_assoc_types_in_ty(inner)),
			},
			Ty::Pointer { mutable, inner } => Ty::Pointer {
				mutable: *mutable,
				inner: Box::new(self.resolve_assoc_types_in_ty(inner)),
			},
			Ty::Array { inner, size } => Ty::Array {
				inner: Box::new(self.resolve_assoc_types_in_ty(inner)),
				size: *size,
			},
			Ty::Tuple(ts) => Ty::Tuple(ts.iter().map(|t| return self.resolve_assoc_types_in_ty(t)).collect()),
			other => other.clone(),
		};
	}

	fn lower_ty(&mut self, rt: &ResolvedType) -> Result<Ty, TypeError>
	{
		return self.lower_ty_core(&rt.core, rt.span);
	}

	fn lower_ty_core(&mut self, core: &ResolvedTypeCore, span: Span) -> Result<Ty, TypeError>
	{
		return Ok(match core {
			ResolvedTypeCore::Primitive { name, .. } => {
				if name == "Self" {
					return Ok(self.fn_ctx.self_ty.clone().unwrap_or(Ty::SelfTy));
				}
				if let Some(ty) = self.generic_scope.lookup(name) {
					return Ok(ty.clone());
				}
				if let Some(ty) = self.fn_ctx.assoc_types.get(name.as_str()) {
					return Ok(ty.clone());
				}
				if let Some(ty) = lower_int_ty(name, &self.generic_scope) {
					return Ok(ty);
				}
				Ty::from_primitive_name(name)
					.ok_or_else(|| return Self::err(span, TypeErrorKind::UnknownType { name: name.clone() }))?
			}

			ResolvedTypeCore::Base { path, generics } => {
				let sym_id: SymbolId = match &path.kind {
					ResolvedPathKind::Resolved(id) => *id,
					ResolvedPathKind::AssocItem { base, member, .. } => {
						if let Some(ty) = self.fn_ctx.assoc_types.get(member) {
							return Ok(ty.clone());
						}
						if let Some(scope_id) = self.global.symbol(*base).introduced_scope {
							let scope = self.global.scope(scope_id);
							for &child_id in &scope.symbols {
								let child = self.global.symbol(child_id);
								if child.name == *member {
									if let Some(ty) = self.caches.env.get(child_id) {
										return Ok(ty.clone());
									}
									return Ok(Ty::named(child_id));
								}
							}
						}
						return Err(Self::err(
							path.original.span(),
							TypeErrorKind::UnresolvedAssocPath {
								path: format!("{}::{}", self.global.symbol(*base).name, member),
							},
						));
					}
					ResolvedPathKind::Primitive(ty) => return Ok(ty.clone()),
				};

				let sym_name: &String = &self.global.symbol(sym_id).name;
				if let Some(ty) = self.fn_ctx.assoc_types.get(sym_name.as_str()) {
					return Ok(ty.clone());
				}
				if let Some(ty) = self.generic_scope.lookup(sym_name) {
					return Ok(ty.clone());
				}
				let gs: Vec<Ty> = generics
					.iter()
					.map(|g| return self.lower_ty(g))
					.collect::<Result<_, _>>()?;
				let named = Ty::Named {
					symbol: sym_id,
					generics: gs,
				};

				if let Ty::Named { ref generics, .. } = named
					&& let Some(bounds) = self.constraints.struct_where_bounds.get(&sym_id).cloned()
				{
					for (param_idx, required_bound) in &bounds {
						let Some(arg_ty) = generics.get(*param_idx) else {
							continue;
						};

						self.apply_blanket_impls_for_ty(arg_ty);
						if !self.ty_satisfies_bound(arg_ty, required_bound) {
							let bound_str = match required_bound {
								TyBound::Trait { symbol, .. } => self.global.symbol(*symbol).name.clone(),
								TyBound::Fn { .. } => "Fn".to_string(),
							};
							return Err(Self::err(
								span,
								TypeErrorKind::TypeMismatch {
									expected: format!("`{}` to implement `{}`", self.fmt_ty(arg_ty), bound_str),
									found: format!("`{}` does not implement `{}`", self.fmt_ty(arg_ty), bound_str),
								},
							));
						}
					}
				}
				named
			}

			ResolvedTypeCore::Reference { mutable, inner } => Ty::Reference {
				mutable: *mutable,
				inner: Box::new(self.lower_ty_core(inner, span)?),
			},

			ResolvedTypeCore::Mutable { inner } => Ty::Mutable {
				inner: Box::new(self.lower_ty_core(inner, span)?),
			},

			ResolvedTypeCore::Pointer { mutable, inner } => Ty::Pointer {
				mutable: *mutable,
				inner: Box::new(self.lower_ty_core(inner, span)?),
			},

			ResolvedTypeCore::Array { inner, size } => {
				let lowered_size: Option<u64> = size.as_ref().and_then(|s| {
					// TODO: This is temporary, so, array's can be used in language for now, but should be good enough for now
					return if let ResolvedExpr::Literal {
						value: lit @ Literal::Int { .. },
						..
					} = s.as_ref()
					{
						#[allow(clippy::cast_possible_truncation)]
						#[allow(clippy::cast_sign_loss)]
						let Ok(r) = read_radix_number(lit)
							.map(|val| return val as u64)
							.inspect_err(|e| eprintln!("{:#?}", e))
						else {
							todo!("make a good error for when parsing the number does not work");
						};
						Some(r)
					} else {
						None
					};
				});

				Ty::Array {
					inner: Box::new(self.lower_ty_core(inner, span)?),
					size: lowered_size,
				}
			}

			ResolvedTypeCore::Tuple(types) => {
				if types.is_empty() {
					Ty::Unit
				} else {
					Ty::Tuple(
						types
							.iter()
							.map(|t| return self.lower_ty(t))
							.collect::<Result<_, _>>()?,
					)
				}
			}

			ResolvedTypeCore::ImplTrait { bounds } => Ty::ImplTrait {
				bounds: bounds
					.iter()
					.map(|b| return self.lower_where_bound(b))
					.collect::<Result<_, _>>()?,
				concrete: None,
			},
		});
	}

	fn ty_satisfies_bound(&self, ty: &Ty, bound: &TyBound) -> bool
	{
		if matches!(ty, Ty::Generic { .. } | Ty::Infer | Ty::SelfTy) {
			return true;
		}

		let TyBound::Trait {
			symbol: trait_sym,
			args: required_args,
		} = bound
		else {
			return true;
		};

		if self.traits.builtins.contains(trait_sym) {
			return matches!(ty, Ty::Primitive(Primitive::Int(_)));
		}

		let Some(key) = TyKey::of(ty) else { return false };

		if self.key_implements_trait(&key, *trait_sym) {
			let binding_args: Vec<(&String, &Ty)> = required_args
				.iter()
				.filter_map(|a| {
					if let TyGenericArg::Binding { name, ty } = a {
						return Some((name, ty));
					}
					return None;
				})
				.collect();

			for (name, required_ty) in &binding_args {
				let Some(actual) = self
					.caches
					.impl_assoc
					.get(&(key.clone(), *trait_sym, (*name).clone()))
					.cloned()
				else {
					continue;
				};

				let substituted = if let Ty::Named { symbol: _, generics } = ty
					&& !generics.is_empty()
					&& let Some(param_names) = self
						.caches
						.impl_assoc_generic_params
						.get(&(key.clone(), *trait_sym, (*name).clone()))
						.cloned()
				{
					let mut subs: HashMap<String, Ty> = HashMap::new();
					for (pname, concrete) in param_names.iter().zip(generics.iter()) {
						subs.insert(pname.clone(), concrete.clone());
					}
					substitute_generics(&actual, &subs)
				} else {
					actual.clone()
				};

				if matches!(substituted, Ty::Generic { .. }) {
					if !matches!(required_ty, Ty::Generic { .. } | Ty::Infer) {
						return false;
					}
					continue;
				}

				if !substituted.is_assignable_to(required_ty) && !required_ty.is_assignable_to(&substituted) {
					return false;
				}
			}
			return true;
		}

		return self.traits.blanket_impls.iter().any(|b| {
			return b.granted_trait == *trait_sym
				&& self.ty_satisfies_bound(
					ty,
					&TyBound::Trait {
						symbol: b.required_builtin,
						args: Vec::new(),
					},
				);
		});
	}

	fn apply_blanket_impls_for_ty(&mut self, concrete_ty: &Ty)
	{
		let Some(key) = TyKey::of(concrete_ty) else { return };

		let applicable: Vec<BlanketImpl> = self
			.traits
			.blanket_impls
			.iter()
			.filter(|b| {
				return self.ty_satisfies_bound(
					concrete_ty,
					&TyBound::Trait {
						symbol: b.required_builtin,
						args: Vec::new(),
					},
				);
			})
			.cloned()
			.collect();

		for blanket in applicable {
			self.traits
				.impls
				.entry(key.clone())
				.or_default()
				.insert(blanket.granted_trait);

			for method in &blanket.methods {
				if self.caches.method.get(&key, &method.name).is_none() {
					let ret = substitute_self(&method.return_ty, concrete_ty);
					self.caches.method.insert(key.clone(), method.name.clone(), ret);
				}
				self.caches
					.method_fn
					.map
					.entry((key.clone(), method.name.clone()))
					.or_insert(method.fn_sym);

				self.constraints
					.blanket_fn_has_self
					.entry(method.fn_sym)
					.or_insert(method.has_self_param);

				for (i, pty) in method.param_tys.iter().enumerate() {
					let concrete_pty: Ty = substitute_self(pty, concrete_ty);
					if self.caches.param.get(method.fn_sym, i).is_none() {
						self.caches.param.insert(method.fn_sym, i, concrete_pty);
					}
				}
			}
		}
	}

	fn lower_where_bound(&mut self, bound: &ResolvedWhereBound) -> Result<TyBound, TypeError>
	{
		return Ok(match bound {
			ResolvedWhereBound::Path { path, args } => {
				let sym_id: SymbolId = match &path.kind {
					ResolvedPathKind::Resolved(id) => *id,
					ResolvedPathKind::AssocItem { base, .. } => *base,
					ResolvedPathKind::Primitive(ty) => {
						return Err(Self::err(
							path.original.span(),
							TypeErrorKind::InvalidPrimitivePosition {
								ty: self.fmt_ty(ty),
								position: "a trait bound",
							},
						));
					}
				};
				TyBound::Trait {
					symbol: sym_id,
					args: args
						.iter()
						.map(|a| return self.lower_generic_arg(a))
						.collect::<Result<_, _>>()?,
				}
			}
			ResolvedWhereBound::Func(ResolvedFuncBound::Fn { args, ret }) => TyBound::Fn {
				args: args.iter().map(|t| return self.lower_ty(t)).collect::<Result<_, _>>()?,
				ret: Box::new(
					ret.as_ref()
						.map(|t| return self.lower_ty(t))
						.transpose()?
						.unwrap_or(Ty::Unit),
				),
			},
		});
	}

	fn lower_generic_arg(&mut self, arg: &ResolvedGenericArg) -> Result<TyGenericArg, TypeError>
	{
		return Ok(match arg {
			ResolvedGenericArg::Type(ty) => TyGenericArg::Type(self.lower_ty(ty)?),
			ResolvedGenericArg::Binding { name, ty, .. } => TyGenericArg::Binding {
				name: name.clone(),
				ty: self.lower_ty(ty)?,
			},
		});
	}

	fn lower_where_constraint(&mut self, c: &ResolvedWhereConstraint) -> Result<TypedWhereConstraint, TypeError>
	{
		return Ok(TypedWhereConstraint {
			ty: c.ty.clone(),
			bounds: c
				.bounds
				.iter()
				.map(|b| return self.lower_where_bound(b))
				.collect::<Result<_, _>>()?,
			type_args: c
				.type_args
				.iter()
				.map(|t| return self.lower_ty(t))
				.collect::<Result<_, _>>()?,
			span: c.span,
		});
	}

	fn lower_where_constraints_with_generics(
		&mut self,
		generic_params: &[(parser::Ident, Span)],
		where_clause: &[ResolvedWhereConstraint],
	) -> Result<Vec<TypedWhereConstraint>, TypeError>
	{
		let mut tmp_frame: GenericFrame = GenericFrame::default();
		for (name, _) in generic_params {
			tmp_frame.0.insert(
				name.clone(),
				Ty::Generic {
					name: name.clone(),
					bounds: Vec::new(),
				},
			);
		}
		self.generic_scope.push(tmp_frame);

		let result: Result<Vec<TypedWhereConstraint>, TypeError> = where_clause
			.iter()
			.map(|c| return self.lower_where_constraint(c))
			.collect::<Result<Vec<_>, _>>();

		self.generic_scope.pop();
		return result;
	}

	fn generic_pairs(params: &[parser::GenericParam]) -> Vec<(parser::Ident, Span)>
	{
		return params.iter().map(|g| return (g.name.clone(), g.span)).collect();
	}

	fn scan_block(&mut self, block: &'a ResolvedTopLevelBlock) -> Result<(), TypeError>
	{
		for decl in &block.items {
			self.scan_decl(decl)?;
		}
		return Ok(());
	}

	fn scan_decl(&mut self, decl: &'a ResolvedTopLevelDecl) -> Result<(), TypeError>
	{
		match decl {
			ResolvedTopLevelDecl::Function(f) => return self.scan_function_sig(&f.signature),
			ResolvedTopLevelDecl::VariableDecl(v) => return self.scan_variable(v),
			ResolvedTopLevelDecl::Struct(s) => return self.scan_struct(s),
			ResolvedTopLevelDecl::Union(u) => return self.scan_union(u),
			ResolvedTopLevelDecl::Enum(e) => {
				return {
					self.scan_enum(e);
					Ok(())
				};
			}
			ResolvedTopLevelDecl::Variant(v) => return self.scan_variant(v),
			ResolvedTopLevelDecl::TypeAlias(t) => return self.scan_type_alias(t),
			ResolvedTopLevelDecl::Trait(t) => return self.scan_trait(t),
			ResolvedTopLevelDecl::Module(m) => return self.scan_module(m),
			ResolvedTopLevelDecl::Impl(i) => return self.scan_impl(i),
			ResolvedTopLevelDecl::Directive(_) => return Ok(()),
		}
	}

	fn scan_function_sig(&mut self, sig: &ResolvedFunctionSignature) -> Result<(), TypeError>
	{
		let where_clause: Vec<TypedWhereConstraint> =
			self.lower_where_constraints_with_generics(&sig.generics, &sig.where_clause)?;
		self.generic_scope.push_params(&sig.generics, &where_clause);

		let return_ty = self.lower_ty(&sig.return_type)?;

		self.caches.env.insert(sig.resolved_name, return_ty);
		self.constraints
			.fn_call_type_cache
			.insert(sig.resolved_name, sig.call_type);

		for (i, param) in sig.params.iter().enumerate() {
			let ty = self.lower_ty(&param.ty)?;
			self.caches.param.insert(sig.resolved_name, i, ty.clone());
			if param.symbol.0 == usize::MAX {
				continue;
			}
			self.caches.env.insert(param.symbol, ty);
		}

		let mut bounds_for_fn: Vec<(String, TyBound)> = Vec::new();
		for constraint in &where_clause {
			for bound in &constraint.bounds {
				bounds_for_fn.push((constraint.ty.clone(), bound.clone()));
			}
		}
		if !bounds_for_fn.is_empty() {
			self.constraints
				.fn_where_bounds
				.insert(sig.resolved_name, bounds_for_fn);
		}

		if let Some(sym) = self.global.symbols.get(sig.resolved_name.0)
			&& let SymbolKind::Function { variadic: true, .. } = &sym.kind
		{
			let is_extern_c = sig
				.modifiers
				.iter()
				.any(|m| matches!(m, parser::Modifier::Extern(Some(parser::ExternLanguage::C))));
			if !is_extern_c {
				self.generic_scope.pop();
				return Err(Self::err(
					sig.span,
					TypeErrorKind::TypeMismatch {
						expected: "variadic functions require `extern(C)`; Leaf variadics are not yet supported".into(),
						found: "variadic non-extern(C) function".into(),
					},
				));
			}
			self.constraints.variadic_fns.insert(sig.resolved_name);
		}

		self.generic_scope.pop();
		return Ok(());
	}

	fn scan_variable(&mut self, v: &ResolvedVariableDecl) -> Result<(), TypeError>
	{
		let ty = self.lower_ty(&v.ty)?;
		self.caches.env.insert(v.resolved_name, ty);
		return Ok(());
	}

	fn scan_struct(&mut self, s: &ResolvedStructDecl) -> Result<(), TypeError>
	{
		self.caches.env.insert(s.resolved_name, Ty::named(s.resolved_name));

		let pairs: Vec<(String, Span)> = Self::generic_pairs(&s.generics);
		let where_clause: Vec<TypedWhereConstraint> =
			self.lower_where_constraints_with_generics(&pairs, &s.where_clause)?;
		self.generic_scope.push_generic_params(&s.generics, &where_clause);

		for field in &s.fields {
			let ty: Ty = self.lower_ty(&field.ty)?;
			self.caches.field.insert(s.resolved_name, &field.name, ty.clone());
			if let Some(default_expr) = &field.default_value {
				let te = self.check_expr(default_expr, Some(&ty))?;
				self.expect_ty(&te.ty, &ty, default_expr.span())?;
				self.caches
					.field_default
					.insert((s.resolved_name, field.name.clone()), te);
			}
		}

		{
			let param_names: Vec<String> = s.generics.iter().map(|g| return g.name.clone()).collect();
			let mut bounds_for_struct: Vec<(usize, TyBound)> = Vec::new();
			for constraint in &where_clause {
				if let Some(idx) = param_names.iter().position(|n| return n == &constraint.ty) {
					for bound in &constraint.bounds {
						bounds_for_struct.push((idx, bound.clone()));
					}
				}
			}
			if !bounds_for_struct.is_empty() {
				self.constraints
					.struct_where_bounds
					.insert(s.resolved_name, bounds_for_struct);
			}
		}

		self.generic_scope.pop();
		return Ok(());
	}

	fn scan_union(&mut self, u: &ResolvedUnionDecl) -> Result<(), TypeError>
	{
		self.caches.env.insert(u.resolved_name, Ty::named(u.resolved_name));

		let pairs: Vec<(String, Span)> = Self::generic_pairs(&u.generics);
		let where_clause: Vec<TypedWhereConstraint> =
			self.lower_where_constraints_with_generics(&pairs, &u.where_clause)?;
		self.generic_scope.push_generic_params(&u.generics, &where_clause);

		for field in &u.fields {
			let ty: Ty = self.lower_ty(&field.ty)?;
			self.caches.field.insert(u.resolved_name, &field.name, ty);
		}

		self.generic_scope.pop();
		return Ok(());
	}

	fn scan_enum(&mut self, e: &ResolvedEnumDecl)
	{
		let enum_ty = Ty::named(e.resolved_name);
		self.caches.env.insert(e.resolved_name, enum_ty.clone());

		if let Some(sym) = self.global.symbols.get(e.resolved_name.0)
			&& let Some(scope_id) = sym.introduced_scope
		{
			let scope = self.global.scope(scope_id);
			for &mem_id in &scope.symbols {
				let mem = self.global.symbol(mem_id);
				if matches!(mem.kind, SymbolKind::EnumVariant) && e.variants.iter().any(|v| return v.name == mem.name) {
					self.caches.env.insert(mem_id, enum_ty.clone());
				}
			}
		}
	}

	fn scan_variant(&mut self, v: &ResolvedVariantDecl) -> Result<(), TypeError>
	{
		self.caches.env.insert(v.resolved_name, Ty::named(v.resolved_name));

		self.caches.variant_generics.insert(
			v.resolved_name,
			v.generics.iter().map(|g| return g.name.clone()).collect(),
		);

		let pairs: Vec<(String, Span)> = Self::generic_pairs(&v.generics);
		let where_clause: Vec<TypedWhereConstraint> = self.lower_where_constraints_with_generics(&pairs, &[])?;
		self.generic_scope.push_generic_params(&v.generics, &where_clause);

		if let Some(sym) = self.global.symbols.get(v.resolved_name.0)
			&& let Some(scope_id) = sym.introduced_scope
		{
			let scope: &Scope = self.global.scope(scope_id);
			for &mem_sym_id in &scope.symbols {
				let mem_sym: &Symbol = self.global.symbol(mem_sym_id);
				if !matches!(mem_sym.kind, SymbolKind::VariantMember) {
					continue;
				}
				if let Some(member) = v.variants.iter().find(|m| return m.name == mem_sym.name)
					&& let Some(ty_node) = &member.ty
				{
					let ty = self.lower_ty(ty_node)?;
					self.caches.env.insert(mem_sym_id, ty);
				}
			}
		}

		{
			let param_names: Vec<String> = v.generics.iter().map(|g| return g.name.clone()).collect();
			let mut bounds_for_struct: Vec<(usize, TyBound)> = Vec::new();
			for constraint in &where_clause {
				if let Some(idx) = param_names.iter().position(|n| return n == &constraint.ty) {
					for bound in &constraint.bounds {
						bounds_for_struct.push((idx, bound.clone()));
					}
				}
			}
			if !bounds_for_struct.is_empty() {
				self.constraints
					.struct_where_bounds
					.insert(v.resolved_name, bounds_for_struct);
			}
		}

		self.generic_scope.pop();
		return Ok(());
	}

	fn scan_type_alias(&mut self, t: &ResolvedTypeAliasDecl) -> Result<(), TypeError>
	{
		let ty: Ty = self.lower_ty(&t.ty)?;
		self.caches.env.insert(t.resolved_name, ty);
		return Ok(());
	}

	fn scan_assoc_type(&mut self, t: &ResolvedAssocTypeDecl) -> Result<(), TypeError>
	{
		if let Some(rty) = &t.ty {
			let ty: Ty = self.lower_ty(rty)?;
			self.caches.env.insert(t.resolved_name, ty);
		}
		return Ok(());
	}

	fn scan_trait(&mut self, t: &'a ResolvedTraitDecl) -> Result<(), TypeError>
	{
		if is_builtin_modifier(&t.modifiers) {
			self.traits.builtins.insert(t.resolved_name);
		}
		self.traits.decls.insert(t.resolved_name, t);
		self.caches.env.insert(t.resolved_name, Ty::named(t.resolved_name));

		let pairs: Vec<(parser::Ident, Span)> = t.generics.iter().map(|g| return (g.name.clone(), g.span)).collect();
		let where_clause = self.lower_where_constraints_with_generics(&pairs, &[])?;
		self.generic_scope.push_params(&pairs, &where_clause);

		for item in &t.items {
			match item {
				ResolvedTraitItem::Function(f) => {
					self.scan_function_sig(&f.signature)?;

					let return_ty = self
						.caches
						.env
						.get(f.signature.resolved_name)
						.cloned()
						.unwrap_or(Ty::Unit);
					self.caches
						.method
						.insert_sym(t.resolved_name, &f.signature.name, return_ty.clone());
					self.caches.method_fn.insert_sym(
						t.resolved_name,
						f.signature.name.clone(),
						f.signature.resolved_name,
					);
				}
				ResolvedTraitItem::TypeAlias(ta) => self.scan_type_alias(ta)?,
				ResolvedTraitItem::AssocType(ta) => self.scan_assoc_type(ta)?,
				ResolvedTraitItem::Const(c) => self.scan_variable(c)?,
			}
		}

		self.generic_scope.pop();
		return Ok(());
	}

	fn scan_module(&mut self, m: &'a ResolvedModuleDecl) -> Result<(), TypeError>
	{
		self.caches.env.insert(m.resolved_name, Ty::named(m.resolved_name));
		if let Some(body) = &m.resolved_body {
			self.scan_block(body)?;
		}
		return Ok(());
	}

	fn scan_impl(&mut self, i: &ResolvedImplDecl) -> Result<(), TypeError>
	{
		let impl_pairs: Vec<(parser::Ident, Span)> = Self::generic_pairs(&i.generics);
		let where_clause: Vec<TypedWhereConstraint> =
			self.lower_where_constraints_with_generics(&impl_pairs, &i.where_clause)?;
		self.generic_scope.push_params(&impl_pairs, &where_clause);

		let target_is_builtin_trait = match &i.resolved_target.kind {
			ResolvedPathKind::Resolved(id) => self.traits.builtins.contains(id),
			_ => false,
		};

		let impl_key: Option<TyKey> = if target_is_builtin_trait {
			None
		} else {
			match &i.resolved_target.kind {
				ResolvedPathKind::Resolved(id) => Some(TyKey::Symbol(*id)),
				ResolvedPathKind::Primitive(ty) => ty.as_primitive().map(|p| return TyKey::Prim(p.clone())),
				ResolvedPathKind::AssocItem { base, .. } => Some(TyKey::Symbol(*base)),
			}
		};

		let self_ty: Ty = match &i.resolved_target.kind {
			ResolvedPathKind::Resolved(id) => {
				let sym = self.global.symbol(*id);
				#[allow(clippy::redundant_else)]
				if matches!(sym.kind, SymbolKind::GenericParam) {
					self.generic_scope.lookup(&sym.name).cloned().unwrap_or_else(|| {
						return Ty::Generic {
							name: sym.name.clone(),
							bounds: Vec::new(),
						};
					})
				} else {
					let generics: Vec<Ty> = i
						.generics
						.iter()
						.map(|g| {
							return self.generic_scope.lookup(&g.name).cloned().unwrap_or_else(|| {
								return Ty::Generic {
									name: g.name.clone(),
									bounds: Vec::new(),
								};
							});
						})
						.collect();
					Ty::Named { symbol: *id, generics }
				}
			}
			ResolvedPathKind::AssocItem { base, .. } => Ty::named(*base),
			ResolvedPathKind::Primitive(ty) => ty.clone(),
		};

		let prev_self: Option<Ty> = self.fn_ctx.self_ty.replace(self_ty);
		let prev_assoc: HashMap<String, Ty> = std::mem::take(&mut self.fn_ctx.assoc_types);

		let type_sym: Option<SymbolId> = match &i.resolved_target.kind {
			ResolvedPathKind::Resolved(id) => Some(*id),
			ResolvedPathKind::AssocItem { base, .. } => Some(*base),
			ResolvedPathKind::Primitive(_) => None,
		};

		for item in &i.items {
			match item {
				ResolvedImplItem::Function(f) => {
					self.scan_function_sig(&f.signature)?;

					if let Some(ref key) = impl_key {
						let return_ty = self
							.caches
							.env
							.get(f.signature.resolved_name)
							.cloned()
							.unwrap_or(Ty::Unit);
						let freturn_ty = self.resolve_assoc_types_in_ty(&return_ty);
						self.caches.method.insert(key.clone(), &f.signature.name, freturn_ty);
						self.caches
							.method_fn
							.insert(key.clone(), f.signature.name.clone(), f.signature.resolved_name);
					}
					let mut bounds_for_fn: Vec<(String, TyBound)> = self
						.constraints
						.fn_where_bounds
						.get(&f.signature.resolved_name)
						.cloned()
						.unwrap_or_default();
					for constraint in &where_clause {
						for bound in &constraint.bounds {
							bounds_for_fn.push((constraint.ty.clone(), bound.clone()));
						}
					}
					if !bounds_for_fn.is_empty() {
						self.constraints
							.fn_where_bounds
							.insert(f.signature.resolved_name, bounds_for_fn);
					}
				}
				ResolvedImplItem::TypeAlias(ta) => self.scan_type_alias(ta)?,
				ResolvedImplItem::AssocType(ta) => {
					self.scan_assoc_type(ta)?;
					if let Some(ty) = self.caches.env.get(ta.resolved_name).cloned() {
						self.fn_ctx.assoc_types.insert(ta.name.clone(), ty.clone());
						if let (Some(key), Some(trait_path)) = (&impl_key, &i.resolved_trait)
							&& let ResolvedPathKind::Resolved(trait_sym_id) = &trait_path.kind
						{
							self.caches
								.impl_assoc
								.insert((key.clone(), *trait_sym_id, ta.name.clone()), ty);
							let param_names: Vec<String> = i.generics.iter().map(|g| return g.name.clone()).collect();
							self.caches
								.impl_assoc_generic_params
								.insert((key.clone(), *trait_sym_id, ta.name.clone()), param_names);
						}
					}
				}
				ResolvedImplItem::Const(c) => self.scan_variable(c)?,
			}
		}

		if let Some(trait_path) = &i.resolved_trait
			&& let ResolvedPathKind::Resolved(trait_sym_id) = &trait_path.kind
		{
			if let Some(ref key) = impl_key {
				self.traits.impls.entry(key.clone()).or_default().insert(*trait_sym_id);
			}

			if type_sym.is_some() {
				let provided: HashSet<String> = i
					.items
					.iter()
					.filter_map(|item| {
						return if let ResolvedImplItem::Function(f) = item {
							Some(f.signature.name.clone())
						} else {
							None
						};
					})
					.collect();

				let trait_sym = *trait_sym_id;
				if let Some(scope_id) = self.global.symbol(trait_sym).introduced_scope {
					let scope = self.global.scope(scope_id);
					for &fn_sym_id in &scope.symbols {
						let fn_sym = self.global.symbol(fn_sym_id);
						if !matches!(fn_sym.kind, SymbolKind::Function { .. }) {
							continue;
						}
						if provided.contains(&fn_sym.name) {
							continue;
						}
						if let Some(ret_ty) = self.caches.method.get_sym(trait_sym, &fn_sym.name).cloned()
							&& let Some(ref key) = impl_key
						{
							self.caches.method.insert(key.clone(), &fn_sym.name, ret_ty);
						}
						if let Some(ref key) = impl_key {
							self.caches
								.method_fn
								.map
								.entry((key.clone(), fn_sym.name.clone()))
								.or_insert(fn_sym_id);
						}
					}
				}
			}
		}

		if target_is_builtin_trait
			&& let ResolvedPathKind::Resolved(builtin_sym) = &i.resolved_target.kind
			&& let Some(trait_path) = &i.resolved_trait
			&& let ResolvedPathKind::Resolved(granted_sym) = &trait_path.kind
		{
			let saved_self = self.fn_ctx.self_ty.take();
			let saved_assoc = std::mem::take(&mut self.fn_ctx.assoc_types);
			for item in &i.items {
				if let ResolvedImplItem::AssocType(ta) = item
					&& let Some(rty) = &ta.ty
					&& let Ok(ty) = self.lower_ty(rty)
				{
					self.fn_ctx.assoc_types.insert(ta.name.clone(), ty);
				}
			}
			let methods: Vec<BlanketMethod> = i
				.items
				.iter()
				.filter_map(|item| {
					return if let ResolvedImplItem::Function(f) = item {
						let has_self_param = f.signature.params.iter().any(|p| return p.symbol.0 == usize::MAX);
						let param_tys: Vec<Ty> = f
							.signature
							.params
							.iter()
							.filter(|p| return p.symbol.0 != usize::MAX)
							.map(|p| return self.lower_ty(&p.ty).unwrap_or(Ty::Infer))
							.collect();
						let return_ty: Ty = self.lower_ty(&f.signature.return_type).unwrap_or(Ty::Unit);
						Some(BlanketMethod {
							name: f.signature.name.clone(),
							param_tys,
							return_ty,
							fn_sym: f.signature.resolved_name,
							has_self_param,
						})
					} else {
						None
					};
				})
				.collect();
			self.fn_ctx.self_ty = saved_self;
			self.fn_ctx.assoc_types = saved_assoc;
			self.traits.blanket_impls.push(BlanketImpl {
				required_builtin: *builtin_sym,
				granted_trait: *granted_sym,
				methods,
			});
		}

		if !i.generics.is_empty()
			&& let Some(trait_path) = &i.resolved_trait
			&& let ResolvedPathKind::Resolved(granted_sym) = &trait_path.kind
		{
			let granted_sym = *granted_sym;

			let required_builtins: Vec<SymbolId> = where_clause
				.iter()
				.flat_map(|c| return c.bounds.iter())
				.filter_map(|b| {
					return if let TyBound::Trait { symbol, .. } = b
						&& self.traits.builtins.contains(symbol)
					{
						Some(*symbol)
					} else {
						None
					};
				})
				.collect();

			if !required_builtins.is_empty() {
				let saved_self = self.fn_ctx.self_ty.take();
				let saved_assoc = std::mem::take(&mut self.fn_ctx.assoc_types);
				for item in &i.items {
					if let ResolvedImplItem::AssocType(ta) = item
						&& let Some(rty) = &ta.ty
						&& let Ok(ty) = self.lower_ty(rty)
					{
						self.fn_ctx.assoc_types.insert(ta.name.clone(), ty);
					}
				}
				let methods: Vec<BlanketMethod> = i
					.items
					.iter()
					.filter_map(|item| {
						return if let ResolvedImplItem::Function(f) = item {
							let has_self_param = f.signature.params.iter().any(|p| return p.symbol.0 == usize::MAX);
							let param_tys: Vec<Ty> = f
								.signature
								.params
								.iter()
								.filter(|p| return p.symbol.0 != usize::MAX)
								.map(|p| return self.lower_ty(&p.ty).unwrap_or(Ty::Infer))
								.collect();
							let return_ty = self.lower_ty(&f.signature.return_type).unwrap_or(Ty::Unit);
							Some(BlanketMethod {
								name: f.signature.name.clone(),
								param_tys,
								return_ty,
								fn_sym: f.signature.resolved_name,
								has_self_param,
							})
						} else {
							None
						};
					})
					.collect();
				self.fn_ctx.self_ty = saved_self;
				self.fn_ctx.assoc_types = saved_assoc;

				for required_sym in required_builtins {
					self.traits.blanket_impls.push(BlanketImpl {
						required_builtin: required_sym,
						granted_trait: granted_sym,
						methods: methods.clone(),
					});
				}
			}
		}

		self.fn_ctx.self_ty = prev_self;
		self.fn_ctx.assoc_types = prev_assoc;
		self.generic_scope.pop();
		return Ok(());
	}

	fn check_block_tld(&mut self, block: &ResolvedTopLevelBlock) -> Result<TypedTopLevelBlock, TypeError>
	{
		let items: Vec<TypedTopLevelDecl> = block
			.items
			.iter()
			.map(|d| return self.check_tld(d))
			.collect::<Result<Vec<_>, _>>()?;
		return Ok(TypedTopLevelBlock {
			items,
			span: block.span,
		});
	}

	fn check_tld(&mut self, decl: &ResolvedTopLevelDecl) -> Result<TypedTopLevelDecl, TypeError>
	{
		return Ok(match decl {
			ResolvedTopLevelDecl::Function(f) => TypedTopLevelDecl::Function(self.check_function(f)?),
			ResolvedTopLevelDecl::VariableDecl(v) => TypedTopLevelDecl::VariableDecl(self.check_var_decl(v, false)?),
			ResolvedTopLevelDecl::Struct(s) => TypedTopLevelDecl::Struct(self.check_struct(s)?),
			ResolvedTopLevelDecl::Union(u) => TypedTopLevelDecl::Union(self.check_union(u)?),
			ResolvedTopLevelDecl::Enum(e) => TypedTopLevelDecl::Enum(self.check_enum(e)?),
			ResolvedTopLevelDecl::Variant(v) => TypedTopLevelDecl::Variant(self.check_variant(v)?),
			ResolvedTopLevelDecl::TypeAlias(t) => TypedTopLevelDecl::TypeAlias(self.check_type_alias(t)?),
			ResolvedTopLevelDecl::Trait(t) => TypedTopLevelDecl::Trait(self.check_trait(t)?),
			ResolvedTopLevelDecl::Module(m) => TypedTopLevelDecl::Module(self.check_module(m)?),
			ResolvedTopLevelDecl::Impl(i) => TypedTopLevelDecl::Impl(self.check_impl(i)?),
			ResolvedTopLevelDecl::Directive(d) => TypedTopLevelDecl::Directive(self.check_directive(d)?),
		});
	}

	fn check_function(&mut self, func: &ResolvedFunctionDecl) -> Result<TypedFunctionDecl, TypeError>
	{
		let sig: &ResolvedFunctionSignature = &func.signature;

		let prev_call_type = self.fn_ctx.call_type;
		self.fn_ctx.call_type = sig.call_type;

		let where_clause: Vec<TypedWhereConstraint> =
			self.lower_where_constraints_with_generics(&sig.generics, &sig.where_clause)?;
		self.generic_scope.push_params(&sig.generics, &where_clause);

		let params: Vec<TypedParam> = sig
			.params
			.iter()
			.map(|p| {
				return Ok(TypedParam {
					symbol: p.symbol,
					name: p.name.clone(),
					ty: self.lower_ty(&p.ty)?,
					mutable: p.mutable,
					variadic: p.variadic,
					span: p.span,
				});
			})
			.collect::<Result<_, TypeError>>()?;

		let return_ty: Ty = self.lower_ty(&sig.return_type).unwrap_or(Ty::Unit);

		self.caches.env.insert(sig.resolved_name, return_ty.clone());

		let typed_sig: TypedFunctionSignature = TypedFunctionSignature {
			resolved_name: sig.resolved_name,
			name: sig.name.clone(),
			modifiers: sig.modifiers.clone(),
			generics: sig.generics.clone(),
			heap_generics: sig.heap_generics.clone(),
			call_type: sig.call_type,
			params,
			return_type: return_ty.clone(),
			where_clause,
			span: sig.span,
		};

		let prev_return: Option<Ty> = self.fn_ctx.return_ty.replace(return_ty.clone());

		let prev_heap_params: HashMap<String, Ty> = std::mem::take(&mut self.fn_ctx.heap_params);
		for hp in &sig.heap_generics {
			let ty: Ty = match &hp.kind {
				ResolvedGenericHeapKind::Forwarded => {
					let trait_sym = match hp.name.as_str() {
						// TODO: I don't like this solution for now, but it's good enough for finishing V0.1
						"alloc" => self.traits.heap_syms.alloc,
						"io" => self.traits.heap_syms.io,
						_ => None,
					};
					let bounds = match trait_sym {
						Some(sym) => vec![TyBound::Trait {
							symbol: sym,
							args: Vec::new(),
						}],
						None => self
							.global
							.symbols
							.iter()
							.enumerate()
							.find_map(|(i, sym)| {
								if sym.name.eq_ignore_ascii_case(&hp.name) && matches!(sym.kind, SymbolKind::Trait) {
									return Some(vec![TyBound::Trait {
										symbol: SymbolId(i),
										args: Vec::new(),
									}]);
								}
								return None;
							})
							.unwrap_or_default(),
					};
					if bounds.is_empty() {
						Ty::Generic {
							name: hp.name.clone(),
							bounds: Vec::new(),
						}
					} else {
						Ty::ImplTrait { bounds, concrete: None }
					}
				}
				ResolvedGenericHeapKind::Forced(resolved_ty) => self.lower_ty(resolved_ty)?,
			};
			self.fn_ctx.heap_params.insert(hp.name.clone(), ty);
		}

		let body: Option<TypedBlock> = func
			.body
			.as_ref()
			.map(|b| {
				let tb = self.check_block_as_value(b)?;
				self.expect_ty(&tb.ty, &return_ty, b.span).map_err(|_| {
					return Self::err(
						b.span,
						TypeErrorKind::ReturnTypeMismatch {
							expected: self.fmt_ty(&return_ty),
							found: self.fmt_ty(&tb.ty),
						},
					);
				})?;
				return Ok(tb);
			})
			.transpose()?;

		self.fn_ctx.return_ty = prev_return;
		self.fn_ctx.heap_params = prev_heap_params;
		self.generic_scope.pop();
		self.fn_ctx.call_type = prev_call_type;

		return Ok(TypedFunctionDecl {
			resolved_name: func.resolved_name,
			signature: typed_sig,
			body,
			docs: func.docs.clone(),
			span: func.span,
		});
	}

	fn check_var_decl(&mut self, v: &ResolvedVariableDecl, allow_infer: bool) -> Result<TypedVariableDecl, TypeError>
	{
		let declared: Ty = self.lower_ty(&v.ty)?;

		let (ty, init) = if declared == Ty::Infer {
			if !allow_infer {
				return Err(Self::err(v.span, TypeErrorKind::CannotInferType));
			}
			match &v.init {
				Some(expr) => {
					let te: TypedExpr = self.check_expr(expr, None)?;
					let inferred: Ty = te.ty.clone();
					self.caches.env.insert(v.resolved_name, inferred.clone());
					(inferred, Some(te))
				}
				None => return Err(Self::err(v.span, TypeErrorKind::CannotInferType)),
			}
		} else if let Ty::ImplTrait { ref bounds, .. } = declared {
			if let Some(expr) = &v.init {
				let is_literal = matches!(expr, ResolvedExpr::Literal { .. });

				if is_literal {
					let te_raw = self.check_expr(expr, None)?;
					if te_raw.ty == Ty::Infer {
						let bound_str = bounds
							.iter()
							.map(|b| {
								return match b {
									TyBound::Trait { symbol, .. } => self.global.symbol(*symbol).name.clone(),
									TyBound::Fn { .. } => "Fn".to_string(),
								};
							})
							.collect::<Vec<_>>()
							.join(" + ");
						return Err(Self::err(
							v.span,
							TypeErrorKind::TypeMismatch {
								expected: format!("`impl {bound_str}` requires a resolved type as initializer"),
								found: "an ambiguous type that can never be resolved \
											(hint: annotate the literal, e.g. `0i64`)"
									.to_string(),
							},
						));
					}
				}

				let te: TypedExpr = self.check_expr(expr, Some(&declared))?;
				let concrete: Ty = te.ty.clone();

				match &concrete {
					Ty::Infer => {
						let bound_str = bounds
							.iter()
							.map(|b| {
								return match b {
									TyBound::Trait { symbol, .. } => self.global.symbol(*symbol).name.clone(),
									TyBound::Fn { .. } => "Fn".to_string(),
								};
							})
							.collect::<Vec<_>>()
							.join(" + ");
						return Err(Self::err(
							v.span,
							TypeErrorKind::TypeMismatch {
								expected: format!("`impl {bound_str}` requires a resolved type as initializer"),
								found: "type could not be resolved".to_string(),
							},
						));
					}

					Ty::ImplTrait { bounds: src_bounds, .. } | Ty::Generic { bounds: src_bounds, .. } => {
						for required in bounds {
							let TyBound::Trait { symbol: req_sym, .. } = required else {
								continue;
							};
							let satisfied = src_bounds.iter().any(|sb| {
								return if let TyBound::Trait { symbol: src_sym, .. } = sb {
									src_sym == req_sym
								} else {
									false
								};
							});
							if !satisfied {
								let bound_str = self.global.symbol(*req_sym).name.clone();
								return Err(Self::err(
									v.span,
									TypeErrorKind::TypeMismatch {
										expected: format!("initializer to implement `{bound_str}`"),
										found: format!("`{bound_str}` is not present on the source type"),
									},
								));
							}
						}
						let pinned = Ty::ImplTrait {
							bounds: bounds.clone(),
							concrete: None,
						};
						self.caches.env.insert(v.resolved_name, pinned);
						(declared, Some(te))
					}

					_ => {
						for bound in bounds {
							self.apply_blanket_impls_for_ty(&concrete);
							if !self.ty_satisfies_bound(&concrete, bound) {
								let bound_str = match bound {
									TyBound::Trait { symbol, .. } => self.global.symbol(*symbol).name.clone(),
									TyBound::Fn { .. } => "Fn".to_string(),
								};
								return Err(Self::err(
									v.span,
									TypeErrorKind::TypeMismatch {
										expected: format!("`{}` to implement `{bound_str}`", self.fmt_ty(&concrete)),
										found: format!("`{}` does not implement `{bound_str}`", self.fmt_ty(&concrete)),
									},
								));
							}
						}
						let pinned = Ty::ImplTrait {
							bounds: bounds.clone(),
							concrete: Some(Box::new(concrete.clone())),
						};
						self.caches.env.insert(v.resolved_name, pinned.clone());
						(pinned, Some(te))
					}
				}
			} else {
				self.caches.env.insert(v.resolved_name, declared.clone());
				(declared, None)
			}
		} else {
			let init: Option<TypedExpr> = v
				.init
				.as_ref()
				.map(|expr| {
					let te = self.check_expr(expr, Some(&declared))?;

					if let (Ty::Generic { name, .. }, Ty::Generic { .. }) = (&te.ty, &declared) {
						let gname = name.clone();
						if !self.expr_provably_returns_generic(&te, &gname) {
							return Err(self.mismatch(expr.span(), &declared, &te.ty));
						}
						return Ok(te);
					}

					if matches!(&te.ty, Ty::Generic { .. }) && !matches!(&declared, Ty::Generic { .. } | Ty::Infer) {
						return Err(self.mismatch(expr.span(), &declared, &te.ty));
					}
					self.expect_ty(&te.ty, &declared, expr.span())?;
					return Ok(te);
				})
				.transpose()?;
			self.caches.env.insert(v.resolved_name, declared.clone());
			(declared, init)
		};

		return Ok(TypedVariableDecl {
			resolved_name: v.resolved_name,
			name: v.name.clone(),
			ty,
			init,
			comp_const: v.comp_const,
			mutable: v.mutable,
			modifiers: v.modifiers.clone(),
			docs: v.docs.clone(),
			span: v.span,
		});
	}

	fn check_struct(&mut self, s: &ResolvedStructDecl) -> Result<TypedStructDecl, TypeError>
	{
		let pairs: Vec<(String, Span)> = Self::generic_pairs(&s.generics);
		let where_clause: Vec<TypedWhereConstraint> =
			self.lower_where_constraints_with_generics(&pairs, &s.where_clause)?;
		self.generic_scope.push_generic_params(&s.generics, &where_clause);

		let fields: Vec<TypedStructField> = s
			.fields
			.iter()
			.map(|f| {
				let ty: Ty = self
					.caches
					.field
					.get(s.resolved_name, &f.name)
					.cloned()
					.ok_or_else(|| {
						return Self::err(f.span, TypeErrorKind::UnknownSymbol { id: s.resolved_name });
					})?;
				let default_value: Option<TypedExpr> = f
					.default_value
					.as_ref()
					.map(|e| {
						let te = self.check_expr(e, Some(&ty))?;
						self.expect_ty(&te.ty, &ty, e.span())?;
						return Ok(te);
					})
					.transpose()?;
				return Ok(TypedStructField {
					name: f.name.clone(),
					ty,
					default_value,
					modifiers: f.modifiers.clone(),
					docs: f.docs.clone(),
					span: f.span,
				});
			})
			.collect::<Result<Vec<_>, TypeError>>()?;

		self.generic_scope.pop();

		return Ok(TypedStructDecl {
			resolved_name: s.resolved_name,
			name: s.name.clone(),
			modifiers: s.modifiers.clone(),
			generics: s.generics.clone(),
			fields,
			where_clause,
			docs: s.docs.clone(),
			span: s.span,
		});
	}

	fn check_union(&mut self, u: &ResolvedUnionDecl) -> Result<TypedUnionDecl, TypeError>
	{
		let pairs: Vec<(String, Span)> = Self::generic_pairs(&u.generics);
		let where_clause: Vec<TypedWhereConstraint> =
			self.lower_where_constraints_with_generics(&pairs, &u.where_clause)?;
		self.generic_scope.push_generic_params(&u.generics, &where_clause);

		let fields: Vec<TypedUnionField> = u
			.fields
			.iter()
			.map(|f| {
				let ty = self
					.caches
					.field
					.get(u.resolved_name, &f.name)
					.cloned()
					.ok_or_else(|| {
						return Self::err(f.span, TypeErrorKind::UnknownSymbol { id: u.resolved_name });
					})?;
				return Ok(TypedUnionField {
					name: f.name.clone(),
					ty,
					modifiers: f.modifiers.clone(),
					docs: f.docs.clone(),
					span: f.span,
				});
			})
			.collect::<Result<Vec<_>, TypeError>>()?;

		self.generic_scope.pop();

		return Ok(TypedUnionDecl {
			resolved_name: u.resolved_name,
			name: u.name.clone(),
			modifiers: u.modifiers.clone(),
			generics: u.generics.clone(),
			fields,
			where_clause,
			docs: u.docs.clone(),
			span: u.span,
		});
	}

	fn check_enum(&mut self, e: &ResolvedEnumDecl) -> Result<TypedEnumDecl, TypeError>
	{
		let self_ty: Ty = Ty::named(e.resolved_name);
		let variants: Vec<TypedEnumVariant> = e
			.variants
			.iter()
			.map(|v| {
				let value = v
					.value
					.as_ref()
					.map(|expr| return self.check_expr(expr, Some(&self_ty)))
					.transpose()?;
				return Ok(TypedEnumVariant {
					name: v.name.clone(),
					value,
					docs: v.docs.clone(),
					span: v.span,
				});
			})
			.collect::<Result<Vec<_>, TypeError>>()?;

		return Ok(TypedEnumDecl {
			resolved_name: e.resolved_name,
			name: e.name.clone(),
			modifiers: e.modifiers.clone(),
			generics: e.generics.clone(),
			variants,
			docs: e.docs.clone(),
			span: e.span,
		});
	}

	fn check_variant(&mut self, v: &ResolvedVariantDecl) -> Result<TypedVariantDecl, TypeError>
	{
		let pairs: Vec<(String, Span)> = Self::generic_pairs(&v.generics);
		let where_clause: Vec<TypedWhereConstraint> = self.lower_where_constraints_with_generics(&pairs, &[])?;
		self.generic_scope.push_generic_params(&v.generics, &where_clause);

		let variants: Vec<TypedVariantMember> = v
			.variants
			.iter()
			.map(|m| {
				let ty = m.ty.as_ref().map(|t| return self.lower_ty(t)).transpose()?;
				let value = m
					.value
					.as_ref()
					.map(|e| {
						let te = self.check_expr(e, ty.as_ref())?;
						if let Some(expected) = &ty {
							self.expect_ty(&te.ty, expected, e.span())?;
						}
						return Ok(te);
					})
					.transpose()?;
				return Ok(TypedVariantMember {
					name: m.name.clone(),
					ty,
					value,
					docs: m.docs.clone(),
					span: m.span,
				});
			})
			.collect::<Result<Vec<_>, TypeError>>()?;

		self.generic_scope.pop();

		return Ok(TypedVariantDecl {
			resolved_name: v.resolved_name,
			name: v.name.clone(),
			modifiers: v.modifiers.clone(),
			generics: v.generics.clone(),
			variants,
			docs: v.docs.clone(),
			span: v.span,
		});
	}

	fn check_type_alias(&mut self, t: &ResolvedTypeAliasDecl) -> Result<TypedTypeAliasDecl, TypeError>
	{
		return Ok(TypedTypeAliasDecl {
			resolved_name: t.resolved_name,
			name: t.name.clone(),
			modifiers: t.modifiers.clone(),
			generics: t.generics.clone(),
			ty: self.lower_ty(&t.ty)?,
			docs: t.docs.clone(),
			span: t.span,
		});
	}

	fn check_assoc_type(&mut self, t: &ResolvedAssocTypeDecl) -> Result<TypedAssocTypeDecl, TypeError>
	{
		return Ok(TypedAssocTypeDecl {
			resolved_name: t.resolved_name,
			name: t.name.clone(),
			modifiers: t.modifiers.clone(),
			generics: t.generics.clone(),
			ty: t.ty.as_ref().map(|ty| return self.lower_ty(ty)).transpose()?,
			docs: t.docs.clone(),
			span: t.span,
		});
	}

	fn check_trait(&mut self, t: &ResolvedTraitDecl) -> Result<TypedTraitDecl, TypeError>
	{
		let super_traits: Vec<TyBound> = t
			.super_traits
			.iter()
			.map(|b| return self.lower_where_bound(b))
			.collect::<Result<Vec<_>, _>>()?;

		let items: Vec<TypedTraitItem> = t
			.items
			.iter()
			.map(|item| {
				return Ok(match item {
					ResolvedTraitItem::Function(f) => TypedTraitItem::Function(self.check_function(f)?),
					ResolvedTraitItem::TypeAlias(ta) => TypedTraitItem::TypeAlias(self.check_type_alias(ta)?),
					ResolvedTraitItem::AssocType(ta) => TypedTraitItem::AssocType(self.check_assoc_type(ta)?),
					ResolvedTraitItem::Const(c) => TypedTraitItem::Const(self.check_var_decl(c, false)?),
				});
			})
			.collect::<Result<Vec<_>, TypeError>>()?;

		return Ok(TypedTraitDecl {
			resolved_name: t.resolved_name,
			name: t.name.clone(),
			modifiers: t.modifiers.clone(),
			generics: t.generics.clone(),
			super_traits,
			items,
			docs: t.docs.clone(),
			span: t.span,
		});
	}

	fn check_module(&mut self, m: &ResolvedModuleDecl) -> Result<TypedModuleDecl, TypeError>
	{
		let resolved_body: Option<TypedTopLevelBlock> = m
			.resolved_body
			.as_ref()
			.map(|body| return self.check_block_tld(body))
			.transpose()?;
		return Ok(TypedModuleDecl {
			resolved_name: m.resolved_name,
			name: m.name.clone(),
			modifiers: m.modifiers.clone(),
			resolved_body,
			docs: m.docs.clone(),
			span: m.span,
		});
	}

	fn check_impl(&mut self, i: &ResolvedImplDecl) -> Result<TypedImplDecl, TypeError>
	{
		let impl_pairs: Vec<(parser::Ident, Span)> = Self::generic_pairs(&i.generics);
		let where_clause: Vec<TypedWhereConstraint> =
			self.lower_where_constraints_with_generics(&impl_pairs, &i.where_clause)?;
		self.generic_scope.push_params(&impl_pairs, &where_clause);

		let self_ty: Ty = match &i.resolved_target.kind {
			ResolvedPathKind::Resolved(id) => {
				let sym = self.global.symbol(*id);
				#[allow(clippy::redundant_else)] // clippy does weird things
				if matches!(sym.kind, SymbolKind::GenericParam) {
					self.generic_scope.lookup(&sym.name).cloned().unwrap_or_else(|| {
						return Ty::Generic {
							name: sym.name.clone(),
							bounds: Vec::new(),
						};
					})
				} else {
					let generics: Vec<Ty> = i
						.generics
						.iter()
						.map(|g| {
							return self.generic_scope.lookup(&g.name).cloned().unwrap_or_else(|| {
								return Ty::Generic {
									name: g.name.clone(),
									bounds: Vec::new(),
								};
							});
						})
						.collect();
					Ty::Named { symbol: *id, generics }
				}
			}
			ResolvedPathKind::AssocItem { base, .. } => Ty::named(*base),
			ResolvedPathKind::Primitive(ty) => ty.clone(),
		};

		let prev_self: Option<Ty> = self.fn_ctx.self_ty.replace(self_ty);
		let prev_assoc: HashMap<String, Ty> = std::mem::take(&mut self.fn_ctx.assoc_types);

		for item in &i.items {
			if let ResolvedImplItem::AssocType(ta) = item
				&& let Some(rty) = &ta.ty
				&& let Ok(ty) = self.lower_ty(rty)
			{
				self.fn_ctx.assoc_types.insert(ta.name.clone(), ty);
			}
		}

		if let Some(trait_path) = &i.resolved_trait
			&& let ResolvedPathKind::Resolved(trait_sym) = &trait_path.kind
			&& let Some(trait_decl) = self.traits.decls.get(trait_sym).copied()
		{
			struct TraitFnInfo
			{
				param_count: usize,
				has_body: bool,
				fn_sym: SymbolId,
			}

			let trait_fns: HashMap<String, TraitFnInfo> = trait_decl
				.items
				.iter()
				.filter_map(|item| {
					return if let ResolvedTraitItem::Function(f) = item {
						let param_count = f
							.signature
							.params
							.iter()
							.filter(|p| return p.symbol.0 != usize::MAX)
							.count();
						let has_body = f.body.is_some();
						let fn_sym = f.signature.resolved_name;
						Some((
							f.signature.name.clone(),
							TraitFnInfo {
								param_count,
								has_body,
								fn_sym,
							},
						))
					} else {
						None
					};
				})
				.collect();

			let impl_fns: HashMap<String, &ResolvedFunctionDecl> = i
				.items
				.iter()
				.filter_map(|item| {
					return if let ResolvedImplItem::Function(f) = item {
						Some((f.signature.name.clone(), f))
					} else {
						None
					};
				})
				.collect();

			for (name, info) in &trait_fns {
				if !info.has_body && !impl_fns.contains_key(name) {
					return Err(Self::err(
						i.span,
						TypeErrorKind::TypeMismatch {
							expected: format!("implementation of required trait function `{name}`"),
							found: String::from("no implementation provided"),
						},
					));
				}
			}

			for (name, func) in &impl_fns {
				match trait_fns.get(name) {
					None => {
						return Err(Self::err(
							func.signature.span,
							TypeErrorKind::TypeMismatch {
								expected: format!("`{name}` to be declared in the trait"),
								found: format!("`{name}` is not a member of the trait"),
							},
						));
					}
					Some(info) => {
						let impl_param_count = func
							.signature
							.params
							.iter()
							.filter(|p| return p.symbol.0 != usize::MAX)
							.count();
						if impl_param_count != info.param_count {
							return Err(Self::err(
								func.signature.span,
								TypeErrorKind::ArgCountMismatch {
									expected: info.param_count,
									found: impl_param_count,
								},
							));
						}
						let mut impl_ret: Ty = self.lower_ty(&func.signature.return_type).unwrap_or(Ty::Unit);

						impl_ret = if let Some(self_ty) = &self.fn_ctx.self_ty {
							substitute_self(&impl_ret, self_ty)
						} else {
							impl_ret
						};
						let mut trait_ret: Ty = self.caches.env.get(info.fn_sym).cloned().unwrap_or(Ty::Unit);
						trait_ret = self.resolve_assoc_types_in_ty(&trait_ret);
						trait_ret = if let Some(self_ty) = &self.fn_ctx.self_ty {
							substitute_self(&trait_ret, self_ty)
						} else {
							trait_ret
						};
						if !impl_ret.is_assignable_to(&trait_ret) {
							return Err(Self::err(
								func.signature.span,
								TypeErrorKind::ReturnTypeMismatch {
									expected: self.fmt_ty(&trait_ret),
									found: self.fmt_ty(&impl_ret),
								},
							));
						}
					}
				}
			}
		}

		let items: Vec<TypedImplItem> = i
			.items
			.iter()
			.map(|item| {
				return Ok(match item {
					ResolvedImplItem::Function(f) => TypedImplItem::Function(self.check_function(f)?),
					ResolvedImplItem::TypeAlias(ta) => TypedImplItem::TypeAlias(self.check_type_alias(ta)?),
					ResolvedImplItem::AssocType(ta) => TypedImplItem::AssocType(self.check_assoc_type(ta)?),
					ResolvedImplItem::Const(c) => TypedImplItem::Const(self.check_var_decl(c, false)?),
				});
			})
			.collect::<Result<Vec<_>, TypeError>>()?;

		self.fn_ctx.self_ty = prev_self;
		self.fn_ctx.assoc_types = prev_assoc;
		self.generic_scope.pop();

		return Ok(TypedImplDecl {
			resolved_target: i.resolved_target.clone(),
			resolved_trait: i.resolved_trait.clone(),
			modifiers: i.modifiers.clone(),
			generics: i.generics.clone(),
			where_clause,
			items,
			docs: i.docs.clone(),
			span: i.span,
		});
	}

	fn check_block(&mut self, block: &ResolvedBlock) -> Result<TypedBlock, TypeError>
	{
		return self.check_block_inner(block, false);
	}

	fn check_block_as_value(&mut self, block: &ResolvedBlock) -> Result<TypedBlock, TypeError>
	{
		return self.check_block_inner(block, true);
	}

	fn try_check_infer_var(&mut self, v: &ResolvedVariableDecl) -> Result<TypedVariableDecl, InferVarResult>
	{
		return self.try_check_infer_var_hinted(v, None);
	}

	fn collect_infer_deps(&self, expr: &ResolvedExpr) -> Vec<SymbolId>
	{
		let mut deps: Vec<SymbolId> = Vec::new();
		self.collect_infer_deps_inner(expr, &mut deps);
		return deps;
	}

	fn collect_infer_deps_inner(&self, expr: &ResolvedExpr, deps: &mut Vec<SymbolId>)
	{
		match expr {
			ResolvedExpr::Identifier { path, .. } => {
				if let ResolvedPathKind::Resolved(id) = &path.kind
					&& self.infer_syms.contains(id)
				{
					deps.push(*id);
				}
			}
			ResolvedExpr::Call { callee, args, .. } => {
				self.collect_infer_deps_inner(callee, deps);
				for a in args {
					self.collect_infer_deps_inner(a, deps);
				}
			}
			ResolvedExpr::Field { base, .. } => self.collect_infer_deps_inner(base, deps),
			ResolvedExpr::Index { base, index, .. } => {
				self.collect_infer_deps_inner(base, deps);
				self.collect_infer_deps_inner(index, deps);
			}
			ResolvedExpr::Unary { expr: inner, .. } | ResolvedExpr::Cast { expr: inner, .. } => {
				self.collect_infer_deps_inner(inner, deps);
			}
			ResolvedExpr::Binary { lhs, rhs, .. } => {
				self.collect_infer_deps_inner(lhs, deps);
				self.collect_infer_deps_inner(rhs, deps);
			}
			ResolvedExpr::Tuple { elements, .. } => {
				for e in elements {
					self.collect_infer_deps_inner(e, deps);
				}
			}
			ResolvedExpr::Block(b) | ResolvedExpr::UnsafeBlock(b) => {
				for s in &b.stmts {
					self.collect_infer_deps_in_stmt(s, deps);
				}
				if let Some(tail) = &b.tail_expr {
					self.collect_infer_deps_inner(tail, deps);
				}
			}
			ResolvedExpr::If {
				cond,
				then_block,
				else_branch,
				..
			} => {
				self.collect_infer_deps_inner(cond, deps);
				for s in &then_block.stmts {
					self.collect_infer_deps_in_stmt(s, deps);
				}
				if let Some(tail) = &then_block.tail_expr {
					self.collect_infer_deps_inner(tail, deps);
				}
				if let Some(e) = else_branch {
					self.collect_infer_deps_inner(e, deps);
				}
			}
			ResolvedExpr::Switch {
				expr: scrutinee, arms, ..
			} => {
				self.collect_infer_deps_inner(scrutinee, deps);
				for arm in arms {
					match &arm.body {
						ResolvedSwitchBody::Expr(e) => self.collect_infer_deps_inner(e, deps),
						ResolvedSwitchBody::Block(b) => {
							for s in &b.stmts {
								self.collect_infer_deps_in_stmt(s, deps);
							}
							if let Some(tail) = &b.tail_expr {
								self.collect_infer_deps_inner(tail, deps);
							}
						}
					}
				}
			}
			ResolvedExpr::Array(arr) => match arr {
				ResolvedArrayLiteral::List { elements, .. } => {
					for e in elements {
						self.collect_infer_deps_inner(e, deps);
					}
				}
				ResolvedArrayLiteral::Repeat { value, count, .. } => {
					self.collect_infer_deps_inner(value, deps);
					self.collect_infer_deps_inner(count, deps);
				}
			},
			ResolvedExpr::StructInit { fields, base, .. } => {
				for (_, e) in fields {
					self.collect_infer_deps_inner(e, deps);
				}
				if let Some(b) = base {
					self.collect_infer_deps_inner(b, deps);
				}
			}
			ResolvedExpr::Range(re) => {
				if let Some(s) = &re.start {
					self.collect_infer_deps_inner(s, deps);
				}
				if let Some(e) = &re.end {
					self.collect_infer_deps_inner(e, deps);
				}
			}
			_ => {}
		}
	}

	fn collect_infer_deps_in_stmt(&self, stmt: &ResolvedStmt, deps: &mut Vec<SymbolId>)
	{
		match stmt {
			ResolvedStmt::Expr(e) => self.collect_infer_deps_inner(e, deps),
			ResolvedStmt::VariableDecl(v) => {
				if let Some(init) = &v.init {
					self.collect_infer_deps_inner(init, deps);
				}
			}
			ResolvedStmt::Assignment { target, value, .. } => {
				self.collect_infer_deps_inner(target, deps);
				self.collect_infer_deps_inner(value, deps);
			}
			ResolvedStmt::Return { value, .. } | ResolvedStmt::Break { value, .. } => {
				if let Some(e) = value {
					self.collect_infer_deps_inner(e, deps);
				}
			}
			ResolvedStmt::If {
				cond,
				then_block,
				else_branch,
				..
			} => {
				self.collect_infer_deps_inner(cond, deps);
				for s in &then_block.stmts {
					self.collect_infer_deps_in_stmt(s, deps);
				}
				if let Some(eb) = else_branch {
					self.collect_infer_deps_in_stmt(eb, deps);
				}
			}
			ResolvedStmt::Loop { body, .. } => {
				for s in &body.stmts {
					self.collect_infer_deps_in_stmt(s, deps);
				}
			}
			ResolvedStmt::Block(b) | ResolvedStmt::Unsafe(b) => {
				for s in &b.stmts {
					self.collect_infer_deps_in_stmt(s, deps);
				}
			}
			ResolvedStmt::Delete { expr, .. } => self.collect_infer_deps_inner(expr, deps),
			_ => {}
		}
	}

	fn drain_backpatch_worklist(
		&mut self,
		bp: &mut BackpatchState<'_>,
		stmts: &mut [TypedStmt],
	) -> Result<(), TypeError>
	{
		while let Some(resolved_sym) = bp.worklist.pop_front() {
			let dependents = bp.dependents.remove(&resolved_sym).unwrap_or_default();

			let resolved_ty = self.caches.env.get(resolved_sym).cloned();
			if resolved_ty.as_ref().is_none_or(|t| return t == &Ty::Infer) {
				if !dependents.is_empty() {
					bp.dependents.entry(resolved_sym).or_default().extend(dependents);
				}
				continue;
			}

			for dep_sym in dependents {
				if !bp.pending.contains_key(&dep_sym) {
					continue;
				}

				let hint = if dep_sym == resolved_sym {
					self.caches
						.env
						.get(dep_sym)
						.filter(|t| return *t != &Ty::Infer)
						.cloned()
				} else {
					None
				};

				let pending = bp.pending.remove(&dep_sym).expect("");
				match self.try_check_infer_var_hinted(pending.resolved, hint.as_ref()) {
					Ok(typed_decl) => {
						self.caches.env.insert(dep_sym, typed_decl.ty.clone());
						self.infer_syms.remove(&dep_sym);
						stmts[pending.stmt_index] = TypedStmt::VariableDecl(typed_decl);
						bp.mark_resolved(dep_sym);
					}
					Err(InferVarResult::Unresolvable(new_deps)) => {
						for d in &new_deps {
							bp.add_dependency(*d, dep_sym);
							if !self.infer_syms.contains(d) {
								bp.mark_resolved(*d);
							}
						}

						if new_deps.is_empty() {
							bp.add_dependency(dep_sym, dep_sym);
						}
						bp.pending.insert(dep_sym, pending);
					}
					Err(InferVarResult::HardError(e)) => return Err(e),
				}
			}
		}
		return Ok(());
	}

	fn try_check_infer_var_hinted(
		&mut self,
		v: &ResolvedVariableDecl,
		hint: Option<&Ty>,
	) -> Result<TypedVariableDecl, InferVarResult>
	{
		let Some(init_expr) = &v.init else {
			return Err(InferVarResult::HardError(Self::err(
				v.span,
				TypeErrorKind::CannotInferType,
			)));
		};

		let result = match hint {
			Some(h) => self
				.check_expr(init_expr, Some(h))
				.or_else(|_| return self.check_expr(init_expr, None)),
			None => self.check_expr(init_expr, None),
		};

		match result {
			Ok(te) if te.ty != Ty::Infer => {
				self.caches.env.insert(v.resolved_name, te.ty.clone());
				return Ok(TypedVariableDecl {
					resolved_name: v.resolved_name,
					name: v.name.clone(),
					ty: te.ty.clone(),
					init: Some(te),
					comp_const: v.comp_const,
					mutable: v.mutable,
					modifiers: v.modifiers.clone(),
					docs: v.docs.clone(),
					span: v.span,
				});
			}
			Ok(_) => {}
			Err(e) => match e.kind {
				TypeErrorKind::CannotInferType
				| TypeErrorKind::UnresolvedIdentifier { .. }
				| TypeErrorKind::UnresolvedAssocPath { .. } => {}
				_ => return Err(InferVarResult::HardError(e)),
			},
		}

		let deps = self.collect_infer_deps(init_expr);

		return Err(InferVarResult::Unresolvable(deps));
	}

	fn pin_infer_sym(&mut self, sym: SymbolId, concrete_ty: &Ty) -> bool
	{
		if !self.infer_syms.contains(&sym) {
			return false;
		}

		if let Some(existing) = self.caches.env.get(sym)
			&& matches!(existing, Ty::ImplTrait { .. })
		{
			return false;
		}
		if concrete_ty == &Ty::Infer || matches!(concrete_ty, Ty::Generic { .. }) {
			return false;
		}
		self.caches.env.insert(sym, concrete_ty.clone());
		self.infer_syms.remove(&sym);
		self.newly_pinned.push(sym);
		return true;
	}

	fn check_stmt(&mut self, stmt: &ResolvedStmt) -> Result<TypedStmt, TypeError>
	{
		return Ok(match stmt {
			ResolvedStmt::VariableDecl(v) => TypedStmt::VariableDecl(self.check_var_decl(v, true)?),

			ResolvedStmt::Assignment {
				target,
				op,
				value,
				span,
			} => {
				let ttarget = self.check_expr(target, None)?;
				let hint = if ttarget.ty == Ty::Infer {
					None
				} else {
					Some(&ttarget.ty)
				};
				let tvalue = self.check_expr(value, hint)?;
				if ttarget.ty != Ty::Infer {
					self.expect_ty(&tvalue.ty, &ttarget.ty, *span)?;
				}

				if ttarget.ty != Ty::Infer
					&& let ResolvedExpr::Identifier { path, .. } = value.clone()
					&& let ResolvedPathKind::Resolved(val_sym) = &path.kind
				{
					self.pin_infer_sym(*val_sym, &ttarget.ty);
				}
				if tvalue.ty != Ty::Infer
					&& let ResolvedExpr::Identifier { path, .. } = target.clone()
					&& let ResolvedPathKind::Resolved(tgt_sym) = &path.kind
				{
					self.pin_infer_sym(*tgt_sym, &tvalue.ty);
				}

				if let Some(trait_name) = assign_op_trait_method!(op)
					&& let Some(fn_sym) = self.op_trait_fn_sym(&ttarget.ty, trait_name)
				{
					let method_name = self.global.symbol(fn_sym).name.clone();
					let callee = TypedExpr {
						ty: Ty::Unit,
						span: *span,
						kind: TypedExprKind::Field {
							base: Box::new(ttarget),
							name: method_name,
						},
					};
					let call_expr = TypedExpr {
						kind: TypedExprKind::Call {
							callee: Box::new(callee),
							call_type: CallType::Regular,
							named_generics: Vec::new(),
							args: vec![tvalue],
						},
						ty: Ty::Unit,
						span: *span,
					};

					return Ok(TypedStmt::Expr(call_expr));
				}

				TypedStmt::Assignment {
					target: ttarget,
					op: *op,
					value: tvalue,
					span: *span,
				}
			}

			ResolvedStmt::Return { value, span } => {
				let expected: Ty = self.fn_ctx.return_ty.clone().unwrap_or(Ty::Unit);
				let tvalue: Option<TypedExpr> = value
					.as_ref()
					.map(|e| {
						let te = self.check_expr(e, Some(&expected))?;
						self.expect_ty(&te.ty, &expected, *span).map_err(|_| {
							return Self::err(
								*span,
								TypeErrorKind::ReturnTypeMismatch {
									expected: self.fmt_ty(&expected),
									found: self.fmt_ty(&te.ty),
								},
							);
						})?;
						if expected != Ty::Unit
							&& expected != Ty::Infer
							&& let ResolvedExpr::Identifier { path, .. } = e.clone()
							&& let ResolvedPathKind::Resolved(ret_sym) = &path.kind
						{
							self.pin_infer_sym(*ret_sym, &expected);
						}
						return Ok(te);
					})
					.transpose()?;

				if value.is_none() && expected != Ty::Unit && expected != Ty::Never {
					return Err(Self::err(
						*span,
						TypeErrorKind::ReturnTypeMismatch {
							expected: self.fmt_ty(&expected),
							found: self.fmt_ty(&Ty::Unit),
						},
					));
				}
				TypedStmt::Return {
					value: tvalue,
					span: *span,
				}
			}

			ResolvedStmt::Expr(e) => {
				let te = self.check_expr(e, None)?;

				if let TypedExprKind::Identifier { path } = &te.kind {
					match &path.kind {
						ResolvedPathKind::Resolved(id)
							if self.caches.env.get(*id).is_none()
								&& matches!(
									self.global.symbol(*id).kind,
									SymbolKind::Struct
										| SymbolKind::Union | SymbolKind::Enum
										| SymbolKind::Variant | SymbolKind::Trait
										| SymbolKind::Module | SymbolKind::TypeAlias
								) =>
						{
							return Err(Self::err(
								e.span(),
								TypeErrorKind::InvalidPrimitivePosition {
									ty: self.global.symbol(*id).name.clone(),
									position: "a value expression",
								},
							));
						}
						ResolvedPathKind::Primitive(Ty::ImplTrait { .. } | Ty::Generic { .. }) => {
							return Err(Self::err(
								e.span(),
								TypeErrorKind::InvalidPrimitivePosition {
									ty: self.fmt_ty(&te.ty),
									position: "a value expression",
								},
							));
						}
						_ => {}
					}
				}
				TypedStmt::Expr(te)
			}

			ResolvedStmt::Break { label, value, span } => {
				let tvalue = value.as_ref().map(|e| return self.check_expr(e, None)).transpose()?;
				TypedStmt::Break {
					label: label.clone(),
					value: tvalue,
					span: *span,
				}
			}

			ResolvedStmt::Continue { label, span } => TypedStmt::Continue {
				label: label.clone(),
				span: *span,
			},

			ResolvedStmt::If {
				cond,
				then_block,
				else_branch,
				span,
			} => {
				let tcond: TypedExpr = self.check_expr(cond, Some(&Ty::Primitive(Primitive::Bool)))?;
				self.expect_ty(&tcond.ty, &Ty::Primitive(Primitive::Bool), *span)?;
				let tthen: TypedBlock = self.check_block(then_block)?;
				let telse: Option<Box<TypedStmt>> = else_branch
					.as_ref()
					.map(|s| return self.check_stmt(s).map(Box::new))
					.transpose()?;
				TypedStmt::If {
					cond: tcond,
					then_block: tthen,
					else_branch: telse,
					span: *span,
				}
			}

			ResolvedStmt::Loop { label, body, span } => TypedStmt::Loop {
				label: label.clone(),
				body: self.check_block(body)?,
				span: *span,
			},

			ResolvedStmt::Delete { expr, span } => TypedStmt::Delete {
				expr: self.check_expr(expr, None)?,
				span: *span,
			},

			ResolvedStmt::Unsafe(b) => TypedStmt::Unsafe(self.check_block(b)?),
			ResolvedStmt::Block(b) => TypedStmt::Block(self.check_block(b)?),
			ResolvedStmt::Directive(d) => TypedStmt::Directive(self.check_directive(d)?),
		});
	}

	fn check_expr(&mut self, expr: &ResolvedExpr, hint: Option<&Ty>) -> Result<TypedExpr, TypeError>
	{
		let span: Span = expr.span();
		let (kind, ty) = match expr {
			ResolvedExpr::Identifier { path, .. } => {
				let ty = match &path.kind {
					ResolvedPathKind::Resolved(id) => {
						let sym_kind = &self.global.symbol(*id).kind;
						if matches!(
							sym_kind,
							SymbolKind::Struct
								| SymbolKind::Union | SymbolKind::Enum
								| SymbolKind::Variant | SymbolKind::Trait
								| SymbolKind::Module | SymbolKind::TypeAlias
						) {
							return Err(Self::err(
								span,
								TypeErrorKind::InvalidPrimitivePosition {
									ty: self.global.symbol(*id).name.clone(),
									position: "a value expression",
								},
							));
						}

						let sym_name = self.global.symbol(*id).name.clone();
						if let Some(hp_ty) = self.fn_ctx.heap_params.get(&sym_name).cloned() {
							return Ok(TypedExpr {
								kind: TypedExprKind::Identifier { path: path.clone() },
								ty: hp_ty,
								span,
							});
						}

						let ty = self.ty_of_symbol(*id, span)?;

						match (&ty, hint) {
							(
								Ty::Named {
									symbol: bare_sym,
									generics,
								},
								Some(h @ Ty::Named { symbol: hint_sym, .. }),
							) if generics.is_empty()
								&& (bare_sym == hint_sym || {
									let resolved = self.resolve_to_struct_sym(*bare_sym);
									resolved == *hint_sym || bare_sym == hint_sym
								}) =>
							{
								h.clone()
							}
							_ => ty,
						}
					}
					ResolvedPathKind::AssocItem { base, member, .. } => {
						let actual: SymbolId = self.resolve_to_struct_sym(*base);
						let base_name: String = self.global.symbol(*base).name.clone();

						self.caches
							.method
							.get_sym(*base, member)
							.or_else(|| return self.caches.method.get_sym(actual, member))
							.cloned()
							.or_else(|| {
								return self
									.caches
									.method_fn
									.get_sym(*base, member)
									.or_else(|| return self.caches.method_fn.get_sym(actual, member))
									.and_then(|&fn_sym| return self.caches.env.get(fn_sym).cloned());
							})
							.or_else(|| return self.find_method_ty_by_name(&base_name, member))
							.or_else(|| {
								let generic_ty = self.caches.env.get(*base).cloned()?;
								let bounds = match &generic_ty {
									Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => bounds.clone(),
									_ => return None,
								};
								for bound in &bounds {
									let TyBound::Trait { symbol: trait_sym, .. } = bound else {
										continue;
									};
									if let Some(ret) = self.caches.method.get_sym(*trait_sym, member).cloned() {
										return if ret == Ty::SelfTy {
											Some(substitute_self(&ret, &generic_ty))
										} else {
											Some(ret)
										};
									}
								}
								return None;
							})
							.or_else(|| {
								let base_sym = self.global.symbol(*base);
								if !matches!(base_sym.kind, SymbolKind::Enum) {
									return None;
								}
								let scope_id = base_sym.introduced_scope?;
								let scope = self.global.scope(scope_id);
								return scope.symbols.iter().copied().find_map(|sid| {
									let s = self.global.symbol(sid);
									if &s.name == member && matches!(s.kind, SymbolKind::EnumVariant) {
										// Variant's type was registered by scan_enum as Ty::named(enum_sym).
										return self.caches.env.get(sid).cloned();
									}
									return None;
								});
							})
							.ok_or_else(|| {
								return Self::err(
									span,
									TypeErrorKind::UnresolvedAssocPath {
										path: format!("{base_name}::{member}"),
									},
								);
							})?
					}
					ResolvedPathKind::Primitive(ty) => {
						return Err(Self::err(
							span,
							TypeErrorKind::InvalidPrimitivePosition {
								ty: self.fmt_ty(ty),
								position: "identifier??",
							},
						));
					}
				};
				let mut tpath = path.clone();

				let base_concrete = match &ty {
					Ty::Named { .. } => Some(&ty),
					_ => None,
				};
				self.finalize_assoc_in_path(&mut tpath, base_concrete);
				(TypedExprKind::Identifier { path: tpath }, ty)
			}

			ResolvedExpr::UnresolvedIdentifier { path, .. } => {
				let name = path.to_string();

				if let Some(ty) = self.fn_ctx.heap_params.get(&name).cloned() {
					return Ok(TypedExpr {
						kind: TypedExprKind::Identifier {
							path: ResolvedPath {
								kind: ResolvedPathKind::Primitive(ty.clone()),
								original: path.clone(),
							},
						},
						ty,
						span,
					});
				}
				let is_trait_method = self.traits.decls.values().any(|t| {
					return t
						.items
						.iter()
						.any(|item| matches!(item, ResolvedTraitItem::Function(f) if f.signature.name == name));
				});

				if is_trait_method {
					return Err(Self::err(
						span,
						TypeErrorKind::UnresolvedIdentifier {
							path: format!("{name}` is not in scope; use `Self::{name}` to call the trait method"),
						},
					));
				}

				return Err(Self::err(span, TypeErrorKind::UnresolvedIdentifier { path: name }));
			}

			ResolvedExpr::AssocPath { base, member, .. } => {
				let base_name = base
					.original
					.segments
					.iter()
					.map(|s| return s.name.as_str())
					.collect::<Vec<_>>()
					.join("::");

				if let Some(generic_ty) = self.generic_scope.lookup(&base_name).cloned() {
					let bounds = match &generic_ty {
						Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => bounds.clone(),
						_ => Vec::new(),
					};
					for bound in &bounds {
						let TyBound::Trait { symbol: trait_sym, .. } = bound else {
							continue;
						};
						if let Some(ret) = self.caches.method.get_sym(*trait_sym, &member.name).cloned() {
							let ty = if ret == Ty::SelfTy { generic_ty } else { ret };
							return Ok(TypedExpr {
								kind: TypedExprKind::Identifier {
									path: ResolvedPath {
										kind: ResolvedPathKind::AssocItem {
											base: *trait_sym,
											member: member.name.clone(),
											item: SymbolId::DUMMY,
											base_type_args: Vec::new(),
										},
										original: base.original.clone(),
									},
								},
								ty,
								span,
							});
						}
					}
				}

				if let ResolvedPathKind::Resolved(base_sym) = &base.kind
					&& let Some(generic_ty) = self.caches.env.get(*base_sym).cloned()
				{
					let bounds = match &generic_ty {
						Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => bounds.clone(),
						_ => Vec::new(),
					};
					for bound in &bounds {
						let TyBound::Trait { symbol: trait_sym, .. } = bound else {
							continue;
						};
						if let Some(ret) = self.caches.method.get_sym(*trait_sym, &member.name).cloned() {
							let ty = if ret == Ty::SelfTy { generic_ty } else { ret };
							return Ok(TypedExpr {
								kind: TypedExprKind::Identifier {
									path: ResolvedPath {
										kind: ResolvedPathKind::AssocItem {
											base: *trait_sym,
											member: member.name.clone(),
											item: SymbolId::DUMMY,
											base_type_args: Vec::new(),
										},
										original: base.original.clone(),
									},
								},
								ty,
								span,
							});
						}
					}
				}

				return Err(Self::err(
					span,
					TypeErrorKind::UnresolvedAssocPath {
						path: format!("{}::{}", base_name, member.name),
					},
				));
			}

			ResolvedExpr::AssocSelf { member, span } => {
				if let Some(self_ty) = &self.fn_ctx.self_ty.clone() {
					let self_sym = match self_ty {
						Ty::Named { symbol, .. } => Some(*symbol),
						_ => None,
					};
					if let Some(sym) = self_sym {
						let actual = self.resolve_to_struct_sym(sym);
						let ret_ty = self
							.caches
							.method
							.get_sym(sym, &member.name)
							.or_else(|| return self.caches.method.get_sym(actual, &member.name))
							.cloned();
						if let Some(ty) = ret_ty {
							return Ok(TypedExpr {
								kind: TypedExprKind::Identifier {
									path: ResolvedPath {
										kind: ResolvedPathKind::AssocItem {
											base: sym,
											member: member.name.clone(),
											item: SymbolId::DUMMY,
											base_type_args: Vec::new(),
										},
										original: parser::Path {
											segments: vec![PathSegment {
												name: "Self".to_string(),
												generics: Vec::new(),
												span: *span,
											}],
											glob: false,
											global: false,
											span: *span,
										},
									},
								},
								ty,
								span: *span,
							});
						}
					}
				}

				for &trait_sym in self.traits.decls.clone().keys() {
					if let Some(ty) = self.caches.method.get_sym(trait_sym, &member.name).cloned() {
						return Ok(TypedExpr {
							kind: TypedExprKind::Identifier {
								path: ResolvedPath {
									kind: ResolvedPathKind::AssocItem {
										base: trait_sym,
										member: member.name.clone(),
										item: SymbolId::DUMMY,
										base_type_args: Vec::new(),
									},
									original: parser::Path {
										segments: vec![PathSegment {
											name: "Self".to_string(),
											generics: Vec::new(),
											span: *span,
										}],
										glob: false,
										global: false,
										span: *span,
									},
								},
							},
							ty,
							span: *span,
						});
					}
				}

				return Err(Self::err(
					*span,
					TypeErrorKind::UnresolvedSelf {
						member: member.to_string(),
					},
				));
			}

			ResolvedExpr::InternalCall { intrinsic, .. } => {
				let ty = hint
					.cloned()
					.ok_or_else(|| return Self::err(span, TypeErrorKind::CannotInferType))?;
				(
					TypedExprKind::InternalCall {
						intrinsic: intrinsic.clone(),
					},
					ty,
				)
			}

			ResolvedExpr::Literal { value, .. } => {
				let ty: Ty = Self::type_of_literal(value, hint);
				(TypedExprKind::Literal { value: value.clone() }, ty)
			}

			ResolvedExpr::Default { .. } => {
				return Err(Self::err(
					span,
					TypeErrorKind::TypeMismatch {
						expected: "`default()` is only allowed as `..default()` in a struct initializer".into(),
						found: "`default()` used in an invalid position".into(),
					},
				));
			}

			ResolvedExpr::Unary { op, expr: inner, .. } => {
				let inner_hint: Option<Ty> = match op {
					UnaryOp::Addr { .. } => match hint {
						Some(Ty::Reference { inner, .. } | Ty::Pointer { inner, .. }) => Some((**inner).clone()),
						_ => None,
					},
					_ => hint.cloned(),
				};
				let tinner = self.check_expr(inner, inner_hint.as_ref())?;

				match op {
					UnaryOp::Addr { mutable } => {
						let want_pointer =
							matches!(hint, Some(Ty::Pointer { .. } | Ty::ImplTrait { concrete: Some(_), .. }))
								|| matches!(
									hint,
									Some(Ty::ImplTrait { concrete, .. })
										if concrete
											.as_ref()
											.is_some_and(|c| matches!(c.as_ref(), Ty::Pointer { .. }))
								);

						let ty = if matches!(hint, Some(Ty::Pointer { .. })) {
							Ty::Pointer {
								mutable: *mutable,
								inner: Box::new(tinner.ty.clone()),
							}
						} else {
							Ty::Reference {
								mutable: *mutable,
								inner: Box::new(tinner.ty.clone()),
							}
						};
						let _: bool = want_pointer;

						(
							TypedExprKind::Unary {
								op: *op,
								expr: Box::new(tinner),
							},
							ty,
						)
					}

					UnaryOp::Deref => {
						let inner_ty = match &tinner.ty {
							Ty::Reference { inner, .. } | Ty::Mutable { inner } => {
								let callee = TypedExpr {
									kind: TypedExprKind::InternalCall {
										intrinsic: Intrinsic::RefDeref,
									},
									ty: Ty::Infer,
									span,
								};
								return Ok(TypedExpr {
									ty: *inner.clone(),
									kind: TypedExprKind::Call {
										callee: Box::new(callee),
										call_type: CallType::Regular,
										named_generics: Vec::new(),
										args: vec![tinner],
									},
									span,
								});
							}

							Ty::Pointer { inner, .. } => {
								let callee = TypedExpr {
									kind: TypedExprKind::InternalCall {
										intrinsic: Intrinsic::PtrDeref,
									},
									ty: Ty::Infer,
									span,
								};
								return Ok(TypedExpr {
									ty: *inner.clone(),
									kind: TypedExprKind::Call {
										callee: Box::new(callee),
										call_type: CallType::Regular,
										named_generics: Vec::new(),
										args: vec![tinner],
									},
									span,
								});
							}

							Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => {
								let deref_fn = bounds.iter().find_map(|bound| {
									let TyBound::Trait { symbol: trait_sym, .. } = bound else {
										return None;
									};
									return self.caches.method_fn.get_sym(*trait_sym, "deref").copied();
								});

								if let Some(fn_sym) = deref_fn {
									let ref_ty = self.caches.env.get(fn_sym).cloned().unwrap_or(Ty::Infer);

									let inner_ty = match &ref_ty {
										Ty::Reference { inner, .. } | Ty::Mutable { inner } => *inner.clone(),
										_ => Ty::Infer,
									};

									let method_callee = TypedExpr {
										ty: Ty::Unit,
										span,
										kind: TypedExprKind::Identifier {
											path: ResolvedPath {
												kind: ResolvedPathKind::AssocItem {
													base: {
														bounds
															.iter()
															.find_map(|b| {
																let TyBound::Trait { symbol, .. } = b else {
																	return None;
																};
																return self
																	.caches
																	.method_fn
																	.get_sym(*symbol, "deref")
																	.map(|_| return *symbol);
															})
															.expect("")
													},
													member: "deref".to_string(),
													item: SymbolId::DUMMY,
													base_type_args: Vec::new(),
												},
												original: parser::Path {
													segments: vec![PathSegment {
														name: self.fmt_ty(&tinner.ty),
														generics: Vec::new(),
														span,
													}],
													glob: false,
													global: false,
													span,
												},
											},
										},
									};

									let deref_call = TypedExpr {
										kind: TypedExprKind::Call {
											callee: Box::new(method_callee),
											call_type: CallType::Regular,
											named_generics: Vec::new(),
											args: vec![tinner],
										},
										ty: ref_ty,
										span,
									};

									let intrinsic_callee = TypedExpr {
										kind: TypedExprKind::InternalCall {
											intrinsic: Intrinsic::RefDeref,
										},
										ty: Ty::Infer,
										span,
									};

									return Ok(TypedExpr {
										kind: TypedExprKind::Call {
											callee: Box::new(intrinsic_callee),
											call_type: CallType::Regular,
											named_generics: Vec::new(),
											args: vec![deref_call],
										},
										ty: inner_ty,
										span,
									});
								}

								Ty::Infer
							}

							Ty::Infer => Ty::Infer,

							other => {
								return Err(Self::err(
									span,
									TypeErrorKind::InvalidUnaryOp {
										op: *op,
										ty: self.fmt_ty(other),
									},
								));
							}
						};

						let intrinsic = Intrinsic::RefDeref;
						let callee = TypedExpr {
							kind: TypedExprKind::InternalCall { intrinsic },
							ty: Ty::Infer,
							span,
						};
						return Ok(TypedExpr {
							kind: TypedExprKind::Call {
								callee: Box::new(callee),
								call_type: CallType::Regular,
								named_generics: Vec::new(),
								args: vec![tinner],
							},
							ty: inner_ty,
							span,
						});
					}

					UnaryOp::Neg | UnaryOp::Not => {
						let trait_name =
							unary_op_trait_method!(op).expect("Neg and Not always have a trait method name");

						let mut nfn_sym: Option<SymbolId> = self.op_trait_fn_sym(&tinner.ty, trait_name);

						if nfn_sym.is_none() {
							let bounds: &[TyBound] = match &tinner.ty {
								Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => bounds,
								_ => &[],
							};
							if !bounds.is_empty()
								&& let Some(op_trait_sym) = self.traits.op_symbols.get(trait_name).copied()
							{
								for bound in bounds {
									let TyBound::Trait { symbol, .. } = bound else { continue };
									if *symbol == op_trait_sym {
										let method_name = self
											.traits
											.decls
											.get(symbol)
											.and_then(|td| {
												return td.items.iter().find_map(|it| {
													return if let ResolvedTraitItem::Function(f) = it {
														Some(f.signature.name.clone())
													} else {
														None
													};
												});
											})
											.unwrap_or_else(|| return trait_name.to_lowercase());
										if let Some(&s) = self.caches.method_fn.get_sym(*symbol, &method_name) {
											nfn_sym = Some(s);
											break;
										}
									}
									for blanket in &self.traits.blanket_impls {
										if blanket.required_builtin == *symbol
											&& blanket.granted_trait == op_trait_sym
											&& let Some(m) = blanket.methods.iter().find(|m| {
												return m.name.eq_ignore_ascii_case(trait_name)
													|| m.name == trait_name.to_lowercase();
											}) {
											nfn_sym = Some(m.fn_sym);
											break;
										}
									}
									if nfn_sym.is_some() {
										break;
									}
								}
							}
						}

						if let Some(fn_sym) = nfn_sym {
							let method_name = self.global.symbol(fn_sym).name.clone();
							let ret_ty = match &tinner.ty {
								Ty::Generic { .. } | Ty::ImplTrait { .. } => tinner.ty.clone(),
								_ => self.check_unary(*op, &tinner.ty, span)?,
							};
							let callee = TypedExpr {
								ty: Ty::Unit,
								span,
								kind: TypedExprKind::Field {
									base: Box::new(tinner),
									name: method_name,
								},
							};
							return Ok(TypedExpr {
								kind: TypedExprKind::Call {
									callee: Box::new(callee),
									call_type: CallType::Regular,
									named_generics: Vec::new(),
									args: vec![],
								},
								ty: ret_ty,
								span,
							});
						}

						if matches!(&tinner.ty, Ty::Infer) {
							let ty = tinner.ty.clone();
							return Ok(TypedExpr {
								kind: TypedExprKind::Unary {
									op: *op,
									expr: Box::new(tinner),
								},
								ty,
								span,
							});
						}

						return Err(Self::err(
							span,
							TypeErrorKind::InvalidUnaryOp {
								op: *op,
								ty: self.fmt_ty(&tinner.ty),
							},
						));
					}
				}
			}

			ResolvedExpr::Binary { op, lhs, rhs, .. } => {
				let tlhs: TypedExpr = self.check_expr(lhs, hint)?;
				let trhs_hint = if tlhs.ty == Ty::Infer { hint } else { Some(&tlhs.ty) };
				let trhs: TypedExpr = self.check_expr(rhs, trhs_hint)?;

				let texpr = self.check_binary(*op, tlhs, trhs, span)?;
				(texpr.kind, texpr.ty)
			}

			ResolvedExpr::Cast {
				ty: target_rt,
				expr: inner,
				..
			} => {
				let target: Ty = self.lower_ty(target_rt)?;
				let tinner: TypedExpr = self.check_expr(inner, None)?;
				self.check_cast(&tinner.ty, &target, span)?;
				(
					TypedExprKind::Cast {
						ty: target.clone(),
						expr: Box::new(tinner),
					},
					target,
				)
			}

			ResolvedExpr::Call {
				callee,
				call_type,
				named_generics,
				args,
				..
			} => {
				let tcallee: TypedExpr = if let ResolvedExpr::InternalCall { intrinsic, .. } = callee.as_ref() {
					TypedExpr {
						kind: TypedExprKind::InternalCall {
							intrinsic: intrinsic.clone(),
						},
						ty: Ty::Infer,
						span: callee.span(),
					}
				} else {
					self.check_expr(callee, None)?
				};

				if let TypedExprKind::InternalCall { intrinsic } = &tcallee.kind {
					let mut unified_int: Option<Ty> = None;
					let mut targs: Vec<TypedExpr> = Vec::with_capacity(args.len());

					for (i, arg) in args.iter().enumerate() {
						let h = intrinsic.param_hint(i, unified_int.as_ref());
						let te = self.check_expr(arg, h.as_ref())?;

						if unified_int.is_none() {
							let resolved_te_ty = self.resolve_named_to_primitive(&te.ty);
							let candidate = if resolved_te_ty.is_integer() || resolved_te_ty.is_float() {
								Some(resolved_te_ty.clone())
							} else {
								self.fn_ctx
									.self_ty
									.as_ref()
									.map(|t| return self.resolve_named_to_primitive(t))
									.filter(|t| return t.is_integer() || t.is_float())
									.clone()
							};
							if let Some(c) = candidate {
								unified_int = Some(c);
							}
						}
						targs.push(te);
					}

					let arg_tys: Vec<Ty> = targs
						.iter()
						.map(|e| return self.resolve_named_to_primitive(&e.ty))
						.collect();

					let ret_ty: Ty = intrinsic.check(&arg_tys, span)?;

					let intrinsic_ty: Ty = if ret_ty == Ty::Infer {
						hint.cloned().unwrap_or(Ty::Infer)
					} else {
						ret_ty
					};
					if intrinsic_ty == Ty::Infer {
						return Err(Self::err(span, TypeErrorKind::CannotInferType));
					}
					return Ok(TypedExpr {
						kind: TypedExprKind::Call {
							callee: Box::new(tcallee),
							call_type: *call_type,
							named_generics: Vec::new(),
							args: targs,
						},
						ty: intrinsic_ty,
						span,
					});
				}

				if !self.callee_is_callable(&tcallee) {
					return Err(Self::err(
						span,
						TypeErrorKind::NotCallable {
							ty: self.fmt_ty(&tcallee.ty),
						},
					));
				}

				let callee_is_known_fn = match &tcallee.kind {
					TypedExprKind::Identifier { path } => match &path.kind {
						ResolvedPathKind::Resolved(id) => {
							matches!(self.global.symbol(*id).kind, SymbolKind::Function { .. })
						}
						ResolvedPathKind::AssocItem { .. } => true,
						ResolvedPathKind::Primitive(_) => false,
					},
					TypedExprKind::Field { .. } => true,
					_ => false,
				};

				let ret_ty: Ty = match &tcallee.ty {
					Ty::ImplTrait { bounds, .. } | Ty::Generic { bounds, .. } if !callee_is_known_fn => bounds
						.iter()
						.find_map(|b| {
							return if let TyBound::Fn { ret, .. } = b {
								Some(*ret.clone())
							} else {
								None
							};
						})
						.unwrap_or_else(|| return hint.cloned().unwrap_or(Ty::Infer)),

					Ty::Named { .. } if !callee_is_known_fn => tcallee.ty.clone(),

					_ => {
						let from_env = if let TypedExprKind::Identifier { path } = &tcallee.kind {
							match &path.kind {
								ResolvedPathKind::Resolved(id)
									if matches!(self.global.symbol(*id).kind, SymbolKind::Function { .. }) =>
								{
									self.caches.env.get(*id).cloned()
								}
								ResolvedPathKind::AssocItem { base, member, .. } => {
									let actual = self.resolve_to_struct_sym(*base);
									self.caches
										.method_fn
										.get_sym(*base, member)
										.or_else(|| return self.caches.method_fn.get_sym(actual, member))
										.and_then(|&fn_sym| return self.caches.env.get(fn_sym).cloned())
										.or_else(|| {
											let base_name = &self.global.symbol(*base).name;
											let generic_ty = self.generic_scope.lookup(base_name)?;
											let bounds = match generic_ty {
												Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => {
													bounds.clone()
												}
												_ => return None,
											};
											for bound in &bounds {
												let TyBound::Trait { symbol: trait_sym, .. } = bound else {
													continue;
												};
												if let Some(&fn_sym) = self.caches.method_fn.get_sym(*trait_sym, member)
												{
													return self
														.caches
														.env
														.get(fn_sym)
														.cloned()
														.map(|ret| return substitute_self(&ret, generic_ty));
												}
											}
											return None;
										})
								}

								_ => None,
							}
						} else if let TypedExprKind::Field { base, name } = &tcallee.kind {
							match &base.ty {
								Ty::Named { symbol, .. } => {
									let actual = self.resolve_to_struct_sym(*symbol);
									self.caches
										.method_fn
										.get_sym(*symbol, name)
										.or_else(|| return self.caches.method_fn.get_sym(actual, name))
										.and_then(|&fn_sym| return self.caches.env.get(fn_sym).cloned())
								}
								Ty::Primitive(p) => {
									let key = TyKey::Prim(p.clone());
									self.caches
										.method_fn
										.get(&key, name)
										.and_then(|&fn_sym| return self.caches.env.get(fn_sym).cloned())
								}
								Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => {
									bounds.iter().find_map(|bound| {
										return if let TyBound::Trait { symbol: trait_sym, .. } = bound {
											self.caches
												.method_fn
												.get_sym(*trait_sym, name)
												.and_then(|&fn_sym| return self.caches.env.get(fn_sym).cloned())
										} else {
											None
										};
									})
								}
								_ => None,
							}
						} else {
							None
						};
						from_env.or_else(|| return hint.cloned()).unwrap_or(Ty::Infer)
					}
				};

				let mut rng: Vec<(String, TypedExpr)> = named_generics
					.iter()
					.map(|(n, rt)| {
						if let ResolvedExpr::Identifier { path, .. } = rt
							&& let ResolvedPathKind::Primitive(forwarded_ty) = &path.kind
						{
							return Ok((
								n.clone(),
								TypedExpr {
									kind: TypedExprKind::Identifier { path: path.clone() },
									ty: forwarded_ty.clone(),
									span: rt.span(),
								},
							));
						}
						if let ResolvedExpr::Identifier { path, .. } = rt
							&& let ResolvedPathKind::Resolved(id) = &path.kind
							&& let Some(forwarded_ty) =
								self.fn_ctx.heap_params.get(&self.global.symbol(*id).name.clone())
						{
							let ty = forwarded_ty.clone();
							return Ok((
								n.clone(),
								TypedExpr {
									kind: TypedExprKind::Identifier { path: path.clone() },
									ty,
									span: rt.span(),
								},
							));
						}
						return self.check_expr(rt, None).map(|te| return (n.clone(), te));
					})
					.collect::<Result<Vec<_>, _>>()?;

				let callee_fn_sym: Option<SymbolId> = match &tcallee.kind {
					TypedExprKind::Identifier { path } => match &path.kind {
						ResolvedPathKind::Resolved(id) => {
							if matches!(self.global.symbol(*id).kind, SymbolKind::Function { .. }) {
								Some(*id)
							} else {
								let fn_name = self.global.symbol(*id).name.clone();
								ParamTypeCache::find_fn_sym_by_name(self.global, &fn_name)
							}
						}
						ResolvedPathKind::AssocItem { base, member, .. } => {
							let actual: SymbolId = self.resolve_to_struct_sym(*base);
							self.caches
								.method_fn
								.get_sym(*base, member)
								.or_else(|| return self.caches.method_fn.get_sym(actual, member))
								.copied()
								.or_else(|| {
									let generic_ty = self.caches.env.get(*base).cloned()?;
									let bounds = match &generic_ty {
										Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => bounds.clone(),
										_ => return None,
									};
									for bound in &bounds {
										let TyBound::Trait { symbol: trait_sym, .. } = bound else {
											continue;
										};
										if let Some(&fn_sym) = self.caches.method_fn.get_sym(*trait_sym, member) {
											return Some(fn_sym);
										}
									}
									return None;
								})
						}
						ResolvedPathKind::Primitive(_) => None,
					},
					TypedExprKind::Field { base, name } => match &base.ty {
						Ty::Named { symbol, .. } => {
							let actual = self.resolve_to_struct_sym(*symbol);
							self.caches
								.method_fn
								.get_sym(*symbol, name)
								.or_else(|| return self.caches.method_fn.get_sym(actual, name))
								.copied()
						}
						Ty::Primitive(p) => {
							let key = TyKey::Prim(p.clone());
							self.caches.method_fn.get(&key, name).copied()
						}
						Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => bounds.iter().find_map(|bound| {
							return if let TyBound::Trait { symbol: trait_sym, .. } = bound {
								self.caches.method_fn.get_sym(*trait_sym, name).copied()
							} else {
								None
							};
						}),
						_ => None,
					},
					_ => None,
				};

				if let Some(fn_sym) = callee_fn_sym {
					let callee_call_type = self
						.constraints
						.fn_call_type_cache
						.get(&fn_sym)
						.copied()
						.unwrap_or(CallType::Regular);

					let call_violation = matches!(
						(self.fn_ctx.call_type, callee_call_type),
						(CallType::Regular | CallType::UserMaybeHeap, CallType::UserHeap)
					);

					if call_violation {
						return Err(Self::err(
							span,
							TypeErrorKind::TypeMismatch {
								expected: format!(
									"callee `{}` ({:?}) is not callable from a {:?} context",
									self.global.symbol(fn_sym).name,
									callee_call_type,
									self.fn_ctx.call_type,
								),
								found: format!(
									"call type mismatch: {:?} cannot call {:?}",
									self.fn_ctx.call_type, callee_call_type
								),
							},
						));
					}
				}

				if *call_type != CallType::Regular {
					for hp_name in ["alloc", "io"] {
						if rng.iter().any(|(n, _)| return n == hp_name) {
							continue;
						}
						let Some(ty) = self.fn_ctx.heap_params.get(hp_name).cloned() else {
							return Err(Self::err(
								span,
								TypeErrorKind::TypeMismatch {
									expected: format!(
										"caller to declare a `{hp_name}` heap parameter (required by a \
                         non-Regular call); add it to the function signature or pass it \
                         explicitly with `<{hp_name}=…>`"
									),
									found: "no matching heap parameter in scope".to_string(),
								},
							));
						};
						let sym = self.heap_param_symbol(hp_name).unwrap_or(SymbolId::DUMMY);
						rng.push((
							hp_name.to_string(),
							TypedExpr {
								kind: TypedExprKind::Identifier {
									path: ResolvedPath {
										kind: ResolvedPathKind::Resolved(sym),
										original: parser::Path {
											segments: vec![PathSegment {
												name: hp_name.to_string(),
												generics: Vec::new(),
												span,
											}],
											glob: false,
											global: false,
											span,
										},
									},
								},
								ty,
								span,
							},
						));
					}
					rng.sort_by(|a, b| return a.0.cmp(&b.0));
				}
				if *call_type != CallType::Regular {
					for (hp_name, required_trait_sym) in
						[("alloc", self.traits.heap_syms.alloc), ("io", self.traits.heap_syms.io)]
					{
						let Some(trait_sym) = required_trait_sym else { continue };

						let token_ty: Option<&Ty> = rng
							.iter()
							.find(|(n, _)| return n == hp_name)
							.map(|(_, te)| return &te.ty);

						match token_ty {
							None => { /* existing "no matching heap param" error */ }
							Some(ty) => {
								let already_satisfies = match ty {
									Ty::ImplTrait { bounds, .. } | Ty::Generic { bounds, .. } => bounds
										.iter()
										.any(|b| matches!(b, TyBound::Trait { symbol, .. } if *symbol == trait_sym)),
									_ => {
										self.apply_blanket_impls_for_ty(ty);
										self.ty_satisfies_bound(
											ty,
											&TyBound::Trait {
												symbol: trait_sym,
												args: Vec::new(),
											},
										)
									}
								};

								if !already_satisfies {
									return Err(Self::err(
										span,
										TypeErrorKind::TypeMismatch {
											expected: format!(
												"`{}` to implement `{}`",
												self.fmt_ty(ty),
												self.global.symbol(trait_sym).name
											),
											found: format!(
												"`{}` does not implement `{}`",
												self.fmt_ty(ty),
												self.global.symbol(trait_sym).name
											),
										},
									));
								}
							}
						}
					}
				}

				let param_offset: usize = match &tcallee.kind {
					TypedExprKind::Field { .. } => 1,
					_ => 0,
				};

				if let Some(fn_sym) = callee_fn_sym {
					#[allow(clippy::maybe_infinite_iter)]
					let expected = (param_offset..)
						.take_while(|&i| return self.caches.param.get(fn_sym, i).is_some())
						.count();
					if self.constraints.variadic_fns.contains(&fn_sym) {
						if args.len() < expected {
							return Err(Self::err(
								span,
								TypeErrorKind::ArgCountMismatch {
									expected,
									found: args.len(),
								},
							));
						}
					} else if args.len() != expected {
						return Err(Self::err(
							span,
							TypeErrorKind::ArgCountMismatch {
								expected,
								found: args.len(),
							},
						));
					}
				}

				let param_types: Vec<Option<Ty>> = (0..args.len())
					.map(|i| {
						return callee_fn_sym
							.and_then(|fn_id| return self.caches.param.get(fn_id, i + param_offset).cloned());
					})
					.collect();

				let mut generic_subs: HashMap<String, Ty> = HashMap::new();

				if let Some(h) = hint {
					extract_hint_generics(&ret_ty, h, &mut generic_subs);
				}

				let mut targs: Vec<TypedExpr> = Vec::with_capacity(args.len());

				for (i, a) in args.iter().enumerate() {
					let param_ty = &param_types[i];

					let effective_hint: Option<Ty> = match param_ty {
						Some(ty) if !matches!(ty, Ty::Generic { .. } | Ty::Infer) => Some(ty.clone()),

						Some(Ty::Generic { name, .. }) => generic_subs.get(name.as_str()).cloned().or_else(|| {
							return match hint {
								Some(h @ Ty::ImplTrait { .. }) => {
									let bindings = h.impl_trait_binding_hints();
									if bindings.is_empty() {
										hint.cloned()
									} else {
										Some(bindings[0].clone())
									}
								}
								_ => hint.cloned(),
							};
						}),

						_ => match hint {
							Some(h @ Ty::ImplTrait { .. }) => {
								let bindings = h.impl_trait_binding_hints();
								if bindings.is_empty() {
									hint.cloned()
								} else {
									Some(bindings[0].clone())
								}
							}
							_ => hint.cloned(),
						},
					};

					let te = self.check_expr(a, effective_hint.as_ref())?;

					if let Some(Ty::Generic { name, .. }) = param_ty
						&& !matches!(te.ty, Ty::Infer | Ty::Generic { .. })
					{
						generic_subs.entry(name.clone()).or_insert_with(|| return te.ty.clone());
					}

					if let Some(pt) = param_ty {
						let resolved_pt = substitute_generics(pt, &generic_subs);
						if !matches!(resolved_pt, Ty::Generic { .. } | Ty::Infer) {
							self.expect_ty(&te.ty, &resolved_pt, a.span())?;
							if let ResolvedExpr::Identifier { path, .. } = a
								&& let ResolvedPathKind::Resolved(arg_sym) = &path.kind
							{
								self.pin_infer_sym(*arg_sym, &resolved_pt);
							}
						}
					}

					targs.push(te);
				}

				if let Some(fn_sym) = callee_fn_sym
					&& let Some(bounds) = self.constraints.fn_where_bounds.get(&fn_sym).cloned()
				{
					for (param_name, bound) in &bounds {
						if let Some(concrete_ty) = generic_subs.get(param_name).cloned() {
							self.apply_blanket_impls_for_ty(&concrete_ty);
							if !self.ty_satisfies_bound(&concrete_ty, bound) {
								let bound_str = match bound {
									TyBound::Trait { symbol, .. } => self.global.symbol(*symbol).name.clone(),
									TyBound::Fn { .. } => "Fn".to_string(),
								};
								return Err(Self::err(
									span,
									TypeErrorKind::TypeMismatch {
										expected: format!(
											"`{}` to implement `{}`",
											self.fmt_ty(&concrete_ty),
											bound_str
										),
										found: format!(
											"`{}` does not implement `{}`",
											self.fmt_ty(&concrete_ty),
											bound_str
										),
									},
								));
							}
						}
					}
				}

				let nret_ty = {
					let after_generic_subs = substitute_generics(&ret_ty, &generic_subs);

					if let TypedExprKind::Identifier { path } = &tcallee.kind {
						if let ResolvedPathKind::AssocItem { base, .. } = &path.kind {
							let base_is_trait = matches!(self.global.symbol(*base).kind, SymbolKind::Trait);
							let receiver_is_generic_like =
								matches!(&tcallee.ty, Ty::Generic { .. } | Ty::ImplTrait { .. });

							let self_concrete = if base_is_trait && receiver_is_generic_like {
								tcallee.ty.clone()
							} else {
								self.caches
									.env
									.get(*base)
									.cloned()
									.unwrap_or_else(|| return Ty::named(*base))
							};
							substitute_self(&after_generic_subs, &self_concrete)
						} else {
							after_generic_subs
						}
					} else if let TypedExprKind::Field { base, .. } = &tcallee.kind {
						self.substitute_assoc_bindings(&after_generic_subs, &base.ty)
					} else {
						after_generic_subs
					}
				};

				if let Some(g) = first_unresolved_generic(&nret_ty)
					&& self.generic_scope.lookup(g).is_none()
				{
					return Err(Self::err(span, TypeErrorKind::CannotInferType));
				}

				let tcallee_span = tcallee.span;
				let (final_callee_kind, final_args) = match tcallee.kind {
					TypedExprKind::Field {
						base: receiver,
						name: method_name,
					} => {
						let assoc_base: Option<SymbolId> = match &receiver.ty {
							Ty::Named { symbol, .. } => {
								let actual = self.resolve_to_struct_sym(*symbol);
								self.caches
									.method_fn
									.get_sym(*symbol, &method_name)
									.or_else(|| return self.caches.method_fn.get_sym(actual, &method_name))
									.map(|_| return *symbol)
							}
							Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => {
								bounds.iter().find_map(|bound| {
									return if let TyBound::Trait { symbol: trait_sym, .. } = bound {
										self.caches
											.method_fn
											.get_sym(*trait_sym, &method_name)
											.map(|_| return *trait_sym)
									} else {
										None
									};
								})
							}
							_ => None,
						};

						match assoc_base {
							Some(type_sym) => {
								let receiver_span = receiver.span;
								let new_callee = TypedExprKind::Identifier {
									path: ResolvedPath {
										kind: ResolvedPathKind::AssocItem {
											base: type_sym,
											member: method_name,
											item: SymbolId::DUMMY,
											base_type_args: Vec::new(),
										},
										original: parser::Path {
											segments: vec![PathSegment {
												name: self.global.symbol(type_sym).name.clone(),
												generics: Vec::new(),
												span: receiver_span,
											}],
											glob: false,
											global: false,
											span: receiver_span,
										},
									},
								};
								let mut new_args = Vec::with_capacity(targs.len() + 1);
								new_args.push(*receiver);
								new_args.extend(targs);
								(new_callee, new_args)
							}
							None => (
								TypedExprKind::Field {
									base: receiver,
									name: method_name,
								},
								targs,
							),
						}
					}
					other_kind => (other_kind, targs),
				};

				let nfinal_callee_kind = match final_callee_kind {
					TypedExprKind::Identifier { mut path } => {
						if let ResolvedPathKind::AssocItem {
							item,
							base_type_args,
							base,
							member,
						} = &mut path.kind
						{
							if let Some(fn_sym) = callee_fn_sym {
								*item = fn_sym;
							} else {
								let (i, _) = self.resolve_assoc_path(*base, member, None);
								*item = i;
							}

							let base_ty = final_args.first().map(|a| return &a.ty);
							if let Some(Ty::Named { generics, .. }) = base_ty
								&& !generics.is_empty()
							{
								base_type_args.clone_from(generics);
							}
							if base_type_args.is_empty()
								&& let Some(Ty::Named {
									symbol: hint_sym,
									generics: hint_gens,
								}) = hint && *hint_sym == *base
								&& !hint_gens.is_empty()
							{
								base_type_args.clone_from(hint_gens);
							}
						}
						TypedExprKind::Identifier { path }
					}
					other => other,
				};

				(
					TypedExprKind::Call {
						callee: Box::new(TypedExpr {
							kind: nfinal_callee_kind,
							ty: Ty::Unit,
							span: tcallee_span,
						}),
						call_type: *call_type,
						named_generics: rng,
						args: final_args,
					},
					nret_ty,
				)
			}

			ResolvedExpr::Field { base, name, .. } => {
				let tbase: TypedExpr = self.check_expr(base, None)?;
				let field_ty: Ty = self.check_field_access(&tbase.ty, name, span)?;
				(
					TypedExprKind::Field {
						base: Box::new(tbase),
						name: name.clone(),
					},
					field_ty,
				)
			}

			ResolvedExpr::Index { base, index, .. } => {
				let tbase: TypedExpr = self.check_expr(base, None)?;
				let tindex: TypedExpr = self.check_expr(index, None)?;
				let elem: Ty = self.index_elem_ty(&tbase.ty, span)?;
				(
					TypedExprKind::Index {
						base: Box::new(tbase),
						index: Box::new(tindex),
					},
					elem,
				)
			}

			ResolvedExpr::Range(re) => {
				let tr: TypedRangeExpr = self.check_range_expr(re, hint)?;
				let ty: Ty = tr.ty.clone();
				(TypedExprKind::Range(tr), ty)
			}

			ResolvedExpr::Tuple { elements, .. } => {
				let hint_tys: Option<&Vec<Ty>> = hint.and_then(|h| {
					if let Ty::Tuple(ts) = h {
						return Some(ts);
					}
					return None;
				});
				let telems: Vec<TypedExpr> = elements
					.iter()
					.enumerate()
					.map(|(i, e)| return self.check_expr(e, hint_tys.and_then(|ts| return ts.get(i))))
					.collect::<Result<_, _>>()?;
				let tys: Vec<Ty> = telems.iter().map(|e| return e.ty.clone()).collect();
				(TypedExprKind::Tuple { elements: telems }, Ty::Tuple(tys))
			}

			ResolvedExpr::Array(arr) => {
				let (kind, ty) = self.check_array_literal(arr, hint)?;
				(TypedExprKind::Array(kind), ty)
			}

			ResolvedExpr::StructInit {
				path,
				fields,
				base,
				has_rest: _,
				..
			} => {
				let raw_sym: SymbolId = match &path.kind {
					ResolvedPathKind::Resolved(id) => *id,
					ResolvedPathKind::AssocItem { base, .. } => *base,
					ResolvedPathKind::Primitive(ty) => {
						return Err(Self::err(
							span,
							TypeErrorKind::InvalidPrimitivePosition {
								ty: self.fmt_ty(ty),
								position: "a struct initializer",
							},
						));
					}
				};
				let struct_sym: SymbolId = self.resolve_to_struct_sym(raw_sym);

				let struct_ty: Ty = match hint {
					Some(
						h @ Ty::Named {
							symbol: h_sym,
							generics: h_gens,
						},
					) if (*h_sym == raw_sym || *h_sym == struct_sym) && !h_gens.is_empty() => h.clone(),
					_ => Ty::named(raw_sym),
				};

				let mut tfields: Vec<(String, TypedExpr)> = fields
					.iter()
					.map(|(name, expr)| match self.caches.field.get(struct_sym, name).cloned() {
						Some(exp) => {
							let te = self.check_expr(expr, Some(&exp))?;
							self.expect_ty(&te.ty, &exp, expr.span()).map_err(|_| {
								return Self::err(
									expr.span(),
									TypeErrorKind::StructFieldTypeMismatch {
										field: name.clone(),
										expected: self.fmt_ty(&exp),
										found: self.fmt_ty(&te.ty),
									},
								);
							})?;
							if let ResolvedExpr::Identifier { path, .. } = expr
								&& let ResolvedPathKind::Resolved(field_sym) = &path.kind
							{
								self.pin_infer_sym(*field_sym, &exp);
							}
							return Ok((name.clone(), te));
						}
						None => {
							return Err(Self::err(
								expr.span(),
								TypeErrorKind::StructUnknownField {
									struct_ty: self.fmt_ty(&struct_ty),
									field: name.clone(),
								},
							));
						}
					})
					.collect::<Result<Vec<_>, TypeError>>()?;

				let tbase_expr = base
					.as_ref()
					.map(|e| {
						return match **e {
							ResolvedExpr::Default { heap_call, .. } => Ok(TypedExpr {
								kind: TypedExprKind::Default { heap_call },
								ty: struct_ty.clone(),
								span: e.span(),
							}),
							_ => self.check_expr(e, Some(&struct_ty)),
						};
					})
					.transpose()?;

				match tbase_expr {
					Some(TypedExpr {
						kind: TypedExprKind::Default { .. },
						..
					}) => {
						let provided: HashSet<&str> = tfields.iter().map(|(n, _)| return n.as_str()).collect();

						let missing_without_default: Vec<String> = self
							.caches
							.field
							.fields_of(struct_sym)
							.into_iter()
							.filter(|(name, _)| return !provided.contains(name.as_str()))
							.filter(|(name, _)| {
								return !self.caches.field_default.contains_key(&(struct_sym, name.clone()));
							})
							.map(|(name, _)| return name)
							.collect();

						if !missing_without_default.is_empty() {
							return Err(Self::err(
								span,
								TypeErrorKind::TypeMismatch {
									expected: format!(
										"default values for fields: {}",
										missing_without_default.join(", ")
									),
									found: "no default value declared on these fields".to_owned(),
								},
							));
						}

						let defaults_to_add: Vec<(String, TypedExpr)> = self
							.caches
							.field
							.fields_of(struct_sym)
							.into_iter()
							.filter(|(name, _)| return !provided.contains(name.as_str()))
							.filter_map(|(name, _)| {
								return self
									.caches
									.field_default
									.get(&(struct_sym, name.clone()))
									.cloned()
									.map(|te| return (name, te));
							})
							.collect();

						tfields.extend(defaults_to_add);
					}

					Some(base_expr) => {
						let provided: HashSet<&str> = tfields.iter().map(|(n, _)| return n.as_str()).collect();

						let missing: Vec<(String, Ty)> = self
							.caches
							.field
							.fields_of(struct_sym)
							.into_iter()
							.filter(|(name, _)| return !provided.contains(name.as_str()))
							.collect();

						for (name, ty) in missing {
							tfields.push((
								name.clone(),
								TypedExpr {
									kind: TypedExprKind::Field {
										base: Box::new(base_expr.clone()),
										name: name.clone(),
									},
									ty,
									span,
								},
							));
						}
					}

					None => {}
				}

				(
					TypedExprKind::StructInit {
						path: path.clone(),
						fields: tfields,
						base: None,
						has_rest: false,
					},
					struct_ty,
				)
			}

			ResolvedExpr::Block(b) => {
				let tb: TypedBlock = self.check_block_as_value(b)?;
				let ty: Ty = tb.ty.clone();
				(TypedExprKind::Block(Box::new(tb)), ty)
			}

			ResolvedExpr::UnsafeBlock(b) => {
				let tb: TypedBlock = self.check_block_as_value(b)?;
				let ty: Ty = tb.ty.clone();
				(TypedExprKind::UnsafeBlock(Box::new(tb)), ty)
			}

			ResolvedExpr::Switch {
				expr: scrutinee, arms, ..
			} => {
				let tscrutinee: TypedExpr = self.check_expr(scrutinee, None)?;
				let mut arm_ty: Option<Ty> = hint.cloned();

				let tarms: Vec<TypedSwitchArm> = arms
					.iter()
					.map(|arm| {
						let tpat = self.check_pattern(&arm.pattern, &tscrutinee.ty)?;
						let tbody = match &arm.body {
							ResolvedSwitchBody::Expr(e) => {
								let te = self.check_expr(e, arm_ty.as_ref())?;
								if let Some(ref exp) = arm_ty {
									self.expect_ty(&te.ty, exp, arm.span).map_err(|_| {
										return Self::err(
											arm.span,
											TypeErrorKind::SwitchArmTypeMismatch {
												first: self.fmt_ty(exp),
												found: self.fmt_ty(&te.ty),
											},
										);
									})?;
								}
								arm_ty.get_or_insert_with(|| return te.ty.clone());
								TypedSwitchBody::Expr(te)
							}
							ResolvedSwitchBody::Block(b) => {
								let tb: TypedBlock = self.check_block_as_value(b)?;
								if let Some(ref exp) = arm_ty {
									self.expect_ty(&tb.ty, exp, arm.span).map_err(|_| {
										return Self::err(
											arm.span,
											TypeErrorKind::SwitchArmTypeMismatch {
												first: self.fmt_ty(exp),
												found: self.fmt_ty(&tb.ty),
											},
										);
									})?;
								}
								arm_ty.get_or_insert_with(|| return tb.ty.clone());
								TypedSwitchBody::Block(tb)
							}
						};
						return Ok(TypedSwitchArm {
							pattern: tpat,
							body: tbody,
							span: arm.span,
						});
					})
					.collect::<Result<Vec<_>, TypeError>>()?;

				let ty: Ty = arm_ty.unwrap_or(Ty::Unit);
				(
					TypedExprKind::Switch {
						expr: Box::new(tscrutinee),
						arms: tarms,
					},
					ty,
				)
			}

			ResolvedExpr::If {
				cond,
				then_block,
				else_branch,
				..
			} => {
				let tcond: TypedExpr = self.check_expr(cond, Some(&Ty::Primitive(Primitive::Bool)))?;
				self.expect_ty(&tcond.ty, &Ty::Primitive(Primitive::Bool), span)?;
				let tthen: TypedBlock = self.check_block_as_value(then_block)?;
				let telse: Option<Box<TypedExpr>> = else_branch
					.as_ref()
					.map(|e| return self.check_expr(e, Some(&tthen.ty)).map(Box::new))
					.transpose()?;

				let ty: Ty = match &telse {
					Some(te) if tthen.ty == Ty::Never => te.ty.clone(),
					Some(te) if te.ty == Ty::Never || te.ty == tthen.ty => tthen.ty.clone(),
					Some(te) => {
						return Err(Self::err(
							span,
							TypeErrorKind::IfBranchTypeMismatch {
								then_ty: self.fmt_ty(&tthen.ty),
								else_ty: self.fmt_ty(&te.ty),
							},
						));
					}
					None => Ty::Unit,
				};

				(
					TypedExprKind::If {
						cond: Box::new(tcond),
						then_block: tthen,
						else_branch: telse,
					},
					ty,
				)
			}

			ResolvedExpr::Loop { label, body, .. } => {
				let tbody: TypedBlock = self.check_block_as_value(body)?;
				let loop_ty: Ty = collect_loop_break_ty(&tbody, label).unwrap_or(Ty::Never);
				(
					TypedExprKind::Loop {
						label: label.clone(),
						body: Box::new(tbody),
					},
					loop_ty,
				)
			}
		};

		if ty == Ty::Infer || ty == Ty::SelfTy {
			let is_pending_ident = if let TypedExprKind::Identifier { path } = &kind {
				matches!(&path.kind, ResolvedPathKind::Resolved(id) if self.infer_syms.contains(id))
			} else {
				false
			};
			let is_infer_literal = matches!(&kind, TypedExprKind::Literal { .. });
			if !is_pending_ident && !is_infer_literal {
				return Err(Self::err(span, TypeErrorKind::CannotInferType));
			}
		}

		return Ok(TypedExpr { kind, ty, span });
	}

	fn resolve_named_to_primitive(&self, ty: &Ty) -> Ty
	{
		if let Ty::Named { symbol, .. } = ty
			&& let Some(resolved) = self.caches.env.get(*symbol)
			&& (resolved.is_numeric() || matches!(resolved, Ty::Primitive(_)))
		{
			return resolved.clone();
		}
		return ty.clone();
	}

	fn substitute_assoc_bindings(&self, ty: &Ty, receiver_ty: &Ty) -> Ty
	{
		let bounds: &Vec<TyBound> = match receiver_ty {
			Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => bounds,
			_ => return ty.clone(),
		};

		let mut subs: HashMap<String, Ty> = std::collections::HashMap::new();
		for bound in bounds {
			if let TyBound::Trait { args, .. } = bound {
				for arg in args {
					if let TyGenericArg::Binding { name, ty: concrete } = arg {
						subs.insert(name.clone(), concrete.clone());
					}
				}
			}
		}

		if subs.is_empty() {
			return ty.clone();
		}
		return self.substitute_with_name_map(ty, &subs);
	}
	fn substitute_with_name_map(&self, ty: &Ty, subs: &HashMap<String, Ty>) -> Ty
	{
		return match ty {
			Ty::Named { symbol, generics } => {
				let sym_name = &self.global.symbol(*symbol).name;
				if let Some(concrete) = subs.get(sym_name.as_str()) {
					return concrete.clone();
				}
				Ty::Named {
					symbol: *symbol,
					generics: generics
						.iter()
						.map(|g| return self.substitute_with_name_map(g, subs))
						.collect(),
				}
			}
			Ty::Generic { name, .. } => subs.get(name.as_str()).cloned().unwrap_or_else(|| return ty.clone()),
			Ty::Reference { mutable, inner } => Ty::Reference {
				mutable: *mutable,
				inner: Box::new(self.substitute_with_name_map(inner, subs)),
			},
			other => other.clone(),
		};
	}

	fn is_fn_type(ty: &Ty) -> bool
	{
		return match ty {
			Ty::Infer => true,
			Ty::Generic { bounds, .. } => bounds.is_empty() || bounds.iter().any(|b| matches!(b, TyBound::Fn { .. })),
			Ty::ImplTrait { bounds, .. } => bounds.iter().any(|b| matches!(b, TyBound::Fn { .. })),
			_ => false,
		};
	}

	fn callee_is_callable(&self, callee: &TypedExpr) -> bool
	{
		match &callee.kind {
			TypedExprKind::InternalCall { .. } => return true,

			TypedExprKind::Identifier { path } => {
				if let ResolvedPathKind::Resolved(id) = &path.kind
					&& matches!(self.global.symbol(*id).kind, SymbolKind::Function { .. })
				{
					return true;
				}
				if let ResolvedPathKind::AssocItem { base, member, .. } = &path.kind {
					let actual = self.resolve_to_struct_sym(*base);
					let base_name = self.global.symbol(*base).name.clone();
					if self.caches.method_fn.get_sym(*base, member).is_some()
						|| self.caches.method_fn.get_sym(actual, member).is_some()
						|| self.find_method_fn_sym_by_name(&base_name, member).is_some()
					{
						return true;
					}
				}
				return Self::is_fn_type(&callee.ty);
			}

			TypedExprKind::Field { base, name } => {
				match &base.ty {
					Ty::Named { symbol, .. } => {
						let actual = self.resolve_to_struct_sym(*symbol);
						if self.caches.method.get_sym(*symbol, name).is_some()
							|| self.caches.method.get_sym(actual, name).is_some()
						{
							return true;
						}
					}
					Ty::Primitive(p) => {
						let key = TyKey::Prim(p.clone());
						if self.caches.method.get(&key, name).is_some() {
							return true;
						}
					}
					_ => {}
				}

				match &base.ty {
					Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => {
						for bound in bounds {
							if let TyBound::Trait { symbol: trait_sym, .. } = bound
								&& self.caches.method.get_sym(*trait_sym, name).is_some()
							{
								return true;
							}
						}
					}
					_ => {}
				}
				return Self::is_fn_type(&callee.ty);
			}

			_ => return Self::is_fn_type(&callee.ty),
		}
	}

	fn type_of_literal(lit: &Literal, hint: Option<&Ty>) -> Ty
	{
		match lit {
			Literal::Int { ty, .. } => {
				if let Some(int_ty) = ty {
					return Ty::Primitive(Primitive::int_from_int_type(int_ty.clone()));
				}
				if let Some(h) = hint {
					if h.is_integer() {
						return h.clone();
					}
					if matches!(
						h,
						Ty::Generic { .. } | Ty::Infer | Ty::SelfTy | Ty::Named { .. } | Ty::ImplTrait { .. }
					) {
						return h.clone();
					}
					for binding in h.impl_trait_binding_hints() {
						if binding.is_integer() {
							return binding.clone();
						}
					}
				}
				return Ty::Infer;
			}
			Literal::Float { bits, .. } => {
				if let Some(b) = bits {
					let prim = match b {
						32 => Primitive::F32,
						64 => Primitive::F64,
						_ => return Ty::Infer,
					};
					return Ty::Primitive(prim);
				}
				if let Some(h) = hint {
					if h.is_float() {
						return h.clone();
					}
					if matches!(
						h,
						Ty::Generic { .. } | Ty::Infer | Ty::SelfTy | Ty::Named { .. } | Ty::ImplTrait { .. }
					) {
						return h.clone();
					}
					for binding in h.impl_trait_binding_hints() {
						if binding.is_float() {
							return binding.clone();
						}
					}
				}
				return Ty::Infer;
			}
			Literal::Bool { .. } => return Ty::Primitive(Primitive::Bool),
			Literal::String { flags, .. } => {
				if flags.contains_single(StringFlags::CSTRING) {
					return Ty::Reference {
						inner: Box::new(Ty::Primitive(Primitive::CStr)),
						mutable: false,
					};
				}
				return Ty::Reference {
					inner: Box::new(Ty::Primitive(Primitive::Str)),
					mutable: false,
				};
			}
			Literal::Char { .. } => return Ty::Primitive(Primitive::Char),
		}
	}

	fn check_unary(&mut self, op: UnaryOp, ty: &Ty, span: Span) -> Result<Ty, TypeError>
	{
		if matches!(op, UnaryOp::Deref) {
			return Ok(match ty {
				Ty::Reference { inner, .. } | Ty::Mutable { inner } | Ty::Pointer { inner, .. } => *inner.clone(),
				Ty::Infer | Ty::Generic { .. } => Ty::Infer,
				other => {
					return Err(Self::err(
						span,
						TypeErrorKind::InvalidUnaryOp {
							op,
							ty: self.fmt_ty(other),
						},
					));
				}
			});
		}

		if let UnaryOp::Addr { mutable } = op {
			return Ok(Ty::Reference {
				mutable,
				inner: Box::new(ty.clone()),
			});
		}

		if matches!(ty, Ty::Generic { .. } | Ty::Infer) {
			return Ok(match op {
				UnaryOp::Deref => Ty::Infer,
				_ => ty.clone(),
			});
		}

		let method_name = unary_op_trait_method!(op).expect("only Addr has no method name, and it was handled above");

		return self.op_trait_return_ty(ty, method_name).map_or_else(
			|| {
				return Err(Self::err(
					span,
					TypeErrorKind::InvalidUnaryOp {
						op,
						ty: self.fmt_ty(ty),
					},
				));
			},
			|ret| {
				return Ok(match op {
					UnaryOp::Deref => match ret {
						Ty::Reference { inner, .. } | Ty::Mutable { inner } => *inner,
						other => other,
					},
					_ => ret,
				});
			},
		);
	}

	fn check_binary(
		&mut self,
		op: BinaryOp,
		tlhs: TypedExpr,
		trhs: TypedExpr,
		span: Span,
	) -> Result<TypedExpr, TypeError>
	{
		let lhs = &tlhs.ty.clone();
		let rhs = &trhs.ty.clone();

		if matches!(
			op,
			BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge
		) && (lhs.is_integer() || matches!(lhs, Ty::Infer | Ty::Generic { .. }))
		{
			let intrinsic = match op {
				BinaryOp::Eq => Intrinsic::IntEq,
				BinaryOp::Ne => Intrinsic::IntNe,
				BinaryOp::Lt => Intrinsic::IntLt,
				BinaryOp::Le => Intrinsic::IntLe,
				BinaryOp::Gt => Intrinsic::IntGt,
				BinaryOp::Ge => Intrinsic::IntGe,
				_ => unreachable!(),
			};
			let callee = TypedExpr {
				kind: TypedExprKind::InternalCall { intrinsic },
				ty: Ty::Infer,
				span,
			};
			return Ok(TypedExpr {
				kind: TypedExprKind::Call {
					callee: Box::new(callee),
					call_type: CallType::Regular,
					named_generics: Vec::new(),
					args: vec![tlhs, trhs],
				},
				ty: Ty::Primitive(Primitive::Bool),
				span,
			});
		}

		if matches!(lhs, Ty::Generic { .. } | Ty::Infer) || matches!(rhs, Ty::Generic { .. } | Ty::Infer) {
			let ty = match op {
				BinaryOp::Eq
				| BinaryOp::Ne
				| BinaryOp::Lt
				| BinaryOp::Gt
				| BinaryOp::Le
				| BinaryOp::Ge
				| BinaryOp::LogicalAnd
				| BinaryOp::LogicalOr => Ty::Primitive(Primitive::Bool),
				_ => lhs.clone(),
			};
			return Ok(TypedExpr {
				kind: TypedExprKind::Binary {
					op,
					lhs: Box::new(tlhs),
					rhs: Box::new(trhs),
				},
				ty,
				span,
			});
		}

		if matches!(op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) {
			if *lhs == Ty::Primitive(Primitive::Bool) && *rhs == Ty::Primitive(Primitive::Bool) {
				return Ok(TypedExpr {
					kind: TypedExprKind::Binary {
						op,
						lhs: Box::new(tlhs),
						rhs: Box::new(trhs),
					},
					ty: Ty::Primitive(Primitive::Bool),
					span,
				});
			}
			return Err(Self::err(
				span,
				TypeErrorKind::InvalidBinaryOp {
					op,
					lhs: self.fmt_ty(lhs),
					rhs: self.fmt_ty(rhs),
				},
			));
		}

		let trait_name = binary_op_trait_method!(op).expect("non-logical ops always have a method name");

		let ret_ty = self.op_trait_return_ty(lhs, trait_name).ok_or_else(|| {
			return Self::err(
				span,
				TypeErrorKind::InvalidBinaryOp {
					op,
					lhs: self.fmt_ty(lhs),
					rhs: self.fmt_ty(rhs),
				},
			);
		})?;

		let out_ty = match op {
			BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => {
				Ty::Primitive(Primitive::Bool)
			}
			_ => ret_ty,
		};

		if let Some(fn_sym) = self.op_trait_fn_sym(lhs, trait_name) {
			let method_name = self.global.symbol(fn_sym).name.clone();

			let oexpected_rhs: Option<Ty> = self
				.traits
				.blanket_impls
				.iter()
				.find_map(|b| {
					return b
						.methods
						.iter()
						.find(|m| return m.fn_sym == fn_sym && m.name == method_name)
						.and_then(|m| return m.param_tys.first())
						.map(|pty| return substitute_self(pty, lhs));
				})
				.or_else(|| {
					return self.caches.param.get(fn_sym, self.rhs_param_index(fn_sym)).cloned();
				});

			if let Some(expected_rhs) = oexpected_rhs
				&& !matches!(expected_rhs, Ty::Generic { .. } | Ty::Infer | Ty::SelfTy)
			{
				self.expect_ty(&trhs.ty, &expected_rhs, span).map_err(|_| {
					return Self::err(
						span,
						TypeErrorKind::InvalidBinaryOp {
							op,
							lhs: self.fmt_ty(lhs),
							rhs: self.fmt_ty(rhs),
						},
					);
				})?;
			}

			let callee = TypedExpr {
				ty: out_ty.clone(),
				span,
				kind: TypedExprKind::Field {
					base: Box::new(tlhs),
					name: method_name,
				},
			};
			return Ok(TypedExpr {
				kind: TypedExprKind::Call {
					callee: Box::new(callee),
					call_type: CallType::Regular,
					named_generics: Vec::new(),
					args: vec![trhs],
				},
				ty: out_ty,
				span,
			});
		}

		return Err(TypeError {
			span,
			kind: TypeErrorKind::InvalidBinaryOp {
				op,
				lhs: self.fmt_ty(&tlhs.ty),
				rhs: self.fmt_ty(&trhs.ty),
			},
			context: Vec::new(),
		});
	}

	fn rhs_param_index(&self, fn_sym: SymbolId) -> usize
	{
		return match self.constraints.blanket_fn_has_self.get(&fn_sym) {
			Some(true) | None => 1,
			Some(false) => 0,
		};
	}

	fn op_trait_fn_sym(&mut self, vty: &Ty, trait_name: &str) -> Option<SymbolId>
	{
		let ty = match vty {
			Ty::Reference { inner, .. } | Ty::Mutable { inner } | Ty::Pointer { inner, .. } => inner,
			other => other,
		};

		let trait_sym = self.traits.op_symbols.get(trait_name).copied().or_else(|| {
			return if self.traits.op_symbols.is_empty() {
				return self.global.symbols.iter().enumerate().find_map(|(i, sym)| {
					return if sym.name == trait_name && matches!(sym.kind, SymbolKind::Trait) {
						Some(SymbolId(i))
					} else {
						None
					};
				});
			} else {
				None
			};
		})?;

		let key = TyKey::of(ty)?;

		self.apply_blanket_impls_for_ty(ty);

		let method_name: String = self
			.traits
			.decls
			.get(&trait_sym)
			.and_then(|td| {
				return td.items.iter().find_map(|item| {
					return if let ResolvedTraitItem::Function(f) = item {
						Some(f.signature.name.clone())
					} else {
						None
					};
				});
			})
			.unwrap_or_else(|| return trait_name.to_lowercase());

		return self.caches.method_fn.get(&key, &method_name).copied();
	}

	fn check_cast(&self, from: &Ty, to: &Ty, span: Span) -> Result<(), TypeError>
	{
		let ok: bool = from == to
			|| from.is_numeric() && to.is_numeric()
			|| from.is_integer() && matches!(to, Ty::Pointer { .. })
			|| matches!(from, Ty::Pointer { .. }) && to.is_integer()
			|| matches!(from, Ty::Pointer { .. }) && matches!(to, Ty::Pointer { .. })
			|| matches!(from, Ty::Generic { .. } | Ty::Infer)
			|| matches!(to, Ty::Generic { .. } | Ty::Infer)
			|| self.types_are_alias_compatible(from, to);
		if !ok {
			return Err(Self::err(
				span,
				TypeErrorKind::InvalidCast {
					from: self.fmt_ty(from),
					to: self.fmt_ty(to),
				},
			));
		}
		return Ok(());
	}

	fn types_are_alias_compatible(&self, a: &Ty, b: &Ty) -> bool
	{
		let sym_a = match a {
			Ty::Named { symbol, .. } => *symbol,
			_ => return false,
		};
		let sym_b = match b {
			Ty::Named { symbol, .. } => *symbol,
			_ => return false,
		};
		return self.resolve_to_struct_sym(sym_a) == self.resolve_to_struct_sym(sym_b);
	}

	fn check_field_access(&mut self, base_ty: &Ty, name: &str, span: Span) -> Result<Ty, TypeError>
	{
		match base_ty {
			Ty::Reference { inner, .. } | Ty::Mutable { inner } | Ty::Pointer { inner, .. } => {
				return self.check_field_access(inner, name, span);
			}

			Ty::Infer | Ty::SelfTy => {
				return Err(Self::err(span, TypeErrorKind::CannotInferType));
			}

			Ty::Generic { bounds, .. } => {
				for bound in bounds {
					if let TyBound::Trait { symbol: trait_sym, .. } = bound
						&& let Some(ty) = self.caches.method.get_sym(*trait_sym, name)
					{
						return Ok(ty.clone());
					}
				}
				return Err(Self::err(
					span,
					TypeErrorKind::UnknownField {
						ty: self.fmt_ty(base_ty),
						field: name.to_owned(),
					},
				));
			}

			Ty::ImplTrait { bounds, .. } => {
				for bound in bounds {
					if let TyBound::Trait { symbol: trait_sym, .. } = bound
						&& let Some(ty) = self.caches.method.get_sym(*trait_sym, name)
					{
						return Ok(ty.clone());
					}
				}
				return Err(Self::err(
					span,
					TypeErrorKind::UnknownField {
						ty: self.fmt_ty(base_ty),
						field: name.to_owned(),
					},
				));
			}

			Ty::Named { symbol, .. } => {
				let actual: SymbolId = self.resolve_to_struct_sym(*symbol);
				let sym: &Symbol = self.global.symbol(actual);
				if !matches!(sym.kind, SymbolKind::Struct | SymbolKind::Union) {
					return Err(Self::err(
						span,
						TypeErrorKind::FieldAccessOnNonStruct {
							ty: self.fmt_ty(base_ty),
						},
					));
				}

				if let Some(ty) = self.caches.field.get(actual, name) {
					return Ok(ty.clone());
				}
				if actual != *symbol
					&& let Some(ty) = self.caches.field.get(*symbol, name)
				{
					return Ok(ty.clone());
				}

				if let Some(ty) = self.find_method_ty(*symbol, name) {
					return Ok(ty);
				}

				return Err(Self::err(
					span,
					TypeErrorKind::UnknownField {
						ty: self.fmt_ty(base_ty),
						field: name.to_owned(),
					},
				));
			}

			Ty::Tuple(v_ty) => {
				let Ok(index) = name.parse::<usize>() else {
					return Err(Self::err(
						span,
						TypeErrorKind::FieldAccessOnNonStruct {
							ty: self.fmt_ty(base_ty),
						},
					));
				};
				let Some(ty): Option<&Ty> = v_ty.get(index) else {
					return Err(Self::err(
						span,
						TypeErrorKind::FieldAccessOnNonStruct {
							ty: self.fmt_ty(base_ty),
						},
					));
				};
				return Ok(ty.clone());
			}

			Ty::Primitive(p) => {
				let key = TyKey::Prim(p.clone());

				self.apply_blanket_impls_for_ty(base_ty);
				if let Some(ty) = self.caches.method.get(&key, name) {
					return Ok(ty.clone());
				}
				return Err(Self::err(
					span,
					TypeErrorKind::FieldAccessOnNonStruct {
						ty: self.fmt_ty(base_ty),
					},
				));
			}

			other => {
				return Err(Self::err(
					span,
					TypeErrorKind::FieldAccessOnNonStruct { ty: self.fmt_ty(other) },
				));
			}
		}
	}

	fn key_implements_trait(&self, key: &TyKey, trait_sym: SymbolId) -> bool
	{
		if self
			.traits
			.impls
			.get(key)
			.is_some_and(|s| return s.contains(&trait_sym))
		{
			return true;
		}

		if let TyKey::Symbol(sym) = key {
			let actual = self.resolve_to_struct_sym(*sym);
			if actual != *sym {
				return self
					.traits
					.impls
					.get(&TyKey::Symbol(actual))
					.is_some_and(|s| return s.contains(&trait_sym));
			}
		}
		return false;
	}

	fn find_method_ty(&self, type_sym: SymbolId, name: &str) -> Option<Ty>
	{
		let actual = self.resolve_to_struct_sym(type_sym);
		return self
			.caches
			.method
			.get_sym(type_sym, name)
			.or_else(|| return self.caches.method.get_sym(actual, name))
			.cloned();
	}

	fn find_method_ty_by_name(&self, type_name: &str, method_name: &str) -> Option<Ty>
	{
		return self.caches.method.iter().find_map(|((key, mname), ty)| {
			if mname != method_name {
				return None;
			}
			let key_name = match key {
				TyKey::Symbol(sym) => self.global.symbol(*sym).name.as_str().to_owned(),
				TyKey::Prim(p) => p.to_string(),
			};
			if key_name == type_name {
				return Some(ty.clone());
			}
			return None;
		});
	}

	fn op_trait_return_ty(&mut self, vty: &Ty, trait_name: &str) -> Option<Ty>
	{
		let ty = match vty {
			Ty::Reference { inner, .. } | Ty::Mutable { inner } | Ty::Pointer { inner, .. } => {
				return self.op_trait_return_ty(inner, trait_name);
			}
			other => other,
		};

		let trait_sym = self.traits.op_symbols.get(trait_name).copied().or_else(|| {
			return if self.traits.op_symbols.is_empty() {
				return self.global.symbols.iter().enumerate().find_map(|(i, sym)| {
					return if sym.name == trait_name && matches!(sym.kind, SymbolKind::Trait) {
						Some(SymbolId(i))
					} else {
						None
					};
				});
			} else {
				None
			};
		})?;

		let key = TyKey::of(ty)?;

		self.apply_blanket_impls_for_ty(ty);

		if !self.key_implements_trait(&key, trait_sym) {
			return None;
		}

		let method_name: String = self
			.traits
			.decls
			.get(&trait_sym)
			.and_then(|td| {
				return td.items.iter().find_map(|item| {
					return if let ResolvedTraitItem::Function(f) = item {
						Some(f.signature.name.clone())
					} else {
						None
					};
				});
			})
			.unwrap_or_else(|| return trait_name.to_lowercase());

		return self.caches.method.get(&key, &method_name).cloned();
	}

	fn find_method_fn_sym_by_name(&self, type_name: &str, method_name: &str) -> Option<SymbolId>
	{
		return self.caches.method_fn.iter().find_map(|((key, mname), &fn_sym)| {
			if mname != method_name {
				return None;
			}
			let key_name = match key {
				TyKey::Symbol(sym) => self.global.symbol(*sym).name.as_str().to_owned(),
				TyKey::Prim(p) => p.to_string(),
			};
			if key_name == type_name {
				return Some(fn_sym);
			}
			return None;
		});
	}

	fn index_elem_ty(&mut self, base_ty: &Ty, span: Span) -> Result<Ty, TypeError>
	{
		match base_ty {
			Ty::Array { inner, .. } | Ty::Pointer { inner, .. } => return Ok(*inner.clone()),

			Ty::Reference { inner, .. } | Ty::Mutable { inner } => {
				return self.index_elem_ty(inner, span);
			}

			Ty::Infer | Ty::SelfTy => {
				return Err(Self::err(span, TypeErrorKind::CannotInferType));
			}

			Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => {
				for bound in bounds {
					if let TyBound::Trait { symbol: trait_sym, .. } = bound
						&& self.global.symbol(*trait_sym).name == "Index"
					{
						if let Some(ret) = self.caches.method.get_sym(*trait_sym, "index") {
							return Ok(ret.clone());
						}
						return Err(Self::err(
							span,
							TypeErrorKind::UnknownField {
								ty: self.fmt_ty(base_ty),
								field: "index".to_owned(),
							},
						));
					}
				}
				return Err(Self::err(
					span,
					TypeErrorKind::IndexOnNonArray {
						ty: self.fmt_ty(base_ty),
					},
				));
			}

			Ty::Named { .. } => {
				return self.op_trait_return_ty(base_ty, "Index").ok_or_else(|| {
					return Self::err(
						span,
						TypeErrorKind::IndexOnNonArray {
							ty: self.fmt_ty(base_ty),
						},
					);
				});
			}

			other => {
				return Err(Self::err(
					span,
					TypeErrorKind::IndexOnNonArray { ty: self.fmt_ty(other) },
				));
			}
		}
	}

	fn check_array_literal(
		&mut self,
		arr: &ResolvedArrayLiteral,
		hint: Option<&Ty>,
	) -> Result<(TypedArrayLiteral, Ty), TypeError>
	{
		let elem_hint: Option<Ty> = hint.and_then(|h| {
			if let Ty::Array { inner, .. } = h {
				return Some(*inner.clone());
			}
			return None;
		});

		return Ok(match arr {
			ResolvedArrayLiteral::List { elements, span } => {
				let telems: Vec<TypedExpr> = elements
					.iter()
					.map(|e| return self.check_expr(e, elem_hint.as_ref()))
					.collect::<Result<_, _>>()?;
				let elem_ty = match telems.first() {
					Some(e) => e.ty.clone(),
					None => elem_hint.clone().ok_or_else(|| {
						return Self::err(*span, TypeErrorKind::CannotInferType);
					})?,
				};
				let len = telems.len() as u64;
				(
					TypedArrayLiteral::List {
						elements: telems,
						span: *span,
					},
					Ty::Array {
						inner: Box::new(elem_ty),
						size: Some(len),
					},
				)
			}
			ResolvedArrayLiteral::Repeat { value, count, span } => {
				let usize_ty = Ty::Primitive(Primitive::Int(IntType {
					bits: IntSize::Size,
					sign: IntSign::Unsigned,
				}));

				let tval: TypedExpr = self.check_expr(value, elem_hint.as_ref())?;
				let tcount: TypedExpr = self.check_expr(count, Some(&usize_ty))?;
				self.expect_ty(&tcount.ty, &usize_ty, count.span())?;

				let static_size: Option<u64> = if let TypedExprKind::Literal {
					value: lit @ Literal::Int { .. },
				} = &tcount.kind
				{
					#[allow(clippy::cast_possible_truncation)]
					#[allow(clippy::cast_sign_loss)]
					let Ok(r) = read_radix_number(lit)
						.map(|val| return val as u64)
						.inspect_err(|e| eprintln!("{:#?}", e))
					else {
						todo!("make a good error for when parsing the number does not work");
					};
					Some(r)
				} else {
					None
				};

				let elem: Ty = tval.ty.clone();
				(
					TypedArrayLiteral::Repeat {
						value: Box::new(tval),
						count: Box::new(tcount),
						span: *span,
					},
					Ty::Array {
						inner: Box::new(elem),
						size: static_size,
					},
				)
			}
		});
	}

	fn check_range_expr(&mut self, re: &ResolvedRangeExpr, hint: Option<&Ty>) -> Result<TypedRangeExpr, TypeError>
	{
		let tstart: Option<TypedExpr> = re.start.as_ref().map(|e| return self.check_expr(e, hint)).transpose()?;
		let end_hint: Option<&Ty> = tstart.as_ref().map(|s| return &s.ty).or(hint);
		let tend = re
			.end
			.as_ref()
			.map(|e| return self.check_expr(e, end_hint))
			.transpose()?;
		let elem = match tstart.as_ref().or(tend.as_ref()) {
			Some(e) => e.ty.clone(),
			None => hint.cloned().ok_or_else(|| {
				return Self::err(re.span, TypeErrorKind::CannotInferType);
			})?,
		};
		return Ok(TypedRangeExpr {
			start: tstart.map(Box::new),
			end: tend.map(Box::new),
			inclusive: re.inclusive,
			ty: elem,
			span: re.span,
		});
	}

	fn check_pattern(&mut self, pattern: &ResolvedPattern, scrutinee: &Ty) -> Result<TypedPattern, TypeError>
	{
		return Ok(match pattern {
			ResolvedPattern::Wildcard { ty, span } => {
				let pat_ty: Ty = ty
					.as_ref()
					.map(|t| return self.lower_ty(t))
					.transpose()?
					.unwrap_or_else(|| return scrutinee.clone());
				TypedPattern::Wildcard {
					ty: pat_ty,
					span: *span,
				}
			}

			ResolvedPattern::Literal { value, span } => {
				let ty: Ty = Self::type_of_literal(value, Some(scrutinee));
				TypedPattern::Literal {
					value: value.clone(),
					ty,
					span: *span,
				}
			}

			ResolvedPattern::TypedIdentifier {
				symbol,
				name,
				ty,
				mutable,
				span,
			} => {
				let declared: Ty = self.lower_ty(ty)?;
				let final_ty: Ty = if declared == Ty::Infer {
					scrutinee.clone()
				} else {
					declared
				};
				self.caches.env.insert(*symbol, final_ty.clone());
				TypedPattern::TypedIdentifier {
					symbol: *symbol,
					name: name.clone(),
					ty: final_ty,
					mutable: *mutable,
					span: *span,
				}
			}

			ResolvedPattern::Variant { path, args, span } => {
				let variant_ty: Ty = match &path.kind {
					ResolvedPathKind::Resolved(id) => Ty::named(*id),
					ResolvedPathKind::AssocItem { base, .. } => Ty::named(*base),
					ResolvedPathKind::Primitive(ty) => {
						return Err(Self::err(
							*span,
							TypeErrorKind::InvalidPrimitivePosition {
								ty: self.fmt_ty(ty),
								position: "a variant pattern",
							},
						));
					}
				};

				let member_sym: Option<SymbolId> = match &path.kind {
					ResolvedPathKind::Resolved(id) => Some(*id),
					ResolvedPathKind::AssocItem { item, .. } if *item != SymbolId::DUMMY => Some(*item),
					ResolvedPathKind::AssocItem { base, member, .. } => {
						self.global.symbol(*base).introduced_scope.and_then(|sc| {
							return self.global.scope(sc).symbols.iter().copied().find(|&s| {
								return self.global.symbol(s).name == *member
									&& matches!(self.global.symbol(s).kind, SymbolKind::VariantMember);
							});
						})
					}
					ResolvedPathKind::Primitive(_) => None,
				};

				let parent_sym: Option<SymbolId> = member_sym.and_then(|ms| {
					return self.global.symbols.iter().enumerate().find_map(|(i, s)| {
						if !matches!(s.kind, SymbolKind::Variant) {
							return None;
						}
						let sc = s.introduced_scope?;
						if self.global.scope(sc).symbols.contains(&ms) {
							return Some(SymbolId(i));
						}
						return None;
					});
				});

				let scrutinee_args: Vec<Ty> = match scrutinee {
					Ty::Named { generics, .. } => generics.clone(),
					_ => Vec::new(),
				};
				let mut subs: HashMap<String, Ty> = HashMap::new();
				if let Some(p) = parent_sym
					&& let Some(names) = self.caches.variant_generics.get(&p)
				{
					for (n, t) in names.iter().zip(scrutinee_args.iter()) {
						subs.insert(n.clone(), t.clone());
					}
				}

				let payload_tys: Vec<Ty> = member_sym
					.and_then(|s| return self.caches.env.get(s).cloned())
					.map(|env_ty| vec![substitute_generics(&env_ty, &subs)])
					.unwrap_or_default();

				let targs: Vec<TypedPattern> = args
					.iter()
					.enumerate()
					.map(|(i, p)| {
						let sub_scrut = payload_tys.get(i).cloned().unwrap_or(Ty::Infer);
						return self.check_pattern(p, &sub_scrut);
					})
					.collect::<Result<_, _>>()?;

				let mut tpath = path.clone();
				self.finalize_assoc_in_path(&mut tpath, Some(&variant_ty));
				TypedPattern::Variant {
					path: tpath,
					args: targs,
					ty: variant_ty,
					span: *span,
				}
			}

			ResolvedPattern::Tuple { patterns, span } => {
				let elem_hints: Option<&Vec<Ty>> = if let Ty::Tuple(ts) = scrutinee { Some(ts) } else { None };
				let tpats: Vec<TypedPattern> = patterns
					.iter()
					.enumerate()
					.map(|(i, p)| {
						let h = elem_hints.and_then(|ts| return ts.get(i)).unwrap_or(&Ty::Infer);
						return self.check_pattern(p, h);
					})
					.collect::<Result<_, _>>()?;
				let tys: Vec<Ty> = tpats.iter().map(|p| return p.ty().clone()).collect();
				TypedPattern::Tuple {
					patterns: tpats,
					ty: Ty::Tuple(tys),
					span: *span,
				}
			}

			ResolvedPattern::Struct {
				path,
				fields,
				has_rest,
				span,
			} => {
				let struct_sym: SymbolId = match &path.kind {
					ResolvedPathKind::Resolved(id) => *id,
					ResolvedPathKind::AssocItem { base, .. } => *base,
					ResolvedPathKind::Primitive(ty) => {
						return Err(Self::err(
							*span,
							TypeErrorKind::InvalidPrimitivePosition {
								ty: self.fmt_ty(ty),
								position: "a variant pattern",
							},
						));
					}
				};
				let mut tpath = path.clone();
				self.finalize_assoc_in_path(&mut tpath, Some(&Ty::named(struct_sym)));
				let tfields: Vec<(String, TypedPattern)> = fields
					.iter()
					.map(|(name, pat)| {
						let fty = self.caches.field.get(struct_sym, name).cloned().ok_or_else(|| {
							return Self::err(
								*span,
								TypeErrorKind::UnknownField {
									ty: self.global.symbol(struct_sym).name.clone(),
									field: name.clone(),
								},
							);
						})?;
						return self.check_pattern(pat, &fty).map(|tp| return (name.clone(), tp));
					})
					.collect::<Result<_, TypeError>>()?;
				TypedPattern::Struct {
					path: tpath.clone(),
					fields: tfields,
					has_rest: *has_rest,
					ty: Ty::named(struct_sym),
					span: *span,
				}
			}

			ResolvedPattern::Range(re) => TypedPattern::Range(self.check_range_expr(re, Some(scrutinee))?),

			ResolvedPattern::Or { patterns, span } => {
				let tpats = patterns
					.iter()
					.map(|p| return self.check_pattern(p, scrutinee))
					.collect::<Result<_, _>>()?;
				TypedPattern::Or {
					patterns: tpats,
					ty: scrutinee.clone(),
					span: *span,
				}
			}
		});
	}

	fn check_directive(&mut self, node: &ResolvedDirectiveNode) -> Result<TypedDirectiveNode, TypeError>
	{
		let directive: TypedDirective = match &node.directive {
			ResolvedDirective::Import { import, visibility } => TypedDirective::Import {
				import: import.clone(),
				visibility: *visibility,
			},
			ResolvedDirective::Use { use_path, visibility } => TypedDirective::Use {
				use_path: use_path.clone(),
				visibility: *visibility,
			},
			ResolvedDirective::Custom { name, params } => TypedDirective::Custom {
				name: name.clone(),
				params: params.clone(),
			},
			ResolvedDirective::ValidateStructPattern {
				struct_path,
				pattern_fields,
				has_rest,
			} => TypedDirective::ValidateStructPattern {
				struct_path: struct_path.clone(),
				pattern_fields: pattern_fields.clone(),
				has_rest: *has_rest,
			},
			ResolvedDirective::ValidateType { ty, expr } => {
				let checked_ty = self.lower_ty(ty)?;
				let texpr = self.check_expr(expr, Some(&checked_ty))?;
				self.expect_ty(&texpr.ty, &checked_ty, node.span)?;
				TypedDirective::ValidateType {
					ty: checked_ty,
					expr: texpr,
				}
			}
		};
		return Ok(TypedDirectiveNode {
			directive,
			body: None,
			span: node.span,
		});
	}

	fn pin_infer_sym_notify(&mut self, sym: SymbolId, concrete_ty: &Ty, bis: &mut BlockInferState<'_>) -> bool
	{
		if !self.infer_syms.contains(&sym) {
			return false;
		}
		if *concrete_ty == Ty::Infer || matches!(concrete_ty, Ty::Generic { .. }) {
			return false;
		}
		self.caches.env.insert(sym, concrete_ty.clone());
		self.infer_syms.remove(&sym);
		self.newly_pinned.push(sym);

		if let Some(expr_ids) = bis.sym_to_exprs.get(&sym).cloned() {
			for id in expr_ids {
				bis.refine_hint(id, concrete_ty.clone());
			}
		}
		bis.notify_sym(sym);
		return true;
	}

	fn flush_newly_pinned(&mut self, bis: &mut BlockInferState<'_>, bp: &mut BackpatchState<'_>)
	{
		for sym in self.newly_pinned.drain(..) {
			if let Some(concrete_ty) = self.caches.env.get(sym).cloned()
				&& concrete_ty != Ty::Infer
				&& let Some(expr_ids) = bis.sym_to_exprs.get(&sym).cloned()
			{
				for id in expr_ids {
					bis.refine_hint(id, concrete_ty.clone());
				}
			}
			bis.notify_sym(sym);
			bp.mark_resolved(sym);
		}
	}

	fn drain_expr_worklist(
		&mut self,
		bis: &mut BlockInferState<'_>,
		bp: &mut BackpatchState<'_>,
		stmts: &mut [TypedStmt],
		tail: &mut Option<Box<TypedExpr>>,
	) -> Result<usize, TypeError>
	{
		let mut resolved = 0;

		loop {
			let ids: Vec<InferExprId> = bis.worklist.drain(..).collect();
			if ids.is_empty() {
				break;
			}

			for id in ids {
				let Some(pending) = bis.pending.get(&id) else {
					continue;
				};

				let blocked = pending.sym_deps.iter().any(|s| return self.infer_syms.contains(s));
				if blocked {
					continue;
				}

				let expr: &ResolvedExpr = pending.expr;
				let hint: Option<Ty> = pending.hint.clone();
				let target: PatchTarget = pending.target.clone();

				match self.check_expr(expr, hint.as_ref()) {
					Ok(te) if te.ty != Ty::Infer && !matches!(te.ty, Ty::SelfTy) => {
						bis.pending.remove(&id);
						resolved += 1;

						for sym in self.newly_pinned.drain(..) {
							bis.notify_sym(sym);
						}

						match target {
							PatchTarget::StmtExpr { stmt_index } => {
								stmts[stmt_index] = TypedStmt::Expr(te);
							}
							PatchTarget::VarDeclInit { stmt_index, sym } => {
								let resolved_ty = te.ty.clone();
								if let TypedStmt::VariableDecl(ref mut vd) = stmts[stmt_index] {
									vd.ty = resolved_ty.clone();
									vd.init = Some(te);
								}
								self.caches.env.insert(sym, resolved_ty.clone());
								self.infer_syms.remove(&sym);
								bp.pending.remove(&sym);
								bp.mark_resolved(sym);
								bis.notify_sym(sym);
							}
							PatchTarget::BlockTail => {
								*tail = Some(Box::new(te));
							}
						}
					}

					Ok(_) => {
						for sym in self.newly_pinned.drain(..) {
							bis.notify_sym(sym);
						}
						if let Some(p) = bis.pending.get(&id) {
							let still_blocked = p.sym_deps.iter().any(|s| return self.infer_syms.contains(s));
							if !still_blocked {
								bis.pending.remove(&id);
							}
						}
					}
					Err(e) => {
						for sym in self.newly_pinned.drain(..) {
							bis.notify_sym(sym);
						}
						match e.kind {
							TypeErrorKind::CannotInferType
							| TypeErrorKind::UnresolvedIdentifier { .. }
							| TypeErrorKind::UnresolvedAssocPath { .. } => {
								if let Some(p) = bis.pending.get(&id) {
									let still_blocked = p.sym_deps.iter().any(|s| return self.infer_syms.contains(s));
									if !still_blocked {
										bis.pending.remove(&id);
									}
								}
							}

							_ => return Err(e),
						}
					}
				}
			}

			if bis.worklist.is_empty() {
				break;
			}
		}

		return Ok(resolved);
	}

	fn finalize_block_inference(
		&mut self,
		bis: &mut BlockInferState<'_>,
		bp: &mut BackpatchState<'_>,
		stmts: &mut [TypedStmt],
		tail: &mut Option<Box<TypedExpr>>,
	) -> Result<(), TypeError>
	{
		if bis.is_empty() {
			return Ok(());
		}

		for &id in bis.pending.keys() {
			bis.worklist.push_back(id);
		}

		loop {
			let before = bis.pending.len();
			self.drain_expr_worklist(bis, bp, stmts, tail)?;
			let after = bis.pending.len();

			if after == 0 {
				return Ok(());
			}
			if after == before {
				let span = bis
					.first_span()
					.or_else(|| {
						return stmts.iter().find_map(|s| {
							return if let TypedStmt::Pending(sp) = s {
								Some(*sp)
							} else {
								None
							};
						});
					})
					.unwrap_or_else(|| {
						return Span::default();
					});
				return Err(TypeError::new(span, TypeErrorKind::CannotInferType));
			}
		}
	}

	fn check_block_inner(&mut self, block: &ResolvedBlock, is_value: bool) -> Result<TypedBlock, TypeError>
	{
		let mut bis: BlockInferState<'_> = BlockInferState::default();
		let mut bp: BackpatchState<'_> = BackpatchState::default();
		let mut stmts: Vec<TypedStmt> = Vec::with_capacity(block.stmts.len());
		let mut tail: Option<Box<TypedExpr>> = None;

		for raw_stmt in &block.stmts {
			let stmt_index = stmts.len();

			match raw_stmt {
				ResolvedStmt::VariableDecl(v) => {
					let declared = self.lower_ty(&v.ty)?;

					#[allow(clippy::if_not_else)]
					if declared != Ty::Infer {
						stmts.push(TypedStmt::VariableDecl(self.check_var_decl(v, false)?));
						self.flush_newly_pinned(&mut bis, &mut bp);
					} else {
						match self.try_check_infer_var(v) {
							Ok(typed_decl) => {
								let sym = typed_decl.resolved_name;
								let ty = typed_decl.ty.clone();
								self.caches.env.insert(sym, ty.clone());

								bis.notify_sym(sym);
								stmts.push(TypedStmt::VariableDecl(typed_decl));
								self.flush_newly_pinned(&mut bis, &mut bp);
							}

							Err(InferVarResult::Unresolvable(sym_deps)) => {
								self.caches.env.insert(v.resolved_name, Ty::Infer);
								self.infer_syms.insert(v.resolved_name);

								for &dep in &sym_deps {
									bp.add_dependency(dep, v.resolved_name);
								}
								for &dep in &sym_deps {
									if !self.infer_syms.contains(&dep) {
										bp.mark_resolved(dep);
									}
								}
								bp.add_dependency(v.resolved_name, v.resolved_name);
								bp.pending.insert(
									v.resolved_name,
									PendingDecl {
										resolved: v,
										stmt_index,
									},
								);

								if let Some(init_expr) = &v.init {
									let mut expr_deps = self.collect_infer_deps(init_expr);
									expr_deps.push(v.resolved_name);
									bis.register(
										init_expr,
										None,
										PatchTarget::VarDeclInit {
											stmt_index,
											sym: v.resolved_name,
										},
										expr_deps,
									);
								}

								stmts.push(TypedStmt::VariableDecl(TypedVariableDecl {
									resolved_name: v.resolved_name,
									name: v.name.clone(),
									ty: Ty::Infer,
									init: None,
									comp_const: v.comp_const,
									mutable: v.mutable,
									modifiers: v.modifiers.clone(),
									docs: v.docs.clone(),
									span: v.span,
								}));
							}

							Err(InferVarResult::HardError(e)) => return Err(e),
						}
					}
				}

				ResolvedStmt::Expr(expr) => match self.check_expr(expr, None) {
					Ok(te) if te.ty != Ty::Infer => {
						self.flush_newly_pinned(&mut bis, &mut bp);
						stmts.push(TypedStmt::Expr(te));
					}
					Ok(_) => {
						self.flush_newly_pinned(&mut bis, &mut bp);
						let deps = self.collect_infer_deps(expr);
						bis.register(expr, None, PatchTarget::StmtExpr { stmt_index }, deps);
						stmts.push(TypedStmt::Pending(expr.span()));
					}
					Err(err) => match err.kind {
						TypeErrorKind::CannotInferType
						| TypeErrorKind::UnresolvedIdentifier { .. }
						| TypeErrorKind::UnresolvedAssocPath { .. } => {
							self.flush_newly_pinned(&mut bis, &mut bp);
							let deps = self.collect_infer_deps(expr);
							if deps.is_empty() {
								return Err(err);
							}
							bis.register(expr, None, PatchTarget::StmtExpr { stmt_index }, deps);
							stmts.push(TypedStmt::Pending(expr.span()));
						}
						_ => return Err(err),
					},
				},

				ResolvedStmt::Return { value, span } => {
					let expected = self.fn_ctx.return_ty.clone().unwrap_or(Ty::Unit);
					let tvalue: Option<TypedExpr> = value
						.as_ref()
						.map(|e| {
							let te = self.check_expr(e, Some(&expected))?;
							self.expect_ty(&te.ty, &expected, *span).map_err(|_| {
								return TypeError::new(
									*span,
									TypeErrorKind::ReturnTypeMismatch {
										expected: self.fmt_ty(&expected),
										found: self.fmt_ty(&te.ty),
									},
								);
							})?;

							if expected != Ty::Unit
								&& expected != Ty::Infer && let ResolvedExpr::Identifier { path, .. } = e.clone()
								&& let ResolvedPathKind::Resolved(ret_sym) = &path.kind
							{
								self.pin_infer_sym_notify(*ret_sym, &te.ty, &mut bis);
							}
							return Ok(te);
						})
						.transpose()?;

					if value.is_none() && expected != Ty::Unit && expected != Ty::Never {
						return Err(TypeError::new(
							*span,
							TypeErrorKind::ReturnTypeMismatch {
								expected: self.fmt_ty(&expected),
								found: self.fmt_ty(&Ty::Unit),
							},
						));
					}

					self.flush_newly_pinned(&mut bis, &mut bp);
					stmts.push(TypedStmt::Return {
						value: tvalue,
						span: *span,
					});
				}

				ResolvedStmt::Assignment {
					target,
					op,
					value,
					span,
				} => {
					let ttarget = self.check_expr(target, None)?;
					let lhs_hint = if ttarget.ty == Ty::Infer {
						None
					} else {
						Some(&ttarget.ty)
					};
					let tvalue = self.check_expr(value, lhs_hint)?;

					if let TypedExprKind::Identifier { path } = &ttarget.kind
						&& let ResolvedPathKind::Resolved(tgt_sym) = &path.kind
						&& let Some(Ty::ImplTrait { concrete, .. }) = self.caches.env.get(*tgt_sym).cloned()
					{
						match concrete {
							Some(concrete_ty) => {
								if tvalue.ty != Ty::Infer && !tvalue.ty.is_assignable_to(&concrete_ty) {
									return Err(Self::err(
										*span,
										TypeErrorKind::TypeMismatch {
											expected: self.fmt_ty(&concrete_ty),
											found: self.fmt_ty(&tvalue.ty),
										},
									));
								}
							}

							None => {
								return Err(Self::err(
									*span,
									TypeErrorKind::TypeMismatch {
										expected: format!(
											"cannot assign to `impl Trait` parameter `{}` — its concrete type is unknown",
											self.fmt_ty(&ttarget.ty)
										),
										found: self.fmt_ty(&tvalue.ty),
									},
								));
							}
						}
					}
					if let TypedExprKind::Identifier { path } = &ttarget.kind
						&& let ResolvedPathKind::Resolved(tgt_sym) = &path.kind
						&& let Some(Ty::Generic { name, bounds: _ }) = self.caches.env.get(*tgt_sym).cloned()
					{
						let value_satisfies_bounds = match &tvalue.ty {
							Ty::Generic { name: val_name, .. } if val_name == &name => true,
							Ty::Generic { .. } | Ty::SelfTy | Ty::Never | Ty::Infer => true,
							_ => self.expr_provably_returns_generic(&tvalue, &name),
						};
						if !value_satisfies_bounds {
							return Err(Self::err(
								*span,
								TypeErrorKind::TypeMismatch {
									expected: format!(
										"a value provably of type `{name}` (e.g. returned from a method on `{name}` that returns `Self`)"
									),
									found: self.fmt_ty(&tvalue.ty),
								},
							));
						}
					}
					if let TypedExprKind::Identifier { path } = &ttarget.kind
						&& let ResolvedPathKind::Resolved(tgt_sym) = &path.kind
						&& let Some(env_ty) = self.caches.env.get(*tgt_sym)
						&& matches!(env_ty, Ty::ImplTrait { concrete: None, .. })
						&& tvalue.ty != Ty::Infer
					{
						return Err(Self::err(
							*span,
							TypeErrorKind::TypeMismatch {
								expected: format!(
									"cannot assign to `impl Trait` parameter `{}` — \
										 its concrete type is unknown at this point",
									self.fmt_ty(&ttarget.ty)
								),
								found: self.fmt_ty(&tvalue.ty),
							},
						));
					}

					if ttarget.ty != Ty::Infer {
						if matches!(&ttarget.ty, Ty::Generic { .. })
							&& !matches!(&tvalue.ty, Ty::Generic { .. } | Ty::Infer | Ty::Never | Ty::SelfTy)
						{
							let generic_name = if let Ty::Generic { name, .. } = &ttarget.ty {
								name.clone()
							} else {
								unreachable!()
							};
							if !self.expr_provably_returns_generic(&tvalue, &generic_name) {
								return Err(Self::err(
									*span,
									TypeErrorKind::TypeMismatch {
										expected: format!(
											"a value provably of type `{generic_name}` (e.g. returned from a method on `{generic_name}` that returns `Self`)"
										),
										found: self.fmt_ty(&tvalue.ty),
									},
								));
							}
						}
						self.expect_ty(&tvalue.ty, &ttarget.ty, *span)?;
					}
					if ttarget.ty != Ty::Infer
						&& let ResolvedExpr::Identifier { path, .. } = value.clone()
						&& let ResolvedPathKind::Resolved(val_sym) = &path.kind
					{
						self.pin_infer_sym_notify(*val_sym, &ttarget.ty, &mut bis);
					}

					if tvalue.ty != Ty::Infer
						&& let ResolvedExpr::Identifier { path, .. } = target.clone()
						&& let ResolvedPathKind::Resolved(tgt_sym) = &path.kind
					{
						self.pin_infer_sym_notify(*tgt_sym, &tvalue.ty, &mut bis);
					}

					self.flush_newly_pinned(&mut bis, &mut bp);

					if let Some(trait_name) = assign_op_trait_method!(op)
						&& let Some(fn_sym) = self.op_trait_fn_sym(&ttarget.ty, trait_name)
					{
						let method_name = self.global.symbol(fn_sym).name.clone();
						let callee = TypedExpr {
							ty: Ty::Unit,
							span: *span,
							kind: TypedExprKind::Field {
								base: Box::new(ttarget),
								name: method_name,
							},
						};
						stmts.push(TypedStmt::Expr(TypedExpr {
							kind: TypedExprKind::Call {
								callee: Box::new(callee),
								call_type: CallType::Regular,
								named_generics: Vec::new(),
								args: vec![tvalue],
							},
							ty: Ty::Unit,
							span: *span,
						}));
					} else {
						stmts.push(TypedStmt::Assignment {
							target: ttarget,
							op: *op,
							value: tvalue,
							span: *span,
						});
					}
				}

				ResolvedStmt::If {
					cond,
					then_block,
					else_branch,
					span,
				} => {
					let tcond = self.check_expr(cond, Some(&Ty::Primitive(Primitive::Bool)))?;
					self.expect_ty(&tcond.ty, &Ty::Primitive(Primitive::Bool), *span)?;

					let tthen = self.check_block_inner(then_block, false)?;
					let telse = else_branch
						.as_ref()
						.map(|s| return self.check_stmt_with_bis(s, &mut bis, &mut bp))
						.transpose()?;
					self.flush_newly_pinned(&mut bis, &mut bp);
					stmts.push(TypedStmt::If {
						cond: tcond,
						then_block: tthen,
						else_branch: telse.map(Box::new),
						span: *span,
					});
				}

				ResolvedStmt::Loop { label, body, span } => {
					let tbody = self.check_block_inner(body, false)?;
					self.flush_newly_pinned(&mut bis, &mut bp);
					stmts.push(TypedStmt::Loop {
						label: label.clone(),
						body: tbody,
						span: *span,
					});
				}

				ResolvedStmt::Unsafe(b) => {
					let tb = self.check_block_inner(b, false)?;
					self.flush_newly_pinned(&mut bis, &mut bp);
					stmts.push(TypedStmt::Unsafe(tb));
				}
				ResolvedStmt::Block(b) => {
					let tb = self.check_block_inner(b, false)?;
					self.flush_newly_pinned(&mut bis, &mut bp);
					stmts.push(TypedStmt::Block(tb));
				}

				other => {
					let typed = self.check_stmt(other)?;
					self.flush_newly_pinned(&mut bis, &mut bp);
					stmts.push(typed);
				}
			}

			self.drain_backpatch_worklist(&mut bp, &mut stmts)?;
			self.drain_expr_worklist(&mut bis, &mut bp, &mut stmts, &mut tail)?;
			self.flush_newly_pinned(&mut bis, &mut bp);
		}

		if let Some(tail_expr) = &block.tail_expr {
			match self.check_expr(tail_expr, None) {
				Ok(te) if te.ty != Ty::Infer => {
					self.flush_newly_pinned(&mut bis, &mut bp);
					tail = Some(Box::new(te));
				}
				Ok(_) => {
					for sym in self.newly_pinned.drain(..) {
						bis.notify_sym(sym);
					}
				}
				Err(err) => match err.kind {
					TypeErrorKind::CannotInferType
					| TypeErrorKind::UnresolvedIdentifier { .. }
					| TypeErrorKind::UnresolvedAssocPath { .. } => {
						for sym in self.newly_pinned.drain(..) {
							bis.notify_sym(sym);
						}
					}
					_ => return Err(err),
				},
			}
		}

		self.finalize_block_inference(&mut bis, &mut bp, &mut stmts, &mut tail)?;

		for sym_id in bp.pending.keys() {
			self.infer_syms.remove(sym_id);
		}
		if let Some((_, decl)) = bp.pending.iter().next() {
			return Err(TypeError::new(decl.resolved.span, TypeErrorKind::CannotInferType));
		}

		debug_assert!(
			stmts.iter().all(|s| return !matches!(s, TypedStmt::Pending(_))),
			"BUG: TypedStmt::Pending survived finalize_block_inference"
		);

		let ty = if is_value {
			tail.as_ref().map_or_else(
				|| {
					return if stmts_always_diverge(&stmts) {
						Ty::Never
					} else {
						Ty::Unit
					};
				},
				|te| return te.ty.clone(),
			)
		} else if stmts_always_diverge(&stmts) {
			Ty::Never
		} else {
			Ty::Unit
		};

		return Ok(TypedBlock {
			stmts,
			tail_expr: tail,
			ty,
			span: block.span,
		});
	}

	fn check_stmt_with_bis(
		&mut self,
		stmt: &ResolvedStmt,
		bis: &mut BlockInferState<'_>,
		bp: &mut BackpatchState<'_>,
	) -> Result<TypedStmt, TypeError>
	{
		let result = self.check_stmt(stmt)?;
		self.flush_newly_pinned(bis, bp);
		return Ok(result);
	}

	fn expr_provably_returns_generic(&self, expr: &TypedExpr, generic_name: &str) -> bool
	{
		if matches!(&expr.ty, Ty::Generic { name, .. } if name == generic_name) {
			return true;
		}

		match &expr.kind {
			TypedExprKind::Identifier { .. } => {
				return matches!(&expr.ty, Ty::Generic { name, .. } if name == generic_name);
			}

			TypedExprKind::Call { callee, args, .. } => {
				if matches!(&callee.kind, TypedExprKind::InternalCall { .. }) {
					return false;
				}

				let lookup_fn_via_bounds = |base_sym: SymbolId, member: &str| -> Option<SymbolId> {
					if let Some(s) = self.caches.method_fn.get_sym(base_sym, member).copied() {
						return Some(s);
					}
					let generic_ty = self.caches.env.get(base_sym)?;
					let (Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. }) = generic_ty else {
						return None;
					};
					return bounds.iter().find_map(|b| {
						let TyBound::Trait { symbol: trait_sym, .. } = b else {
							return None;
						};
						return self.caches.method_fn.get_sym(*trait_sym, member).copied();
					});
				};

				match &callee.kind {
					TypedExprKind::Identifier { path } => match &path.kind {
						ResolvedPathKind::AssocItem { base, member, .. } => {
							let base_name = &self.global.symbol(*base).name;
							let base_is_target_generic = base_name == generic_name;

							let Some(fn_sym) = lookup_fn_via_bounds(*base, member) else {
								return false;
							};
							if !self.fn_return_is_self_or_generic(fn_sym, generic_name) {
								return false;
							}

							if base_is_target_generic {
								return true;
							}

							return args.first().is_some_and(|arg| match &arg.ty {
								Ty::Generic { name, .. } if name == generic_name => return true,
								Ty::Reference { inner, .. } | Ty::Mutable { inner } | Ty::Pointer { inner, .. } => {
									return matches!(inner.as_ref(), Ty::Generic { name, .. } if name == generic_name);
								}
								_ => return false,
							});
						}
						ResolvedPathKind::Resolved(id) => {
							return self.fn_return_is_self_or_generic(*id, generic_name);
						}
						ResolvedPathKind::Primitive(_) => return false,
					},

					TypedExprKind::Field { base, name } => {
						let receiver_is_generic = matches!(&base.ty, Ty::Generic { name, .. } if name == generic_name);
						if !receiver_is_generic {
							return false;
						}
						let fn_sym = match &base.ty {
							Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => {
								bounds.iter().find_map(|bound| {
									let TyBound::Trait { symbol: trait_sym, .. } = bound else {
										return None;
									};
									return self.caches.method_fn.get_sym(*trait_sym, name).copied();
								})
							}
							_ => None,
						};
						return fn_sym.is_some_and(|s| return self.fn_return_is_self_or_generic(s, generic_name));
					}

					_ => return false,
				}
			}

			TypedExprKind::Block(block) | TypedExprKind::UnsafeBlock(block) => {
				if let Some(tail) = &block.tail_expr {
					return self.expr_provably_returns_generic(tail, generic_name);
				}
				return false;
			}

			TypedExprKind::If {
				then_block,
				else_branch,
				..
			} => {
				let then_ok = then_block
					.tail_expr
					.as_ref()
					.is_some_and(|t| return self.expr_provably_returns_generic(t, generic_name));
				let else_ok = else_branch
					.as_ref()
					.is_some_and(|e| return self.expr_provably_returns_generic(e, generic_name));
				return then_ok && else_ok;
			}

			_ => return false,
		}
	}

	fn fn_return_is_self_or_generic(&self, fn_sym: SymbolId, generic_name: &str) -> bool
	{
		let Some(ret_ty) = self.caches.env.get(fn_sym) else {
			return false;
		};
		return match ret_ty {
			Ty::SelfTy => true,
			Ty::Generic { name, .. } if name == generic_name => true,

			Ty::Named { symbol, .. } => {
				let sym_name = &self.global.symbol(*symbol).name;
				sym_name == generic_name
			}
			_ => false,
		};
	}

	fn resolve_assoc_path(&self, base: SymbolId, member: &str, base_concrete: Option<&Ty>) -> (SymbolId, Vec<Ty>)
	{
		let actual = self.resolve_to_struct_sym(base);
		let item = self
			.caches
			.method_fn
			.get_sym(base, member)
			.or_else(|| return self.caches.method_fn.get_sym(actual, member))
			.copied()
			.or_else(|| {
				let generic_ty = self.caches.env.get(base).cloned()?;
				let bounds = match &generic_ty {
					Ty::Generic { bounds, .. } | Ty::ImplTrait { bounds, .. } => bounds.clone(),
					_ => return None,
				};
				return bounds.iter().find_map(|b| {
					let TyBound::Trait { symbol: trait_sym, .. } = b else {
						return None;
					};
					return self.caches.method_fn.get_sym(*trait_sym, member).copied();
				});
			})
			.unwrap_or(SymbolId::DUMMY);

		let base_type_args = match base_concrete {
			Some(Ty::Named { generics, .. }) => generics.clone(),
			_ => Vec::new(),
		};

		return (item, base_type_args);
	}

	fn finalize_assoc_in_path(&self, path: &mut ResolvedPath, base_concrete: Option<&Ty>)
	{
		if let ResolvedPathKind::AssocItem {
			base,
			member,
			item,
			base_type_args,
		} = &mut path.kind
		{
			if *item != SymbolId::DUMMY {
				return;
			}
			let (resolved, args) = self.resolve_assoc_path(*base, member, base_concrete);
			*item = resolved;
			*base_type_args = args;
		}
	}
}

fn stmts_always_diverge(stmts: &[TypedStmt]) -> bool
{
	for stmt in stmts {
		match stmt {
			TypedStmt::Return { .. }
			| TypedStmt::Break { .. }
			| TypedStmt::Continue { .. }
			| TypedStmt::Expr(TypedExpr { ty: Ty::Never, .. }) => {
				return true;
			}
			TypedStmt::If {
				then_block,
				else_branch,
				..
			} => {
				let then_diverges = stmts_always_diverge(&then_block.stmts) || then_block.ty == Ty::Never;
				let else_diverges = else_branch.as_ref().is_some_and(|e| {
					return match e.as_ref() {
						TypedStmt::Block(b) => stmts_always_diverge(&b.stmts) || b.ty == Ty::Never,
						TypedStmt::If { .. } => stmts_always_diverge(std::slice::from_ref(e)),
						TypedStmt::Return { .. } | TypedStmt::Break { .. } | TypedStmt::Continue { .. } => true,
						_ => false,
					};
				});
				if then_diverges && else_diverges {
					return true;
				}
			}
			_ => {}
		}
	}
	return false;
}

fn first_unresolved_generic(ty: &Ty) -> Option<&str>
{
	match ty {
		Ty::Generic { name, .. } => return Some(name.as_str()),

		Ty::Named { generics, .. } => {
			for g in generics {
				if let Some(n) = first_unresolved_generic(g) {
					return Some(n);
				}
			}
			return None;
		}

		Ty::Reference { inner, .. } | Ty::Mutable { inner } | Ty::Pointer { inner, .. } | Ty::Array { inner, .. } => {
			return first_unresolved_generic(inner);
		}

		Ty::Tuple(ts) => {
			for t in ts {
				if let Some(n) = first_unresolved_generic(t) {
					return Some(n);
				}
			}
			return None;
		}

		Ty::ImplTrait { bounds, .. } => {
			for bound in bounds {
				match bound {
					TyBound::Trait { args, .. } => {
						for arg in args {
							let inner_ty = match arg {
								TyGenericArg::Type(t) => t,
								TyGenericArg::Binding { ty, .. } => ty,
							};
							if let Some(n) = first_unresolved_generic(inner_ty) {
								return Some(n);
							}
						}
					}
					TyBound::Fn { args, ret } => {
						for a in args {
							if let Some(n) = first_unresolved_generic(a) {
								return Some(n);
							}
						}
						if let Some(n) = first_unresolved_generic(ret) {
							return Some(n);
						}
					}
				}
			}
			return None;
		}

		Ty::Infer => return Some("_"),

		_ => return None,
	}
}

fn resolve_ops_module(global: &GlobalSymbolTable, path: &[String]) -> HashMap<String, SymbolId>
{
	let mut map = HashMap::new();

	let (mut current_scope, start_idx) = 'outer: {
		for len in (1..=path.len()).rev() {
			if let Some(&scope_id) = global.module_roots.get(&path[..len]) {
				break 'outer (scope_id, len);
			}
		}
		return map;
	};

	for segment in &path[start_idx..] {
		let scope = global.scope(current_scope);
		let found = scope.symbols.iter().find_map(|&id| {
			let sym = global.symbol(id);
			if &sym.name == segment && matches!(sym.kind, SymbolKind::Module) {
				return sym.introduced_scope;
			}
			return None;
		});
		match found {
			Some(next_scope) => current_scope = next_scope,
			None => return map,
		}
	}

	for &id in &global.scope(current_scope).symbols {
		let sym = global.symbol(id);
		if matches!(sym.kind, SymbolKind::Trait) {
			map.insert(sym.name.clone(), id);
		}
	}

	return map;
}

fn resolve_trait_at_path(global: &GlobalSymbolTable, path: &[&str]) -> Option<SymbolId>
{
	let (trait_name, module_path) = path.split_last()?;

	let string_path: Vec<String> = module_path.iter().map(|s| return s.to_string()).collect();
	let (mut current_scope, start_idx) = 'outer: {
		for len in (1..=string_path.len()).rev() {
			if let Some(&scope_id) = global.module_roots.get(&string_path[..len]) {
				break 'outer (scope_id, len);
			}
		}
		return None;
	};

	for segment in &string_path[start_idx..] {
		let scope = global.scope(current_scope);
		let found = scope.symbols.iter().find_map(|&id| {
			let sym = global.symbol(id);
			if &sym.name == segment && matches!(sym.kind, SymbolKind::Module) {
				return sym.introduced_scope;
			}
			return None;
		});
		match found {
			Some(next_scope) => current_scope = next_scope,
			None => return None,
		}
	}

	let scope = global.scope(current_scope);
	return scope.symbols.iter().copied().find(|&id| {
		let sym = global.symbol(id);
		return sym.name == *trait_name && matches!(sym.kind, SymbolKind::Trait);
	});
}

pub fn check_types(
	module: &ResolvedModule,
	global: &GlobalSymbolTable,
	all_modules: &[ResolvedModule],
) -> Result<TypedModule, CompileError>
{
	let mut checker = Checker::new(global, module.ast.source_index);
	checker.traits.op_symbols = resolve_ops_module(global, &["std".to_string(), "ops".to_string()]);
	checker.traits.heap_syms = HeapTraitSyms {
		alloc: resolve_trait_at_path(global, ALLOC_TRAIT_PATH),
		io: resolve_trait_at_path(global, IO_TRAIT_PATH),
	};

	for m in all_modules {
		checker.source_index = m.ast.source_index;
		checker.scan_block(&m.ast.top_level_block).map_err(CompileError::Type)?;
	}

	checker.source_index = module.ast.source_index;
	let top = checker
		.check_block_tld(&module.ast.top_level_block)
		.map_err(CompileError::Type)?;

	checker.caches.trait_impls = checker.traits.impls;
	checker.caches.copy_sym = resolve_trait_at_path(global, COPY_TRAIT_PATH).unwrap_or_else(|| {
		todo!(
			"the Copy trait should be defined in the stdlib (make better error when the checker is switched to the new diagnostics system)"
		);
		// checker.diagnostics.push(compiler_bug!(
		// 	Span::default(),
		// 	"the Copy trait should be defined in the stdlib"
		// ));
	});

	return Ok(TypedModule {
		path: module.path.clone(),
		ast: TypedAST {
			top_level_block: top,
			source_index: module.ast.source_index,
			span: module.ast.span,
		},
		caches: checker.caches,
	});
}

fn substitute_generics(ty: &Ty, subs: &HashMap<String, Ty>) -> Ty
{
	return match ty {
		Ty::Generic { name, .. } => subs.get(name).cloned().unwrap_or_else(|| return ty.clone()),

		Ty::Named { symbol, generics } => Ty::Named {
			symbol: *symbol,
			generics: generics.iter().map(|g| return substitute_generics(g, subs)).collect(),
		},

		Ty::Reference { mutable, inner } => Ty::Reference {
			mutable: *mutable,
			inner: Box::new(substitute_generics(inner, subs)),
		},

		Ty::Mutable { inner } => Ty::Mutable {
			inner: Box::new(substitute_generics(inner, subs)),
		},

		Ty::Pointer { mutable, inner } => Ty::Pointer {
			mutable: *mutable,
			inner: Box::new(substitute_generics(inner, subs)),
		},

		Ty::Array { inner, size } => Ty::Array {
			inner: Box::new(substitute_generics(inner, subs)),
			size: *size,
		},

		Ty::Tuple(ts) => Ty::Tuple(ts.iter().map(|t| return substitute_generics(t, subs)).collect()),

		Ty::ImplTrait { bounds, .. } => Ty::ImplTrait {
			bounds: bounds
				.iter()
				.map(|b| return substitute_generic_bound(b, subs))
				.collect(),
			concrete: None,
		},

		other => other.clone(),
	};
}

fn substitute_self(ty: &Ty, concrete: &Ty) -> Ty
{
	return match ty {
		Ty::SelfTy => concrete.clone(),
		Ty::Generic { name, .. } if name == "Self" => concrete.clone(),

		Ty::Named { symbol, generics } => Ty::Named {
			symbol: *symbol,
			generics: generics.iter().map(|g| return substitute_self(g, concrete)).collect(),
		},
		Ty::Reference { mutable, inner } => Ty::Reference {
			mutable: *mutable,
			inner: Box::new(substitute_self(inner, concrete)),
		},
		Ty::Mutable { inner } => Ty::Mutable {
			inner: Box::new(substitute_self(inner, concrete)),
		},
		Ty::Pointer { mutable, inner } => Ty::Pointer {
			mutable: *mutable,
			inner: Box::new(substitute_self(inner, concrete)),
		},
		Ty::Array { inner, size } => Ty::Array {
			inner: Box::new(substitute_self(inner, concrete)),
			size: *size,
		},
		Ty::Tuple(ts) => Ty::Tuple(ts.iter().map(|t| return substitute_self(t, concrete)).collect()),
		Ty::ImplTrait { bounds, .. } => Ty::ImplTrait {
			bounds: bounds
				.iter()
				.map(|b| return substitute_self_in_bound(b, concrete))
				.collect(),
			concrete: None,
		},
		other => other.clone(),
	};
}

fn substitute_self_in_bound(bound: &TyBound, concrete: &Ty) -> TyBound
{
	return match bound {
		TyBound::Trait { symbol, args } => TyBound::Trait {
			symbol: *symbol,
			args: args
				.iter()
				.map(|a| {
					return match a {
						TyGenericArg::Type(t) => TyGenericArg::Type(substitute_self(t, concrete)),
						TyGenericArg::Binding { name, ty } => TyGenericArg::Binding {
							name: name.clone(),
							ty: substitute_self(ty, concrete),
						},
					};
				})
				.collect(),
		},
		TyBound::Fn { args, ret } => TyBound::Fn {
			args: args.iter().map(|t| return substitute_self(t, concrete)).collect(),
			ret: Box::new(substitute_self(ret, concrete)),
		},
	};
}

fn extract_hint_generics(ret_ty: &Ty, hint: &Ty, subs: &mut HashMap<String, Ty>)
{
	match (ret_ty, hint) {
		(Ty::Generic { name, .. }, concrete) if !matches!(concrete, Ty::Generic { .. } | Ty::Infer) => {
			subs.entry(name.clone()).or_insert_with(|| return concrete.clone());
		}
		(
			Ty::Named {
				symbol: s1,
				generics: g1,
			},
			Ty::Named {
				symbol: s2,
				generics: g2,
			},
		) if s1 == s2 && g1.len() == g2.len() => {
			for (a, b) in g1.iter().zip(g2.iter()) {
				extract_hint_generics(a, b, subs);
			}
		}
		(Ty::Reference { inner: i1, .. }, Ty::Reference { inner: i2, .. })
		| (Ty::Mutable { inner: i1 }, Ty::Mutable { inner: i2 })
		| (Ty::Pointer { inner: i1, .. }, Ty::Pointer { inner: i2, .. })
		| (Ty::Array { inner: i1, .. }, Ty::Array { inner: i2, .. }) => {
			extract_hint_generics(i1, i2, subs);
		}
		(Ty::Tuple(ts1), Ty::Tuple(ts2)) if ts1.len() == ts2.len() => {
			for (a, b) in ts1.iter().zip(ts2.iter()) {
				extract_hint_generics(a, b, subs);
			}
		}
		_ => {}
	}
}

fn substitute_generic_bound(bound: &TyBound, subs: &HashMap<String, Ty>) -> TyBound
{
	return match bound {
		TyBound::Trait { symbol, args } => TyBound::Trait {
			symbol: *symbol,
			args: args
				.iter()
				.map(|a| {
					return match a {
						TyGenericArg::Type(ty) => TyGenericArg::Type(substitute_generics(ty, subs)),
						TyGenericArg::Binding { name, ty } => TyGenericArg::Binding {
							name: name.clone(),
							ty: substitute_generics(ty, subs),
						},
					};
				})
				.collect(),
		},
		TyBound::Fn { args, ret } => TyBound::Fn {
			args: args.iter().map(|t| return substitute_generics(t, subs)).collect(),
			ret: Box::new(substitute_generics(ret, subs)),
		},
	};
}

fn lower_int_ty(name: &str, _scope: &GenericScope) -> Option<Ty>
{
	let (sign, rest) = if let Some(n) = name.strip_prefix('i') {
		(IntSign::Signed, n)
	} else if let Some(n) = name.strip_prefix('u') {
		(IntSign::Unsigned, n)
	} else {
		return None;
	};
	let width = if let Ok(n) = rest.parse::<u16>() {
		IntSize::Fixed(n)
	} else if !rest.is_empty() {
		if rest == "size" {
			IntSize::Size
		} else {
			return None;
		}
	} else {
		return None;
	};
	return Some(Ty::Primitive(Primitive::Int(IntType { sign, bits: width })));
}

fn is_builtin_modifier(modifiers: &[parser::Modifier]) -> bool
{
	return modifiers.iter().any(|m| {
		return matches!(
			m,
			parser::Modifier::Directive(parser::Directive::Custom { name, .. })
				if name == "builtin"
		);
	});
}

fn collect_loop_break_ty(block: &TypedBlock, loop_label: &str) -> Option<Ty>
{
	let mut out: Option<Ty> = None;
	collect_breaks_in_stmts(&block.stmts, loop_label, &mut out);
	if let Some(tail) = &block.tail_expr {
		collect_breaks_in_expr(tail, loop_label, &mut out);
	}
	return out;
}

fn merge_break_ty(out: &mut Option<Ty>, candidate: &Ty)
{
	match out {
		None => *out = Some(candidate.clone()),
		Some(Ty::Never) if !matches!(candidate, Ty::Never) => *out = Some(candidate.clone()),
		_ => {}
	}
}

fn collect_breaks_in_stmts(stmts: &[TypedStmt], loop_label: &str, out: &mut Option<Ty>)
{
	for s in stmts {
		match s {
			TypedStmt::Break {
				label, value: Some(v), ..
			} if label == loop_label => {
				merge_break_ty(out, &v.ty);
			}

			TypedStmt::Loop { label, .. } if label == loop_label => {}

			TypedStmt::Loop { body, .. } => {
				collect_breaks_in_stmts(&body.stmts, loop_label, out);
				if let Some(t) = &body.tail_expr {
					collect_breaks_in_expr(t, loop_label, out);
				}
			}

			TypedStmt::If {
				then_block,
				else_branch,
				..
			} => {
				collect_breaks_in_stmts(&then_block.stmts, loop_label, out);
				if let Some(t) = &then_block.tail_expr {
					collect_breaks_in_expr(t, loop_label, out);
				}
				if let Some(eb) = else_branch {
					collect_breaks_in_stmts(std::slice::from_ref(eb.as_ref()), loop_label, out);
				}
			}

			TypedStmt::Block(b) | TypedStmt::Unsafe(b) => {
				collect_breaks_in_stmts(&b.stmts, loop_label, out);
				if let Some(t) = &b.tail_expr {
					collect_breaks_in_expr(t, loop_label, out);
				}
			}

			TypedStmt::Expr(e) => collect_breaks_in_expr(e, loop_label, out),

			TypedStmt::VariableDecl(v) => {
				if let Some(init) = &v.init {
					collect_breaks_in_expr(init, loop_label, out);
				}
			}
			TypedStmt::Assignment { target, value, .. } => {
				collect_breaks_in_expr(target, loop_label, out);
				collect_breaks_in_expr(value, loop_label, out);
			}
			TypedStmt::Return { value: Some(v), .. } => {
				collect_breaks_in_expr(v, loop_label, out);
			}
			TypedStmt::Delete { expr, .. } => collect_breaks_in_expr(expr, loop_label, out),

			TypedStmt::Return { value: None, .. }
			| TypedStmt::Break { .. }
			| TypedStmt::Continue { .. }
			| TypedStmt::Directive(_)
			| TypedStmt::Pending(_) => {}
		}
	}
}

fn collect_breaks_in_expr(expr: &TypedExpr, loop_label: &str, out: &mut Option<Ty>)
{
	match &expr.kind {
		TypedExprKind::Block(b) | TypedExprKind::UnsafeBlock(b) => {
			collect_breaks_in_stmts(&b.stmts, loop_label, out);
			if let Some(t) = &b.tail_expr {
				collect_breaks_in_expr(t, loop_label, out);
			}
		}

		TypedExprKind::If {
			then_block,
			else_branch,
			..
		} => {
			collect_breaks_in_stmts(&then_block.stmts, loop_label, out);
			if let Some(t) = &then_block.tail_expr {
				collect_breaks_in_expr(t, loop_label, out);
			}
			if let Some(e) = else_branch {
				collect_breaks_in_expr(e, loop_label, out);
			}
		}

		TypedExprKind::Loop { label, .. } if label == loop_label => {
			// Shadowed by inner loop with the same label.
		}
		TypedExprKind::Loop { body, .. } => {
			collect_breaks_in_stmts(&body.stmts, loop_label, out);
			if let Some(t) = &body.tail_expr {
				collect_breaks_in_expr(t, loop_label, out);
			}
		}

		TypedExprKind::Switch { arms, .. } => {
			for arm in arms {
				match &arm.body {
					TypedSwitchBody::Expr(e) => collect_breaks_in_expr(e, loop_label, out),
					TypedSwitchBody::Block(b) => {
						collect_breaks_in_stmts(&b.stmts, loop_label, out);
						if let Some(t) = &b.tail_expr {
							collect_breaks_in_expr(t, loop_label, out);
						}
					}
				}
			}
		}

		_ => {}
	}
}
