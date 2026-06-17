#![allow(unused)]

// TODO: remove all `Stmt::Delete`

#[cfg(test)]
#[path = "../../tests/monomorphization/tests.rs"]
mod tests;

mod display;

use std::{
	collections::{HashMap, HashSet, VecDeque},
	fmt::Write,
};

use leaf_proc::{CompileErrorKind, Spanned, compiler_bug};

use crate::{
	CompileDiagnostic,
	diagnostics::{CompileError, DiagnosticBuilder, ErrorCode},
	lexer::{IntBase, IntSign, IntSize, IntType, Span, Spanned},
	mir::{
		BlockId, ConstBodyId, LocalId, MirAggregateKind, MirBasicBlock, MirBody, MirCallee, MirConstBody, MirFunction,
		MirGlobal, MirItem, MirLiteral, MirLiteralValue, MirLocal, MirModule, MirOperand, MirParam, MirPlace,
		MirPlaceBase, MirProjection, MirRvalue, MirStmt, MirSwitchArm, MirTerminator, MirTypeDef, MirTypeDefKind,
	},
	parser::{self, BinaryOp, UnaryOp},
	symbol_collection::{GlobalSymbolTable, SymbolId},
	type_analysis::{Primitive, Ty, TyBound, TyKey},
};

#[derive(Debug, Clone, Spanned)]
pub struct MonoError
{
	pub span: Span,
	pub kind: MonoErrorKind,
}

#[derive(Debug, Clone, CompileErrorKind)]
#[compile_error_variant(CompileError::Mono)]
pub enum MonoErrorKind
{
	#[error_msg("couldn't find a main function")]
	#[error_code(ErrorCode::MonoNoMainEntry)]
	NoMainEntry,

	#[error_msg("cannot resolve trait method `{name}` to a concrete implementation")]
	#[error_code(ErrorCode::MonoAbstractTraitMethodCall)]
	AbstractTraitMethodCall
	{
		name: String
	},
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MonoTy
{
	Primitive(Primitive),
	Named
	{
		symbol: SymbolId,
		type_args: Vec<MonoTy>,
		mangled_name: String,
	},
	Reference
	{
		mutable: bool,
		inner: Box<MonoTy>,
	},
	Pointer
	{
		mutable: bool,
		inner: Box<MonoTy>,
	},
	Array
	{
		inner: Box<MonoTy>,
		size: Option<u64>,
	},
	Tuple(Vec<MonoTy>),
}

impl MonoTy
{
	pub const fn unit() -> Self
	{
		return MonoTy::Tuple(Vec::new());
	}

	/// Determines whether this type is zero-sized.
	///
	/// `typedefs` maps a monomorphized type instance `(symbol, type_args)` to its
	/// lowered `MonoTypeDefKind`, used to recurse into struct/union/variant fields.
	/// If an instance isn't present in the map (e.g. not yet monomorphized, or a
	/// recursive type), it's conservatively treated as non-ZST.
	///
	/// Enums with at most one variant, and single-member variants whose member is
	/// either absent or itself a ZST, are considered ZST.
	pub fn is_zst(&self, typedefs: &HashMap<(SymbolId, Vec<MonoTy>), MonoTypeDefKind>) -> bool
	{
		return match self {
			MonoTy::Tuple(elems) => elems.is_empty() || elems.iter().all(|t| return t.is_zst(typedefs)),

			MonoTy::Array { size: Some(0), .. } => true,
			MonoTy::Array {
				size: Some(_), inner, ..
			} => inner.is_zst(typedefs),
			MonoTy::Array { size: None, .. }
			| MonoTy::Reference { .. }
			| MonoTy::Pointer { .. }
			| MonoTy::Primitive(_) => false,

			MonoTy::Named { symbol, type_args, .. } => {
				let key = (*symbol, type_args.clone());
				match typedefs.get(&key) {
					None => false,
					Some(MonoTypeDefKind::Struct { fields } | MonoTypeDefKind::Union { fields }) => {
						fields.iter().all(|(_, t)| return t.is_zst(typedefs))
					}
					Some(MonoTypeDefKind::Enum { variants }) => variants.len() <= 1,
					Some(MonoTypeDefKind::Variant { members }) => {
						members.len() == 1 && members[0].1.as_ref().is_none_or(|t| return t.is_zst(typedefs))
					}
					Some(MonoTypeDefKind::TypeAlias { ty }) => ty.is_zst(typedefs),
				}
			}
		};
	}
}

#[derive(Debug, Clone)]
pub struct MonoModule
{
	pub path: Vec<String>,
	pub items: Vec<MonoItem>,

	pub const_bodies: Vec<MonoConstBody>,

	pub entry: String,

	pub option_symbol: Option<SymbolId>,
}

#[derive(Debug, Clone)]
pub enum MonoItem
{
	Function(MonoFunction),
	Global(MonoGlobal),
	TypeDef(MonoTypeDef),
}

#[derive(Debug, Clone, Spanned)]
pub struct MonoFunction
{
	pub symbol: SymbolId,
	pub type_args: Vec<MonoTy>,
	pub mangled_name: String,
	pub params: Vec<MonoParam>,
	pub return_ty: Option<MonoTy>,
	pub body: Option<MonoBody>,
	pub modifiers: Vec<parser::Modifier>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MonoParam
{
	pub local: LocalId,
	pub name: String,
	pub ty: MonoTy,
	pub mutable: bool,
}

#[derive(Debug, Clone, Spanned)]
pub struct MonoGlobal
{
	pub symbol: SymbolId,
	pub mangled_name: String,
	pub ty: MonoTy,
	pub init: ConstBodyId,
	pub mutable: bool,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MonoTypeDef
{
	pub symbol: SymbolId,
	pub type_args: Vec<MonoTy>,
	pub mangled_name: String,
	pub kind: MonoTypeDefKind,
	pub span: Span,
}

impl MonoTypeDef
{
	pub fn is_option_variant(&self, option_symbol: Option<SymbolId>) -> bool
	{
		let Some(noption_symbol) = option_symbol else {
			return false;
		};
		return self.symbol == noption_symbol && matches!(self.kind, MonoTypeDefKind::Variant { .. });
	}
}

#[derive(Debug, Clone)]
pub enum MonoTypeDefKind
{
	Struct
	{
		fields: Vec<(String, MonoTy)>
	},
	Union
	{
		fields: Vec<(String, MonoTy)>
	},
	Enum
	{
		variants: Vec<(String, Option<ConstBodyId>)>,
	},
	Variant
	{
		members: Vec<(String, Option<MonoTy>)>
	},
	TypeAlias
	{
		ty: MonoTy
	},
}

#[derive(Debug, Clone)]
pub struct MonoConstBody
{
	pub body: MonoBody,
	pub result: LocalId,
}

#[derive(Debug, Clone)]
pub struct MonoBody
{
	pub locals: Vec<MonoLocal>,
	pub param_count: usize,
	pub blocks: Vec<MonoBasicBlock>,
	pub return_local: Option<LocalId>,
}

#[derive(Debug, Clone, Spanned)]
pub struct MonoLocal
{
	pub id: LocalId,
	pub ty: MonoTy,
	pub name: Option<String>,
	pub mutable: bool,
	pub is_temp: bool,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MonoBasicBlock
{
	pub id: BlockId,
	pub stmts: Vec<MonoStmt>,
	pub terminator: MonoTerminator,
}

#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum MonoStmt
{
	Assign
	{
		place: MonoPlace,
		rvalue: MonoRvalue,
		span: Span,
	},
	Call
	{
		callee: MonoCallee,
		args: Vec<MonoOperand>,
		span: Span,
	},
	Delete
	{
		operand: MonoOperand,
		span: Span,
	},
	Nop,
}

#[derive(Debug, Clone)]
pub enum MonoTerminator
{
	Goto
	{
		target: BlockId,
	},
	Branch
	{
		cond: MonoOperand,
		then_block: BlockId,
		else_block: BlockId,
	},
	CallAndContinue
	{
		callee: MonoCallee,
		args: Vec<MonoOperand>,
		dest: MonoPlace,
		next: BlockId,
		unwind: Option<BlockId>,
		span: Span,
	},
	Return,
	Unreachable,
	Switch
	{
		scrutinee: MonoOperand,
		arms: Vec<MonoSwitchArm>,
		otherwise: BlockId,
	},
}

#[derive(Debug, Clone)]
pub struct MonoSwitchArm
{
	pub value: MonoOperand,
	pub target: BlockId,
}

#[derive(Debug, Clone)]
pub struct MonoPlace
{
	pub base: MonoPlaceBase,
	pub projections: Vec<MonoProjection>,
	pub ty: MonoTy,
}

#[derive(Debug, Clone)]
pub enum MonoPlaceBase
{
	Local(LocalId),
	Global(SymbolId),
}

#[derive(Debug, Clone)]
pub enum MonoProjection
{
	Field
	{
		name: String,
		ty: MonoTy,
	},
	Index
	{
		index: LocalId,
		ty: MonoTy,
	},
	Deref,
}

#[derive(Debug, Clone)]
pub enum MonoOperand
{
	Copy(MonoPlace),
	Move(MonoPlace),
	Const(MonoLiteral),
}

impl MonoOperand
{
	pub const fn ty(&self) -> &MonoTy
	{
		match self {
			MonoOperand::Copy(p) | MonoOperand::Move(p) => return &p.ty,
			MonoOperand::Const(c) => return &c.ty,
		}
	}
}

#[derive(Debug, Clone)]
pub struct MonoLiteral
{
	pub value: MirLiteralValue,
	pub ty: MonoTy,
}

#[derive(Debug, Clone)]
pub enum MonoRvalue
{
	Use(MonoOperand),
	Unary
	{
		op: UnaryOp,
		operand: MonoOperand,
	},
	Binary
	{
		op: BinaryOp,
		lhs: MonoOperand,
		rhs: MonoOperand,
	},
	Cast
	{
		ty: MonoTy,
		operand: MonoOperand,
	},
	Ref
	{
		mutable: bool,
		place: MonoPlace,
	},
	RawPtr
	{
		mutable: bool,
		place: MonoPlace,
	},
	Aggregate
	{
		kind: MonoAggregateKind,
		fields: Vec<(String, MonoOperand)>,
	},
	Array
	{
		elements: Vec<MonoOperand>,
		elem_ty: MonoTy,
	},
	ArrayRepeat
	{
		value: MonoOperand,
		count: ConstBodyId,
		elem_ty: MonoTy,
	},
	Tuple(Vec<MonoOperand>),
	Discriminant(MonoPlace),
}

#[derive(Debug, Clone)]
pub enum MonoAggregateKind
{
	Struct
	{
		symbol: SymbolId,
		mangled_name: String,
	},
	Union
	{
		symbol: SymbolId,
		mangled_name: String,
	},
	VariantMember
	{
		parent: SymbolId,
		parent_mangled: String,
		member: String,
	},
	Tuple,
}

#[derive(Debug, Clone)]
pub enum MonoCallee
{
	Direct
	{
		symbol: SymbolId,
		type_args: Vec<MonoTy>,
		mangled_name: String,
	},
	Indirect(LocalId),
	Intrinsic(crate::type_analysis::intrinsics::Intrinsic),
}

type Instance = (SymbolId, Vec<MonoTy>, Vec<(String, MonoTy)>);

type ConstBodyKey = (usize, u32);

#[derive(Copy, Clone)]
enum ItemRef<'a>
{
	Function(usize, &'a MirFunction),
	Global(usize, &'a MirGlobal),
	TypeDef(usize, &'a MirTypeDef),
}

struct Monomorphizer<'a>
{
	global: &'a GlobalSymbolTable,
	modules: &'a [MirModule],
	diagnostics: Vec<DiagnosticBuilder>,

	functions: HashMap<SymbolId, (usize, usize)>,
	globals: HashMap<SymbolId, (usize, usize)>,
	typedefs: HashMap<SymbolId, (usize, usize)>,

	reachable_fns: HashSet<Instance>,
	reachable_types: HashSet<Instance>,
	reachable_globals: HashSet<SymbolId>,
	fn_worklist: VecDeque<Instance>,
	type_worklist: VecDeque<Instance>,

	const_body_map: HashMap<ConstBodyKey, ConstBodyId>,
	out_const_bodies: Vec<Option<MonoConstBody>>,

	fn_name_cache: HashMap<Instance, String>,
	type_name_cache: HashMap<Instance, String>,

	out_functions: Vec<MonoFunction>,
	out_globals: Vec<MonoGlobal>,
	out_typedefs: Vec<MonoTypeDef>,

	/// Lowered `MonoTypeDefKind` for every monomorphized type instance, keyed by
	/// `(symbol, type_args)`. Populated incrementally as types are monomorphized,
	/// so that [`MonoTy::is_zst`] can be queried during body lowering.
	typedef_kinds: HashMap<(SymbolId, Vec<MonoTy>), MonoTypeDefKind>,

	method_dispatch: HashMap<(TyKey, String), SymbolId>,
}

type Subst = HashMap<String, MonoTy>;

impl<'a> Monomorphizer<'a>
{
	fn new(global: &'a GlobalSymbolTable, modules: &'a [MirModule]) -> Self
	{
		let mut functions = HashMap::new();
		let mut globals = HashMap::new();
		let mut typedefs = HashMap::new();

		for (mi, m) in modules.iter().enumerate() {
			for (ii, item) in m.items.iter().enumerate() {
				match item {
					MirItem::Function(f) => {
						functions.insert(f.symbol, (mi, ii));
					}
					MirItem::Global(g) => {
						globals.insert(g.symbol, (mi, ii));
					}
					MirItem::TypeDef(t) => {
						typedefs.insert(t.symbol, (mi, ii));
					}
				}
			}
		}

		let mut method_dispatch: HashMap<(TyKey, String), SymbolId> = HashMap::new();
		for m in modules {
			for (k, &v) in &m.method_dispatch {
				method_dispatch.entry(k.clone()).or_insert(v);
			}
		}

		return Self {
			global,
			modules,
			diagnostics: Vec::new(),
			functions,
			globals,
			typedefs,
			reachable_fns: HashSet::new(),
			reachable_types: HashSet::new(),
			reachable_globals: HashSet::new(),
			fn_worklist: VecDeque::new(),
			type_worklist: VecDeque::new(),
			const_body_map: HashMap::new(),
			out_const_bodies: Vec::new(),
			fn_name_cache: HashMap::new(),
			type_name_cache: HashMap::new(),
			out_functions: Vec::new(),
			out_globals: Vec::new(),
			out_typedefs: Vec::new(),
			typedef_kinds: HashMap::new(),
			method_dispatch,
		};
	}

	/// Returns whether `ty` is zero-sized, using the typedefs monomorphized so far.
	///
	/// Because type monomorphization and function body lowering are interleaved
	/// (see [`Monomorphizer::run`]), a `Named` type reached during lowering may not
	/// yet have an entry in `typedef_kinds` if it hasn't been processed off the
	/// type worklist. In that case this conservatively returns `false`.
	fn is_zst(&self, ty: &MonoTy) -> bool
	{
		return ty.is_zst(&self.typedef_kinds);
	}

	fn lookup_item(&self, sym: SymbolId) -> Option<ItemRef<'a>>
	{
		if let Some(&(mi, ii)) = self.functions.get(&sym)
			&& let MirItem::Function(f) = &self.modules[mi].items[ii]
		{
			return Some(ItemRef::Function(mi, f));
		}
		if let Some(&(mi, ii)) = self.globals.get(&sym)
			&& let MirItem::Global(g) = &self.modules[mi].items[ii]
		{
			return Some(ItemRef::Global(mi, g));
		}
		if let Some(&(mi, ii)) = self.typedefs.get(&sym)
			&& let MirItem::TypeDef(t) = &self.modules[mi].items[ii]
		{
			return Some(ItemRef::TypeDef(mi, t));
		}
		return None;
	}

	fn resolve_module_path(&self, path: &[&str]) -> Option<SymbolId>
	{
		let (item_name, mod_segments) = path.split_last()?;
		let string_path: Vec<String> = mod_segments.iter().map(|s| return s.to_string()).collect();

		let (mut current_scope, start_idx) = {
			let mut found = None;
			for len in (1..=string_path.len()).rev() {
				if let Some(&scope_id) = self.global.module_roots.get(&string_path[..len]) {
					found = Some((scope_id, len));
					break;
				}
			}
			found?
		};

		for segment in &string_path[start_idx..] {
			let scope = self.global.scope(current_scope);
			let next = scope.symbols.iter().find_map(|&id| {
				let sym = self.global.symbol(id);
				return (sym.name == *segment).then(|| return sym.introduced_scope).flatten();
			})?;
			current_scope = next;
		}

		let scope = self.global.scope(current_scope);
		return scope.symbols.iter().copied().find(|&id| {
			return &self.global.symbol(id).name == item_name;
		});
	}

	fn dispatch_key(&self, ty: &Ty, subst: &Subst) -> Option<TyKey>
	{
		return match ty {
			Ty::Named { symbol, .. } => Some(TyKey::Symbol(*symbol)),
			Ty::Primitive(p) => Some(TyKey::Prim(p.clone())),
			Ty::Reference { inner, .. } | Ty::Pointer { inner, .. } | Ty::Mutable { inner } => {
				self.dispatch_key(inner, subst)
			}
			Ty::Generic { name, .. } => subst.get(name.as_str()).and_then(|t| return Self::dispatch_key_mono(t)),
			Ty::ImplTrait { concrete: Some(c), .. } => self.dispatch_key(c, subst),

			Ty::ImplTrait { concrete: None, bounds } => {
				for b in bounds {
					if let TyBound::Trait { symbol, .. } = b {
						let hp_name = match self.global.symbol(*symbol).name.as_str() {
							"Alloc" => Some("alloc"),
							"IO" => Some("io"),
							_ => None,
						};
						if let Some(name) = hp_name
							&& let Some(concrete) = subst.get(name)
						{
							return Self::dispatch_key_mono(concrete);
						}
					}
				}
				None
			}

			_ => None,
		};
	}

	fn dispatch_key_mono(ty: &MonoTy) -> Option<TyKey>
	{
		return match ty {
			MonoTy::Named { symbol, .. } => Some(TyKey::Symbol(*symbol)),
			MonoTy::Primitive(p) => Some(TyKey::Prim(p.clone())),
			MonoTy::Reference { inner, .. } | MonoTy::Pointer { inner, .. } => Self::dispatch_key_mono(inner),
			_ => None,
		};
	}

	fn sanitize(name: &str) -> String
	{
		return name
			.chars()
			.map(|c| {
				return if c.is_ascii_alphanumeric() {
					c.to_string()
				} else if c == '#' {
					"leaf_internal__".to_string()
				} else {
					"_".to_string()
				};
			})
			.collect();
	}

	fn mangle_primitive(p: &Primitive) -> String
	{
		use crate::lexer::{IntSign, IntSize, IntType};

		return match p {
			Primitive::Int(IntType { bits, sign }) => {
				let prefix = match sign {
					IntSign::Signed => "i",
					IntSign::Unsigned => "u",
				};
				let width = match bits {
					IntSize::Fixed(n) => n.to_string(),
					IntSize::Size => {
						return match sign {
							IntSign::Signed => "isize".to_string(),
							IntSign::Unsigned => "usize".to_string(),
						};
					}
				};
				format!("{prefix}{width}")
			}
			Primitive::F32 => "f32".to_string(),
			Primitive::F64 => "f64".to_string(),
			Primitive::Bool => "bool".to_string(),
			Primitive::Char => "char".to_string(),
			Primitive::Str => "str".to_string(),
			Primitive::CStr => "cstr".to_string(),
		};
	}

	fn mangle_ty(ty: &MonoTy) -> String
	{
		return match ty {
			MonoTy::Primitive(p) => Self::mangle_primitive(p),
			MonoTy::Named { mangled_name, .. } => mangled_name.clone(),
			MonoTy::Reference { mutable, inner } => {
				format!("ref{}_{}", if *mutable { "mut" } else { "" }, Self::mangle_ty(inner))
			}
			MonoTy::Pointer { mutable, inner } => {
				format!("ptr{}_{}", if *mutable { "mut" } else { "" }, Self::mangle_ty(inner))
			}
			MonoTy::Array { inner, size } => format!(
				"arr{}_{}",
				size.map_or_else(|| return "dyn".to_string(), |s| return s.to_string()),
				Self::mangle_ty(inner)
			),
			MonoTy::Tuple(elems) => {
				if elems.is_empty() {
					return "unit".to_string();
				}
				let parts: Vec<String> = elems.iter().map(Self::mangle_ty).collect();
				format!("tup{}_{}", parts.len(), parts.join("_"))
			}
		};
	}

	fn mangle_with_args(path: &[String], name: &str, type_args: &[MonoTy], heap_bindings: &[(String, MonoTy)])
	-> String
	{
		let mut out = path
			.iter()
			.map(|s| return Self::sanitize(s))
			.collect::<Vec<_>>()
			.join("_");
		if !out.is_empty() {
			out.push('_');
		}
		out.push_str(&Self::sanitize(name));

		if !type_args.is_empty() {
			out.push_str("__");
			for t in type_args {
				let m = Self::mangle_ty(t);
				let _ = write!(out, "{}{}", m.len(), m);
			}
		}

		if !heap_bindings.is_empty() {
			let mut sorted: Vec<&(String, MonoTy)> = heap_bindings.iter().collect();
			sorted.sort_by(|a, b| return a.0.cmp(&b.0));

			out.push_str("__h");
			for (n, t) in sorted {
				out.push('_');
				out.push_str(&Self::sanitize(n));
				out.push('_');
				out.push_str(&Self::mangle_ty(t));
			}
		}

		return out;
	}

	fn fn_mangled_name(&mut self, sym: SymbolId, type_args: &[MonoTy], heap_bindings: &[(String, MonoTy)]) -> String
	{
		if let Some(ItemRef::Function(_, f)) = self.lookup_item(sym) {
			if let Some(explicit) = Self::explicit_mangle_name(&f.modifiers) {
				if type_args.is_empty() && heap_bindings.is_empty() {
					return explicit;
				}
				todo!("for now, `@mangle_name()` can't have any generics")
			}

			let is_extern_c = f
				.modifiers
				.iter()
				.any(|m| matches!(m, parser::Modifier::Extern(Some(parser::ExternLanguage::C))));
			if is_extern_c {
				todo!("write a good error, but an extern(C) function should always have a `@mangle_name()`")
			}
		}
		let mut sorted_hb: Vec<(String, MonoTy)> = heap_bindings.to_vec();
		sorted_hb.sort_by(|a, b| return a.0.cmp(&b.0));

		let key: Instance = (sym, type_args.to_vec(), sorted_hb.clone());
		if let Some(n) = self.fn_name_cache.get(&key) {
			return n.clone();
		}
		let scope_path = self.global.scope_path(sym);
		let name = self.global.symbol(sym).name.clone();
		let mangled = Self::mangle_with_args(&scope_path, &name, type_args, &sorted_hb);
		self.fn_name_cache.insert(key, mangled.clone());
		return mangled;
	}

	fn type_mangled_name(&mut self, sym: SymbolId, type_args: &[MonoTy]) -> String
	{
		let key: Instance = (sym, type_args.to_vec(), Vec::new());
		if let Some(n) = self.type_name_cache.get(&key) {
			return n.clone();
		}
		let scope_path = self.global.scope_path(sym);
		let name = self.global.symbol(sym).name.clone();
		let mangled = Self::mangle_with_args(&scope_path, &name, type_args, &[]);
		self.type_name_cache.insert(key, mangled.clone());
		return mangled;
	}

	fn explicit_mangle_name(modifiers: &[parser::Modifier]) -> Option<String>
	{
		for m in modifiers {
			if let parser::Modifier::Directive(parser::Directive::MangleName { name }) = m {
				return Some(name.clone());
			}
		}
		return None;
	}

	fn build_subst(
		&mut self,
		f: &MirFunction,
		type_args: &[MonoTy],
		heap_bindings: &[(String, MonoTy)],
		span: Span,
	) -> Subst
	{
		let mut subst = Subst::new();

		if type_args.len() > f.generics.len() {
			self.diagnostics.push(compiler_bug!(
				span,
				"function `{}` instantiated with too many type arguments ({} declared, {} provided)",
				f.name,
				f.generics.len(),
				type_args.len(),
			));
		}
		for ((ident, _), ty) in f.generics.iter().zip(type_args.iter()) {
			subst.insert(ident.clone(), ty.clone());
		}

		for hp in &f.heap_generics {
			if let Some((_, ty)) = heap_bindings.iter().find(|(n, _)| return n == &hp.name) {
				subst.insert(hp.name.clone(), ty.clone());
			} else if let Some(default) = self.default_for_heap_param(&hp.name) {
				subst.insert(hp.name.clone(), default);
			}
		}

		if !subst.contains_key("Self")
			&& let Some((_, ty)) = heap_bindings.iter().find(|(n, _)| return n == "Self")
		{
			subst.insert("Self".to_string(), ty.clone());
		}
		if !subst.contains_key("Self")
			&& let Some(first) = f.params.first()
		{
			let base = peel_receiver(&first.ty);
			if !matches!(base, Ty::SelfTy | Ty::Infer)
				&& let Some(mono_self) = self.lower_ty(base, &subst)
			{
				subst.insert("Self".to_string(), mono_self);
			}
		}

		return subst;
	}

	fn build_type_subst(&mut self, t: &MirTypeDef, type_args: &[MonoTy], span: Span) -> Subst
	{
		let mut subst = Subst::new();
		if type_args.len() > t.generics.len() {
			self.diagnostics.push(compiler_bug!(
				span,
				"type `{}` instantiated with too many type arguments ({} declared, {} provided)",
				t.name,
				t.generics.len(),
				type_args.len(),
			));
		}
		for (gp, ty) in t.generics.iter().zip(type_args.iter()) {
			subst.insert(gp.name.clone(), ty.clone());
		}
		return subst;
	}

	fn lower_ty(&mut self, ty: &Ty, subst: &Subst) -> Option<MonoTy>
	{
		match ty {
			Ty::Unit | Ty::Never => return None,

			Ty::Primitive(p) => return Some(MonoTy::Primitive(p.clone())),

			Ty::Named { symbol, generics } => {
				let lowered_args: Vec<MonoTy> =
					generics.iter().filter_map(|g| return self.lower_ty(g, subst)).collect();
				let mangled = self.type_mangled_name(*symbol, &lowered_args);
				let instance: Instance = (*symbol, lowered_args.clone(), Vec::new());
				self.enqueue_type(&instance);
				return Some(MonoTy::Named {
					symbol: *symbol,
					type_args: lowered_args,
					mangled_name: mangled,
				});
			}

			Ty::Reference { mutable, inner } => {
				let inner_mono = self.lower_ty(inner, subst).unwrap_or_else(MonoTy::unit);
				return Some(MonoTy::Reference {
					mutable: *mutable,
					inner: Box::new(inner_mono),
				});
			}
			Ty::Pointer { mutable, inner } => {
				let inner_mono = self.lower_ty(inner, subst).unwrap_or_else(MonoTy::unit);
				return Some(MonoTy::Pointer {
					mutable: *mutable,
					inner: Box::new(inner_mono),
				});
			}
			Ty::Mutable { inner } => return self.lower_ty(inner, subst),

			Ty::Array { inner, size } => {
				if matches!(size, Some(0)) {
					return None;
				}
				let inner_mono = self.lower_ty(inner, subst)?;
				return Some(MonoTy::Array {
					inner: Box::new(inner_mono),
					size: *size,
				});
			}

			Ty::Tuple(elems) => {
				let mono_elems: Vec<MonoTy> = elems.iter().filter_map(|e| return self.lower_ty(e, subst)).collect();
				if mono_elems.is_empty() {
					return None;
				}
				return Some(MonoTy::Tuple(mono_elems));
			}

			Ty::Generic { name, .. } => {
				if let Some(concrete) = subst.get(name.as_str()) {
					return Some(concrete.clone());
				}
				self.diagnostics.push(compiler_bug!(
					Span::default(),
					"unsubstituted generic `{}` reached monomorphization",
					name,
				));
				return None;
			}

			Ty::ImplTrait { concrete, bounds } => {
				if let Some(c) = concrete {
					return self.lower_ty(c, subst);
				}

				for b in bounds {
					if let TyBound::Trait { symbol, .. } = b {
						let hp_name = match self.global.symbol(*symbol).name.as_str() {
							"Alloc" => Some("alloc"),
							"IO" => Some("io"),
							_ => None,
						};
						if let Some(name) = hp_name
							&& let Some(concrete) = subst.get(name)
						{
							return Some(concrete.clone());
						}
					}
				}

				self.diagnostics.push(compiler_bug!(
					Span::default(),
					"unresolved `impl Trait` reached monomorphization: {:?}",
					bounds
				));
				return None;
			}

			Ty::Infer => {
				dbg!(subst);
				self.diagnostics
					.push(compiler_bug!(Span::default(), "`Ty::Infer` reached monomorphization"));
				return None;
			}
			Ty::SelfTy => {
				if let Some(concrete) = subst.get("Self") {
					return Some(concrete.clone());
				}
				self.diagnostics.push(compiler_bug!(
					Span::default(),
					"`Ty::SelfTy` reached monomorphization with no `Self` binding in scope"
				));
				return None;
			}
		}
	}

	fn lower_ty_or_unit(&mut self, ty: &Ty, subst: &Subst) -> MonoTy
	{
		return self.lower_ty(ty, subst).unwrap_or_else(MonoTy::unit);
	}

	fn enqueue_fn(&mut self, instance: Instance)
	{
		if self.reachable_fns.insert(instance.clone()) {
			self.fn_worklist.push_back(instance);
		}
	}

	fn enqueue_type(&mut self, instance: &Instance)
	{
		if !self.reachable_types.insert(instance.clone()) {
			return;
		}
		self.type_worklist.push_back(instance.clone());
	}

	/// Eagerly lowers the field/member/alias types of a type instance, so that any
	/// `Named` types they reference are themselves enqueued. This must happen
	/// before [`Monomorphizer::mono_typedef`] computes the final `MonoTypeDef`,
	/// since that step also performs the lowering but its result is what gets
	/// cached into `typedef_kinds`.
	fn enqueue_global(&mut self, sym: SymbolId)
	{
		self.reachable_globals.insert(sym);
	}

	fn intern_const_body(&mut self, module_idx: usize, id: ConstBodyId, subst: &Subst) -> ConstBodyId
	{
		let key: ConstBodyKey = (module_idx, id.0);
		if let Some(&existing) = self.const_body_map.get(&key) {
			return existing;
		}

		#[allow(clippy::cast_possible_truncation)]
		let new_id = ConstBodyId(self.out_const_bodies.len() as u32);
		self.const_body_map.insert(key, new_id);
		self.out_const_bodies.push(None);

		let module = &self.modules[module_idx];
		if (id.0 as usize) >= module.const_bodies.len() {
			self.diagnostics.push(compiler_bug!(
				Span::default(),
				"const body id out of range during monomorphization"
			));
			self.out_const_bodies[new_id.0 as usize] = Some(MonoConstBody {
				body: MonoBody {
					locals: Vec::new(),
					param_count: 0,
					blocks: Vec::new(),
					return_local: None,
				},
				result: LocalId(0),
			});
			return new_id;
		}

		let cb = &module.const_bodies[id.0 as usize];
		let body = self.lower_body(&cb.body, subst, module_idx);
		self.out_const_bodies[new_id.0 as usize] = Some(MonoConstBody {
			body,
			result: cb.result,
		});
		return new_id;
	}

	fn find_entry(&self) -> Option<SymbolId>
	{
		let mut user_main: Option<SymbolId> = None;
		let mut any_main: Option<SymbolId> = None;
		for m in self.modules {
			for item in &m.items {
				if let MirItem::Function(f) = item
					&& f.name == "main"
				{
					if m.path.is_empty() {
						user_main = Some(f.symbol);
					}
					any_main.get_or_insert(f.symbol);
				}
			}
		}
		return user_main.or(any_main);
	}

	fn run(&mut self) -> MonoModule
	{
		let Some(entry_sym) = self.find_entry() else {
			self.diagnostics.push(
				MonoError {
					span: Span::default(),
					kind: MonoErrorKind::NoMainEntry,
				}
				.build(),
			);

			return MonoModule {
				path: Vec::new(),
				items: Vec::new(),
				const_bodies: Vec::new(),
				entry: String::new(),
				option_symbol: None,
			};
		};

		let entry_subst_extras: Subst = if let Some(ItemRef::Function(_, main_fn)) = self.lookup_item(entry_sym) {
			let mut s = Subst::new();
			for hp in &main_fn.heap_generics {
				if let Some(default_ty) = self.default_for_heap_param(&hp.name) {
					s.insert(hp.name.clone(), default_ty);
				} else {
					self.diagnostics.push(compiler_bug!(
						main_fn.span,
						"no default concrete type for heap-param `{}` of `main`",
						hp.name,
					));
				}
			}
			s
		} else {
			Subst::new()
		};

		let option_symbol = self.resolve_module_path(&["core", "options", "Option"]);

		let entry_heap_bindings: Vec<(String, MonoTy)> = {
			let mut v: Vec<(String, MonoTy)> = entry_subst_extras.into_iter().collect();
			v.sort_by(|a, b| return a.0.cmp(&b.0));
			v
		};
		let entry_instance: Instance = (entry_sym, Vec::new(), entry_heap_bindings.clone());
		self.enqueue_fn(entry_instance);
		let entry_mangled = self.fn_mangled_name(entry_sym, &[], &entry_heap_bindings);

		loop {
			if let Some(instance) = self.fn_worklist.pop_front() {
				self.mono_one_function(instance);
				continue;
			}
			if let Some(instance) = self.type_worklist.pop_front() {
				if let Some(td) = self.mono_typedef(instance.clone()) {
					let key = (td.symbol, td.type_args.clone());
					self.typedef_kinds.insert(key, td.kind.clone());
					self.out_typedefs.push(td);
				}
				continue;
			}
			break;
		}

		self.prune_zst();

		let global_syms: Vec<SymbolId> = self.reachable_globals.iter().copied().collect();
		for sym in global_syms {
			if let Some(g) = self.mono_global(sym) {
				self.out_globals.push(g);
			}
		}

		let entry_path: Vec<String> = self
			.modules
			.iter()
			.find(|m| {
				return m
					.items
					.iter()
					.any(|it| matches!(it, MirItem::Function(f) if f.symbol == entry_sym));
			})
			.map(|m| return m.path.clone())
			.unwrap_or_default();

		self.synthesize_main_wrapper(entry_sym, &[], &entry_heap_bindings, &entry_mangled);

		let mut items: Vec<MonoItem> = Vec::new();
		for t in std::mem::take(&mut self.out_typedefs) {
			items.push(MonoItem::TypeDef(t));
		}
		for g in std::mem::take(&mut self.out_globals) {
			items.push(MonoItem::Global(g));
		}
		for f in std::mem::take(&mut self.out_functions) {
			items.push(MonoItem::Function(f));
		}

		let const_bodies: Vec<MonoConstBody> = std::mem::take(&mut self.out_const_bodies)
			.into_iter()
			.enumerate()
			.map(|(i, slot)| {
				return slot.unwrap_or_else(|| {
					self.diagnostics.push(compiler_bug!(
						Span::default(),
						"const body slot {} was reserved but never filled",
						i
					));
					return MonoConstBody {
						body: MonoBody {
							locals: Vec::new(),
							param_count: 0,
							blocks: Vec::new(),
							return_local: None,
						},
						result: LocalId(0),
					};
				});
			})
			.collect();

		return MonoModule {
			path: entry_path,
			items,
			const_bodies,
			entry: entry_mangled,
			option_symbol,
		};
	}

	fn mono_one_function(&mut self, instance: Instance)
	{
		let (sym, type_args, heap_bindings) = instance;
		let Some(ItemRef::Function(mi, f)) = self.lookup_item(sym) else {
			self.diagnostics.push(compiler_bug!(
				Span::default(),
				"call to unknown function symbol reached monomorphization"
			));
			return;
		};

		let subst = self.build_subst(f, &type_args, &heap_bindings, f.span);
		let mangled = self.fn_mangled_name(sym, &type_args, &heap_bindings);

		let params: Vec<MonoParam> = f
			.params
			.iter()
			.filter_map(|p| {
				let ty = self.lower_ty(&p.ty, &subst)?;
				return Some(MonoParam {
					local: p.local,
					name: p.name.clone(),
					ty,
					mutable: p.mutable,
				});
			})
			.collect();

		let return_ty = self.lower_ty(&f.return_ty, &subst);

		let body = f.body.as_ref().map(|b| return self.lower_body(b, &subst, mi));

		self.out_functions.push(MonoFunction {
			symbol: sym,
			type_args,
			mangled_name: mangled,
			params,
			return_ty,
			body,
			modifiers: f.modifiers.clone(),
			span: f.span,
		});
	}

	fn mono_global(&mut self, sym: SymbolId) -> Option<MonoGlobal>
	{
		if !self.globals.contains_key(&sym) {
			let kind_hint = if self.typedefs.contains_key(&sym) {
				"type/variant"
			} else {
				"unknown"
			};
			self.diagnostics.push(compiler_bug!(
				Span::default(),
				"MIR referenced SymbolId({:?}) as a global, but it is a {}; \
             the MIR builder should lower enum variants as constants, not globals",
				sym,
				kind_hint,
			));
			return None;
		}
		let Some(ItemRef::Global(mi, g)) = self.lookup_item(sym) else {
			self.diagnostics.push(compiler_bug!(
				Span::default(),
				"reachable global symbol has no MIR definition"
			));
			return None;
		};

		let subst = Subst::new();
		let ty = self.lower_ty(&g.ty, &subst)?;
		let init = self.intern_const_body(mi, g.init, &subst);
		let scope_path = self.global.scope_path(sym);
		let mangled = Self::mangle_with_args(&scope_path, &g.name, &[], &[]);

		return Some(MonoGlobal {
			symbol: sym,
			mangled_name: mangled,
			ty,
			init,
			mutable: g.mutable,
			span: g.span,
		});
	}

	fn mono_typedef(&mut self, instance: Instance) -> Option<MonoTypeDef>
	{
		let (sym, type_args, _) = instance;
		let Some(ItemRef::TypeDef(mi, t)) = self.lookup_item(sym) else {
			return None;
		};
		let subst = self.build_type_subst(t, &type_args, t.span);
		let mangled = self.type_mangled_name(sym, &type_args);

		let kind = match &t.kind {
			MirTypeDefKind::Struct { fields } => MonoTypeDefKind::Struct {
				fields: fields
					.iter()
					.filter_map(|(name, ty)| {
						let mono_ty = self.lower_ty(ty, &subst)?;
						return Some((name.clone(), mono_ty));
					})
					.collect(),
			},
			MirTypeDefKind::Union { fields } => MonoTypeDefKind::Union {
				fields: fields
					.iter()
					.filter_map(|(name, ty)| {
						let mono_ty = self.lower_ty(ty, &subst)?;
						return Some((name.clone(), mono_ty));
					})
					.collect(),
			},
			MirTypeDefKind::Enum { variants } => MonoTypeDefKind::Enum {
				variants: variants
					.iter()
					.map(|(name, cb)| {
						let new_cb = cb.map(|c| return self.intern_const_body(mi, c, &subst));
						return (name.clone(), new_cb);
					})
					.collect(),
			},
			MirTypeDefKind::Variant { members } => MonoTypeDefKind::Variant {
				members: members
					.iter()
					.map(|(name, mty)| {
						let mono = mty.as_ref().and_then(|ty| return self.lower_ty(ty, &subst));
						return (name.clone(), mono);
					})
					.collect(),
			},
			MirTypeDefKind::TypeAlias { ty } => MonoTypeDefKind::TypeAlias {
				ty: self.lower_ty_or_unit(ty, &subst),
			},
		};

		return Some(MonoTypeDef {
			symbol: sym,
			type_args,
			mangled_name: mangled,
			kind,
			span: t.span,
		});
	}

	fn lower_body(&mut self, body: &MirBody, subst: &Subst, module_idx: usize) -> MonoBody
	{
		let locals: Vec<MonoLocal> = body
			.locals
			.iter()
			.map(|l| {
				return MonoLocal {
					id: l.id,
					ty: self.lower_ty_or_unit(&l.ty, subst),
					name: l.name.clone(),
					mutable: l.mutable,
					is_temp: l.is_temp,
					span: l.span,
				};
			})
			.collect();

		let blocks: Vec<MonoBasicBlock> = body
			.blocks
			.iter()
			.map(|b| return self.lower_block(b, subst, module_idx))
			.collect();

		let pruned = prune_unreachable_blocks(blocks);

		return MonoBody {
			locals,
			param_count: body.param_count,
			blocks: pruned,
			return_local: body.return_local,
		};
	}

	fn lower_block(&mut self, block: &MirBasicBlock, subst: &Subst, module_idx: usize) -> MonoBasicBlock
	{
		let stmts: Vec<MonoStmt> = block
			.stmts
			.iter()
			.filter_map(|s| return self.lower_stmt(s, subst, module_idx))
			.collect();

		let terminator = self.lower_terminator(&block.terminator, subst, module_idx);

		return MonoBasicBlock {
			id: block.id,
			stmts,
			terminator,
		};
	}

	fn lower_stmt(&mut self, stmt: &MirStmt, subst: &Subst, module_idx: usize) -> Option<MonoStmt>
	{
		return Some(match stmt {
			MirStmt::Assign { place, rvalue, span } => {
				let mono_place = self.lower_place(place, subst, module_idx);
				if self.is_zst(&mono_place.ty) {
					return None;
				}
				let mono_rvalue = self.lower_rvalue_with_hint(rvalue, subst, module_idx, &mono_place.ty);
				MonoStmt::Assign {
					place: mono_place,
					rvalue: mono_rvalue,
					span: *span,
				}
			}
			MirStmt::Call {
				callee,
				type_args,
				named_generics,
				args,
				span,
			} => MonoStmt::Call {
				callee: self.lower_callee(callee, type_args, named_generics, args, None, subst),
				args: self.lower_args(args, subst, module_idx),
				span: *span,
			},
			MirStmt::Delete { operand, span } => MonoStmt::Delete {
				operand: self.lower_operand(operand, subst, module_idx),
				span: *span,
			},
			MirStmt::Nop => MonoStmt::Nop,
		});
	}

	fn lower_terminator(&mut self, term: &MirTerminator, subst: &Subst, module_idx: usize) -> MonoTerminator
	{
		return match term {
			MirTerminator::Goto { target } => MonoTerminator::Goto { target: *target },
			MirTerminator::Branch {
				cond,
				then_block,
				else_block,
			} => MonoTerminator::Branch {
				cond: self.lower_operand(cond, subst, module_idx),
				then_block: *then_block,
				else_block: *else_block,
			},
			MirTerminator::CallAndContinue {
				callee,
				type_args,
				named_generics,
				args,
				dest,
				next,
				unwind,
				span,
			} => {
				let mono_dest = self.lower_place(dest, subst, module_idx);
				MonoTerminator::CallAndContinue {
					callee: self.lower_callee(callee, type_args, named_generics, args, Some(&mono_dest.ty), subst),
					args: self.lower_args(args, subst, module_idx),
					dest: mono_dest,
					next: *next,
					unwind: *unwind,
					span: *span,
				}
			}
			MirTerminator::Return => MonoTerminator::Return,
			MirTerminator::Unreachable => MonoTerminator::Unreachable,
			MirTerminator::Switch {
				scrutinee,
				arms,
				otherwise,
			} => MonoTerminator::Switch {
				scrutinee: self.lower_operand(scrutinee, subst, module_idx),
				arms: arms
					.iter()
					.map(|a| {
						return MonoSwitchArm {
							value: self.lower_operand(&a.value, subst, module_idx),
							target: a.target,
						};
					})
					.collect(),
				otherwise: *otherwise,
			},
		};
	}

	fn lower_callee(
		&mut self,
		callee: &MirCallee,
		type_args: &[Ty],
		named_generics: &[(String, Ty)],
		args: &[MirOperand],
		dest_ty: Option<&MonoTy>,
		subst: &Subst,
	) -> MonoCallee
	{
		return match callee {
			MirCallee::Direct(sym) => {
				let mut effective_sym = *sym;
				if self.lookup_item(effective_sym).is_none() {
					let recv_ty = args.first().map(|a| return a.ty());
					let method_name = &self.global.symbol(*sym).name;
				}
				if self.lookup_item(effective_sym).is_none()
					&& let Some(recv) = args.first()
					&& let Some(key) = self.dispatch_key(recv.ty(), subst)
				{
					let method_name = self.global.symbol(*sym).name.clone();
					if let Some(&impl_sym) = self.method_dispatch.get(&(key, method_name)) {
						effective_sym = impl_sym;
					}
				}

				if self.lookup_item(effective_sym).is_none()
					&& let Some(self_ty) = subst.get("Self").cloned()
					&& let Some(key) = Self::dispatch_key_mono(&self_ty)
				{
					let method_name = self.global.symbol(*sym).name.clone();
					if let Some(&impl_sym) = self.method_dispatch.get(&(key, method_name)) {
						effective_sym = impl_sym;
					}
				}

				if self.lookup_item(effective_sym).is_none() {
					let name = self.global.symbol(*sym).name.clone();
					self.diagnostics.push(
						MonoError {
							span: Span::default(),
							kind: MonoErrorKind::AbstractTraitMethodCall { name },
						}
						.build(),
					);
				}

				let mut concrete_args: Vec<MonoTy> = type_args
					.iter()
					.filter_map(|t| return self.lower_ty(t, subst))
					.collect();

				let mut named_sorted: Vec<&(String, Ty)> = named_generics.iter().collect();
				named_sorted.sort_by(|a, b| return a.0.cmp(&b.0));

				let mut callee_heap_bindings: Vec<(String, MonoTy)> = Vec::new();
				for (name, ty) in named_sorted {
					if let Some(mt) = self.lower_ty(ty, subst) {
						callee_heap_bindings.push((name.clone(), mt));
					}
				}

				if let Some(ItemRef::Function(_, callee_fn)) = self.lookup_item(effective_sym) {
					let mut bound: HashMap<String, MonoTy> = HashMap::new();
					for ((ident, _), ty) in callee_fn.generics.iter().zip(concrete_args.iter()) {
						bound.insert(ident.clone(), ty.clone());
					}

					for (param, arg) in callee_fn.params.iter().zip(args.iter()) {
						debug_assert!(
							!matches!(param.ty, Ty::ImplTrait { .. }),
							"desugarer must remove `impl Trait` from function parameters \
                 before MIR lowering (in `{}`)",
							callee_fn.name,
						);
						if let Some(arg_mono) = self.lower_ty(arg.ty(), subst) {
							unify_generic(&param.ty, &arg_mono, &mut bound);
						}
					}

					if let Some(d) = dest_ty {
						unify_generic(&callee_fn.return_ty, d, &mut bound);
					}

					let mut rebuilt: Vec<MonoTy> = Vec::with_capacity(callee_fn.generics.len());
					for (ident, _) in &callee_fn.generics {
						if let Some(t) = bound.get(ident) {
							rebuilt.push(t.clone());
						} else {
							self.diagnostics.push(compiler_bug!(
								Span::default(),
								"could not infer type argument `{}` for call to `{}`",
								ident,
								callee_fn.name,
							));
							rebuilt.push(MonoTy::unit());
						}
					}
					concrete_args = rebuilt;
				}

				let mangled = self.fn_mangled_name(effective_sym, &concrete_args, &callee_heap_bindings);
				self.enqueue_fn((effective_sym, concrete_args.clone(), callee_heap_bindings.clone()));
				MonoCallee::Direct {
					symbol: effective_sym,
					type_args: concrete_args,
					mangled_name: mangled,
				}
			}
			MirCallee::Indirect(l) => MonoCallee::Indirect(*l),
			MirCallee::Intrinsic(i) => MonoCallee::Intrinsic(i.clone()),
		};
	}

	fn lower_args(&mut self, args: &[MirOperand], subst: &Subst, module_idx: usize) -> Vec<MonoOperand>
	{
		return args
			.iter()
			.map(|a| return self.lower_operand(a, subst, module_idx))
			.collect();
	}

	fn lower_place(&mut self, place: &MirPlace, subst: &Subst, module_idx: usize) -> MonoPlace
	{
		let ty = self.lower_ty_or_unit(&place.ty, subst);

		let base = match &place.base {
			MirPlaceBase::Local(l) => MonoPlaceBase::Local(*l),
			MirPlaceBase::Global(s) => {
				self.enqueue_global(*s);
				MonoPlaceBase::Global(*s)
			}
		};

		let projections: Vec<MonoProjection> = place
			.projections
			.iter()
			.map(|p| return self.lower_projection(p, subst))
			.collect();

		return MonoPlace { base, projections, ty };
	}

	fn lower_projection(&mut self, proj: &MirProjection, subst: &Subst) -> MonoProjection
	{
		return match proj {
			MirProjection::Field { name, ty } => MonoProjection::Field {
				name: name.clone(),
				ty: self.lower_ty_or_unit(ty, subst),
			},
			MirProjection::Index { index, ty } => MonoProjection::Index {
				index: *index,
				ty: self.lower_ty_or_unit(ty, subst),
			},
			MirProjection::Deref => MonoProjection::Deref,
		};
	}

	fn lower_operand(&mut self, op: &MirOperand, subst: &Subst, module_idx: usize) -> MonoOperand
	{
		return match op {
			MirOperand::Copy(p) => MonoOperand::Copy(self.lower_place(p, subst, module_idx)),
			MirOperand::Move(p) => MonoOperand::Move(self.lower_place(p, subst, module_idx)),
			MirOperand::Const(lit) => MonoOperand::Const(self.lower_literal(lit, subst, module_idx)),
		};
	}

	fn lower_literal(&mut self, lit: &MirLiteral, subst: &Subst, module_idx: usize) -> MonoLiteral
	{
		let value = match &lit.value {
			MirLiteralValue::ConstBody(id) => {
				let new_id = self.intern_const_body(module_idx, *id, subst);
				MirLiteralValue::ConstBody(new_id)
			}
			other => other.clone(),
		};
		return MonoLiteral {
			value,
			ty: self.lower_ty_or_unit(&lit.ty, subst),
		};
	}

	fn lower_rvalue(&mut self, rvalue: &MirRvalue, subst: &Subst, module_idx: usize, dest_ty: &MonoTy) -> MonoRvalue
	{
		return match rvalue {
			MirRvalue::Use(op) => MonoRvalue::Use(self.lower_operand(op, subst, module_idx)),
			MirRvalue::Unary { op, operand } => MonoRvalue::Unary {
				op: *op,
				operand: self.lower_operand(operand, subst, module_idx),
			},
			MirRvalue::Binary { op, lhs, rhs } => MonoRvalue::Binary {
				op: *op,
				lhs: self.lower_operand(lhs, subst, module_idx),
				rhs: self.lower_operand(rhs, subst, module_idx),
			},
			MirRvalue::Cast { ty, operand } => MonoRvalue::Cast {
				ty: self.lower_ty_or_unit(ty, subst),
				operand: self.lower_operand(operand, subst, module_idx),
			},
			MirRvalue::Ref { mutable, place } => MonoRvalue::Ref {
				mutable: *mutable,
				place: self.lower_place(place, subst, module_idx),
			},
			MirRvalue::RawPtr { mutable, place } => MonoRvalue::RawPtr {
				mutable: *mutable,
				place: self.lower_place(place, subst, module_idx),
			},
			MirRvalue::Aggregate { kind, fields } => MonoRvalue::Aggregate {
				kind: self.lower_aggregate_kind(kind, dest_ty),
				fields: fields
					.iter()
					.filter_map(|(name, op)| {
						let mono_op = self.lower_operand(op, subst, module_idx);
						if self.is_zst(mono_op.ty()) {
							return None;
						}
						return Some((name.clone(), mono_op));
					})
					.collect(),
			},
			MirRvalue::Array { elements, elem_ty } => MonoRvalue::Array {
				elements: elements
					.iter()
					.map(|e| return self.lower_operand(e, subst, module_idx))
					.collect(),
				elem_ty: self.lower_ty_or_unit(elem_ty, subst),
			},
			MirRvalue::ArrayRepeat { value, count, elem_ty } => MonoRvalue::ArrayRepeat {
				value: self.lower_operand(value, subst, module_idx),
				count: self.intern_const_body(module_idx, *count, subst),
				elem_ty: self.lower_ty_or_unit(elem_ty, subst),
			},
			MirRvalue::Tuple(elems) => {
				let lowered: Vec<MonoOperand> = elems
					.iter()
					.map(|e| return self.lower_operand(e, subst, module_idx))
					.collect(); // TODO: useless collect, need to remove later, but don't have time to fix it atm
				MonoRvalue::Tuple(lowered.into_iter().filter(|o| return !self.is_zst(o.ty())).collect())
			}
			MirRvalue::Discriminant(place) => MonoRvalue::Discriminant(self.lower_place(place, subst, module_idx)),
		};
	}

	fn lower_rvalue_with_hint(
		&mut self,
		rvalue: &MirRvalue,
		subst: &Subst,
		module_idx: usize,
		dest_ty: &MonoTy,
	) -> MonoRvalue
	{
		if let MirRvalue::Use(MirOperand::Const(lit)) = rvalue
			&& matches!(lit.ty, Ty::Infer)
		{
			let value = match &lit.value {
				MirLiteralValue::ConstBody(id) => {
					let new_id = self.intern_const_body(module_idx, *id, subst);
					MirLiteralValue::ConstBody(new_id)
				}
				other => other.clone(),
			};
			return MonoRvalue::Use(MonoOperand::Const(MonoLiteral {
				value,
				ty: dest_ty.clone(),
			}));
		}

		if let MirRvalue::Aggregate { kind, fields } = rvalue {
			let extended = self.build_aggregate_subst(kind, dest_ty, subst);
			let mono_kind = self.lower_aggregate_kind(kind, dest_ty);
			return MonoRvalue::Aggregate {
				kind: mono_kind,
				fields: fields
					.iter()
					.filter_map(|(name, op)| {
						let mono_op = self.lower_operand(op, &extended, module_idx);
						if self.is_zst(mono_op.ty()) {
							return None;
						}
						return Some((name.clone(), mono_op));
					})
					.collect(),
			};
		}

		return self.lower_rvalue(rvalue, subst, module_idx, dest_ty);
	}

	fn lower_aggregate_kind(&mut self, kind: &MirAggregateKind, dest_ty: &MonoTy) -> MonoAggregateKind
	{
		match kind {
			MirAggregateKind::Struct(s) => {
				let mangled = match dest_ty {
					MonoTy::Named { symbol, type_args, .. } if symbol == s => self.type_mangled_name(*s, type_args),
					_ => self.global.symbol(*s).name.clone(),
				};

				return MonoAggregateKind::Struct {
					symbol: *s,
					mangled_name: mangled,
				};
			}

			MirAggregateKind::Union(s) => {
				let mangled = match dest_ty {
					MonoTy::Named { symbol, type_args, .. } if symbol == s => self.type_mangled_name(*s, type_args),
					_ => self.global.symbol(*s).name.clone(),
				};

				return MonoAggregateKind::Union {
					symbol: *s,
					mangled_name: mangled,
				};
			}

			MirAggregateKind::VariantMember { parent, member } => {
				let mangled = match dest_ty {
					MonoTy::Named { symbol, type_args, .. } if symbol == parent => {
						self.type_mangled_name(*parent, type_args)
					}
					_ => self.global.symbol(*parent).name.clone(),
				};

				return MonoAggregateKind::VariantMember {
					parent: *parent,
					parent_mangled: mangled,
					member: member.clone(),
				};
			}

			MirAggregateKind::Tuple => return MonoAggregateKind::Tuple,
		}
	}

	fn build_aggregate_subst(&self, kind: &MirAggregateKind, dest_ty: &MonoTy, base_subst: &Subst) -> Subst
	{
		let mut extended = base_subst.clone();

		let (sym, type_args) = match (kind, dest_ty) {
			(MirAggregateKind::Struct(s) | MirAggregateKind::Union(s), MonoTy::Named { symbol, type_args, .. })
				if symbol == s =>
			{
				(*s, type_args)
			}

			(MirAggregateKind::VariantMember { parent, .. }, MonoTy::Named { symbol, type_args, .. })
				if symbol == parent =>
			{
				(*parent, type_args)
			}

			_ => return extended,
		};

		if let Some(ItemRef::TypeDef(_, t)) = self.lookup_item(sym) {
			for (gp, ty) in t.generics.iter().zip(type_args.iter()) {
				extended.insert(gp.name.clone(), ty.clone());
			}
		}

		return extended;
	}

	fn type_name_cache_lookup(&self, sym: SymbolId) -> String
	{
		for ((s, _, _), name) in &self.type_name_cache {
			if *s == sym {
				return name.clone();
			}
		}
		return self.global.symbol(sym).name.clone();
	}

	fn default_for_heap_param(&mut self, hp_name: &str) -> Option<MonoTy>
	{
		let default_path: &[&str] = match hp_name {
			"alloc" => &["std", "alloc", "CAlloc"],
			"io" => &["std", "io", "StdIo"],
			_ => return None,
		};
		let sym = self.resolve_module_path(default_path)?;
		let mangled = self.type_mangled_name(sym, &[]);
		self.enqueue_type(&(sym, Vec::new(), Vec::new()));
		return Some(MonoTy::Named {
			symbol: sym,
			type_args: Vec::new(),
			mangled_name: mangled,
		});
	}

	fn synthesize_main_wrapper(
		&mut self,
		entry_sym: SymbolId,
		entry_type_args: &[MonoTy],
		entry_heap_bindings: &[(String, MonoTy)],
		entry_mangled: &str,
	)
	{
		const I32_TY: MonoTy = MonoTy::Primitive(Primitive::Int(IntType {
			bits: IntSize::Fixed(32),
			sign: IntSign::Signed,
		}));

		let entry_return_ty: Option<MonoTy> = self
			.out_functions
			.iter()
			.find(|f| return f.mangled_name == entry_mangled)
			.and_then(|f| return f.return_ty.clone());

		let mut locals: Vec<MonoLocal> = Vec::new();
		let result_local: LocalId = LocalId(0);

		locals.push(MonoLocal {
			id: result_local,
			ty: I32_TY.clone(),
			name: None,
			mutable: false,
			is_temp: true,
			span: Span::default(),
		});
		let blocks: Vec<MonoBasicBlock> = if entry_return_ty
			== Some(MonoTy::Primitive(Primitive::Int(IntType {
				bits: IntSize::Fixed(32),
				sign: IntSign::Signed,
			}))) {
			// User `main` returns i32: call it directly into the result local.

			let dest: MonoPlace = MonoPlace {
				base: MonoPlaceBase::Local(result_local),
				projections: Vec::new(),
				ty: I32_TY,
			};

			vec![
				MonoBasicBlock {
					id: BlockId(0),
					stmts: Vec::new(),
					terminator: MonoTerminator::CallAndContinue {
						callee: MonoCallee::Direct {
							symbol: entry_sym,
							type_args: entry_type_args.to_vec(),
							mangled_name: entry_mangled.to_string(),
						},
						args: Vec::new(),
						dest,
						next: BlockId(1),
						unwind: None,
						span: Span::default(),
					},
				},
				MonoBasicBlock {
					id: BlockId(1),
					stmts: Vec::new(),
					terminator: MonoTerminator::Return,
				},
			]
		} else {
			// User `main` returns unit/anything else: call as a statement, then return 0.

			let stmts: Vec<MonoStmt> = vec![
				MonoStmt::Call {
					callee: MonoCallee::Direct {
						symbol: entry_sym,
						type_args: entry_type_args.to_vec(),
						mangled_name: entry_mangled.to_string(),
					},
					args: Vec::new(),
					span: Span::default(),
				},
				MonoStmt::Assign {
					place: MonoPlace {
						base: MonoPlaceBase::Local(result_local),
						projections: Vec::new(),
						ty: I32_TY,
					},
					rvalue: MonoRvalue::Use(MonoOperand::Const(MonoLiteral {
						value: MirLiteralValue::Literal(parser::Literal::Int {
							value: "0".to_string(),
							base: crate::lexer::IntBase::Decimal,
							ty: None,
							span: Span::default(),
						}),
						ty: I32_TY,
					})),
					span: Span::default(),
				},
			];

			vec![MonoBasicBlock {
				id: BlockId(0),
				stmts,
				terminator: MonoTerminator::Return,
			}]
		};

		self.push_main_wrapper(locals, blocks, Some(result_local), entry_sym, I32_TY);
	}

	fn push_main_wrapper(
		&mut self,
		locals: Vec<MonoLocal>,
		blocks: Vec<MonoBasicBlock>,
		return_local: Option<LocalId>,
		entry_sym: SymbolId,
		i32_ty: MonoTy,
	)
	{
		self.out_functions.push(MonoFunction {
			symbol: entry_sym,
			type_args: Vec::new(),
			mangled_name: "main".to_string(),
			params: Vec::new(),
			return_ty: Some(i32_ty),
			body: Some(MonoBody {
				locals,
				param_count: 0,
				blocks,
				return_local,
			}),
			modifiers: Vec::new(),
			span: Span::default(),
		});
	}

	fn prune_zst(&mut self)
	{
		for f in &mut self.out_functions {
			if let Some(body) = &mut f.body {
				prune_zst_body(body, &self.typedef_kinds);
			}
		}
		for cb in self.out_const_bodies.iter_mut().flatten() {
			prune_zst_body(&mut cb.body, &self.typedef_kinds);
		}
	}
}

fn unify_generic(param: &Ty, arg: &MonoTy, out: &mut HashMap<String, MonoTy>)
{
	match (param, arg) {
		(Ty::Generic { name, .. }, concrete) => {
			out.entry(name.clone()).or_insert_with(|| return concrete.clone());
		}
		(Ty::Mutable { inner }, c) => unify_generic(inner, c, out),

		(Ty::Reference { inner: pi, .. }, MonoTy::Reference { inner: ai, .. })
		| (Ty::Pointer { inner: pi, .. }, MonoTy::Pointer { inner: ai, .. }) => {
			unify_generic(pi, ai, out);
		}

		(Ty::Reference { inner: pi, .. } | Ty::Pointer { inner: pi, .. }, ai) => {
			unify_generic(pi, ai, out);
		}

		(pi, MonoTy::Reference { inner: ai, .. } | MonoTy::Pointer { inner: ai, .. }) => {
			unify_generic(pi, ai, out);
		}

		(Ty::Array { inner: pi, .. }, MonoTy::Array { inner: ai, .. }) => unify_generic(pi, ai, out),

		(Ty::Tuple(ps), MonoTy::Tuple(as_)) if ps.len() == as_.len() => {
			for (p, a) in ps.iter().zip(as_) {
				unify_generic(p, a, out);
			}
		}

		(Ty::Named { generics: pg, .. }, MonoTy::Named { type_args: ag, .. }) if pg.len() == ag.len() => {
			for (p, a) in pg.iter().zip(ag) {
				unify_generic(p, a, out);
			}
		}

		_ => {}
	}
}

fn peel_receiver(ty: &Ty) -> &Ty
{
	let mut cur = ty;
	loop {
		match cur {
			Ty::Reference { inner, .. } | Ty::Pointer { inner, .. } | Ty::Mutable { inner } => cur = inner,
			_ => return cur,
		}
	}
}

fn prune_unreachable_blocks(blocks: Vec<MonoBasicBlock>) -> Vec<MonoBasicBlock>
{
	if blocks.is_empty() {
		return blocks;
	}

	let n = blocks.len();
	let mut reachable = vec![false; n];
	let mut stack: Vec<u32> = vec![0];
	reachable[0] = true;

	while let Some(idx) = stack.pop() {
		let block = &blocks[idx as usize];
		let mut visit = |t: BlockId| {
			let i = t.0 as usize;
			if i < n && !reachable[i] {
				reachable[i] = true;
				stack.push(t.0);
			}
		};
		match &block.terminator {
			MonoTerminator::Goto { target } => visit(*target),
			MonoTerminator::Branch {
				then_block, else_block, ..
			} => {
				visit(*then_block);
				visit(*else_block);
			}
			MonoTerminator::CallAndContinue { next, unwind, .. } => {
				visit(*next);
				if let Some(u) = unwind {
					visit(*u);
				}
			}
			MonoTerminator::Switch { arms, otherwise, .. } => {
				for a in arms {
					visit(a.target);
				}
				visit(*otherwise);
			}
			MonoTerminator::Return | MonoTerminator::Unreachable => {}
		}
	}

	let mut remap: Vec<u32> = vec![u32::MAX; n];
	let mut new_idx: u32 = 0;
	for (i, &live) in reachable.iter().enumerate() {
		if live {
			remap[i] = new_idx;
			new_idx += 1;
		}
	}

	let remap_id = |b: BlockId| -> BlockId {
		let r = remap[b.0 as usize];
		return BlockId(if r == u32::MAX { 0 } else { r });
	};

	let mut out: Vec<MonoBasicBlock> = Vec::with_capacity(new_idx as usize);
	for (i, block) in blocks.into_iter().enumerate() {
		if !reachable[i] {
			continue;
		}
		let new_id = BlockId(remap[i]);
		let new_term = match block.terminator {
			MonoTerminator::Goto { target } => MonoTerminator::Goto {
				target: remap_id(target),
			},
			MonoTerminator::Branch {
				cond,
				then_block,
				else_block,
			} => MonoTerminator::Branch {
				cond,
				then_block: remap_id(then_block),
				else_block: remap_id(else_block),
			},
			MonoTerminator::CallAndContinue {
				callee,
				args,
				dest,
				next,
				unwind,
				span,
			} => MonoTerminator::CallAndContinue {
				callee,
				args,
				dest,
				next: remap_id(next),
				unwind: unwind.map(remap_id),
				span,
			},
			MonoTerminator::Switch {
				scrutinee,
				arms,
				otherwise,
			} => MonoTerminator::Switch {
				scrutinee,
				arms: arms
					.into_iter()
					.map(|a| {
						return MonoSwitchArm {
							value: a.value,
							target: remap_id(a.target),
						};
					})
					.collect(),
				otherwise: remap_id(otherwise),
			},
			other @ (MonoTerminator::Return | MonoTerminator::Unreachable) => other,
		};
		out.push(MonoBasicBlock {
			id: new_id,
			stmts: block.stmts,
			terminator: new_term,
		});
	}

	return out;
}

fn mono_ty_key(ty: &MonoTy) -> Option<TyKey>
{
	return match ty {
		MonoTy::Named { symbol, .. } => Some(TyKey::Symbol(*symbol)),
		MonoTy::Primitive(p) => Some(TyKey::Prim(p.clone())),

		MonoTy::Reference { inner, .. } | MonoTy::Pointer { inner, .. } => mono_ty_key(inner),
		_ => None,
	};
}

pub fn monomorphize(modules: &[MirModule], global: &GlobalSymbolTable) -> (MonoModule, Vec<DiagnosticBuilder>)
{
	let mut mono = Monomorphizer::new(global, modules);
	let mono_mod = mono.run();
	return (mono_mod, mono.diagnostics);
}

fn prune_zst_body(body: &mut MonoBody, typedef_kinds: &HashMap<(SymbolId, Vec<MonoTy>), MonoTypeDefKind>)
{
	body.locals.retain(|l| return !l.ty.is_zst(typedef_kinds));

	for block in &mut body.blocks {
		prune_zst_block(block, typedef_kinds);
	}
}

fn prune_zst_block(block: &mut MonoBasicBlock, typedef_kinds: &HashMap<(SymbolId, Vec<MonoTy>), MonoTypeDefKind>)
{
	block.stmts.retain_mut(|stmt| match stmt {
		MonoStmt::Assign { place, .. } => return !place.ty.is_zst(typedef_kinds),
		MonoStmt::Call { args, .. } => {
			args.retain(|a| return !a.ty().is_zst(typedef_kinds));
			return true;
		}
		MonoStmt::Delete { .. } | MonoStmt::Nop => return true,
	});

	if let MonoTerminator::CallAndContinue { args, .. } = &mut block.terminator {
		args.retain(|a| return !a.ty().is_zst(typedef_kinds));
	}
}
