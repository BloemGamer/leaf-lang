use std::{collections::HashMap, fmt::Write};

use leaf_proc::{compiler_bug, compiler_not_implemented};

// TODO: const is ignored for now, at least for function parameters it should not

use crate::{
	Span,
	backend::{BackendInput, BackendOutput, BackendResult, CompilerBackend, OutputKind},
	diagnostics::DiagnosticBuilder,
	lexer::{Spanned, StringFlags},
	mir::{LocalId, MirLiteralValue},
	monomorphization::{
		MonoAggregateKind, MonoBasicBlock, MonoBody, MonoCallee, MonoConstBody, MonoFunction, MonoGlobal, MonoItem,
		MonoLiteral, MonoLocal, MonoOperand, MonoParam, MonoPlace, MonoPlaceBase, MonoProjection, MonoRvalue, MonoStmt,
		MonoTerminator, MonoTy, MonoTypeDef, MonoTypeDefKind,
	},
	parser::{BinaryOp, Literal, UnaryOp, read_radix_number},
	source_map::SourceMap,
	symbol_collection::{SymbolId, SymbolKind},
	type_analysis::Primitive,
};

use super::BackendOptions;

const DEFAULT_TYPE_DEFS: &[&str] = &["typedef struct LeafStr {
\tsize_t len;
\tchar* data;
} LeafStr;"];

const DEFAULT_ASSERTS: &[&str] = &[
	"sizeof(size_t) == sizeof(void*)",   // size_t should be the size of a pointer
	"sizeof(intptr_t) == sizeof(void*)", // intptr_t should be the size of a pointer
];

const DEFAULT_C_HEADERS: &[&str] = &[
	"<stdint.h>", // needed for the basic types
	"<stddef.h>", // needed for size_t
];
const DEFAULT_C_DEFINES: &[&str] = &[];

#[derive(Default)]
struct Collected
{
	tuples: Vec<MonoTy>,
	types: Vec<MonoTypeDef>,
	functions: Vec<MonoFunction>,
	globals: Vec<MonoGlobal>,
}

pub struct CBackend
{
	diagnostics: Vec<DiagnosticBuilder>,
}

impl CompilerBackend for CBackend
{
	fn name(&self) -> &'static str
	{
		return "c";
	}

	fn supported_outputs(&self) -> &'static [OutputKind]
	{
		return &[
			OutputKind::Ir,
			OutputKind::Asm,
			OutputKind::Object,
			OutputKind::Executable,
			OutputKind::StaticLib,
			OutputKind::DynamicLib,
		];
	}

	fn compile(&mut self, input: &BackendInput<'_>) -> BackendResult<BackendOutput>
	{
		let c_path: std::path::PathBuf = input.options.output_path.with_extension("c");

		let Ok(file) = self.codegen_module(input) else {
			return Err(std::mem::take(&mut self.diagnostics));
		};

		if let Err(e) = std::fs::write(&c_path, file) {
			todo!("couldn't write file: {e}");
		}

		return Ok((
			BackendOutput {
				primary: c_path,
				artifacts: Vec::new(),
			},
			std::mem::take(&mut self.diagnostics),
		));
	}
}

impl CBackend
{
	pub const fn new() -> CBackend
	{
		return CBackend {
			diagnostics: Vec::new(),
		};
	}

	fn codegen_module(&mut self, input: &BackendInput<'_>) -> Result<String, ()>
	{
		let mut out: String = String::new();

		let collected: Collected = self.collect(input);
		self.write_header(&collected, input, &mut out)?;
		self.write_types(&collected, input, &mut out)?;
		self.write_globals(&collected, input, &mut out)?;
		self.write_function_prototypes(&collected, input, &mut out)?;
		self.write_functions(&collected, input, &mut out)?;

		return Ok(out);
	}

	fn collect(&self, input: &BackendInput<'_>) -> Collected
	{
		let mut collected: Collected = Collected::default();

		for item in &input.module.items {
			match item.clone() {
				MonoItem::Function(f) => {
					for ty in &f.type_args {
						collect_tuples(ty, &mut collected.tuples);
					}
					if let Some(ty) = &f.return_ty {
						collect_tuples(ty, &mut collected.tuples);
					}
					if let Some(body) = &f.body {
						for MonoLocal { ty, .. } in &body.locals {
							collect_tuples(ty, &mut collected.tuples);
						}
					}
					collected.functions.push(f);
				}
				MonoItem::Global(g) => {
					collect_tuples(&g.ty, &mut collected.tuples);
					collected.globals.push(g);
				}
				MonoItem::TypeDef(td) => {
					match &td.kind {
						MonoTypeDefKind::Struct { fields } | MonoTypeDefKind::Union { fields } => {
							for (_, ty) in fields {
								collect_tuples(ty, &mut collected.tuples);
							}
						}

						MonoTypeDefKind::Enum { .. } => {}

						MonoTypeDefKind::Variant { members }
							if td.is_option_variant(input.module.option_symbol)
								&& members.iter().any(|(name, ty)| {
									return name == "Some" && matches!(ty, Some(MonoTy::Pointer { .. }));
								}) =>
						{
							for (_, oty) in members {
								if let Some(ty) = oty {
									collect_tuples(ty, &mut collected.tuples);
								}
							}
						}
						MonoTypeDefKind::Variant { members } => {
							for (_, oty) in members {
								if let Some(ty) = oty {
									collect_tuples(ty, &mut collected.tuples);
								}
							}
						}

						MonoTypeDefKind::TypeAlias { ty } => {
							collect_tuples(ty, &mut collected.tuples);
						}
					}

					collected.types.push(td);
				}
			}
		}

		return collected;
	}

	fn write_header(&mut self, collected: &Collected, input: &BackendInput<'_>, out: &mut impl Write)
	-> Result<(), ()>
	{
		for header in DEFAULT_C_HEADERS {
			writeln!(out, "#include {}", header).map_err(|_| ())?;
		}
		for define in DEFAULT_C_DEFINES {
			writeln!(out, "#define {}", define).map_err(|_| ())?;
		}
		for assert in DEFAULT_ASSERTS {
			writeln!(out, "static_assert({});", assert).map_err(|_| ())?;
		}
		for ty in DEFAULT_TYPE_DEFS {
			writeln!(out, "{}", ty).map_err(|_| ())?;
		}
		return Ok(());
	}

	fn write_types(&mut self, collected: &Collected, input: &BackendInput<'_>, out: &mut impl Write) -> Result<(), ()>
	{
		// TODO: this probably works, but maybe it sometimes fails
		for td in &collected.types {
			match &td.kind {
				MonoTypeDefKind::Struct { .. } => {
					writeln!(out, "typedef struct {0} {0};", td.mangled_name).map_err(|_| ())?;
				}

				MonoTypeDefKind::Union { .. } => {
					writeln!(out, "typedef union {0} {0};", td.mangled_name).map_err(|_| ())?;
				}

				MonoTypeDefKind::Enum { .. } => {
					writeln!(out, "typedef enum {0} {0};", td.mangled_name).map_err(|_| ())?;
				}

				MonoTypeDefKind::Variant { members }
					if td.is_option_variant(input.module.option_symbol)
						&& members.iter().any(|(name, ty)| {
							return name == "Some" && matches!(ty, Some(MonoTy::Pointer { .. }));
						}) => {}
				MonoTypeDefKind::Variant { .. } => {
					writeln!(out, "typedef struct {0} {0};", td.mangled_name).map_err(|_| ())?;
					writeln!(out, "typedef enum {0}Tag {0}Tag;", td.mangled_name).map_err(|_| ())?;
				}

				MonoTypeDefKind::TypeAlias { .. } => {}
			}
		}

		for nt in &collected.tuples {
			let MonoTy::Tuple(elems) = nt else { continue };
			writeln!(out, "typedef struct {0} {0};", tuple_type_name(elems)).map_err(|_| ())?;
		}

		for td in &collected.types {
			match &td.kind {
				MonoTypeDefKind::Variant { members }
					if td.is_option_variant(input.module.option_symbol)
						&& members.iter().any(|(name, ty)| {
							return name == "Some" && matches!(ty, Some(MonoTy::Pointer { .. }));
						}) =>
				{
					for (name, oty) in members {
						if name != "Some" {
							continue;
						}
						if let Some(ty) = oty {
							writeln!(out, "typedef {} {};", mono_ty_to_string(ty), td.mangled_name).map_err(|_| ())?;
						}
					}
				}
				MonoTypeDefKind::TypeAlias { ty } => {
					writeln!(out, "typedef {} {};", mono_ty_to_string(ty), td.mangled_name).map_err(|_| ())?;
				}
				MonoTypeDefKind::Struct { .. }
				| MonoTypeDefKind::Union { .. }
				| MonoTypeDefKind::Enum { .. }
				| MonoTypeDefKind::Variant { .. } => {}
			}
		}

		for nt in &collected.tuples {
			let MonoTy::Tuple(elems) = nt else { continue };
			writeln!(out, "struct {} {{", tuple_type_name(elems)).map_err(|_| ())?;
			for (i, e) in elems.iter().enumerate() {
				writeln!(out, "\t{} _{};", mono_ty_to_string(e), i).map_err(|_| ())?;
			}
			writeln!(out, "}};").map_err(|_| ())?;
		}

		for td in &collected.types {
			match &td.kind {
				MonoTypeDefKind::Struct { fields } => {
					writeln!(out, "struct {} {{", td.mangled_name).map_err(|_| ())?;
					for (name, ty) in fields {
						writeln!(out, "\t{} {};", mono_ty_to_string(ty), name).map_err(|_| ())?;
					}
					writeln!(out, "}};").map_err(|_| ())?;
				}
				MonoTypeDefKind::Union { fields } => {
					writeln!(out, "union {} {{", td.mangled_name).map_err(|_| ())?;
					for (name, ty) in fields {
						writeln!(out, "\t{} {};", mono_ty_to_string(ty), name).map_err(|_| ())?;
					}
				}
				MonoTypeDefKind::Enum { variants } => {
					writeln!(out, "enum {} {{", td.mangled_name).map_err(|_| ())?;
					for (name, init) in variants {
						writeln!(out, "\t{},", name).map_err(|_| ())?;
						if let Some(ci) = init {
							if let Some(cb) = input.module.const_bodies.get(ci.0 as usize) {
								if let Some(value) = try_eval_simple_const(cb) {
									match &value.value {
										MirLiteralValue::Literal(literal) => match literal {
											lit @ Literal::Int { .. } => {
												write!(
													out,
													" = {}",
													read_radix_number(lit)
														.expect("literal should be valid, make better error")
												)
												.map_err(|_| ())?;
											}
											Literal::Float { .. }
											| Literal::Bool { .. }
											| Literal::String { .. }
											| Literal::Char { .. } => todo!("enum value should be an int"),
										},
										MirLiteralValue::ZeroInit => {
											unreachable!("should not be able to happen, make better error")
										}
										MirLiteralValue::Undef => {}
										MirLiteralValue::ConstBody(_) => todo!("ConstBodyEval"),
									}
								} else {
									todo!("ConstBodyEval")
								}
							} else {
								todo!("ConstBodyId was not a valid index")
							}
						}
					}
					writeln!(out, "}};").map_err(|_| ())?;
				}
				MonoTypeDefKind::Variant { members }
					if td.is_option_variant(input.module.option_symbol)
						&& members.iter().any(|(name, ty)| {
							return name == "Some" && matches!(ty, Some(MonoTy::Pointer { .. }));
						}) =>
				{
					// a Option<*T>, so no declaration needed
				}
				MonoTypeDefKind::Variant { members } => {
					writeln!(out, "struct {} {{", td.mangled_name).map_err(|_| ())?;
					{
						writeln!(out, "\tunion {{").map_err(|_| ())?;
						for (name, oty) in members {
							if let Some(ty) = oty {
								writeln!(out, "\t\t{} {};", mono_ty_to_string(ty), name).map_err(|_| ())?;
							}
						}
						writeln!(out, "\t}} data;").map_err(|_| ())?;

						writeln!(out, "\tenum {}Tag {{", td.mangled_name).map_err(|_| ())?;
						for (name, _) in members {
							writeln!(out, "\t\t{}_{},", td.mangled_name, name).map_err(|_| ())?;
						}
						writeln!(out, "\t}} tag;").map_err(|_| ())?;
					}
					writeln!(out, "}};").map_err(|_| ())?;
				}

				MonoTypeDefKind::TypeAlias { .. } => {}
			}
		}
		return Ok(());
	}

	fn write_globals(&mut self, collected: &Collected, input: &BackendInput<'_>, out: &mut impl Write)
	-> Result<(), ()>
	{
		for global in &collected.globals {
			if matches!(&global.ty, MonoTy::Tuple(t) if t.is_empty()) {
				continue;
			}
			write!(out, "{} {}", mono_ty_to_string(&global.ty), global.mangled_name).map_err(|_| ())?;
			if let Some(cb) = input.module.const_bodies.get(global.init.0 as usize) {
				if let Some(value) = try_eval_simple_const(cb) {
					match &value.value {
						MirLiteralValue::Literal(literal) => match literal {
							lit @ Literal::Int { .. } => {
								write!(
									out,
									" = {}",
									read_radix_number(lit).expect("literal should be valid, make better error")
								)
								.map_err(|_| ())?;
							}
							Literal::Float { .. }
							| Literal::Bool { .. }
							| Literal::String { .. }
							| Literal::Char { .. } => todo!("enum value should be an int"),
						},
						MirLiteralValue::ZeroInit => {
							unreachable!("should not be able to happen, make better error")
						}
						MirLiteralValue::Undef => {}
						MirLiteralValue::ConstBody(_) => todo!("ConstBodyEval"),
					}
				} else {
					todo!("ConstBodyEval")
				}
			} else {
				todo!("ConstBodyId was not a valid index")
			}
		}
		return Ok(());
	}
	fn write_function_prototypes(
		&mut self,
		collected: &Collected,
		input: &BackendInput<'_>,
		out: &mut impl Write,
	) -> Result<(), ()>
	{
		for f in &collected.functions {
			self.write_function_prototype(f, input, out)?;
			writeln!(out, ";").map_err(|_| ())?;
		}
		return Ok(());
	}

	fn write_function_prototype(
		&mut self,
		f: &MonoFunction,
		input: &BackendInput<'_>,
		out: &mut impl Write,
	) -> Result<(), ()>
	{
		write_span(f.span(), input.source_map, out, input.options).map_err(|_| ())?;
		if f.body.is_some() && f.mangled_name != "main" {
			write!(out, "static ").map_err(|_| ())?;
		}
		if f.body.is_none() {
			write!(out, "extern ").map_err(|_| ())?;
		}
		if let Some(ty) = &f.return_ty {
			write!(out, "{} ", mono_ty_to_string(ty)).map_err(|_| ())?;
		} else {
			write!(out, "void ").map_err(|_| ())?;
		}
		write!(out, "{} (", f.mangled_name).map_err(|_| ())?;
		if f.params.is_empty() {
			write!(out, "void").map_err(|_| ())?;
		} else {
			write!(out, "{} {}", mono_ty_to_string(&f.params[0].ty), f.params[0].name).map_err(|_| ())?;
			for arg in &f.params[1..] {
				write!(out, ", {} {}", mono_ty_to_string(&arg.ty), arg.name).map_err(|_| ())?;
			}
			if matches!(
				input.symbols.symbol(f.symbol).kind,
				SymbolKind::Function { variadic: true, .. }
			) {
				write!(out, ", ...").map_err(|_| ())?;
			}
		}
		write!(out, ")").map_err(|_| ())?;

		return Ok(());
	}

	fn write_functions(
		&mut self,
		collected: &Collected,
		input: &BackendInput<'_>,
		out: &mut impl Write,
	) -> Result<(), ()>
	{
		for f in &collected.functions {
			let Some(body) = &f.body else {
				continue;
			};

			self.write_function_prototype(f, input, out)?;
			writeln!(out, " {{").map_err(|_| ())?;
			let tag_local_types: HashMap<LocalId, String> = collect_discriminant_local_types(body);

			for local in &body.locals {
				if matches!(&local.ty, MonoTy::Tuple(t) if t.is_empty()) {
					continue;
				}
				write_span(local.span(), input.source_map, out, input.options).map_err(|_| ())?;

				let ty_str: String = tag_local_types
					.get(&local.id)
					.map_or_else(|| return mono_ty_to_string(&local.ty), |tag_ty| return tag_ty.clone());
				write!(out, "\t{} ", ty_str).map_err(|_| ())?;

				if let Some(name) = &local.name {
					let n = format!("{}_{}", name.replace('#', "leaf_tmp_"), local.id.0);
					write!(out, "{}", n).map_err(|_| ())?;
				} else {
					write!(out, "leaf_local_tmp_{}", local.id.0).map_err(|_| ())?;
				}
				writeln!(out, ";").map_err(|_| ())?;
			}

			for block in &body.blocks {
				self.write_basic_block(block, f, input, out)?;
			}

			writeln!(out, "}}").map_err(|_| ())?;
		}
		return Ok(());
	}

	fn write_basic_block(
		&mut self,
		block: &MonoBasicBlock,
		f: &MonoFunction,
		input: &BackendInput<'_>,
		out: &mut impl Write,
	) -> Result<(), ()>
	{
		writeln!(out, "bb{}: {{ (void)0;", block.id.0).map_err(|_| ())?;

		for stmt in &block.stmts {
			match stmt {
				MonoStmt::Assign { place, rvalue, span } => {
					write_span(*span, input.source_map, out, input.options).map_err(|_| ())?;
					write!(out, "\t").map_err(|_| ())?;
					self.write_place(place, f, input, out).map_err(|_| ())?;
					write!(out, " = ").map_err(|_| ())?;
					self.write_rvalue(rvalue, f, input, out).map_err(|_| ())?;
					writeln!(out, ";").map_err(|_| ())?;

					if let MonoRvalue::Aggregate {
						kind: MonoAggregateKind::VariantMember {
							parent_mangled, member, ..
						},
						..
					} = rvalue
					{
						let is_opt_ptr = input.module.items.iter().any(|item| {
							let MonoItem::TypeDef(td) = item else { return false };
							if &td.mangled_name != parent_mangled {
								return false;
							}
							return td.is_option_variant(input.module.option_symbol)
								&& matches!(&td.kind, MonoTypeDefKind::Variant { members }
									if members.iter().any(|(n, ty)|
										return n == "Some"
										&& matches!(ty, Some(MonoTy::Pointer { .. }))));
						});
						if !is_opt_ptr {
							write!(out, "\t").map_err(|_| ())?;
							self.write_place(place, f, input, out).map_err(|_| ())?;
							writeln!(out, ".tag = {}_{};", parent_mangled, member).map_err(|_| ())?;
						}
					}
				}

				MonoStmt::Call { callee, args, span } => {
					write_span(*span, input.source_map, out, input.options).map_err(|_| ())?;
					write!(out, "\t").map_err(|_| ())?;
					self.write_callee(callee, f, out).map_err(|_| ())?;
					write!(out, "(").map_err(|_| ())?;
					for (i, arg) in args.iter().enumerate() {
						if i > 0 {
							write!(out, ", ").map_err(|_| ())?;
						}
						self.write_operand(arg, f, input, out).map_err(|_| ())?;
					}
					writeln!(out, ");").map_err(|_| ())?;
				}

				MonoStmt::Delete { span, .. } => self
					.diagnostics
					.push(compiler_bug!(*span, "`MonoStmt::Delete` should not be in the backend")),
				MonoStmt::Nop => {}
			}
		}

		match &block.terminator {
			MonoTerminator::Goto { target } => {
				writeln!(out, "\tgoto bb{};", target.0).map_err(|_| ())?;
			}

			MonoTerminator::Branch {
				cond,
				then_block,
				else_block,
			} => {
				write!(out, "\tif (").map_err(|_| ())?;
				self.write_operand(cond, f, input, out).map_err(|_| ())?;
				writeln!(out, ") goto bb{}; else goto bb{};", then_block.0, else_block.0).map_err(|_| ())?;
			}

			MonoTerminator::CallAndContinue {
				callee,
				args,
				dest,
				next,
				unwind: _,
				span,
			} => {
				write_span(*span, input.source_map, out, input.options).map_err(|_| ())?;
				let dest_is_unit: bool = matches!(&dest.ty, MonoTy::Tuple(t) if t.is_empty());
				write!(out, "\t").map_err(|_| ())?;
				if !dest_is_unit {
					self.write_place(dest, f, input, out).map_err(|_| ())?;
					write!(out, " = ").map_err(|_| ())?;
				}
				self.write_callee(callee, f, out).map_err(|_| ())?;
				write!(out, "(").map_err(|_| ())?;
				for (i, arg) in args.iter().enumerate() {
					if i > 0 {
						write!(out, ", ").map_err(|_| ())?;
					}
					self.write_operand(arg, f, input, out).map_err(|_| ())?;
				}
				writeln!(out, ");").map_err(|_| ())?;
				writeln!(out, "\tgoto bb{};", next.0).map_err(|_| ())?;
			}

			MonoTerminator::Return => {
				if let Some(ret) = f.body.as_ref().and_then(|b| b.return_local) {
					writeln!(out, "\treturn {};", local_name(f, ret)).map_err(|_| ())?;
				} else {
					writeln!(out, "\treturn;").map_err(|_| ())?;
				}
			}

			MonoTerminator::Unreachable => {
				writeln!(out, "\t__builtin_unreachable();").map_err(|_| ())?;
			}

			MonoTerminator::Switch {
				scrutinee,
				arms,
				otherwise,
			} => {
				write!(out, "\tswitch (").map_err(|_| ())?;
				self.write_operand(scrutinee, f, input, out).map_err(|_| ())?;
				writeln!(out, ") {{").map_err(|_| ())?;
				for arm in arms {
					write!(out, "\t\tcase ").map_err(|_| ())?;
					self.write_operand(&arm.value, f, input, out).map_err(|_| ())?;
					writeln!(out, ": goto bb{};", arm.target.0).map_err(|_| ())?;
				}
				writeln!(out, "\t\tdefault: goto bb{};", otherwise.0).map_err(|_| ())?;
				writeln!(out, "\t}}").map_err(|_| ())?;
			}
		}

		writeln!(out, "}}").map_err(|_| ())?;
		return Ok(());
	}

	fn write_callee(&mut self, callee: &MonoCallee, f: &MonoFunction, out: &mut impl Write) -> std::fmt::Result
	{
		return match callee {
			MonoCallee::Direct { mangled_name, .. } => write!(out, "{}", mangled_name),
			MonoCallee::Indirect(l) => write!(out, "{}", local_name(f, *l)),
			MonoCallee::Intrinsic(_) => {
				self.diagnostics.push(compiler_not_implemented!(
					Span::default(),
					"intrinsics not yet implemented"
				));
				// TODO: real intrinsic lowering
				write!(out, "/* intrinsic */ (void)")
			}
		};
	}

	fn write_place(
		&mut self,
		place: &MonoPlace,
		f: &MonoFunction,
		input: &BackendInput<'_>,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		match &place.base {
			MonoPlaceBase::Local(l) => {
				write!(out, "{}", local_name(f, *l))?;
			}
			MonoPlaceBase::Global(sym) => {
				write!(out, "{}", global_mangled_name(input, *sym))?;
			}
		}

		let base_ty: &MonoTy = match &place.base {
			MonoPlaceBase::Local(l) => {
				let body = f.body.as_ref().expect("local reference outside a function body");
				let local = body
					.locals
					.iter()
					.find(|loc| return loc.id == *l)
					.expect("unknown local id");
				&local.ty
			}
			MonoPlaceBase::Global(sym) => input
				.module
				.items
				.iter()
				.find_map(|item| {
					return if let MonoItem::Global(g) = item
						&& g.symbol == *sym
					{
						Some(&g.ty)
					} else {
						None
					};
				})
				.expect("unknown global symbol"),
		};
		let mut current_ty = base_ty;

		for proj in &place.projections {
			match proj {
				MonoProjection::Field { name, ty: field_ty } => {
					match current_ty {
						MonoTy::Tuple(_) => {
							write!(out, "._{}", name)?;
						}

						MonoTy::Named { symbol, type_args, .. } => {
							if option_ptr_variant_typedef(*symbol, type_args, input).is_some() {
							} else {
								let mut is_variant = false;
								for item in &input.module.items {
									if let MonoItem::TypeDef(td) = item
										&& td.symbol == *symbol && td.type_args == *type_args
									{
										if matches!(td.kind, MonoTypeDefKind::Variant { .. }) {
											is_variant = true;
										}
										break;
									}
								}

								if is_variant {
									write!(out, ".data.{}", name)?;
								} else {
									write!(out, ".{}", name)?;
								}
							}
						}

						_ => {
							write!(out, ".{}", name)?;
						}
					}

					current_ty = field_ty;
				}

				MonoProjection::Index { index, .. } => {
					write!(out, "[{}]", local_name(f, *index))?;
				}

				MonoProjection::Deref => {
					write!(out, "[0]")?; // TODO: I don't like it, but it should work
				}
			}
		}

		Ok(())
	}

	fn write_operand(
		&mut self,
		op: &MonoOperand,
		f: &MonoFunction,
		input: &BackendInput<'_>,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return match op {
			MonoOperand::Copy(p) | MonoOperand::Move(p) => self.write_place(p, f, input, out),
			MonoOperand::Const(lit) => self.write_literal(lit, out),
		};
	}

	fn write_literal(&mut self, lit: &MonoLiteral, out: &mut impl Write) -> std::fmt::Result
	{
		return match &lit.value {
			MirLiteralValue::Literal(literal) => match literal {
				Literal::Int { .. } => {
					let n: i128 = read_radix_number(literal).expect("literal should be valid, make better error");
					write!(out, "{}", n)
				}
				Literal::Float { value, bits, span: _ } => {
					write!(out, "{}", value)?;
					if let Some(b) = bits {
						match b {
							32 => {
								write!(out, "f")?;
							}
							64 => {
								write!(out, "F")?;
							}
							_ => {
								unreachable!()
							}
						}
					}
					Ok(())
				}
				Literal::Bool { value, .. } => write!(out, "{}", if *value { "true" } else { "false" }),
				Literal::Char { value, .. } => write!(out, "'{}'", value),
				Literal::String { value, flags, .. } => {
					if flags.contains_all(StringFlags::CSTRING) {
						write!(out, "{:?}", value)?;
					} else {
						write!(out, "(LeafStr){{ .data = {:?}, .len = {} }}", value, value.len())?;
					}

					Ok(())
				}
			},
			MirLiteralValue::ZeroInit => write!(out, "{{0}}"),
			MirLiteralValue::Undef => write!(out, "/* undef */ {{0}}"),
			MirLiteralValue::ConstBody(_) => write!(out, "/* TODO: const body eval */ {{0}}"),
		};
	}

	fn write_rvalue(
		&mut self,
		rvalue: &MonoRvalue,
		f: &MonoFunction,
		input: &BackendInput<'_>,
		out: &mut impl Write,
	) -> std::fmt::Result
	{
		return match rvalue {
			MonoRvalue::Use(op) => self.write_operand(op, f, input, out),

			MonoRvalue::Unary { op, operand } => {
				write!(out, "{}", unary_op_to_string(*op))?;
				self.write_operand(operand, f, input, out)
			}

			MonoRvalue::Binary { op, lhs, rhs } => {
				self.write_operand(lhs, f, input, out)?;
				write!(out, " {} ", binary_op_to_string(*op))?;
				self.write_operand(rhs, f, input, out)
			}

			MonoRvalue::Cast { ty, operand } => {
				write!(out, "({})", mono_ty_to_string(ty))?;
				self.write_operand(operand, f, input, out)
			}

			MonoRvalue::Ref { place, .. } | MonoRvalue::RawPtr { place, .. } => {
				write!(out, "&")?;
				self.write_place(place, f, input, out)
			}

			MonoRvalue::Aggregate { kind, fields } => {
				if let MonoAggregateKind::VariantMember {
					parent_mangled, member, ..
				} = kind && let Some(_td) = input.module.items.iter().find_map(|item| {
					let MonoItem::TypeDef(td) = item else { return None };
					if &td.mangled_name != parent_mangled {
						return None;
					}
					let is_opt_ptr = td.is_option_variant(input.module.option_symbol)
						&& matches!(&td.kind, MonoTypeDefKind::Variant { members }
					if members.iter().any(|(n, ty)| n == "Some" && matches!(ty, Some(MonoTy::Pointer { .. }))));
					return if is_opt_ptr { Some(td) } else { None };
				}) {
					return if member == "Some" {
						let (_, op) = fields.first().expect("Option::Some(*T) must have exactly one field");
						self.write_operand(op, f, input, out)
					} else {
						write!(out, "nullptr")
					};
				}

				let mut is_tuple: bool = false;
				let name = match kind {
					MonoAggregateKind::Struct { mangled_name, .. } | MonoAggregateKind::Union { mangled_name, .. } => {
						mangled_name.clone()
					}
					MonoAggregateKind::VariantMember { parent_mangled, .. } => parent_mangled.clone(),
					MonoAggregateKind::Tuple => {
						is_tuple = true;
						String::new()
					}
				};
				if name.is_empty() {
					write!(out, "{{ ")?;
				} else {
					write!(out, "({}){{ ", name)?;
				}
				for (i, (fname, op)) in fields.iter().enumerate() {
					if i > 0 {
						write!(out, ", ")?;
					}
					match kind {
						MonoAggregateKind::VariantMember { member, .. } => {
							write!(out, ".data.{} = ", member)?;
						}
						_ => {
							if is_tuple {
								write!(out, "._{} = ", fname)?;
							} else {
								write!(out, ".{} = ", fname)?;
							}
						}
					}
					self.write_operand(op, f, input, out)?;
				}
				write!(out, " }}")
			}

			MonoRvalue::Array { elements, .. } => {
				write!(out, "{{ ")?;
				for (i, e) in elements.iter().enumerate() {
					if i > 0 {
						write!(out, ", ")?;
					}
					self.write_operand(e, f, input, out)?;
				}
				write!(out, " }}")
			}

			MonoRvalue::ArrayRepeat { value, count, .. } => {
				let cb = &input.module.const_bodies[count.0 as usize];
				let repeats = if let Some(MonoLiteral {
					value: MirLiteralValue::Literal(lit @ Literal::Int { .. }),
					ty: _,
				}) = try_eval_simple_const(cb)
				{
					read_radix_number(lit).unwrap_or_else(|e| {
						self.diagnostics.push(*e);
						return 1;
					})
				} else {
					todo!()
				};
				write!(out, "{{ ")?;
				for i in 0..repeats {
					if i > 0 {
						write!(out, ", ")?;
					}
					self.write_operand(value, f, input, out)?;
				}
				write!(out, " }}")
			}

			MonoRvalue::Tuple(elems) => {
				let tys: Vec<MonoTy> = elems.iter().map(|op| operand_ty(op, f, input)).collect();
				write!(out, "({}){{ ", tuple_type_name(&tys))?;
				for (i, e) in elems.iter().enumerate() {
					if i > 0 {
						write!(out, ", ")?;
					}
					write!(out, "._{} = ", i)?;
					self.write_operand(e, f, input, out)?;
				}
				write!(out, " }}")
			}

			MonoRvalue::Discriminant(place) => {
				if ty_is_option_ptr_variant(&place.ty, input).is_some() {
					write!(out, "(")?;
					self.write_place(place, f, input, out)?;
					write!(out, " != nullptr)")
				} else {
					self.write_place(place, f, input, out)?;
					write!(out, ".tag")
				}
			}
		};
	}
}

fn write_span(span: Span, source_map: &SourceMap, out: &mut impl Write, options: &BackendOptions) -> std::fmt::Result
{
	if options.debug_info {
		if let Some(file) = source_map.get(span.source_index) {
			writeln!(out, "#line {} \"{}\"", span.start_line, file.path.display())?;
		}
	}
	return Ok(());
}

fn mono_ty_to_string(ty: &MonoTy) -> String
{
	return match ty {
		MonoTy::Primitive(p) => primitive_to_string(p),

		MonoTy::Named { mangled_name, .. } => mangled_name.clone(),

		MonoTy::Reference { inner, .. } | MonoTy::Pointer { inner, .. }
			if matches!(**inner, MonoTy::Primitive(Primitive::Str)) =>
		{
			"LeafStr".to_string()
		}

		MonoTy::Reference { inner, .. } | MonoTy::Pointer { inner, .. }
			if matches!(**inner, MonoTy::Primitive(Primitive::CStr)) =>
		{
			"const char*".to_string()
		}

		MonoTy::Reference { inner, .. } | MonoTy::Pointer { inner, .. }
			if matches!(**inner, MonoTy::Array { size: None, .. }) =>
		{
			"LeafArray".to_string()
		}

		MonoTy::Reference { inner, .. } | MonoTy::Pointer { inner, .. }
			if matches!(**inner, MonoTy::Array { size: Some(_), .. }) =>
		{
			let MonoTy::Array { size: Some(n), .. } = &**inner else {
				unreachable!()
			};
			format!("LeafArray_{n}")
		}

		MonoTy::Reference { mutable, inner } | MonoTy::Pointer { mutable, inner } => {
			format!(
				"{}*{}",
				mono_ty_to_string(inner),
				if *mutable { "" } else { " /*const*/" }
			)
		}

		MonoTy::Array { inner, size } => size.as_ref().map_or_else(
			|| format!("{}[]", mono_ty_to_string(inner)),
			|n| format!("{}[{}]", mono_ty_to_string(inner), n),
		),

		MonoTy::Tuple(elems) => {
			if elems.is_empty() {
				"void".to_string()
			} else {
				tuple_type_name(elems)
			}
		}
	};
}

fn primitive_to_string(p: &Primitive) -> String
{
	use crate::lexer::{IntSign, IntSize, IntType};
	return match p {
		Primitive::Bool => "bool".to_string(),
		Primitive::Char => "char".to_string(),
		Primitive::F32 => "float".to_string(),
		Primitive::F64 => "double".to_string(),
		Primitive::Str => "LeafStr".to_string(),
		Primitive::CStr => "const char".to_string(),
		Primitive::Int(IntType { bits, sign }) => {
			let prefix = match sign {
				IntSign::Signed => "int",
				IntSign::Unsigned => "uint",
			};
			match bits {
				IntSize::Fixed(8) => format!("{prefix}8_t"),
				IntSize::Fixed(16) => format!("{prefix}16_t"),
				IntSize::Fixed(32) => format!("{prefix}32_t"),
				IntSize::Fixed(64) => format!("{prefix}64_t"),
				IntSize::Fixed(n) => {
					let prefix = match sign {
						IntSign::Signed => "signed",
						IntSign::Unsigned => "unsigned",
					};
					format!("{prefix} _BitInt({n})")
				}
				IntSize::Size => match sign {
					IntSign::Signed => "intptr_t".to_string(),
					IntSign::Unsigned => "size_t".to_string(),
				},
			}
		}
	};
}

fn tuple_type_name(elems: &[MonoTy]) -> String
{
	let parts: Vec<String> = elems.iter().map(mono_ty_to_string).collect();
	return format!("LeafTupple{}_{}", parts.len(), parts.join("_"));
}

fn collect_tuples(ty: &MonoTy, out: &mut Vec<MonoTy>)
{
	match ty {
		MonoTy::Tuple(elems) if !elems.is_empty() => {
			for e in elems {
				collect_tuples(e, out);
			}
			if !out.iter().any(|t| return t == ty) {
				out.push(ty.clone());
			}
		}
		MonoTy::Reference { inner, .. } | MonoTy::Pointer { inner, .. } | MonoTy::Array { inner, .. } => {
			collect_tuples(inner, out);
		}
		_ => {}
	}
}

fn try_eval_simple_const(cb: &MonoConstBody) -> Option<&MonoLiteral>
{
	let block0: &crate::monomorphization::MonoBasicBlock = cb.body.blocks.first()?;
	for stmt in &block0.stmts {
		if let MonoStmt::Assign {
			place,
			rvalue: MonoRvalue::Use(MonoOperand::Const(lit)),
			..
		} = stmt && matches!(place.base, MonoPlaceBase::Local(id) if id == cb.result)
		{
			return Some(lit);
		}
	}
	return None;
}

fn local_name(f: &MonoFunction, id: LocalId) -> String
{
	let body: &MonoBody = f.body.as_ref().expect("local reference outside a function body");
	let local: &MonoLocal = body
		.locals
		.iter()
		.find(|l| return l.id == id)
		.expect("unknown local id");
	if let Some(name) = &local.name {
		return format!("{}_{}", name.replace('#', "leaf_tmp_"), id.0);
	}
	return format!("leaf_local_tmp_{}", id.0);
}

fn global_mangled_name(input: &BackendInput<'_>, sym: SymbolId) -> String
{
	for item in &input.module.items {
		if let MonoItem::Global(g) = item
			&& g.symbol == sym
		{
			return g.mangled_name.clone();
		}
	}
	unreachable!("reachable global symbol has no MonoGlobal entry")
}

const fn unary_op_to_string(op: UnaryOp) -> &'static str
{
	return match op {
		UnaryOp::Neg => "-",
		UnaryOp::Not => "!",
		UnaryOp::Deref => "*",
		UnaryOp::Addr { mutable: _ } => "&",
	};
}

const fn binary_op_to_string(op: BinaryOp) -> &'static str
{
	return match op {
		BinaryOp::Add => "+",
		BinaryOp::Sub => "-",
		BinaryOp::Mul => "*",
		BinaryOp::Div => "/",
		BinaryOp::BitAnd => "&",
		BinaryOp::BitOr => "|",
		BinaryOp::BitXor => "^",
		BinaryOp::Shl => "<<",
		BinaryOp::Shr => ">>",
		BinaryOp::Eq => "==",
		BinaryOp::Ne => "!=",
		BinaryOp::Lt => "<",
		BinaryOp::Le => "<=",
		BinaryOp::Gt => ">",
		BinaryOp::Ge => ">=",
		BinaryOp::LogicalOr => "||",
		BinaryOp::LogicalAnd => "&&",
		BinaryOp::Mod => "%",
	};
}

fn collect_discriminant_local_types(body: &MonoBody) -> HashMap<LocalId, String>
{
	let mut map: HashMap<LocalId, String> = HashMap::new();
	for block in &body.blocks {
		for stmt in &block.stmts {
			let MonoStmt::Assign {
				place,
				rvalue: MonoRvalue::Discriminant(disc_place),
				..
			} = stmt
			else {
				continue;
			};
			if !place.projections.is_empty() {
				continue;
			}
			let MonoPlaceBase::Local(lid) = &place.base else {
				continue;
			};

			let final_ty: &MonoTy = resolve_place_ty(disc_place);
			if let MonoTy::Named { mangled_name, .. } = final_ty {
				map.insert(*lid, format!("{}Tag", mangled_name));
			}
		}
	}
	return map;
}

fn resolve_place_ty(place: &MonoPlace) -> &MonoTy
{
	if let Some(last) = place.projections.last()
		&& let MonoProjection::Field { ty, .. } = last
	{
		return ty;
	}
	return &place.ty;
}

fn operand_ty<'a>(op: &'a MonoOperand, f: &'a MonoFunction, input: &'a BackendInput<'_>) -> MonoTy
{
	return match op {
		MonoOperand::Copy(p) | MonoOperand::Move(p) => p.ty.clone(),
		MonoOperand::Const(lit) => lit.ty.clone(),
	};
}

fn option_ptr_variant_typedef<'a>(
	symbol: SymbolId,
	type_args: &[MonoTy],
	input: &'a BackendInput<'_>,
) -> Option<&'a MonoTypeDef>
{
	for item in &input.module.items {
		let MonoItem::TypeDef(td) = item else { continue };
		if td.symbol != symbol || td.type_args != *type_args {
			continue;
		}

		let MonoTypeDefKind::Variant { members } = &td.kind else {
			return None;
		};

		let is_opt_ptr = td.is_option_variant(input.module.option_symbol)
			&& members.iter().any(|(name, ty)| {
				return name == "Some" && matches!(ty, Some(MonoTy::Pointer { .. }));
			});

		return if is_opt_ptr { Some(td) } else { None };
	}
	return None;
}

fn ty_is_option_ptr_variant<'a>(ty: &MonoTy, input: &'a BackendInput<'_>) -> Option<&'a MonoTypeDef>
{
	let MonoTy::Named { symbol, type_args, .. } = ty else {
		return None;
	};
	return option_ptr_variant_typedef(*symbol, type_args, input);
}
