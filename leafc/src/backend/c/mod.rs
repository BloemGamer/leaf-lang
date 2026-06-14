use std::fmt::Write;

use crate::{
	Span,
	backend::{BackendInput, BackendOutput, BackendResult, CompilerBackend, OutputKind},
	diagnostics::DiagnosticBuilder,
	lexer::Spanned,
	monomorphization::{MonoFunction, MonoGlobal, MonoItem, MonoLocal, MonoTy, MonoTypeDef, MonoTypeDefKind},
	source_map::{self, SourceMap},
	type_analysis::Primitive,
};

const DEFAULT_C_HEADERS: &[&str] = &[
	"<stdint.h>", // needed for the basic types
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
		self.write_function_prototypes(&collected, input, &mut out)?;

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
							// a Option<*T>, so no need to forward declare it
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
							writeln!(out, "typedef {} {};", td.mangled_name, mono_ty_to_string(ty)).map_err(|_| ())?;
						}
					}
				}
				MonoTypeDefKind::TypeAlias { ty } => {
					writeln!(out, "typedef {} {};", td.mangled_name, mono_ty_to_string(ty)).map_err(|_| ())?;
				}
				MonoTypeDefKind::Struct { .. }
				| MonoTypeDefKind::Union { .. }
				| MonoTypeDefKind::Enum { .. }
				| MonoTypeDefKind::Variant { .. } => {}
			}
		}

		for td in &collected.types {
			match &td.kind {
				MonoTypeDefKind::Struct { fields } => {
					writeln!(out, "struct {} {{", td.mangled_name).map_err(|_| ())?;
					for (name, ty) in fields {
						writeln!(out, "\t{} _{};", mono_ty_to_string(ty), name).map_err(|_| ())?;
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
						if let Some(_i) = init {
							todo!()
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
						writeln!(out, "\t}} data").map_err(|_| ())?;

						writeln!(out, "\tenum {{").map_err(|_| ())?;
						for (name, _) in members {
							writeln!(out, "\t\t{}_{},", td.mangled_name, name).map_err(|_| ())?;
						}
						writeln!(out, "\t}} tag").map_err(|_| ())?;
					}
					writeln!(out, "}};").map_err(|_| ())?;
				}

				MonoTypeDefKind::TypeAlias { .. } => {}
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
			write_span(f.span(), input.source_map, out).map_err(|_| ())?;
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
			if f.type_args.is_empty() {
				write!(out, "void").map_err(|_| ())?;
			} else {
				write!(out, "{} {}", mono_ty_to_string(&f.params[0].ty), f.params[0].name).map_err(|_| ())?;
				for arg in &f.params[1..] {
					write!(out, ", {} {}", mono_ty_to_string(&arg.ty), arg.name).map_err(|_| ())?;
				}
			}
			writeln!(out, ");").map_err(|_| ())?;
		}
		return Ok(());
	}
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
			format!("{}{}*", if *mutable { "mut " } else { "" }, mono_ty_to_string(inner))
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

fn write_span(span: Span, source_map: &SourceMap, out: &mut impl Write) -> std::fmt::Result
{
	if let Some(file) = source_map.get(span.source_index) {
		writeln!(out, "#line {} {}", span.start_line, file.path.display());
	}
	return Ok(());
}
