// Correctness / Safety
#![warn(clippy::absurd_extreme_comparisons)]
#![warn(clippy::bool_assert_comparison)]
#![warn(clippy::cast_ptr_alignment)]
#![warn(clippy::cast_slice_from_raw_parts)]
#![warn(clippy::collapsible_if)]
#![warn(clippy::cmp_owned)]
#![warn(clippy::eq_op)]
#![warn(clippy::float_cmp)]
#![warn(clippy::float_cmp_const)]
#![warn(clippy::identity_op)]
#![warn(clippy::implicit_clone)]
#![warn(clippy::implicit_hasher)]
#![warn(clippy::infallible_destructuring_match)]
#![warn(clippy::invalid_upcast_comparisons)]
#![warn(clippy::if_same_then_else)]
#![warn(clippy::manual_assert)]
#![warn(clippy::manual_memcpy)]
#![warn(clippy::manual_range_contains)]
#![warn(clippy::match_like_matches_macro)]
#![warn(clippy::match_same_arms)]
#![warn(clippy::multiple_unsafe_ops_per_block)]
#![warn(clippy::op_ref)]
#![warn(clippy::ptr_as_ptr)]
#![warn(clippy::ptr_offset_with_cast)]
#![warn(clippy::redundant_pattern_matching)]
#![warn(clippy::redundant_pub_crate)]
#![warn(clippy::same_item_push)]
#![warn(clippy::single_match)]
#![warn(clippy::trait_duplication_in_bounds)]
#![warn(clippy::transmute_ptr_to_ptr)]
#![warn(clippy::unchecked_time_subtraction)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(clippy::unused_async)]
#![warn(clippy::useless_conversion)]
#![warn(clippy::while_let_loop)]
#![warn(clippy::zero_prefixed_literal)]
#![warn(clippy::suspicious)]
// Error Handling
#![warn(clippy::fallible_impl_from)]
#![warn(clippy::from_over_into)]
#![warn(clippy::manual_ok_or)]
#![warn(clippy::manual_unwrap_or)]
#![warn(clippy::panic)]
#![warn(clippy::unnecessary_wraps)]
#![warn(clippy::unwrap_used)]
#![cfg_attr(test, allow(clippy::panic, clippy::unwrap_used))]
// Performance / Allocation
#![warn(clippy::borrowed_box)]
#![warn(clippy::box_collection)]
#![warn(clippy::large_stack_arrays)]
#![warn(clippy::naive_bytecount)]
#![warn(clippy::needless_borrow)]
#![warn(clippy::needless_collect)]
#![warn(clippy::needless_pass_by_value)]
#![warn(clippy::or_fun_call)]
#![warn(clippy::rc_buffer)]
#![warn(clippy::rc_mutex)]
#![warn(clippy::redundant_allocation)]
#![warn(clippy::redundant_clone)]
#![warn(clippy::slow_vector_initialization)]
#![warn(clippy::trivially_copy_pass_by_ref)]
#![warn(clippy::inefficient_to_string)]
#![warn(clippy::result_large_err)]
#![warn(clippy::useless_vec)]
#![warn(clippy::vec_box)]
#![warn(clippy::unsound_collection_transmute)]
#![warn(clippy::needless_range_loop)]
#![warn(clippy::iter_skip_zero)]
// Iterator
#![warn(clippy::manual_flatten)]
#![warn(clippy::manual_map)]
#![warn(clippy::needless_for_each)]
// Control Flow / Code Structure
#![warn(clippy::branches_sharing_code)]
#![warn(clippy::match_bool)]
#![warn(clippy::match_wildcard_for_single_variants)]
#![warn(clippy::never_loop)]
#![warn(clippy::redundant_guards)]
#![warn(clippy::unnested_or_patterns)]
// Style
#![warn(clippy::enum_glob_use)]
#![warn(clippy::implicit_return)]
#![warn(clippy::items_after_statements)]
#![warn(clippy::let_underscore_untyped)]
#![warn(clippy::mixed_read_write_in_expression)]
#![warn(clippy::module_name_repetitions)]
#![warn(clippy::redundant_static_lifetimes)]
#![warn(clippy::rest_pat_in_fully_bound_structs)]
// #![warn(clippy::self_named_module_files)]
#![warn(clippy::shadow_reuse)]
#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::style)]
#![warn(clippy::verbose_bit_mask)]
// Docs
#![warn(clippy::doc_markdown)]
#![warn(clippy::missing_errors_doc)]
#![warn(clippy::missing_panics_doc)]
#![warn(clippy::missing_safety_doc)]
// Warning Collections
#![warn(clippy::nursery)]
#![warn(clippy::pedantic)]
// Cargo
// #![warn(clippy::cargo)]
// #![warn(clippy::cargo_common_metadata)]

// Allow
#![allow(clippy::needless_return)]
#![allow(clippy::use_self)]
// #![allow(clippy::result_large_err)] // TODO: in the future, maybe fix all of them
#![allow(clippy::self_named_module_files)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::similar_names)]
#![allow(clippy::uninlined_format_args)]
// #![allow(dead_code)]

// #![warn(clippy::todo)]

use std::{borrow::Cow, error::Error, fs, path};

use self::{
	diagnostics::{Diagnostic, DiagnosticRenderer, TextRenderer},
	parser::Parser,
	source_map::{SourceIndex, SourceMap},
	symbol_collection::{ExportSymbol, ExportTable, SymbolCollectionResult},
	type_analysis::{DependencyTypeExportRef, ExportTypeTable},
};

mod backend;
mod desugar;
mod lexer;
mod mir;
mod monomorphization;
mod name_resolution;
mod parser;
mod symbol_collection;
mod type_analysis;

mod diagnostics;
mod modules;
mod source_map;
mod util;

#[allow(clippy::struct_excessive_bools)]
#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args
{
	#[arg(short, long)]
	lexed: bool,
	#[arg(short, long)]
	parsed: bool,
	#[arg(long)]
	modules: bool,
	#[arg(short, long)]
	desugared: bool,
	#[arg(short, long)]
	symbols: bool,
	#[arg(short, long)]
	name_resolution: bool,
	#[arg(short, long)]
	types: bool,
	#[arg(short, long)]
	mir: bool,
	#[arg(long)]
	mono: bool,

	/// Emit GCC/Clang-compatible C after MIR monomorphization.
	#[arg(long)]
	emit_c: bool,

	/// Output path for --emit-c. Defaults to leaf-out.c.
	#[arg(long, default_value = "leaf-out.c")]
	c_out: path::PathBuf,

	#[arg(short, long)]
	release: bool,
}

impl Args
{
	const fn all_false(&self) -> bool
	{
		return !(self.lexed
			|| self.parsed
			|| self.desugared
			|| self.modules
			|| self.symbols
			|| self.name_resolution
			|| self.types
			|| self.mir
			|| self.mono
			|| self.emit_c);
	}
}

const CORELIB_PATH: &str = "std/core";
const STDLIB_PATH: &str = "std/std";
const PROJECT_ROOT: &str = "leaf-test";

const CORE_CRATE_NAME: &str = "core";
const STD_CRATE_NAME: &str = "std";
const PROJECT_CRATE_NAME: &str = "main";

type CompilerResult<T> = Result<T, Box<dyn Error>>;

#[derive(Debug)]
struct CrateInput
{
	name: String,
	root: path::PathBuf,
}

#[derive(Debug)]
struct CompiledCrate
{
	name: String,
	root: path::PathBuf,
	files: Vec<CompiledFile>,
	diagnostics: Vec<Diagnostic>,
	export_table: ExportTable<'static>,
	type_export_table: ExportTypeTable<'static>,
	c_translation_unit: Option<String>,
}

#[derive(Debug)]
struct CompiledFile
{
	file_id: SourceIndex,
	path: path::PathBuf,
	module_path: Vec<String>,
}

fn main()
{
	if let Err(error) = run().map(|_| ()) {
		eprintln!("error: {error}");
		std::process::exit(1);
	}
}

fn run() -> CompilerResult<(Vec<Diagnostic>, Vec<Diagnostic>)>
{
	let args: Args = <Args as clap::Parser>::parse();
	let mut source_map: SourceMap = SourceMap::new();

	let crate_inputs = [
		CrateInput {
			name: CORE_CRATE_NAME.to_owned(),
			root: path::PathBuf::from(CORELIB_PATH),
		},
		CrateInput {
			name: STD_CRATE_NAME.to_owned(),
			root: path::PathBuf::from(STDLIB_PATH),
		},
		CrateInput {
			name: PROJECT_CRATE_NAME.to_owned(),
			root: path::PathBuf::from(PROJECT_ROOT),
		},
	];

	let mut compiled_crates = Vec::<CompiledCrate>::new();
	let mut diagnostics = Vec::<Diagnostic>::new();
	let module_diagnostics = Vec::<Diagnostic>::new();

	for crate_input in crate_inputs {
		let compiled_crate = compile_crate(&crate_input, &compiled_crates, &mut source_map, &args)?;

		if args.modules {
			print_crate_modules(&compiled_crate);
		}

		diagnostics.extend(compiled_crate.diagnostics.clone());
		compiled_crates.push(compiled_crate);
	}

	if args.emit_c {
		let mut c_source = String::from(backend::emit_prelude());

		for compiled_crate in &compiled_crates {
			if let Some(c_translation_unit) = &compiled_crate.c_translation_unit {
				c_source.push_str("\n/* crate: ");
				c_source.push_str(&compiled_crate.name);
				c_source.push_str(" */\n");
				c_source.push_str(c_translation_unit);
				c_source.push('\n');
			}
		}

		fs::write(&args.c_out, c_source)?;
		println!("wrote C translation unit: {}", args.c_out.display());
	}

	if args.all_false() {
		render_diagnostics(&diagnostics, &source_map);
	}

	return Ok((diagnostics, module_diagnostics));
}

fn compile_crate(
	crate_input: &CrateInput,
	dependencies: &[CompiledCrate],
	source_map: &mut SourceMap,
	args: &Args,
) -> CompilerResult<CompiledCrate>
{
	let files = modules::find_all_files(&crate_input.root, crate_input.name.clone());
	let mut compiled_files = Vec::<CompiledFile>::new();
	let mut crate_diagnostics = Vec::<Diagnostic>::new();
	let mut crate_export_table = ExportTable::default();
	let mut crate_type_export_table = ExportTypeTable::default();
	let mut c_translation_units = Vec::<String>::new();

	for file in files {
		let file_source = fs::read_to_string(&file.path)?;
		let file_name = file.path.to_string_lossy().into_owned();

		// Keep a copy in SourceMap for diagnostics.
		let file_id = source_map.add_file(file_name, file_source.clone());

		// Feed the compiler pipeline from a stable 'static string instead of borrowing
		// from SourceMap. This prevents long-lived HIR/export data from holding an
		// immutable borrow of source_map across loop iterations.
		//
		// This intentionally leaks source text for the duration of the compiler run,
		// which is acceptable for a short-lived compiler process. A cleaner long-term
		// design is to make exported metadata deeply owned instead.
		let lexer_source: &'static str = Box::leak(file_source.into_boxed_str());

		let lexer = lexer::BasicLexer::new(lexer_source, file_id);

		let parser = Parser::new(lexer);
		let (mut parser_diagnostics, program) = parser.parse_program();
		crate_diagnostics.append(&mut parser_diagnostics);

		if args.parsed {
			render_diagnostics(&crate_diagnostics, source_map);
			println!("parsed {}", format_module_path(&file.module_path));
			println!("{program}");
		}

		let (mut desugar_diagnostics, desugared) = desugar::Desugarer::new().lower_program_with_diagnostics(program);
		crate_diagnostics.append(&mut desugar_diagnostics);

		if args.desugared {
			render_diagnostics(&crate_diagnostics, source_map);
			println!("desugared {}", format_module_path(&file.module_path));
			println!("{desugared}");
		}

		let SymbolCollectionResult {
			table: symbols,
			diagnostics: mut symbol_diagnostics,
			exports,
		} = symbol_collection::collect_symbols(&desugared);
		crate_diagnostics.append(&mut symbol_diagnostics);

		merge_export_table(&mut crate_export_table, export_table_to_owned(&exports));

		if args.symbols {
			render_diagnostics(&crate_diagnostics, source_map);
			println!("crate: {}", crate_input.name);
			println!("module: {}", format_module_path(&file.module_path));
			println!("dependencies: {}", format_dependencies(dependencies));
			println!("symbols:\n{symbols:#?}");
			println!("exports:\n{crate_export_table:#?}");
		}

		let dependency_exports = dependencies
			.iter()
			.map(|dependency| name_resolution::DependencyExportRef {
				crate_name: dependency.name.as_str(),
				exports: &dependency.export_table,
			})
			.collect::<Vec<_>>();

		let dependency_type_exports = dependencies
			.iter()
			.map(|dependency| DependencyTypeExportRef {
				crate_name: dependency.name.as_str(),
				type_exports: &dependency.type_export_table,
			})
			.collect::<Vec<_>>();

		let name_resolution::NameResolutionResult {
			program: named_hir,
			diagnostics: mut name_resolution_diagnostics,
		} = name_resolution::resolve_names(&desugared, &symbols, &dependency_exports);
		crate_diagnostics.append(&mut name_resolution_diagnostics);

		if args.name_resolution {
			render_diagnostics(&crate_diagnostics, source_map);
			println!("crate: {}", crate_input.name);
			println!("module: {}", format_module_path(&file.module_path));
			println!("dependencies: {}", format_dependencies(dependencies));
			println!("named HIR:\n{}", named_hir);
		}

		let type_analysis::TypeResolutionResult {
			program: typed_hir,
			diagnostics: mut type_diagnostics,
			exports: type_exports,
		} = type_analysis::resolve_types(&named_hir, &symbols, &dependency_type_exports);
		crate_diagnostics.append(&mut type_diagnostics);

		merge_type_export_table(&mut crate_type_export_table, type_exports.clone());

		if args.types {
			render_diagnostics(&crate_diagnostics, source_map);
			println!("crate: {}", crate_input.name);
			println!("module: {}", format_module_path(&file.module_path));
			println!("dependencies: {}", format_dependencies(dependencies));
			println!("type exports:\n{crate_type_export_table:#?}");
			println!("typed HIR:\n{typed_hir}");
		}

		let mir_program = mir::lower_to_mir(&typed_hir);

		if args.mir {
			render_diagnostics(&crate_diagnostics, source_map);
			println!("crate: {}", crate_input.name);
			println!("module: {}", format_module_path(&file.module_path));
			println!("block MIR:\n{mir_program}");
		}

		let mono_program = monomorphization::monomorphize(
			mir_program,
			monomorphization::MonoOptions {
				emit_c_main: crate_input.name == PROJECT_CRATE_NAME,
			},
		);

		if args.mono {
			render_diagnostics(&crate_diagnostics, source_map);
			println!("crate: {}", crate_input.name);
			println!("module: {}", format_module_path(&file.module_path));
			println!("monomorphized MIR:\n{mono_program}");
		}

		if args.emit_c {
			c_translation_units.push(backend::emit_c(
				&mono_program,
				backend::CBackendOptions { emit_prelude: false },
			));
		}

		compiled_files.push(CompiledFile {
			file_id,
			path: file.path,
			module_path: file.module_path,
		});
	}

	return Ok(CompiledCrate {
		name: crate_input.name.clone(),
		root: crate_input.root.clone(),
		files: compiled_files,
		diagnostics: crate_diagnostics,
		export_table: crate_export_table,
		type_export_table: crate_type_export_table,
		c_translation_unit: if c_translation_units.is_empty() {
			None
		} else {
			Some(c_translation_units.join("\n"))
		},
	});
}

fn export_table_to_owned(exports: &ExportTable<'_>) -> ExportTable<'static>
{
	let mut owned = ExportTable::default();

	for symbol in &exports.symbols {
		let id = owned.symbols.len();
		let full_path = symbol
			.full_path
			.iter()
			.map(|segment| Cow::Owned(segment.to_string()))
			.collect::<Vec<_>>();

		owned.by_path.insert((symbol.namespace, full_path.clone()), id);
		owned.symbols.push(ExportSymbol {
			name: Cow::Owned(symbol.name.to_string()),
			kind: symbol.kind,
			namespace: symbol.namespace,
			defined_at: symbol.defined_at,
			full_path,
		});
	}

	return owned;
}

fn merge_export_table(target: &mut ExportTable<'static>, source: ExportTable<'static>)
{
	for symbol in source.symbols {
		let id = target.symbols.len();
		target.by_path.insert((symbol.namespace, symbol.full_path.clone()), id);
		target.symbols.push(symbol);
	}
}

fn merge_type_export_table(target: &mut ExportTypeTable<'static>, source: ExportTypeTable<'static>)
{
	target.values.extend(source.values);
	target.types.extend(source.types);
	target.interfaces.extend(source.interfaces);
}

fn print_crate_modules(compiled_crate: &CompiledCrate)
{
	println!("crate {} modules:", compiled_crate.name);
	println!("  root => {}", compiled_crate.root.display());

	for file in &compiled_crate.files {
		println!("  {} => {}", format_module_path(&file.module_path), file.path.display());
	}
}

fn format_dependencies(dependencies: &[CompiledCrate]) -> String
{
	if dependencies.is_empty() {
		return "<none>".to_owned();
	}

	return dependencies
		.iter()
		.map(|compiled_crate| return compiled_crate.name.as_str())
		.collect::<Vec<_>>()
		.join(", ");
}

fn format_module_path(module_path: &[String]) -> String
{
	if module_path.is_empty() {
		return "<root>".to_owned();
	}

	return module_path.join("::");
}

fn render_diagnostics(diagnostics: &[Diagnostic], source_map: &SourceMap)
{
	for diagnostic in diagnostics {
		let renderer = TextRenderer::new(diagnostic, source_map, diagnostics::ColorChoice::Auto);
		println!("{renderer}");
	}
}
