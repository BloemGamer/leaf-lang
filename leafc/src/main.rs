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
#![allow(clippy::result_large_err)] // TODO: in the future, maybe fix all of them
#![allow(clippy::self_named_module_files)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::similar_names)]
#![allow(clippy::uninlined_format_args)]
// #![allow(dead_code)]

// #![warn(clippy::todo)]

use std::{
	collections::{HashSet, VecDeque},
	fs, path,
	process::exit,
};

use crate::{
	config::Config,
	diagnostics::{CompileDiagnostic, CompileError},
};

use self::{
	backend::{BackendInput, BackendOptions, CompilerBackend, c::CBackend},
	config::ColourConf,
	desugar::DesugaredAST,
	diagnostics::{CompileDiagnosticRenderer, DiagnosticBuilder, OldStyleRenderer, use_colour},
	lexer::{Lexer, Span, expander::ExpandedLexer},
	mir::MirModule,
	modules::{ModuleError, ModuleErrorKind},
	name_resolution::ResolvedModule,
	parser::{AST, ExprEnum, Parser},
	source_map::{SourceIndex, SourceMap},
	symbol_collection::{GlobalSymbolTable, LocalSymbolTable},
	type_analysis::TypedModule,
};

mod backend;
mod desugar;
mod lexer;
mod mir;
mod modules;
mod monomorphization;
mod name_resolution;
mod parser;
mod symbol_collection;
mod type_analysis;

mod config;
mod diagnostics;
mod source_map;
mod utils;

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

	#[arg(short, long, default_value_t = ColourConf::Auto)]
	colour: ColourConf,
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
			|| self.mir);
	}
}

const STDLIB_PATH: &str = "std/std.leaf";

fn main()
{
	const FILE_NAME: &str = "leaf-test/main.leaf";
	let args: Args = <Args as clap::Parser>::parse();
	let config: Config = Config {
		colour: if use_colour(args.colour) {
			ColourConf::Always
		} else {
			ColourConf::Never
		},
		..Default::default()
	};
	let mut source_map: SourceMap = SourceMap::default();

	let (res, diagnostics) = run(&args, &config, FILE_NAME, &mut source_map);

	for d in diagnostics {
		let diag = d.finish();
		let renderer = OldStyleRenderer::new(&diag, &source_map, &config);
		eprintln!("{}", renderer);
	}
	match res.inspect_err(|e| {
		let Some(err) = e.as_ref() else {
			return;
		};
		let diag: diagnostics::Diagnostic = err.to_diagnostic();
		let renderer: OldStyleRenderer<'_> = OldStyleRenderer::new(&diag, &source_map, &config);
		eprintln!("{}", renderer);
	}) {
		Ok(()) => {}
		Err(e) => {
			eprintln!("{:?}", e);
			exit(1)
		}
	}
}

fn run(
	args: &Args,
	config: &Config,
	filename: impl Into<path::PathBuf> + Clone,
	source_map: &mut SourceMap,
) -> (Result<(), Option<CompileError>>, Vec<DiagnosticBuilder>)
{
	let mut diagnostics = Vec::new();
	let mut queue: VecDeque<modules::PendingModule> = VecDeque::from([
		//Stdlib root namespace
		modules::PendingModule {
			logical_path: vec!["std".to_string()],
			file_path: {
				let mut tmp = path::PathBuf::from(STDLIB_PATH);
				tmp.pop();
				tmp.push("std.leaf");
				tmp
			},
			declared_at_span: Span {
				source_index: SourceIndex::new(0),
				start: 0,
				end: 0,
				start_line: 0,
				start_col: 0,
				end_line: 0,
				end_col: 0,
			},
		},
		// User entry module
		modules::PendingModule {
			logical_path: vec![],
			file_path: filename.clone().into(),
			declared_at_span: Span {
				source_index: SourceIndex::new(0),
				start: 0,
				end: 0,
				start_line: 0,
				start_col: 0,
				end_line: 0,
				end_col: 0,
			},
		},
	]);
	let mut visited: HashSet<Vec<String>> = HashSet::new();

	let mut pending_modules: Vec<(Vec<String>, DesugaredAST, LocalSymbolTable)> = Vec::new();

	while let Some(pm) = queue.pop_front() {
		if args.modules {
			println!("::{}", pm.logical_path.join("::"));
		}
		if !visited.insert(pm.logical_path.clone()) {
			continue;
		}

		let res = fs::read_to_string(&pm.file_path).map_err(|e| {
			let kind: ModuleErrorKind = if e.kind() == std::io::ErrorKind::NotFound {
				ModuleErrorKind::FileNotFound(pm.file_path.clone())
			} else {
				ModuleErrorKind::IoError(e.to_string())
			};
			return CompileError::from(ModuleError {
				logical_path: pm.logical_path.clone(),
				span: pm.declared_at_span,
				kind,
				context: Vec::new(),
			});
		});
		let source: String = match res {
			Ok(s) => s,
			e @ Err(_) => return (e.map(|_| ()).map_err(Option::Some), diagnostics),
		};

		let lexer: Lexer<'_, '_> = Lexer::new_add_to_source_map(config, source, pm.file_path.clone(), source_map);
		if args.lexed {
			println!(
				"-------------------------------------------------------\n::{} =>\n{:#?}",
				pm.logical_path.join("::"),
				lexer.clone().collect::<Vec<_>>()
			);
		}
		let expanded_lexer: ExpandedLexer = ExpandedLexer::new(lexer);

		let mut parser = Parser::from(expanded_lexer);
		parser.allow_type_inference = true;
		let res = parser.parse_program();
		let ast: AST = match res {
			Ok((ast, diags)) => {
				diagnostics.extend(diags);
				ast
			}
			Err(err) => {
				diagnostics.extend(err);
				return (Err(None), diagnostics);
			}
		};
		if args.parsed {
			println!(
				"-------------------------------------------------------\n::{} =>\n{}",
				pm.logical_path.join("::"),
				ast
			);
		}

		let ret = modules::collect_pending(&ast, &pm.file_path, &pm.logical_path);
		queue.extend(match ret {
			Ok(p) => p,
			e @ Err(_) => {
				return (
					e.map(|_| ()).map_err(CompileError::Module).map_err(Option::Some),
					diagnostics,
				);
			}
		});

		let res = desugar::desugar_program(ast);
		let desugared: DesugaredAST = match res {
			Ok((ast, mut diags)) => {
				diagnostics.append(&mut diags);
				ast
			}
			Err(mut diags) => {
				diagnostics.append(&mut diags);
				return (Err(None), diagnostics);
			}
		};
		if args.desugared {
			println!(
				"-------------------------------------------------------\n::{} =>\n{}",
				pm.logical_path.join("::"),
				desugared
			);
		}

		let ret = symbol_collection::collect_symbols(&desugared, pm.logical_path.clone());
		let local_symbols: LocalSymbolTable = match ret {
			Ok((ls, mut diags)) => {
				diagnostics.append(&mut diags);
				ls
			}
			Err(mut diags) => {
				diagnostics.append(&mut diags);
				return (Err(None), diagnostics);
			}
		};
		if args.symbols {
			println!(
				"-------------------------------------------------------\n::{} =>\n{:#?}",
				pm.logical_path.join("::"),
				local_symbols
			);
		}

		pending_modules.push((pm.logical_path, desugared, local_symbols));
	}

	let global_symbols: GlobalSymbolTable = symbol_collection::merge_symbol_tables(&pending_modules);

	if args.symbols {
		println!(
			"-------------------------------------------------------\n(global symbols) =>\n{:#?}",
			global_symbols
		);
	}

	let mut resolved_modules: Vec<ResolvedModule> = Vec::new();
	for (path, desugared, symbols) in &pending_modules {
		let ret = name_resolution::resolve_names(path, desugared, symbols, &global_symbols, &pending_modules);
		let resolved: ResolvedModule = match ret {
			Ok((rm, mut diags)) => {
				diagnostics.append(&mut diags);
				rm
			}
			Err(mut diags) => {
				diagnostics.append(&mut diags);
				return (Err(None), diagnostics);
			}
		};
		resolved_modules.push(resolved);
	}

	if args.name_resolution {
		for ResolvedModule { ast, path, symbols: _ } in &resolved_modules {
			println!(
				"-------------------------------------------------------\n::{} =>\n{}",
				path.join("::"),
				ast
			);
		}
	}

	let mut typed_modules: Vec<type_analysis::TypedModule> = Vec::new();
	for resolved in &resolved_modules {
		let ret = type_analysis::check_types(resolved, &global_symbols, &resolved_modules);
		let typed: TypedModule = match ret {
			Ok(t) => t,
			e @ Err(_) => return (e.map(|_| ()).map_err(Option::Some), diagnostics),
		};
		typed_modules.push(typed);
	}

	if args.types {
		for TypedModule { ast, path, caches: _ } in &typed_modules {
			println!(
				"-------------------------------------------------------\n::{} =>\n{}",
				path.join("::"),
				ast
			);
		}
	}

	let mut mir_modules: Vec<MirModule> = Vec::new();
	for tmod in &typed_modules {
		let ret = mir::lower_module(tmod, &global_symbols);
		let mir_mod: MirModule = match ret {
			Ok((mm, mut diags)) => {
				diagnostics.append(&mut diags);
				mm
			}
			Err(mut diags) => {
				diagnostics.append(&mut diags);
				return (Err(None), diagnostics);
			}
		};
		mir_modules.push(mir_mod);
	}

	if args.mir {
		for m in &mir_modules {
			println!("{}", m);
		}
	}

	let (mono_mod, mut diags) = monomorphization::monomorphize(&mir_modules, &global_symbols);
	diagnostics.append(&mut diags);

	if args.mono {
		println!("{}", mono_mod);
	}

	let mut backend: CBackend = CBackend::new();
	let mut backend_options: BackendOptions = BackendOptions::from_config(config);
	backend_options.output_path = {
		let tmp: path::PathBuf = filename.into();
		tmp
	};
	let backend_input: BackendInput = BackendInput {
		module: &mono_mod,
		symbols: &global_symbols,
		options: &backend_options,
		source_map,
	};
	if let Err(diags) = backend.validate(&backend_input) {
		diagnostics.extend(diags);
		return (Err(None), diagnostics);
	}
	let backend_output: backend::BackendOutput = match backend.compile(&backend_input) {
		Ok((output, diags)) => {
			diagnostics.extend(diags);
			output
		}
		Err(diags) => {
			diagnostics.extend(diags);
			return (Err(None), diagnostics);
		}
	};

	if args.all_false() {
		let _: std::io::Result<()> = print_backend_output(&backend_output);
	}

	return (Ok(()), diagnostics);
}

fn print_backend_output(output: &backend::BackendOutput) -> std::io::Result<()>
{
	println!(
		"-------------------------------------------------------\nPrimary: {}",
		output.primary.display()
	);
	println!("{}", std::fs::read_to_string(&output.primary)?);

	for artifact in &output.artifacts {
		println!(
			"-------------------------------------------------------\nArtifact: {}",
			artifact.display()
		);
		println!("{}", std::fs::read_to_string(artifact)?);
	}

	return Ok(());
}
