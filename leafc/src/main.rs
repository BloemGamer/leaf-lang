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
#![allow(clippy::uninlined_format_args)]
// #![allow(dead_code)]

// #![warn(clippy::todo)]

use std::{
	collections::{HashSet, VecDeque},
	fs, path,
};

use self::{
	desugar::{DesugarError, DesugaredAST},
	lexer::{Lexer, Span},
	modules::{ModuleError, ModuleErrorKind},
	name_resolution::{NameResolutionError, ResolvedModule},
	parser::{AST, ParseError, Parser},
	source_map::{SourceIndex, SourceMap},
	symbol_collection::{GlobalSymbolTable, LocalSymbolTable, SymbolCollectionError},
};

mod desugar;
mod lexer;
mod modules;
mod name_resolution;
mod parser;
mod symbol_collection;

mod source_map;

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct Config {}

pub trait CompileDiagnostic
{
	#[allow(clippy::missing_errors_doc)]
	fn fmt_with_source(&self, f: &mut impl std::fmt::Write, sm: &crate::source_map::SourceMap) -> std::fmt::Result;
	#[allow(clippy::missing_errors_doc)]
	fn to_string_with_source(&self, sm: &crate::source_map::SourceMap) -> Result<String, std::fmt::Error>
	{
		let mut out: String = String::new();
		self.fmt_with_source(&mut out, sm)?;
		return Ok(out);
	}
}

#[derive(Debug, Clone)]
pub enum CompileError
{
	ParseError(ParseError),
	DesugarError(DesugarError),
	ModuleError(ModuleError),
	SymbolCollectionError(SymbolCollectionError),
	NameResolutionError(NameResolutionError),
}

impl std::fmt::Display for CompileError
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			CompileError::ParseError(error) => {
				write!(f, "{}", error)
			}
			CompileError::DesugarError(error) => {
				write!(f, "{}", error)
			}
			CompileError::ModuleError(error) => {
				write!(f, "{}", error)
			}
			CompileError::SymbolCollectionError(error) => {
				write!(f, "{}", error)
			}
			CompileError::NameResolutionError(error) => {
				write!(f, "{}", error)
			}
		};
	}
}

impl std::error::Error for CompileError {}

impl CompileDiagnostic for CompileError
{
	#[allow(clippy::missing_errors_doc)]
	fn fmt_with_source(&self, f: &mut impl std::fmt::Write, sm: &crate::source_map::SourceMap) -> std::fmt::Result
	{
		return match self {
			CompileError::ParseError(err) => err.fmt_with_source(f, sm),
			CompileError::DesugarError(err) => err.fmt_with_source(f, sm),
			CompileError::ModuleError(err) => err.fmt_with_source(f, sm),
			CompileError::SymbolCollectionError(err) => err.fmt_with_source(f, sm),
			CompileError::NameResolutionError(err) => err.fmt_with_source(f, sm),
		};
	}
}

#[allow(clippy::struct_excessive_bools)]
#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args
{
	#[arg(short, long)]
	lexed: bool,
	#[arg(short, long)]
	parsed: bool,
	#[arg(short, long)]
	modules: bool,
	#[arg(short, long)]
	desugared: bool,
	#[arg(short, long)]
	symbols: bool,
	#[arg(short, long)]
	name_resolution: bool,
}

impl Args
{
	const fn all_false(&self) -> bool
	{
		return !(self.lexed || self.parsed || self.desugared || self.modules || self.symbols || self.name_resolution);
	}
}

fn main()
{
	const FILE_NAME: &str = "leaf-test/main.leaf";
	let args: Args = <Args as clap::Parser>::parse();
	let config: Config = Config::default();
	let mut source_map: SourceMap = SourceMap::default();

	run(&args, &config, FILE_NAME, &mut source_map)
		.inspect_err(|e| println!("{}", e.to_string_with_source(&source_map).expect("")))
		.expect("found an error in the program");
}

fn run(
	args: &Args,
	config: &Config,
	filename: impl Into<path::PathBuf> + Clone,
	source_map: &mut SourceMap,
) -> Result<(), CompileError>
{
	let mut queue: VecDeque<modules::PendingModule> = VecDeque::from([modules::PendingModule {
		logical_path: Vec::new(),
		file_path: filename.into(),
		declared_at_span: Span {
			start: 0,
			end: 0,
			start_line: 0,
			start_col: 0,
			end_line: 0,
			end_col: 0,
		},
		declared_at_source: SourceIndex::new(0),
	}]);
	let mut visited: HashSet<Vec<String>> = HashSet::new();

	// Phase 1: parse, desugar, and collect local symbols for each module
	let mut pending_modules: Vec<(Vec<String>, DesugaredAST, LocalSymbolTable)> = Vec::new();

	while let Some(pm) = queue.pop_front() {
		if args.modules {
			println!("::{}", pm.logical_path.join("::"));
		}
		if !visited.insert(pm.logical_path.clone()) {
			continue;
		}

		let source: String = fs::read_to_string(&pm.file_path).map_err(|e| {
			let kind: ModuleErrorKind = if e.kind() == std::io::ErrorKind::NotFound {
				ModuleErrorKind::FileNotFound(pm.file_path.clone())
			} else {
				ModuleErrorKind::IoError(e.to_string())
			};
			return CompileError::from(ModuleError {
				logical_path: pm.logical_path.clone(),
				span: pm.declared_at_span,
				source_index: pm.declared_at_source,
				kind,
			});
		})?;

		let lexer: Lexer<'_, '_> = Lexer::new_add_to_source_map(config, source, pm.file_path.clone(), source_map);
		if args.lexed {
			println!(
				"-------------------------------------------------------\n::{} =>\n{:#?}",
				pm.logical_path.join("::"),
				lexer.clone().collect::<Vec<_>>()
			);
		}

		let ast: AST = Parser::from(lexer).try_into()?;
		if args.parsed {
			println!(
				"-------------------------------------------------------\n::{} =>\n{}",
				pm.logical_path.join("::"),
				ast
			);
		}

		queue.extend(modules::collect_pending(&ast, &pm.file_path, &pm.logical_path));

		let desugared: DesugaredAST = ast.try_into()?;
		if args.desugared {
			println!(
				"-------------------------------------------------------\n::{} =>\n{}",
				pm.logical_path.join("::"),
				desugared
			);
		}

		// Pass logical_path so the local table knows which module it belongs to
		let local_symbols: LocalSymbolTable =
			symbol_collection::collect_symbols(&desugared, pm.logical_path.clone(), desugared.source_index)?;
		if args.symbols {
			println!(
				"-------------------------------------------------------\n::{} =>\n{:#?}",
				pm.logical_path.join("::"),
				local_symbols
			);
		}

		pending_modules.push((pm.logical_path, desugared, local_symbols));
	}

	// Phase 2: merge all local symbol tables into one globally consistent table.
	// After this point, every SymbolId and ScopeId is valid across all modules.
	let global_symbols: GlobalSymbolTable = symbol_collection::merge_symbol_tables(&pending_modules);

	if args.symbols {
		println!(
			"-------------------------------------------------------\n(global symbols) =>\n{:#?}",
			global_symbols
		);
	}

	// Phase 3: name resolution — each module resolved against the global table.
	// The ASTs and their logical paths are all we need now; local tables are done.
	// let ast_modules: Vec<(Vec<String>, DesugaredAST)> = pending_modules
	// 	.into_iter()
	// 	.map(|(path, desugared, _local)| (path, desugared))
	// 	.collect();

	let mut resolved_modules: Vec<ResolvedModule> = Vec::new();
	for (path, desugared, symbols) in &pending_modules {
		let resolved: ResolvedModule =
			name_resolution::resolve_names(path, desugared, symbols, &global_symbols, &pending_modules)?;
		resolved_modules.push(resolved);
	}

	if args.name_resolution || args.all_false() {
		for ResolvedModule { path, ast, symbols: _ } in &resolved_modules {
			println!(
				"-------------------------------------------------------\n::{} =>\n{}",
				path.join("::"),
				ast
			);
		}
	}

	return Ok(());
}
