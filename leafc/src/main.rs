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
// Cargo
// #![warn(clippy::cargo)]
// #![warn(clippy::cargo_common_metadata)]

// Allow
#![allow(clippy::needless_return)]
#![allow(clippy::use_self)]
#![allow(clippy::result_large_err)] // TODO: in the future, maybe fix all of them
#![allow(clippy::self_named_module_files)]
// #![allow(dead_code)]

// #![warn(clippy::todo)]

use std::{fs, process::exit};

use self::{
	desugar::{DesugarError, Desugarer},
	lexer::Lexer,
	parser::{ParseError, Parser},
	source_map::SourceMap,
};

mod desugar;
mod lexer;
mod parser;
mod source_map;

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct Config {}

#[derive(Debug, Clone)]
pub enum CompileError
{
	ParseError(ParseError),
	DesugarError(DesugarError),
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
		};
	}
}

impl std::error::Error for CompileError {}

impl CompileError
{
	#[allow(clippy::missing_errors_doc)]
	pub fn fmt_with_source(&self, f: &mut impl std::fmt::Write, sm: &crate::source_map::SourceMap) -> std::fmt::Result
	{
		return match self {
			CompileError::ParseError(err) => err.write(f, sm),
			CompileError::DesugarError(err) => err.write(f, sm),
		};
	}

	#[allow(clippy::missing_errors_doc)]
	pub fn to_string_with_source(&self, sm: &crate::source_map::SourceMap) -> Result<String, std::fmt::Error>
	{
		let mut out: String = String::new();
		self.fmt_with_source(&mut out, sm)?;
		return Ok(out);
	}
}

fn main()
{
	const FILE_NAME: &str = "leaf-test/main.leaf";
	let config: Config = Config::default();
	let mut source_map: SourceMap = SourceMap::default();

	let file: String = fs::read_to_string(FILE_NAME).map_or_else(
		|_| {
			println!("could not open file");
			exit(1)
		},
		|f| return f,
	);
	let lexed: Lexer = Lexer::new_add_to_source_map(&config, file, FILE_NAME, &mut source_map);
	// println!("{:#?}", lexed.clone().collect::<Vec<_>>());
	let parsed: Parser = lexed.into();
	let program: Result<parser::Program, CompileError> = parsed.try_into();
	// match &program {
	// 	Ok(ast) => {
	// 		println!("{ast:#?}");
	// 	}
	// 	Err(e) => {
	// 		println!("{e}");
	// 	}
	// }

	let mut desugager: Desugarer = Desugarer::new();

	let desugared: Result<parser::Program, CompileError> = desugager.desugar_program(
		program
			.inspect_err(|e| println!("{}", e.to_string_with_source(&source_map).expect("")))
			.expect("found an error in the program"),
	);
	match &desugared {
		Ok(ast) => {
			println!("{ast}");
		}
		Err(e) => {
			println!("{}", e.to_string_with_source(&source_map).expect(""));
		}
	}
}
