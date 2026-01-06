// Correctness / Safety
#![warn(clippy::correctness)]
#![warn(clippy::integer_division)]
#![warn(clippy::implicit_hasher)]
#![warn(clippy::implicit_clone)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(clippy::manual_assert)]
#![warn(clippy::single_match)]
#![warn(clippy::match_same_arms)]
#![warn(clippy::if_same_then_else)]
#![warn(clippy::redundant_pattern_matching)]
#![warn(clippy::collapsible_if)]
#![warn(clippy::bool_assert_comparison)]
#![warn(clippy::cmp_owned)]
#![warn(clippy::float_cmp)]
#![warn(clippy::manual_range_contains)]
#![warn(clippy::same_item_push)]
#![warn(clippy::unchecked_time_subtraction)]
#![warn(clippy::useless_conversion)]
#![warn(clippy::zero_prefixed_literal)]
#![warn(clippy::fallible_impl_from)]
#![warn(clippy::multiple_unsafe_ops_per_block)]
#![warn(clippy::ptr_as_ptr)]
#![warn(clippy::transmute_ptr_to_ptr)]
// Error handeling
#![warn(clippy::manual_ok_or)]
#![warn(clippy::manual_unwrap_or)]
#![warn(clippy::unnecessary_wraps)]
#![warn(clippy::panic)]
#![warn(clippy::unwrap_used)]
#![cfg_attr(test, allow(clippy::panic, clippy::unwrap_used))]
// Performance / Allocation
#![warn(clippy::inefficient_to_string)]
#![warn(clippy::slow_vector_initialization)]
#![warn(clippy::needless_collect)]
#![warn(clippy::redundant_allocation)]
#![warn(clippy::manual_memcpy)]
#![warn(clippy::redundant_clone)]
#![warn(clippy::trivially_copy_pass_by_ref)]
#![warn(clippy::needless_pass_by_value)]
#![warn(clippy::needless_borrow)]
#![warn(clippy::or_fun_call)]
#![warn(clippy::verbose_bit_mask)]
#![warn(clippy::rc_buffer)]
#![warn(clippy::rc_mutex)]
#![warn(clippy::large_stack_arrays)]
#![warn(clippy::borrowed_box)]
#![warn(clippy::redundant_static_lifetimes)]
#![warn(clippy::naive_bytecount)]
#![warn(clippy::needless_range_loop)]
#![warn(clippy::iter_skip_zero)]
#![warn(clippy::result_large_err)]
// Iterator
#![warn(clippy::needless_for_each)]
#![warn(clippy::manual_map)]
#![warn(clippy::manual_flatten)]
// Control flow / code structure
#![warn(clippy::branches_sharing_code)]
#![warn(clippy::match_wildcard_for_single_variants)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::never_loop)]
#![warn(clippy::match_bool)]
#![warn(clippy::unnested_or_patterns)]
#![warn(clippy::redundant_guards)]
// Style
#![warn(clippy::style)]
#![warn(clippy::shadow_reuse)]
#![warn(clippy::enum_glob_use)]
#![warn(clippy::implicit_return)]
#![warn(clippy::module_name_repetitions)]
#![warn(clippy::self_named_module_files)]
#![warn(clippy::let_underscore_untyped)]
#![warn(clippy::rest_pat_in_fully_bound_structs)]
// Docs
#![warn(clippy::doc_markdown)]
#![warn(clippy::missing_safety_doc)]
#![warn(clippy::missing_panics_doc)]
#![warn(clippy::missing_errors_doc)]
// Warning collections
#![warn(clippy::nursery)]
// Cargo
// #![warn(clippy::cargo)]
// #![warn(clippy::cargo_common_metadata)]
// Allow
#![allow(clippy::use_self)]
#![allow(clippy::needless_return)]
// #![allow(dead_code)]

// #![warn(clippy::todo)]

use std::{fs, process::exit};

use self::desuger::Desugarer;
use self::lexer::Lexer;
use self::parser::Parser;

mod desuger;
mod lexer;
mod parser;

#[derive(Debug, Eq, PartialEq, Clone, Default)]
#[allow(clippy::exhaustive_structs)]
pub struct Config {}

fn main()
{
	let config: Config = Config::default();
	let file: String = fs::read_to_string("slang-test/main.sl").map_or_else(
		|_| {
			println!("could not open file");
			exit(1)
		},
		|f| return f,
	);
	let lexed: Lexer = Lexer::new(&config, &file);
	// println!("{:#?}", lexed.clone().collect::<Vec<_>>());
	let parsed: Parser = lexed.into();
	let program: Result<parser::Program, parser::ParseError> = parsed.into();
	match &program {
		Ok(ast) => {
			println!("{ast:#?}");
		}
		Err(e) => {
			println!("{e}");
		}
	}

	let mut desugager: Desugarer = Desugarer::new();

	let desugared = desugager.desugar_program(program.expect("found an error in the program"));
	println!("{desugared}");
}
