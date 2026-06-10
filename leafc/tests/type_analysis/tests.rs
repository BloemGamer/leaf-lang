use std::collections::{HashSet, VecDeque};
use std::fs;
use std::path;

use crate::{
	config::Config,
	desugar::{self, DesugaredAST},
	diagnostics::{
		CompileDiagnostic, CompileDiagnosticRenderer, CompileError, DiagnosticBuilder, OldStyleRenderer, Severity,
	},
	lexer::expander::ExpandedLexer,
	lexer::{Lexer, Span},
	modules::{self, ModuleError, ModuleErrorKind},
	name_resolution::{self, ResolvedModule},
	parser::{AST, Parser},
	source_map::{SourceIndex, SourceMap},
	symbol_collection::{self, LocalSymbolTable},
	type_analysis,
};

use super::TypedModule;

fn parse_and_analyse<const TYPE_INFERENCE: bool>(
	source: impl Into<String>,
	filename: impl Into<path::PathBuf> + Clone,
	source_map: &mut SourceMap,
) -> Result<TypedModule, Vec<DiagnosticBuilder>>
{
	let config = Config::default();
	let mut diagnostics: Vec<DiagnosticBuilder> = Vec::new();

	let dummy_span = Span {
		source_index: SourceIndex::new(0),
		start: 0,
		end: 0,
		start_line: 0,
		start_col: 0,
		end_line: 0,
		end_col: 0,
	};

	let user_source: String = source.into();
	let user_path: path::PathBuf = filename.into();

	let mut queue: VecDeque<modules::PendingModule> = VecDeque::from([
		modules::PendingModule {
			logical_path: vec!["std".to_string()],
			file_path: {
				let mut tmp = path::PathBuf::from("../std/std.leaf");
				tmp.pop();
				tmp.push("std.leaf");
				tmp
			},
			declared_at_span: dummy_span,
		},
		modules::PendingModule {
			logical_path: vec![],
			file_path: user_path,
			declared_at_span: dummy_span,
		},
	]);
	let mut visited: HashSet<Vec<String>> = HashSet::new();
	let mut pending_modules: Vec<(Vec<String>, DesugaredAST, LocalSymbolTable)> = Vec::new();

	while let Some(pm) = queue.pop_front() {
		if !visited.insert(pm.logical_path.clone()) {
			continue;
		}

		let raw: String = if pm.logical_path.is_empty() {
			user_source.clone()
		} else {
			fs::read_to_string(&pm.file_path).map_err(|e| {
				let kind = if e.kind() == std::io::ErrorKind::NotFound {
					ModuleErrorKind::FileNotFound(pm.file_path.clone())
				} else {
					ModuleErrorKind::IoError(e.to_string())
				};
				return vec![
					ModuleError {
						logical_path: pm.logical_path.clone(),
						span: pm.declared_at_span,
						kind,
						context: Vec::new(),
					}
					.build(),
				];
			})?
		};

		let lexer = Lexer::new_add_to_source_map(&config, raw, pm.file_path.clone(), source_map);
		let expanded_lexer = ExpandedLexer::new(lexer);

		let mut parser = Parser::from(expanded_lexer);
		parser.allow_type_inference = TYPE_INFERENCE;

		let ast: AST = parser
			.parse_program()
			.map(|(ast, mut diags)| {
				diagnostics.append(&mut diags);
				return ast;
			})
			.map_err(|mut diags| {
				diagnostics.append(&mut diags);
				return diagnostics.clone();
			})?;

		let children = modules::collect_pending(&ast, &pm.file_path, &pm.logical_path)
			.map_err(|e| vec![CompileError::Module(e).build()])?;
		queue.extend(children);

		let desugared: DesugaredAST = desugar::desugar_program(ast)
			.map(|(tmp_ast, mut diags)| {
				diagnostics.append(&mut diags);
				return tmp_ast;
			})
			.map_err(|mut diags| {
				diagnostics.append(&mut diags);
				return diagnostics.clone();
			})?;

		let local_symbols = symbol_collection::collect_symbols(&desugared, pm.logical_path.clone())
			.map(|(ast, mut diags)| {
				diagnostics.append(&mut diags);
				return ast;
			})
			.map_err(|mut diags| {
				diagnostics.append(&mut diags);
				return diagnostics.clone();
			})?;

		pending_modules.push((pm.logical_path, desugared, local_symbols));
	}

	let global_symbols = symbol_collection::merge_symbol_tables(&pending_modules);

	let mut resolved_modules: Vec<ResolvedModule> = Vec::new();
	for (path, desugared, symbols) in &pending_modules {
		let (resolved, _) =
			name_resolution::resolve_names(path, desugared, symbols, &global_symbols, &pending_modules)?;
		resolved_modules.push(resolved);
	}

	let mut std_errors: Vec<DiagnosticBuilder> = Vec::new();
	let mut user_errors: Vec<DiagnosticBuilder> = Vec::new();
	let mut user_typed: Option<TypedModule> = None;

	for resolved in &resolved_modules {
		let is_user = resolved.path.is_empty();
		match type_analysis::check_types(resolved, &global_symbols, &resolved_modules) {
			Ok(typed) => {
				if is_user {
					user_typed = Some(typed);
				}
			}
			Err(e) => {
				let diag = e.build();
				if is_user {
					user_errors.push(diag);
				} else {
					std_errors.push(diag);
				}
			}
		}
	}

	if !std_errors.is_empty() {
		return Err(std_errors);
	}
	if !user_errors.is_empty() {
		return Err(user_errors);
	}

	return user_typed.ok_or_else(|| {
		return vec![DiagnosticBuilder {
			code: None,
			severity: Severity::Bug,
			message: "Module was never type checked".into(),
			labels: Vec::new(),
			notes: Vec::new(),
			helps: Vec::new(),
			suggestions: Vec::new(),
			related: Vec::new(),
		}];
	});
}

// ============================================================================
// Tests
// ============================================================================

/// Prepend the std import every snippet needs.
fn src(body: &str) -> String
{
	return format!("@use std::*;\n{body}");
}

/// Assert that the snippet type-checks without errors.
fn ok(body: &str)
{
	let s = src(body);
	let config = Config::default();
	let mut source_map = SourceMap::default();
	let r = parse_and_analyse::<true>(s, "test.leaf", &mut source_map);
	let _: Result<TypedModule, Vec<DiagnosticBuilder>> = r.clone().inspect_err(|e| {
		for d in e.clone() {
			let diag = d.finish();
			let renderer = OldStyleRenderer::new(&diag, &source_map, &config);
			eprintln!("{}", renderer);
		}
	});
	assert!(r.is_ok(), "expected Ok, got errors:\n");
}

/// Assert that at least one diagnostic message contains `needle`.
fn err(body: &str, needle: &str)
{
	let s = src(body);
	let config = Config::default();
	let mut source_map = SourceMap::default();
	let diags = parse_and_analyse::<true>(s, "test.leaf", &mut source_map)
		.expect_err(&format!("expected an error containing {needle:?}, but got Ok"));
	for d in diags.clone() {
		let diag = d.finish();
		let renderer = OldStyleRenderer::new(&diag, &source_map, &config);
		eprintln!("{}", renderer);
	}
	let any = diags.iter().any(|d| return d.message.contains(needle));
	assert!(any, "expected a diagnostic containing {needle:?}, got:\n");
}

// ── primitives & literals ────────────────────────────────────────────────

#[test]
fn integer_literal_explicit_type()
{
	ok("fn! main() { var x: i64 = 42i64; }");
}

#[test]
fn integer_literal_inferred_from_annotation()
{
	ok("fn! main() { var x: i64 = 42; }");
}

#[test]
fn float_literal_explicit()
{
	ok("fn! main() { var x: f64 = 3.14f64; }");
}

#[test]
fn bool_literal()
{
	ok("fn! main() { var b: bool = true; }");
}

#[test]
fn string_literal_is_ref_str()
{
	ok(r#"fn! main() { var s: &str = "hello"; }"#);
}

#[test]
fn cstring_literal()
{
	ok(r#"fn! main() { var s: &cstr = c"hello"; }"#);
}

#[test]
fn char_literal()
{
	ok("fn! main() { var c: char = 'x'; }");
}

// ── type mismatches ──────────────────────────────────────────────────────

#[test]
fn int_to_bool_is_mismatch()
{
	err("fn! main() { var b: bool = 1i64; }", "type mismatch");
}

#[test]
fn wrong_int_width_is_mismatch()
{
	err("fn! main() { var x: i32 = 1i64; }", "type mismatch");
}

#[test]
fn float_to_int_is_mismatch()
{
	err("fn! main() { var x: i64 = 1.0f64; }", "type mismatch");
}

// ── type inference ───────────────────────────────────────────────────────

#[test]
fn infer_integer_from_annotated_literal()
{
	ok("fn! main() { var x: _ = 10i64; }");
}

#[test]
fn infer_bool_from_rhs()
{
	ok("fn! main() { var x: _ = false; }");
}

#[test]
fn infer_chain_across_two_vars()
{
	ok("fn! main() { var x: _ = 7i64; var y: _ = x; }");
}

#[test]
fn cannot_infer_bare_untyped_literal()
{
	// `_` annotation with a bare integer and no context must fail
	err("fn! main() { var x: _ = 0; }", "cannot infer type");
}

// ── struct init syntax (`field -> value`) ────────────────────────────────

#[test]
fn struct_init_arrow_syntax()
{
	ok(r"
			struct Point { x: i64, y: i64 }
			fn! main() { var p: Point = Point{ x -> 1i64, y -> 2i64 }; }
		");
}

#[test]
fn struct_field_access()
{
	ok(r"
			struct Point { x: i64, y: i64 }
			fn! main() {
				var p: Point = Point{ x -> 1i64, y -> 2i64 };
				var _: i64 = p.x;
			}
		");
}

#[test]
fn struct_init_wrong_field_type_is_error()
{
	err(
		r"
			struct Foo { x: i64 }
			fn! main() { var _ = Foo{ x -> true }; }
		",
		"field `x`",
	);
}

#[test]
fn struct_init_unknown_field_is_error()
{
	err(
		r"
			struct Foo { x: i64 }
			fn! main() { var _ = Foo{ x -> 1i64, ghost -> 99i64 }; }
		",
		"no field",
	);
}

#[test]
fn struct_unknown_field_read_is_error()
{
	err(
		r"
			struct Point { x: i64 }
			fn! main() { var p: _ = Point{ x -> 1i64 }; var _ = p.z; }
		",
		"no field",
	);
}

// ── self methods ─────────────────────────────────────────────────────────

#[test]
fn method_call_on_self()
{
	ok(r"
			struct A { a: i64 }
			impl A {
				fn get(self) -> i64 { return self.a; }
				fn double(self) -> i64 { return self.get(); }
			}
			fn! main() {
				var a: A = A{ a -> 1i64 };
				var _: i64 = a.double();
			}
		");
}

#[test]
fn self_struct_init_in_method()
{
	ok(r"
			struct A { a: i64 }
			impl A {
				fn make(self) -> Self {
					return Self{ a -> 0i64 };
				}
			}
			fn! main() {
				var a: A = A{ a -> 1i64 };
				var _: A = a.make();
			}
		");
}

#[test]
fn method_return_type_mismatch_is_error()
{
	err(
		r"
			struct A { a: i64 }
			impl A {
				fn bad(self) -> i64 { return true; }
			}
		",
		"return type mismatch",
	);
}

// ── type aliases ─────────────────────────────────────────────────────────

#[test]
fn type_alias_shares_methods()
{
	// From the sample: `type B = A` and then `impl B` adds methods;
	// both `A::t7` and `B::t7` should resolve.
	ok(r"
			struct A { a: i64 }
			type B = A;
			impl A { fn t7() {} }
			fn! main() {
				A::t7();
				B::t7();
			}
		");
}

#[test]
fn type_alias_cast()
{
	// `(A)B{ a -> 0 }` — cast from alias B back to A
	ok(r"
			struct A { a: i64 }
			type B = A;
			fn! main() {
				var z: A = <A>B{ a -> 0i64 };
			}
		");
}

// ── traits ───────────────────────────────────────────────────────────────

#[test]
fn trait_default_method_calls_required()
{
	// `tr_f2` has a default body that calls `Self::tr_f1`
	ok(r"
			trait Tr {
				fn tr_f1(i: i64);
				fn tr_f2(i: i64) { Self::tr_f1(i); }
			}
			struct A { a: i64 }
			impl Tr for A { fn tr_f1(i: i64) {} }
			fn! main() {
				A::tr_f1(1i64);
				A::tr_f2(1i64);
			}
		");
}

#[test]
fn missing_required_trait_method_is_error()
{
	err(
		r"
			trait Tr { fn tr_f1(i: i64); }
			struct A { a: i64 }
			impl Tr for A { }
		",
		"required trait function",
	);
}

#[test]
fn extra_method_not_in_trait_is_error()
{
	err(
		r"
			trait Tr { fn tr_f1(i: i64); }
			struct A { a: i64 }
			impl Tr for A {
				fn tr_f1(i: i64) {}
				fn extra(i: i64) {}
			}
		",
		"not a member of the trait",
	);
}

#[test]
fn trait_impl_return_type_mismatch_is_error()
{
	err(
		r"
			trait HasValue { fn value(self) -> i64; }
			struct Num { }
			impl HasValue for Num { fn value(self) -> bool { return true; } }
		",
		"return type mismatch",
	);
}

// ── operator overloading via std traits ──────────────────────────────────

#[test]
fn add_operator_via_trait()
{
	// mirrors `impl std::ops::Add<B> for B` from the sample
	ok(r"
			struct A { a: i64 }
			type B = A;
			impl std::ops::Add<B> for B {
				assoc Output = Self;
				fn add(self, other: Self) -> Self::Output { return self; }
			}
			fn! main() {
				var x: B = B{ a -> 1i64 };
				var y: B = B{ a -> 2i64 };
				var _: B = x + y;
			}
		");
}

#[test]
fn add_on_type_without_impl_is_error()
{
	err(
		r"
			struct NoAdd { v: i64 }
			fn! main() {
				var x: NoAdd = NoAdd{ v -> 1i64 };
				var _ = x + x;
			}
		",
		"cannot be applied",
	);
}

// ── functions ────────────────────────────────────────────────────────────

#[test]
fn simple_free_function_call()
{
	ok(r"
			fn helper(x: i64) -> i64 { return x; }
			fn! main() { var _: i64 = helper(1i64); }
		");
}

#[test]
fn too_few_args_is_error()
{
	err(
		r"
			fn add(a: i64, b: i64) -> i64 { return a; }
			fn! main() { add(1i64); }
		",
		"wrong number of arguments",
	);
}

#[test]
fn too_many_args_is_error()
{
	err(
		r"
			fn add(a: i64, b: i64) -> i64 { return a; }
			fn! main() { add(1i64, 2i64, 3i64); }
		",
		"wrong number of arguments",
	);
}

#[test]
fn return_type_mismatch_is_error()
{
	err("fn bad() -> i64 { return true; }", "return type mismatch");
}

#[test]
fn empty_return_in_typed_fn_is_error()
{
	err("fn bad() -> i64 { return; }", "return type mismatch");
}

#[test]
fn void_function_no_return_ok()
{
	ok("fn nothing() {}");
}

// ── heap-call functions (`fn!`) ──────────────────────────────────────────

#[test]
fn heap_fn_with_explicit_allocator()
{
	ok(r"
			fn!<alloc -> std::CAlloc> heap_fn() {
				var _: i64 = 0i64;
			}
		");
}

#[test]
fn heap_fn_calling_other_heap_fn()
{
	ok(r"
			fn!<alloc -> std::CAlloc> inner() {}
			fn!<alloc -> std::CAlloc> outer() { inner!(); }
		");
}

#[test]
fn heap_fn_in_call()
{
	ok(r"
			fn! inner() {}
			fn! outer() { inner!<alloc -> alloc>(); }
		");
}

#[test]
fn heap_fn_in_call_no_alloc_trait()
{
	err(
		r"
			struct Foo {}
			fn! inner() {}
			fn! outer() {
				var f: Foo = Foo{};
				inner!<alloc -> f>();
			}
		",
		"",
	);
}

// ── generics ─────────────────────────────────────────────────────────────

#[test]
fn generic_function_inferred_type_param()
{
	ok(r"
			fn identity<T>(x: T) -> T { return x; }
			fn! main() { var _: i64 = identity(5i64); }
		");
}

#[test]
fn generic_struct_explicit_param()
{
	ok(r"
			fn! main() {
				var r: Range<i64> = std::Range::new(1i64, 10i64);
			}
		");
}

#[test]
fn generic_struct_i32()
{
	ok(r"
			fn! main() {
				var r: Range<i32> = std::Range::new(1, 2);
			}
		");
}

// ── where clauses ────────────────────────────────────────────────────────

#[test]
fn where_clause_satisfied_ok()
{
	ok(r"
			trait Numeric {}
			struct MyNum {}
			impl Numeric for MyNum {}
			fn requires<T>(_x: T) where T: Numeric {}
			fn! main() { requires(MyNum{}); }
		");
}

#[test]
fn where_clause_not_satisfied_is_error()
{
	err(
		r"
			trait Numeric {}
			struct NotNumeric {}
			fn requires<T>(_x: T) where T: Numeric {}
			fn! main() { requires(NotNumeric{}); }
		",
		"does not implement",
	);
}

#[test]
fn generic_the_same_generic_var()
{
	ok(r"
			fn! f<T: Create>(input: T) { var b: T = T::create(); }
		");
}

#[test]
fn generic_not_the_same_generic_var()
{
	err(
		r"
			fn! f<T: Iterator<Item = i64>>(input: T) { var b: Range<i64> = input; }
		",
		"",
	);
}

#[test]
fn generic_not_the_same_generic_change_input()
{
	err(
		r"
			fn! f<T: Iterator<Item = i64>>(input: &mut T) { *input = 0..10; }
		",
		"",
	);
}

// ── impl Trait ───────────────────────────────────────────────────────────

#[test]
fn impl_trait_local_variable()
{
	// mirrors `var mut aa: impl Int = 0i64` from the sample
	ok(r"
			fn!<alloc -> std::CAlloc> test() {
				var mut aa: impl Int = 0i64;
			}
		");
}

#[test]
fn impl_trait_var_reassign_same_concrete_type()
{
	ok(r"
			fn!<alloc -> std::CAlloc> test() {
				var mut aa: impl Int = 0i64;
				var mut ab: impl Int = aa;
			}
		");
}

#[test]
fn impl_trait_parameter_stays_opaque()
{
	// The parameter `i: impl Int` must not be reassignable to a different type.
	// Matches test3_should_fail in the sample.
	err(
		r"
			fn!<alloc -> std::CAlloc> test(i: impl Int) {
				i = 1i64;
			}
		",
		"",
	);
}

#[test]
fn impl_trait_var_wrong_concrete_type_is_error()
{
	err(
		r"
			trait Marker { fn mark(self); }
			struct A {}
			fn! main() { var _: impl Marker = A{}; }
		",
		"does not implement",
	);
}

// ── impl Iterator / ranges ───────────────────────────────────────────────

#[test]
fn range_assigned_to_impl_iterator()
{
	// `var mut a: impl Iterator<Item = i64> = 0..10`
	ok(r"
			fn!<alloc -> std::CAlloc> test() {
				var mut a: impl Iterator<Item = i64> = 0..10;
				a = 0..10;
			}
		");
}

#[test]
fn impl_iterator_next_via_question_mark()
{
	// mirrors `fn! test7`
	ok(r"
			fn! test7(inp: impl Iterator<Item = i64>) -> Option<i64> {
				var a: Option<i64> = inp.next?();
				return a;
			}
		");
}

// ── if / switch / control flow ───────────────────────────────────────────

#[test]
fn if_else_same_type()
{
	ok(r"
			fn! main() {
				var d: i64 = 1i64;
				var _: i64 = if d == 1i64 { 10i64 } else { 20i64 };
			}
		");
}

#[test]
fn if_else_type_mismatch_is_error()
{
	err(
		r"fn! main() { var _ = if true { 1i64 } else { false }; }",
		"branches have different types",
	);
}

#[test]
fn switch_wildcard_arm()
{
	ok(r"
			fn! main() {
				var mut d: i64 = 0i64;
				switch d {
					1i64 => {},
					_    => {},
				}
			}
		");
}

#[test]
fn switch_arm_type_mismatch_is_error()
{
	err(
		r"
			fn! main() {
				var x: i64 = 1i64;
				var _ = switch x {
					1i64 => true,
					_ => 42i64,
				};
			}
		",
		"switch arms have different types",
	);
}

#[test]
fn switch_variant_pattern()
{
	// mirrors the `switch d { Option::Some(b: i64) => … }` in the sample
	ok(r"
			fn! main() {
				var mut d: i64 = 0i64;
				switch d {
					Option::Some(b: i64) => {},
					_ => {},
				}
			}
		");
}

#[test]
fn for_loop_range()
{
	// `for i: i64 in 1..10 {}`
	ok(r"
			fn! main() {
				for i: i64 in 1..10 {}
			}
		");
}

#[test]
fn if_let_variant_pattern()
{
	// `if var Option::Some(b: i64) = b {} else {}`
	ok(r"
			fn! main() {
				var b: i64 = 0i64;
				if var Option::Some(b: i64) = b {} else {}
			}
		");
}

// ── casts ────────────────────────────────────────────────────────────────

#[test]
fn numeric_widening_cast()
{
	ok("fn! main() { var _: i64 = <i64>1i32; }");
}

#[test]
fn bool_to_int_cast_is_error()
{
	err("fn! main() { var _ = <i64>true; }", "cannot cast");
}

#[test]
fn int_to_pointer_cast()
{
	ok("fn! main() { var _: *i64 = <*i64>0usize; }");
}

// ── assignment ───────────────────────────────────────────────────────────

#[test]
fn reassign_same_type_ok()
{
	ok("fn! main() { var mut x: i64 = 1i64; x = 2i64; }");
}

#[test]
fn reassign_wrong_type_is_error()
{
	err("fn! main() { var mut x: i64 = 1i64; x = true; }", "type mismatch");
}

#[test]
fn compound_assign_add()
{
	// `asdf = asdf + 1i64 + 2` from the sample
	ok(r"
			fn! main() {
				var mut asdf: i64 = 1i64 + 2i64;
				asdf = asdf + 1i64 + 2i64;
			}
		");
}

// ── never / diverging ────────────────────────────────────────────────────

#[test]
fn never_assignable_to_any_return_type()
{
	ok(r"
			fn panic_fn() -> ! { loop {} }
			fn needs_i64() -> i64 { return panic_fn(); }
		");
}

#[test]
fn loop_without_break_is_never()
{
	ok("fn diverge() -> ! { loop {} }");
}

// ── modules & @use ───────────────────────────────────────────────────────

#[test]
fn module_declaration_and_call()
{
	ok(r"
			module math {
				pub fn square(x: i64) -> i64 { return x; }
			}
			fn! main() { var _: i64 = math::square(4i64); }
		");
}

#[test]
fn use_directive_brings_name_into_scope()
{
	ok(r"
			module util {
				pub fn helper() {}
			}
			@use util::helper;
			fn! main() { helper(); }
		");
}

#[test]
fn global_path_ignores_use()
{
	// `::t2::t::test()` must resolve even when a local `@use` shadows the name
	ok(r"
			module outer {
				pub module inner {
					pub fn greet() {}
				}
			}
			@use outer::inner;
			fn! main() {
				inner::greet();
				::outer::inner::greet();
			}
		");
}

// ── struct destructure pattern ────────────────────────────────────────────

#[test]
fn struct_pattern_destructure()
{
	// `var A{ a -> b: i64 } = A{ a -> { var c: i64 = 0; c } }`
	ok(r"
			struct A { a: i64 }
			fn! main() {
				var A{ a -> b: i64 } = A{ a -> { var c: i64 = 0i64; c } };
				var _: i64 = b;
			}
		");
}

// ── tuples ───────────────────────────────────────────────────────────────

#[test]
fn tuple_construction_and_index()
{
	// `var a: (i64, i64) = (0, 1); a.0;`
	ok(r"
			fn! main() {
				var a: (i64, i64) = (0i64, 1i64);
				var _: i64 = a.0;
			}
		");
}

#[test]
fn tuple_type_mismatch_is_error()
{
	err("fn! main() { var _: (i64, bool) = (1i64, 2i64); }", "type mismatch");
}

// ── unsafe blocks ────────────────────────────────────────────────────────

#[test]
fn unsafe_block_allowed()
{
	ok("fn! main() { unsafe { var _: i64 = 1i64; } }");
}

// ── cross-statement inference ────────────────────────────────────────────

#[test]
fn infer_pinned_by_function_param_type()
{
	ok(r"
			fn takes_i64(_x: i64) {}
			fn! main() { var x: _ = 1i64; takes_i64(x); }
		");
}

#[test]
fn infer_from_typed_variable_use()
{
	ok("fn! main() { var x: _ = 5i64; var _: i64 = x; }");
}

// ── heap allocation (`Alloc.alloc()`) ────────────────────────────────────

#[test]
fn alloc_returns_pointer()
{
	// `var asd: *i64 = alloc.alloc()` from the sample
	ok(r"
			fn! main() {
				var asd: *i64 = alloc.alloc(10);
			}
		");
}

#[test]
fn alloc_wrong_pointer_type_is_error()
{
	err(
		r"
			fn! main() {
				var asd: *bool = alloc.alloc(10);
				var _: *i64 = asd;
			}
		",
		"type mismatch",
	);
}

// ── associated types (`assoc Output = …`) ────────────────────────────────

#[test]
fn assoc_type_in_trait_impl()
{
	ok(r"
			struct A { a: i64 }
			type B = A;
			impl std::ops::Add<B> for B {
				assoc Output = Self;
				fn add(self, other: Self) -> Self::Output { return self; }
			}
			fn! main() {
				var x: B = B{ a -> 1i64 };
				var _: B = x + x;
			}
		");
}

#[test]
fn self_output_assoc_type_resolves()
{
	// Self::Output in return position should resolve to the implementing type
	ok(r"
			struct Val { v: i64 }
			impl std::ops::Add<Val> for Val {
				assoc Output = Self;
				fn add(self, _other: Self) -> Self::Output { return self; }
			}
			fn! main() {
				var a: Val = Val{ v -> 1i64 };
				var b: Val = Val{ v -> 2i64 };
				var c: Val = a + b;
			}
		");
}

// ── static / associated function calls ──────────────────────────────────

#[test]
fn static_method_call_via_double_colon()
{
	// `A::t7()` and `B::t7()` from the sample
	ok(r"
			struct A { a: i64 }
			type B = A;
			impl A { fn t7() {} }
			fn! main() {
				A::t7();
				B::t7();
			}
		");
}

#[test]
fn trait_static_method_via_type()
{
	// `B::tr_f1(1)` and `B::tr_f2(1)` from the sample
	ok(r"
			trait Tr {
				fn tr_f1(i: i64);
				fn tr_f2(i: i64) { Self::tr_f1(i); }
			}
			struct A { a: i64 }
			type B = A;
			impl Tr for B { fn tr_f1(i: i64) {} }
			fn! main() {
				B::tr_f1(1i64);
				B::tr_f2(1i64);
			}
		");
}

#[test]
fn static_method_wrong_arg_type_is_error()
{
	err(
		r"
			struct A { a: i64 }
			impl A { fn takes_i64(x: i64) {} }
			fn! main() { A::takes_i64(true); }
		",
		"type mismatch",
	);
}

// ── heap-call with explicit generic allocator (`fn!<alloc -> …>`) ────────

#[test]
fn heap_fn_named_generic_alloc_call()
{
	// `B::t8!<alloc -> alloc>(8)` — explicit allocator forwarding
	ok(r"
			struct A { a: i64 }
			type B = A;
			impl B { fn! t8(i: i64) {} }
			fn! main() {
				B::t8!<alloc -> alloc>(8i64);
				B::t8!(8i64);
			}
		");
}

// ── `std::Q` and other stdlib constructors ───────────────────────────────

#[test]
fn std_q_new()
{
	// `std::Q::new(0)` from the sample
	ok(r"
			fn! main() {
				std::Q::new(0i64);
			}
		");
}

// ── printf / extern-C variadic ───────────────────────────────────────────

#[test]
fn printf_cstring_format()
{
	// `printf(c"%d\n")` — extern(C) variadic; only the format arg is required
	ok(r#"
			fn! main() {
				printf(c"%d\n");
			}
		"#);
}

#[test]
fn printf_with_integer_arg()
{
	ok(r#"
			fn! main() {
				var n: i64 = 42i64;
				printf(c"%d\n", n);
			}
		"#);
}

// ── `@if cfg::os` conditional compilation ───────────────────────────────

#[test]
fn cfg_os_conditional_module()
{
	// Both branches expose the same `pub fn test()` so callers always type-check
	ok(r#"
			module t2 {
				@if cfg::os == "linux" {
					pub fn test() { var _: &str = "Linux"; }
				} @else {
					pub fn test() { var _: &str = "Other"; }
				}
			}
			fn! main() { t2::test(); }
		"#);
}

#[test]
#[ignore = "for now, the `@if cfg::` is done in the lexer, so type checking can't be tested yet"]
fn cfg_branch_type_mismatch_is_error()
{
	// If the two branches disagree on the return type the checker must catch it
	err(
		r#"
			module t2 {
				@if cfg::os == "linux" {
					pub fn test() -> i64 { return 1i64; }
				} @else {
					pub fn test() -> bool { return true; }
				}
			}
			fn! main() {
				var _: i64 = t2::test();
			}
		"#,
		"type mismatch",
	);
}

// ── block expressions as values ──────────────────────────────────────────

#[test]
fn block_expression_as_initializer()
{
	// `A{ a -> { var c: i64 = 0; c } }` — block whose tail is the value
	ok(r"
			struct A { a: i64 }
			fn! main() {
				var _: A = A{ a -> { var c: i64 = 0i64; c } };
			}
		");
}

#[test]
fn block_expression_wrong_tail_type_is_error()
{
	err(
		r"
			struct A { a: i64 }
			fn! main() {
				var _: A = A{ a -> { var c: bool = true; c } };
			}
		",
		"field `a`",
	);
}

// ── range expressions ────────────────────────────────────────────────────

#[test]
fn exclusive_range_i64()
{
	ok(r"
			fn! main() {
				var r: Range<i64> = std::Range::new(1i64, 10i64);
			}
		");
}

#[test]
fn range_used_in_for_loop()
{
	ok(r"
			fn! main() {
				for i: i64 in 1i64..10i64 {}
			}
		");
}

#[test]
fn range_element_type_mismatch_is_error()
{
	err(
		r"
			fn! main() {
				for i: bool in 1i64..10i64 {}
			}
		",
		"type mismatch",
	);
}

// ── multiple `@use` and path resolution ──────────────────────────────────

#[test]
fn use_glob_import()
{
	// `@use t2::*` should bring all pub items into scope
	ok(r"
			module t2 {
				pub fn greet() {}
				pub fn farewell() {}
			}
			@use t2::*;
			fn! main() {
				greet();
				farewell();
			}
		");
}

#[test]
fn use_specific_item()
{
	ok(r"
			module t2 {
				pub fn greet() {}
				pub fn other() {}
			}
			@use t2::greet;
			fn! main() {
				greet();
			}
		");
}

#[test]
fn call_non_imported_item_is_error()
{
	err(
		r"
			module t2 {
				pub fn greet() {}
				pub fn other() {}
			}
			@use t2::greet;
			fn! main() {
				other();
			}
		",
		"unresolved identifier",
	);
}

// ── iterator + Create trait bound ────────────────────────────────────────

#[test]
fn generic_fn_with_multiple_bounds()
{
	// mirrors `fn!<…> test3<T: Iterator<Item = i64> + Create>(input: T) -> impl Int`
	ok(r"
			fn!<alloc -> std::CAlloc> test3<T: Iterator<Item = i64> + Create>(input: T) -> impl Int {
				var a: T = input;
				input = T::create();
				return 0i64;
			}
		");
}

#[test]
fn generic_missing_bound_is_error()
{
	err(
		r"
			fn!<alloc -> std::CAlloc> test3<T: Iterator<Item = i64>>(input: T) -> impl Int {
				input = T::create();   // Create bound missing → T::create() unresolved
				return 0i64;
			}
		",
		"unresolved",
	);
}

// default()

#[test]
fn default_in_struct_with_default_value()
{
	ok(r"
		struct Foo {
			x: i64,
			y: i64 = 0,
		}
		fn a() -> Foo {
			return Foo{x -> 1, ..default()};
		}
		");
}

#[test]
fn default_in_struct_without_default_value()
{
	err(
		r"
		struct Foo {
			x: i64,
			y: i64 = 0,
		}
		fn a() -> Foo {
			return Foo{y -> 1, ..default()};
		}
		",
		"",
	);
}

#[test]
fn default_outside_of_struct()
{
	err(
		r"
		fn a() -> i64 {
			return default();
		}
		",
		"",
	);
}
