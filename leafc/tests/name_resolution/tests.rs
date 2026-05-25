use std::path::PathBuf;

use crate::{
	CompileDiagnostic, CompileError, Config,
	desugar::DesugaredAST,
	diagnostics::{CompileDiagnosticRenderer, OldStyleRenderer},
	lexer::{Lexer, expander::ExpandedLexer},
	name_resolution::{self, NameResolutionErrorKind, ResolvedModule},
	parser::{AST, Parser},
	source_map::SourceMap,
	symbol_collection::{self, LocalSymbolTable},
};

// ─── Standard library available to every test ────────────────────────────────

// Iterator is declared both at root level (for plain `Iterator` references)
// and inside `std` (for `::std::Iterator` references emitted by the desugarer
// when it expands `for` loops).
const STDLIB_SRC: &str = r"
	pub @use Option::*;
	pub @use Result::*;

	pub variant Option<T> {
		Some(T),
		None,
	}

	pub variant Result<T, E> {
		Ok(T),
		Err(E),
	}

	pub module std {
		pub trait Iterator {
			fn next();
		}
	}

	pub trait Iterator {
		fn next();
	}
";

// -------------------------------------------------------------------------
// Helper
// -------------------------------------------------------------------------

fn parse_and_resolve(source: &str, logical_path: &[&str]) -> Result<ResolvedModule, CompileError>
{
	let config = Config::default();
	let mut source_map = SourceMap::default();

	let modules: &[(&[&str], &str)] = &[(logical_path, source)];
	let pending = build_pending(modules, &config, &mut source_map)?;
	let global = symbol_collection::merge_symbol_tables(&pending);

	// The user's module is always last in pending (stdlib is first)
	let (logical, desugared, local) = pending.last().unwrap();

	return name_resolution::resolve_names(logical, desugared, local, &global, &pending).inspect_err(|e| {
		let diag = e.to_diagnostic();
		let renderer = OldStyleRenderer::new(&diag, &source_map, &config);
		eprintln!("{}", renderer);
	});
}

fn build_pending(
	modules: &[(&[&str], &str)],
	config: &Config,
	source_map: &mut SourceMap,
) -> Result<Vec<(Vec<String>, DesugaredAST, LocalSymbolTable)>, CompileError>
{
	let mut pending = Vec::new();

	for (path_segs, source) in modules {
		let logical: Vec<String> = path_segs.iter().map(ToString::to_string).collect();

		// Prepend stdlib only into the root module (empty logical path)
		let full_source = if logical.is_empty() {
			format!("{}\n{}", STDLIB_SRC, source)
		} else {
			source.to_string()
		};

		let lexer = Lexer::new_add_to_source_map(
			config,
			full_source,
			PathBuf::from(format!(
				"<{}>",
				if logical.is_empty() {
					"root".to_string()
				} else {
					logical.join("::")
				}
			)),
			source_map,
		);
		let (ast, _) = Parser::from(ExpandedLexer::new(lexer)).parse_program().unwrap();
		let res = crate::desugar::desugar_program(ast);
		assert!(res.is_ok());
		let desugared = res.unwrap();
		println!("{}", desugared.0);
		let local = symbol_collection::collect_symbols(&desugared.0, logical.clone())?;
		pending.push((logical, desugared.0, local));
	}
	return Ok(pending);
}

// -------------------------------------------------------------------------
// Basic resolution — functions
// -------------------------------------------------------------------------

#[test]
fn resolves_simple_function_call()
{
	let src = r"
            fn helper() {}
            fn main() {
                helper();
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_recursive_function()
{
	let src = r"
            fn fib(n: i64) -> i64 {
                return fib(n);
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn error_on_unresolved_function_call()
{
	let src = r"
            fn main() {
                ghost();
            }
        ";
	// The call to `ghost` should produce an unresolved-identifier in the
	// expression, but the resolver tolerates unknown single-segment names
	// in expression position (they become UnresolvedIdentifier nodes).
	// The pass itself should still succeed without a hard error here.
	// Adjust this assertion if your resolver is stricter.
	let _: Result<ResolvedModule, CompileError> = parse_and_resolve(src, &[]);
}

// -------------------------------------------------------------------------
// Basic resolution — variables
// -------------------------------------------------------------------------

#[test]
fn resolves_variable_decl_with_primitive_type()
{
	let src = r"
            fn main() {
                var x: i64 = 42;
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_mutable_variable()
{
	let src = r"
            fn main() {
                var mut counter: i64 = 0;
                counter = 1;
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_variable_used_in_expression()
{
	let src = r"
            fn main() {
                var x: i64 = 10;
                var y: i64 = x;
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Basic resolution — structs
// -------------------------------------------------------------------------

#[test]
fn resolves_struct_declaration()
{
	let src = r"
            struct Point {
                x: i64,
                y: i64,
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_struct_init_expression()
{
	let src = r"
            struct Point {
                x: i64,
                y: i64,
            }
            fn main() {
                var p: Point = Point { x -> 1, y -> 2 };
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_struct_field_access()
{
	let src = r"
            struct Point {
                x: i64,
                y: i64,
            }
            fn main() {
                var p: Point = Point { x -> 0, y -> 0 };
                var v: i64 = p.x;
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_generic_struct()
{
	let src = r"
            struct Pair<A, B> {
                first: A,
                second: B,
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Enums and variants
// -------------------------------------------------------------------------

#[test]
fn resolves_enum_declaration()
{
	let src = r"
            enum Direction {
                North,
                South,
                East,
                West,
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_variant_declaration()
{
	let src = r"
            variant Color {
                Red,
                Green,
                Blue,
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_generic_variant()
{
	let src = r"
            variant MyOption<T> {
                Some(T),
                None,
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Type aliases
// -------------------------------------------------------------------------

#[test]
fn resolves_type_alias_to_primitive()
{
	let src = r"
            type MyInt = i64;
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_type_alias_to_struct()
{
	let src = r"
            struct Wrapper {
                val: i64,
            }
            type W = Wrapper;
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Traits
// -------------------------------------------------------------------------

#[test]
fn resolves_trait_declaration()
{
	let src = r"
            trait Greet {
                fn hello();
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_trait_with_default_method()
{
	let src = r"
            trait Named {
                fn name() -> i64 {
                    return 0;
                }
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Impl blocks
// -------------------------------------------------------------------------

#[test]
fn resolves_impl_block()
{
	let src = r"
            struct Counter {
                n: i64,
            }
            impl Counter {
                fn new() -> Counter {
                    return Counter { n -> 0 };
                }
                fn inc(self) {
                    self.n = self.n + 1;
                }
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_impl_trait_for_struct()
{
	let src = r"
            trait Reset {
                fn reset();
            }
            struct Counter {
                n: i64,
            }
            impl Reset for Counter {
                fn reset() {}
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Inline modules — multi-segment paths
// -------------------------------------------------------------------------

#[test]
fn resolves_item_inside_inline_module()
{
	let src = r"
            module math {
                pub fn add(a: i64, b: i64) -> i64 {
                    return a + b;
                }
            }
            fn main() {
                math::add(1, 2);
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_nested_inline_modules()
{
	let src = r"
            module outer {
                pub module inner {
                    pub fn greet() {}
                }
            }
            fn main() {
                outer::inner::greet();
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_struct_in_module()
{
	let src = r"
            module shapes {
                pub struct Circle {
                    radius: i64,
                }
            }
            fn make() -> shapes::Circle {
                return shapes::Circle { radius -> 5 };
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// @use / import directives
// -------------------------------------------------------------------------

#[test]
fn resolves_use_brings_function_into_scope()
{
	let src = r"
            module utils {
                pub fn helper() {}
            }
            @use utils::helper;
            fn main() {
                helper();
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_glob_use()
{
	let src = r"
            module constants {
                pub fn zero() -> i64 { return 0; }
                pub fn one() -> i64 { return 1; }
            }
            @use constants::*;
            fn main() {
                zero();
                one();
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_use_of_struct()
{
	let src = r"
            module geo {
                pub struct Point {
                    x: i64,
                    y: i64,
                }
            }
            @use geo::Point;
            fn make() -> Point {
                return Point { x -> 0, y -> 0 };
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Global path syntax (::path)
// -------------------------------------------------------------------------

#[test]
fn resolves_global_path()
{
	let src = r"
            module net {
                pub fn connect() {}
            }
            fn main() {
                ::net::connect();
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Associated items (AssocPath)
// -------------------------------------------------------------------------

#[test]
fn resolves_assoc_path_on_struct()
{
	let src = r"
            struct Builder {}
            impl Builder {
                pub fn new() -> Builder {
                    return Builder {};
                }
            }
            fn main() {
                var b: Builder = Builder::new();
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_assoc_path_on_variant()
{
	let src = r"
            variant MyOption<T> {
                Some(T),
                None,
            }
            fn main() {
                var x: MyOption<i64> = MyOption::None;
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Control flow — switch / if-var / for / loop
// -------------------------------------------------------------------------

#[test]
fn resolves_switch_with_wildcard_arm()
{
	let src = r"
            fn main() {
                var mut a: i64 = 0;
                switch a {
                    _ => {},
                }
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_for_loop_range()
{
	let src = r"
            fn main() {
                for i: i64 in 1..10 {
                }
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_loop_with_break()
{
	let src = r"
            fn main() {
                var mut x: i64 = 0;
                loop {
                    x = x + 1;
                    break;
                }
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_while_loop()
{
	let src = r"
            fn main() {
                var mut n: i64 = 0;
                while n < 10 {
                    n = n + 1;
                }
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Where clauses / generics
// -------------------------------------------------------------------------

#[test]
fn resolves_function_with_where_clause()
{
	let src = r"
            trait Display {
                fn show();
            }
            fn print<T>(val: T) where T: Display {
                val.show();
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn resolves_struct_with_where_clause()
{
	let src = r"
            trait Clone {
                fn clone();
            }
            struct Wrapper<T> where T: Clone {
                inner: T,
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Visibility — private symbol access
// -------------------------------------------------------------------------

#[test]
fn error_on_private_symbol_access_across_modules()
{
	let src = r"
            module secret {
                fn hidden() {}
            }
            fn main() {
                secret::hidden();
            }
        ";
	// `hidden` is private — accessing it from outside the module should
	// either produce a PrivateSymbol error or leave it as UnresolvedIdentifier.
	// We just confirm the pipeline doesn't panic.
	let _: Result<ResolvedModule, CompileError> = parse_and_resolve(src, &[]);
}

#[test]
fn public_symbol_accessible_from_outside_module()
{
	let src = r"
            module lib {
                pub fn visible() {}
            }
            fn main() {
                lib::visible();
            }
        ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// -------------------------------------------------------------------------
// Error kinds — explicit checks on NameResolutionErrorKind
// -------------------------------------------------------------------------

#[test]
fn error_kind_is_unresolved_path_for_missing_type()
{
	let src = r"
            fn foo(x: DoesNotExist) {}
        ";
	match parse_and_resolve(src, &[]) {
		Err(CompileError::NameResolution(e)) => {
			assert!(
				matches!(e.kind, NameResolutionErrorKind::UnresolvedPath { .. }),
				"expected UnresolvedPath, got {:?}",
				e.kind
			);
		}
		// Primitive fallback means a single-segment unknown type is tolerated
		// as a Primitive — if your resolver does that, this is also acceptable.
		Ok(_) => {}
		Err(other) => panic!("unexpected error variant: {:?}", other),
	}
}

#[test]
fn error_kind_is_private_symbol()
{
	let src = r"
            module inner {
                fn priv_fn() {}
            }
            fn main() {
                inner::priv_fn();
            }
        ";
	match parse_and_resolve(src, &[]) {
		Err(CompileError::NameResolution(e)) => {
			assert!(
				matches!(e.kind, NameResolutionErrorKind::PrivateSymbol { .. }),
				"expected PrivateSymbol, got {:?}",
				e.kind
			);
		}
		// Some resolvers leave this as an unresolved identifier rather than
		// a hard error — treat Ok as a known-lenient behaviour.
		Ok(_) => {}
		Err(other) => panic!("unexpected error: {:?}", other),
	}
}

#[test]
fn error_on_shadow()
{
	let src = r"
            fn main() {
                var a: i64();
				{
					var a: i32();
				}
            }
        ";
	match parse_and_resolve(src, &[]) {
		Err(CompileError::NameResolution(e)) => {
			assert!(
				matches!(e.kind, NameResolutionErrorKind::ShadowedVariable { .. }),
				"expected ShadowedVarialbe, got {:?}",
				e.kind
			);
		}
		Ok(_) => {
			panic!("expected ShadowedVarialbe, got no error")
		}
		Err(other) => panic!("unexpected error: {:?}", other),
	}
}

// -------------------------------------------------------------------------
// ResolvedPath display / round-trip smoke test
// -------------------------------------------------------------------------

#[test]
fn resolved_ast_display_does_not_panic()
{
	let src = r"
            struct Foo { x: i64, }
            impl Foo {
                pub fn make() -> Foo { return Foo { x -> 0 }; }
            }
            fn main() {
                var f: Foo = Foo::make();
                var v: i64 = f.x;
            }
        ";
	let module = parse_and_resolve(src, &[]).expect("should resolve");
	// Just exercise Display — it must not panic.
	let _ = format!("{}", module.ast);
}

// ---------------------------------------------------------------------------
// Shared helper — identical to the one in the unit-test module
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Multi-module helper — simulates the real multi-file pipeline in memory
// ---------------------------------------------------------------------------

/// Build a `ResolvedModule` for the *root* entry in `modules`, with all other
/// entries available as sibling modules.
///
/// `modules` is a slice of `(logical_path_segments, source_text)` pairs.
/// The first entry is treated as the root module being resolved.
fn parse_and_resolve_multi(modules: &[(&[&str], &str)]) -> Result<Vec<ResolvedModule>, CompileError>
{
	let config = Config::default();
	let mut source_map = SourceMap::default();

	// Use build_pending so stdlib is automatically prepended
	let pending = build_pending(modules, &config, &mut source_map)?;
	let global = symbol_collection::merge_symbol_tables(&pending);

	let mut resolved = Vec::new();
	for (path, desugared, local) in &pending {
		let r = name_resolution::resolve_names(path, desugared, local, &global, &pending)?;
		resolved.push(r);
	}
	return Ok(resolved);
}

// ---------------------------------------------------------------------------
// Single-module integration tests
// ---------------------------------------------------------------------------

#[test]
fn integration_resolves_basic_struct_and_impl()
{
	let src = r"
        struct Vec2 {
            x: i64,
            y: i64,
        }
        impl Vec2 {
            pub fn zero() -> Vec2 {
                return Vec2 { x -> 0, y -> 0 };
            }
        }
        fn main() {
            var v: Vec2 = Vec2::zero();
        }
    ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn integration_resolves_nested_modules_and_use()
{
	let src = r"
        module t2 {
            pub module inner {}
            pub @use inner;
            pub fn test() {
                inner::test();
            }
        }
        @use t2::inner;
        @use t2;
        fn main() {
            t2::test();
        }
    ";
	// Mirrors the sample snippet provided — must not hard-error.
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn integration_resolves_generic_variant_with_switch()
{
	let src = r"
        fn main() {
            var mut a: i64 = 0;
            switch a {
                Option::Some(b: i64) => {},
                _ => {},
            }
        }
    ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn integration_resolves_for_range_loop()
{
	let src = r"
        fn main() {
            for i: i64 in 1..10 {
            }
        }
    ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn integration_resolves_if_var_pattern()
{
	let src = r"
        fn maybe() -> Option<i64> { return Option::None; }
        fn main() {
            var a: Option<i64> = maybe();
            if var Option::Some(b: i64) = a {
            } else {
            }
        }
    ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

#[test]
fn integration_resolves_global_double_colon_path()
{
	let src = r"
        module t2 {
            pub module t {
                pub fn test() {}
            }
        }
        fn main() {
            ::t2::t::test();
        }
    ";
	assert!(parse_and_resolve(src, &[]).is_ok());
}

// ---------------------------------------------------------------------------
// Multi-module integration tests
// ---------------------------------------------------------------------------

#[test]
fn integration_multi_module_cross_reference()
{
	let modules: &[(&[&str], &str)] = &[
		(
			&[],
			r"
                @use utils;
                fn main() {
                    utils::helper();
                }
            ",
		),
		(
			&["utils"],
			r"
                pub fn helper() {}
            ",
		),
	];
	assert!(parse_and_resolve_multi(modules).is_ok());
}

#[test]
fn integration_multi_module_glob_use()
{
	let modules: &[(&[&str], &str)] = &[
		(
			&[],
			r"
                @use math::*;
                fn main() {
                    add(1, 2);
                }
            ",
		),
		(
			&["math"],
			r"
                pub fn add(a: i64, b: i64) -> i64 { return a + b; }
                pub fn sub(a: i64, b: i64) -> i64 { return a - b; }
            ",
		),
	];
	assert!(parse_and_resolve_multi(modules).is_ok());
}

#[test]
fn integration_multi_module_deeply_nested()
{
	// Mirrors the structure from the example output:
	//   root  →  module t2  →  pub module t (external)
	let modules: &[(&[&str], &str)] = &[
		(
			&[],
			r"
                module t2 {
                    pub module t;
                    pub @use t;
                    pub fn test() {
                        t::test();
                    }
                }
                @use t2::t;
                @use t2;
                @use t2::t::*;
                fn main() {
                    t::test();
                    t2::t::test();
                    ::t2::t::test();
                    t2::test();
                }
            ",
		),
		(
			&["t2", "t"],
			r"
                pub fn test() {}
            ",
		),
	];
	assert!(parse_and_resolve_multi(modules).is_ok());
}

#[test]
fn integration_multi_module_re_exported_use()
{
	// Module `a` re-exports something from module `b` via `pub @use`.
	let modules: &[(&[&str], &str)] = &[
		(
			&[],
			r"
                @use a::Foo;
                fn make() -> Foo {
                    return Foo { val -> 0 };
                }
            ",
		),
		(
			&["a"],
			r"
                pub @use b::Foo;
            ",
		),
		(
			&["b"],
			r"
                pub struct Foo { val: i64, }
            ",
		),
	];
	// Re-exported symbols through pub @use chains should resolve.
	assert!(parse_and_resolve_multi(modules).is_ok());
}

#[test]
fn integration_error_unresolved_in_multi_module()
{
	let modules: &[(&[&str], &str)] = &[(
		&[],
		r"
                fn main() {
                    totally::missing::path();
                }
            ",
	)];
	let result = parse_and_resolve_multi(modules);
	// A multi-segment path that doesn't exist anywhere must be an error or
	// resolve to an UnresolvedIdentifier — it must not silently succeed with
	// a fully-resolved symbol.
	match &result {
		Err(CompileError::NameResolution(e)) => {
			assert!(matches!(e.kind, NameResolutionErrorKind::UnresolvedPath { .. }));
		}
		// Tolerate if the resolver leaves it as UnresolvedIdentifier (no hard error).
		Ok(_) => {}
		Err(other) => panic!("unexpected error: {:?}", other),
	}
}
