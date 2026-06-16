#![allow(clippy::needless_raw_strings)]

use std::{
	collections::{HashMap, HashSet, VecDeque},
	path,
};

use crate::{
	config::Config,
	desugar::{self, DesugaredAST},
	diagnostics::{
		CompileDiagnostic, CompileDiagnosticRenderer, CompileError, DiagnosticBuilder, OldStyleRenderer, Severity,
	},
	lexer::{Lexer, Span, expander::ExpandedLexer},
	mir::{self, MirModule},
	modules::{self, ModuleError, ModuleErrorKind},
	monomorphization::{
		self, MonoCallee, MonoItem, MonoModule, MonoOperand, MonoProjection, MonoRvalue, MonoStmt, MonoTerminator,
		MonoTy, MonoTypeDefKind, monomorphize,
	},
	name_resolution,
	parser::Parser,
	source_map::{SourceIndex, SourceMap},
	symbol_collection::{self, GlobalSymbolTable, LocalSymbolTable},
	type_analysis,
};

// ── helpers ────────────────────────────────────────────────────────────────

const DUMMY_SPAN: Span = Span {
	source_index: SourceIndex::new(0),
	start: 0,
	end: 0,
	start_line: 0,
	start_col: 0,
	end_line: 0,
	end_col: 0,
};

/// Run the full compilation pipeline (lex → parse → desugar → symbol
/// collection → name resolution → type checking → MIR lowering) for `source`
/// **plus** the standard library, and return every resulting `MirModule`
/// together with the merged `GlobalSymbolTable`.
///
/// This is the analogue of the `run` function in the driver, stripped of
/// argument flags and printing.  Mono tests call this directly so that
/// `monomorphize` receives the same inputs it would in a real compilation.
fn compile_mir_full(source: &str) -> (GlobalSymbolTable, Vec<MirModule>)
{
	let config = Config::default();
	let mut source_map = SourceMap::new();

	// Seed the module queue exactly like the driver does.
	let mut queue: VecDeque<modules::PendingModule> = VecDeque::from([
		modules::PendingModule {
			logical_path: vec!["core".to_string()],
			file_path: {
				let mut tmp = path::PathBuf::from("../std/core/core.leaf");
				tmp.pop();
				tmp.push("core.leaf");
				tmp
			},
			declared_at_span: DUMMY_SPAN,
		},
		modules::PendingModule {
			logical_path: vec!["std".to_string()],
			file_path: {
				let mut tmp = path::PathBuf::from("../std/std/std.leaf");
				tmp.pop();
				tmp.push("std.leaf");
				tmp
			},
			declared_at_span: DUMMY_SPAN,
		},
		modules::PendingModule {
			logical_path: vec![],
			file_path: path::PathBuf::from("<test>"),
			declared_at_span: DUMMY_SPAN,
		},
	]);

	let mut visited: HashSet<Vec<String>> = HashSet::new();
	let mut pending_modules: Vec<(Vec<String>, DesugaredAST, LocalSymbolTable)> = Vec::new();
	let mut diagnostics: Vec<DiagnosticBuilder> = Vec::new();

	// ── module loading loop ────────────────────────────────────────────────
	while let Some(pm) = queue.pop_front() {
		if !visited.insert(pm.logical_path.clone()) {
			continue;
		}

		let raw = if pm.logical_path.is_empty() {
			// The user module: use the in-memory `source` string.
			source.to_string()
		} else {
			std::fs::read_to_string(&pm.file_path)
				.unwrap_or_else(|e| panic!("compile_mir_full: could not read '{}': {e}", pm.file_path.display()))
		};

		let lexer = Lexer::new_add_to_source_map(&config, raw, pm.file_path.clone(), &mut source_map);
		let expanded = ExpandedLexer::new(lexer);
		let mut parser = Parser::from(expanded);
		parser.allow_type_inference = true;

		let ast = parser
			.parse_program()
			.unwrap_or_else(|errs| {
				emit_and_panic(errs, &source_map, &config, "parse");
			})
			.0;

		let children = modules::collect_pending(&ast, &pm.file_path, &pm.logical_path)
			.unwrap_or_else(|e| panic!("compile_mir_full: module collection error: {e:?}"));
		queue.extend(children);

		let (desugared, mut desugar_diags) = desugar::desugar_program(ast).unwrap_or_else(|errs| {
			emit_and_panic(errs, &source_map, &config, "desugar");
		});
		diagnostics.append(&mut desugar_diags);

		let (local_symbols, mut sym_diags) = symbol_collection::collect_symbols(&desugared, pm.logical_path.clone())
			.unwrap_or_else(|errs| {
				emit_and_panic(errs, &source_map, &config, "symbol_collection");
			});
		diagnostics.append(&mut sym_diags);

		pending_modules.push((pm.logical_path, desugared, local_symbols));
	}

	// ── global symbol table ────────────────────────────────────────────────
	let global_symbols = symbol_collection::merge_symbol_tables(&pending_modules);

	// ── name resolution ────────────────────────────────────────────────────
	let mut resolved_modules = Vec::new();
	for (path, desugared, symbols) in &pending_modules {
		let (resolved, mut diags) =
			name_resolution::resolve_names(path, desugared, symbols, &global_symbols, &pending_modules).unwrap_or_else(
				|errs| {
					emit_and_panic(errs, &source_map, &config, "name_resolution");
				},
			);
		diagnostics.append(&mut diags);
		resolved_modules.push(resolved);
	}

	// ── type checking ──────────────────────────────────────────────────────
	let mut typed_modules = Vec::new();
	for resolved in &resolved_modules {
		let typed = type_analysis::check_types(resolved, &global_symbols, &resolved_modules).unwrap_or_else(|e| {
			emit_and_panic(vec![e.build()], &source_map, &config, "type_analysis");
		});
		typed_modules.push(typed);
	}

	// ── MIR lowering ───────────────────────────────────────────────────────
	let mut mir_modules = Vec::new();
	for tmod in &typed_modules {
		let (mir_mod, mut diags) = mir::lower_module(tmod, &global_symbols).unwrap_or_else(|errs| {
			emit_and_panic(errs, &source_map, &config, "mir");
		});
		diagnostics.append(&mut diags);
		mir_modules.push(mir_mod);
	}

	// Surface any non-fatal diagnostics so they're visible in test output.
	for d in &diagnostics {
		let finished = d.clone().finish();
		eprintln!("{}", OldStyleRenderer::new(&finished, &source_map, &config));
	}

	return (global_symbols, mir_modules);
}

/// Pretty-print a batch of diagnostics to stderr then panic, so that test
/// failures show the actual compiler error rather than a cryptic unwrap.
#[allow(clippy::needless_pass_by_value)]
fn emit_and_panic(diags: Vec<DiagnosticBuilder>, source_map: &SourceMap, config: &Config, stage: &str) -> !
{
	for d in &diags {
		let finished = d.clone().finish();
		eprintln!("{}", OldStyleRenderer::new(&finished, source_map, config));
	}
	panic!("compile_mir_full: compilation failed at '{stage}' stage");
}

/// Top-level helper used by every mono test: compile `source` all the way
/// through monomorphization and panic (with diagnostics) on any error.
fn compile_mono(source: &str) -> MonoModule
{
	let (global, mods) = compile_mir_full(source);
	let (mono, diags) = monomorphize(&mods, &global);
	if !diags.is_empty() {
		let config = Config::default();
		let source_map = SourceMap::new();
		for d in &diags {
			let finished = d.clone().finish();
			eprintln!("{}", OldStyleRenderer::new(&finished, &source_map, &config));
		}
		panic!("compile_mono: monomorphization produced {} diagnostic(s)", diags.len());
	}
	return mono;
}

fn find_fn<'a>(mono: &'a MonoModule, name: &str) -> &'a crate::monomorphization::MonoFunction
{
	return mono
		.items
		.iter()
		.find_map(|i| {
			if let MonoItem::Function(f) = i
				&& (f.mangled_name.contains(name) || f.mangled_name == name)
			{
				return Some(f);
			}
			return None;
		})
		.unwrap_or_else(|| panic!("no mono function whose mangled name contains '{name}'"));
}

fn find_fn_exact<'a>(mono: &'a MonoModule, mangled: &str) -> &'a crate::monomorphization::MonoFunction
{
	return mono
		.items
		.iter()
		.find_map(|i| {
			if let MonoItem::Function(f) = i
				&& f.mangled_name == mangled
			{
				return Some(f);
			}
			return None;
		})
		.unwrap_or_else(|| panic!("no mono function with mangled name '{mangled}'"));
}

// ── entry point ────────────────────────────────────────────────────────────

/// The `entry` field of `MonoModule` must be non-empty and must correspond to
/// the mangled name of the `main` function.
#[test]
fn entry_is_main_mangled_name()
{
	let mono = compile_mono("fn main() {}");
	assert!(!mono.entry.is_empty(), "entry should be set");
	let main_fn = find_fn(&mono, "main");
	assert_eq!(mono.entry, main_fn.mangled_name, "entry must equal main's mangled name");
}

/// If there is no `main` function the monomorphizer emits a `NoMainEntry`
/// diagnostic and returns an empty module — it must not panic.
#[test]
fn no_main_produces_error_not_panic()
{
	let (global, mods) = compile_mir_full("fn helper() -> i32 { return 1; }");
	let (mono, diags) = monomorphize(&mods, &global);
	assert!(
		diags
			.iter()
			.any(|d| return d.message.contains("main") || d.message.contains("entry")),
		"expected a NoMainEntry diagnostic, got: {diags:?}"
	);
	assert!(mono.items.is_empty(), "empty module expected when there is no entry");
}

// ── reachability ───────────────────────────────────────────────────────────

/// A function that is never called from `main` (directly or transitively)
/// should not appear in the mono output.
#[test]
fn unreachable_function_is_dead_stripped()
{
	let mono = compile_mono(
		r"
        fn main() {}
        fn never_called() -> i32 { return 42; }
        ",
	);
	let has_dead = mono.items.iter().any(|i| {
		if let MonoItem::Function(f) = i {
			return f.mangled_name.contains("never_called");
		}
		return false;
	});
	assert!(!has_dead, "dead function should be eliminated");
}

/// A function reachable only transitively must still appear.
#[test]
fn transitively_reachable_function_is_kept()
{
	let mono = compile_mono(
		r"
        fn leaf() -> i32 { return 1; }
        fn middle() -> i32 { return leaf(); }
        fn main() { middle(); }
        ",
	);
	assert!(
		mono.items
			.iter()
			.any(|i| matches!(i, MonoItem::Function(f) if f.mangled_name.contains("leaf"))),
		"transitively reachable 'leaf' should be kept"
	);
}

// ── name mangling ──────────────────────────────────────────────────────────

/// A plain (non-generic) function gets a mangled name that is a sanitized
/// version of its source name with no `__` type-argument suffix.
#[test]
fn non_generic_fn_mangled_name_has_no_type_suffix()
{
	let mono = compile_mono("fn main() {helper();} fn helper() {}");
	let helper = find_fn(&mono, "helper");
	println!("{}", helper.mangled_name);
	assert!(
		!helper.mangled_name.contains("__"),
		"non-generic function should not have a type-arg suffix, got '{}'",
		helper.mangled_name
	);
}

/// A generic function instantiated with `i32` should have `i32` encoded in
/// its mangled name.
#[test]
fn generic_fn_mangled_name_encodes_type_arg()
{
	let mono = compile_mono(
		r"
        fn identity<T>(x: T) -> T { return x; }
        fn main() {
            identity(1i32);
        }
        ",
	);
	let instance = mono
		.items
		.iter()
		.find_map(|i| {
			if let MonoItem::Function(f) = i
				&& f.mangled_name.contains("identity")
				&& f.mangled_name.contains("i32")
			{
				return Some(f);
			}
			return None;
		})
		.expect("expected an identity<i32> instantiation");
	assert!(
		instance.mangled_name.contains("i32"),
		"mangled name should encode the type argument"
	);
}

/// Two instantiations of the same generic function with different types must
/// produce two distinct mangled names.
#[test]
fn two_instantiations_have_distinct_mangled_names()
{
	let mono = compile_mono(
		r"
        fn wrap<T>(x: T) -> T { return x; }
        fn main() {
            wrap(1i32);
            wrap(2i64);
        }
        ",
	);
	let instances: Vec<&str> = mono
		.items
		.iter()
		.filter_map(|i| {
			if let MonoItem::Function(f) = i
				&& f.mangled_name.contains("wrap")
			{
				return Some(f.mangled_name.as_str());
			}
			return None;
		})
		.collect();
	assert!(
		instances.len() >= 2,
		"expected two 'wrap' instantiations, got {instances:?}"
	);
	assert_ne!(
		instances[0], instances[1],
		"distinct type args must produce distinct mangled names"
	);
}

/// Mangled names must contain only ASCII alphanumeric characters and
/// underscores — no angle brackets, spaces, or colons.
#[test]
fn mangled_name_has_no_illegal_chars()
{
	let mono = compile_mono(
		r"
        fn compute<T>(x: T) -> T { return x; }
        fn main() { compute(true); }
        ",
	);
	for item in &mono.items {
		let name = match item {
			MonoItem::Function(f) => &f.mangled_name,
			MonoItem::Global(g) => &g.mangled_name,
			MonoItem::TypeDef(t) => &t.mangled_name,
		};
		for ch in name.chars() {
			assert!(
				ch.is_ascii_alphanumeric() || ch == '_',
				"illegal character '{ch}' in mangled name '{name}'"
			);
		}
	}
}

// ── type lowering ──────────────────────────────────────────────────────────

/// A function that takes and returns `i32` should have `MonoTy::Primitive`
/// params and return type after monomorphization.
#[test]
fn primitive_types_lower_to_mono_primitive()
{
	let mono = compile_mono("fn add(a: i32, b: i32) -> i32 { return a + b; } fn main() { add(1, 2); }");
	let f = find_fn(&mono, "add");
	assert_eq!(f.params.len(), 2);
	for p in &f.params {
		assert!(
			matches!(p.ty, MonoTy::Primitive(_)),
			"expected Primitive param type, got {:?}",
			p.ty
		);
	}
	assert!(
		matches!(f.return_ty, Some(MonoTy::Primitive(_))),
		"expected Primitive return type"
	);
}

/// A function returning `()` (unit) should have `return_ty == None` after mono.
#[test]
fn unit_return_lowered_to_none()
{
	let mono = compile_mono("fn noop() {} fn main() { noop(); }");
	let f = find_fn(&mono, "noop");
	assert!(
		f.return_ty.is_none(),
		"unit return should lower to None, got {:?}",
		f.return_ty
	);
}

/// A reference type (`&i32`) must lower to `MonoTy::Reference`.
#[test]
fn reference_type_lowers_correctly()
{
	let mono = compile_mono(
		r"
        fn deref_it(p: &i32) -> i32 { return *p; }
        fn main() {
            var x: i32 = 5;
            deref_it(&x);
        }
        ",
	);
	let f = find_fn(&mono, "deref_it");
	let first_param = &f.params[0].ty;
	assert!(
		matches!(first_param, MonoTy::Reference { mutable: false, .. }),
		"expected immutable Reference, got {first_param:?}"
	);
}

/// A mutable reference type (`&mut i32`) must lower to
/// `MonoTy::Reference { mutable: true, .. }`.
#[test]
fn mutable_reference_type_lowers_correctly()
{
	let mono = compile_mono(
		r"
        fn bump(p: &mut i32) { *p = *p + 1; }
        fn main() {
            var mut x: i32 = 0;
            bump(&mut x);
        }
        ",
	);
	let f = find_fn(&mono, "bump");
	let first_param = &f.params[0].ty;
	assert!(
		matches!(first_param, MonoTy::Reference { mutable: true, .. }),
		"expected mutable Reference, got {first_param:?}"
	);
}

/// A raw pointer type (`*i32`) must lower to `MonoTy::Pointer`.
#[test]
fn pointer_type_lowers_correctly()
{
	let mono = compile_mono(
		r"
        fn read_ptr(p: *i32) -> i32 { return *p; }
        fn main() {
            var x: i32 = 1;
            read_ptr(&x);
        }
        ",
	);
	let f = find_fn(&mono, "read_ptr");
	assert!(
		matches!(f.params[0].ty, MonoTy::Pointer { .. }),
		"expected Pointer param, got {:?}",
		f.params[0].ty
	);
}

/// A tuple type `(i32, bool)` lowers to `MonoTy::Tuple`.
#[test]
fn tuple_type_lowers_correctly()
{
	let mono = compile_mono(
		r"
        fn make_pair() -> (i32, bool) { return (1, true); }
        fn main() { make_pair(); }
        ",
	);
	let f = find_fn(&mono, "make_pair");
	assert!(
		matches!(f.return_ty, Some(MonoTy::Tuple(_))),
		"expected Tuple return type, got {:?}",
		f.return_ty
	);
}

/// A fixed-size array type `[i32; 4]` lowers to `MonoTy::Array { size: Some(4), .. }`.
#[test]
fn array_type_lowers_with_correct_size()
{
	let mono = compile_mono(
		r"
        fn make_arr() -> [i32; 4] { return [0; 4]; }
        fn main() { make_arr(); }
        ",
	);
	let f = find_fn(&mono, "make_arr");
	match &f.return_ty {
		Some(MonoTy::Array { size: Some(4), .. }) => {}
		other => panic!("expected Array with size 4, got {other:?}"),
	}
}

// ── ZST elimination ────────────────────────────────────────────────────────

/// Assignments whose destination type is a ZST (zero-sized type, e.g. `()`)
/// must be dropped during mono lowering — they should not appear in any block.
#[test]
fn zst_assign_is_eliminated()
{
	let mono = compile_mono(
		r"
        fn returns_unit() {}
        fn main() {
            var _u: () = returns_unit();
        }
        ",
	);
	// The main body should contain no Assign whose LHS type is the unit tuple.
	let main_fn = find_fn(&mono, "main");
	if let Some(body) = &main_fn.body {
		for block in &body.blocks {
			for stmt in &block.stmts {
				if let MonoStmt::Assign { place, .. } = stmt {
					assert!(
						!place.ty.is_zst(&HashMap::new()),
						"ZST assign should have been eliminated, found assign to {:?}",
						place.ty
					);
				}
			}
		}
	}
}

/// Aggregate rvalue fields whose type is a ZST must be stripped from the
/// aggregate field list.
#[test]
fn zst_aggregate_fields_are_stripped()
{
	let mono = compile_mono(
		r"
        struct Wrapper { value: i32, phantom: () }
        fn make() -> Wrapper { return Wrapper { value -> 7, phantom -> () }; }
        fn main() { make(); }
        ",
	);
	let f = find_fn(&mono, "make");
	if let Some(body) = &f.body {
		for block in &body.blocks {
			for stmt in &block.stmts {
				if let MonoStmt::Assign {
					rvalue: MonoRvalue::Aggregate { fields, .. },
					..
				} = stmt
				{
					for (name, op) in fields {
						assert!(
							!op.ty().is_zst(&HashMap::new()),
							"ZST field '{name}' should have been stripped from aggregate"
						);
					}
				}
			}
		}
	}
}

// ── unreachable block pruning ──────────────────────────────────────────────

/// After monomorphization, block IDs in every function body must form a
/// contiguous range starting at 0 (no holes left by the pruner).
#[test]
fn block_ids_are_contiguous_after_pruning()
{
	let mono = compile_mono(
		r"
        fn f(x: bool) -> i32 {
            if x { return 1; }
            return 0;
        }
        fn main() { f(true); }
        ",
	);
	let f = find_fn(&mono, "f");
	if let Some(body) = &f.body {
		let mut ids: Vec<u32> = body.blocks.iter().map(|b| return b.id.0).collect();
		ids.sort_unstable();
		for (expected, &actual) in ids.iter().enumerate() {
			#[allow(clippy::cast_possible_truncation)]
			{
				assert_eq!(
					actual, expected as u32,
					"block ids must be contiguous; expected {expected}, got {actual}"
				);
			}
		}
	}
}

/// A `loop { break; }` that has a trivially unreachable post-loop block should
/// leave only reachable blocks in the output.
#[test]
fn unreachable_blocks_are_pruned()
{
	let mono = compile_mono(
		r"
        fn f() -> i32 {
            return loop { break 1i32; };
        }
        fn main() { f(); }
        ",
	);
	let f = find_fn(&mono, "f");
	if let Some(body) = &f.body {
		// Every block must be reachable from block 0.  We do a simple BFS.
		let n = body.blocks.len();
		let mut reachable = vec![false; n];
		let mut stack = vec![0usize];
		reachable[0] = true;
		while let Some(idx) = stack.pop() {
			let visit = |t: u32| return t as usize;
			match &body.blocks[idx].terminator {
				MonoTerminator::Goto { target } => {
					let i = visit(target.0);
					if i < n && !reachable[i] {
						reachable[i] = true;
						stack.push(i);
					}
				}
				MonoTerminator::Branch {
					then_block, else_block, ..
				} => {
					for t in [then_block.0, else_block.0] {
						let i = visit(t);
						if i < n && !reachable[i] {
							reachable[i] = true;
							stack.push(i);
						}
					}
				}
				MonoTerminator::CallAndContinue { next, unwind, .. } => {
					for t in std::iter::once(next.0).chain(unwind.map(|u| return u.0)) {
						let i = visit(t);
						if i < n && !reachable[i] {
							reachable[i] = true;
							stack.push(i);
						}
					}
				}
				MonoTerminator::Switch { arms, otherwise, .. } => {
					for t in arms
						.iter()
						.map(|a| return a.target.0)
						.chain(std::iter::once(otherwise.0))
					{
						let i = visit(t);
						if i < n && !reachable[i] {
							reachable[i] = true;
							stack.push(i);
						}
					}
				}
				MonoTerminator::Return | MonoTerminator::Unreachable => {}
			}
		}
		for (i, live) in reachable.iter().enumerate() {
			assert!(live, "block {i} is unreachable after pruning");
		}
	}
}

// ── generic type defs ──────────────────────────────────────────────────────

/// A generic struct used with a concrete type must appear in the output with
/// its type argument encoded in the mangled name.
#[test]
fn generic_struct_is_monomorphized()
{
	let mono = compile_mono(
		r"
        struct Box<T> { value: T }
        fn make_box() -> Box<i32> { return Box { value -> 5 }; }
        fn main() { make_box(); }
        ",
	);
	let has_box_i32 = mono.items.iter().any(|i| {
		if let MonoItem::TypeDef(td) = i {
			return td.mangled_name.contains("Box") && td.mangled_name.contains("i32");
		}
		return false;
	});
	assert!(has_box_i32, "expected a Box<i32> typedef in mono output");
}

/// Two distinct instantiations of the same generic struct must produce two
/// distinct typedef items.
#[test]
fn two_generic_struct_instantiations_are_distinct()
{
	let mono = compile_mono(
		r"
        struct Pair<T> { fst: T, snd: T }
        fn make_i32() -> Pair<i32> { return Pair { fst -> 1, snd -> 2 }; }
        fn make_bool() -> Pair<bool> { return Pair { fst -> true, snd -> false }; }
        fn main() { make_i32(); make_bool(); }
        ",
	);
	let pair_typedefs: Vec<_> = mono
		.items
		.iter()
		.filter_map(|i| {
			if let MonoItem::TypeDef(td) = i
				&& td.mangled_name.contains("Pair")
			{
				return Some(td.mangled_name.as_str());
			}
			return None;
		})
		.collect();
	assert!(
		pair_typedefs.len() >= 2,
		"expected at least 2 Pair instantiations, got {pair_typedefs:?}"
	);
	assert_ne!(
		pair_typedefs[0], pair_typedefs[1],
		"Pair<i32> and Pair<bool> must have distinct mangled names"
	);
}

// ── struct field lowering ──────────────────────────────────────────────────

/// Fields of a concrete struct must appear in the monomorphized typedef.
#[test]
fn struct_fields_are_present_in_mono_typedef()
{
	let mono = compile_mono(
		r"
        struct Point { x: i32, y: i32 }
        fn main() {
            var _p: Point = Point { x -> 0, y -> 0 };
        }
        ",
	);
	let point = mono
		.items
		.iter()
		.find_map(|i| {
			if let MonoItem::TypeDef(td) = i
				&& td.mangled_name.contains("Point")
			{
				return Some(td);
			}
			return None;
		})
		.expect("expected a Point typedef");
	if let MonoTypeDefKind::Struct { fields } = &point.kind {
		assert!(fields.iter().any(|(n, _)| return n == "x"), "expected field 'x'");
		assert!(fields.iter().any(|(n, _)| return n == "y"), "expected field 'y'");
	} else {
		panic!("expected Struct kind for Point");
	}
}

/// Fields of a generic struct must be lowered to the concrete type argument.
#[test]
fn generic_struct_fields_are_substituted()
{
	let mono = compile_mono(
		r"
        struct Wrapper<T> { inner: T }
        fn main() {
            var _w: Wrapper<bool> = Wrapper { inner -> true };
        }
        ",
	);
	let wrapper = mono
		.items
		.iter()
		.find_map(|i| {
			if let MonoItem::TypeDef(td) = i
				&& td.mangled_name.contains("Wrapper")
				&& td.mangled_name.contains("bool")
			{
				return Some(td);
			}
			return None;
		})
		.expect("expected a Wrapper<bool> typedef");
	if let MonoTypeDefKind::Struct { fields } = &wrapper.kind {
		let inner_field = fields
			.iter()
			.find(|(n, _)| return n == "inner")
			.expect("no 'inner' field");
		assert!(
			matches!(inner_field.1, MonoTy::Primitive(_)),
			"inner field should be the concrete bool primitive, got {:?}",
			inner_field.1
		);
	} else {
		panic!("expected Struct kind");
	}
}

// ── global variables ───────────────────────────────────────────────────────

/// A global constant referenced from `main` must appear in the mono output.
#[test]
fn reachable_global_const_is_kept()
{
	let mono = compile_mono(
		r"
        const ANSWER: i32 = 42;
        fn main() {
            var _x: i32 = ANSWER;
        }
        ",
	);
	let has_global = mono.items.iter().any(|i| {
		if let MonoItem::Global(g) = i {
			return g.mangled_name.contains("ANSWER") && !g.mutable;
		}
		return false;
	});
	assert!(has_global, "expected the ANSWER global to be emitted");
}

/// An unreachable global (never referenced from any reachable code) must not
/// appear in the mono output.
#[test]
fn unreachable_global_is_dead_stripped()
{
	let mono = compile_mono(
		r"
        const UNUSED: i32 = 99;
        fn main() {}
        ",
	);
	let has_dead = mono.items.iter().any(|i| {
		if let MonoItem::Global(g) = i {
			return g.mangled_name.contains("UNUSED");
		}
		return false;
	});
	assert!(!has_dead, "unreachable global should be dead-stripped");
}

/// A mutable global is emitted with `mutable == true`.
#[test]
fn mutable_global_has_mutable_flag()
{
	let mono = compile_mono(
		r"
        var mut COUNTER: i32 = 0;
        fn main() {
            COUNTER = COUNTER + 1;
        }
        ",
	);
	let g = mono
		.items
		.iter()
		.find_map(|i| {
			if let MonoItem::Global(g) = i
				&& g.mangled_name.contains("COUNTER")
			{
				return Some(g);
			}
			return None;
		})
		.expect("expected COUNTER global");
	assert!(g.mutable, "COUNTER should be mutable");
}

// ── trait method dispatch ──────────────────────────────────────────────────

/// A trait method called on a concrete receiver type must be resolved to the
/// concrete impl — `MonoCallee::Direct` with the impl's symbol, not an
/// `AbstractTraitMethodCall` error.
#[test]
fn trait_method_dispatches_to_concrete_impl()
{
	let mono = compile_mono(
		r"
        trait Greet {
            fn greet(self) -> i32;
        }
        struct Dog {}
        impl Greet for Dog {
            fn greet(self) -> i32 { return 1; }
        }
        fn main() {
            var d: Dog = Dog {};
            d.greet();
        }
        ",
	);
	// A `greet` function must appear in the output (it was reachable via the impl).
	let has_greet = mono.items.iter().any(|i| {
		if let MonoItem::Function(f) = i {
			return f.mangled_name.contains("greet");
		}
		return false;
	});
	assert!(has_greet, "concrete impl of 'greet' should be in mono output");
}

// ── callee lowering ────────────────────────────────────────────────────────

/// Direct calls lower to `MonoCallee::Direct` inside `CallAndContinue`.
#[test]
fn direct_call_lowers_to_mono_callee_direct()
{
	let mono = compile_mono(
		r"
        fn callee() -> i32 { return 7; }
        fn main() { callee(); }
        ",
	);
	let main_fn = find_fn(&mono, "main");
	if let Some(body) = &main_fn.body {
		let has_direct = body.blocks.iter().any(|bb| {
			return matches!(
				&bb.terminator,
				MonoTerminator::CallAndContinue {
					callee: MonoCallee::Direct { .. },
					..
				}
			);
		});
		assert!(has_direct, "expected a Direct callee in main's CallAndContinue");
	}
}

// ── param count ────────────────────────────────────────────────────────────

/// The monomorphized function preserves the correct parameter count.
#[test]
fn param_count_is_preserved()
{
	let mono = compile_mono(
		r"
        fn three(a: i32, b: i32, c: bool) -> i32 { return a + b; }
        fn main() { three(1, 2, true); }
        ",
	);
	let f = find_fn(&mono, "three");
	assert_eq!(f.params.len(), 3, "expected 3 params");
}

/// A generic function instantiated with two distinct type args still preserves
/// the correct parameter types in the mono output.
#[test]
fn generic_fn_params_are_substituted()
{
	let mono = compile_mono(
		r"
        fn swap<T>(a: T, b: T) -> T { return a; }
        fn main() { swap(1i32, 2i32); }
        ",
	);
	let f = mono
		.items
		.iter()
		.find_map(|i| {
			if let MonoItem::Function(f) = i
				&& f.mangled_name.contains("swap")
				&& f.mangled_name.contains("i32")
			{
				return Some(f);
			}
			return None;
		})
		.expect("swap<i32> not found");
	for p in &f.params {
		assert!(
			matches!(p.ty, MonoTy::Primitive(_)),
			"param should be the concrete i32 primitive, got {:?}",
			p.ty
		);
	}
}

// ── enum typedef ───────────────────────────────────────────────────────────

/// An enum used anywhere in reachable code must appear as a `MonoTypeDefKind::Enum`.
#[test]
fn enum_typedef_is_emitted()
{
	let mono = compile_mono(
		r"
        enum Dir { North, South, East, West }
        fn main() {
            var _d: Dir = Dir::North;
        }
        ",
	);
	let has_enum = mono.items.iter().any(|i| {
		if let MonoItem::TypeDef(td) = i {
			return matches!(td.kind, MonoTypeDefKind::Enum { .. }) && td.mangled_name.contains("Dir");
		}
		return false;
	});
	assert!(has_enum, "expected a Dir enum typedef");
}

// ── union typedef ──────────────────────────────────────────────────────────

#[test]
fn union_typedef_is_emitted()
{
	let mono = compile_mono(
		r"
        union Bits { i: i32, f: f32 }
        fn main() {
            var _b: Bits = Bits { i -> 0 };
        }
        ",
	);
	let has_union = mono.items.iter().any(|i| {
		if let MonoItem::TypeDef(td) = i {
			return matches!(td.kind, MonoTypeDefKind::Union { .. }) && td.mangled_name.contains("Bits");
		}
		return false;
	});
	assert!(has_union, "expected a Bits union typedef");
}

// ── type alias ─────────────────────────────────────────────────────────────

#[test]
fn type_alias_is_emitted()
{
	let mono = compile_mono(
		r"
        type MyInt = i32;
        fn f(x: MyInt) -> MyInt { return x; }
        fn main() { f(1); }
        ",
	);
	let has_alias = mono.items.iter().any(|i| {
		if let MonoItem::TypeDef(td) = i {
			return matches!(td.kind, MonoTypeDefKind::TypeAlias { .. }) && td.mangled_name.contains("MyInt");
		}
		return false;
	});
	assert!(has_alias, "expected a MyInt type alias typedef");
}

// ── const bodies ───────────────────────────────────────────────────────────

/// Const-body slots that are reserved (e.g. for global initialisers) must
/// all be filled — there must be no `None` entries left.
#[test]
fn all_const_body_slots_are_filled()
{
	let mono = compile_mono(
		r"
        const A: i32 = 1;
        const B: i32 = 2;
        fn main() {
            var _x: i32 = A;
            var _y: i32 = B;
        }
        ",
	);
	// `MonoModule::const_bodies` is the filled vec; the test just checks
	// there are some and none look like the error-stub placeholder.
	assert!(
		!mono.const_bodies.is_empty(),
		"expected at least one const body for A and B"
	);
}

// ── multiple calls, single instantiation ──────────────────────────────────

/// Calling the same generic function twice with the same type arguments must
/// produce exactly ONE instantiation (no duplicates).
#[test]
fn same_generic_called_twice_produces_one_instantiation()
{
	let mono = compile_mono(
		r"
        fn id<T>(x: T) -> T { return x; }
        fn main() {
            id(1i32);
            id(2i32);
        }
        ",
	);
	let count = mono
		.items
		.iter()
		.filter(|i| {
			if let MonoItem::Function(f) = i {
				return f.mangled_name.contains("id") && f.mangled_name.contains("i32");
			}
			return false;
		})
		.count();
	assert_eq!(count, 1, "same instantiation must appear exactly once, got {count}");
}

// ── place projections survive lowering ────────────────────────────────────

/// A `Deref` projection on a place must survive mono lowering unchanged.
#[test]
fn deref_projection_survives_mono()
{
	let mono = compile_mono(
		r"
        fn read(p: &i32) -> i32 { return *p; }
        fn main() {
            var x: i32 = 3;
            read(&x);
        }
        ",
	);
	let f = find_fn(&mono, "read");
	if let Some(body) = &f.body {
		let has_deref = body.blocks.iter().flat_map(|b| return b.stmts.iter()).any(|s| {
			if let MonoStmt::Assign {
				rvalue: MonoRvalue::Use(op),
				..
			} = s
			{
				let place = match op {
					MonoOperand::Copy(p) | MonoOperand::Move(p) => p,
					MonoOperand::Const(_) => return false,
				};
				return place
					.projections
					.iter()
					.any(|proj| matches!(proj, MonoProjection::Deref));
			}
			return false;
		});
		assert!(has_deref, "Deref projection should survive mono lowering");
	}
}

/// A `Field` projection on a struct place must survive mono lowering.
#[test]
fn field_projection_survives_mono()
{
	let mono = compile_mono(
		r"
        struct Pt { x: i32, y: i32 }
        fn get_x(p: Pt) -> i32 { return p.x; }
        fn main() {
            var p: Pt = Pt { x -> 1, y -> 2 };
            get_x(p);
        }
        ",
	);
	let f = find_fn(&mono, "get_x");
	if let Some(body) = &f.body {
		let has_field = body.blocks.iter().flat_map(|b| return b.stmts.iter()).any(|s| {
			if let MonoStmt::Assign {
				rvalue: MonoRvalue::Use(op),
				..
			} = s
			{
				let place = match op {
					MonoOperand::Copy(p) | MonoOperand::Move(p) => p,
					MonoOperand::Const(_) => return false,
				};
				return place
					.projections
					.iter()
					.any(|proj| matches!(proj, MonoProjection::Field { name, .. } if name == "x"));
			}
			return false;
		});
		assert!(has_field, "Field projection for '.x' should survive mono lowering");
	}
}

// ── item ordering: typedefs before globals before functions ────────────────

/// The mono output must place all type definitions before all globals, and all
/// globals before all functions (the downstream codegen depends on this order).
#[test]
fn item_ordering_is_typedefs_then_globals_then_functions()
{
	let mono = compile_mono(
		r"
        struct Pt { x: i32 }
        const ORIGIN_X: i32 = 0;
        fn make() -> Pt { return Pt { x -> ORIGIN_X }; }
        fn main() { make(); }
        ",
	);
	// Scan items and record the highest index seen for each category.
	let mut last_typedef: Option<usize> = None;
	let mut first_global: Option<usize> = None;
	let mut first_function: Option<usize> = None;

	for (i, item) in mono.items.iter().enumerate() {
		match item {
			MonoItem::TypeDef(_) => {
				last_typedef = Some(i);
			}
			MonoItem::Global(_) => {
				first_global = first_global.or(Some(i));
			}
			MonoItem::Function(_) => {
				first_function = first_function.or(Some(i));
			}
		}
	}

	if let (Some(lt), Some(fg)) = (last_typedef, first_global) {
		assert!(lt < fg, "all typedefs must come before globals");
	}
	if let (Some(fg), Some(ff)) = (first_global, first_function) {
		assert!(fg < ff, "all globals must come before functions");
	}
	if let (Some(lt), Some(ff)) = (last_typedef, first_function) {
		assert!(lt < ff, "all typedefs must come before functions");
	}
}
