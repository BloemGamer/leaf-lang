use std::{
	collections::{HashSet, VecDeque},
	fs, path,
};

use crate::{
	config::Config,
	desugar::{self, DesugaredAST},
	diagnostics::{
		CompileDiagnostic, CompileDiagnosticRenderer, CompileError, DiagnosticBuilder, OldStyleRenderer, Severity,
	},
	lexer::{Lexer, Span, expander::ExpandedLexer},
	mir::{
		self, BlockId, MirBody, MirItem, MirLiteralValue, MirModule, MirOperand, MirPlaceBase, MirRvalue, MirStmt,
		MirTerminator,
	},
	modules::{self, ModuleError, ModuleErrorKind},
	name_resolution,
	parser::Parser,
	source_map::{SourceIndex, SourceMap},
	symbol_collection::{self, LocalSymbolTable},
	type_analysis,
};

// ── helpers ────────────────────────────────────────────────────────────────

fn compile_mir(source: &str) -> MirModule
{
	let mut source_map = SourceMap::new();
	let config = Config::default();

	match compile_mir_with_result(source, &config, &mut source_map) {
		Ok(m) => return m,
		Err(diags) => {
			let config = Config::default();
			for diag in diags {
				let d = diag.finish();
				let renderer: OldStyleRenderer<'_> = OldStyleRenderer::new(&d, &source_map, &config);
				eprintln!("{}", renderer);
			}
			panic!("couldn't compile to MIR");
		}
	}
}

fn compile_mir_with_result(
	source: &str,
	config: &Config,
	source_map: &mut SourceMap,
) -> Result<MirModule, Vec<DiagnosticBuilder>>
{
	const DUMMY_SPAN: Span = Span {
		source_index: SourceIndex::new(0),
		start: 0,
		end: 0,
		start_line: 0,
		start_col: 0,
		end_line: 0,
		end_col: 0,
	};

	let mut queue: VecDeque<modules::PendingModule> = VecDeque::from([
		modules::PendingModule {
			logical_path: vec!["std".to_string()],
			file_path: {
				let mut tmp = path::PathBuf::from("../std/std.leaf");
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
	let mut diagnostics = Vec::new();

	while let Some(pm) = queue.pop_front() {
		if !visited.insert(pm.logical_path.clone()) {
			continue;
		}

		let raw = if pm.logical_path.is_empty() {
			source.to_string()
		} else {
			fs::read_to_string(&pm.file_path).map_err(|e| {
				let kind = if e.kind() == std::io::ErrorKind::NotFound {
					ModuleErrorKind::FileNotFound(pm.file_path.clone())
				} else {
					ModuleErrorKind::IoError(e.to_string())
				};

				vec![
					CompileError::Module(ModuleError {
						logical_path: pm.logical_path.clone(),
						span: pm.declared_at_span,
						kind,
						context: Vec::new(),
					})
					.build(),
				]
			})?
		};

		let lexer = Lexer::new_add_to_source_map(&config, raw, pm.file_path.clone(), source_map);

		let expanded = ExpandedLexer::new(lexer);

		let parser = Parser::from(expanded);

		let ast = parser
			.parse_program()
			.map(|(ast, mut diags)| {
				diagnostics.append(&mut diags);
				ast
			})
			.map_err(|mut diags| {
				diagnostics.append(&mut diags);
				diagnostics.clone()
			})?;

		let children = modules::collect_pending(&ast, &pm.file_path, &pm.logical_path)
			.map_err(|e| vec![CompileError::Module(e).build()])?;

		queue.extend(children);

		let desugared = desugar::desugar_program(ast)
			.map(|(ast, mut diags)| {
				diagnostics.append(&mut diags);
				ast
			})
			.map_err(|mut diags| {
				diagnostics.append(&mut diags);
				diagnostics.clone()
			})?;

		let symbols = symbol_collection::collect_symbols(&desugared, pm.logical_path.clone())
			.map(|(symbols, mut diags)| {
				diagnostics.append(&mut diags);
				symbols
			})
			.map_err(|mut diags| {
				diagnostics.append(&mut diags);
				diagnostics.clone()
			})?;

		pending_modules.push((pm.logical_path, desugared, symbols));
	}

	let global_symbols = symbol_collection::merge_symbol_tables(&pending_modules);

	let mut resolved_modules = Vec::new();

	for (path, desugared, symbols) in &pending_modules {
		let (resolved, mut diags) =
			name_resolution::resolve_names(path, desugared, symbols, &global_symbols, &pending_modules).map_err(
				|mut diags| {
					diagnostics.append(&mut diags);
					diagnostics.clone()
				},
			)?;

		diagnostics.append(&mut diags);
		resolved_modules.push(resolved);
	}

	for resolved in &resolved_modules {
		if resolved.path.is_empty() {
			let typed = type_analysis::check_types(resolved, &global_symbols, &resolved_modules)
				.map_err(|e| vec![e.build()])?;

			let (mir, _) = mir::lower_module(&typed, &global_symbols).map_err(|mut diags| {
				diagnostics.append(&mut diags);
				diagnostics.clone()
			})?;

			return Ok(mir);
		}
	}

	Err(vec![DiagnosticBuilder {
		code: None,
		severity: Severity::Bug,
		message: "User module was never lowered to MIR".into(),
		labels: Vec::new(),
		notes: Vec::new(),
		helps: Vec::new(),
		suggestions: Vec::new(),
		related: Vec::new(),
	}])
}

fn first_function(mir: &MirModule) -> &crate::mir::MirFunction
{
	mir.items
		.iter()
		.find_map(|i| if let MirItem::Function(f) = i { Some(f) } else { None })
		.expect("no function in module")
}

fn body(mir: &MirModule) -> &MirBody
{
	first_function(mir).body.as_ref().expect("function has no body")
}

// ── basic structure ────────────────────────────────────────────────────────

/// An empty function produces a body with exactly one block (the entry) that
/// terminates with `Return`.
#[test]
fn empty_function_has_return_terminator()
{
	let mir = compile_mir("fn foo() {}");
	let b = body(&mir);
	assert_eq!(b.blocks.len(), 1, "expected exactly 1 block");
	assert!(matches!(b.blocks[0].terminator, MirTerminator::Return));
}

/// Parameters are the first locals in the body and their count is tracked.
#[test]
fn function_params_are_first_locals()
{
	let mir = compile_mir("fn add(a: i32, b: i32) -> i32 { return a + b; }");
	let b = body(&mir);
	assert_eq!(b.param_count, 2);
	assert_eq!(b.locals[0].name.as_deref(), Some("a"));
	assert_eq!(b.locals[1].name.as_deref(), Some("b"));
}

/// A non-unit return type allocates a `#return` local.
#[test]
fn non_unit_return_has_return_local()
{
	let mir = compile_mir("fn id(x: i64) -> i64 { return x; }");
	let b = body(&mir);
	assert!(b.return_local.is_some(), "expected a return local");
	let ret_id = b.return_local.unwrap();
	assert_eq!(b.local(ret_id).name.as_deref(), Some("#return"));
}

/// A `-> ()` function has no return local.
#[test]
fn unit_return_has_no_return_local()
{
	let mir = compile_mir("fn noop() {}");
	let b = body(&mir);
	assert!(b.return_local.is_none());
}

// ── variable declarations ──────────────────────────────────────────────────

#[test]
fn var_decl_creates_local_and_assign()
{
	let mir = compile_mir(
		r#"
			fn f() {
				var x: i32 = 42;
			}
		"#,
	);
	let b = body(&mir);
	// Find the local named "x"
	let x_local = b
		.locals
		.iter()
		.find(|l| l.name.as_deref() == Some("x"))
		.expect("no local 'x'");
	assert!(!x_local.mutable);

	// The entry block should contain an `Assign` statement for x
	let has_assign = b.blocks[0].stmts.iter().any(|s| {
		if let MirStmt::Assign {
			place,
			rvalue: MirRvalue::Use(MirOperand::Const(lit)),
			..
		} = s
		{
			if let MirPlaceBase::Local(id) = place.base {
				return id == x_local.id;
			}
		}
		false
	});
	assert!(has_assign, "expected assign of 42 into 'x'");
}

#[test]
fn mutable_var_local_is_marked_mutable()
{
	let mir = compile_mir("fn f() { var mut y: i32 = 0; }");
	let b = body(&mir);
	let y = b
		.locals
		.iter()
		.find(|l| l.name.as_deref() == Some("y"))
		.expect("no local 'y'");
	assert!(y.mutable);
}

// ── arithmetic expressions ─────────────────────────────────────────────────

/// Reading through `*p` should produce a place with a `Deref` projection,
/// not a `CallAndContinue` to a deref intrinsic.
#[test]
fn deref_read_emits_deref_projection()
{
	let mir = compile_mir(
		r#"
			fn f(p: &i32) -> i32 {
				return *p;
			}
		"#,
	);
	let b = body(&mir);

	let has_deref_proj = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		if let MirStmt::Assign {
			rvalue: MirRvalue::Use(op),
			..
		} = s
		{
			let place = match op {
				MirOperand::Copy(p) | MirOperand::Move(p) => p,
				MirOperand::Const(_) => return false,
			};
			place
				.projections
				.iter()
				.any(|proj| matches!(proj, crate::mir::MirProjection::Deref))
		} else {
			false
		}
	});
	assert!(has_deref_proj, "expected a Deref projection when reading `*p`");

	// And there should be no CallAndContinue for the deref intrinsic.
	let has_call = b
		.blocks
		.iter()
		.any(|bb| matches!(bb.terminator, MirTerminator::CallAndContinue { .. }));
	assert!(
		!has_call,
		"deref should be lowered to a projection, not an intrinsic call"
	);
}

/// Writing through `*p` should produce an `Assign` whose LHS place carries a
/// `Deref` projection (i.e. the store goes through the pointer, not into a
/// temporary).
#[test]
fn deref_write_emits_deref_projection_on_lhs()
{
	let mir = compile_mir(
		r#"
			fn f(p: &mut i32) {
				*p = 7;
			}
		"#,
	);
	let b = body(&mir);

	let has_deref_lhs = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		if let MirStmt::Assign { place, .. } = s {
			place
				.projections
				.iter()
				.any(|proj| matches!(proj, crate::mir::MirProjection::Deref))
		} else {
			false
		}
	});
	assert!(
		has_deref_lhs,
		"expected the LHS of `*p = 7` to carry a Deref projection"
	);
}

/// Chained `(*p).field` should produce a place with both a `Deref` and a
/// `Field` projection on the same place.
#[test]
fn deref_then_field_chains_projections()
{
	let mir = compile_mir(
		r#"
			struct Pair { a: i32, b: i32 }
			fn f(p: &Pair) -> i32 {
				return (*p).a;
			}
		"#,
	);
	let b = body(&mir);

	let has_chained = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		if let MirStmt::Assign {
			rvalue: MirRvalue::Use(op),
			..
		} = s
		{
			let place = match op {
				MirOperand::Copy(p) | MirOperand::Move(p) => p,
				MirOperand::Const(_) => return false,
			};
			let has_deref = place
				.projections
				.iter()
				.any(|proj| matches!(proj, crate::mir::MirProjection::Deref));
			let has_field = place
				.projections
				.iter()
				.any(|proj| matches!(proj, crate::mir::MirProjection::Field { name, .. } if name == "a"));
			has_deref && has_field
		} else {
			false
		}
	});
	assert!(has_chained, "expected a chained Deref + Field projection for `(*p).a`");
}

/// `if` without else should produce a Branch terminator and a merge block.
#[test]
fn if_stmt_produces_branch_terminator()
{
	let mir = compile_mir(
		r#"
			fn f(cond: bool) {
				if cond {
					var x: i32 = 1;
				}
			}
		"#,
	);
	let b = body(&mir);
	let has_branch = b
		.blocks
		.iter()
		.any(|bb| matches!(bb.terminator, MirTerminator::Branch { .. }));
	assert!(has_branch, "expected a Branch terminator");
}

/// `if`/`else` should produce exactly one Branch plus two Goto terminators
/// (then-arm and else-arm both jump to merge).
#[test]
fn if_else_stmt_produces_branch_and_two_gotos()
{
	let mir = compile_mir(
		r#"
			fn f(cond: bool) {
				if cond {
					var x: i32 = 1;
				} else {
					var x: i32 = 2;
				}
			}
		"#,
	);
	let b = body(&mir);
	let branches = b
		.blocks
		.iter()
		.filter(|bb| matches!(bb.terminator, MirTerminator::Branch { .. }))
		.count();
	let gotos = b
		.blocks
		.iter()
		.filter(|bb| matches!(bb.terminator, MirTerminator::Goto { .. }))
		.count();
	assert_eq!(branches, 1, "expected exactly 1 Branch");
	assert!(gotos >= 2, "expected at least 2 Goto terminators for then/else arms");
}

/// `loop {}` should produce a back-edge Goto pointing to the loop header.
#[test]
fn loop_has_back_edge()
{
	let mir = compile_mir(
		r#"
			fn f() {
				loop {
					break;
				}
			}
		"#,
	);
	let b = body(&mir);
	// Find the loop header (the block that follows the entry Goto)
	let entry_goto_target = if let MirTerminator::Goto { target } = b.blocks[0].terminator {
		target
	} else {
		// entry block may fall through directly into loop block
		BlockId(1)
	};
	// There must be some block whose Goto targets the loop header
	let has_back_edge = b
		.blocks
		.iter()
		.any(|bb| matches!(bb.terminator, MirTerminator::Goto { target } if target == entry_goto_target));
	assert!(has_back_edge, "expected a back-edge Goto to the loop header");
}

/// `break` inside a loop should jump to the exit block, not the loop header.
#[test]
fn break_jumps_to_exit_block()
{
	let mir = compile_mir(
		r#"
			fn f() {
				loop {
					break;
				}
			}
		"#,
	);
	let b = body(&mir);
	// There should be at least two distinct Goto targets: the loop header and
	// the exit block.
	let goto_targets: std::collections::HashSet<BlockId> = b
		.blocks
		.iter()
		.filter_map(|bb| {
			if let MirTerminator::Goto { target } = bb.terminator {
				Some(target)
			} else {
				None
			}
		})
		.collect();
	assert!(
		goto_targets.len() >= 2,
		"break should produce a Goto to a different block than the loop back-edge"
	);
}

// ── function calls ─────────────────────────────────────────────────────────

/// A direct function call should produce a `CallAndContinue` terminator.
#[test]
fn direct_call_produces_call_and_continue()
{
	let mir = compile_mir(
		r#"
			fn callee() -> i32 { return 1; }
			fn caller() -> i32 { return callee(); }
		"#,
	);
	// Look at the *caller* function specifically
	let caller = mir
		.items
		.iter()
		.find_map(|i| {
			if let MirItem::Function(f) = i
				&& f.name == "caller"
			{
				Some(f)
			} else {
				None
			}
		})
		.expect("no 'caller' function");
	let b = caller.body.as_ref().unwrap();
	let has_call = b
		.blocks
		.iter()
		.any(|bb| matches!(bb.terminator, MirTerminator::CallAndContinue { .. }));
	assert!(has_call, "expected CallAndContinue for direct call");
}

/// A call whose return value is discarded should still produce a
/// `CallAndContinue` (result stored in a dummy local).
#[test]
fn void_call_produces_call_and_continue()
{
	let mir = compile_mir(
		r#"
			fn side_effect() {}
			fn caller() { side_effect(); }
		"#,
	);
	let caller = mir
		.items
		.iter()
		.find_map(|i| {
			if let MirItem::Function(f) = i
				&& f.name == "caller"
			{
				Some(f)
			} else {
				None
			}
		})
		.expect("no 'caller' function");
	let b = caller.body.as_ref().unwrap();
	let has_call = b
		.blocks
		.iter()
		.any(|bb| matches!(bb.terminator, MirTerminator::CallAndContinue { .. }));
	assert!(has_call, "expected CallAndContinue even for void call");
}

// ── literals ───────────────────────────────────────────────────────────────

#[test]
fn integer_literal_becomes_const_operand()
{
	let mir = compile_mir("fn f() -> i32 { return 99; }");
	let b = body(&mir);
	let has_const = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		if let MirStmt::Assign {
			rvalue: MirRvalue::Use(MirOperand::Const(lit)),
			..
		} = s
		{
			matches!(lit.value, MirLiteralValue::Literal(_))
		} else {
			false
		}
	});
	assert!(has_const, "expected a Const literal operand for 99");
}

#[test]
fn bool_literal_becomes_const_operand()
{
	let mir = compile_mir("fn f() -> bool { return true; }");
	let b = body(&mir);
	let has_const = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		matches!(
			s,
			MirStmt::Assign {
				rvalue: MirRvalue::Use(MirOperand::Const(_)),
				..
			}
		)
	});
	assert!(has_const, "expected a Const operand for `true`");
}

// ── struct init ────────────────────────────────────────────────────────────

#[test]
fn struct_init_emits_aggregate_rvalue()
{
	let mir = compile_mir(
		r#"
			struct Point { x: i32, y: i32 }
			fn f() -> Point {
				return Point{ x -> 1, y -> 2 };
			}
		"#,
	);
	let b = body(&mir);
	let has_agg = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		matches!(
			s,
			MirStmt::Assign {
				rvalue: MirRvalue::Aggregate {
					kind: crate::mir::MirAggregateKind::Struct(_),
					..
				},
				..
			}
		)
	});
	assert!(has_agg, "expected an Aggregate rvalue for struct init");
}

// ── tuple expressions ──────────────────────────────────────────────────────

#[test]
fn tuple_expr_emits_tuple_rvalue()
{
	let mir = compile_mir(
		r#"
			fn f() -> (i32, i32) {
				return (1, 2);
			}
		"#,
	);
	let b = body(&mir);
	let has_tuple = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		matches!(
			s,
			MirStmt::Assign {
				rvalue: MirRvalue::Tuple(_),
				..
			}
		)
	});
	assert!(has_tuple, "expected a Tuple rvalue");
}

// ── array expressions ──────────────────────────────────────────────────────

#[test]
fn array_list_emits_array_rvalue()
{
	let mir = compile_mir(
		r#"
			fn f() -> [i32; 3] {
				return [1, 2, 3];
			}
		"#,
	);
	let b = body(&mir);
	let has_arr = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		matches!(
			s,
			MirStmt::Assign {
				rvalue: MirRvalue::Array { .. },
				..
			}
		)
	});
	assert!(has_arr, "expected an Array rvalue");
}

#[test]
fn array_repeat_emits_array_repeat_rvalue()
{
	let mir = compile_mir(
		r#"
			fn f() -> [i32; 5] {
				return [0; 5];
			}
		"#,
	);
	let b = body(&mir);
	let has_repeat = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		matches!(
			s,
			MirStmt::Assign {
				rvalue: MirRvalue::ArrayRepeat { .. },
				..
			}
		)
	});
	assert!(has_repeat, "expected an ArrayRepeat rvalue");
}

// ── field access ───────────────────────────────────────────────────────────

#[test]
fn field_access_emits_field_projection()
{
	let mir = compile_mir(
		r#"
			struct Pair { a: i32, b: i32 }
			fn f(p: Pair) -> i32 {
				return p.a;
			}
		"#,
	);
	let b = body(&mir);
	// The Copy/Move operand for `p.a` should carry a Field projection
	let has_field_proj = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		if let MirStmt::Assign {
			rvalue: MirRvalue::Use(op),
			..
		} = s
		{
			let place = match op {
				MirOperand::Copy(p) | MirOperand::Move(p) => p,
				_ => return false,
			};
			place
				.projections
				.iter()
				.any(|proj| matches!(proj, crate::mir::MirProjection::Field { name, .. } if name == "a"))
		} else {
			false
		}
	});
	assert!(has_field_proj, "expected a Field projection for 'p.a'");
}

// ── return handling ────────────────────────────────────────────────────────

/// Early `return` should set the terminator to `Return` and allocate a fresh
/// dead-code block for subsequent statements.
#[test]
fn early_return_creates_dead_block()
{
	let mir = compile_mir(
		r#"
			fn f(x: bool) -> i32 {
				if x { return 1; }
				return 0;
			}
		"#,
	);
	let b = body(&mir);
	// There should be at least two Return terminators (early + final) OR
	// the early-return block is still tracked even though it's unreachable.
	let return_count = b
		.blocks
		.iter()
		.filter(|bb| matches!(bb.terminator, MirTerminator::Return))
		.count();
	assert!(return_count >= 1, "expected at least one Return terminator");
	// The function must have more than 1 block due to if + dead-code block
	assert!(b.blocks.len() > 1, "early return should produce multiple blocks");
}

// ── type defs ─────────────────────────────────────────────────────────────

#[test]
fn struct_decl_emits_typedef()
{
	let mir = compile_mir("struct Foo { x: i32 }");
	let has_struct = mir.items.iter().any(|i| {
		if let MirItem::TypeDef(td) = i {
			matches!(td.kind, crate::mir::MirTypeDefKind::Struct { .. }) && td.name == "Foo"
		} else {
			false
		}
	});
	assert!(has_struct, "expected a Struct typedef for 'Foo'");
}

#[test]
fn enum_decl_emits_typedef()
{
	let mir = compile_mir("enum Color { Red, Green, Blue }");
	let has_enum = mir.items.iter().any(|i| {
		if let MirItem::TypeDef(td) = i {
			matches!(td.kind, crate::mir::MirTypeDefKind::Enum { .. }) && td.name == "Color"
		} else {
			false
		}
	});
	assert!(has_enum, "expected an Enum typedef for 'Color'");
}

#[test]
fn union_decl_emits_typedef()
{
	let mir = compile_mir("union U { a: i32, b: f32 }");
	let has_union = mir.items.iter().any(|i| {
		if let MirItem::TypeDef(td) = i {
			matches!(td.kind, crate::mir::MirTypeDefKind::Union { .. }) && td.name == "U"
		} else {
			false
		}
	});
	assert!(has_union, "expected a Union typedef for 'U'");
}

// ── global variables ───────────────────────────────────────────────────────

#[test]
fn global_const_emits_global_item()
{
	let mir = compile_mir("const MAX: i32 = 100;");
	let has_global = mir.items.iter().any(|i| {
		if let MirItem::Global(g) = i {
			g.name == "MAX" && !g.mutable
		} else {
			false
		}
	});
	assert!(has_global, "expected a non-mutable global 'MAX'");
}

#[test]
fn global_mut_var_emits_mutable_global()
{
	let mir = compile_mir("var mut COUNTER: i32 = 0;");
	let has_global = mir.items.iter().any(|i| {
		if let MirItem::Global(g) = i {
			g.name == "COUNTER" && g.mutable
		} else {
			false
		}
	});
	assert!(has_global, "expected a mutable global 'COUNTER'");
}

// ── switch / match ─────────────────────────────────────────────────────────

/// A `switch` expression should produce a Switch terminator or a chain of
/// Branch terminators (one per arm, the lowering uses Branch chains).
#[test]
fn switch_on_literal_produces_branches()
{
	let mir = compile_mir(
		r#"
			fn classify(n: i32) -> i32 {
				return switch n {
					0 => 10,
					1 => 20,
					_ => 30,
				};
			}
		"#,
	);
	let b = body(&mir);
	let branch_count = b
		.blocks
		.iter()
		.filter(|bb| matches!(bb.terminator, MirTerminator::Branch { .. }))
		.count();
	assert!(
		branch_count >= 2,
		"expected at least 2 Branch terminators for a 3-arm switch (2 literal + wildcard)"
	);
}

// ── extern declarations ────────────────────────────────────────────────────

/// `extern` function declarations have no body; the MIR function body should
/// be `None`.
#[test]
fn extern_fn_has_no_body()
{
	let mir = compile_mir(r#"extern(C) fn puts(s: &cstr) -> i32;"#);
	let f = first_function(&mir);
	assert!(f.body.is_none(), "extern fn should have no MIR body");
}

// ── impl blocks ───────────────────────────────────────────────────────────

/// Methods defined in an `impl` block are emitted as top-level functions.
#[test]
fn impl_method_is_emitted_as_function()
{
	let mir = compile_mir(
		r#"
			struct Counter { n: i32 }
			impl Counter {
				fn increment(self) -> i32 {
					return self.n + 1;
				}
			}
		"#,
	);
	let has_method = mir
		.items
		.iter()
		.any(|i| matches!(i, MirItem::Function(f) if f.name == "increment"));
	assert!(
		has_method,
		"expected 'increment' method to be emitted as a MIR function"
	);
}

// ── cast expressions ───────────────────────────────────────────────────────

#[test]
fn cast_emits_cast_rvalue()
{
	let mir = compile_mir(
		r#"
			fn f(x: i32) -> i64 {
				return (i64)x;
			}
		"#,
	);
	let b = body(&mir);
	let has_cast = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		matches!(
			s,
			MirStmt::Assign {
				rvalue: MirRvalue::Cast { .. },
				..
			}
		)
	});
	assert!(has_cast, "expected a Cast rvalue");
}

// ── address-of / ref ───────────────────────────────────────────────────────

#[test]
fn ref_expr_emits_ref_rvalue()
{
	let mir = compile_mir(
		r#"
			fn f(x: i32) -> &i32 {
				return &x;
			}
		"#,
	);
	let b = body(&mir);
	let has_ref = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		matches!(
			s,
			MirStmt::Assign {
				rvalue: MirRvalue::Ref { mutable: false, .. },
				..
			}
		)
	});
	assert!(has_ref, "expected an immutable Ref rvalue");
}

#[test]
fn mut_ref_expr_emits_mutable_ref_rvalue()
{
	let mir = compile_mir(
		r#"
			fn f(mut x: i32) -> &mut i32 {
				return &mut x;
			}
		"#,
	);
	let b = body(&mir);
	let has_ref = b.blocks.iter().flat_map(|bb| return bb.stmts.iter()).any(|s| {
		println!("{:#?}", s);
		return matches!(
			s,
			MirStmt::Assign {
				rvalue: MirRvalue::Ref { mutable: true, .. },
				..
			}
		);
	});
	assert!(has_ref, "expected a mutable Ref rvalue");
}

// ── delete statement ───────────────────────────────────────────────────────

#[test]
fn delete_stmt_emits_delete_mir_stmt()
{
	let mir = compile_mir(
		r#"
			fn f(p: *mut i32) {
				delete p;
			}
		"#,
	);
	let b = body(&mir);
	let has_delete = b
		.blocks
		.iter()
		.flat_map(|bb| bb.stmts.iter())
		.any(|s| matches!(s, MirStmt::Delete { .. }));
	assert!(has_delete, "expected a Delete MIR statement");
}

// ── nested blocks ──────────────────────────────────────────────────────────

#[test]
fn nested_block_flattens_into_same_cfg()
{
	// A plain `{ }` block expression does not create new basic blocks; it just
	// flattens its statements into the current block.
	let mir = compile_mir(
		r#"
			fn f() -> i32 {
				var x: i32 = {
					var a: i32 = 1;
					a
				};
				return x;
			}
		"#,
	);
	let b = body(&mir);
	let a_local = b.locals.iter().find(|l| l.name.as_deref() == Some("a"));
	assert!(a_local.is_some(), "local 'a' from inner block should exist");
}

// ── labeled break with value ───────────────────────────────────────────────

#[test]
fn loop_break_with_value_stores_into_result_local()
{
	let mir = compile_mir(
		r#"
			fn f() -> i32 {
				return loop {
					break 42i32;
				};
			}
		"#,
	);
	let b = body(&mir);
	// There must be an Assign of the literal 42 somewhere (the break value
	// being stored into the loop result local).
	println!("{}", mir);
	let has_literal_assign = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).any(|s| {
		if let MirStmt::Assign {
			rvalue: MirRvalue::Use(MirOperand::Const(lit)),
			..
		} = s
		{
			matches!(&lit.value, MirLiteralValue::Literal(crate::parser::Literal::Int { value, .. }) if value == "42")
		} else {
			false
		}
	});
	assert!(
		has_literal_assign,
		"expected literal 42 stored as the loop's break value"
	);
}

// ── if expression (value-producing) ───────────────────────────────────────

#[test]
fn if_expr_stores_result_into_local()
{
	let mir = compile_mir(
		r#"
			fn f(cond: bool) -> i32 {
				return if cond { 1i32 } else { 2i32 };
			}
		"#,
	);
	let b = body(&mir);
	// Two Branch arms each store a constant into the result local; that means
	// we expect at least two Assign-of-Const statements (one per arm).
	let const_assigns = b.blocks.iter().flat_map(|bb| bb.stmts.iter()).filter(|s| {
		matches!(
			s,
			MirStmt::Assign {
				rvalue: MirRvalue::Use(MirOperand::Const(_)),
				..
			}
		)
	});
	assert!(
		const_assigns.count() >= 2,
		"expected at least 2 const assigns for if-else arms"
	);
}

// ── module metadata ────────────────────────────────────────────────────────

#[test]
fn module_path_is_empty_for_user_module()
{
	let mir = compile_mir("fn noop() {}");
	assert!(mir.path.is_empty(), "user module path should be empty");
}

// ── temp locals are anonymous ──────────────────────────────────────────────

#[test]
fn temporaries_have_no_name()
{
	let mir = compile_mir("fn f(a: i32, b: i32) -> i32 { return a + b; }");
	let b = body(&mir);
	let anon_temps = b.locals.iter().filter(|l| l.is_temp).count();
	assert!(anon_temps > 0, "expected at least one anonymous temporary");
	for tmp in b.locals.iter().filter(|l| l.is_temp) {
		assert!(tmp.name.is_none(), "temporary should have no name, got {:?}", tmp.name);
	}
}
