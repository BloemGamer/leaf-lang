#[cfg(test)]
#[allow(clippy::module_inception)]
mod tests
{
	use crate::desugar::*;
	use crate::parser::{
		AssignOp, BinaryOp, FunctionSignature, GenericParam, ImplTarget, Literal, Param, Path, UnaryOp,
	};

	// Helper to create a simple identifier expression
	fn ident(name: &str) -> Expr
	{
		return Expr::Identifier {
			path: Path::simple(vec![name.to_string()], Span::default()),
			span: Span::default(),
		};
	}

	// Helper to create a literal int expression
	fn int_lit(value: i64) -> Expr
	{
		return Expr::Literal {
			value: Literal::Int(value),
			span: Span::default(),
		};
	}

	// Helper to create a literal bool expression
	fn bool_lit(value: bool) -> Expr
	{
		return Expr::Literal {
			value: Literal::Bool(value),
			span: Span::default(),
		};
	}

	// Helper to create a simple type
	fn simple_type(name: &str) -> Type
	{
		return Type {
			modifiers: vec![],
			core: Box::new(TypeCore::Base {
				path: Path::simple(vec![name.to_string()], Span::default()),
				generics: vec![],
			}),
			span: Span::default(),
		};
	}

	// Helper to create a typed identifier pattern
	fn typed_ident_pattern(name: &str, type_name: &str) -> Pattern
	{
		return Pattern::TypedIdentifier {
			path: Path::simple(vec![name.to_string()], Span::default()),
			ty: simple_type(type_name),
			call_constructor: None,
			span: Span::default(),
		};
	}

	// Helper to create a simple for loop pattern (variant with no args)
	fn simple_for_pattern(name: &str) -> Pattern
	{
		return Pattern::Variant {
			path: Path::simple(vec![name.to_string()], Span::default()),
			args: vec![],
			span: Span::default(),
		};
	}

	#[test]
	fn test_desugar_if_with_else_unchanged()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::If {
			cond: bool_lit(true),
			then_block: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			else_branch: Some(Box::new(Stmt::Block(Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			}))),
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(input);
		assert!(result.is_ok(), "desugar_stmt failed");
		let output = result.unwrap();

		match output {
			Stmt::If {
				else_branch: Some(_), ..
			} => {}
			_ => panic!("Expected if with else branch, got {:?}", output),
		}
	}

	#[test]
	fn test_compound_assignment_not_desugared()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::Assignment {
			target: ident("x"),
			op: AssignOp::AddAssign,
			value: int_lit(5),
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(input);
		assert!(result.is_ok(), "desugar_stmt failed");
		let output = result.unwrap();

		// Should keep AddAssign, NOT desugar to Add + Assign
		match output {
			Stmt::Assignment {
				op: AssignOp::AddAssign,
				..
			} => {
				// Success - compound assignment preserved
			}
			_ => panic!("Expected AddAssign to be preserved, got {:?}", output),
		}
	}

	#[test]
	fn test_gen_temp_unique()
	{
		let mut desugarer = Desugarer::new();

		let temp1 = desugarer.gen_temp("test");
		let temp2 = desugarer.gen_temp("test");
		let temp3 = desugarer.gen_temp("test");

		// Should generate unique names
		assert_ne!(temp1, temp2);
		assert_ne!(temp2, temp3);
		assert_ne!(temp1, temp3);
	}

	#[test]
	fn test_desugar_if_var()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::IfVar {
			pattern: typed_ident_pattern("x", "i32"),
			expr: int_lit(42),
			then_block: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			else_branch: None,
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(input).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Block(block) => {
				assert_eq!(block.stmts.len(), 2);
				assert!(matches!(block.stmts[0], Stmt::VariableDecl(_)));
				assert!(matches!(block.stmts[1], Stmt::Expr(Expr::Switch { .. })));
			}
			_ => panic!("Expected Stmt::Block, got {:?}", output),
		}
	}

	#[test]
	fn test_desugar_while_var_loop()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::WhileVarLoop {
			label: None,
			pattern: typed_ident_pattern("x", "i32"),
			expr: ident("some_value"),
			body: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(input).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Loop { body, .. } => {
				assert_eq!(body.stmts.len(), 2);
				assert!(matches!(body.stmts[0], Stmt::VariableDecl(_)));
				assert!(matches!(body.stmts[1], Stmt::Expr(Expr::Switch { .. })));
			}
			_ => panic!("Expected desugared loop, got {:?}", output),
		}
	}

	fn assert_no_expr_block(stmt: &Stmt)
	{
		match stmt {
			Stmt::Expr(Expr::Block(_)) => {
				panic!("Found Expr::Block used as statement: {:?}", stmt)
			}

			Stmt::Block(block) => {
				for stmt in &block.stmts {
					assert_no_expr_block(stmt);
				}
			}

			Stmt::If {
				then_block,
				else_branch,
				..
			} => {
				for stmt in &then_block.stmts {
					assert_no_expr_block(stmt);
				}
				if let Some(else_stmt) = else_branch {
					assert_no_expr_block(else_stmt);
				}
			}

			Stmt::Loop { body, .. } | Stmt::While { body, .. } | Stmt::Unsafe(body) => {
				for stmt in &body.stmts {
					assert_no_expr_block(stmt);
				}
			}

			_ => {}
		}
	}

	#[test]
	fn test_no_expr_block_in_desugared_if_var()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::IfVar {
			pattern: Pattern::Wildcard {
				span: Span::default(),
				ty: None,
			},
			expr: ident("x"),
			then_block: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			else_branch: None,
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		assert_no_expr_block(&output);
	}

	#[test]
	fn test_desugar_switch_arm_pattern_recursive()
	{
		let mut desugarer = Desugarer::new();

		let arm = SwitchArm {
			pattern: Pattern::Or {
				patterns: vec![
					Pattern::Variant {
						path: Path::simple(vec!["Some".into()], Span::default()),
						args: vec![Pattern::Wildcard {
							span: Span::default(),
							ty: None,
						}],
						span: Span::default(),
					},
					Pattern::Variant {
						path: Path::simple(vec!["None".into()], Span::default()),
						args: vec![],
						span: Span::default(),
					},
				],
				span: Span::default(),
			},
			body: SwitchBody::Block(Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_switch_arm(arm).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let out = result.unwrap();

		match out.pattern {
			Pattern::Or { patterns, .. } => assert_eq!(patterns.len(), 2),
			_ => panic!("Expected Pattern::Or"),
		}
	}

	#[test]
	fn test_desugar_for_loop_shape()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::For {
			label: None,
			pattern: simple_for_pattern("x"),
			iter: ident("iter"),
			body: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(input).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Block(block) => {
				assert_eq!(block.stmts.len(), 2);
				assert!(matches!(block.stmts[0], Stmt::VariableDecl(_)));
				assert!(matches!(block.stmts[1], Stmt::Loop { .. }));
			}
			_ => panic!("Expected desugared for-loop block"),
		}
	}

	#[test]
	fn test_nested_if_var_desugar()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::IfVar {
			pattern: typed_ident_pattern("x", "i32"),
			expr: ident("value"),
			then_block: Block {
				stmts: vec![Stmt::IfVar {
					pattern: Pattern::Wildcard {
						span: Span::default(),
						ty: None,
					},
					expr: ident("y"),
					then_block: Block {
						stmts: vec![],
						tail_expr: None,
						span: Span::default(),
					},
					else_branch: None,
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			},
			else_branch: None,
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(input).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// Top-level should be a block with variable decl and switch
		match output {
			Stmt::Block(block) => {
				assert_eq!(block.stmts.len(), 2);
				if let Stmt::Expr(Expr::Switch { arms, .. }) = &block.stmts[1] {
					// Then branch also should be desugared
					match &arms[0].body {
						SwitchBody::Block(inner_block) => {
							assert!(matches!(inner_block.stmts[0], Stmt::Block(_)));
						}
						SwitchBody::Expr(_) => panic!("Expected inner block in switch arm"),
					}
				} else {
					panic!("Expected Switch expression at top level");
				}
			}
			_ => panic!("Expected Stmt::Block"),
		}
	}

	#[test]
	fn test_for_tuple_pattern()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::For {
			label: None,
			pattern: Pattern::Tuple {
				patterns: vec![simple_for_pattern("x"), simple_for_pattern("y")],
				span: Span::default(),
			},
			iter: ident("iter"),
			body: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(input).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Block(block) => {
				assert_eq!(block.stmts.len(), 2);
				// Ensure the loop still exists
				assert!(matches!(block.stmts[1], Stmt::Loop { .. }));
			}
			_ => panic!("Expected Stmt::Block"),
		}
	}

	#[test]
	fn test_expr_block_in_call()
	{
		let mut desugarer = Desugarer::new();

		let input = Expr::Call {
			callee: Box::new(ident("foo")),
			call_type: CallType::Regular,
			named_generics: Vec::new(),
			args: vec![Expr::Block(Box::new(Block {
				stmts: vec![Stmt::VariableDecl(VariableDecl {
					pattern: typed_ident_pattern("x", "i32"),
					init: Some(ident("y")),
					comp_const: false,
					span: Span::default(),
				})],
				tail_expr: Some(Box::new(ident("x"))),
				span: Span::default(),
			}))],
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(input).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		if let Expr::Call { args, .. } = output {
			// Ensure the inner block is desugared into a Block expression
			assert!(matches!(args[0], Expr::Block(_)));
		} else {
			panic!("Expected Expr::Call");
		}
	}

	#[test]
	fn test_desugar_array_literals()
	{
		let mut desugarer = Desugarer::new();

		let list_array = ArrayLiteral::List {
			elements: vec![ident("x"), ident("y")],
			span: Span::default(),
		};
		let repeat_array = ArrayLiteral::Repeat {
			value: Box::new(ident("z")),
			count: Box::new(ident("n")),
			span: Span::default(),
		};

		let result_list = desugarer
			.desugar_array_literal(list_array)
			.inspect_err(|e| eprintln!("{e}"));
		assert!(result_list.is_ok());
		let desugared_list = result_list.unwrap();

		let result_repeat = desugarer
			.desugar_array_literal(repeat_array)
			.inspect_err(|e| eprintln!("{e}"));
		assert!(result_repeat.is_ok());
		let desugared_repeat = result_repeat.unwrap();

		match desugared_list {
			ArrayLiteral::List { elements, .. } => assert_eq!(elements.len(), 2),
			ArrayLiteral::Repeat { .. } => panic!("Expected List variant"),
		}

		match desugared_repeat {
			ArrayLiteral::Repeat { .. } => {}
			ArrayLiteral::List { .. } => panic!("Expected Repeat variant"),
		}
	}

	#[test]
	fn test_desugar_nested_switch_pattern()
	{
		let mut desugarer = Desugarer::new();
		let arm = SwitchArm {
			pattern: Pattern::Tuple {
				patterns: vec![
					Pattern::Variant {
						path: Path::simple(vec!["Some".into()], Span::default()),
						args: vec![Pattern::Wildcard {
							span: Span::default(),
							ty: None,
						}],
						span: Span::default(),
					},
					Pattern::Or {
						patterns: vec![
							Pattern::Wildcard {
								span: Span::default(),
								ty: None,
							},
							Pattern::Literal {
								value: Literal::Int(5),
								span: Span::default(),
							},
						],
						span: Span::default(),
					},
				],
				span: Span::default(),
			},
			body: SwitchBody::Block(Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_switch_arm(arm).unwrap();

		// The pattern should be expanded to:
		// (Some(_), _) | (Some(_), 5)
		match result.pattern {
			Pattern::Or { patterns, .. } => {
				assert_eq!(patterns.len(), 2);

				// First pattern: (Some(_), _)
				match &patterns[0] {
					Pattern::Tuple {
						patterns: tuple_patterns,
						..
					} => {
						assert_eq!(tuple_patterns.len(), 2);

						// First element: Some(_)
						assert!(matches!(
							&tuple_patterns[0],
							Pattern::Variant { path, args, .. }
							if path.segments.len() == 1
							&& path.segments[0].name == "Some"
							&& args.len() == 1
							&& matches!(args[0], Pattern::Wildcard { .. })
						));

						// Second element: _
						assert!(matches!(&tuple_patterns[1], Pattern::Wildcard { .. }));
					}
					_ => panic!("Expected first pattern to be a Tuple"),
				}

				// Second pattern: (Some(_), 5)
				match &patterns[1] {
					Pattern::Tuple {
						patterns: tuple_patterns,
						..
					} => {
						assert_eq!(tuple_patterns.len(), 2);

						// First element: Some(_)
						assert!(matches!(
							&tuple_patterns[0],
							Pattern::Variant { path, args, .. }
							if path.segments.len() == 1
							&& path.segments[0].name == "Some"
							&& args.len() == 1
							&& matches!(args[0], Pattern::Wildcard { .. })
						));

						// Second element: 5
						assert!(matches!(
							&tuple_patterns[1],
							Pattern::Literal {
								value: Literal::Int(5),
								..
							}
						));
					}
					_ => panic!("Expected second pattern to be a Tuple"),
				}
			}
			_ => panic!(
				"Expected desugared pattern to be an Or pattern, got: {:?}",
				result.pattern
			),
		}
	}

	#[test]
	fn test_desugar_top_level_program()
	{
		let mut desugarer = Desugarer::new();

		let program = Program {
			items: vec![TopLevelDecl::VariableDecl(VariableDecl {
				pattern: typed_ident_pattern("x", "i32"),
				init: Some(int_lit(42)),
				comp_const: true,
				span: Span::default(),
			})],
			span: Span::default(),
		};

		let result = desugarer.desugar_program(program).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();
		assert_eq!(output.items.len(), 1);
	}

	#[test]
	fn test_desugar_namespace_recursively()
	{
		let mut desugarer = Desugarer::new();

		let ns = NamespaceDecl {
			modifiers: vec![],
			name: Path::simple(vec!["test".into()], Span::default()),
			body: Program {
				items: vec![TopLevelDecl::Function(FunctionDecl {
					signature: FunctionSignature {
						modifiers: vec![],
						name: Path::simple(vec!["foo".into()], Span::default()),
						generics: vec![],
						params: vec![],
						return_type: None,
						where_clause: vec![],
						call_type: CallType::Regular,
						heap_generics: Vec::new(),
						span: Span::default(),
					},
					body: Some(Block {
						stmts: vec![Stmt::For {
							label: None,
							pattern: simple_for_pattern("i"),
							iter: ident("items"),
							body: Block {
								stmts: vec![],
								tail_expr: None,
								span: Span::default(),
							},
							span: Span::default(),
						}],
						tail_expr: None,
						span: Span::default(),
					}),
					span: Span::default(),
				})],
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_namespace(ns).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// Verify the for loop inside was desugared
		if let TopLevelDecl::Function(func) = &output.body.items[0]
			&& let Some(body) = &func.body
		{
			// For loop should be desugared to a block
			assert!(matches!(body.stmts[0], Stmt::Block(_)));
		}
	}

	#[test]
	fn test_desugar_impl_with_functions()
	{
		let mut desugarer = Desugarer::new();

		let impl_decl = ImplDecl {
			modifiers: vec![],
			generics: vec![],
			target: ImplTarget {
				path: Path::simple(vec!["MyType".into()], Span::default()),
				generics: vec![],
				span: Span::default(),
			},
			trait_path: None,
			where_clause: vec![],
			body: vec![ImplItem::Function(FunctionDecl {
				signature: FunctionSignature {
					modifiers: vec![],
					name: Path::simple(vec!["method".into()], Span::default()),
					generics: vec![],
					params: vec![],
					return_type: None,
					where_clause: vec![],
					call_type: CallType::Regular,
					heap_generics: Vec::new(),
					span: Span::default(),
				},
				body: Some(Block {
					stmts: vec![Stmt::WhileVarLoop {
						label: None,
						pattern: Pattern::Wildcard {
							span: Span::default(),
							ty: None,
						},
						expr: ident("x"),
						body: Block {
							stmts: vec![],
							tail_expr: None,
							span: Span::default(),
						},
						span: Span::default(),
					}],
					tail_expr: None,
					span: Span::default(),
				}),
				span: Span::default(),
			})],
			span: Span::default(),
		};

		let result = desugarer.desugar_impl(impl_decl).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();
		assert_eq!(output.body.len(), 1);
	}

	#[test]
	fn test_desugar_trait_with_default_impl()
	{
		let mut desugarer = Desugarer::new();

		let trait_decl = TraitDecl {
			modifiers: vec![],
			name: Path::simple(vec!["MyTrait".into()], Span::default()),
			generics: vec![],
			super_traits: vec![],
			items: vec![TraitItem::Function {
				signature: FunctionSignature {
					modifiers: vec![],
					name: Path::simple(vec!["method".into()], Span::default()),
					generics: vec![],
					params: vec![],
					return_type: None,
					where_clause: vec![],
					call_type: CallType::Regular,
					heap_generics: Vec::new(),
					span: Span::default(),
				},
				body: Some(Block {
					stmts: vec![Stmt::For {
						label: None,
						pattern: simple_for_pattern("i"),
						iter: ident("items"),
						body: Block {
							stmts: vec![],
							tail_expr: None,
							span: Span::default(),
						},
						span: Span::default(),
					}],
					tail_expr: None,
					span: Span::default(),
				}),
				span: Span::default(),
			}],
			span: Span::default(),
		};

		let result = desugarer.desugar_trait(trait_decl).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();
		assert_eq!(output.items.len(), 1);
	}

	#[test]
	fn test_desugar_return_with_nested_expr()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Return {
			value: Some(Expr::Binary {
				op: BinaryOp::Add,
				lhs: Box::new(Expr::Block(Box::new(Block {
					stmts: vec![],
					tail_expr: Some(Box::new(int_lit(1))),
					span: Span::default(),
				}))),
				rhs: Box::new(int_lit(2)),
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Return { value: Some(_), .. } => (),
			_ => panic!("Expected return statement"),
		}
	}

	#[test]
	fn test_desugar_assignment_with_complex_target()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Assignment {
			target: Expr::Index {
				base: Box::new(Expr::Field {
					base: Box::new(ident("obj")),
					name: Path::simple(vec!["arr".into()], Span::default()),
					span: Span::default(),
				}),
				index: Box::new(int_lit(0)),
				span: Span::default(),
			},
			op: AssignOp::Assign,
			value: int_lit(42),
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Assignment { .. } => (),
			_ => panic!("Expected assignment"),
		}
	}

	#[test]
	fn test_desugar_nested_for_in_if()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::If {
			cond: bool_lit(true),
			then_block: Block {
				stmts: vec![Stmt::For {
					label: None,
					pattern: simple_for_pattern("x"),
					iter: ident("items"),
					body: Block {
						stmts: vec![],
						tail_expr: None,
						span: Span::default(),
					},
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			},
			else_branch: None,
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::If { then_block, .. } => {
				// For loop inside should be desugared
				assert!(matches!(then_block.stmts[0], Stmt::Block(_)));
			}
			_ => panic!("Expected if statement"),
		}
	}

	#[test]
	fn test_desugar_while_with_complex_condition()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::While {
			label: None,
			cond: Expr::Binary {
				op: BinaryOp::Lt,
				lhs: Box::new(Expr::Call {
					callee: Box::new(ident("get_count")),
					call_type: CallType::Regular,
					named_generics: Vec::new(),
					args: vec![],
					span: Span::default(),
				}),
				rhs: Box::new(int_lit(10)),
				span: Span::default(),
			},
			body: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// While loops are desugared into loop { if !cond { break } body }
		match output {
			Stmt::Loop { body, .. } => {
				// First statement should be if !cond { break }
				assert!(matches!(body.stmts[0], Stmt::If { .. }));
				if let Stmt::If { cond, .. } = &body.stmts[0] {
					// Condition should be negated (Unary Not)
					assert!(matches!(cond, Expr::Unary { op: UnaryOp::Not, .. }));
				}
			}
			_ => panic!("Expected loop statement, got {:?}", output),
		}
	}

	#[test]
	fn test_desugar_loop_with_nested_structures()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Loop {
			label: None,
			body: Block {
				stmts: vec![Stmt::IfVar {
					pattern: Pattern::Wildcard {
						span: Span::default(),
						ty: None,
					},
					expr: ident("x"),
					then_block: Block {
						stmts: vec![Stmt::Break {
							label: None,
							value: None,
							span: Span::default(),
						}],
						tail_expr: None,
						span: Span::default(),
					},
					else_branch: None,
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Loop { body, .. } => {
				assert_eq!(body.stmts.len(), 1);
				// if var should be desugared inside
				assert!(matches!(body.stmts[0], Stmt::Block(_)));
			}
			_ => panic!("Expected loop statement"),
		}
	}

	#[test]
	fn test_desugar_unsafe_block_with_for()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Unsafe(Block {
			stmts: vec![Stmt::For {
				label: None,
				pattern: simple_for_pattern("i"),
				iter: ident("range"),
				body: Block {
					stmts: vec![],
					tail_expr: None,
					span: Span::default(),
				},
				span: Span::default(),
			}],
			tail_expr: None,
			span: Span::default(),
		});

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Unsafe(block) => {
				// For loop should be desugared inside unsafe block
				assert!(matches!(block.stmts[0], Stmt::Block(_)));
			}
			_ => panic!("Expected unsafe block"),
		}
	}

	#[test]
	fn test_desugar_expr_call_with_if_var_in_args()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Call {
			callee: Box::new(ident("foo")),
			call_type: CallType::Regular,
			named_generics: Vec::new(),
			args: vec![Expr::Block(Box::new(Block {
				stmts: vec![],
				tail_expr: Some(Box::new(int_lit(1))),
				span: Span::default(),
			}))],
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Call { args, .. } => {
				assert_eq!(args.len(), 1);
				assert!(matches!(args[0], Expr::Block(_)));
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_desugar_struct_init_with_block_fields()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::StructInit {
			path: Path::simple(vec!["Point".into()], Span::default()),
			fields: vec![
				(
					"x".into(),
					Expr::Block(Box::new(Block {
						stmts: vec![],
						tail_expr: Some(Box::new(int_lit(1))),
						span: Span::default(),
					})),
				),
				("y".into(), int_lit(2)),
			],
			base: None,
			span: Span::default(),
			has_rest: false,
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::StructInit { fields, .. } => {
				assert_eq!(fields.len(), 2);
			}
			_ => panic!("Expected struct init"),
		}
	}

	#[test]
	fn test_desugar_switch_with_nested_blocks()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Switch {
			expr: Box::new(ident("x")),
			arms: vec![SwitchArm {
				pattern: Pattern::Wildcard {
					span: Span::default(),
					ty: None,
				},
				body: SwitchBody::Block(Block {
					stmts: vec![Stmt::For {
						label: None,
						pattern: simple_for_pattern("i"),
						iter: ident("items"),
						body: Block {
							stmts: vec![],
							tail_expr: None,
							span: Span::default(),
						},
						span: Span::default(),
					}],
					tail_expr: None,
					span: Span::default(),
				}),
				span: Span::default(),
			}],
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Switch { arms, .. } => {
				match &arms[0].body {
					SwitchBody::Block(block) => {
						// For loop should be desugared
						assert!(matches!(block.stmts[0], Stmt::Block(_)));
					}
					SwitchBody::Expr(_) => panic!("Expected block body"),
				}
			}
			_ => panic!("Expected switch expression"),
		}
	}

	#[test]
	fn test_desugar_array_literal_list()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Array(ArrayLiteral::List {
			elements: vec![
				Expr::Binary {
					op: BinaryOp::Add,
					lhs: Box::new(int_lit(1)),
					rhs: Box::new(int_lit(2)),
					span: Span::default(),
				},
				int_lit(3),
			],
			span: Span::default(),
		});

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Array(ArrayLiteral::List { elements, .. }) => {
				assert_eq!(elements.len(), 2);
			}
			_ => panic!("Expected array literal"),
		}
	}

	#[test]
	fn test_desugar_array_repeat_complex()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Array(ArrayLiteral::Repeat {
			value: Box::new(Expr::Binary {
				op: BinaryOp::Mul,
				lhs: Box::new(ident("x")),
				rhs: Box::new(int_lit(2)),
				span: Span::default(),
			}),
			count: Box::new(int_lit(10)),
			span: Span::default(),
		});

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Array(ArrayLiteral::Repeat { value, .. }) => {
				assert!(matches!(*value, Expr::Binary { .. }));
			}
			_ => panic!("Expected array repeat"),
		}
	}

	#[test]
	fn test_desugar_field_access_chain()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Field {
			base: Box::new(Expr::Field {
				base: Box::new(ident("obj")),
				name: Path::simple(vec!["inner".into()], Span::default()),
				span: Span::default(),
			}),
			name: Path::simple(vec!["field".into()], Span::default()),
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Field { base, .. } => {
				assert!(matches!(*base, Expr::Field { .. }));
			}
			_ => panic!("Expected field access"),
		}
	}

	#[test]
	fn test_desugar_index_with_complex_expr()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Index {
			base: Box::new(ident("arr")),
			index: Box::new(Expr::Binary {
				op: BinaryOp::Add,
				lhs: Box::new(ident("i")),
				rhs: Box::new(int_lit(1)),
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Index { index, .. } => {
				assert!(matches!(*index, Expr::Binary { .. }));
			}
			_ => panic!("Expected index expression"),
		}
	}

	#[test]
	fn test_desugar_tuple_with_blocks()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Tuple {
			elements: vec![
				int_lit(1),
				Expr::Block(Box::new(Block {
					stmts: vec![],
					tail_expr: Some(Box::new(int_lit(2))),
					span: Span::default(),
				})),
				int_lit(3),
			],
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Tuple { elements, .. } => {
				assert_eq!(elements.len(), 3);
				assert!(matches!(elements[1], Expr::Block(_)));
			}
			_ => panic!("Expected tuple"),
		}
	}

	#[test]
	fn test_desugar_cast_with_nested_expr()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Cast {
			ty: Box::new(simple_type("i64")),
			expr: Box::new(Expr::Binary {
				op: BinaryOp::Add,
				lhs: Box::new(ident("a")),
				rhs: Box::new(ident("b")),
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Cast { expr, .. } => {
				assert!(matches!(*expr, Expr::Binary { .. }));
			}
			_ => panic!("Expected cast"),
		}
	}

	#[test]
	fn test_desugar_unary_with_nested_binary()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Unary {
			op: UnaryOp::Neg,
			expr: Box::new(Expr::Binary {
				op: BinaryOp::Mul,
				lhs: Box::new(ident("a")),
				rhs: Box::new(ident("b")),
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Unary { expr, .. } => {
				assert!(matches!(*expr, Expr::Binary { .. }));
			}
			_ => panic!("Expected unary expression"),
		}
	}

	#[test]
	fn test_desugar_pattern_variant_with_nested_patterns()
	{
		let mut desugarer = Desugarer::new();

		let pattern = Pattern::Variant {
			path: Path::simple(vec!["Some".into()], Span::default()),
			args: vec![Pattern::Tuple {
				patterns: vec![
					Pattern::Wildcard {
						span: Span::default(),
						ty: None,
					},
					typed_ident_pattern("x", "i32"),
				],
				span: Span::default(),
			}],
			span: Span::default(),
		};

		let result = desugarer.desugar_pattern(pattern).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Pattern::Variant { args, .. } => {
				assert_eq!(args.len(), 1);
				assert!(matches!(args[0], Pattern::Tuple { .. }));
			}
			_ => panic!("Expected variant pattern"),
		}
	}

	#[test]
	fn test_desugar_pattern_or_with_variants()
	{
		let mut desugarer = Desugarer::new();

		let pattern = Pattern::Or {
			patterns: vec![
				Pattern::Variant {
					path: Path::simple(vec!["Some".into()], Span::default()),
					args: vec![Pattern::Wildcard {
						span: Span::default(),
						ty: None,
					}],
					span: Span::default(),
				},
				Pattern::Variant {
					path: Path::simple(vec!["None".into()], Span::default()),
					args: vec![],
					span: Span::default(),
				},
			],
			span: Span::default(),
		};

		let result = desugarer.desugar_pattern(pattern).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Pattern::Or { patterns, .. } => {
				assert_eq!(patterns.len(), 2);
			}
			_ => panic!("Expected or pattern"),
		}
	}

	#[test]
	fn test_desugar_multiple_sequential_for_loops()
	{
		let mut desugarer = Desugarer::new();

		let block = Block {
			stmts: vec![
				Stmt::For {
					label: None,
					pattern: simple_for_pattern("i"),
					iter: ident("range1"),
					body: Block {
						stmts: vec![],
						tail_expr: None,
						span: Span::default(),
					},
					span: Span::default(),
				},
				Stmt::For {
					label: None,
					pattern: simple_for_pattern("j"),
					iter: ident("range2"),
					body: Block {
						stmts: vec![],
						tail_expr: None,
						span: Span::default(),
					},
					span: Span::default(),
				},
			],
			tail_expr: None,
			span: Span::default(),
		};

		let result = desugarer.desugar_block(block).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		assert_eq!(output.stmts.len(), 2);
		assert!(matches!(output.stmts[0], Stmt::Block(_)));
		assert!(matches!(output.stmts[1], Stmt::Block(_)));
	}

	#[test]
	fn test_desugar_variable_decl_with_pattern()
	{
		let mut desugarer = Desugarer::new();

		let var = VariableDecl {
			pattern: Pattern::Tuple {
				patterns: vec![
					typed_ident_pattern("x", "i32"),
					Pattern::Wildcard {
						span: Span::default(),
						ty: None,
					},
				],
				span: Span::default(),
			},
			init: Some(int_lit(42)),
			comp_const: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_variable_decl(var).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output.pattern {
			Pattern::Tuple { patterns, .. } => {
				assert_eq!(patterns.len(), 2);
			}
			_ => panic!("Expected tuple pattern"),
		}
	}

	#[test]
	fn test_gen_temp_with_different_names()
	{
		let mut desugarer = Desugarer::new();

		let temp1 = desugarer.gen_temp("loop");
		let temp2 = desugarer.gen_temp("ifvar");
		let temp3 = desugarer.gen_temp("whilevar");

		assert!(temp1.starts_with("#__tmp_"));
		assert!(temp2.starts_with("#__tmp_"));
		assert!(temp3.starts_with("#__tmp_"));
		assert_ne!(temp1, temp2);
		assert_ne!(temp2, temp3);
	}

	#[test]
	fn test_desugar_if_var_with_complex_pattern()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::IfVar {
			pattern: Pattern::Struct {
				path: Path::simple(vec!["Point".into()], Span::default()),
				fields: vec![("x".into(), typed_ident_pattern("x_val", "i32"))],
				has_rest: false,
				span: Span::default(),
			},
			expr: ident("point"),
			then_block: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			else_branch: None,
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Block(block) => {
				assert_eq!(block.stmts.len(), 2);
			}
			_ => panic!("Expected block"),
		}
	}

	#[test]
	fn test_desugar_while_var_with_nested_pattern()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::WhileVarLoop {
			label: None,
			pattern: Pattern::Variant {
				path: Path::simple(vec!["Ok".into()], Span::default()),
				args: vec![Pattern::Tuple {
					patterns: vec![
						Pattern::Wildcard {
							span: Span::default(),
							ty: None,
						},
						typed_ident_pattern("val", "i32"),
					],
					span: Span::default(),
				}],
				span: Span::default(),
			},
			expr: ident("result"),
			body: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Loop { body, .. } => {
				assert_eq!(body.stmts.len(), 2);
			}
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_desugar_labeled_for_loop()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::For {
			label: Some("outer".into()),
			pattern: simple_for_pattern("x"),
			iter: ident("iter"),
			body: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(input).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Block(block) => {
				// Second statement should be the loop with the label
				if let Stmt::Loop { label, .. } = &block.stmts[1] {
					assert_eq!(label, &Some("outer".into()));
				} else {
					panic!("Expected labeled loop");
				}
			}
			_ => panic!("Expected block"),
		}
	}

	#[test]
	fn test_desugar_break_with_value()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Loop {
			label: None,
			body: Block {
				stmts: vec![Stmt::Break {
					label: None,
					value: Some(int_lit(42)),
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Loop { body, .. } => {
				if let Stmt::Break { value, .. } = &body.stmts[0] {
					assert!(value.is_some());
				} else {
					panic!("Expected break statement");
				}
			}
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_desugar_continue_with_label()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Loop {
			label: Some("outer".into()),
			body: Block {
				stmts: vec![Stmt::Continue {
					label: Some("outer".into()),
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Loop { body, .. } => {
				if let Stmt::Continue { label, .. } = &body.stmts[0] {
					assert_eq!(label, &Some("outer".into()));
				} else {
					panic!("Expected continue statement");
				}
			}
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_desugar_if_expr()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::If {
			cond: Box::new(bool_lit(true)),
			then_block: Block {
				stmts: vec![],
				tail_expr: Some(Box::new(int_lit(1))),
				span: Span::default(),
			},
			else_branch: Some(Box::new(Expr::Block(Box::new(Block {
				stmts: vec![],
				tail_expr: Some(Box::new(int_lit(2))),
				span: Span::default(),
			})))),
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::If { .. } => (),
			_ => panic!("Expected if expression"),
		}
	}

	#[test]
	fn test_desugar_if_var_expr()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::IfVar {
			pattern: Pattern::Variant {
				path: Path::simple(vec!["Some".into()], Span::default()),
				args: vec![typed_ident_pattern("x", "i32")],
				span: Span::default(),
			},
			expr: Box::new(ident("opt")),
			then_block: Block {
				stmts: vec![],
				tail_expr: Some(Box::new(ident("x"))),
				span: Span::default(),
			},
			else_branch: None,
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Block(block) => {
				assert_eq!(block.stmts.len(), 1);
				assert!(block.tail_expr.is_some());
			}
			_ => panic!("Expected block expression"),
		}
	}

	#[test]
	fn test_desugar_loop_expr()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Loop {
			label: Some("outer".into()),
			body: Box::new(Block {
				stmts: vec![Stmt::Break {
					label: Some("outer".into()),
					value: Some(int_lit(42)),
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Loop { label, body, .. } => {
				assert_eq!(label, Some("outer".into()));
				assert_eq!(body.stmts.len(), 1);
			}
			_ => panic!("Expected loop expression"),
		}
	}

	#[test]
	fn test_desugar_unsafe_block_expr()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::UnsafeBlock(Box::new(Block {
			stmts: vec![Stmt::For {
				label: None,
				pattern: simple_for_pattern("i"),
				iter: ident("range"),
				body: Block {
					stmts: vec![],
					tail_expr: None,
					span: Span::default(),
				},
				span: Span::default(),
			}],
			tail_expr: None,
			span: Span::default(),
		}));

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::UnsafeBlock(block) => {
				// For loop should be desugared
				assert!(matches!(block.stmts[0], Stmt::Block(_)));
			}
			_ => panic!("Expected unsafe block expression"),
		}
	}

	#[test]
	fn test_unlabeled_while_gets_label()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::While {
			label: None,
			cond: bool_lit(true),
			body: Block {
				stmts: vec![Stmt::Continue {
					label: None,
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// While loops are desugared into loop statements
		match output {
			Stmt::Loop { label, body, .. } => {
				// Loop should have a generated label
				assert!(label.is_some());
				let loop_label = label.unwrap();
				assert!(loop_label.starts_with("#__loop_"));

				// Continue should have the same label
				// It's in the body statements after the if-break
				if let Stmt::Continue { label, .. } = &body.stmts[1] {
					assert_eq!(label, &Some(loop_label));
				} else {
					panic!("Expected continue statement");
				}
			}
			_ => panic!("Expected loop, got {:?}", output),
		}
	}

	#[test]
	fn test_labeled_loop_preserves_label()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Loop {
			label: Some("my_loop".into()),
			body: Block {
				stmts: vec![Stmt::Break {
					label: None,
					value: None,
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Loop { label, body, .. } => {
				// Should preserve the user-provided label
				assert_eq!(label, Some("my_loop".into()));

				// Break should get the same label
				if let Stmt::Break { label, .. } = &body.stmts[0] {
					assert_eq!(label, &Some("my_loop".into()));
				}
			}
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_break_with_explicit_label_unchanged()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Loop {
			label: Some("outer".into()),
			body: Block {
				stmts: vec![Stmt::Loop {
					label: None,
					body: Block {
						stmts: vec![Stmt::Break {
							label: Some("outer".into()),
							value: None,
							span: Span::default(),
						}],
						tail_expr: None,
						span: Span::default(),
					},
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Loop {
				label: outer_label,
				body: outer_body,
				..
			} => {
				assert_eq!(outer_label, Some("outer".into()));

				if let Stmt::Loop { body: inner_body, .. } = &outer_body.stmts[0] {
					// Break should still reference the outer loop
					if let Stmt::Break { label, .. } = &inner_body.stmts[0] {
						assert_eq!(label, &Some("outer".into()));
					}
				}
			}
			_ => panic!("Expected outer loop"),
		}
	}

	#[test]
	fn test_for_loop_gets_label()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::For {
			label: None,
			pattern: simple_for_pattern("x"),
			iter: ident("iter"),
			body: Block {
				stmts: vec![Stmt::Break {
					label: None,
					value: None,
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(input).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Block(block) => {
				// Second statement should be the desugared loop
				if let Stmt::Loop { label, .. } = &block.stmts[1] {
					assert!(label.is_some());
					let loop_label = label.as_ref().unwrap();
					assert!(loop_label.starts_with("#__loop_"));
				} else {
					panic!("Expected loop");
				}
			}
			_ => panic!("Expected block"),
		}
	}

	#[test]
	fn test_while_var_gets_label()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::WhileVarLoop {
			label: None,
			pattern: Pattern::Wildcard {
				span: Span::default(),
				ty: None,
			},
			expr: ident("x"),
			body: Block {
				stmts: vec![Stmt::Continue {
					label: None,
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Stmt::Loop { label, body, .. } => {
				assert!(label.is_some());
				let loop_label = label.unwrap();

				if let Stmt::Expr(Expr::Switch { arms, .. }) = &body.stmts[1]
					&& let SwitchBody::Block(match_block) = &arms[0].body
					&& let Some(Stmt::Continue { label, .. }) = match_block.stmts.first()
				{
					assert_eq!(label, &Some(loop_label));
				}
			}
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_loop_expr_gets_label()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Loop {
			label: None,
			body: Box::new(Block {
				stmts: vec![Stmt::Break {
					label: None,
					value: Some(int_lit(42)),
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		match output {
			Expr::Loop { label, body, .. } => {
				assert!(label.is_some());
				let loop_label = label.unwrap();

				if let Stmt::Break { label: break_label, .. } = &body.stmts[0] {
					assert_eq!(break_label, &Some(loop_label));
				}
			}
			_ => panic!("Expected loop expression"),
		}
	}

	#[test]
	fn test_multiple_functions_reset_loop_counter()
	{
		let mut desugarer = Desugarer::new();

		let func1 = FunctionDecl {
			signature: FunctionSignature {
				modifiers: vec![],
				name: Path::simple(vec!["func1".into()], Span::default()),
				generics: vec![],
				params: vec![],
				return_type: None,
				where_clause: vec![],
				call_type: CallType::Regular,
				heap_generics: Vec::new(),
				span: Span::default(),
			},
			body: Some(Block {
				stmts: vec![Stmt::Loop {
					label: None,
					body: Block {
						stmts: vec![],
						tail_expr: None,
						span: Span::default(),
					},
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let func2 = FunctionDecl {
			signature: FunctionSignature {
				modifiers: vec![],
				name: Path::simple(vec!["func2".into()], Span::default()),
				generics: vec![],
				params: vec![],
				return_type: None,
				where_clause: vec![],
				call_type: CallType::Regular,
				heap_generics: Vec::new(),
				span: Span::default(),
			},
			body: Some(Block {
				stmts: vec![Stmt::Loop {
					label: None,
					body: Block {
						stmts: vec![],
						tail_expr: None,
						span: Span::default(),
					},
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result1 = desugarer.desugar_function(func1).inspect_err(|e| eprintln!("{e}"));
		assert!(result1.is_ok());

		let result2 = desugarer.desugar_function(func2).inspect_err(|e| eprintln!("{e}"));
		assert!(result2.is_ok());

		// Just verify it doesn't panic - loop stack should be properly managed
	}

	#[cfg(test)]
	mod constructor_tests
	{
		use super::*;

		#[test]
		fn test_desugar_var_with_constructor_call()
		{
			let mut desugarer = Desugarer::new();

			let var = VariableDecl {
				pattern: Pattern::TypedIdentifier {
					path: Path::simple(vec!["x".to_string()], Span::default()),
					ty: Type {
						modifiers: vec![],
						core: Box::new(TypeCore::Base {
							path: Path::simple(vec!["Point".to_string()], Span::default()),
							generics: vec![],
						}),
						span: Span::default(),
					},
					call_constructor: Some(CallType::Regular),
					span: Span::default(),
				},
				init: None,
				comp_const: false,
				span: Span::default(),
			};

			let result = desugarer.desugar_variable_decl(var).inspect_err(|e| eprintln!("{e}"));
			assert!(result.is_ok());
			let output = result.unwrap();

			// Should have generated Point::new() as the initializer
			assert!(output.init.is_some());
			match output.init.unwrap() {
				Expr::Call { callee, args, .. } => {
					match *callee {
						Expr::Identifier { path, .. } => {
							assert_eq!(
								path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
								vec!["Point", "create"]
							);
						}
						_ => panic!("Expected identifier in callee"),
					}
					assert_eq!(args.len(), 0); // create() takes no arguments
				}
				_ => panic!("Expected call expression"),
			}

			// call_constructor should be set to false after processing
			match output.pattern {
				Pattern::TypedIdentifier { call_constructor, .. } => {
					assert!(call_constructor.is_none());
				}
				_ => panic!("Expected TypedIdentifier pattern"),
			}
		}

		#[test]
		fn test_desugar_var_with_constructor_and_existing_init()
		{
			let mut desugarer = Desugarer::new();

			// If there's already an initializer, call_constructor should be ignored
			let var = VariableDecl {
				pattern: Pattern::TypedIdentifier {
					path: Path::simple(vec!["x".to_string()], Span::default()),
					ty: simple_type("Point"),
					call_constructor: Some(CallType::Regular),
					span: Span::default(),
				},
				init: Some(Expr::Literal {
					value: Literal::Int(42),
					span: Span::default(),
				}),
				comp_const: false,
				span: Span::default(),
			};

			let result = desugarer.desugar_variable_decl(var).inspect_err(|e| eprintln!("{e}"));
			assert!(result.is_ok());
			let output = result.unwrap();

			// Should keep the existing initializer
			match output.init.unwrap() {
				Expr::Literal {
					value: Literal::Int(42),
					..
				} => (),
				_ => panic!("Expected original initializer to be preserved"),
			}
		}

		#[test]
		fn test_desugar_var_with_generic_type_constructor()
		{
			let mut desugarer = Desugarer::new();

			let var = VariableDecl {
				pattern: Pattern::TypedIdentifier {
					path: Path::simple(vec!["vec".to_string()], Span::default()),
					ty: Type {
						modifiers: vec![],
						core: Box::new(TypeCore::Base {
							path: Path::simple(vec!["Vec".to_string()], Span::default()),
							generics: vec![Type {
								modifiers: vec![],
								core: Box::new(TypeCore::Base {
									path: Path::simple(vec!["i32".to_string()], Span::default()),
									generics: vec![],
								}),
								span: Span::default(),
							}],
						}),
						span: Span::default(),
					},
					call_constructor: Some(CallType::Regular),
					span: Span::default(),
				},
				init: None,
				comp_const: false,
				span: Span::default(),
			};

			let result = desugarer.desugar_variable_decl(var).inspect_err(|e| eprintln!("{e}"));
			assert!(result.is_ok());
			let output = result.unwrap();

			// Should generate Vec::create()
			assert!(output.init.is_some());
			match output.init.unwrap() {
				Expr::Call { callee, .. } => match *callee {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["Vec", "create"]
						);
					}
					_ => panic!("Expected Vec::new identifier"),
				},
				_ => panic!("Expected call expression"),
			}
		}

		#[test]
		fn test_desugar_const_with_constructor()
		{
			let mut desugarer = Desugarer::new();

			let var = VariableDecl {
				pattern: Pattern::TypedIdentifier {
					path: Path::simple(vec!["CONFIG".to_string()], Span::default()),
					ty: simple_type("Config"),
					call_constructor: Some(CallType::Regular),
					span: Span::default(),
				},
				init: None,
				comp_const: true, // const instead of var
				span: Span::default(),
			};

			let result = desugarer.desugar_variable_decl(var).inspect_err(|e| eprintln!("{e}"));
			assert!(result.is_ok());
			let output = result.unwrap();

			// Should still generate Config::new()
			assert!(output.init.is_some());
			match output.init.unwrap() {
				Expr::Call { callee, .. } => match *callee {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["Config", "create"]
						);
					}
					_ => panic!("Expected Config::create identifier"),
				},
				_ => panic!("Expected call expression"),
			}
		}

		#[test]
		fn test_desugar_qualified_type_constructor()
		{
			let mut desugarer = Desugarer::new();

			let var = VariableDecl {
				pattern: Pattern::TypedIdentifier {
					path: Path::simple(vec!["cfg".to_string()], Span::default()),
					ty: Type {
						modifiers: vec![],
						core: Box::new(TypeCore::Base {
							path: Path::simple(
								vec!["std".to_string(), "config".to_string(), "Config".to_string()],
								Span::default(),
							),
							generics: vec![],
						}),
						span: Span::default(),
					},
					call_constructor: Some(CallType::Regular),
					span: Span::default(),
				},
				init: None,
				comp_const: false,
				span: Span::default(),
			};

			let result = desugarer.desugar_variable_decl(var).inspect_err(|e| eprintln!("{e}"));
			assert!(result.is_ok());
			let output = result.unwrap();

			// Should generate std::config::Config::new()
			match output.init.unwrap() {
				Expr::Call { callee, .. } => match *callee {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["std", "config", "Config", "create"]
						);
					}
					_ => panic!("Expected qualified path"),
				},
				_ => panic!("Expected call expression"),
			}
		}

		#[test]
		fn test_desugar_var_without_constructor_unchanged()
		{
			let mut desugarer = Desugarer::new();

			// Normal variable without constructor syntax
			let var = VariableDecl {
				pattern: Pattern::TypedIdentifier {
					path: Path::simple(vec!["x".to_string()], Span::default()),
					ty: simple_type("i32"),
					call_constructor: None,
					span: Span::default(),
				},
				init: Some(int_lit(42)),
				comp_const: false,
				span: Span::default(),
			};

			let result = desugarer.desugar_variable_decl(var).inspect_err(|e| eprintln!("{e}"));
			assert!(result.is_ok());
			let output = result.unwrap();

			// Should keep original initializer
			match output.init.unwrap() {
				Expr::Literal {
					value: Literal::Int(42),
					..
				} => (),
				_ => panic!("Expected original initializer"),
			}
		}

		#[test]
		fn test_desugar_multiple_vars_with_constructors()
		{
			let mut desugarer = Desugarer::new();

			let program = Program {
				items: vec![
					TopLevelDecl::VariableDecl(VariableDecl {
						pattern: Pattern::TypedIdentifier {
							path: Path::simple(vec!["a".to_string()], Span::default()),
							ty: simple_type("Point"),
							call_constructor: Some(CallType::Regular),
							span: Span::default(),
						},
						init: None,
						comp_const: false,
						span: Span::default(),
					}),
					TopLevelDecl::VariableDecl(VariableDecl {
						pattern: Pattern::TypedIdentifier {
							path: Path::simple(vec!["b".to_string()], Span::default()),
							ty: simple_type("Config"),
							call_constructor: Some(CallType::Regular),
							span: Span::default(),
						},
						init: None,
						comp_const: false,
						span: Span::default(),
					}),
				],
				span: Span::default(),
			};

			let result = desugarer.desugar_program(program).inspect_err(|e| eprintln!("{e}"));
			assert!(result.is_ok());
			let output = result.unwrap();

			// Both should have generated constructor calls
			assert_eq!(output.items.len(), 2);

			for item in &output.items {
				match item {
					TopLevelDecl::VariableDecl(var) => {
						assert!(var.init.is_some());
						match &var.init {
							Some(Expr::Call { .. }) => (),
							_ => panic!("Expected constructor call"),
						}
					}
					_ => panic!("Expected variable declaration"),
				}
			}
		}

		#[test]
		fn test_desugar_constructor_in_function()
		{
			let mut desugarer = Desugarer::new();

			let func = FunctionDecl {
				signature: FunctionSignature {
					modifiers: vec![],
					name: Path::simple(vec!["test".into()], Span::default()),
					generics: vec![],
					params: vec![],
					return_type: None,
					where_clause: vec![],
					call_type: CallType::Regular,
					heap_generics: Vec::new(),
					span: Span::default(),
				},
				body: Some(Block {
					stmts: vec![Stmt::VariableDecl(VariableDecl {
						pattern: Pattern::TypedIdentifier {
							path: Path::simple(vec!["local".to_string()], Span::default()),
							ty: simple_type("LocalType"),
							call_constructor: Some(CallType::Regular),
							span: Span::default(),
						},
						init: None,
						comp_const: false,
						span: Span::default(),
					})],
					tail_expr: None,
					span: Span::default(),
				}),
				span: Span::default(),
			};

			let result = desugarer.desugar_function(func).inspect_err(|e| eprintln!("{e}"));
			assert!(result.is_ok());
			let output = result.unwrap();

			// Check the variable inside the function
			let body = output.body.unwrap();
			match &body.stmts[0] {
				Stmt::VariableDecl(var) => {
					assert!(var.init.is_some());
					match &var.init {
						Some(Expr::Call { callee, .. }) => match callee.as_ref() {
							Expr::Identifier { path, .. } => {
								assert_eq!(
									path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
									vec!["LocalType", "create"]
								);
							}
							_ => panic!("Expected identifier callee"),
						},
						_ => panic!("Expected constructor call"),
					}
				}
				_ => panic!("Expected variable declaration"),
			}
		}

		#[test]
		fn test_constructor_call_preserves_type_info()
		{
			let mut desugarer = Desugarer::new();

			let original_ty = Type {
				modifiers: vec![],
				core: Box::new(TypeCore::Base {
					path: Path::simple(vec!["MyType".to_string()], Span::default()),
					generics: vec![Type {
						modifiers: vec![],
						core: Box::new(TypeCore::Base {
							path: Path::simple(vec!["String".to_string()], Span::default()),
							generics: vec![],
						}),
						span: Span::default(),
					}],
				}),
				span: Span::default(),
			};

			let var = VariableDecl {
				pattern: Pattern::TypedIdentifier {
					path: Path::simple(vec!["x".to_string()], Span::default()),
					ty: original_ty,
					call_constructor: Some(CallType::Regular),
					span: Span::default(),
				},
				init: None,
				comp_const: false,
				span: Span::default(),
			};

			let result = desugarer.desugar_variable_decl(var).inspect_err(|e| eprintln!("{e}"));
			assert!(result.is_ok());
			let output = result.unwrap();

			// Pattern should still have the type information
			match &output.pattern {
				Pattern::TypedIdentifier { ty, .. } => {
					// Type should be preserved
					match ty.core.as_ref() {
						TypeCore::Base { path, generics } => {
							assert_eq!(
								path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
								vec!["MyType"]
							);
							assert_eq!(generics.len(), 1);
						}
						_ => panic!("Expected base type"),
					}
				}
				_ => panic!("Expected TypedIdentifier pattern"),
			}
		}
	}

	#[test]
	fn test_desugar_range_full()
	{
		let mut desugarer = Desugarer::new();

		// Test a..b (exclusive end)
		let range = RangeExpr {
			start: Some(Box::new(int_lit(1))),
			end: Some(Box::new(int_lit(10))),
			inclusive: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_range(range).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// Should become Range::new(1, 10)
		match output {
			Expr::Call { callee, args, .. } => {
				match callee.as_ref() {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["Range", "new"]
						);
					}
					_ => panic!("Expected identifier callee"),
				}
				assert_eq!(args.len(), 2);
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_desugar_range_inclusive()
	{
		let mut desugarer = Desugarer::new();

		// Test a..=b (inclusive end)
		let range = RangeExpr {
			start: Some(Box::new(int_lit(1))),
			end: Some(Box::new(int_lit(10))),
			inclusive: true,
			span: Span::default(),
		};

		let result = desugarer.desugar_range(range).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// Should become RangeInclusive::new(1, 10)
		match output {
			Expr::Call { callee, args, .. } => {
				match callee.as_ref() {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["RangeInclusive", "new"]
						);
					}
					_ => panic!("Expected identifier callee"),
				}
				assert_eq!(args.len(), 2);
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_desugar_range_from()
	{
		let mut desugarer = Desugarer::new();

		// Test a.. (no end)
		let range = RangeExpr {
			start: Some(Box::new(int_lit(5))),
			end: None,
			inclusive: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_range(range).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// Should become RangeFrom::new(5)
		match output {
			Expr::Call { callee, args, .. } => {
				match callee.as_ref() {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["RangeFrom", "new"]
						);
					}
					_ => panic!("Expected identifier callee"),
				}
				assert_eq!(args.len(), 1);
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_desugar_range_to()
	{
		let mut desugarer = Desugarer::new();

		// Test ..b (no start, exclusive end)
		let range = RangeExpr {
			start: None,
			end: Some(Box::new(int_lit(10))),
			inclusive: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_range(range).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// Should become RangeTo::new(10)
		match output {
			Expr::Call { callee, args, .. } => {
				match callee.as_ref() {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["RangeTo", "new"]
						);
					}
					_ => panic!("Expected identifier callee"),
				}
				assert_eq!(args.len(), 1);
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_desugar_range_to_inclusive()
	{
		let mut desugarer = Desugarer::new();

		// Test ..=b (no start, inclusive end)
		let range = RangeExpr {
			start: None,
			end: Some(Box::new(int_lit(10))),
			inclusive: true,
			span: Span::default(),
		};

		let result = desugarer.desugar_range(range).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// Should become RangeToInclusive::new(10)
		match output {
			Expr::Call { callee, args, .. } => {
				match callee.as_ref() {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["RangeToInclusive", "new"]
						);
					}
					_ => panic!("Expected identifier callee"),
				}
				assert_eq!(args.len(), 1);
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_desugar_range_full_unbounded()
	{
		let mut desugarer = Desugarer::new();

		// Test .. (no start, no end)
		let range = RangeExpr {
			start: None,
			end: None,
			inclusive: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_range(range).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// Should become RangeFull::new()
		match output {
			Expr::Call { callee, args, .. } => {
				match callee.as_ref() {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["RangeFull", "new"]
						);
					}
					_ => panic!("Expected identifier callee"),
				}
				assert_eq!(args.len(), 0);
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_desugar_range_with_complex_expressions()
	{
		let mut desugarer = Desugarer::new();

		// Test (a + 1)..(b * 2)
		let range = RangeExpr {
			start: Some(Box::new(Expr::Binary {
				op: BinaryOp::Add,
				lhs: Box::new(ident("a")),
				rhs: Box::new(int_lit(1)),
				span: Span::default(),
			})),
			end: Some(Box::new(Expr::Binary {
				op: BinaryOp::Mul,
				lhs: Box::new(ident("b")),
				rhs: Box::new(int_lit(2)),
				span: Span::default(),
			})),
			inclusive: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_range(range).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// Should become Range::new(a + 1, b * 2)
		match output {
			Expr::Call { callee, args, .. } => {
				match callee.as_ref() {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["Range", "new"]
						);
					}
					_ => panic!("Expected identifier callee"),
				}
				assert_eq!(args.len(), 2);
				// Both args should be binary expressions
				assert!(matches!(args[0], Expr::Binary { .. }));
				assert!(matches!(args[1], Expr::Binary { .. }));
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_desugar_range_with_identifiers()
	{
		let mut desugarer = Desugarer::new();

		// Test start..end
		let range = RangeExpr {
			start: Some(Box::new(ident("start"))),
			end: Some(Box::new(ident("end"))),
			inclusive: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_range(range).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// Should become Range::new(start, end)
		match output {
			Expr::Call { callee, args, .. } => {
				match callee.as_ref() {
					Expr::Identifier { path, .. } => {
						assert_eq!(
							path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
							vec!["Range", "new"]
						);
					}
					_ => panic!("Expected identifier callee"),
				}
				assert_eq!(args.len(), 2);
				assert!(matches!(args[0], Expr::Identifier { .. }));
				assert!(matches!(args[1], Expr::Identifier { .. }));
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_desugar_range_in_for_loop()
	{
		let mut desugarer = Desugarer::new();

		// Test for i in 0..10 { }
		let stmt = Stmt::For {
			label: None,
			pattern: simple_for_pattern("i"),
			iter: Expr::Range(RangeExpr {
				start: Some(Box::new(int_lit(0))),
				end: Some(Box::new(int_lit(10))),
				inclusive: false,
				span: Span::default(),
			}),
			body: Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// The for loop should be desugared and the range should become Range::new(0, 10)
		match output {
			Stmt::Block(block) => {
				// First statement should be the iterator variable declaration
				if let Stmt::VariableDecl(var_decl) = &block.stmts[0] {
					// The init should be the desugared range
					match &var_decl.init {
						Some(Expr::Call { callee, args, .. }) => {
							match callee.as_ref() {
								Expr::Identifier { path, .. } => {
									assert_eq!(
										path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
										vec!["Range", "new"]
									);
								}
								_ => panic!("Expected Range::new identifier"),
							}
							assert_eq!(args.len(), 2);
						}
						_ => panic!("Expected Range::new call"),
					}
				} else {
					panic!("Expected variable declaration");
				}
			}
			_ => panic!("Expected block"),
		}
	}

	#[test]
	fn test_desugar_range_in_array_index()
	{
		let mut desugarer = Desugarer::new();

		// Test arr[0..5]
		let expr = Expr::Index {
			base: Box::new(ident("arr")),
			index: Box::new(Expr::Range(RangeExpr {
				start: Some(Box::new(int_lit(0))),
				end: Some(Box::new(int_lit(5))),
				inclusive: false,
				span: Span::default(),
			})),
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		// The range should be desugared to Range::new(0, 5)
		match output {
			Expr::Index { index, .. } => match index.as_ref() {
				Expr::Call { callee, args, .. } => {
					match callee.as_ref() {
						Expr::Identifier { path, .. } => {
							assert_eq!(
								path.segments.iter().map(|s| return s.name.as_str()).collect::<Vec<_>>(),
								vec!["Range", "new"]
							);
						}
						_ => panic!("Expected Range::new"),
					}
					assert_eq!(args.len(), 2);
				}
				_ => panic!("Expected call expression"),
			},
			_ => panic!("Expected index expression"),
		}
	}

	// Add this module to your existing tests file

	// Helper to create a simple generic parameter
	fn generic_param(name: &str, bounds: Vec<WhereBound>) -> GenericParam
	{
		return GenericParam {
			name: name.to_string(),
			bounds,
			span: Span::default(),
		};
	}

	// Helper to create a simple where bound (trait bound)
	fn trait_bound(trait_name: &str) -> WhereBound
	{
		return WhereBound::Path {
			path: Path::simple(vec![trait_name.to_string()], Span::default()),
			args: Vec::new(),
		};
	}

	// Helper to create a where constraint
	fn where_constraint(type_name: &str, bounds: Vec<WhereBound>, type_args: Vec<Type>) -> WhereConstraint
	{
		return WhereConstraint {
			ty: Path::simple(vec![type_name.to_string()], Span::default()),
			bounds,
			type_args,
			span: Span::default(),
		};
	}

	#[test]
	fn test_desugar_simple_generic_bound()
	{
		let desugarer = Desugarer::new();

		// fn foo<T: Clone>()
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![trait_bound("Clone")])],
			params: vec![],
			return_type: None,
			where_clause: vec![],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_ok(), "Failed to desugar: {:?}", result.err());
		let output = result.unwrap();

		// After desugaring:
		// 1. Generic parameter should have no bounds
		assert!(
			output.generics[0].bounds.is_empty(),
			"Generic should have no bounds after desugaring"
		);

		// 2. Where clause should contain T: Clone
		assert_eq!(output.where_clause.len(), 1, "Should have one where constraint");
		assert_eq!(output.where_clause[0].ty.segments.len(), 1);
		assert_eq!(output.where_clause[0].ty.segments[0].name, "T");
		assert_eq!(output.where_clause[0].bounds.len(), 1);
		assert!(output.where_clause[0].type_args.is_empty());
	}

	#[test]
	fn test_desugar_multiple_bounds()
	{
		let desugarer = Desugarer::new();

		// fn foo<T: Clone + Send + Debug>()
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param(
				"T",
				vec![trait_bound("Clone"), trait_bound("Send"), trait_bound("Debug")],
			)],
			params: vec![],
			return_type: None,
			where_clause: vec![],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_ok());
		let output = result.unwrap();

		// Generic should have no bounds
		assert!(output.generics[0].bounds.is_empty());

		// Where clause should have T: Clone + Send + Debug
		assert_eq!(output.where_clause.len(), 1);
		assert_eq!(output.where_clause[0].bounds.len(), 3);
	}

	#[test]
	fn test_desugar_multiple_generics()
	{
		let desugarer = Desugarer::new();

		// fn foo<T: Clone, U: Send, V>()
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![
				generic_param("T", vec![trait_bound("Clone")]),
				generic_param("U", vec![trait_bound("Send")]),
				generic_param("V", vec![]), // No bounds
			],
			params: vec![],
			return_type: None,
			where_clause: vec![],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_ok());
		let output = result.unwrap();

		// All generics should have no bounds
		assert!(output.generics[0].bounds.is_empty());
		assert!(output.generics[1].bounds.is_empty());
		assert!(output.generics[2].bounds.is_empty());

		// Where clause should have T: Clone and U: Send (but not V)
		assert_eq!(output.where_clause.len(), 2);

		assert_eq!(output.where_clause[0].ty.segments.len(), 1);
		assert_eq!(output.where_clause[0].ty.segments[0].name, "T");
		assert_eq!(output.where_clause[0].bounds.len(), 1);

		assert_eq!(output.where_clause[1].ty.segments.len(), 1);
		assert_eq!(output.where_clause[1].ty.segments[0].name, "U");
		assert_eq!(output.where_clause[1].bounds.len(), 1);
	}

	#[test]
	fn test_error_on_duplicate_simple_constraint()
	{
		let desugarer = Desugarer::new();

		// fn foo<T: Clone>() where T: Send
		// ERROR: T appears in both places
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![trait_bound("Clone")])],
			params: vec![],
			return_type: None,
			where_clause: vec![where_constraint("T", vec![trait_bound("Send")], vec![])],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_err(), "Should error on duplicate constraint");

		match result {
			Err(CompileError::DesugarError(e)) => {
				// Verify error message mentions the duplicate
				let error_str = format!("{}", e);
				assert!(error_str.contains("T"), "Error should mention type parameter T");
				assert!(error_str.contains("bounds"), "Error should mention bounds");
			}
			_ => panic!("Expected DesugarError"),
		}
	}

	#[test]
	fn test_error_on_complex_type_using_bounded_param()
	{
		let desugarer = Desugarer::new();

		// fn foo<T: Clone>() where Vec<T>: Send
		// ERROR: T appears in where clause
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![trait_bound("Clone")])],
			params: vec![],
			return_type: None,
			where_clause: vec![where_constraint(
				"Vec",
				vec![trait_bound("Send")],
				vec![simple_type("T")],
			)],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_err(), "Should error when T appears in Vec<T>");

		match result {
			Err(CompileError::DesugarError(e)) => {
				let error_str = format!("{}", e);
				assert!(error_str.contains("T"), "Error should mention T");
			}
			_ => panic!("Expected DesugarError"),
		}
	}

	#[test]
	fn test_ok_different_params_in_where()
	{
		let desugarer = Desugarer::new();

		// fn foo<T: Clone, U>() where Vec<U>: Send
		// OK: T has bounds, but where clause only mentions U
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![
				generic_param("T", vec![trait_bound("Clone")]),
				generic_param("U", vec![]),
			],
			params: vec![],
			return_type: None,
			where_clause: vec![where_constraint(
				"Vec",
				vec![trait_bound("Send")],
				vec![simple_type("U")],
			)],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_ok(), "Should be OK when different params used");

		let output = result.unwrap();

		// T's bounds should be moved to where clause
		assert!(output.generics[0].bounds.is_empty());
		assert!(output.generics[1].bounds.is_empty());

		// Should have two where constraints: T: Clone and Vec<U>: Send
		assert_eq!(output.where_clause.len(), 2);
	}

	#[test]
	fn test_no_bounds_no_where()
	{
		let desugarer = Desugarer::new();

		// fn foo<T, U>()
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![]), generic_param("U", vec![])],
			params: vec![],
			return_type: None,
			where_clause: vec![],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_ok());
		let output = result.unwrap();

		// No bounds should be moved
		assert!(output.where_clause.is_empty());
		assert!(output.generics[0].bounds.is_empty());
		assert!(output.generics[1].bounds.is_empty());
	}

	#[test]
	fn test_only_where_clause_no_generic_bounds()
	{
		let desugarer = Desugarer::new();

		// fn foo<T>() where Vec<T>: Clone
		// OK: T has no bounds in generic list
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![])],
			params: vec![],
			return_type: None,
			where_clause: vec![where_constraint(
				"Vec",
				vec![trait_bound("Clone")],
				vec![simple_type("T")],
			)],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_ok(), "Should be OK when T has no generic bounds");

		let output = result.unwrap();

		// Where clause should be unchanged
		assert_eq!(output.where_clause.len(), 1);
		assert_eq!(output.where_clause[0].ty.segments.len(), 1);
		assert_eq!(output.where_clause[0].ty.segments[0].name, "Vec");
	}

	#[test]
	fn test_heap_generics_bounds()
	{
		let desugarer = Desugarer::new();

		// fn!<A: Allocator> foo<T: Clone>()
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![trait_bound("Clone")])],
			params: vec![],
			return_type: None,
			where_clause: vec![],
			call_type: CallType::UserHeap,
			heap_generics: vec![generic_param("A", vec![trait_bound("Allocator")])],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_ok());
		let output = result.unwrap();

		// Both regular and heap generics should have no bounds
		assert!(output.generics[0].bounds.is_empty());
		assert!(output.heap_generics[0].bounds.is_empty());

		// Where clause should have both T: Clone and A: Allocator
		assert_eq!(output.where_clause.len(), 2);

		// Check that both constraints are present (order doesn't matter)
		let has_t_clone = output
			.where_clause
			.iter()
			.any(|w| return w.ty.segments.len() == 1 && w.ty.segments[0].name == "T" && w.bounds.len() == 1);
		let has_a_alloc = output
			.where_clause
			.iter()
			.any(|w| return w.ty.segments.len() == 1 && w.ty.segments[0].name == "A" && w.bounds.len() == 1);

		assert!(has_t_clone, "Should have T: Clone in where clause");
		assert!(has_a_alloc, "Should have A: Allocator in where clause");
	}

	#[test]
	fn test_error_heap_generic_in_where_clause()
	{
		let desugarer = Desugarer::new();

		// fn!<A: Allocator> foo<T>() where Vec<A>: Clone
		// ERROR: A has bounds in heap generics and appears in where clause
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![])],
			params: vec![],
			return_type: None,
			where_clause: vec![where_constraint(
				"Vec",
				vec![trait_bound("Clone")],
				vec![simple_type("A")],
			)],
			call_type: CallType::UserHeap,
			heap_generics: vec![generic_param("A", vec![trait_bound("Allocator")])],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(
			result.is_err(),
			"Should error when heap generic appears in where clause"
		);
	}

	#[test]
	fn test_nested_type_args()
	{
		let desugarer = Desugarer::new();

		// fn foo<T: Clone>() where HashMap<K, Vec<T>>: Debug
		// ERROR: T appears nested in where clause
		let vec_t = Type {
			modifiers: vec![],
			core: Box::new(TypeCore::Base {
				path: Path::simple(vec!["Vec".to_string()], Span::default()),
				generics: vec![simple_type("T")],
			}),
			span: Span::default(),
		};

		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![trait_bound("Clone")])],
			params: vec![],
			return_type: None,
			where_clause: vec![where_constraint(
				"HashMap",
				vec![trait_bound("Debug")],
				vec![simple_type("K"), vec_t],
			)],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_err(), "Should error when T appears nested in Vec<T>");
	}

	#[test]
	fn test_tuple_type_with_bounded_param()
	{
		let desugarer = Desugarer::new();

		// fn foo<T: Clone>() where (T, U): Debug
		// ERROR: T appears in tuple
		let tuple_type = Type {
			modifiers: vec![],
			core: Box::new(TypeCore::Tuple(vec![simple_type("T"), simple_type("U")])),
			span: Span::default(),
		};

		// Note: This test assumes your where_constraint helper can handle non-simple types
		// You might need to construct the WhereConstraint manually
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![trait_bound("Clone")])],
			params: vec![],
			return_type: None,
			where_clause: vec![WhereConstraint {
				ty: Path::simple(vec!["tuple".to_string()], Span::default()), // placeholder
				bounds: vec![trait_bound("Debug")],
				type_args: vec![tuple_type],
				span: Span::default(),
			}],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_err(), "Should error when T appears in tuple type");
	}

	#[test]
	fn test_desugar_impl_generics()
	{
		let mut desugarer = Desugarer::new();

		// impl<T: Clone> MyTrait for MyType<T> { }
		let impl_decl = ImplDecl {
			modifiers: vec![],
			generics: vec![generic_param("T", vec![trait_bound("Clone")])],
			target: ImplTarget {
				path: Path::simple(vec!["MyType".into()], Span::default()),
				generics: vec![],
				span: Span::default(),
			},
			trait_path: Some(ImplTarget {
				path: Path::simple(vec!["MyTrait".into()], Span::default()),
				generics: vec![],
				span: Span::default(),
			}),
			where_clause: vec![],
			body: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_impl(impl_decl);
		assert!(result.is_ok());
		let output = result.unwrap();

		// Generic should have no bounds
		println!("{:#?}", output);
		assert!(output.generics[0].bounds.is_empty());

		// Where clause should have T: Clone
		assert_eq!(output.where_clause.len(), 1);
		assert_eq!(output.where_clause[0].ty.segments.len(), 1);
		assert_eq!(output.where_clause[0].ty.segments[0].name, "T");
	}

	#[test]
	fn test_error_impl_generic_in_where()
	{
		let mut desugarer = Desugarer::new();

		// impl<T: Clone> MyType<T> where T: Send { }
		// ERROR: T has bounds in generic list and appears in where clause
		let impl_decl = ImplDecl {
			modifiers: vec![],
			generics: vec![generic_param("T", vec![trait_bound("Clone")])],
			target: ImplTarget {
				path: Path::simple(vec!["MyType".into()], Span::default()),
				generics: vec![],
				span: Span::default(),
			},
			trait_path: None,
			where_clause: vec![where_constraint("T", vec![trait_bound("Send")], vec![])],
			body: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_impl(impl_decl);
		assert!(result.is_err(), "Should error on duplicate constraint in impl");
	}

	#[test]
	fn test_reference_type_with_bounded_param()
	{
		let desugarer = Desugarer::new();

		// fn foo<T: Clone>() where &T: Debug
		// ERROR: T appears in reference type
		let ref_type = Type {
			modifiers: vec![],
			core: Box::new(TypeCore::Reference {
				mutable: false,
				inner: Box::new(TypeCore::Base {
					path: Path::simple(vec!["T".to_string()], Span::default()),
					generics: vec![],
				}),
			}),
			span: Span::default(),
		};

		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![trait_bound("Clone")])],
			params: vec![],
			return_type: None,
			where_clause: vec![WhereConstraint {
				ty: Path::simple(vec!["ref".to_string()], Span::default()),
				bounds: vec![trait_bound("Debug")],
				type_args: vec![ref_type],
				span: Span::default(),
			}],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_err(), "Should error when T appears in &T");
	}

	#[test]
	fn test_complete_function_desugaring()
	{
		let mut desugarer = Desugarer::new();

		// fn foo<T: Clone + Send>(x: T) -> T { x }
		let func = FunctionDecl {
			signature: FunctionSignature {
				modifiers: vec![],
				name: Path::simple(vec!["foo".into()], Span::default()),
				generics: vec![generic_param("T", vec![trait_bound("Clone"), trait_bound("Send")])],
				params: vec![Param {
					ty: simple_type("T"),
					pattern: Pattern::TypedIdentifier {
						path: Path::simple(vec!["x".into()], Span::default()),
						ty: simple_type("T"),
						call_constructor: None,
						span: Span::default(),
					},
					mutable: false,
					variadic: false,
					span: Span::default(),
				}],
				return_type: Some(simple_type("T")),
				where_clause: vec![],
				call_type: CallType::Regular,
				heap_generics: vec![],
				span: Span::default(),
			},
			body: Some(Block {
				stmts: vec![],
				tail_expr: Some(Box::new(ident("x"))),
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_function(func).inspect_err(|e| println!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

		assert!(output.signature.generics[0].bounds.is_empty());
		assert_eq!(output.signature.where_clause.len(), 1);
		assert_eq!(output.signature.where_clause[0].ty.segments.len(), 1);
		assert_eq!(output.signature.where_clause[0].ty.segments[0].name, "T");
		assert_eq!(output.signature.where_clause[0].bounds.len(), 2);
	}

	#[test]
	fn test_array_type_with_bounded_param()
	{
		let desugarer = Desugarer::new();

		// fn foo<T: Clone>() where [T; 10]: Debug
		// ERROR: T appears in array type
		let array_type = Type {
			modifiers: vec![],
			core: Box::new(TypeCore::Array {
				inner: Box::new(TypeCore::Base {
					path: Path::simple(vec!["T".to_string()], Span::default()),
					generics: vec![],
				}),
				size: Some(Box::new(int_lit(10))),
			}),
			span: Span::default(),
		};

		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![generic_param("T", vec![trait_bound("Clone")])],
			params: vec![],
			return_type: None,
			where_clause: vec![WhereConstraint {
				ty: Path::simple(vec!["array".to_string()], Span::default()),
				bounds: vec![trait_bound("Debug")],
				type_args: vec![array_type],
				span: Span::default(),
			}],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_err(), "Should error when T appears in [T; 10]");
	}
	// ========== Pattern Desugaring Tests ==========

	#[test]
	fn test_desugar_pattern_literal()
	{
		let mut desugarer = Desugarer::new();

		let pattern = Pattern::Literal {
			value: Literal::Int(42),
			span: Span::default(),
		};

		let result = desugarer.desugar_pattern(pattern);
		assert!(result.is_ok());

		match result.unwrap() {
			Pattern::Literal {
				value: Literal::Int(42),
				..
			} => (),
			_ => panic!("Expected literal pattern to be unchanged"),
		}
	}

	#[test]
	fn test_desugar_pattern_range()
	{
		let mut desugarer = Desugarer::new();

		let pattern = Pattern::Range(RangeExpr {
			start: Some(Box::new(int_lit(1))),
			end: Some(Box::new(int_lit(10))),
			inclusive: false,
			span: Span::default(),
		});

		let result = desugarer.desugar_pattern(pattern);
		assert!(result.is_ok());

		match result.unwrap() {
			Pattern::Range(_) => (),
			_ => panic!("Expected range pattern to be unchanged"),
		}
	}

	#[test]
	fn test_desugar_pattern_single_tuple_unwraps()
	{
		let mut desugarer = Desugarer::new();

		// Single element tuple should unwrap to just the element
		let pattern = Pattern::Tuple {
			patterns: vec![typed_ident_pattern("x", "i32")],
			span: Span::default(),
		};

		let result = desugarer.desugar_pattern(pattern);
		assert!(result.is_ok());

		match result.unwrap() {
			Pattern::TypedIdentifier { .. } => (),
			_ => panic!("Single element tuple should unwrap"),
		}
	}

	#[test]
	fn test_desugar_pattern_single_or_unwraps()
	{
		let mut desugarer = Desugarer::new();

		// Single pattern in Or should unwrap
		let pattern = Pattern::Or {
			patterns: vec![typed_ident_pattern("x", "i32")],
			span: Span::default(),
		};

		let result = desugarer.desugar_pattern(pattern);
		assert!(result.is_ok());

		match result.unwrap() {
			Pattern::TypedIdentifier { .. } => (),
			_ => panic!("Single Or pattern should unwrap"),
		}
	}

	#[test]
	fn test_expand_or_in_struct_pattern()
	{
		let mut desugarer = Desugarer::new();

		// struct Point { x: (A | B), y: i32 }
		let pattern = Pattern::Struct {
			path: Path::simple(vec!["Point".into()], Span::default()),
			fields: vec![
				(
					"x".into(),
					Pattern::Or {
						patterns: vec![
							Pattern::Variant {
								path: Path::simple(vec!["A".into()], Span::default()),
								args: vec![],
								span: Span::default(),
							},
							Pattern::Variant {
								path: Path::simple(vec!["B".into()], Span::default()),
								args: vec![],
								span: Span::default(),
							},
						],
						span: Span::default(),
					},
				),
				("y".into(), typed_ident_pattern("y_val", "i32")),
			],
			has_rest: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_pattern(pattern);
		assert!(result.is_ok());

		// Should expand to: Point { x: A, y } | Point { x: B, y }
		match result.unwrap() {
			Pattern::Or { patterns, .. } => {
				assert_eq!(patterns.len(), 2);
				for p in patterns {
					assert!(matches!(p, Pattern::Struct { .. }));
				}
			}
			_ => panic!("Expected Or pattern with expanded struct patterns"),
		}
	}

	// ========== Statement Desugaring Tests ==========

	#[test]
	fn test_desugar_delete_statement()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Delete {
			expr: ident("x"),
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt);
		assert!(result.is_ok());

		match result.unwrap() {
			Stmt::Delete { .. } => (),
			_ => panic!("Expected delete statement"),
		}
	}

	// ========== Assignment Desugaring Tests ==========

	#[test]
	fn test_desugar_struct_assignment()
	{
		let mut desugarer = Desugarer::new();

		// Point { x, y } = value
		let stmt = Stmt::Assignment {
			target: Expr::StructInit {
				path: Path::simple(vec!["Point".into()], Span::default()),
				fields: vec![("x".into(), ident("x")), ("y".into(), ident("y"))],
				base: None,
				has_rest: false,
				span: Span::default(),
			},
			op: AssignOp::Assign,
			value: ident("point"),
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt);
		assert!(result.is_ok());

		// Should create a temp variable and individual field assignments
		match result.unwrap() {
			Stmt::Block(block) => {
				assert!(block.stmts.len() > 1);
				assert!(matches!(block.stmts[0], Stmt::VariableDecl(_)));
			}
			_ => panic!("Expected block with temp and assignments"),
		}
	}

	#[test]
	fn test_desugar_tuple_assignment()
	{
		let mut desugarer = Desugarer::new();

		// (a, b) = value
		let stmt = Stmt::Assignment {
			target: Expr::Tuple {
				elements: vec![ident("a"), ident("b")],
				span: Span::default(),
			},
			op: AssignOp::Assign,
			value: ident("tuple"),
			span: Span::default(),
		};

		let result = desugarer.desugar_stmt(stmt);
		assert!(result.is_ok());

		match result.unwrap() {
			Stmt::Block(block) => {
				// Should have temp decl + individual assignments
				assert!(block.stmts.len() >= 2);
			}
			_ => panic!("Expected block"),
		}
	}

	// ========== Variable Declaration Tests ==========

	#[test]
	fn test_desugar_var_decl_struct_pattern()
	{
		let mut desugarer = Desugarer::new();

		let var = VariableDecl {
			pattern: Pattern::Struct {
				path: Path::simple(vec!["Point".into()], Span::default()),
				fields: vec![
					("x".into(), typed_ident_pattern("x_val", "i32")),
					("y".into(), typed_ident_pattern("y_val", "i32")),
				],
				has_rest: false,
				span: Span::default(),
			},
			init: Some(ident("point")),
			comp_const: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_variable_decl(var);
		assert!(result.is_ok());
	}

	#[test]
	fn test_desugar_var_decl_struct_pattern_with_rest()
	{
		let mut desugarer = Desugarer::new();

		let var = VariableDecl {
			pattern: Pattern::Struct {
				path: Path::simple(vec!["Point".into()], Span::default()),
				fields: vec![("x".into(), typed_ident_pattern("x_val", "i32"))],
				has_rest: true, // .. pattern
				span: Span::default(),
			},
			init: Some(ident("point")),
			comp_const: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_variable_decl(var);
		assert!(result.is_ok());
	}

	#[test]
	fn test_desugar_var_decl_wildcard_with_type()
	{
		let mut desugarer = Desugarer::new();

		let var = VariableDecl {
			pattern: Pattern::Wildcard {
				ty: Some(simple_type("i32")),
				span: Span::default(),
			},
			init: Some(int_lit(42)),
			comp_const: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_variable_decl(var);
		assert!(result.is_ok());
	}

	#[test]
	fn test_desugar_pattern_to_statements_variant_error()
	{
		let mut desugarer = Desugarer::new();

		let pattern = Pattern::Variant {
			path: Path::simple(vec!["Some".into()], Span::default()),
			args: vec![typed_ident_pattern("x", "i32")],
			span: Span::default(),
		};

		let result = desugarer.desugar_pattern_to_statements(pattern, ident("value"), Span::default(), false);

		assert!(result.is_err(), "Variant patterns in var bindings should error");
	}

	// ========== Function Parameter Tests ==========

	#[test]
	fn test_desugar_function_with_variadic_param()
	{
		let mut desugarer = Desugarer::new();

		let func = FunctionDecl {
			signature: FunctionSignature {
				modifiers: vec![],
				name: Path::simple(vec!["test".into()], Span::default()),
				generics: vec![],
				params: vec![Param {
					pattern: typed_ident_pattern("args", "Array"),
					ty: simple_type("Array"),
					mutable: false,
					variadic: true,
					span: Span::default(),
				}],
				return_type: None,
				where_clause: vec![],
				call_type: CallType::Regular,
				heap_generics: vec![],
				span: Span::default(),
			},
			body: Some(Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_function(func);
		assert!(result.is_ok());

		let output = result.unwrap();
		assert_eq!(output.signature.params.len(), 1);
		assert!(output.signature.params[0].variadic);
	}

	#[test]
	fn test_desugar_function_with_pattern_param()
	{
		let mut desugarer = Desugarer::new();

		// fn test((x, y): (i32, i32)) { }
		let func = FunctionDecl {
			signature: FunctionSignature {
				modifiers: vec![],
				name: Path::simple(vec!["test".into()], Span::default()),
				generics: vec![],
				params: vec![Param {
					pattern: Pattern::Tuple {
						patterns: vec![typed_ident_pattern("x", "i32"), typed_ident_pattern("y", "i32")],
						span: Span::default(),
					},
					ty: Type {
						modifiers: vec![],
						core: Box::new(TypeCore::Tuple(vec![simple_type("i32"), simple_type("i32")])),
						span: Span::default(),
					},
					mutable: false,
					variadic: false,
					span: Span::default(),
				}],
				return_type: None,
				where_clause: vec![],
				call_type: CallType::Regular,
				heap_generics: vec![],
				span: Span::default(),
			},
			body: Some(Block {
				stmts: vec![],
				tail_expr: None,
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let result = desugarer.desugar_function(func);
		assert!(result.is_ok());

		let output = result.unwrap();
		// Should create a temp param and destructure in body
		assert_eq!(output.signature.params.len(), 1);
		assert!(matches!(
			output.signature.params[0].pattern,
			Pattern::TypedIdentifier { .. }
		));

		// Body should have the destructuring
		if let Some(body) = output.body {
			assert!(body.stmts.len() > 0);
		}
	}

	// ========== Expression Tests ==========

	#[test]
	fn test_desugar_expr_default()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Default {
			heap_call: CallType::Regular,
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr);
		assert!(result.is_ok());

		match result.unwrap() {
			Expr::Default { .. } => (),
			_ => panic!("Default should remain unchanged"),
		}
	}

	#[test]
	fn test_desugar_struct_init_with_base()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::StructInit {
			path: Path::simple(vec!["Point".into()], Span::default()),
			fields: vec![("x".into(), int_lit(5))],
			base: Some(Box::new(ident("base_point"))),
			has_rest: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_expr(expr);
		assert!(result.is_ok());

		match result.unwrap() {
			Expr::StructInit { base, .. } => {
				assert!(base.is_some());
			}
			_ => panic!("Expected struct init"),
		}
	}

	// ========== TopLevel Declarations Tests ==========

	#[test]
	fn test_desugar_struct_unchanged()
	{
		let mut desugarer = Desugarer::new();

		let struct_decl = TopLevelDecl::Struct(crate::parser::StructDecl {
			modifiers: vec![],
			name: Path::simple(vec!["Point".into()], Span::default()),
			generics: vec![],
			fields: vec![],
			where_clause: vec![],
			span: Span::default(),
		});

		let result = desugarer.desugar_top_level_decl(struct_decl);
		assert!(result.is_ok());

		match result.unwrap() {
			TopLevelDecl::Struct(_) => (),
			_ => panic!("Struct should remain unchanged"),
		}
	}

	#[test]
	fn test_desugar_union_unchanged()
	{
		let mut desugarer = Desugarer::new();

		let union_decl = TopLevelDecl::Union(crate::parser::UnionDecl {
			modifiers: vec![],
			name: Path::simple(vec!["MyUnion".into()], Span::default()),
			generics: vec![],
			fields: vec![],
			where_clause: vec![],
			span: Span::default(),
		});

		let result = desugarer.desugar_top_level_decl(union_decl);
		assert!(result.is_ok());

		match result.unwrap() {
			TopLevelDecl::Union(_) => (),
			_ => panic!("Union should remain unchanged"),
		}
	}

	#[test]
	fn test_desugar_enum_unchanged()
	{
		let mut desugarer = Desugarer::new();

		let enum_decl = TopLevelDecl::Enum(crate::parser::EnumDecl {
			modifiers: vec![],
			name: Path::simple(vec!["MyEnum".into()], Span::default()),
			generics: vec![],
			variants: vec![],
			where_clause: vec![],
			span: Span::default(),
		});

		let result = desugarer.desugar_top_level_decl(enum_decl);
		assert!(result.is_ok());

		match result.unwrap() {
			TopLevelDecl::Enum(_) => (),
			_ => panic!("Enum should remain unchanged"),
		}
	}

	#[test]
	fn test_desugar_variant_unchanged()
	{
		let mut desugarer = Desugarer::new();

		let variant_decl = TopLevelDecl::Variant(crate::parser::VariantDecl {
			modifiers: vec![],
			name: Path::simple(vec!["MyVariant".into()], Span::default()),
			generics: vec![],
			variants: vec![],
			where_clause: vec![],
			span: Span::default(),
		});

		let result = desugarer.desugar_top_level_decl(variant_decl);
		assert!(result.is_ok());

		match result.unwrap() {
			TopLevelDecl::Variant(_) => (),
			_ => panic!("Variant should remain unchanged"),
		}
	}

	#[test]
	fn test_desugar_type_alias_unchanged()
	{
		let mut desugarer = Desugarer::new();

		let alias = TopLevelDecl::TypeAlias(crate::parser::TypeAliasDecl {
			modifiers: vec![],
			name: Path::simple(vec!["MyType".into()], Span::default()),
			generics: vec![],
			ty: simple_type("i32"),
			span: Span::default(),
		});

		let result = desugarer.desugar_top_level_decl(alias);
		assert!(result.is_ok());

		match result.unwrap() {
			TopLevelDecl::TypeAlias(_) => (),
			_ => panic!("TypeAlias should remain unchanged"),
		}
	}

	// ========== Impl Tests ==========

	#[test]
	fn test_desugar_impl_type_alias()
	{
		let mut desugarer = Desugarer::new();

		let impl_decl = ImplDecl {
			modifiers: vec![],
			generics: vec![],
			target: ImplTarget {
				path: Path::simple(vec!["MyType".into()], Span::default()),
				generics: vec![],
				span: Span::default(),
			},
			trait_path: None,
			where_clause: vec![],
			body: vec![ImplItem::TypeAlias(crate::parser::TypeAliasDecl {
				modifiers: vec![],
				name: Path::simple(vec!["AssocType".into()], Span::default()),
				generics: vec![],
				ty: simple_type("i32"),
				span: Span::default(),
			})],
			span: Span::default(),
		};

		let result = desugarer.desugar_impl(impl_decl);
		assert!(result.is_ok());
	}

	#[test]
	fn test_desugar_impl_const()
	{
		let mut desugarer = Desugarer::new();

		let impl_decl = ImplDecl {
			modifiers: vec![],
			generics: vec![],
			target: ImplTarget {
				path: Path::simple(vec!["MyType".into()], Span::default()),
				generics: vec![],
				span: Span::default(),
			},
			trait_path: None,
			where_clause: vec![],
			body: vec![ImplItem::Const(VariableDecl {
				pattern: typed_ident_pattern("CONST", "i32"),
				init: Some(int_lit(42)),
				comp_const: true,
				span: Span::default(),
			})],
			span: Span::default(),
		};

		let result = desugarer.desugar_impl(impl_decl);
		assert!(result.is_ok());
	}

	// ========== Trait Tests ==========

	#[test]
	fn test_desugar_trait_type_alias()
	{
		let mut desugarer = Desugarer::new();

		let trait_decl = TraitDecl {
			modifiers: vec![],
			name: Path::simple(vec!["MyTrait".into()], Span::default()),
			generics: vec![],
			super_traits: vec![],
			items: vec![TraitItem::TypeAlias(crate::parser::TypeAliasDecl {
				modifiers: vec![],
				name: Path::simple(vec!["AssocType".into()], Span::default()),
				generics: vec![],
				ty: simple_type("i32"),
				span: Span::default(),
			})],
			span: Span::default(),
		};

		let result = desugarer.desugar_trait(trait_decl);
		assert!(result.is_ok());
	}

	#[test]
	fn test_desugar_trait_const()
	{
		let mut desugarer = Desugarer::new();

		let trait_decl = TraitDecl {
			modifiers: vec![],
			name: Path::simple(vec!["MyTrait".into()], Span::default()),
			generics: vec![],
			super_traits: vec![],
			items: vec![TraitItem::Const(VariableDecl {
				pattern: typed_ident_pattern("CONST", "i32"),
				init: Some(int_lit(42)),
				comp_const: true,
				span: Span::default(),
			})],
			span: Span::default(),
		};

		let result = desugarer.desugar_trait(trait_decl);
		assert!(result.is_ok());
	}

	// ========== Type Constructor Error Tests ==========

	#[test]
	fn test_type_to_constructor_call_non_base_type_error()
	{
		let desugarer = Desugarer::new();

		let tuple_type = Type {
			modifiers: vec![],
			core: Box::new(TypeCore::Tuple(vec![simple_type("i32")])),
			span: Span::default(),
		};

		let result = desugarer.type_to_constructor_call(&tuple_type, CallType::Regular);
		assert!(result.is_err(), "Should error on non-base type constructor");
	}

	// ========== Where Clause Tests ==========

	#[test]
	fn test_where_clause_with_func_bound()
	{
		let desugarer = Desugarer::new();

		// fn foo<F>() where F: Fn(i32) -> i32
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![crate::parser::GenericParam {
				name: "F".to_string(),
				bounds: vec![],
				span: Span::default(),
			}],
			params: vec![],
			return_type: None,
			where_clause: vec![WhereConstraint {
				ty: Path::simple(vec!["F".to_string()], Span::default()),
				bounds: vec![WhereBound::Func(FuncBound::Fn {
					args: vec![simple_type("i32")],
					ret: Some(simple_type("i32")),
				})],
				type_args: vec![],
				span: Span::default(),
			}],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_ok());
	}

	#[test]
	fn test_where_clause_with_generic_args()
	{
		let desugarer = Desugarer::new();

		// fn foo<T>() where Vec<T>: Clone
		let sig = FunctionSignature {
			modifiers: vec![],
			name: Path::simple(vec!["foo".into()], Span::default()),
			generics: vec![crate::parser::GenericParam {
				name: "T".to_string(),
				bounds: vec![],
				span: Span::default(),
			}],
			params: vec![],
			return_type: None,
			where_clause: vec![WhereConstraint {
				ty: Path::simple(vec!["Vec".to_string()], Span::default()),
				bounds: vec![WhereBound::Path {
					path: Path::simple(vec!["Clone".to_string()], Span::default()),
					args: vec![GenericArg::Type(simple_type("i32"))],
				}],
				type_args: vec![simple_type("T")],
				span: Span::default(),
			}],
			call_type: CallType::Regular,
			heap_generics: vec![],
			span: Span::default(),
		};

		let result = desugarer.desugar_function_signature(sig);
		assert!(result.is_ok());
	}

	// ========== Helper Function Tests ==========

	#[test]
	fn test_has_nested_patterns_false()
	{
		let patterns = vec![
			typed_ident_pattern("x", "i32"),
			Pattern::Wildcard {
				ty: None,
				span: Span::default(),
			},
		];

		assert!(!Desugarer::has_nested_patterns(&patterns));
	}

	#[test]
	fn test_has_nested_patterns_true()
	{
		let patterns = vec![
			typed_ident_pattern("x", "i32"),
			Pattern::Tuple {
				patterns: vec![typed_ident_pattern("y", "i32")],
				span: Span::default(),
			},
		];

		assert!(Desugarer::has_nested_patterns(&patterns));
	}

	// ========== Cartesian Product Tests ==========

	#[test]
	fn test_cartesian_product_patterns_empty()
	{
		let desugarer = Desugarer::new();
		let result = desugarer.cartesian_product_patterns(vec![]);
		assert_eq!(result.len(), 1);
		assert_eq!(result[0].len(), 0);
	}

	#[test]
	fn test_cartesian_product_patterns_single()
	{
		let desugarer = Desugarer::new();
		let lists = vec![vec![typed_ident_pattern("x", "i32"), typed_ident_pattern("y", "i32")]];

		let result = desugarer.cartesian_product_patterns(lists);
		assert_eq!(result.len(), 2);
		assert_eq!(result[0].len(), 1);
		assert_eq!(result[1].len(), 1);
	}

	#[test]
	fn test_cartesian_product_patterns_multiple()
	{
		let desugarer = Desugarer::new();
		let lists = vec![
			vec![typed_ident_pattern("a", "i32"), typed_ident_pattern("b", "i32")],
			vec![typed_ident_pattern("x", "i32"), typed_ident_pattern("y", "i32")],
		];

		let result = desugarer.cartesian_product_patterns(lists);
		// 2 x 2 = 4 combinations
		assert_eq!(result.len(), 4);
		for combo in result {
			assert_eq!(combo.len(), 2);
		}
	}

	// ========== Edge Cases ==========

	#[test]
	fn test_desugar_nested_tuple_in_struct()
	{
		let mut desugarer = Desugarer::new();

		let var = VariableDecl {
			pattern: Pattern::Struct {
				path: Path::simple(vec!["Point".into()], Span::default()),
				fields: vec![(
					"coords".into(),
					Pattern::Tuple {
						patterns: vec![typed_ident_pattern("x", "i32"), typed_ident_pattern("y", "i32")],
						span: Span::default(),
					},
				)],
				has_rest: false,
				span: Span::default(),
			},
			init: Some(ident("point")),
			comp_const: false,
			span: Span::default(),
		};

		let result = desugarer.desugar_variable_decl(var);
		assert!(result.is_ok());
	}

	#[test]
	fn test_loop_stack_management()
	{
		let mut desugarer = Desugarer::new();

		// Push and pop loops
		let label1 = desugarer.push_loop(Some("outer".into()));
		assert_eq!(label1, "outer");
		assert_eq!(desugarer.current_loop(), Some(&"outer".to_string()));

		let label2 = desugarer.push_loop(None);
		assert!(label2.starts_with("#__loop_"));
		assert_eq!(desugarer.current_loop(), Some(&label2));

		desugarer.pop_loop();
		assert_eq!(desugarer.current_loop(), Some(&"outer".to_string()));

		desugarer.pop_loop();
		assert_eq!(desugarer.current_loop(), None);
	}

	#[test]
	fn test_desugar_error_display()
	{
		let error = DesugarError::generic(Span::default(), "test error", crate::source_map::SourceIndex::default());

		let display = format!("{}", error);
		assert!(display.contains("test error"));
	}

	#[test]
	fn test_desugar_error_with_context()
	{
		let error = DesugarError::generic(Span::default(), "test error", crate::source_map::SourceIndex::default())
			.with_context("while desugaring function");

		let display = format!("{}", error);
		assert!(display.contains("while desugaring function"));
	}
}
