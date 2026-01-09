#[cfg(test)]
#[allow(clippy::module_inception)]
mod tests
{
	use crate::desuger::*;
	use crate::parser::{AssignOp, BinaryOp, FunctionSignature, ImplTarget, Literal, Path, UnaryOp};

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
			call_constructor: false,
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
			pattern: Pattern::Wildcard { span: Span::default() },
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
						args: vec![Pattern::Wildcard { span: Span::default() }],
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
			name: Path::simple(vec!["x".into()], Span::default()),
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
					pattern: Pattern::Wildcard { span: Span::default() },
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
			name: Path::simple(vec!["x".into(), "y".into()], Span::default()),
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
						args: vec![Pattern::Wildcard { span: Span::default() }],
						span: Span::default(),
					},
					Pattern::Or {
						patterns: vec![
							Pattern::Wildcard { span: Span::default() },
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

		let result = desugarer.desugar_switch_arm(arm).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let out = result.unwrap();

		if let Pattern::Tuple { patterns, .. } = out.pattern {
			assert_eq!(patterns.len(), 2);
		} else {
			panic!("Expected Pattern::Tuple");
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
						heap_func: false,
						span: Span::default(),
					},
					body: Some(Block {
						stmts: vec![Stmt::For {
							label: None,
							name: Path::simple(vec!["i".into()], Span::default()),
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
					heap_func: false,
					span: Span::default(),
				},
				body: Some(Block {
					stmts: vec![Stmt::WhileVarLoop {
						label: None,
						pattern: Pattern::Wildcard { span: Span::default() },
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
					heap_func: false,
					span: Span::default(),
				},
				body: Some(Block {
					stmts: vec![Stmt::For {
						label: None,
						name: Path::simple(vec!["i".into()], Span::default()),
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
					name: "arr".into(),
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
					name: Path::simple(vec!["x".into()], Span::default()),
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
					pattern: Pattern::Wildcard { span: Span::default() },
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
				name: Path::simple(vec!["i".into()], Span::default()),
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
			span: Span::default(),
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
				pattern: Pattern::Wildcard { span: Span::default() },
				body: SwitchBody::Block(Block {
					stmts: vec![Stmt::For {
						label: None,
						name: Path::simple(vec!["i".into()], Span::default()),
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
				name: "inner".into(),
				span: Span::default(),
			}),
			name: "field".into(),
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
					Pattern::Wildcard { span: Span::default() },
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
					args: vec![Pattern::Wildcard { span: Span::default() }],
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
					name: Path::simple(vec!["i".into()], Span::default()),
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
					name: Path::simple(vec!["j".into()], Span::default()),
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
					Pattern::Wildcard { span: Span::default() },
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
						Pattern::Wildcard { span: Span::default() },
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
			name: Path::simple(vec!["x".into()], Span::default()),
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
				name: Path::simple(vec!["i".into()], Span::default()),
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
			name: Path::simple(vec!["x".into()], Span::default()),
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
			pattern: Pattern::Wildcard { span: Span::default() },
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
				heap_func: false,
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
				heap_func: false,
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
					call_constructor: true,
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
							assert_eq!(path.segments, vec!["Point", "create"]);
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
					assert!(!call_constructor);
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
					call_constructor: true,
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
					call_constructor: true,
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
						assert_eq!(path.segments, vec!["Vec", "create"]);
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
					call_constructor: true,
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
						assert_eq!(path.segments, vec!["Config", "create"]);
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
					call_constructor: true,
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
						assert_eq!(path.segments, vec!["std", "config", "Config", "create"]);
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
					call_constructor: false,
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
							call_constructor: true,
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
							call_constructor: true,
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
					heap_func: false,
					span: Span::default(),
				},
				body: Some(Block {
					stmts: vec![Stmt::VariableDecl(VariableDecl {
						pattern: Pattern::TypedIdentifier {
							path: Path::simple(vec!["local".to_string()], Span::default()),
							ty: simple_type("LocalType"),
							call_constructor: true,
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
								assert_eq!(path.segments, vec!["LocalType", "create"]);
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
					call_constructor: true,
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
							assert_eq!(path.segments, vec!["MyType"]);
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
						assert_eq!(path.segments, vec!["Range", "new"]);
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
						assert_eq!(path.segments, vec!["RangeInclusive", "new"]);
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
						assert_eq!(path.segments, vec!["RangeFrom", "new"]);
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
						assert_eq!(path.segments, vec!["RangeTo", "new"]);
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
						assert_eq!(path.segments, vec!["RangeToInclusive", "new"]);
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
						assert_eq!(path.segments, vec!["RangeFull", "new"]);
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
						assert_eq!(path.segments, vec!["Range", "new"]);
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
						assert_eq!(path.segments, vec!["Range", "new"]);
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
			name: Path::simple(vec!["i".into()], Span::default()),
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
									assert_eq!(path.segments, vec!["Range", "new"]);
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
							assert_eq!(path.segments, vec!["Range", "new"]);
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
}
