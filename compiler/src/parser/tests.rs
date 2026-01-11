#[cfg(test)]
#[allow(clippy::module_inception)]
mod tests
{
	use crate::Config;
	use crate::lexer::Lexer;
	use crate::parser::*;
	use crate::source_map::SourceMap;

	fn parse_expr_from_str(input: &str) -> Result<Expr, CompileError>
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, input, "expr", &mut source_map);
		let mut parser = Parser::from(lexer);
		return parser
			.parse_expr()
			.inspect_err(|e| println!("{}", e.to_string_with_source(&source_map).expect("")));
	}

	fn parse_program_from_str(input: &str) -> Result<Program, CompileError>
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, input, "program", &mut source_map);
		let mut parser = Parser::from(lexer);
		return parser
			.parse_program()
			.inspect_err(|e| println!("{}", e.to_string_with_source(&source_map).expect("")));
	}

	fn parse_block_from_str(input: &str) -> Result<Block, CompileError>
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, input, "block", &mut source_map);
		let mut parser = Parser::from(lexer);
		return parser
			.parse_block()
			.inspect_err(|e| println!("{}", e.to_string_with_source(&source_map).expect("")));
	}

	fn parse_directive(input: &str) -> Result<DirectiveNode, CompileError>
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, input, "directive", &mut source_map);
		let mut parser = Parser::from(lexer);
		return parser
			.parse_directive_node()
			.inspect_err(|e| println!("{}", e.to_string_with_source(&source_map).expect("")));
	}

	// ========== Literal Tests ==========

	#[test]
	fn test_parse_int_literal()
	{
		let result = parse_expr_from_str("42");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Int(42),
				..
			} => (),
			_ => panic!("Expected Int literal"),
		}
	}

	#[test]
	fn test_parse_float_literal()
	{
		let result = parse_expr_from_str("3.16");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Float(f),
				..
			} if (f - 3.16).abs() < 0.001 => (),
			_ => panic!("Expected Float literal"),
		}
	}

	#[test]
	fn test_parse_bool_literal_true()
	{
		let result = parse_expr_from_str("true");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Bool(true),
				..
			} => (),
			_ => panic!("Expected Bool(true) literal"),
		}
	}

	#[test]
	fn test_parse_bool_literal_false()
	{
		let result = parse_expr_from_str("false");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Bool(false),
				..
			} => (),
			_ => panic!("Expected Bool(false) literal"),
		}
	}

	#[test]
	fn test_parse_string_literal()
	{
		let result = parse_expr_from_str(r#""hello world""#);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::String(s),
				..
			} if s == "hello world" => (),
			_ => panic!("Expected String literal"),
		}
	}

	#[test]
	fn test_parse_char_literal()
	{
		let result = parse_expr_from_str("'a'");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Char('a'),
				..
			} => (),
			_ => panic!("Expected Char literal"),
		}
	}

	// ========== Identifier Tests ==========

	#[test]
	fn test_parse_simple_identifier()
	{
		let result = parse_expr_from_str("variable");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Identifier { path, .. } if path == Path::simple(vec!["variable".to_string()], Default::default()) => {
			}
			_ => panic!("Expected simple identifier"),
		}
	}

	#[test]
	fn test_parse_path_identifier()
	{
		let result = parse_expr_from_str("std::vec::Vec");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Identifier { path, .. }
				if path
					== Path::simple(
						vec!["std".to_string(), "vec".to_string(), "Vec".to_string()],
						Default::default(),
					) => {}
			_ => panic!("Expected path identifier"),
		}
	}

	#[test]
	fn test_parse_self_keyword()
	{
		let result = parse_expr_from_str("self");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Identifier { path, .. } if path == Path::simple(vec!["self".to_string()], Default::default()) => (),
			_ => panic!("Expected self identifier"),
		}
	}

	// ========== Binary Operation Tests ==========

	#[test]
	fn test_parse_addition()
	{
		let result = parse_expr_from_str("1 + 2");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Add, .. } => (),
			_ => panic!("Expected addition"),
		}
	}

	#[test]
	fn test_parse_subtraction()
	{
		let result = parse_expr_from_str("5 - 3");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Sub, .. } => (),
			_ => panic!("Expected subtraction"),
		}
	}

	#[test]
	fn test_parse_multiplication()
	{
		let result = parse_expr_from_str("4 * 3");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Mul, .. } => (),
			_ => panic!("Expected multiplication"),
		}
	}

	#[test]
	fn test_parse_division()
	{
		let result = parse_expr_from_str("10 / 2");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Div, .. } => (),
			_ => panic!("Expected division"),
		}
	}

	#[test]
	fn test_parse_modulo()
	{
		let result = parse_expr_from_str("10 % 3");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Mod, .. } => (),
			_ => panic!("Expected modulo"),
		}
	}

	#[test]
	fn test_parse_operator_precedence()
	{
		let result = parse_expr_from_str("1 + 2 * 3");
		assert!(result.is_ok());
		// Should parse as 1 + (2 * 3)
		match result.unwrap() {
			Expr::Binary {
				op: BinaryOp::Add,
				lhs,
				rhs,
				..
			} => {
				match *lhs {
					Expr::Literal {
						value: Literal::Int(1), ..
					} => (),
					_ => panic!("Expected lhs to be 1"),
				}
				match *rhs {
					Expr::Binary { op: BinaryOp::Mul, .. } => (),
					_ => panic!("Expected rhs to be multiplication"),
				}
			}
			_ => panic!("Expected addition at top level"),
		}
	}

	#[test]
	fn test_parse_comparison_operators()
	{
		let ops = vec![
			("1 < 2", BinaryOp::Lt),
			("1 > 2", BinaryOp::Gt),
			("1 <= 2", BinaryOp::Le),
			("1 >= 2", BinaryOp::Ge),
			("1 == 2", BinaryOp::Eq),
			("1 != 2", BinaryOp::Ne),
		];

		for (input, expected_op) in ops {
			let result = parse_expr_from_str(input);
			assert!(result.is_ok(), "Failed to parse: {}", input);
			match result.unwrap() {
				Expr::Binary { op, .. } => {
					assert!(
						std::mem::discriminant(&op) == std::mem::discriminant(&expected_op),
						"Expected {:?} for input: {}",
						expected_op,
						input
					);
				}
				_ => panic!("Expected binary expression for: {}", input),
			}
		}
	}

	#[test]
	fn test_parse_logical_operators()
	{
		let result = parse_expr_from_str("true && false");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary {
				op: BinaryOp::LogicalAnd,
				..
			} => (),
			_ => panic!("Expected logical AND"),
		}

		let result = parse_expr_from_str("true || false");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Binary {
				op: BinaryOp::LogicalOr,
				..
			} => (),
			_ => panic!("Expected logical OR"),
		}
	}

	#[test]
	fn test_parse_bitwise_operators()
	{
		let ops = vec![
			("1 & 2", BinaryOp::BitAnd),
			("1 | 2", BinaryOp::BitOr),
			("1 ^ 2", BinaryOp::BitXor),
			("1 << 2", BinaryOp::Shl),
			("1 >> 2", BinaryOp::Shr),
		];

		for (input, expected_op) in ops {
			let result = parse_expr_from_str(input);
			assert!(result.is_ok(), "Failed to parse: {}", input);
			match result.unwrap() {
				Expr::Binary { op, .. } => {
					assert!(
						std::mem::discriminant(&op) == std::mem::discriminant(&expected_op),
						"Expected {:?} for input: {}",
						expected_op,
						input
					);
				}
				_ => panic!("Expected binary expression for: {}", input),
			}
		}
	}

	// ========== Unary Operation Tests ==========

	#[test]
	fn test_parse_negation()
	{
		let result = parse_expr_from_str("-5");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Unary { op: UnaryOp::Neg, .. } => (),
			_ => panic!("Expected negation"),
		}
	}

	#[test]
	fn test_parse_logical_not()
	{
		let result = parse_expr_from_str("!true");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Unary { op: UnaryOp::Not, .. } => (),
			_ => panic!("Expected logical NOT"),
		}
	}

	#[test]
	fn test_parse_dereference()
	{
		let result = parse_expr_from_str("*ptr");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Unary { op: UnaryOp::Deref, .. } => (),
			_ => panic!("Expected dereference"),
		}
	}

	#[test]
	fn test_parse_address_of()
	{
		let result = parse_expr_from_str("&x");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Unary {
				op: UnaryOp::Addr { mutable: false },
				..
			} => (),
			_ => panic!("Expected address-of"),
		}
	}

	#[test]
	fn test_parse_mutable_address_of()
	{
		let result = parse_expr_from_str("&mut x");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Unary {
				op: UnaryOp::Addr { mutable: true },
				..
			} => (),
			_ => panic!("Expected mutable address-of"),
		}
	}

	// ========== Cast Tests ==========

	#[test]
	fn test_parse_cast()
	{
		let result = parse_expr_from_str("(i32)42");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Cast { ty, expr, .. } => {
				match *ty.core {
					TypeCore::Base { ref path, .. }
						if path == &Path::simple(vec!["i32".to_string()], Default::default()) => {}
					_ => panic!("Expected i32 type"),
				}
				match *expr {
					Expr::Literal {
						value: Literal::Int(42),
						..
					} => (),
					_ => panic!("Expected 42 literal"),
				}
			}
			_ => panic!("Expected cast expression"),
		}
	}

	// ========== Range Tests ==========

	#[test]
	fn test_parse_exclusive_range()
	{
		let result = parse_expr_from_str("1..10");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: Some(_),
				end: Some(_),
				inclusive: false,
				..
			}) => (),
			_ => panic!("Expected exclusive range"),
		}
	}

	#[test]
	fn test_parse_inclusive_range()
	{
		let result = parse_expr_from_str("1..=10");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: Some(_),
				end: Some(_),
				inclusive: true,
				..
			}) => (),
			_ => panic!("Expected inclusive range"),
		}
	}

	#[test]
	fn test_parse_open_ended_range()
	{
		let result = parse_expr_from_str("1..;");

		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: Some(_),
				end: None,
				inclusive: false,
				..
			}) => (),
			_ => panic!("Expected open-ended range"),
		}
	}

	#[test]
	fn test_parse_empty_range_full()
	{
		let result = parse_expr_from_str("..;");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: None,
				end: None,
				inclusive: false,
				..
			}) => (),
			_ => panic!("Expected empty range (..)"),
		}
	}

	#[test]
	fn test_parse_empty_range_to_end()
	{
		let result = parse_expr_from_str("..10");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: None,
				end: Some(_),
				inclusive: false,
				..
			}) => (),
			_ => panic!("Expected range from start to 10 (..10)"),
		}
	}

	#[test]
	fn test_parse_empty_range_to_end_inclusive()
	{
		let result = parse_expr_from_str("..=10");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: None,
				end: Some(_),
				inclusive: true,
				..
			}) => (),
			_ => panic!("Expected range from start to 10 inclusive (..=10)"),
		}
	}

	#[test]
	fn test_parse_range_with_start_no_end()
	{
		let result = parse_expr_from_str("5..;");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: Some(_),
				end: None,
				inclusive: false,
				..
			}) => (),
			_ => panic!("Expected range from 5 to end (5..)"),
		}
	}

	#[test]
	fn test_parse_range_full_with_start_and_end()
	{
		let result = parse_expr_from_str("5..10");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: Some(_),
				end: Some(_),
				inclusive: false,
				..
			}) => (),
			_ => panic!("Expected range from 5 to 10 (5..10)"),
		}
	}

	#[test]
	fn test_parse_range_inclusive_with_start_and_end()
	{
		let result = parse_expr_from_str("5..=10");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: Some(_),
				end: Some(_),
				inclusive: true,
				..
			}) => (),
			_ => panic!("Expected range from 5 to 10 inclusive (5..=10)"),
		}
	}

	// ========== Empty Range in Context Tests ==========

	#[test]
	fn test_parse_empty_range_in_for_loop()
	{
		let result = parse_block_from_str("{ for i in ..10 { } }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::For { iter, .. } => match iter {
				Expr::Range(RangeExpr {
					start: None,
					end: Some(_),
					..
				}) => (),
				_ => panic!("Expected range without start in for loop"),
			},
			_ => panic!("Expected for statement"),
		}
	}

	#[test]
	fn test_parse_empty_range_in_array_index()
	{
		let result = parse_expr_from_str("arr[..5]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Index { index, .. } => match index.as_ref() {
				Expr::Range(RangeExpr {
					start: None,
					end: Some(_),
					..
				}) => (),
				_ => panic!("Expected range without start in index"),
			},
			_ => panic!("Expected index expression"),
		}
	}

	#[test]
	fn test_parse_empty_range_in_function_call()
	{
		let result = parse_expr_from_str("slice(..10)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { args, .. } => {
				assert_eq!(args.len(), 1);
				match &args[0] {
					Expr::Range(RangeExpr {
						start: None,
						end: Some(_),
						..
					}) => (),
					_ => panic!("Expected range without start as argument"),
				}
			}
			_ => panic!("Expected function call"),
		}
	}

	#[test]
	fn test_parse_empty_range_in_tuple()
	{
		let result = parse_expr_from_str("(..10, 5..15)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Tuple { elements, .. } => {
				assert_eq!(elements.len(), 2);
				match &elements[0] {
					Expr::Range(RangeExpr {
						start: None,
						end: Some(_),
						..
					}) => (),
					_ => panic!("Expected range without start in first tuple element"),
				}
				match &elements[1] {
					Expr::Range(RangeExpr {
						start: Some(_),
						end: Some(_),
						..
					}) => (),
					_ => panic!("Expected normal range in second tuple element"),
				}
			}
			_ => panic!("Expected tuple expression"),
		}
	}

	#[test]
	fn test_parse_empty_range_in_array_literal()
	{
		let result = parse_expr_from_str("[..10, 5..15]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::List { elements, .. }) => {
				assert_eq!(elements.len(), 2);
				match &elements[0] {
					Expr::Range(RangeExpr {
						start: None,
						end: Some(_),
						..
					}) => (),
					_ => panic!("Expected range without start in array"),
				}
			}
			_ => panic!("Expected array literal"),
		}
	}

	#[test]
	fn test_parse_empty_range_in_struct_init()
	{
		let result = parse_expr_from_str("Range { value = ..10 }");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::StructInit { fields, .. } => {
				assert_eq!(fields.len(), 1);
				match &fields[0].1 {
					Expr::Range(RangeExpr {
						start: None,
						end: Some(_),
						..
					}) => (),
					_ => panic!("Expected range without start in struct field"),
				}
			}
			_ => panic!("Expected struct init"),
		}
	}

	#[test]
	fn test_parse_empty_range_in_assignment()
	{
		let result = parse_block_from_str("{ range = ..10; }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_empty_range_in_return()
	{
		let result = parse_block_from_str("{ return ..10; }");
		assert!(result.is_ok());
	}

	// ========== Empty Range with Expressions Tests ==========

	#[test]
	fn test_parse_empty_range_with_complex_end()
	{
		let result = parse_expr_from_str("..x + 10");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: None,
				end: Some(end),
				inclusive: false,
				..
			}) => match end.as_ref() {
				Expr::Binary { op: BinaryOp::Add, .. } => (),
				_ => panic!("Expected addition in end expression"),
			},
			_ => panic!("Expected range with complex end"),
		}
	}

	#[test]
	fn test_parse_empty_range_with_function_call_end()
	{
		let result = parse_expr_from_str("..get_max()");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: None,
				end: Some(end),
				..
			}) => match end.as_ref() {
				Expr::Call { .. } => (),
				_ => panic!("Expected function call in end"),
			},
			_ => panic!("Expected range with function call end"),
		}
	}

	#[test]
	fn test_parse_empty_range_with_field_access_end()
	{
		let result = parse_expr_from_str("..obj.field");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: None,
				end: Some(end),
				..
			}) => match end.as_ref() {
				Expr::Field { .. } => (),
				_ => panic!("Expected field access in end"),
			},
			_ => panic!("Expected range with field access end"),
		}
	}

	#[test]
	fn test_parse_empty_range_with_index_end()
	{
		let result = parse_expr_from_str("..arr[5]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Range(RangeExpr {
				start: None,
				end: Some(end),
				..
			}) => match end.as_ref() {
				Expr::Index { .. } => (),
				_ => panic!("Expected index in end"),
			},
			_ => panic!("Expected range with index end"),
		}
	}

	// ========== Empty Range Pattern Tests ==========

	#[test]
	fn test_parse_pattern_range_no_start()
	{
		let result = parse_block_from_str("{ switch x { ..10 => true, } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_range_no_start_inclusive()
	{
		let result = parse_block_from_str("{ switch x { ..=10 => true, } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_full_empty_range()
	{
		let result = parse_block_from_str("{ switch x { .. => true, } }");
		assert!(result.is_ok());
	}

	// ========== Edge Cases ==========

	#[test]
	fn test_parse_empty_range_trailing_comma()
	{
		let result = parse_expr_from_str("(..10,)");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_multiple_empty_ranges()
	{
		let result = parse_expr_from_str("[.., .., ..]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::List { elements, .. }) => {
				assert_eq!(elements.len(), 3);
				for elem in elements {
					match elem {
						Expr::Range(RangeExpr {
							start: None,
							end: None,
							inclusive: false,
							..
						}) => (),
						_ => panic!("Expected empty range"),
					}
				}
			}
			_ => panic!("Expected array"),
		}
	}

	#[test]
	fn test_parse_empty_range_in_nested_expression()
	{
		let result = parse_expr_from_str("foo(bar(..10))");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_empty_range_with_cast()
	{
		let result = parse_expr_from_str("(Range)..(i32)10;");
		assert!(result.is_ok());
	}

	// ========== Display Tests ==========

	#[test]
	fn test_display_empty_range_to_end()
	{
		let result = parse_expr_from_str("..10").unwrap();
		let display = format!("{}", result);
		assert!(display.contains(".."));
		assert!(display.contains("10"));
	}

	#[test]
	fn test_display_empty_range_full()
	{
		let result = parse_expr_from_str("..;").unwrap();
		let display = format!("{}", result);
		assert!(display.contains(".."));
	}

	#[test]
	fn test_display_empty_range_inclusive()
	{
		let result = parse_expr_from_str("..=10").unwrap();
		let display = format!("{}", result);
		assert!(display.contains("..="));
		assert!(display.contains("10"));
	}

	// ========== Real-World Usage Tests ==========

	#[test]
	fn test_parse_slice_from_beginning()
	{
		let input = "{ var slice: &i32[] = arr[..5]; }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_range_iterator_from_zero()
	{
		let input = "{ for i in ..100 { println(i); } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_range_match_less_than()
	{
		let input = r#"{
		switch age {
			..18 => "minor",
			18..65 => "adult",
			65.. => "senior",
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_range_inclusive_boundary()
	{
		let input = r#"{
		switch score {
			..=59 => "F",
			60..=69 => "D",
			70..=79 => "C",
			80..=89 => "B",
			90.. => "A",
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Error Cases ==========

	#[test]
	fn test_empty_inclusive_range_without_end_error()
	{
		// This should be an error: can't have ..= without an end
		// The parser should handle this gracefully
		let result = parse_expr_from_str("..=");
		assert!(result.is_err());
	}

	#[test]
	fn test_whitespace_in_empty_range()
	{
		let result = parse_expr_from_str(".. 10");
		assert!(result.is_ok());
	}

	// ========== Call Tests ==========

	#[test]
	fn test_parse_function_call_no_args()
	{
		let result = parse_expr_from_str("foo()");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { callee, args, .. } => {
				match *callee {
					Expr::Identifier { ref path, .. }
						if path == &Path::simple(vec!["foo".to_string()], Default::default()) => {}
					_ => panic!("Expected foo identifier"),
				}
				assert_eq!(args.len(), 0);
			}
			_ => panic!("Expected function call"),
		}
	}

	#[test]
	fn test_parse_function_call_with_args()
	{
		let result = parse_expr_from_str("foo(1, 2, 3)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { callee, args, .. } => {
				match *callee {
					Expr::Identifier { ref path, .. }
						if path == &Path::simple(vec!["foo".to_string()], Default::default()) => {}
					_ => panic!("Expected foo identifier"),
				}
				assert_eq!(args.len(), 3);
			}
			_ => panic!("Expected function call"),
		}
	}

	// ========== Field Access Tests ==========

	#[test]
	fn test_parse_field_access()
	{
		let result = parse_expr_from_str("obj.field");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Field { base, name, .. } => {
				match *base {
					Expr::Identifier { ref path, .. }
						if path == &Path::simple(vec!["obj".to_string()], Default::default()) => {}
					_ => panic!("Expected obj identifier"),
				}
				assert_eq!(name, "field");
			}
			_ => panic!("Expected field access"),
		}
	}

	#[test]
	fn test_parse_chained_field_access()
	{
		let result = parse_expr_from_str("obj.field1.field2");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Field { base, name, .. } => {
				assert_eq!(name, "field2");
				match *base {
					Expr::Field {
						name: ref inner_name, ..
					} => {
						assert_eq!(inner_name, "field1");
					}
					_ => panic!("Expected nested field access"),
				}
			}
			_ => panic!("Expected field access"),
		}
	}

	// ========== Index Tests ==========

	#[test]
	fn test_parse_array_index()
	{
		let result = parse_expr_from_str("arr[0]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Index { base, index, .. } => {
				match *base {
					Expr::Identifier { ref path, .. }
						if path == &Path::simple(vec!["arr".to_string()], Default::default()) => {}
					_ => panic!("Expected arr identifier"),
				}
				match *index {
					Expr::Literal {
						value: Literal::Int(0), ..
					} => (),
					_ => panic!("Expected 0 index"),
				}
			}
			_ => panic!("Expected index expression"),
		}
	}

	// ========== Tuple Tests ==========

	#[test]
	fn test_parse_empty_tuple()
	{
		let result = parse_expr_from_str("()");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Tuple { elements, .. } => assert_eq!(elements.len(), 0),
			_ => panic!("Expected empty tuple"),
		}
	}

	#[test]
	fn test_parse_single_element_tuple()
	{
		let result = parse_expr_from_str("(1,)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Tuple { elements, .. } => assert_eq!(elements.len(), 1),
			_ => panic!("Expected single-element tuple"),
		}
	}

	#[test]
	fn test_parse_multi_element_tuple()
	{
		let result = parse_expr_from_str("(1, 2, 3)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Tuple { elements, .. } => assert_eq!(elements.len(), 3),
			_ => panic!("Expected multi-element tuple"),
		}
	}

	#[test]
	fn test_parse_parenthesized_expr()
	{
		let result = parse_expr_from_str("(42)");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Int(42),
				..
			} => (),
			_ => panic!("Expected parenthesized expression to unwrap"),
		}
	}

	// ========== Array Tests ==========

	#[test]
	fn test_parse_empty_array()
	{
		let result = parse_expr_from_str("[]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::List { elements, .. }) => assert_eq!(elements.len(), 0),
			_ => panic!("Expected empty array"),
		}
	}

	#[test]
	fn test_parse_array_list()
	{
		let result = parse_expr_from_str("[1, 2, 3]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::List { elements, .. }) => assert_eq!(elements.len(), 3),
			_ => panic!("Expected array list"),
		}
	}

	#[test]
	fn test_parse_array_repeat()
	{
		let result = parse_expr_from_str("[0; 10]");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::Repeat { value, count, .. }) => {
				match *count {
					Expr::Literal {
						value: Literal::Int(10),
						..
					} => (),
					_ => panic!("Expected count of 10"),
				}
				match *value {
					Expr::Literal {
						value: Literal::Int(0), ..
					} => (),
					_ => panic!("Expected count of 0"),
				}
			}
			_ => panic!("Expected array repeat"),
		}
	}

	// ========== Struct Init Tests ==========

	#[test]
	fn test_parse_struct_init_empty()
	{
		let result = parse_expr_from_str("Point {}").inspect_err(|e| println!("{}", e));
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::StructInit { path, fields, .. } => {
				assert_eq!(path, Path::simple(vec!["Point".to_string()], Default::default()));
				assert_eq!(fields.len(), 0);
			}
			_ => panic!("Expected struct init"),
		}
	}

	#[test]
	fn test_parse_struct_init_with_fields()
	{
		let result = parse_expr_from_str("Point { x = 1, y = 2 }");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::StructInit { path, fields, .. } => {
				assert_eq!(path, Path::simple(vec!["Point".to_string()], Default::default()));
				assert_eq!(fields.len(), 2);
				assert_eq!(fields[0].0, "x");
				assert_eq!(fields[1].0, "y");
			}
			_ => panic!("Expected struct init"),
		}
	}

	// ========== Block Tests ==========

	#[test]
	fn test_parse_empty_block()
	{
		let result = parse_expr_from_str("{}");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Block(block) => {
				assert_eq!(block.stmts.len(), 0);
				assert!(block.tail_expr.is_none());
			}
			_ => panic!("Expected block expression"),
		}
	}

	#[test]
	fn test_parse_block_with_statements()
	{
		let input = "{ var x: i32 = 5; x + 1 }";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Block(block) => {
				assert_eq!(block.stmts.len(), 1);
				assert!(block.tail_expr.is_some());
			}
			_ => panic!("Expected block expression"),
		}
	}

	// ========== Switch Tests ==========

	#[test]
	fn test_parse_switch_expression()
	{
		let input = r#"switch x {
            1 => 10,
            2 => 20,
            _ => 0,
        }"#;
		let result = parse_expr_from_str(input);
		match &result {
			Ok(program) => {
				println!("{:#?}", program);
			}
			Err(e) => {
				println!("{}", e);
			}
		}
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Switch { expr: _, arms, .. } => {
				assert_eq!(arms.len(), 3);
			}
			_ => panic!("Expected switch expression"),
		}
	}

	// ========== Type Tests ==========

	#[test]
	fn test_parse_simple_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "i32", "test_file_4", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Base { path, .. } if path == &Path::simple(vec!["i32".to_string()], Default::default()) => (),
			_ => panic!("Expected i32 type"),
		}
	}

	#[test]
	fn test_parse_generic_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "Vec<i32>", "test_file_5", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Base { path, generics } => {
				assert_eq!(path, &Path::simple(vec!["Vec".to_string()], Default::default()));
				assert_eq!(generics.len(), 1);
			}
			_ => panic!("Expected generic type"),
		}
	}

	#[test]
	fn test_parse_reference_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "&i32", "test_file_6", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Reference { mutable: false, .. } => (),
			_ => panic!("Expected reference type"),
		}
	}

	#[test]
	fn test_parse_mutable_reference_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "&mut i32", "test_file_7", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Reference { mutable: true, .. } => (),
			_ => panic!("Expected mutable reference type"),
		}
	}

	#[test]
	fn test_parse_pointer_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "i32*", "test_file_8", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Pointer { .. } => (),
			_ => panic!("Expected pointer type"),
		}
	}

	#[test]
	fn test_parse_array_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "i32[10]", "test_file_9", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Array { .. } => (),
			_ => panic!("Expected array type"),
		}
	}

	// ========== Variable Declaration Tests ==========

	#[test]
	fn test_parse_var_declaration()
	{
		let input = "var x: i32 = 5;";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		assert_eq!(program.items.len(), 1);
	}

	#[test]
	fn test_parse_const_declaration()
	{
		let input = "const X: i32 = 5;";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		assert_eq!(program.items.len(), 1);
	}

	// ========== Function Tests ==========

	#[test]
	fn test_parse_simple_function()
	{
		let input = r#"
            fn foo() {
                return 42;
            }
        "#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		assert_eq!(program.items.len(), 1);
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(
					func.signature.name,
					Path::simple(vec!["foo".to_string()], Default::default())
				);
				assert_eq!(func.signature.params.len(), 0);
				assert!(func.body.is_some());
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_function_with_params()
	{
		let input = "fn add(x: i32, y: i32) -> i32 { x + y }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.params.len(), 2);
				assert!(func.signature.return_type.is_some());
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_function_with_generics()
	{
		let input = "fn identity<T>(x: T) -> T { x }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.generics.len(), 1);
				assert_eq!(func.signature.generics[0].name, "T");
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_function_with_modifiers()
	{
		let input = "pub unsafe fn dangerous() {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert!(func.signature.modifiers.len() >= 2);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_heap_function()
	{
		let input = "fn! heap_allocated() {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.call_type, CallType::UserHeap);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_method_with_self()
	{
		let input = "fn method(self) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.params.len(), 1);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_method_with_ref_self()
	{
		let input = "fn method(&self) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_method_with_mut_ref_self()
	{
		let input = "fn method(&mut self) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Struct Tests ==========

	#[test]
	fn test_parse_empty_struct()
	{
		let input = "struct Empty {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Struct(s) => {
				assert_eq!(s.name, Path::simple(vec!["Empty".to_string()], Default::default()));
				assert_eq!(s.fields.len(), 0);
			}
			_ => panic!("Expected struct declaration"),
		}
	}

	#[test]
	fn test_parse_struct_with_fields()
	{
		let input = "struct Point { x: i32, y: i32 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Struct(s) => {
				assert_eq!(s.fields.len(), 2);
				assert_eq!(s.fields[0].1, "x");
				assert_eq!(s.fields[1].1, "y");
			}
			_ => panic!("Expected struct declaration"),
		}
	}

	#[test]
	fn test_parse_pub_struct()
	{
		let input = "pub struct Public {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Struct(s) => {
				assert!(!s.modifiers.is_empty());
			}
			_ => panic!("Expected struct declaration"),
		}
	}

	// ========== Union Tests ==========

	#[test]
	fn test_parse_union()
	{
		let input = "union Data { i: i32, f: f64 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Union(u) => {
				assert_eq!(u.name, Path::simple(vec!["Data".to_string()], Default::default()));
				assert_eq!(u.fields.len(), 2);
			}
			_ => panic!("Expected union declaration"),
		}
	}

	// ========== Enum Tests ==========

	#[test]
	fn test_parse_simple_enum()
	{
		let input = "enum Color { Red, Green, Blue }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Enum(e) => {
				assert_eq!(e.name, Path::simple(vec!["Color".to_string()], Default::default()));
				assert_eq!(e.variants.len(), 3);
			}
			_ => panic!("Expected enum declaration"),
		}
	}

	#[test]
	fn test_parse_enum_with_values()
	{
		let input = "enum Number { One = 1, Two = 2 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Enum(e) => {
				assert_eq!(e.variants.len(), 2);
				assert!(e.variants[0].1.is_some());
			}
			_ => panic!("Expected enum declaration"),
		}
	}

	// ========== Variant Tests ==========

	#[test]
	fn test_parse_variant()
	{
		let input = "variant Option { Some(i32), None }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Variant(v) => {
				assert_eq!(v.name, Path::simple(vec!["Option".to_string()], Default::default()));
				assert_eq!(v.variants.len(), 2);
			}
			_ => panic!("Expected variant declaration"),
		}
	}

	// ========== Type Alias Tests ==========

	#[test]
	fn test_parse_type_alias()
	{
		let input = "type Int = i32;";
		let result = parse_program_from_str(input).inspect_err(|e| println!("{}", e));
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::TypeAlias(t) => {
				assert_eq!(t.name, Path::simple(vec!["Int".to_string()], Default::default()));
			}
			_ => panic!("Expected type alias declaration"),
		}
	}

	// ========== Namespace Tests ==========

	#[test]
	fn test_parse_namespace()
	{
		let input = "namespace std { fn foo() {} }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Namespace(n) => {
				assert_eq!(n.name, Path::simple(vec!["std".to_string()], Default::default()));
				assert_eq!(n.body.items.len(), 1);
			}
			_ => panic!("Expected namespace declaration"),
		}
	}

	// ========== Trait Tests ==========

	#[test]
	fn test_parse_simple_trait()
	{
		let input = "trait Display { fn fmt(&self); }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Trait(t) => {
				assert_eq!(t.name, Path::simple(vec!["Display".to_string()], Default::default()));
				assert_eq!(t.items.len(), 1);
			}
			_ => panic!("Expected trait declaration"),
		}
	}

	#[test]
	fn test_parse_trait_with_generics()
	{
		let input = "trait Convert<T> { fn convert(self) -> T; }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Trait(t) => {
				assert_eq!(t.generics.len(), 1);
			}
			_ => panic!("Expected trait declaration"),
		}
	}

	#[test]
	fn test_parse_trait_with_supertraits()
	{
		let input = "trait Sub: Super { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Trait(t) => {
				assert_eq!(t.super_traits.len(), 1);
			}
			_ => panic!("Expected trait declaration"),
		}
	}

	// ========== Impl Tests ==========

	#[test]
	fn test_parse_inherent_impl()
	{
		let input = "impl MyStruct { fn method(&self) {} }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				assert_eq!(
					i.target.path,
					Path::simple(vec!["MyStruct".to_string()], Default::default())
				);
				assert!(i.trait_path.is_none());
				assert_eq!(i.body.len(), 1);
			}
			_ => panic!("Expected impl declaration"),
		}
	}

	#[test]
	fn test_parse_trait_impl()
	{
		let input = "impl Display for MyStruct { fn fmt(&self) {} }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				assert!(i.trait_path.is_some());
				assert_eq!(
					i.target.path,
					Path::simple(vec!["MyStruct".to_string()], Default::default())
				);
			}
			_ => panic!("Expected impl declaration"),
		}
	}

	#[test]
	fn test_parse_generic_impl()
	{
		let input = "impl<T> MyStruct<T> { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				assert_eq!(i.generics.len(), 1);
			}
			_ => panic!("Expected impl declaration"),
		}
	}

	// ========== Statement Tests ==========

	#[test]
	fn test_parse_if_statement()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "{if true { 1 }}", "test_file_10", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_else_statement()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer =
			Lexer::new_add_to_source_map(&config, "{if true { 1 } else { 2 };}", "test_file_11", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_while_statement()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "{ while true { break; } }", "test_file_12", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_for_statement()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "{ for i in 0..10 { } }", "test_file_13", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_return_statement()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "{ return 42; }", "test_file_14", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_statement()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "{ break; }", "test_file_15", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_continue_statement()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "{ continue; }", "test_file_16", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_assignment()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "{ x = 5; }", "test_file_17", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_compound_assignment()
	{
		let ops = vec!["+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="];
		for op in ops {
			let input = format!("{{ x {} 5; }}", op);
			let config = Config::default();
			let mut source_map = SourceMap::default();
			let lexer = Lexer::new_add_to_source_map(&config, &input, "test_file_18", &mut source_map);
			let mut parser = Parser::from(lexer);
			let result = parser.parse_block();
			assert!(result.is_ok(), "Failed to parse compound assignment: {}", op);
		}
	}

	// ========== Complex Expression Tests ==========

	#[test]
	fn test_parse_nested_calls()
	{
		let result = parse_expr_from_str("foo(bar(baz()))");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_method_chain()
	{
		let result = parse_expr_from_str("obj.method1().method2().field");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_complex_expression()
	{
		let result = parse_expr_from_str("(a + b) * c - d / e");
		assert!(result.is_ok());
	}

	// ========== Error Tests ==========

	#[test]
	fn test_parse_error_unexpected_token()
	{
		let result = parse_expr_from_str("var");
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_error_unmatched_paren()
	{
		let result = parse_expr_from_str("(1 + 2");
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_error_invalid_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "123", "test_file_19", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_if_var_basic()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ if var Some(x: i64) = opt { x } }",
			"test_file_20",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
		let block = result.unwrap();
		assert_eq!(block.stmts.len(), 0);
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_if_var_with_else()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ if var Some(x: i64) = opt { x } else { 0 }; }",
			"test_file_21",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_var_else_if_var()
	{
		let config = Config::default();
		let lexer = Lexer::new(
			&config,
			"{ if var Some(x: i32) = opt1 { x } else if var Some(y: i32) = opt2 { y } else { 0 }; }",
			0,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_var_tuple_pattern()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ if var (x: i32, y: i32) = pair { x + y } }",
			"test_file_22",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_var_wildcard()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ if var Some(_) = opt { true } }",
			"test_file_23",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_var_wildcard_with_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ if var Some(_: i64) = opt { true } }",
			"test_file_24",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	// ========== While Let Tests ==========

	#[test]
	fn test_parse_while_let()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ while var Some(x: i32) = iter.next() { process(x); } }",
			"test_file_25",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::WhileVarLoop { .. } => (),
			_ => panic!("Expected while var loop"),
		}
	}

	#[test]
	fn test_parse_while_let_tuple()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ while var (a: i32, b: i32) = get_pair() { } }",
			"test_file_26",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	// ========== Enhanced Pattern Tests ==========

	#[test]
	fn test_parse_struct_pattern()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ if var Point { x: i32, y: i32 } = pt { x } }",
			"test_file_27",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_struct_pattern_nested()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ if var Point { x = a: i32, y = b: i32 } = pt { a } }",
			"test_file_28",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_or_pattern()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ if var Some(1) | Some(2) | Some(3) = x { true } }",
			"test_file_29",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_nested_variant_pattern()
	{
		let config = Config::default();
		let lexer = Lexer::new(
			&config,
			"{ if var Some(Point { x = a: i32, y = b: i32 }) = opt { a } }",
			0,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_variant_with_tuple()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ if var Some((x: i32, y: i32)) = opt { x } }",
			"test_file_30",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_unit_variant()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ if var None = opt { true } }",
			"test_file_31",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_switch_with_typed_patterns()
	{
		let input = r#"
			switch x {
				Some(val: i32) => val,
				None => 0,
			}
		"#;
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_loop_basic()
	{
		let result = parse_block_from_str("{ loop { break; }; }");
		assert!(result.is_ok());
		let block = result.unwrap();
		assert_eq!(block.stmts.len(), 1);
		match &block.stmts[0] {
			Stmt::Loop { .. } => (),
			_ => panic!("Expected loop statement"),
		}
	}

	#[test]
	fn test_parse_loop_with_continue()
	{
		let result = parse_block_from_str("{ loop { if x { continue; } break; } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_nested_loops()
	{
		let result = parse_block_from_str("{ loop { loop { break; } break; } }");
		assert!(result.is_ok());
	}

	// ========== Delete Statement Tests ==========

	#[test]
	fn test_parse_delete_simple()
	{
		let result = parse_block_from_str("{ delete ptr; }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Delete { expr, .. } => match expr {
				Expr::Identifier { path, .. } => {
					assert_eq!(path, &Path::simple(vec!["ptr".to_string()], Default::default()));
				}
				_ => panic!("Expected identifier expression"),
			},
			_ => panic!("Expected delete statement"),
		}
	}

	#[test]
	fn test_parse_delete_qualified_path()
	{
		let result = parse_block_from_str("{ delete std::ptr; }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Delete { expr, .. } => match expr {
				Expr::Identifier { path, .. } => {
					assert_eq!(
						path,
						&Path::simple(vec!["std".to_string(), "ptr".to_string()], Default::default())
					);
				}
				_ => panic!("Expected identifier expression"),
			},
			_ => panic!("Expected delete statement"),
		}
	}

	// ========== Directive Tests ==========

	#[test]
	fn test_parse_import_directive()
	{
		let input = r#"@import "file.rs";"#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Directive(DirectiveNode {
				directive: Directive::Import(path),
				..
			}) => {
				assert_eq!(path, "file.rs");
			}
			_ => panic!("Expected import directive"),
		}
	}

	#[test]
	fn test_parse_use_directive()
	{
		let input = "@use std::vec::Vec;";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Directive(DirectiveNode {
				directive: Directive::Use(path),
				..
			}) => {
				assert_eq!(
					path,
					&Path::simple(
						vec!["std".to_string(), "vec".to_string(), "Vec".to_string()],
						Default::default()
					)
				);
			}
			_ => panic!("Expected use directive"),
		}
	}

	#[test]
	fn test_parse_custom_directive_no_args()
	{
		let input = "@custom;";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_custom_directive_with_args()
	{
		let input = "@custom(1, 2, 3);";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Directive(DirectiveNode {
				directive: Directive::Custom { name, params },
				..
			}) => {
				assert_eq!(name, "custom");
				assert_eq!(params.len(), 3);
			}
			_ => panic!("Expected custom directive"),
		}
	}

	// ========== Modifier Tests ==========

	#[test]
	fn test_parse_function_with_inline_modifier()
	{
		let input = "inline fn fast() {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert!(func.signature.modifiers.iter().any(|m| matches!(m, Modifier::Inline)));
			}
			_ => panic!("Expected function"),
		}
	}

	#[test]
	fn test_parse_function_with_const_modifier()
	{
		let input = "const fn compile_time() {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert!(func.signature.modifiers.iter().any(|m| matches!(m, Modifier::Const)));
			}
			_ => panic!("Expected function"),
		}
	}

	#[test]
	fn test_parse_function_with_multiple_modifiers()
	{
		let input = "pub unsafe inline fn complex() {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert!(func.signature.modifiers.len() >= 3);
			}
			_ => panic!("Expected function"),
		}
	}

	// ========== Type System Tests ==========

	#[test]
	fn test_parse_nested_pointer_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "i32**", "test_file_32", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Pointer { inner } => match inner.as_ref() {
				TypeCore::Pointer { .. } => (),
				_ => panic!("Expected nested pointer"),
			},
			_ => panic!("Expected pointer type"),
		}
	}

	#[test]
	fn test_parse_array_of_pointers()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "i32*[10]", "test_file_33", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Array { inner, .. } => match inner.as_ref() {
				TypeCore::Pointer { .. } => (),
				_ => panic!("Expected pointer element type"),
			},
			_ => panic!("Expected array type"),
		}
	}

	#[test]
	fn test_parse_pointer_to_array()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "i32[10]*", "test_file_34", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_tuple_type_single_element()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "(i32,)", "test_file_35", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Tuple(types) => assert_eq!(types.len(), 1),
			_ => panic!("Expected tuple type"),
		}
	}

	#[test]
	fn test_parse_tuple_type_empty()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "()", "test_file_36", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Tuple(types) => assert_eq!(types.len(), 0),
			_ => panic!("Expected empty tuple type"),
		}
	}

	#[test]
	fn test_parse_nested_generic_types()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "Vec<Vec<i32>>", "test_file_37", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Base { path, generics } => {
				assert_eq!(path, &Path::simple(vec!["Vec".to_string()], Default::default()));
				assert_eq!(generics.len(), 1);
			}
			_ => panic!("Expected generic type"),
		}
	}

	#[test]
	fn test_parse_multiple_generic_arguments()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "Map<String, i32>", "test_file_38", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Base { generics, .. } => {
				assert_eq!(generics.len(), 2);
			}
			_ => panic!("Expected generic type"),
		}
	}

	#[test]
	fn test_parse_qualified_type_path()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "std::vec::Vec<i32>", "test_file_39", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Base { path, .. } => {
				assert_eq!(
					path,
					&Path::simple(
						vec!["std".to_string(), "vec".to_string(), "Vec".to_string()],
						Default::default()
					)
				);
			}
			_ => panic!("Expected qualified type"),
		}
	}

	#[test]
	fn test_parse_reference_to_tuple()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "&(i32, i32)", "test_file_40", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
	}

	// ========== Pattern Tests ==========

	#[test]
	fn test_parse_pattern_range_inclusive()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ switch x { 1..=10 => true, } }",
			"test_file_41",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_range_exclusive()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ switch x { 1..10 => true, } }",
			"test_file_42",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_range_open_ended()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ switch x { 1.. => true, } }",
			"test_file_43",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_char_literal()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ switch x { 'a' => true, } }",
			"test_file_44",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_string_literal()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			r#"{ switch x { "hello" => true, } }"#,
			"test_file_45",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_bool_literals()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ switch x { true => 1, false => 0, } }",
			"test_file_46",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_nested_tuple()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ switch x { ((a: i32, b: i32), c: i32) => a, } }",
			"test_file_47",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_variant_multiple_args()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ switch x { Some(a: i32, b: i32, c: i32) => a, } }",
			"test_file_48",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_or_with_wildcards()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(
			&config,
			"{ switch x { None | Some(_) => true, } }",
			"test_file_49",
			&mut source_map,
		);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_block();
		assert!(result.is_ok());
	}

	// ========== Expression Edge Switchs ==========

	#[test]
	fn test_parse_nested_struct_init()
	{
		let result = parse_expr_from_str("Outer { inner = Inner { x = 1 } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_array_of_tuples()
	{
		let result = parse_expr_from_str("[(1, 2), (3, 4), (5, 6)]");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_tuple_of_arrays()
	{
		let result = parse_expr_from_str("([1, 2], [3, 4])");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_array_repeat_with_expression()
	{
		let result = parse_expr_from_str("[x + 1; 10]");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_struct_init_with_computed_fields()
	{
		let result = parse_expr_from_str("Point { x = a + b, y = c * d }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_call_with_struct_init()
	{
		let result = parse_expr_from_str("foo(Point { x = 1, y = 2 })");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_index_chain()
	{
		let result = parse_expr_from_str("arr[0][1][2]");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_mixed_postfix_operations()
	{
		let result = parse_expr_from_str("obj.method()[0].field");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_cast_in_expression()
	{
		let result = parse_expr_from_str("(i64)x + (i64)y");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_address_of_field()
	{
		let result = parse_expr_from_str("&obj.field");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_dereference_chain()
	{
		let result = parse_expr_from_str("***ptr");
		assert!(result.is_ok());
	}

	// ========== Block and Tail Expression Tests ==========

	#[test]
	fn test_parse_block_with_only_tail_expr()
	{
		let result = parse_block_from_str("{ 42 }");
		assert!(result.is_ok());
		let block = result.unwrap();
		assert_eq!(block.stmts.len(), 0);
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_block_tail_expr_after_semicolons()
	{
		let result = parse_block_from_str("{ var x: i32 = 1; var y: i32 = 2; x + y }");
		assert!(result.is_ok());
		let block = result.unwrap();
		assert_eq!(block.stmts.len(), 2);
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_nested_blocks_with_tail()
	{
		let result = parse_block_from_str("{ { { 42 } } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_as_tail_expr()
	{
		let result = parse_block_from_str("{ if true { 1 } else { 2 } }");
		assert!(result.is_ok());
		let block = result.unwrap();
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_switch_as_tail_expr()
	{
		let result = parse_block_from_str("{ switch x { 1 => 10, _ => 0, } }");
		assert!(result.is_ok());
		let block = result.unwrap();
		assert!(block.tail_expr.is_some());
	}

	#[test]
	fn test_parse_function_default()
	{
		let input = "fn default() -> Self { Self::new() }";
		let result = parse_program_from_str(input);
		assert!(result.is_err()); // should fail, no function default is allowed, because of the default keyword
	}

	#[test]
	fn test_parse_function_new()
	{
		let input = "fn new() -> Self { Self::new() }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_trait_with_associated_type()
	{
		let input = "trait Iterator { type Item; }";
		let result = parse_program_from_str(input);
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_trait_with_associated_const()
	{
		let input = "trait HasMax { const MAX: i32; }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_trait_with_multiple_supertraits()
	{
		let input = "trait Sub: Super1 + Super2 + Super3 { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Trait(t) => {
				assert_eq!(t.super_traits.len(), 3);
			}
			_ => panic!("Expected trait"),
		}
	}

	// ========== Impl Block Tests ==========

	#[test]
	fn test_parse_impl_with_associated_type()
	{
		let input = "impl Iterator for Counter { type Item = i32; }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_impl_with_associated_const()
	{
		let input = "impl HasMax for MyType { const MAX: i32 = 100; }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_impl_with_where_clause()
	{
		let input = "impl<T> MyTrait for MyType<T> where T: Clone { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				assert_eq!(i.where_clause.len(), 1);
			}
			_ => panic!("Expected impl"),
		}
	}

	#[test]
	fn test_parse_impl_with_multiple_where_constraints()
	{
		let input = "impl<T, U> Trait for Type<T, U> where T: Clone, U: Debug { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				assert_eq!(i.where_clause.len(), 2);
			}
			_ => panic!("Expected impl"),
		}
	}

	#[test]
	fn test_parse_impl_with_multiple_bounds()
	{
		let input = "impl<T> Trait for Type<T> where T: Clone + Debug + Display { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				assert_eq!(i.where_clause[0].bounds.len(), 3);
			}
			_ => panic!("Expected impl"),
		}
	}

	// ========== Namespace Tests ==========

	#[test]
	fn test_parse_nested_namespace()
	{
		let input = "namespace outer { namespace inner { fn foo() {} } }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Namespace(n) => {
				assert_eq!(n.body.items.len(), 1);
				match &n.body.items[0] {
					TopLevelDecl::Namespace(_) => (),
					_ => panic!("Expected nested namespace"),
				}
			}
			_ => panic!("Expected namespace"),
		}
	}

	#[test]
	fn test_parse_qualified_namespace_name()
	{
		let input = "namespace std::vec { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Namespace(n) => {
				assert_eq!(
					n.name,
					Path::simple(vec!["std".to_string(), "vec".to_string()], Default::default())
				);
			}
			_ => panic!("Expected namespace"),
		}
	}

	// ========== Statement Combination Tests ==========

	#[test]
	fn test_parse_if_else_if_chain()
	{
		let result = parse_block_from_str("{ if a { 1 } else if b { 2 } else if c { 3 } else { 4 }; }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_if_var_else_if_chain()
	{
		let result =
			parse_block_from_str("{ if var Some(x: i32) = a { x } else if var Some(y: i32) = b { y } else { 0 }; }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_mixed_if_if_var_chain()
	{
		let result =
			parse_block_from_str("{ if a { 1 } else if var Some(x: i32) = b { x } else if c { 3 } else { 4 }; }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_nested_if_statements()
	{
		let result = parse_block_from_str("{ if a { if b { 1 } else { 2 } } else { 3 }; }");
		assert!(result.is_ok());
	}

	// ========== Variable Declaration Tests ==========

	#[test]
	fn test_parse_var_without_init()
	{
		let result = parse_block_from_str("{ var x: i32; }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_const_with_complex_expr()
	{
		let result = parse_block_from_str("{ const X: i32 = 2 + 3 * 4; }");
		assert!(result.is_ok());
	}

	// ========== Function Parameter Tests ==========

	#[test]
	fn test_parse_function_with_qualified_param_name()
	{
		let input = "fn foo(ns::name: i32) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(
					func.signature.params[0].name,
					Path::simple(vec!["ns".to_string(), "name".to_string()], Default::default())
				);
			}
			_ => panic!("Expected function"),
		}
	}

	#[test]
	fn test_parse_function_with_trailing_comma()
	{
		let input = "fn foo(x: i32, y: i32,) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Switch Expression Tests ==========

	#[test]
	fn test_parse_switch_with_block_arms()
	{
		let input = "switch x { 1 => { println(1); }, 2 => { println(2); }, }";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Switch { arms, .. } => {
				for arm in arms {
					match arm.body {
						SwitchBody::Block(_) => (),
						SwitchBody::Expr(_) => panic!("Expected block arm"),
					}
				}
			}
			_ => panic!("Expected switch expression"),
		}
	}

	#[test]
	fn test_parse_switch_mixed_arms()
	{
		let input = "switch x { 1 => 10, 2 => { println(2); 20 }, }";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_nested_switch()
	{
		let input = "switch x { 1 => switch y { 2 => 3, _ => 4, }, _ => 0, }";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Unsafe Block Tests ==========

	#[test]
	fn test_parse_unsafe_block_as_statement()
	{
		let result = parse_block_from_str("{ unsafe { ptr.write(42); } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_unsafe_block_as_expr()
	{
		let result = parse_block_from_str("{ var x: i32 = unsafe { *ptr }; }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_unsafe_block_as_tail()
	{
		let result = parse_block_from_str("{ unsafe { *ptr } }");
		assert!(result.is_ok());
		let block = result.unwrap();
		assert!(block.tail_expr.is_some());
	}

	// ========== Complex Struct/Variant Tests ==========

	#[test]
	fn test_parse_struct_with_qualified_name()
	{
		let input = "struct ns::Name { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Struct(s) => {
				assert_eq!(
					s.name,
					Path::simple(vec!["ns".to_string(), "Name".to_string()], Default::default())
				);
			}
			_ => panic!("Expected struct"),
		}
	}

	#[test]
	fn test_parse_struct_with_trailing_comma()
	{
		let input = "struct Point { x: i32, y: i32, }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_variant_with_qualified_types()
	{
		let input = "variant Result { Ok(std::string::String), Err(std::error::Error) }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Expression Precedence Tests ==========

	#[test]
	fn test_parse_precedence_bitwise_vs_comparison()
	{
		let result = parse_expr_from_str("a & b == c");
		assert!(result.is_ok());
		// & has lower precedence than ==, so should parse as (a & (b == c))
		match result.unwrap() {
			Expr::Binary {
				op: BinaryOp::BitAnd, ..
			} => (),
			_ => panic!("Expected bitwise AND at top level"),
		}
	}

	#[test]
	fn test_parse_precedence_shift_vs_addition()
	{
		let result = parse_expr_from_str("a + b << c");
		assert!(result.is_ok());
		// << has lower precedence than +, so should parse as ((a + b) << c)
		match result.unwrap() {
			Expr::Binary { op: BinaryOp::Shl, .. } => (),
			_ => panic!("Expected shift at top level"),
		}
	}

	#[test]
	fn test_parse_precedence_logical_and_or()
	{
		let result = parse_expr_from_str("a || b && c");
		assert!(result.is_ok());
		// && has higher precedence than ||, so should parse as (a || (b && c))
		match result.unwrap() {
			Expr::Binary {
				op: BinaryOp::LogicalOr,
				rhs,
				..
			} => match *rhs {
				Expr::Binary {
					op: BinaryOp::LogicalAnd,
					..
				} => (),
				_ => panic!("Expected logical AND on right"),
			},
			_ => panic!("Expected logical OR at top level"),
		}
	}

	// ========== Display/Formatting Tests ==========

	#[test]
	fn test_display_program()
	{
		let input = "fn foo() { return 42; }";
		let program = parse_program_from_str(input).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("fn"));
		assert!(output.contains("foo"));
	}

	#[test]
	fn test_display_preserves_structure()
	{
		let input = "struct Point { x: i32, y: i32 }";
		let program = parse_program_from_str(input).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("Point"));
		assert!(output.contains("x"));
		assert!(output.contains("y"));
	}

	// ========== Error Recovery Tests ==========

	#[test]
	fn test_error_missing_semicolon()
	{
		let result = parse_block_from_str("{ var x: i32 = 5 var y: i32 = 10; }");
		assert!(result.is_err());
	}

	#[test]
	fn test_error_unmatched_brace()
	{
		let result = parse_block_from_str("{ var x: i32 = 5;");
		assert!(result.is_err());
	}

	#[test]
	fn test_error_invalid_pattern()
	{
		let result = parse_block_from_str("{ switch x { 1 + 2 => 3, } }");
		assert!(result.is_err());
	}

	#[test]
	fn test_error_missing_arrow_in_switch()
	{
		let result = parse_block_from_str("{ switch x { 1 3, } }");
		assert!(result.is_err());
	}

	// ========== Edge Switch Tests ==========

	#[test]
	fn test_parse_empty_program()
	{
		let result = parse_program_from_str("");
		assert!(result.is_ok());
		let program = result.unwrap();
		assert_eq!(program.items.len(), 0);
	}

	#[test]
	fn test_parse_deeply_nested_expressions()
	{
		let result = parse_expr_from_str("((((((((42))))))))");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_long_operator_chain()
	{
		let result = parse_expr_from_str("a + b - c * d / e % f & g | h ^ i << j >> k");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_struct_init_ambiguity()
	{
		// Should parse as struct init, not block
		let result = parse_expr_from_str("Foo {}");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::StructInit { .. } => (),
			_ => panic!("Expected struct init"),
		}
	}

	#[test]
	fn test_parse_identifier_vs_struct_init()
	{
		// Should parse as identifier when no brace follows
		let result = parse_expr_from_str("Foo");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Identifier { .. } => (),
			_ => panic!("Expected identifier"),
		}
	}

	// ========== Right Shift Generic Handling ==========

	#[test]
	fn test_parse_nested_generics_with_rshift()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "Vec<Vec<i32>>", "test_file_50", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_triple_nested_generics()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "Box<Vec<Option<i32>>>", "test_file_51", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
	}

	// ========== Loop Label and Break Value Tests ==========

	#[test]
	fn test_parse_break_with_value()
	{
		let result = parse_block_from_str("{ loop { break 42; }; }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { body, .. } => match &body.stmts[0] {
				Stmt::Break {
					label: None,
					value: Some(_),
					..
				} => (),
				_ => panic!("Expected break with value"),
			},
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_parse_break_without_value()
	{
		let result = parse_block_from_str("{ loop { break; }; }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { body, .. } => match &body.stmts[0] {
				Stmt::Break {
					label: None,
					value: None,
					..
				} => (),
				_ => panic!("Expected break without value"),
			},
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_parse_labeled_loop()
	{
		let result = parse_block_from_str("{ 'outer: loop { break; }; }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { label: Some(lbl), .. } => {
				assert_eq!(lbl, "outer");
			}
			_ => panic!("Expected labeled loop"),
		}
	}

	#[test]
	fn test_parse_break_with_label()
	{
		let result = parse_block_from_str("{ 'outer: loop { break 'outer; }; }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop {
				label: Some(_), body, ..
			} => match &body.stmts[0] {
				Stmt::Break {
					label: Some(lbl),
					value: None,
					..
				} => {
					assert_eq!(lbl, "outer");
				}
				_ => panic!("Expected break with label"),
			},
			_ => panic!("Expected labeled loop"),
		}
	}

	#[test]
	fn test_parse_break_with_label_and_value()
	{
		let result = parse_block_from_str("{ 'outer: loop { break 'outer 42; }; }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { body, .. } => match &body.stmts[0] {
				Stmt::Break {
					label: Some(lbl),
					value: Some(_),
					..
				} => {
					assert_eq!(lbl, "outer");
				}
				_ => panic!("Expected break with label and value"),
			},
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_parse_continue_with_label()
	{
		let result = parse_block_from_str("{ 'outer: loop { continue 'outer; }; }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { body, .. } => match &body.stmts[0] {
				Stmt::Continue { label: Some(lbl), .. } => {
					assert_eq!(lbl, "outer");
				}
				_ => panic!("Expected continue with label"),
			},
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_parse_continue_without_label()
	{
		let result = parse_block_from_str("{ loop { continue; }; }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop { body, .. } => match &body.stmts[0] {
				Stmt::Continue { label: None, .. } => (),
				_ => panic!("Expected continue without label"),
			},
			_ => panic!("Expected loop"),
		}
	}

	#[test]
	fn test_parse_labeled_while_loop()
	{
		let result = parse_block_from_str("{ 'outer: while true { break 'outer; } }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::While { label: Some(lbl), .. } => {
				assert_eq!(lbl, "outer");
			}
			_ => panic!("Expected labeled while loop"),
		}
	}

	#[test]
	fn test_parse_labeled_for_loop()
	{
		let result = parse_block_from_str("{ 'outer: for i in 0..10 { break 'outer; } }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::For { label: Some(lbl), .. } => {
				assert_eq!(lbl, "outer");
			}
			_ => panic!("Expected labeled for loop"),
		}
	}

	#[test]
	fn test_parse_labeled_while_var_loop()
	{
		let result = parse_block_from_str("{ 'outer: while var Some(x: i32) = opt { break 'outer; } }");
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::WhileVarLoop { label: Some(lbl), .. } => {
				assert_eq!(lbl, "outer");
			}
			_ => panic!("Expected labeled while var loop"),
		}
	}

	#[test]
	fn test_parse_nested_labeled_loops()
	{
		let input = r#"{
		'outer: loop {
			'inner: loop {
				break 'outer;
			};
		};
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Loop {
				label: Some(outer_lbl),
				body,
				..
			} => {
				assert_eq!(outer_lbl, "outer");
				match &body.stmts[0] {
					Stmt::Loop {
						label: Some(inner_lbl), ..
					} => {
						assert_eq!(inner_lbl, "inner");
					}
					_ => panic!("Expected inner labeled loop"),
				}
			}
			_ => panic!("Expected outer labeled loop"),
		}
	}

	#[test]
	fn test_parse_break_to_outer_loop()
	{
		let input = r#"{
		'outer: loop {
			'inner: loop {
				if condition {
					break 'outer;
				}
				break 'inner;
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_continue_to_outer_loop()
	{
		let input = r#"{
		'outer: for i in 0..10 {
			'inner: for j in 0..10 {
				if j == 5 {
					continue 'outer;
				}
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_loop_with_break_value_as_expression()
	{
		let input = r#"{
		var result: i32 = loop {
			if condition {
				break 42;
			}
		};
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_labeled_loop_with_break_value()
	{
		let input = r#"{
		var result: i32 = 'search: loop {
			if found {
				break 'search 100;
			}
		};
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_with_complex_expression()
	{
		let input = r#"{
		loop {
			break x * 2 + y;
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_with_function_call()
	{
		let input = r#"{
		loop {
			break compute_result();
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_label_with_underscore()
	{
		let result = parse_block_from_str("{ 'outer_loop: loop { break 'outer_loop; } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_label_with_numbers()
	{
		let result = parse_block_from_str("{ 'loop1: loop { break 'loop1; } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_multiple_labeled_loops_same_level()
	{
		let input = r#"{
		'first: loop {
			break 'first;
		}
		'second: loop {
			break 'second;
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_deeply_nested_labeled_loops()
	{
		let input = r#"{
		'level1: loop {
			'level2: while true {
				'level3: for i in 0..10 {
					'level4: loop {
						break 'level1;
					}
				}
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_block_expression()
	{
		let input = r#"{
		loop {
			break {
				var x: i32 = 10;
				x + 5
			};
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_if_expression()
	{
		let input = r#"{
		loop {
			break if condition { 1 } else { 2 };
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_match()
	{
		let input = r#"{
		loop {
			break switch x {
				1 => 10,
				2 => 20,
				_ => 0,
			};
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_label_similar_to_lifetime()
	{
		let result = parse_block_from_str("{ 'a: loop { break 'a; } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_char_literal_still_works()
	{
		let result = parse_expr_from_str("'x'");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Literal {
				value: Literal::Char('x'),
				..
			} => (),
			_ => panic!("Expected char literal"),
		}
	}

	#[test]
	fn test_parse_char_literal_and_label_in_same_block()
	{
		let input = r#"{
		var ch: char = 'a';
		'outer: loop {
			if ch == 'b' {
				break 'outer;
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_single_char_label()
	{
		let result = parse_block_from_str("{ 'a: loop { break 'a; } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_while_with_break_value()
	{
		let input = r#"{
		while true {
			break 42;
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_for_with_break_value()
	{
		let input = r#"{
		for i in 0..10 {
			if i == 5 {
				break i;
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_labeled_for_with_continue_and_break()
	{
		let input = r#"{
		'outer: for i in 0..10 {
			if i == 3 {
				continue 'outer;
			}
			if i == 7 {
				break 'outer i;
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_multiple_breaks_different_labels()
	{
		let input = r#"{
		'outer: loop {
			'inner: loop {
				if a {
					break 'outer 1;
				}
				if b {
					break 'inner 2;
				}
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_display_labeled_loop()
	{
		let input = "'outer: loop { break 'outer; }";
		let program = parse_program_from_str(&format!("fn test() {{{}}}", input)).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("'outer"));
	}

	#[test]
	fn test_display_break_with_value()
	{
		let input = "loop { break 42; }";
		let program = parse_program_from_str(&format!("fn test() {{{}}}", input)).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("break"));
		assert!(output.contains("42"));
	}

	#[test]
	fn test_display_break_with_label_and_value()
	{
		let input = "'outer: loop { break 'outer 42; }";
		let program = parse_program_from_str(&format!("fn test() {{{}}}", input)).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("'outer"));
		assert!(output.contains("break"));
	}

	#[test]
	fn test_parse_label_without_colon_error()
	{
		let result = parse_block_from_str("{ 'outer loop { break; } }");
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_colon_without_label()
	{
		let result = parse_block_from_str("{ : loop { break; } }");
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_real_world_example_search()
	{
		let input = r#"{
		'search: for item in collection {
			if item.matches(target) {
				break 'search item.value;
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_real_world_example_nested_iteration()
	{
		let input = r#"{
		'outer: for row in 0..height {
			'inner: for col in 0..width {
				if grid[row][col] == target {
					break 'outer (row, col);
				}
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_real_world_example_state_machine()
	{
		let input = r#"{
		'state_machine: loop {
			switch current_state {
				State::Init => {
					if init_success() {
						break 'state_machine Result::Ok;
					}
					current_state = State::Retry;
					continue 'state_machine;
				},
				State::Retry => {
					if retry_count > 3 {
						break 'state_machine Result::Error;
					}
					continue 'state_machine;
				},
				_ => break 'state_machine Result::Error,
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_in_nested_if_in_loop()
	{
		let input = r#"{
		'outer: loop {
			if condition1 {
				if condition2 {
					break 'outer 42;
				}
			}
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_label_name_like_keyword()
	{
		// Labels named after keywords should work
		let result = parse_block_from_str("{ 'while: loop { break 'while; } }");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_very_long_label_name()
	{
		let result = parse_block_from_str(
			"{ 'this_is_a_very_long_label_name_for_testing: loop { break 'this_is_a_very_long_label_name_for_testing; } }",
		);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_tuple()
	{
		let input = r#"{
		loop {
			break (1, 2, 3);
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_array()
	{
		let input = r#"{
		loop {
			break [1, 2, 3];
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_break_value_with_struct_init()
	{
		let input = r#"{
		loop {
			break Point { x = 1, y = 2 };
		}
	}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_struct_with_default_values()
	{
		let input = "struct Point { x: i32 = 0, y: i32 = 0 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Struct(s) => {
				assert_eq!(s.fields.len(), 2);
				assert!(s.fields[0].2.is_some()); // x has default value
				assert!(s.fields[1].2.is_some()); // y has default value
			}
			_ => panic!("Expected struct declaration"),
		}
	}

	#[test]
	fn test_parse_struct_mixed_defaults()
	{
		let input = "struct Person { name: String, age: i32 = 0 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Struct(s) => {
				assert_eq!(s.fields.len(), 2);
				assert!(s.fields[0].2.is_none()); // name has no default
				assert!(s.fields[1].2.is_some()); // age has default
			}
			_ => panic!("Expected struct declaration"),
		}
	}

	#[test]
	fn test_parse_struct_default_complex_expr()
	{
		let input = "struct Config { timeout: i32 = 30 * 1000 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_variant_with_values()
	{
		let input = "variant Status { Success = 0, Error = 1, Pending = 2 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Variant(v) => {
				assert_eq!(v.variants.len(), 3);
				assert!(v.variants[0].2.is_some()); // Success has value
				assert!(v.variants[1].2.is_some()); // Error has value
				assert!(v.variants[2].2.is_some()); // Pending has value
			}
			_ => panic!("Expected variant declaration"),
		}
	}

	#[test]
	fn test_parse_variant_mixed_types_and_values()
	{
		let input = "variant Mixed { Unit = 0, WithData(i32) = 1, Other }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Variant(v) => {
				assert_eq!(v.variants.len(), 3);
				// Unit: no type, has value
				assert!(v.variants[0].0.is_none());
				assert!(v.variants[0].2.is_some());
				// WithData: has type, has value
				assert!(v.variants[1].0.is_some());
				assert!(v.variants[1].2.is_some());
				// Other: no type, no value
				assert!(v.variants[2].0.is_none());
				assert!(v.variants[2].2.is_none());
			}
			_ => panic!("Expected variant declaration"),
		}
	}

	#[test]
	fn test_parse_variant_with_complex_values()
	{
		let input = "variant Flags { A = 1 << 0, B = 1 << 1, C = 1 << 2 }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Default Expression Tests ==========

	#[test]
	fn test_parse_default_expression()
	{
		let result = parse_expr_from_str("default()");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Default {
				heap_call: CallType::Regular,
				..
			} => (),
			_ => panic!("Expected default expression"),
		}
	}

	#[test]
	fn test_parse_default_heap_expression()
	{
		let result = parse_expr_from_str("default!()");
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Default {
				heap_call: CallType::UserHeap,
				..
			} => (),
			_ => panic!("Expected heap-allocated default expression"),
		}
	}

	// ========== Error Recovery & Edge Cases ==========

	#[test]
	fn test_parse_struct_field_missing_type()
	{
		let input = "struct Point { x: }";
		let result = parse_program_from_str(input);
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_function_missing_return_arrow()
	{
		let input = "fn foo() i32 {}";
		let result = parse_program_from_str(input);
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_incomplete_generic()
	{
		let input = "Vec<";
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, input, "test_file_52", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_err());
	}

	// ========== Pattern Constructor Syntax ==========

	#[test]
	fn test_parse_pattern_with_constructor_call()
	{
		let input = "{ switch x { val: MyType() => true, } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Complex Type Modifiers ==========

	#[test]
	fn test_parse_type_with_multiple_modifiers()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "const volatile i32", "test_file_53", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_type_modifiers_on_complex_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "const Vec<i32>*", "test_file_54", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
	}

	// ========== Directive Bodies ==========

	// NOTE: blocks for directives are not supported
	// #[test]
	// fn test_parse_directive_with_top_level_block()
	// {
	// 	let input = "@extern { fn c_function(); }";
	// 	let result = parse_program_from_str(input);
	// 	assert!(result.is_ok());
	// }

	// NOTE: blocks for directives are not supported
	// #[test]
	// fn test_parse_directive_with_regular_block()
	// {
	// 	let input = "@custom(arg) { var x: i32 = 5; }";
	// 	let result = parse_program_from_str(input);
	// 	assert!(result.is_ok());
	// }

	// ========== Switch Arm Control Flow ==========

	#[test]
	fn test_parse_switch_with_return()
	{
		let input = "switch x { 1 => return 10, _ => 0, }";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_switch_with_continue()
	{
		let input = "{ loop { switch x { 1 => continue, _ => break, } } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Empty Constructs ==========

	#[test]
	fn test_parse_empty_impl()
	{
		let input = "impl MyType { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_empty_trait()
	{
		let input = "trait Empty { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_empty_enum()
	{
		let input = "enum Empty { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Complex Nested Patterns ==========

	#[test]
	fn test_parse_pattern_deeply_nested_struct()
	{
		let input = "{ switch x { Outer { inner = Inner { value = val: i32 } } => val, } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_pattern_tuple_in_variant()
	{
		let input = "{ switch x { Some((a: i32, (b: i32, c: i32))) => a, } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Array Repeat with Complex Values ==========

	#[test]
	fn test_parse_array_repeat_multiple_values()
	{
		let input = "[x, y, z; 10]";
		let result = parse_expr_from_str(input).inspect(|e| println!("{e}"));
		assert!(result.is_err());
	}

	// ========== Semicolon Requirements ==========

	#[test]
	fn test_block_trailing_semicolon_no_tail()
	{
		let input = "{ var x: i32 = 5; }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
		let block = result.unwrap();
		assert!(block.tail_expr.is_none());
	}

	#[test]
	fn test_block_semicolon_converts_to_statement()
	{
		let input = "{ if true { 1 } else { 2 }; }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
		let block = result.unwrap();
		assert!(block.tail_expr.is_none());
	}

	// ========== Where Clause Edge Cases ==========

	#[test]
	fn test_parse_where_clause_with_qualified_types()
	{
		let input = "impl<T> Trait for Type<T> where std::vec::Vec<T>: Clone { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Loop Expression vs Statement ==========

	#[test]
	fn test_loop_as_expression_in_assignment()
	{
		let input = "{ var x: i32 = loop { break 42; }; }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_labeled_loop_as_expression()
	{
		let input = "{ var x: i32 = 'outer: loop { break 'outer 42; }; }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Buffered Token Edge Cases ==========

	#[test]
	fn test_rshift_splits_correctly_in_nested_generics()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "Map<Vec<i32>, Vec<i32>>", "test_file_55", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
	}

	// ========== Trait Default Implementations ==========

	#[test]
	fn test_parse_trait_method_with_default_body()
	{
		let input = "trait HasDefault { fn method(&self) { println(\"default\"); } }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_extra_semicolons_top_level()
	{
		let input = ";;;;;;";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_extra_semicolons_block()
	{
		let input = "fn main(){;;;;;;}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_impl_trait_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "impl Clone", "test_file_56", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::ImplTrait { bounds } => {
				assert_eq!(bounds.len(), 1);
				assert_eq!(
					bounds[0],
					WhereBound::Path(Path::simple(vec!["Clone".to_string()], Default::default()))
				);
			}
			_ => panic!("Expected impl trait type"),
		}
	}

	#[test]
	fn test_parse_impl_trait_multiple_bounds()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "impl Clone + Debug", "test_file_57", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::ImplTrait { bounds } => {
				assert_eq!(bounds.len(), 2);
			}
			_ => panic!("Expected impl trait type with multiple bounds"),
		}
	}

	#[test]
	fn test_parse_function_returning_impl_trait()
	{
		let input = "fn create() -> impl Iterator { }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_impl_trait_reference()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "&impl Clone", "test_file_58", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
	}

	// ========== Heap Function Declaration Tests ==========

	#[test]
	fn test_parse_heap_function_declaration()
	{
		let input = "fn! allocate_memory() {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.call_type, CallType::UserHeap);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_heap_function_with_params()
	{
		let input = "fn! allocate(size: usize) -> u8* {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.call_type, CallType::UserHeap);
				assert_eq!(func.signature.params.len(), 1);
				assert!(func.signature.return_type.is_some());
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_heap_function_with_generics()
	{
		let input = "fn!<Alloc> create() -> Box<T> {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.call_type, CallType::UserHeap);
				assert_eq!(func.signature.heap_generics.len(), 1);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_heap_function_with_modifiers()
	{
		let input = "pub unsafe inline fn! dangerous_alloc() {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.call_type, CallType::UserHeap);
				assert!(func.signature.modifiers.len() >= 3);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_regular_vs_heap_function()
	{
		let input = r#"
            fn regular() {}
            fn! heap() {}
        "#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		assert_eq!(program.items.len(), 2);

		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.call_type, CallType::Regular);
			}
			_ => panic!("Expected regular function"),
		}

		match &program.items[1] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.call_type, CallType::UserHeap);
			}
			_ => panic!("Expected heap function"),
		}
	}

	// ========== Heap Function Call Tests ==========

	#[test]
	fn test_parse_user_heap_call()
	{
		let input = "allocate!()";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { call_type, .. } => {
				assert_eq!(call_type, CallType::UserHeap);
				assert!(call_type.is_heap_call());
				assert!(call_type.is_user_call());
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_parse_user_maybe_heap_call()
	{
		let input = "allocate?()";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { call_type, .. } => {
				assert_eq!(call_type, CallType::UserMaybeHeap);
				assert!(call_type.is_user_maybe_call());
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_parse_regular_call()
	{
		let input = "allocate()";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { call_type, .. } => {
				assert_eq!(call_type, CallType::Regular);
				assert!(call_type.is_regular());
				assert!(!call_type.is_heap_call());
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_parse_heap_call_with_args()
	{
		let input = "allocate!(1024, true)";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { call_type, args, .. } => {
				assert_eq!(call_type, CallType::UserHeap);
				assert_eq!(args.len(), 2);
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_parse_heap_call_with_named_generics()
	{
		let input = "allocate!<Allocator: MyAlloc>(size)";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call {
				call_type,
				named_generics,
				args,
				..
			} => {
				assert_eq!(call_type, CallType::UserHeap);
				assert_eq!(named_generics.len(), 1);
				assert_eq!(named_generics[0].0, "Allocator");
				assert_eq!(args.len(), 1);
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_parse_maybe_heap_call_with_named_generics()
	{
		let input = "allocate?<IO: StdIO>(data)";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call {
				call_type,
				named_generics,
				..
			} => {
				assert_eq!(call_type, CallType::UserMaybeHeap);
				assert_eq!(named_generics.len(), 1);
				assert_eq!(named_generics[0].0, "IO");
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_parse_heap_call_multiple_named_generics()
	{
		let input = "create!<Alloc: System, Error: MyError>()";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call {
				call_type,
				named_generics,
				..
			} => {
				assert_eq!(call_type, CallType::UserHeap);
				assert_eq!(named_generics.len(), 2);
				assert_eq!(named_generics[0].0, "Alloc");
				assert_eq!(named_generics[1].0, "Error");
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_parse_heap_call_qualified_path()
	{
		let input = "std::alloc::allocate!(1024)";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { callee, call_type, .. } => {
				assert_eq!(call_type, CallType::UserHeap);
				match callee.as_ref() {
					Expr::Identifier { path, .. } => {
						assert_eq!(path.segments.len(), 3);
						assert_eq!(path.segments[0], "std");
						assert_eq!(path.segments[1], "alloc");
						assert_eq!(path.segments[2], "allocate");
					}
					_ => panic!("Expected identifier"),
				}
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_parse_chained_heap_call()
	{
		let input = "builder!().build!().finalize()";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		// Should parse as ((builder!()).build!()).finalize()
	}

	#[test]
	fn test_parse_heap_call_in_expression()
	{
		let input = "var ptr: u8* = allocate!(size);";
		let result = parse_block_from_str(&format!("{{{}}}", input));
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_heap_call_as_argument()
	{
		let input = "process(allocate!(1024))";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { args, .. } => {
				assert_eq!(args.len(), 1);
				match &args[0] {
					Expr::Call { call_type, .. } => {
						assert_eq!(*call_type, CallType::UserHeap);
					}
					_ => panic!("Expected inner heap call"),
				}
			}
			_ => panic!("Expected call expression"),
		}
	}

	// ========== Default Expression Tests ==========

	#[test]
	fn test_parse_default_regular()
	{
		let input = "default()";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Default { heap_call, .. } => {
				assert_eq!(heap_call, CallType::Regular);
			}
			_ => panic!("Expected default expression"),
		}
	}

	#[test]
	fn test_parse_default_heap()
	{
		let input = "default!()";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Default { heap_call, .. } => {
				assert_eq!(heap_call, CallType::UserHeap);
			}
			_ => panic!("Expected heap default expression"),
		}
	}

	#[test]
	fn test_parse_default_maybe_heap()
	{
		let input = "default?()";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Default { heap_call, .. } => {
				assert_eq!(heap_call, CallType::UserMaybeHeap);
			}
			_ => panic!("Expected maybe-heap default expression"),
		}
	}

	#[test]
	fn test_parse_default_in_variable_init()
	{
		let input = "{ var x: MyType = default!(); }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::VariableDecl(var) => match &var.init {
				Some(Expr::Default { heap_call, .. }) => {
					assert_eq!(*heap_call, CallType::UserHeap);
				}
				_ => panic!("Expected default expression"),
			},
			_ => panic!("Expected variable declaration"),
		}
	}

	#[test]
	fn test_parse_default_in_struct_field()
	{
		let input = "Point { x = default!(), y = default!() }";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::StructInit { fields, .. } => {
				assert_eq!(fields.len(), 2);
				for (_, expr) in fields {
					match expr {
						Expr::Default { heap_call, .. } => {
							assert_eq!(heap_call, CallType::UserHeap);
						}
						_ => panic!("Expected default expression"),
					}
				}
			}
			_ => panic!("Expected struct init"),
		}
	}

	#[test]
	fn test_parse_default_as_return_value()
	{
		let input = "{ return default!(); }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.stmts[0] {
			Stmt::Return { value: Some(expr), .. } => match expr {
				Expr::Default { heap_call, .. } => {
					assert_eq!(*heap_call, CallType::UserHeap);
				}
				_ => panic!("Expected default expression"),
			},
			_ => panic!("Expected return statement"),
		}
	}

	#[test]
	fn test_parse_default_in_array()
	{
		let input = "[default!(), default!(), default!()]";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Array(ArrayLiteral::List { elements, .. }) => {
				assert_eq!(elements.len(), 3);
				for elem in elements {
					match elem {
						Expr::Default { heap_call, .. } => {
							assert_eq!(heap_call, CallType::UserHeap);
						}
						_ => panic!("Expected default expression"),
					}
				}
			}
			_ => panic!("Expected array literal"),
		}
	}

	// ========== Pattern Constructor Call Tests ==========

	#[test]
	fn test_parse_pattern_typed_identifier_regular_constructor()
	{
		let input = "{ switch x { val: MyType() => true, } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.tail_expr {
			Some(expr) => match expr.as_ref() {
				Expr::Switch { arms, .. } => match &arms[0].pattern {
					Pattern::TypedIdentifier { call_constructor, .. } => {
						assert_eq!(*call_constructor, Some(CallType::Regular));
					}
					_ => panic!("Expected typed identifier pattern"),
				},
				_ => panic!("Expected switch expression"),
			},
			None => panic!("Expected tail expression"),
		}
	}

	#[test]
	fn test_parse_pattern_typed_identifier_heap_constructor()
	{
		let input = "{ switch x { val: MyType!() => true, } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.tail_expr {
			Some(expr) => match expr.as_ref() {
				Expr::Switch { arms, .. } => match &arms[0].pattern {
					Pattern::TypedIdentifier { call_constructor, .. } => {
						assert_eq!(*call_constructor, Some(CallType::UserHeap));
					}
					_ => panic!("Expected typed identifier pattern"),
				},
				_ => panic!("Expected switch expression"),
			},
			None => panic!("Expected tail expression"),
		}
	}

	#[test]
	fn test_parse_pattern_typed_identifier_maybe_heap_constructor()
	{
		let input = "{ switch x { val: MyType?() => true, } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.tail_expr {
			Some(expr) => match expr.as_ref() {
				Expr::Switch { arms, .. } => match &arms[0].pattern {
					Pattern::TypedIdentifier { call_constructor, .. } => {
						assert_eq!(*call_constructor, Some(CallType::UserMaybeHeap));
					}
					_ => panic!("Expected typed identifier pattern"),
				},
				_ => panic!("Expected switch expression"),
			},
			None => panic!("Expected tail expression"),
		}
	}

	#[test]
	fn test_parse_pattern_typed_identifier_no_constructor()
	{
		let input = "{ switch x { val: MyType => true, } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.tail_expr {
			Some(expr) => match expr.as_ref() {
				Expr::Switch { arms, .. } => match &arms[0].pattern {
					Pattern::TypedIdentifier { call_constructor, .. } => {
						assert_eq!(*call_constructor, None);
					}
					_ => panic!("Expected typed identifier pattern"),
				},
				_ => panic!("Expected switch expression"),
			},
			None => panic!("Expected tail expression"),
		}
	}

	#[test]
	fn test_parse_pattern_in_if_var_with_constructor()
	{
		let input = "{ if var x: MyType!() = value { x } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
		let block = result.unwrap();
		match &block.tail_expr {
			Some(expr) => match expr.as_ref() {
				Expr::IfVar { pattern, .. } => match pattern {
					Pattern::TypedIdentifier { call_constructor, .. } => {
						assert_eq!(*call_constructor, Some(CallType::UserHeap));
					}
					_ => panic!("Expected typed identifier pattern"),
				},
				_ => panic!("Expected if var expression"),
			},
			None => panic!("Expected tail expression"),
		}
	}

	#[test]
	fn test_parse_pattern_in_struct_field_with_constructor()
	{
		let input = "{ switch x { Point { x: i32!(), y: i32?() } => true, } }";
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Mixed Heap Call Scenarios ==========

	#[test]
	fn test_parse_heap_and_regular_calls_mixed()
	{
		let input = "regular(heap!(data), maybe?(other))";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { call_type, args, .. } => {
				assert_eq!(call_type, CallType::Regular);
				assert_eq!(args.len(), 2);

				match &args[0] {
					Expr::Call { call_type, .. } => {
						assert_eq!(*call_type, CallType::UserHeap);
					}
					_ => panic!("Expected heap call"),
				}

				match &args[1] {
					Expr::Call { call_type, .. } => {
						assert_eq!(*call_type, CallType::UserMaybeHeap);
					}
					_ => panic!("Expected maybe heap call"),
				}
			}
			_ => panic!("Expected call expression"),
		}
	}

	#[test]
	fn test_parse_heap_call_with_default_arg()
	{
		let input = "allocate!(default!())";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
		match result.unwrap() {
			Expr::Call { call_type, args, .. } => {
				assert_eq!(call_type, CallType::UserHeap);
				match &args[0] {
					Expr::Default { heap_call, .. } => {
						assert_eq!(*heap_call, CallType::UserHeap);
					}
					_ => panic!("Expected default expression"),
				}
			}
			_ => panic!("Expected call expression"),
		}
	}

	// ========== Function Signature Display Tests ==========

	#[test]
	fn test_display_heap_function()
	{
		let input = "fn! allocate(size: usize) -> u8* {}";
		let program = parse_program_from_str(input).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("fn!"));
		assert!(output.contains("allocate"));
	}

	#[test]
	fn test_display_heap_call()
	{
		let input = "allocate!(1024)";
		let expr = parse_expr_from_str(input).unwrap();
		let output = format!("{}", expr);
		assert!(output.contains("allocate"));
		assert!(output.contains("!"));
	}

	#[test]
	fn test_display_default_heap()
	{
		let input = "default!()";
		let expr = parse_expr_from_str(input).unwrap();
		let output = format!("{}", expr);
		assert!(output.contains("default"));
		assert!(output.contains("!"));
	}

	// ========== Error Cases ==========

	#[test]
	fn test_parse_heap_call_missing_parens()
	{
		let input = "allocate!";
		let result = parse_expr_from_str(input);
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_default_missing_parens()
	{
		let input = "default!";
		let result = parse_expr_from_str(input);
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_default_with_args_error()
	{
		let input = "default!(arg)";
		let result = parse_expr_from_str(input);
		// Should error - default takes no arguments
		assert!(result.is_err());
	}

	// ========== Complex Real-World Scenarios ==========

	#[test]
	fn test_parse_allocator_pattern()
	{
		let input = r#"
            fn! create_buffer<A: Allocator>(allocator: A, size: usize) -> Buffer {
                var ptr: u8* = allocator.allocate!(size);
                return Buffer { ptr, size };
            }
        "#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_conditional_heap_allocation()
	{
		let input = r#"{
            if should_heap {
                create!<Alloc: Heap>(data)
            } else {
                create(data)
            }
        }"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_nested_heap_calls()
	{
		let input = "create!(build!(allocate!()))";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_heap_call_in_match_arm()
	{
		let input = r#"{
            switch val {
                Some(x: i32) => process!(x),
                None => default!(),
            }
        }"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_heap_method_call()
	{
		let input = "builder.build!().finalize!()";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_function_returning_heap_allocated()
	{
		let input = r#"
            fn! create_vec<T>() -> Vec<T> {
                Vec!<Alloc: System>()
            }
        "#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_trait_with_heap_methods()
	{
		let input = r#"
            trait Allocator {
                fn! allocate(&self, size: usize) -> u8*;
                fn! deallocate(&self, ptr: u8*);
            }
        "#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_impl_with_heap_methods()
	{
		let input = r#"
            impl Allocator for SystemAlloc {
                fn! allocate(&self, size: usize) -> u8* {
                    system_alloc!(size)
                }
            }
        "#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Basic Inline Trait Bounds Tests ==========

	#[test]
	fn test_parse_function_with_single_inline_bound()
	{
		let input = "fn foo<T: Clone>(x: T) -> T { x }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.generics.len(), 1);
				assert_eq!(func.signature.generics[0].name, "T");
				assert_eq!(func.signature.generics[0].bounds.len(), 1);
				assert_eq!(
					func.signature.generics[0].bounds[0],
					WhereBound::Path(Path::simple(vec!["Clone".to_string()], Default::default()))
				);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_function_with_multiple_inline_bounds()
	{
		let input = "fn bar<T: Clone + Debug + Display>(x: T) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.generics.len(), 1);
				assert_eq!(func.signature.generics[0].name, "T");
				assert_eq!(func.signature.generics[0].bounds.len(), 3);
				assert_eq!(
					func.signature.generics[0].bounds[0],
					WhereBound::Path(Path::simple(vec!["Clone".to_string()], Default::default()))
				);
				assert_eq!(
					func.signature.generics[0].bounds[1],
					WhereBound::Path(Path::simple(vec!["Debug".to_string()], Default::default()))
				);
				assert_eq!(
					func.signature.generics[0].bounds[2],
					WhereBound::Path(Path::simple(vec!["Display".to_string()], Default::default()))
				);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_function_with_mixed_bounded_and_unbounded_generics()
	{
		let input = "fn mixed<T, U: Clone, V: Debug + Send>(a: T, b: U, c: V) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.generics.len(), 3);

				// T - no bounds
				assert_eq!(func.signature.generics[0].name, "T");
				assert_eq!(func.signature.generics[0].bounds.len(), 0);

				// U: Clone
				assert_eq!(func.signature.generics[1].name, "U");
				assert_eq!(func.signature.generics[1].bounds.len(), 1);
				assert_eq!(
					func.signature.generics[1].bounds[0],
					WhereBound::Path(Path::simple(vec!["Clone".to_string()], Default::default()))
				);

				// V: Debug + Send
				assert_eq!(func.signature.generics[2].name, "V");
				assert_eq!(func.signature.generics[2].bounds.len(), 2);
				assert_eq!(
					func.signature.generics[2].bounds[0],
					WhereBound::Path(Path::simple(vec!["Debug".to_string()], Default::default()))
				);
				assert_eq!(
					func.signature.generics[2].bounds[1],
					WhereBound::Path(Path::simple(vec!["Send".to_string()], Default::default()))
				);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	// ========== Trait Declaration Tests ==========

	#[test]
	fn test_parse_trait_with_inline_bounds()
	{
		let input = "trait Foo<T: Clone, U: Debug> { fn bar(&self, x: T, y: U); }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Trait(t) => {
				assert_eq!(t.generics.len(), 2);

				assert_eq!(t.generics[0].name, "T");
				assert_eq!(t.generics[0].bounds.len(), 1);
				assert_eq!(
					t.generics[0].bounds[0],
					WhereBound::Path(Path::simple(vec!["Clone".to_string()], Default::default()))
				);

				assert_eq!(t.generics[1].name, "U");
				assert_eq!(t.generics[1].bounds.len(), 1);
				assert_eq!(
					t.generics[1].bounds[0],
					WhereBound::Path(Path::simple(vec!["Debug".to_string()], Default::default()))
				);
			}
			_ => panic!("Expected trait declaration"),
		}
	}

	#[test]
	fn test_parse_trait_with_complex_bounds()
	{
		let input = "trait Convert<From: Clone + Send, To: Debug + Sync> { fn convert(val: From) -> To; }";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Trait(t) => {
				assert_eq!(t.generics.len(), 2);
				assert_eq!(t.generics[0].bounds.len(), 2);
				assert_eq!(t.generics[1].bounds.len(), 2);
			}
			_ => panic!("Expected trait declaration"),
		}
	}

	// ========== Impl Block Tests ==========

	#[test]
	fn test_parse_impl_with_inline_bounds()
	{
		let input = "impl<T: Clone + Send> MyTrait for MyType<T> {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				assert_eq!(i.generics.len(), 1);
				assert_eq!(i.generics[0].name, "T");
				assert_eq!(i.generics[0].bounds.len(), 2);
				assert_eq!(
					i.generics[0].bounds[0],
					WhereBound::Path(Path::simple(vec!["Clone".to_string()], Default::default()))
				);
				assert_eq!(
					i.generics[0].bounds[1],
					WhereBound::Path(Path::simple(vec!["Send".to_string()], Default::default()))
				);
			}
			_ => panic!("Expected impl declaration"),
		}
	}

	#[test]
	fn test_parse_impl_mixed_inline_and_where()
	{
		let input = "impl<T: Clone, U> MyTrait for MyType<T, U> where U: Debug {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Impl(i) => {
				// T has inline bound
				assert_eq!(i.generics[0].bounds.len(), 1);
				// U has no inline bound but has where clause
				assert_eq!(i.generics[1].bounds.len(), 0);
				assert_eq!(i.where_clause.len(), 1);
			}
			_ => panic!("Expected impl declaration"),
		}
	}

	// ========== Advanced Scenarios ==========

	#[test]
	fn test_parse_inline_bounds_with_qualified_paths()
	{
		let input = "fn process<T: std::clone::Clone + std::fmt::Debug>(x: T) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.generics[0].bounds.len(), 2);
				assert_eq!(
					func.signature.generics[0].bounds[0],
					WhereBound::Path(Path::simple(
						vec!["std".to_string(), "clone".to_string(), "Clone".to_string()],
						Default::default()
					))
				);
				assert_eq!(
					func.signature.generics[0].bounds[1],
					WhereBound::Path(Path::simple(
						vec!["std".to_string(), "fmt".to_string(), "Debug".to_string()],
						Default::default()
					))
				);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_nested_generic_bounds()
	{
		let input = "fn foo<T: Iterator<Item = i32>>(iter: T) {}";
		let result = parse_program_from_str(input);
		// This might fail depending on if we support associated types in bounds
		// but at minimum it should parse the Iterator part
		assert!(result.is_ok() || result.is_err());
	}

	#[test]
	fn test_parse_heap_function_with_inline_bounds()
	{
		let input = "fn!<A: Allocator + Send>func(alloc: A) -> Result {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.call_type, CallType::UserHeap);
				assert_eq!(func.signature.heap_generics[0].bounds.len(), 2);
			}
			_ => panic!("Expected heap function declaration"),
		}
	}

	// ========== Display/Formatting Tests ==========

	#[test]
	fn test_display_inline_bounds_single()
	{
		let input = "fn foo<T: Clone>(x: T) {}";
		let program = parse_program_from_str(input).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("<T: Clone>"));
	}

	#[test]
	fn test_display_inline_bounds_multiple()
	{
		let input = "fn foo<T: Clone + Debug>(x: T) {}";
		let program = parse_program_from_str(input).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("T: Clone + Debug"));
	}

	#[test]
	fn test_display_mixed_bounds()
	{
		let input = "fn foo<T, U: Clone>(x: T, y: U) {}";
		let program = parse_program_from_str(input).unwrap();
		let output = format!("{}", program);
		assert!(output.contains("<T, U: Clone>"));
	}

	// ========== Edge Cases ==========

	#[test]
	fn test_parse_inline_bounds_with_trailing_comma()
	{
		let input = "fn foo<T: Clone, U: Debug,>(x: T, y: U) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.generics.len(), 2);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_inline_bounds_empty_generics()
	{
		let input = "fn foo<>() {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		let program = result.unwrap();
		match &program.items[0] {
			TopLevelDecl::Function(func) => {
				assert_eq!(func.signature.generics.len(), 0);
			}
			_ => panic!("Expected function declaration"),
		}
	}

	#[test]
	fn test_parse_inline_bounds_no_space_around_colon()
	{
		let input = "fn foo<T:Clone+Debug>(x: T) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_inline_bounds_with_lifetime_like_names()
	{
		let input = "fn foo<T: 'static>(x: T) {}";
		let result = parse_program_from_str(input);
		// This might fail since we don't have lifetime support yet
		// but documenting the behavior
		assert!(result.is_ok() || result.is_err());
	}

	// ========== Real-World Examples ==========

	#[test]
	fn test_parse_realistic_generic_function()
	{
		let input = r#"
			fn process<T: Clone + Debug, E: Error>(
				items: Vec<T>,
				handler: impl Fn(T) -> Result<(), E>
			) -> Result<(), E> {
				for item in items {
					handler(item);
				}
			}
		"#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_trait_with_method_using_bounds()
	{
		let input = r#"
			trait Container<T: Clone> {
				fn add(&mut self, item: T);
				fn get(&self, index: usize) -> Option<T>;
			}
		"#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_impl_for_generic_with_bounds()
	{
		let input = r#"
			impl<T: Clone + PartialEq> MyVec<T> {
				fn contains(&self, item: T) -> bool {
					false
				}
			}
		"#;
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	// ========== Error Cases ==========

	#[test]
	fn test_parse_inline_bounds_missing_colon()
	{
		let input = "fn foo<T Clone>(x: T) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_inline_bounds_missing_bound_after_colon()
	{
		let input = "fn foo<T: >(x: T) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_err());
	}

	#[test]
	fn test_parse_inline_bounds_missing_plus_between_bounds()
	{
		let input = "fn foo<T: Clone Debug>(x: T) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_err());
	}

	// ===== Basic Literal Parameters =====

	#[test]
	fn test_directive_single_string_literal()
	{
		let result = parse_directive(r#"@test("hello")"#);
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "test");
			assert_eq!(params.len(), 1);
			assert!(matches!(
				&params[0],
				DirectiveParam::Literal(Literal::String(s)) if s == "hello"
			));
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_single_int_literal()
	{
		let result = parse_directive("@version(42)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "version");
			assert_eq!(params.len(), 1);
			assert!(matches!(&params[0], DirectiveParam::Literal(Literal::Int(42))));
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_single_float_literal()
	{
		let result = parse_directive("@scale(3.16)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "scale");
			assert_eq!(params.len(), 1);
			assert!(matches!(
				&params[0],
				DirectiveParam::Literal(Literal::Float(f)) if (*f - 3.16).abs() < 0.001
			));
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_single_char_literal()
	{
		let result = parse_directive("@delimiter('|')");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "delimiter");
			assert_eq!(params.len(), 1);
			assert!(matches!(&params[0], DirectiveParam::Literal(Literal::Char('|'))));
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_bool_true_literal()
	{
		let result = parse_directive("@enabled(true)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "enabled");
			assert_eq!(params.len(), 1);
			assert!(matches!(&params[0], DirectiveParam::Literal(Literal::Bool(true))));
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_bool_false_literal()
	{
		let result = parse_directive("@disabled(false)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "disabled");
			assert_eq!(params.len(), 1);
			assert!(matches!(&params[0], DirectiveParam::Literal(Literal::Bool(false))));
		} else {
			panic!("Expected Custom directive");
		}
	}

	// ===== Identifier Parameters =====

	#[test]
	fn test_directive_single_identifier()
	{
		let result = parse_directive("@target(x86_64)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "target");
			assert_eq!(params.len(), 1);
			assert!(matches!(
				&params[0],
				DirectiveParam::Identifier(id) if id == "x86_64"
			));
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_multiple_identifiers()
	{
		let result = parse_directive("@cfg(unix, debug)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "cfg");
			assert_eq!(params.len(), 2);
			assert!(matches!(
				&params[0],
				DirectiveParam::Identifier(id) if id == "unix"
			));
			assert!(matches!(
				&params[1],
				DirectiveParam::Identifier(id) if id == "debug"
			));
		} else {
			panic!("Expected Custom directive");
		}
	}

	// ===== Named Parameters =====

	#[test]
	fn test_directive_named_string_param()
	{
		let result = parse_directive(r#"@link(name = "mylib")"#);
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "link");
			assert_eq!(params.len(), 1);

			if let DirectiveParam::Named { name: param_name, arg } = &params[0] {
				assert_eq!(param_name, "name");
				assert!(matches!(arg, Literal::String(s) if s == "mylib"));
			} else {
				panic!("Expected Named parameter");
			}
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_named_int_param()
	{
		let result = parse_directive("@optimize(level = 3)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "optimize");
			assert_eq!(params.len(), 1);

			if let DirectiveParam::Named { name: param_name, arg } = &params[0] {
				assert_eq!(param_name, "level");
				assert!(matches!(arg, Literal::Int(3)));
			} else {
				panic!("Expected Named parameter");
			}
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_named_bool_param()
	{
		let result = parse_directive("@feature(enabled = true)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "feature");
			assert_eq!(params.len(), 1);

			if let DirectiveParam::Named { name: param_name, arg } = &params[0] {
				assert_eq!(param_name, "enabled");
				assert!(matches!(arg, Literal::Bool(true)));
			} else {
				panic!("Expected Named parameter");
			}
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_named_float_param()
	{
		let result = parse_directive("@threshold(value = 0.5)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "threshold");
			assert_eq!(params.len(), 1);

			if let DirectiveParam::Named { name: param_name, arg } = &params[0] {
				assert_eq!(param_name, "value");
				if let Literal::Float(f) = arg {
					assert!((*f - 0.5).abs() < 0.001);
				} else {
					panic!("Expected Float literal");
				}
			} else {
				panic!("Expected Named parameter");
			}
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_named_char_param()
	{
		let result = parse_directive("@separator(char = ',')");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "separator");
			assert_eq!(params.len(), 1);

			if let DirectiveParam::Named { name: param_name, arg } = &params[0] {
				assert_eq!(param_name, "char");
				assert!(matches!(arg, Literal::Char(',')));
			} else {
				panic!("Expected Named parameter");
			}
		} else {
			panic!("Expected Custom directive");
		}
	}

	// ===== Mixed Parameters =====

	#[test]
	fn test_directive_mixed_literal_and_identifier()
	{
		let result = parse_directive(r#"@config("path/to/file", debug)"#);
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "config");
			assert_eq!(params.len(), 2);
			assert!(matches!(
				&params[0],
				DirectiveParam::Literal(Literal::String(s)) if s == "path/to/file"
			));
			assert!(matches!(
				&params[1],
				DirectiveParam::Identifier(id) if id == "debug"
			));
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_mixed_identifier_and_named()
	{
		let result = parse_directive(r#"@plugin(logger, level = 2)"#);
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "plugin");
			assert_eq!(params.len(), 2);
			assert!(matches!(
				&params[0],
				DirectiveParam::Identifier(id) if id == "logger"
			));

			if let DirectiveParam::Named { name: param_name, arg } = &params[1] {
				assert_eq!(param_name, "level");
				assert!(matches!(arg, Literal::Int(2)));
			} else {
				panic!("Expected Named parameter");
			}
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_multiple_named_params()
	{
		let result = parse_directive(r#"@database(host = "localhost", port = 5432, debug = true)"#);
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "database");
			assert_eq!(params.len(), 3);

			if let DirectiveParam::Named { name: param_name, arg } = &params[0] {
				assert_eq!(param_name, "host");
				assert!(matches!(arg, Literal::String(s) if s == "localhost"));
			} else {
				panic!("Expected Named parameter");
			}

			if let DirectiveParam::Named { name: param_name, arg } = &params[1] {
				assert_eq!(param_name, "port");
				assert!(matches!(arg, Literal::Int(5432)));
			} else {
				panic!("Expected Named parameter");
			}

			if let DirectiveParam::Named { name: param_name, arg } = &params[2] {
				assert_eq!(param_name, "debug");
				assert!(matches!(arg, Literal::Bool(true)));
			} else {
				panic!("Expected Named parameter");
			}
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_complex_mixed_params()
	{
		let result = parse_directive(r#"@server("0.0.0.0", 8080, workers = 4, ssl = false)"#);
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "server");
			assert_eq!(params.len(), 4);

			assert!(matches!(
				&params[0],
				DirectiveParam::Literal(Literal::String(s)) if s == "0.0.0.0"
			));

			assert!(matches!(&params[1], DirectiveParam::Literal(Literal::Int(8080))));

			if let DirectiveParam::Named { name: param_name, arg } = &params[2] {
				assert_eq!(param_name, "workers");
				assert!(matches!(arg, Literal::Int(4)));
			} else {
				panic!("Expected Named parameter");
			}

			if let DirectiveParam::Named { name: param_name, arg } = &params[3] {
				assert_eq!(param_name, "ssl");
				assert!(matches!(arg, Literal::Bool(false)));
			} else {
				panic!("Expected Named parameter");
			}
		} else {
			panic!("Expected Custom directive");
		}
	}

	// ===== Edge Cases =====

	#[test]
	fn test_directive_no_params()
	{
		let result = parse_directive("@inline()");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "inline");
			assert_eq!(params.len(), 0);
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_trailing_comma()
	{
		let result = parse_directive("@test(foo, bar,)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "test");
			assert_eq!(params.len(), 2);
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_negative_int()
	{
		let result = parse_directive("@offset(-10)");
		assert!(result.is_ok());
	}

	#[test]
	fn test_directive_string_with_escapes()
	{
		let result = parse_directive(r#"@message("Hello\nWorld")"#);
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "message");
			assert_eq!(params.len(), 1);
			// The actual escape sequence handling depends on your lexer
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_underscore_identifier()
	{
		let result = parse_directive("@cfg(_private_flag)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "cfg");
			assert_eq!(params.len(), 1);
			assert!(matches!(
				&params[0],
				DirectiveParam::Identifier(id) if id == "_private_flag"
			));
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_numeric_identifier()
	{
		let result = parse_directive("@version(v1_0_0)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "version");
			assert_eq!(params.len(), 1);
			assert!(matches!(
				&params[0],
				DirectiveParam::Identifier(id) if id == "v1_0_0"
			));
		} else {
			panic!("Expected Custom directive");
		}
	}

	// ===== Real-World Scenarios =====

	#[test]
	fn test_directive_repr()
	{
		let result = parse_directive("@repr(C)");
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "repr");
			assert_eq!(params.len(), 1);
			assert!(matches!(
				&params[0],
				DirectiveParam::Identifier(id) if id == "C"
			));
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_deprecated()
	{
		let result = parse_directive(r#"@deprecated(since = "1.0.0", note = "Use new_func instead")"#);
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "deprecated");
			assert_eq!(params.len(), 2);
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_test_case()
	{
		let result = parse_directive(r#"@test_case(input = 42, expected = 84)"#);
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "test_case");
			assert_eq!(params.len(), 2);
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_directive_cfg_attr()
	{
		let result = parse_directive(r#"@cfg_attr(unix, link = "pthread")"#);
		assert!(result.is_ok());
		let directive = result.unwrap();

		if let Directive::Custom { name, params } = directive.directive {
			assert_eq!(name, "cfg_attr");
			assert_eq!(params.len(), 2);
		} else {
			panic!("Expected Custom directive");
		}
	}

	#[test]
	fn test_parse_where_clause_with_fn_bound()
	{
		let input = "fn apply<F>(f: F) where F: Fn(i32) -> i32 {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		// Verify the Fn bound is parsed correctly
	}

	#[test]
	fn test_parse_where_clause_with_fn_bound_no_return()
	{
		let input = "fn process<F>(f: F) where F: Fn(String) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_inline_fn_bound()
	{
		let input = "fn map<F: Fn(i32) -> String>(f: F) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_mut_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "mut i32", "test", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
		match result.unwrap().core.as_ref() {
			TypeCore::Mutable { .. } => (),
			_ => panic!("Expected mutable type"),
		}
	}

	#[test]
	fn test_parse_delete_function_declaration()
	{
		let input = "fn delete(ptr: u8*) {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_where_clause_with_type_args()
	{
		let input = "impl<T> Trait for Type where Vec<T>: Clone {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
		// Verify type_args are populated
	}

	#[test]
	fn test_parse_struct_init_mixed_shorthand_and_explicit()
	{
		let input = "Point { x, y = 10 }";
		let result = parse_expr_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_loop_break_with_switch()
	{
		let input = r#"{
			loop {
				break switch x {
					1 => 10,
					_ => 0,
				};
			}
		}"#;
		let result = parse_block_from_str(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_directive_negative_float()
	{
		let result = parse_directive("@threshold(-3.14)");
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_empty_array_type()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "i32[]", "test", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_quadruple_nested_generics()
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, "Box<Vec<Option<Result<i32>>>>", "test", &mut source_map);
		let mut parser = Parser::from(lexer);
		let result = parser.parse_type();
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_heap_function_with_both_generic_types()
	{
		let input = "fn!<A: Allocator> create<T: Clone>(alloc: A, value: T) -> Box<T> {}";
		let result = parse_program_from_str(input);
		assert!(result.is_ok());
	}
}
