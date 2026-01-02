use crate::parser::{
	ArrayLiteral, Block, CaseArm, CaseBody, Expr, FunctionDecl, Ident, ImplDecl, ImplItem, NamespaceDecl, Pattern,
	Program, Spanned, Stmt, TopLevelDecl, TraitDecl, TraitItem, Type, TypeCore, VariableDecl,
};

#[derive(Debug, Default)]
pub struct Desugarer
{
	tmp_counter: usize,
}

impl Desugarer
{
	pub fn new() -> Self
	{
		return Default::default();
	}

	fn gen_temp(&mut self, name: &str) -> Ident
	{
		let name: String = format!("#__tmp_{}_{}", self.tmp_counter, name); // # only internals can use it, only at the start,
		// and this will be changed in compiletime, so the users can use `__` at the start fo their variables
		self.tmp_counter += 1;
		return name;
	}

	pub fn desugar_program(&mut self, program: Program) -> Program
	{
		return Program {
			items: program
				.items
				.into_iter()
				.map(|item| self.desugar_top_level_item(item))
				.collect(),
		};
	}

	fn desugar_top_level_item(&mut self, item: Spanned<TopLevelDecl>) -> Spanned<TopLevelDecl>
	{
		let Spanned { node, span } = item;
		let desugared: TopLevelDecl = self.desugar_top_level_decl(node);
		return Spanned { node: desugared, span };
	}

	fn desugar_top_level_decl(&mut self, decl: TopLevelDecl) -> TopLevelDecl
	{
		return match decl {
			TopLevelDecl::Function(func) => TopLevelDecl::Function(self.desugar_function(func)),

			TopLevelDecl::Namespace(ns) => TopLevelDecl::Namespace(self.desugar_namespace(ns)),

			TopLevelDecl::Impl(impl_decl) => TopLevelDecl::Impl(self.desugar_impl(impl_decl)),

			TopLevelDecl::Trait(trait_decl) => TopLevelDecl::Trait(self.desugar_trait(trait_decl)),

			TopLevelDecl::VariableDecl(var) => TopLevelDecl::VariableDecl(var),
			TopLevelDecl::Struct(s) => TopLevelDecl::Struct(s),
			TopLevelDecl::Union(u) => TopLevelDecl::Union(u),
			TopLevelDecl::Enum(e) => TopLevelDecl::Enum(e),
			TopLevelDecl::Variant(v) => TopLevelDecl::Variant(v),
			TopLevelDecl::TypeAlias(t) => TopLevelDecl::TypeAlias(t),
			TopLevelDecl::Directive(d) => TopLevelDecl::Directive(d),
		};
	}

	fn desugar_function(&mut self, mut func: FunctionDecl) -> FunctionDecl
	{
		if let Some(body) = func.body {
			func.body = Some(self.desugar_block(body));
		}
		return func;
	}

	fn desugar_namespace(&mut self, mut ns: NamespaceDecl) -> NamespaceDecl
	{
		ns.body = self.desugar_program(ns.body);
		return ns;
	}

	fn desugar_impl(&mut self, mut impl_decl: ImplDecl) -> ImplDecl
	{
		impl_decl.body = impl_decl
			.body
			.into_iter()
			.map(|item| {
				let Spanned { node, span } = item;
				let desugared = match node {
					ImplItem::Function(func) => ImplItem::Function(self.desugar_function(func)),
					ImplItem::TypeAlias(t) => ImplItem::TypeAlias(t),
					ImplItem::Const(c) => ImplItem::Const(c),
				};
				Spanned { node: desugared, span }
			})
			.collect();
		return impl_decl;
	}

	fn desugar_trait(&mut self, mut trait_decl: TraitDecl) -> TraitDecl
	{
		trait_decl.items = trait_decl
			.items
			.into_iter()
			.map(|item| {
				let Spanned { node, span } = item;
				let desugared = match node {
					TraitItem::Function(sig, body) => {
						let desugared_body = body.map(|b| self.desugar_block(b));
						TraitItem::Function(sig, desugared_body)
					}
					TraitItem::TypeAlias(t) => TraitItem::TypeAlias(t),
					TraitItem::Const(c) => TraitItem::Const(c),
				};
				Spanned { node: desugared, span }
			})
			.collect();
		return trait_decl;
	}

	fn desugar_block(&mut self, block: Block) -> Block
	{
		return Block {
			stmts: block.stmts.into_iter().map(|stmt| self.desugar_stmt(stmt)).collect(),
			tail_expr: block.tail_expr.map(|expr| Box::new(self.desugar_expr(*expr))),
		};
	}

	fn desugar_stmt(&mut self, stmt: Stmt) -> Stmt
	{
		return match stmt {
			Stmt::For { name, iter, body } => self.desugar_for_loop(name, iter, body),
			Stmt::If {
				cond,
				then_block,
				else_branch: else_stmt,
			} => Stmt::If {
				cond: self.desugar_expr(cond),
				then_block: self.desugar_block(then_block),
				else_branch: else_stmt.map(|else_stmt| Box::new(self.desugar_stmt(*else_stmt))),
			},

			Stmt::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
			} => self.desugar_if_var(pattern, expr, then_block, else_branch),

			Stmt::While { cond, body } => Stmt::While {
				cond: self.desugar_expr(cond),
				body: self.desugar_block(body),
			},

			Stmt::Loop { body } => Stmt::Loop {
				body: self.desugar_block(body),
			},

			Stmt::WhileVarLoop { pattern, expr, body } => self.desugar_while_var_loop(pattern, expr, body),

			Stmt::VariableDecl(var) => Stmt::VariableDecl(self.desugar_variable_decl(var)),

			Stmt::Assignment { target, op, value } => Stmt::Assignment {
				target: self.desugar_expr(target),
				op,
				value: self.desugar_expr(value),
			},

			Stmt::Return(expr) => Stmt::Return(expr.map(|e| self.desugar_expr(e))),

			Stmt::Expr(expr) => Stmt::Expr(self.desugar_expr(expr)),

			Stmt::Unsafe(block) => Stmt::Unsafe(self.desugar_block(block)),

			Stmt::Delete(path) => Stmt::Delete(path),

			// Bre
			Stmt::Break => Stmt::Break,
			Stmt::Continue => Stmt::Continue,
		};
	}

	/// Desugar a for loop into a loop with iterator protocol.
	///
	/// ```
	/// for name in iter { body }
	/// ```
	///
	/// Becomes:
	///
	/// ```
	/// {
	///     let __iter = iter;
	///     loop {
	///         case __iter.next() {
	///             Some(name) => { body },
	///             None => break,
	///         }
	///     }
	/// }
	/// ```
	fn desugar_for_loop(&mut self, name: Vec<Ident>, iter: Expr, body: Block) -> Stmt
	{
		let iter_temp: Vec<Ident> = vec![self.gen_temp("loop")];

		let desugared_iter: Expr = self.desugar_expr(iter);
		let desugared_body: Block = self.desugar_block(body);

		let name_pattern: Pattern = Pattern::Variant {
			path: name.clone(),
			args: vec![],
		};

		let iter_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				name: iter_temp[0].clone(),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: vec!["_".to_string()],
						generics: vec![],
					}),
				},
			},
			init: Some(desugared_iter),
			comp_const: false,
		});

		let next_call: Expr = Expr::Call {
			callee: Box::new(Expr::Field {
				base: Box::new(Expr::Identifier(iter_temp.clone())),
				name: "next".to_string(),
			}),
			args: vec![],
		};

		let some_arm: CaseArm = CaseArm {
			pattern: Pattern::Variant {
				path: vec!["Some".to_string()],
				args: vec![name_pattern],
			},
			body: CaseBody::Block(desugared_body),
		};

		let none_arm: CaseArm = CaseArm {
			pattern: Pattern::Variant {
				path: vec!["None".to_string()],
				args: vec![],
			},
			body: CaseBody::Expr(Expr::Block(Box::new(Block {
				stmts: vec![Stmt::Break],
				tail_expr: None,
			}))),
		};

		let case_expr: Expr = Expr::Case {
			expr: Box::new(next_call),
			arms: vec![some_arm, none_arm],
		};

		let loop_stmt: Stmt = Stmt::Loop {
			body: Block {
				stmts: vec![Stmt::Expr(case_expr)],
				tail_expr: None,
			},
		};

		return Stmt::Expr(Expr::Block(Box::new(Block {
			stmts: vec![iter_decl, loop_stmt],
			tail_expr: None,
		})));
	}

	/// Desugar an if-var statement into a case expression.
	///
	/// ```
	/// if var pattern = expr {
	///     then_block
	/// } else {
	///     else_block
	/// }
	/// ```
	///
	/// Becomes:
	///
	/// ```
	/// {
	///     var __tmp = expr;
	///     case __tmp {
	///         pattern => { then_block },
	///         _ => { else_block },
	///     }
	/// }
	/// ```
	fn desugar_if_var(
		&mut self,
		pattern: Pattern,
		expr: Expr,
		then_block: Block,
		else_branch: Option<Box<Stmt>>,
	) -> Stmt
	{
		let temp_var: Ident = self.gen_temp("ifvar");

		let desugared_expr: Expr = self.desugar_expr(expr);
		let desugared_then: Block = self.desugar_block(then_block);

		let temp_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				name: temp_var.clone(),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: vec!["_".to_string()],
						generics: vec![],
					}),
				},
			},
			init: Some(desugared_expr),
			comp_const: false,
		});

		let match_arm: CaseArm = CaseArm {
			pattern,
			body: CaseBody::Block(desugared_then),
		};

		let else_arm: CaseArm = CaseArm {
			pattern: Pattern::Wildcard,
			body: if let Some(else_stmt) = else_branch {
				let desugared_else = self.desugar_stmt(*else_stmt);
				// Unwrap the "if true { block } else {}" wrapper that the parser creates
				match desugared_else {
					Stmt::If {
						cond: Expr::Literal(crate::parser::Literal::Bool(true)),
						then_block: block,
						else_branch: None,
					} => CaseBody::Block(block),
					// If it's another if statement (else if), wrap it
					other => CaseBody::Block(Block {
						stmts: vec![other],
						tail_expr: None,
					}),
				}
			} else {
				CaseBody::Block(Block {
					stmts: vec![],
					tail_expr: None,
				})
			},
		};

		let case_expr: Expr = Expr::Case {
			expr: Box::new(Expr::Identifier(vec![temp_var])),
			arms: vec![match_arm, else_arm],
		};

		return Stmt::Expr(Expr::Block(Box::new(Block {
			stmts: vec![temp_decl, Stmt::Expr(case_expr)],
			tail_expr: None,
		})));
	}

	/// Desugar a while-var loop into a regular loop with pattern matching.
	///
	/// ```
	/// while var pattern = expr {
	///     body
	/// }
	/// ```
	///
	/// Becomes:
	///
	/// ```
	/// loop {
	///     let __tmp = expr;
	///     case __tmp {
	///         pattern => { body },
	///         _ => break,
	///     }
	/// }
	/// ```
	fn desugar_while_var_loop(&mut self, pattern: Pattern, expr: Expr, body: Block) -> Stmt
	{
		let temp_var: Ident = self.gen_temp("whilevar");

		let desugared_expr: Expr = self.desugar_expr(expr);
		let desugared_body: Block = self.desugar_block(body);

		let temp_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				name: temp_var.clone(),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: vec!["_".to_string()],
						generics: vec![],
					}),
				},
			},
			init: Some(desugared_expr),
			comp_const: false,
		});

		let match_arm: CaseArm = CaseArm {
			pattern,
			body: CaseBody::Block(desugared_body),
		};

		let break_arm: CaseArm = CaseArm {
			pattern: Pattern::Wildcard,
			body: CaseBody::Expr(Expr::Block(Box::new(Block {
				stmts: vec![Stmt::Break],
				tail_expr: None,
			}))),
		};

		let case_expr: Expr = Expr::Case {
			expr: Box::new(Expr::Identifier(vec![temp_var])),
			arms: vec![match_arm, break_arm],
		};

		return Stmt::Loop {
			body: Block {
				stmts: vec![temp_decl, Stmt::Expr(case_expr)],
				tail_expr: None,
			},
		};
	}

	fn desugar_variable_decl(&mut self, mut var: VariableDecl) -> VariableDecl
	{
		var.init = var.init.map(|init| self.desugar_expr(init));
		return var;
	}

	fn desugar_expr(&mut self, expr: Expr) -> Expr
	{
		return match expr {
			Expr::Unary { op, expr } => Expr::Unary {
				op,
				expr: Box::new(self.desugar_expr(*expr)),
			},

			Expr::Binary { op, lhs, rhs } => Expr::Binary {
				op,
				lhs: Box::new(self.desugar_expr(*lhs)),
				rhs: Box::new(self.desugar_expr(*rhs)),
			},

			Expr::Cast { ty, expr } => Expr::Cast {
				ty,
				expr: Box::new(self.desugar_expr(*expr)),
			},

			Expr::Call { callee, args } => Expr::Call {
				callee: Box::new(self.desugar_expr(*callee)),
				args: args.into_iter().map(|arg| self.desugar_expr(arg)).collect(),
			},

			Expr::Field { base, name } => Expr::Field {
				base: Box::new(self.desugar_expr(*base)),
				name,
			},

			Expr::Index { base, index } => Expr::Index {
				base: Box::new(self.desugar_expr(*base)),
				index: Box::new(self.desugar_expr(*index)),
			},

			Expr::Tuple(exprs) => Expr::Tuple(exprs.into_iter().map(|e| self.desugar_expr(e)).collect()),

			Expr::Array(array_lit) => Expr::Array(self.desugar_array_literal(array_lit)),

			Expr::StructInit { path, fields } => Expr::StructInit {
				path,
				fields: fields
					.into_iter()
					.map(|(name, expr)| (name, self.desugar_expr(expr)))
					.collect(),
			},

			Expr::Block(block) => Expr::Block(Box::new(self.desugar_block(*block))),

			Expr::Case { expr, arms } => Expr::Case {
				expr: Box::new(self.desugar_expr(*expr)),
				arms: arms.into_iter().map(|arm| self.desugar_case_arm(arm)).collect(),
			},

			Expr::Identifier(_) => expr,
			Expr::Literal(_) => expr,
			Expr::Range(_) => expr,
		};
	}

	fn desugar_array_literal(&mut self, array_lit: ArrayLiteral) -> ArrayLiteral
	{
		return match array_lit {
			ArrayLiteral::List(exprs) => ArrayLiteral::List(exprs.into_iter().map(|e| self.desugar_expr(e)).collect()),
			ArrayLiteral::Repeat { value, count } => ArrayLiteral::Repeat {
				value: value.into_iter().map(|e| self.desugar_expr(e)).collect(),
				count: Box::new(self.desugar_expr(*count)),
			},
		};
	}

	fn desugar_case_arm(&mut self, arm: CaseArm) -> CaseArm
	{
		return CaseArm {
			pattern: arm.pattern,
			body: match arm.body {
				CaseBody::Expr(expr) => CaseBody::Expr(self.desugar_expr(expr)),
				CaseBody::Block(block) => CaseBody::Block(self.desugar_block(block)),
			},
		};
	}
}

#[cfg(test)]
mod tests
{
	use crate::parser::{AssignOp, Literal};

	use super::*;

	// Helper to create a simple identifier expression
	fn ident(name: &str) -> Expr
	{
		Expr::Identifier(vec![name.to_string()])
	}

	#[test]
	fn test_desugar_if_with_else_unchanged()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::If {
			cond: Expr::Literal(Literal::Bool(true)),
			then_block: Block {
				stmts: vec![],
				tail_expr: None,
			},
			else_branch: Some(Box::new(Stmt::Expr(Expr::Block(Box::new(Block {
				stmts: vec![],
				tail_expr: None,
			}))))),
		};

		let output = desugarer.desugar_stmt(input);

		// Should keep the else branch
		match output {
			Stmt::If {
				else_branch: Some(_), ..
			} => {
				// Success - else branch preserved
			}
			_ => panic!("Expected if-else, got {:?}", output),
		}
	}

	#[test]
	fn test_compound_assignment_not_desugared()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::Assignment {
			target: ident("x"),
			op: AssignOp::AddAssign,
			value: Expr::Literal(Literal::Int(5)),
		};

		let output = desugarer.desugar_stmt(input);

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
			pattern: Pattern::TypedIdentifier {
				name: "x".to_string(),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: vec!["i32".to_string()],
						generics: vec![],
					}),
				},
			},
			expr: Expr::Literal(Literal::Int(42)),
			then_block: Block {
				stmts: vec![],
				tail_expr: None,
			},
			else_branch: None,
		};

		let output = desugarer.desugar_stmt(input);

		// Should desugar to a block with temp var and case expression
		match output {
			Stmt::Expr(Expr::Block(block)) => {
				assert_eq!(block.stmts.len(), 2);
				// First statement should be variable declaration
				assert!(matches!(block.stmts[0], Stmt::VariableDecl(_)));
				// Second statement should be case expression
				assert!(matches!(block.stmts[1], Stmt::Expr(Expr::Case { .. })));
			}
			_ => panic!("Expected block with case expression, got {:?}", output),
		}
	}

	#[test]
	fn test_desugar_while_var_loop()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::WhileVarLoop {
			pattern: Pattern::TypedIdentifier {
				name: "x".to_string(),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: vec!["i32".to_string()],
						generics: vec![],
					}),
				},
			},
			expr: ident("some_value"),
			body: Block {
				stmts: vec![],
				tail_expr: None,
			},
		};

		let output = desugarer.desugar_stmt(input);

		// Should desugar to a loop with temp var and case expression
		match output {
			Stmt::Loop { body } => {
				assert_eq!(body.stmts.len(), 2);
				// First statement should be variable declaration
				assert!(matches!(body.stmts[0], Stmt::VariableDecl(_)));
				// Second statement should be case expression with break on wildcard
				assert!(matches!(body.stmts[1], Stmt::Expr(Expr::Case { .. })));
			}
			_ => panic!("Expected loop with case expression, got {:?}", output),
		}
	}
}
