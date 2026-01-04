use crate::parser::{
	ArrayLiteral, Block, BlockContent, DirectiveNode, Expr, FunctionDecl, Ident, ImplDecl, ImplItem, NamespaceDecl,
	Pattern, Program, Spanned, Stmt, SwitchArm, SwitchBody, TopLevelDecl, TraitDecl, TraitItem, Type, TypeCore,
	VariableDecl,
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

	fn desugar_block_content(&mut self, block: BlockContent) -> BlockContent
	{
		return match block {
			BlockContent::Block(block) => BlockContent::Block(self.desugar_block(block)),
			BlockContent::TopLevelBlock(block) => BlockContent::TopLevelBlock(self.desugar_program(block)),
		};
	}

	fn desugar_stmt(&mut self, stmt: Stmt) -> Stmt
	{
		debug_assert!(
			!matches!(
				stmt,
				Stmt::Expr(Expr::Block(_)) | Stmt::Expr(Expr::If { .. }) | Stmt::Expr(Expr::IfVar { .. })
			),
			"This should just be Stmt::.."
		);
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
			Stmt::Block(block) => Stmt::Block(self.desugar_block(block)),
			Stmt::Directive(directive) => Stmt::Directive(DirectiveNode {
				directive: directive.directive,
				body: directive.body.map(|body| self.desugar_block_content(body)),
			}),

			Stmt::Delete(path) => Stmt::Delete(path),

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
	///         switch __iter.next() {
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

		let some_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Variant {
				path: vec!["Some".to_string()],
				args: vec![name_pattern],
			},
			body: SwitchBody::Block(desugared_body),
		};

		let none_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Variant {
				path: vec!["None".to_string()],
				args: vec![],
			},
			body: SwitchBody::Block(Block {
				stmts: vec![Stmt::Break],
				tail_expr: None,
			}),
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(next_call),
			arms: vec![some_arm, none_arm],
		};

		let loop_stmt: Stmt = Stmt::Loop {
			body: Block {
				stmts: vec![Stmt::Expr(switch_expr)],
				tail_expr: None,
			},
		};

		return Stmt::Block(Block {
			stmts: vec![iter_decl, loop_stmt],
			tail_expr: None,
		});
	}

	/// Desugar an if-var statement into a switch expression.
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
	///     switch __tmp {
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

		let match_arm: SwitchArm = SwitchArm {
			pattern: self.desugar_pattern(pattern),
			body: SwitchBody::Block(desugared_then),
		};

		let else_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard,
			body: if let Some(else_stmt) = else_branch {
				SwitchBody::Block(Block {
					stmts: vec![self.desugar_stmt(*else_stmt)],
					tail_expr: None,
				})
			} else {
				SwitchBody::Block(Block {
					stmts: vec![],
					tail_expr: None,
				})
			},
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(Expr::Identifier(vec![temp_var])),
			arms: vec![match_arm, else_arm],
		};

		return Stmt::Block(Block {
			stmts: vec![temp_decl, Stmt::Expr(switch_expr)],
			tail_expr: None,
		});
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
	///     switch __tmp {
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

		let match_arm: SwitchArm = SwitchArm {
			pattern: self.desugar_pattern(pattern),
			body: SwitchBody::Block(desugared_body),
		};

		let break_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard,
			body: SwitchBody::Block(Block {
				stmts: vec![Stmt::Break],
				tail_expr: None,
			}),
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(Expr::Identifier(vec![temp_var])),
			arms: vec![match_arm, break_arm],
		};

		return Stmt::Loop {
			body: Block {
				stmts: vec![temp_decl, Stmt::Expr(switch_expr)],
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

			Expr::Switch { expr, arms } => Expr::Switch {
				expr: Box::new(self.desugar_expr(*expr)),
				arms: arms.into_iter().map(|arm| self.desugar_switch_arm(arm)).collect(),
			},

			Expr::Identifier(_) => expr,
			Expr::Literal(_) => expr,
			Expr::Range(_) => expr,
			Expr::If {
				cond,
				then_block,
				else_branch,
			} => self.desugar_if_expr(*cond, then_block, else_branch),

			Expr::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
			} => self.desugar_if_var_expr(pattern, *expr, then_block, else_branch),
		};
	}

	fn desugar_if_expr(&mut self, cond: Expr, then_block: Block, else_branch: Option<Box<Expr>>) -> Expr
	{
		Expr::If {
			cond: Box::new(self.desugar_expr(cond)),
			then_block: self.desugar_block(then_block),
			else_branch: else_branch.map(|e| Box::new(self.desugar_expr(*e))),
		}
	}

	fn desugar_if_var_expr(
		&mut self,
		pattern: Pattern,
		expr: Expr,
		then_block: Block,
		else_branch: Option<Box<Expr>>,
	) -> Expr
	{
		let temp_var = self.gen_temp("ifvar_expr");

		let desugared_expr = self.desugar_expr(expr);
		let desugared_then = self.desugar_block(then_block);

		let temp_decl = Stmt::VariableDecl(VariableDecl {
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

		let match_arm = SwitchArm {
			pattern: self.desugar_pattern(pattern),
			body: SwitchBody::Block(desugared_then),
		};

		let else_arm = SwitchArm {
			pattern: Pattern::Wildcard,
			body: if let Some(else_expr) = else_branch {
				let desugared_else = self.desugar_expr(*else_expr);

				match desugared_else {
					Expr::Block(block) => SwitchBody::Block(*block),
					other_expr => SwitchBody::Block(Block {
						stmts: vec![],
						tail_expr: Some(Box::new(other_expr)),
					}),
				}
			} else {
				SwitchBody::Block(Block {
					stmts: vec![],
					tail_expr: None,
				})
			},
		};

		let switch_expr = Expr::Switch {
			expr: Box::new(Expr::Identifier(vec![temp_var])),
			arms: vec![match_arm, else_arm],
		};

		Expr::Block(Box::new(Block {
			stmts: vec![temp_decl],
			tail_expr: Some(Box::new(switch_expr)),
		}))
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

	fn desugar_switch_arm(&mut self, arm: SwitchArm) -> SwitchArm
	{
		return SwitchArm {
			pattern: self.desugar_pattern(arm.pattern),
			body: match arm.body {
				SwitchBody::Expr(expr) => SwitchBody::Expr(self.desugar_expr(expr)),
				SwitchBody::Block(block) => SwitchBody::Block(self.desugar_block(block)),
			},
		};
	}

	fn desugar_pattern(&mut self, pattern: Pattern) -> Pattern
	{
		match pattern {
			Pattern::Wildcard => Pattern::Wildcard,

			Pattern::Literal(lit) => Pattern::Literal(lit),

			Pattern::TypedIdentifier { name, ty } => Pattern::TypedIdentifier { name, ty },

			Pattern::Variant { path, args } => Pattern::Variant {
				path,
				args: args.into_iter().map(|p| self.desugar_pattern(p)).collect(),
			},

			Pattern::Tuple(patterns) => Pattern::Tuple(patterns.into_iter().map(|p| self.desugar_pattern(p)).collect()),

			Pattern::Struct { path, fields } => Pattern::Struct {
				path,
				fields: fields
					.into_iter()
					.map(|(name, pat)| (name, self.desugar_pattern(pat)))
					.collect(),
			},

			Pattern::Range(range) => Pattern::Range(range),

			Pattern::Or(patterns) => Pattern::Or(patterns.into_iter().map(|p| self.desugar_pattern(p)).collect()),
		}
	}
}

#[cfg(test)]
mod tests
{
	use crate::lexer::Span;
	use crate::parser::{AssignOp, BinaryOp, FunctionSignature, ImplTarget, Literal, UnaryOp};

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
			else_branch: Some(Box::new(Stmt::Block(Block {
				stmts: vec![],
				tail_expr: None,
			}))),
		};

		let output = desugarer.desugar_stmt(input);

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

		match output {
			Stmt::Loop { body } => {
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

			Stmt::Loop { body } | Stmt::While { body, .. } | Stmt::Unsafe(body) => {
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
			pattern: Pattern::Wildcard,
			expr: ident("x"),
			then_block: Block {
				stmts: vec![],
				tail_expr: None,
			},
			else_branch: None,
		};

		let output = desugarer.desugar_stmt(stmt);

		assert_no_expr_block(&output);
	}

	#[test]
	fn test_desugar_switch_arm_pattern_recursive()
	{
		let mut desugarer = Desugarer::new();

		let arm = SwitchArm {
			pattern: Pattern::Or(vec![
				Pattern::Variant {
					path: vec!["Some".into()],
					args: vec![Pattern::Wildcard],
				},
				Pattern::Variant {
					path: vec!["None".into()],
					args: vec![],
				},
			]),
			body: SwitchBody::Block(Block {
				stmts: vec![],
				tail_expr: None,
			}),
		};

		let out = desugarer.desugar_switch_arm(arm);

		match out.pattern {
			Pattern::Or(ps) => assert_eq!(ps.len(), 2),
			_ => panic!("Expected Pattern::Or"),
		}
	}

	#[test]
	fn test_desugar_for_loop_shape()
	{
		let mut desugarer = Desugarer::new();

		let input = Stmt::For {
			name: vec!["x".into()],
			iter: ident("iter"),
			body: Block {
				stmts: vec![],
				tail_expr: None,
			},
		};

		let output = desugarer.desugar_stmt(input);

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
			expr: ident("value"),
			then_block: Block {
				stmts: vec![Stmt::IfVar {
					pattern: Pattern::Wildcard,
					expr: ident("y"),
					then_block: Block {
						stmts: vec![],
						tail_expr: None,
					},
					else_branch: None,
				}],
				tail_expr: None,
			},
			else_branch: None,
		};

		let output = desugarer.desugar_stmt(input);

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
						_ => panic!("Expected inner block in switch arm"),
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
			name: vec!["x".into(), "y".into()],
			iter: ident("iter"),
			body: Block {
				stmts: vec![],
				tail_expr: None,
			},
		};

		let output = desugarer.desugar_stmt(input);

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
					init: Some(ident("y")),
					comp_const: false,
				})],
				tail_expr: Some(Box::new(ident("x"))),
			}))],
		};

		let output = desugarer.desugar_expr(input);

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

		let list_array = ArrayLiteral::List(vec![ident("x"), ident("y")]);
		let repeat_array = ArrayLiteral::Repeat {
			value: vec![ident("z")],
			count: Box::new(ident("n")),
		};

		let desugared_list = desugarer.desugar_array_literal(list_array);
		let desugared_repeat = desugarer.desugar_array_literal(repeat_array);

		match desugared_list {
			ArrayLiteral::List(v) => assert_eq!(v.len(), 2),
			_ => panic!("Expected List variant"),
		}

		match desugared_repeat {
			ArrayLiteral::Repeat { value, .. } => assert_eq!(value.len(), 1),
			_ => panic!("Expected Repeat variant"),
		}
	}

	#[test]
	fn test_desugar_nested_switch_pattern()
	{
		let mut desugarer = Desugarer::new();

		let arm = SwitchArm {
			pattern: Pattern::Tuple(vec![
				Pattern::Variant {
					path: vec!["Some".into()],
					args: vec![Pattern::Wildcard],
				},
				Pattern::Or(vec![Pattern::Wildcard, Pattern::Literal(Literal::Int(5))]),
			]),
			body: SwitchBody::Block(Block {
				stmts: vec![],
				tail_expr: None,
			}),
		};

		let out = desugarer.desugar_switch_arm(arm);

		if let Pattern::Tuple(patterns) = out.pattern {
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
			items: vec![Spanned {
				node: TopLevelDecl::VariableDecl(VariableDecl {
					pattern: Pattern::TypedIdentifier {
						name: "x".into(),
						ty: Type {
							modifiers: vec![],
							core: Box::new(TypeCore::Base {
								path: vec!["i32".into()],
								generics: vec![],
							}),
						},
					},
					init: Some(Expr::Literal(Literal::Int(42))),
					comp_const: true,
				}),
				span: Span::default(),
			}],
		};

		let output = desugarer.desugar_program(program);
		assert_eq!(output.items.len(), 1);
	}

	#[test]
	fn test_desugar_namespace_recursively()
	{
		let mut desugarer = Desugarer::new();

		let ns = NamespaceDecl {
			modifiers: vec![],
			name: vec!["test".into()],
			body: Program {
				items: vec![Spanned {
					node: TopLevelDecl::Function(FunctionDecl {
						signature: FunctionSignature {
							modifiers: vec![],
							name: vec!["foo".into()],
							generics: vec![],
							params: vec![],
							return_type: None,
							where_clause: vec![],
							heap_func: false,
						},
						body: Some(Block {
							stmts: vec![Stmt::For {
								name: vec!["i".into()],
								iter: ident("items"),
								body: Block {
									stmts: vec![],
									tail_expr: None,
								},
							}],
							tail_expr: None,
						}),
					}),
					span: Span::default(),
				}],
			},
		};

		let output = desugarer.desugar_namespace(ns);
		// Verify the for loop inside was desugared
		if let TopLevelDecl::Function(func) = &output.body.items[0].node
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
				path: vec!["MyType".into()],
				generics: vec![],
			},
			trait_path: None,
			where_clause: vec![],
			body: vec![Spanned {
				node: ImplItem::Function(FunctionDecl {
					signature: FunctionSignature {
						modifiers: vec![],
						name: vec!["method".into()],
						generics: vec![],
						params: vec![],
						return_type: None,
						where_clause: vec![],
						heap_func: false,
					},
					body: Some(Block {
						stmts: vec![Stmt::WhileVarLoop {
							pattern: Pattern::Wildcard,
							expr: ident("x"),
							body: Block {
								stmts: vec![],
								tail_expr: None,
							},
						}],
						tail_expr: None,
					}),
				}),
				span: Span::default(),
			}],
		};

		let output = desugarer.desugar_impl(impl_decl);
		assert_eq!(output.body.len(), 1);
	}

	#[test]
	fn test_desugar_trait_with_default_impl()
	{
		let mut desugarer = Desugarer::new();

		let trait_decl = TraitDecl {
			modifiers: vec![],
			name: vec!["MyTrait".into()],
			generics: vec![],
			super_traits: vec![],
			items: vec![Spanned {
				node: TraitItem::Function(
					FunctionSignature {
						modifiers: vec![],
						name: vec!["method".into()],
						generics: vec![],
						params: vec![],
						return_type: None,
						where_clause: vec![],
						heap_func: false,
					},
					Some(Block {
						stmts: vec![Stmt::For {
							name: vec!["i".into()],
							iter: ident("items"),
							body: Block {
								stmts: vec![],
								tail_expr: None,
							},
						}],
						tail_expr: None,
					}),
				),
				span: Span::default(),
			}],
		};

		let output = desugarer.desugar_trait(trait_decl);
		assert_eq!(output.items.len(), 1);
	}

	#[test]
	fn test_desugar_return_with_nested_expr()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Return(Some(Expr::Binary {
			op: BinaryOp::Add,
			lhs: Box::new(Expr::Block(Box::new(Block {
				stmts: vec![],
				tail_expr: Some(Box::new(Expr::Literal(Literal::Int(1)))),
			}))),
			rhs: Box::new(Expr::Literal(Literal::Int(2))),
		}));

		let output = desugarer.desugar_stmt(stmt);

		match output {
			Stmt::Return(Some(_)) => (),
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
				}),
				index: Box::new(Expr::Literal(Literal::Int(0))),
			},
			op: AssignOp::Assign,
			value: Expr::Literal(Literal::Int(42)),
		};

		let output = desugarer.desugar_stmt(stmt);

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
			cond: Expr::Literal(Literal::Bool(true)),
			then_block: Block {
				stmts: vec![Stmt::For {
					name: vec!["x".into()],
					iter: ident("items"),
					body: Block {
						stmts: vec![],
						tail_expr: None,
					},
				}],
				tail_expr: None,
			},
			else_branch: None,
		};

		let output = desugarer.desugar_stmt(stmt);

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
			cond: Expr::Binary {
				op: BinaryOp::Lt,
				lhs: Box::new(Expr::Call {
					callee: Box::new(ident("get_count")),
					args: vec![],
				}),
				rhs: Box::new(Expr::Literal(Literal::Int(10))),
			},
			body: Block {
				stmts: vec![],
				tail_expr: None,
			},
		};

		let output = desugarer.desugar_stmt(stmt);

		match output {
			Stmt::While { cond, .. } => {
				assert!(matches!(cond, Expr::Binary { .. }));
			}
			_ => panic!("Expected while statement"),
		}
	}

	#[test]
	fn test_desugar_loop_with_nested_structures()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Loop {
			body: Block {
				stmts: vec![Stmt::IfVar {
					pattern: Pattern::Wildcard,
					expr: ident("x"),
					then_block: Block {
						stmts: vec![Stmt::Break],
						tail_expr: None,
					},
					else_branch: None,
				}],
				tail_expr: None,
			},
		};

		let output = desugarer.desugar_stmt(stmt);

		match output {
			Stmt::Loop { body } => {
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
				name: vec!["i".into()],
				iter: ident("range"),
				body: Block {
					stmts: vec![],
					tail_expr: None,
				},
			}],
			tail_expr: None,
		});

		let output = desugarer.desugar_stmt(stmt);

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
				tail_expr: Some(Box::new(Expr::Literal(Literal::Int(1)))),
			}))],
		};

		let output = desugarer.desugar_expr(expr);

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
			path: vec!["Point".into()],
			fields: vec![
				(
					"x".into(),
					Expr::Block(Box::new(Block {
						stmts: vec![],
						tail_expr: Some(Box::new(Expr::Literal(Literal::Int(1)))),
					})),
				),
				("y".into(), Expr::Literal(Literal::Int(2))),
			],
		};

		let output = desugarer.desugar_expr(expr);

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
				pattern: Pattern::Wildcard,
				body: SwitchBody::Block(Block {
					stmts: vec![Stmt::For {
						name: vec!["i".into()],
						iter: ident("items"),
						body: Block {
							stmts: vec![],
							tail_expr: None,
						},
					}],
					tail_expr: None,
				}),
			}],
		};

		let output = desugarer.desugar_expr(expr);

		match output {
			Expr::Switch { arms, .. } => {
				match &arms[0].body {
					SwitchBody::Block(block) => {
						// For loop should be desugared
						assert!(matches!(block.stmts[0], Stmt::Block(_)));
					}
					_ => panic!("Expected block body"),
				}
			}
			_ => panic!("Expected switch expression"),
		}
	}

	#[test]
	fn test_desugar_array_literal_list()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Array(ArrayLiteral::List(vec![
			Expr::Binary {
				op: BinaryOp::Add,
				lhs: Box::new(Expr::Literal(Literal::Int(1))),
				rhs: Box::new(Expr::Literal(Literal::Int(2))),
			},
			Expr::Literal(Literal::Int(3)),
		]));

		let output = desugarer.desugar_expr(expr);

		match output {
			Expr::Array(ArrayLiteral::List(items)) => {
				assert_eq!(items.len(), 2);
			}
			_ => panic!("Expected array literal"),
		}
	}

	#[test]
	fn test_desugar_array_repeat_complex()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Array(ArrayLiteral::Repeat {
			value: vec![Expr::Binary {
				op: BinaryOp::Mul,
				lhs: Box::new(ident("x")),
				rhs: Box::new(Expr::Literal(Literal::Int(2))),
			}],
			count: Box::new(Expr::Literal(Literal::Int(10))),
		});

		let output = desugarer.desugar_expr(expr);

		match output {
			Expr::Array(ArrayLiteral::Repeat { value, .. }) => {
				assert_eq!(value.len(), 1);
				assert!(matches!(value[0], Expr::Binary { .. }));
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
			}),
			name: "field".into(),
		};

		let output = desugarer.desugar_expr(expr);

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
				rhs: Box::new(Expr::Literal(Literal::Int(1))),
			}),
		};

		let output = desugarer.desugar_expr(expr);

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

		let expr = Expr::Tuple(vec![
			Expr::Literal(Literal::Int(1)),
			Expr::Block(Box::new(Block {
				stmts: vec![],
				tail_expr: Some(Box::new(Expr::Literal(Literal::Int(2)))),
			})),
			Expr::Literal(Literal::Int(3)),
		]);

		let output = desugarer.desugar_expr(expr);

		match output {
			Expr::Tuple(items) => {
				assert_eq!(items.len(), 3);
				assert!(matches!(items[1], Expr::Block(_)));
			}
			_ => panic!("Expected tuple"),
		}
	}

	#[test]
	fn test_desugar_cast_with_nested_expr()
	{
		let mut desugarer = Desugarer::new();

		let expr = Expr::Cast {
			ty: Box::new(Type {
				modifiers: vec![],
				core: Box::new(TypeCore::Base {
					path: vec!["i64".into()],
					generics: vec![],
				}),
			}),
			expr: Box::new(Expr::Binary {
				op: BinaryOp::Add,
				lhs: Box::new(ident("a")),
				rhs: Box::new(ident("b")),
			}),
		};

		let output = desugarer.desugar_expr(expr);

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
			}),
		};

		let output = desugarer.desugar_expr(expr);

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
			path: vec!["Some".into()],
			args: vec![Pattern::Tuple(vec![
				Pattern::Wildcard,
				Pattern::TypedIdentifier {
					name: "x".into(),
					ty: Type {
						modifiers: vec![],
						core: Box::new(TypeCore::Base {
							path: vec!["i32".into()],
							generics: vec![],
						}),
					},
				},
			])],
		};

		let output = desugarer.desugar_pattern(pattern);

		match output {
			Pattern::Variant { args, .. } => {
				assert_eq!(args.len(), 1);
				assert!(matches!(args[0], Pattern::Tuple(_)));
			}
			_ => panic!("Expected variant pattern"),
		}
	}

	#[test]
	fn test_desugar_pattern_or_with_variants()
	{
		let mut desugarer = Desugarer::new();

		let pattern = Pattern::Or(vec![
			Pattern::Variant {
				path: vec!["Some".into()],
				args: vec![Pattern::Wildcard],
			},
			Pattern::Variant {
				path: vec!["None".into()],
				args: vec![],
			},
		]);

		let output = desugarer.desugar_pattern(pattern);

		match output {
			Pattern::Or(patterns) => {
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
					name: vec!["i".into()],
					iter: ident("range1"),
					body: Block {
						stmts: vec![],
						tail_expr: None,
					},
				},
				Stmt::For {
					name: vec!["j".into()],
					iter: ident("range2"),
					body: Block {
						stmts: vec![],
						tail_expr: None,
					},
				},
			],
			tail_expr: None,
		};

		let output = desugarer.desugar_block(block);

		assert_eq!(output.stmts.len(), 2);
		assert!(matches!(output.stmts[0], Stmt::Block(_)));
		assert!(matches!(output.stmts[1], Stmt::Block(_)));
	}

	#[test]
	fn test_desugar_variable_decl_with_pattern()
	{
		let mut desugarer = Desugarer::new();

		let var = VariableDecl {
			pattern: Pattern::Tuple(vec![
				Pattern::TypedIdentifier {
					name: "x".into(),
					ty: Type {
						modifiers: vec![],
						core: Box::new(TypeCore::Base {
							path: vec!["i32".into()],
							generics: vec![],
						}),
					},
				},
				Pattern::Wildcard,
			]),
			init: Some(Expr::Literal(Literal::Int(42))),
			comp_const: false,
		};

		let output = desugarer.desugar_variable_decl(var);

		match output.pattern {
			Pattern::Tuple(patterns) => {
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
				path: vec!["Point".into()],
				fields: vec![(
					"x".into(),
					Pattern::TypedIdentifier {
						name: "x_val".into(),
						ty: Type {
							modifiers: vec![],
							core: Box::new(TypeCore::Base {
								path: vec!["i32".into()],
								generics: vec![],
							}),
						},
					},
				)],
			},
			expr: ident("point"),
			then_block: Block {
				stmts: vec![],
				tail_expr: None,
			},
			else_branch: None,
		};

		let output = desugarer.desugar_stmt(stmt);

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
			pattern: Pattern::Variant {
				path: vec!["Ok".into()],
				args: vec![Pattern::Tuple(vec![
					Pattern::Wildcard,
					Pattern::TypedIdentifier {
						name: "val".into(),
						ty: Type {
							modifiers: vec![],
							core: Box::new(TypeCore::Base {
								path: vec!["i32".into()],
								generics: vec![],
							}),
						},
					},
				])],
			},
			expr: ident("result"),
			body: Block {
				stmts: vec![],
				tail_expr: None,
			},
		};

		let output = desugarer.desugar_stmt(stmt);

		match output {
			Stmt::Loop { body } => {
				assert_eq!(body.stmts.len(), 2);
			}
			_ => panic!("Expected loop"),
		}
	}
}
