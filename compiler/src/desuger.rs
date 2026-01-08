use crate::{
	CompileError,
	lexer::Span,
	parser::{
		ArrayLiteral, Block, BlockContent, DirectiveNode, Expr, FunctionDecl, Ident, ImplDecl, ImplItem, NamespaceDecl,
		Path, Pattern, Program, Spanned, Stmt, SwitchArm, SwitchBody, TopLevelDecl, TraitDecl, TraitItem, Type,
		TypeCore, VariableDecl,
	},
};

#[derive(Debug, Default)]
pub struct Desugarer
{
	tmp_counter: usize,
	loop_stack: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum DesugarErrorKind
{
	InvalidConstructorType
	{
		reason: String
	},
	InvalidPattern
	{
		reason: String
	},
	Generic
	{
		message: String
	},
}

#[derive(Debug, Clone)]
pub struct DesugarError
{
	pub span: Span,
	pub kind: DesugarErrorKind,
	pub context: Vec<String>,
}

impl DesugarError
{
	pub const fn new(span: Span, kind: DesugarErrorKind) -> Self
	{
		return Self {
			span,
			kind,
			context: Vec::new(),
		};
	}

	pub fn with_context(mut self, ctx: impl Into<String>) -> Self
	{
		self.context.push(ctx.into());
		return self;
	}

	pub fn invalid_constructor_type(span: Span, reason: impl Into<String>) -> Self
	{
		return Self::new(span, DesugarErrorKind::InvalidConstructorType { reason: reason.into() });
	}

	pub fn invalid_pattern(span: Span, reason: impl Into<String>) -> Self
	{
		return Self::new(span, DesugarErrorKind::InvalidPattern { reason: reason.into() });
	}

	pub fn generic(span: Span, message: impl Into<String>) -> Self
	{
		return Self::new(
			span,
			DesugarErrorKind::Generic {
				message: message.into(),
			},
		);
	}
}

impl std::fmt::Display for DesugarError
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "Desugar error at {:?}: ", self.span)?;

		match &self.kind {
			DesugarErrorKind::InvalidConstructorType { reason } => {
				write!(f, "invalid constructor type: {}", reason)?;
			}
			DesugarErrorKind::InvalidPattern { reason } => {
				write!(f, "invalid pattern: {}", reason)?;
			}
			DesugarErrorKind::Generic { message } => {
				write!(f, "{}", message)?;
			}
		}

		if !self.context.is_empty() {
			write!(f, "\n  while desugaring: {}", self.context.join(" → "))?;
		}

		return Ok(());
	}
}

impl std::error::Error for DesugarError {}

impl Desugarer
{
	pub fn new() -> Self
	{
		return Desugarer::default();
	}

	fn gen_temp(&mut self, name: &str) -> Ident
	{
		let new_name: Ident = format!("#__tmp_{}_{}", self.tmp_counter, name);
		self.tmp_counter += 1;
		return new_name;
	}

	fn gen_loop_label(&self) -> Ident
	{
		return format!("#__loop_{}", self.loop_stack.len());
	}

	fn push_loop(&mut self, label: Option<String>) -> String
	{
		let actual_label = label.unwrap_or_else(|| return self.gen_loop_label());
		self.loop_stack.push(actual_label.clone());
		return actual_label;
	}

	fn pop_loop(&mut self)
	{
		debug_assert!(!self.loop_stack.is_empty(), "Popping empty loop stack");
		self.loop_stack.pop();
	}

	fn current_loop(&self) -> Option<&String>
	{
		return self.loop_stack.last();
	}

	#[allow(clippy::result_large_err)]
	pub fn desugar_program(&mut self, program: Program) -> Result<Program, CompileError>
	{
		let items: Vec<TopLevelDecl> = program
			.items
			.into_iter()
			.map(|item| return self.desugar_top_level_decl(item))
			.collect::<Result<Vec<_>, _>>()?;

		return Ok(Program {
			items,
			span: program.span,
		});
	}

	#[allow(clippy::result_large_err)]
	fn desugar_top_level_decl(&mut self, decl: TopLevelDecl) -> Result<TopLevelDecl, CompileError>
	{
		return Ok(match decl {
			TopLevelDecl::Function(func) => TopLevelDecl::Function(self.desugar_function(func)?),
			TopLevelDecl::Namespace(ns) => TopLevelDecl::Namespace(self.desugar_namespace(ns)?),
			TopLevelDecl::Impl(impl_decl) => TopLevelDecl::Impl(self.desugar_impl(impl_decl)?),
			TopLevelDecl::Trait(trait_decl) => TopLevelDecl::Trait(self.desugar_trait(trait_decl)?),
			TopLevelDecl::Directive(d) => TopLevelDecl::Directive(self.desugar_directive_node(d)?),
			TopLevelDecl::VariableDecl(var) => TopLevelDecl::VariableDecl(self.desugar_variable_decl(var)?),
			TopLevelDecl::Struct(s) => TopLevelDecl::Struct(s),
			TopLevelDecl::Union(u) => TopLevelDecl::Union(u),
			TopLevelDecl::Enum(e) => TopLevelDecl::Enum(e),
			TopLevelDecl::Variant(v) => TopLevelDecl::Variant(v),
			TopLevelDecl::TypeAlias(t) => TopLevelDecl::TypeAlias(t),
		});
	}

	#[allow(clippy::result_large_err)]
	fn desugar_function(&mut self, mut func: FunctionDecl) -> Result<FunctionDecl, CompileError>
	{
		debug_assert!(self.loop_stack.is_empty(), "loop_stack should be empty");

		if let Some(body) = func.body {
			func.body = Some(self.desugar_block(body)?);
		}
		return Ok(func);
	}

	#[allow(clippy::result_large_err)]
	fn desugar_namespace(&mut self, mut ns: NamespaceDecl) -> Result<NamespaceDecl, CompileError>
	{
		ns.body = self.desugar_program(ns.body)?;
		return Ok(ns);
	}

	#[allow(clippy::result_large_err)]
	fn desugar_impl(&mut self, mut impl_decl: ImplDecl) -> Result<ImplDecl, CompileError>
	{
		impl_decl.body = impl_decl
			.body
			.into_iter()
			.map(|item| {
				return Ok(match item {
					ImplItem::Function(func) => ImplItem::Function(self.desugar_function(func)?),
					ImplItem::TypeAlias(t) => ImplItem::TypeAlias(t),
					ImplItem::Const(c) => ImplItem::Const(self.desugar_variable_decl(c)?),
				});
			})
			.collect::<Result<Vec<_>, _>>()?;
		return Ok(impl_decl);
	}

	#[allow(clippy::result_large_err)]
	fn desugar_trait(&mut self, mut trait_decl: TraitDecl) -> Result<TraitDecl, CompileError>
	{
		trait_decl.items = trait_decl
			.items
			.into_iter()
			.map(|item| {
				return Ok(match item {
					TraitItem::Function { signature, body, span } => {
						debug_assert!(self.loop_stack.is_empty());
						let desugared_body = body.map(|b| return self.desugar_block(b)).transpose()?;
						return Ok(TraitItem::Function {
							signature,
							body: desugared_body,
							span,
						});
					}
					TraitItem::TypeAlias(t) => TraitItem::TypeAlias(t),
					TraitItem::Const(c) => TraitItem::Const(self.desugar_variable_decl(c)?),
				});
			})
			.collect::<Result<Vec<_>, _>>()?;
		return Ok(trait_decl);
	}

	#[allow(clippy::result_large_err)]
	fn desugar_directive_node(&mut self, mut directive: DirectiveNode) -> Result<DirectiveNode, CompileError>
	{
		directive.body = directive
			.body
			.map(|body| return self.desugar_block_content(body))
			.transpose()?;
		return Ok(directive);
	}

	#[allow(clippy::result_large_err)]
	fn desugar_block(&mut self, block: Block) -> Result<Block, CompileError>
	{
		let stmts: Vec<Stmt> = block
			.stmts
			.into_iter()
			.map(|stmt| return self.desugar_stmt(stmt))
			.collect::<Result<Vec<_>, _>>()?;

		let tail_expr = block
			.tail_expr
			.map(|expr| return Ok(Box::new(self.desugar_expr(*expr)?)))
			.transpose()?;

		return Ok(Block {
			stmts,
			tail_expr,
			span: block.span,
		});
	}

	#[allow(clippy::result_large_err)]
	fn desugar_block_content(&mut self, content: BlockContent) -> Result<BlockContent, CompileError>
	{
		return Ok(match content {
			BlockContent::Block(block) => BlockContent::Block(self.desugar_block(block)?),
			BlockContent::TopLevelBlock(block) => BlockContent::TopLevelBlock(self.desugar_program(block)?),
		});
	}

	#[allow(clippy::result_large_err)]
	fn desugar_stmt(&mut self, stmt: Stmt) -> Result<Stmt, CompileError>
	{
		return Ok(match stmt {
			Stmt::For {
				label,
				name,
				iter,
				body,
				span,
			} => self.desugar_for_loop(label, name, iter, body, span)?,

			Stmt::If {
				cond,
				then_block,
				else_branch,
				span,
			} => Stmt::If {
				cond: self.desugar_expr(cond)?,
				then_block: self.desugar_block(then_block)?,
				else_branch: else_branch
					.map(|stmt| return Ok(Box::new(self.desugar_stmt(*stmt)?)))
					.transpose()?,
				span,
			},

			Stmt::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
				span,
			} => self.desugar_if_var(pattern, expr, then_block, else_branch, span)?,

			Stmt::While {
				label,
				cond,
				body,
				span,
			} => {
				let actual_label = self.push_loop(label);
				let desugared = Stmt::While {
					label: Some(actual_label),
					cond: self.desugar_expr(cond)?,
					body: self.desugar_block(body)?,
					span,
				};
				self.pop_loop();
				desugared
			}

			Stmt::Loop { label, body, span } => {
				let actual_label = self.push_loop(label);
				let desugared = Stmt::Loop {
					label: Some(actual_label),
					body: self.desugar_block(body)?,
					span,
				};
				self.pop_loop();
				desugared
			}

			Stmt::WhileVarLoop {
				label,
				pattern,
				expr,
				body,
				span,
			} => self.desugar_while_var_loop(label, pattern, expr, body, span)?,

			Stmt::VariableDecl(var) => Stmt::VariableDecl(self.desugar_variable_decl(var)?),

			Stmt::Assignment {
				target,
				op,
				value,
				span,
			} => Stmt::Assignment {
				target: self.desugar_expr(target)?,
				op,
				value: self.desugar_expr(value)?,
				span,
			},

			Stmt::Return { value, span } => Stmt::Return {
				value: value.map(|e| return self.desugar_expr(e)).transpose()?,
				span,
			},

			Stmt::Expr(expr) => Stmt::Expr(self.desugar_expr(expr)?),

			Stmt::Break { label, value, span } => {
				let actual_label = label.or_else(|| return self.current_loop().cloned());
				Stmt::Break {
					label: actual_label,
					value: value.map(|v| return self.desugar_expr(v)).transpose()?,
					span,
				}
			}

			Stmt::Continue { label, span } => {
				let actual_label = label.or_else(|| return self.current_loop().cloned());
				Stmt::Continue {
					label: actual_label,
					span,
				}
			}

			Stmt::Unsafe(block) => Stmt::Unsafe(self.desugar_block(block)?),
			Stmt::Block(block) => Stmt::Block(self.desugar_block(block)?),

			Stmt::Directive(directive) => Stmt::Directive(self.desugar_directive_node(directive)?),

			Stmt::Delete { expr, span } => Stmt::Delete {
				expr: self.desugar_expr(expr)?,
				span,
			},
		});
	}

	#[allow(clippy::result_large_err)]
	fn desugar_for_loop(
		&mut self,
		label: Option<String>,
		name: Path,
		iter: Expr,
		body: Block,
		span: Span,
	) -> Result<Stmt, CompileError>
	{
		let iter_temp: Ident = self.gen_temp("loop");
		let iter_span: Span = iter.span();

		let desugared_iter: Expr = self.desugar_expr(iter)?;

		let actual_label: Ident = self.push_loop(label);
		let desugared_body: Block = self.desugar_block(body)?;

		let name_pattern: Pattern = if name.segments.len() == 1 {
			Pattern::TypedIdentifier {
				path: Path::simple(vec![name.segments[0].clone()], Span::default()),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], Span::default()),
						generics: vec![],
					}),
					span: Span::default(),
				},
				call_constructor: false,
				span: Span::default(),
			}
		} else {
			Pattern::Variant {
				path: name,
				args: vec![],
				span: Span::default(),
			}
		};

		let iter_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![iter_temp.clone()], Span::default()),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], Span::default()),
						generics: vec![],
					}),
					span: Span::default(),
				},
				call_constructor: false,
				span: Span::default(),
			},
			init: Some(desugared_iter),
			comp_const: false,
			span: iter_span,
		});

		let next_call: Expr = Expr::Call {
			callee: Box::new(Expr::Field {
				base: Box::new(Expr::Identifier {
					path: Path::simple(vec![iter_temp], Span::default()),
					span: Span::default(),
				}),
				name: "next".to_string(),
				span: Span::default(),
			}),
			args: vec![],
			span: Span::default(),
		};

		let some_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Variant {
				path: Path::simple(vec!["Some".to_string()], Span::default()),
				args: vec![name_pattern],
				span: Span::default(),
			},
			body: SwitchBody::Block(desugared_body),
			span: Span::default(),
		};

		let none_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Variant {
				path: Path::simple(vec!["None".to_string()], Span::default()),
				args: vec![],
				span: Span::default(),
			},
			body: SwitchBody::Block(Block {
				stmts: vec![Stmt::Break {
					label: Some(actual_label.clone()),
					value: None,
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(next_call),
			arms: vec![some_arm, none_arm],
			span: Span::default(),
		};

		let loop_stmt: Stmt = Stmt::Loop {
			label: Some(actual_label),
			body: Block {
				stmts: vec![Stmt::Expr(switch_expr)],
				tail_expr: None,
				span: Span::default(),
			},
			span: Span::default(),
		};

		self.pop_loop();

		return Ok(Stmt::Block(Block {
			stmts: vec![iter_decl, loop_stmt],
			tail_expr: None,
			span,
		}));
	}

	#[allow(clippy::result_large_err)]
	fn desugar_if_var(
		&mut self,
		pattern: Pattern,
		expr: Expr,
		then_block: Block,
		else_branch: Option<Box<Stmt>>,
		span: Span,
	) -> Result<Stmt, CompileError>
	{
		let temp_var: Ident = self.gen_temp("ifvar");
		let expr_span: Span = expr.span();

		let desugared_expr: Expr = self.desugar_expr(expr)?;
		let desugared_then: Block = self.desugar_block(then_block)?;

		let temp_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![temp_var.clone()], Span::default()),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], Span::default()),
						generics: vec![],
					}),
					span: Span::default(),
				},
				call_constructor: false,
				span: Span::default(),
			},
			init: Some(desugared_expr),
			comp_const: false,
			span: expr_span,
		});

		let match_arm: SwitchArm = SwitchArm {
			pattern: self.desugar_pattern(pattern)?,
			body: SwitchBody::Block(desugared_then),
			span: Span::default(),
		};

		let else_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard { span: Span::default() },
			body: else_branch.map_or_else(
				|| {
					return Ok(SwitchBody::Block(Block {
						stmts: vec![],
						tail_expr: None,
						span: Span::default(),
					}));
				},
				|else_stmt| {
					return Ok(SwitchBody::Block(Block {
						stmts: vec![self.desugar_stmt(*else_stmt)?],
						tail_expr: None,
						span: Span::default(),
					}));
				},
			)?,
			span: Span::default(),
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(Expr::Identifier {
				path: Path::simple(vec![temp_var], Span::default()),
				span: Span::default(),
			}),
			arms: vec![match_arm, else_arm],
			span: Span::default(),
		};

		return Ok(Stmt::Block(Block {
			stmts: vec![temp_decl, Stmt::Expr(switch_expr)],
			tail_expr: None,
			span,
		}));
	}

	#[allow(clippy::result_large_err)]
	fn desugar_while_var_loop(
		&mut self,
		label: Option<String>,
		pattern: Pattern,
		expr: Expr,
		body: Block,
		span: Span,
	) -> Result<Stmt, CompileError>
	{
		let temp_var: Ident = self.gen_temp("whilevar");
		let expr_span: Span = expr.span();

		let desugared_expr: Expr = self.desugar_expr(expr)?;

		let actual_label: Ident = self.push_loop(label);
		let desugared_body: Block = self.desugar_block(body)?;

		let temp_decl = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![temp_var.clone()], Span::default()),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], Span::default()),
						generics: vec![],
					}),
					span: Span::default(),
				},
				call_constructor: false,
				span: Span::default(),
			},
			init: Some(desugared_expr),
			comp_const: false,
			span: expr_span,
		});

		let match_arm: SwitchArm = SwitchArm {
			pattern: self.desugar_pattern(pattern)?,
			body: SwitchBody::Block(desugared_body),
			span: Span::default(),
		};

		let break_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard { span: Span::default() },
			body: SwitchBody::Block(Block {
				stmts: vec![Stmt::Break {
					label: Some(actual_label.clone()),
					value: None,
					span: Span::default(),
				}],
				tail_expr: None,
				span: Span::default(),
			}),
			span: Span::default(),
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(Expr::Identifier {
				path: Path::simple(vec![temp_var], Span::default()),
				span: Span::default(),
			}),
			arms: vec![match_arm, break_arm],
			span: Span::default(),
		};

		let result = Stmt::Loop {
			label: Some(actual_label),
			body: Block {
				stmts: vec![temp_decl, Stmt::Expr(switch_expr)],
				tail_expr: None,
				span: Span::default(),
			},
			span,
		};

		self.pop_loop();

		return Ok(result);
	}

	#[allow(clippy::result_large_err)]
	fn desugar_variable_decl(&mut self, mut var: VariableDecl) -> Result<VariableDecl, CompileError>
	{
		let needs_constructor: bool = match &var.pattern {
			Pattern::TypedIdentifier { call_constructor, .. } => *call_constructor && var.init.is_none(),
			_ => false,
		};

		if needs_constructor
			&& let Pattern::TypedIdentifier {
				ty, call_constructor, ..
			} = &var.pattern
			&& *call_constructor
		{
			var.init = Some(self.type_to_constructor_call(ty)?);

			if let Pattern::TypedIdentifier { path, ty, span, .. } = var.pattern.clone() {
				var.pattern = Pattern::TypedIdentifier {
					path,
					ty,
					call_constructor: false,
					span,
				};
			}
		}

		var.init = var.init.map(|init| return self.desugar_expr(init)).transpose()?;
		var.pattern = self.desugar_pattern(var.pattern)?;
		return Ok(var);
	}

	#[allow(clippy::result_large_err)]
	fn desugar_expr(&mut self, expr: Expr) -> Result<Expr, CompileError>
	{
		return Ok(match expr {
			Expr::Unary { op, expr, span } => Expr::Unary {
				op,
				expr: Box::new(self.desugar_expr(*expr)?),
				span,
			},

			Expr::Binary { op, lhs, rhs, span } => Expr::Binary {
				op,
				lhs: Box::new(self.desugar_expr(*lhs)?),
				rhs: Box::new(self.desugar_expr(*rhs)?),
				span,
			},

			Expr::Cast { ty, expr, span } => Expr::Cast {
				ty,
				expr: Box::new(self.desugar_expr(*expr)?),
				span,
			},

			Expr::Call { callee, args, span } => Expr::Call {
				callee: Box::new(self.desugar_expr(*callee)?),
				args: args
					.into_iter()
					.map(|arg| return self.desugar_expr(arg))
					.collect::<Result<Vec<_>, _>>()?,
				span,
			},

			Expr::Field { base, name, span } => Expr::Field {
				base: Box::new(self.desugar_expr(*base)?),
				name,
				span,
			},

			Expr::Index { base, index, span } => Expr::Index {
				base: Box::new(self.desugar_expr(*base)?),
				index: Box::new(self.desugar_expr(*index)?),
				span,
			},

			Expr::Tuple { elements, span } => Expr::Tuple {
				elements: elements
					.into_iter()
					.map(|e| return self.desugar_expr(e))
					.collect::<Result<Vec<_>, _>>()?,
				span,
			},

			Expr::Array(array_lit) => Expr::Array(self.desugar_array_literal(array_lit)?),

			Expr::StructInit { path, fields, span } => Expr::StructInit {
				path,
				fields: fields
					.into_iter()
					.map(|(name, expr)| return Ok((name, self.desugar_expr(expr)?)))
					.collect::<Result<Vec<_>, _>>()?,
				span,
			},

			Expr::Block(block) => Expr::Block(Box::new(self.desugar_block(*block)?)),

			Expr::UnsafeBlock(block) => Expr::UnsafeBlock(Box::new(self.desugar_block(*block)?)),

			Expr::Switch { expr, arms, span } => Expr::Switch {
				expr: Box::new(self.desugar_expr(*expr)?),
				arms: arms
					.into_iter()
					.map(|arm| return self.desugar_switch_arm(arm))
					.collect::<Result<Vec<_>, _>>()?,
				span,
			},

			Expr::If {
				cond,
				then_block,
				else_branch,
				span,
			} => self.desugar_if_expr(*cond, then_block, else_branch, span)?,

			Expr::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
				span,
			} => self.desugar_if_var_expr(pattern, *expr, then_block, else_branch, span)?,

			Expr::Loop { label, body, span } => {
				let actual_label = self.push_loop(label);
				let desugared = Expr::Loop {
					label: Some(actual_label),
					body: Box::new(self.desugar_block(*body)?),
					span,
				};
				self.pop_loop();
				desugared
			}

			Expr::Identifier { .. } | Expr::Literal { .. } | Expr::Range(_) | Expr::Default { .. } => expr,
		});
	}

	#[allow(clippy::result_large_err)]
	fn desugar_if_expr(
		&mut self,
		cond: Expr,
		then_block: Block,
		else_branch: Option<Box<Expr>>,
		span: Span,
	) -> Result<Expr, CompileError>
	{
		return Ok(Expr::If {
			cond: Box::new(self.desugar_expr(cond)?),
			then_block: self.desugar_block(then_block)?,
			else_branch: else_branch
				.map(|e| return Ok(Box::new(self.desugar_expr(*e)?)))
				.transpose()?,
			span,
		});
	}

	#[allow(clippy::result_large_err)]
	fn desugar_if_var_expr(
		&mut self,
		pattern: Pattern,
		expr: Expr,
		then_block: Block,
		else_branch: Option<Box<Expr>>,
		span: Span,
	) -> Result<Expr, CompileError>
	{
		let temp_var: Ident = self.gen_temp("ifvar_expr");
		let expr_span: Span = expr.span();

		let desugared_expr: Expr = self.desugar_expr(expr)?;
		let desugared_then: Block = self.desugar_block(then_block)?;

		let temp_decl = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![temp_var.clone()], Span::default()),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], Span::default()),
						generics: vec![],
					}),
					span: Span::default(),
				},
				call_constructor: false,
				span: Span::default(),
			},
			init: Some(desugared_expr),
			comp_const: false,
			span: expr_span,
		});

		let match_arm: SwitchArm = SwitchArm {
			pattern: self.desugar_pattern(pattern)?,
			body: SwitchBody::Block(desugared_then),
			span: Span::default(),
		};

		let else_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard { span: Span::default() },
			body: else_branch.map_or_else(
				|| {
					return Ok(SwitchBody::Block(Block {
						stmts: vec![],
						tail_expr: None,
						span: Span::default(),
					}));
				},
				|else_expr| {
					let desugared_else = self.desugar_expr(*else_expr)?;

					return Ok(match desugared_else {
						Expr::Block(block) => SwitchBody::Block(*block),
						other_expr => SwitchBody::Block(Block {
							stmts: vec![],
							tail_expr: Some(Box::new(other_expr)),
							span: Span::default(),
						}),
					});
				},
			)?,
			span: Span::default(),
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(Expr::Identifier {
				path: Path::simple(vec![temp_var], Span::default()),
				span: Span::default(),
			}),
			arms: vec![match_arm, else_arm],
			span: Span::default(),
		};

		return Ok(Expr::Block(Box::new(Block {
			stmts: vec![temp_decl],
			tail_expr: Some(Box::new(switch_expr)),
			span,
		})));
	}

	#[allow(clippy::result_large_err)]
	fn type_to_constructor_call(&self, ty: &Type) -> Result<Expr, CompileError>
	{
		let mut path = match ty.core.as_ref() {
			TypeCore::Base { path, .. } => path.clone(),
			_ => {
				return Err(CompileError::DesugarError(DesugarError::invalid_constructor_type(
					ty.span(),
					"only basic types can have a constructor call",
				)));
			}
		};

		path.segments.push("create".to_string());

		return Ok(Expr::Call {
			callee: Box::new(Expr::Identifier { path, span: ty.span }),
			args: vec![],
			span: ty.span,
		});
	}

	#[allow(clippy::result_large_err)]
	fn desugar_array_literal(&mut self, array_lit: ArrayLiteral) -> Result<ArrayLiteral, CompileError>
	{
		return Ok(match array_lit {
			ArrayLiteral::List { elements, span } => ArrayLiteral::List {
				elements: elements
					.into_iter()
					.map(|e| return self.desugar_expr(e))
					.collect::<Result<Vec<_>, _>>()?,
				span,
			},
			ArrayLiteral::Repeat { value, count, span } => ArrayLiteral::Repeat {
				value: value
					.into_iter()
					.map(|e| return self.desugar_expr(e))
					.collect::<Result<Vec<_>, _>>()?,
				count: Box::new(self.desugar_expr(*count)?),
				span,
			},
		});
	}

	#[allow(clippy::result_large_err)]
	fn desugar_switch_arm(&mut self, arm: SwitchArm) -> Result<SwitchArm, CompileError>
	{
		return Ok(SwitchArm {
			pattern: self.desugar_pattern(arm.pattern)?,
			body: match arm.body {
				SwitchBody::Expr(expr) => SwitchBody::Expr(self.desugar_expr(expr)?),
				SwitchBody::Block(block) => SwitchBody::Block(self.desugar_block(block)?),
			},
			span: arm.span,
		});
	}

	#[allow(clippy::result_large_err)]
	fn desugar_pattern(&mut self, pattern: Pattern) -> Result<Pattern, CompileError>
	{
		return Ok(match pattern {
			Pattern::Wildcard { span } => Pattern::Wildcard { span },
			Pattern::Literal { value, span } => Pattern::Literal { value, span },
			Pattern::TypedIdentifier {
				path,
				ty,
				call_constructor,
				span,
			} => Pattern::TypedIdentifier {
				path,
				ty,
				call_constructor,
				span,
			},

			Pattern::Variant { path, args, span } => Pattern::Variant {
				path,
				args: args
					.into_iter()
					.map(|p| return self.desugar_pattern(p))
					.collect::<Result<Vec<_>, _>>()?,
				span,
			},

			Pattern::Tuple { patterns, span } => {
				let desugared: Vec<Pattern> = patterns
					.into_iter()
					.map(|p| return self.desugar_pattern(p))
					.collect::<Result<Vec<_>, _>>()?;

				if desugared.len() == 1 {
					desugared.into_iter().next().expect("len == 1, so should not error")
				} else {
					Pattern::Tuple {
						patterns: desugared,
						span,
					}
				}
			}

			Pattern::Struct { path, fields, span } => Pattern::Struct {
				path,
				fields: fields
					.into_iter()
					.map(|(name, pat)| return Ok((name, self.desugar_pattern(pat)?)))
					.collect::<Result<Vec<_>, _>>()?,
				span,
			},

			Pattern::Range(range) => Pattern::Range(range),

			Pattern::Or { patterns, span } => {
				let mut flattened: Vec<Pattern> = Vec::new();
				for pat in patterns {
					let desugared = self.desugar_pattern(pat)?;
					match desugared {
						Pattern::Or { patterns: inner, .. } => {
							flattened.extend(inner);
						}
						other => flattened.push(other),
					}
				}

				if flattened.len() == 1 {
					flattened.into_iter().next().expect("len == 1, so should not error")
				} else {
					Pattern::Or {
						patterns: flattened,
						span,
					}
				}
			}
		});
	}
}

#[cfg(test)]
mod tests
{
	use super::*;
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
			value: vec![ident("z")],
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
			ArrayLiteral::Repeat { value, .. } => assert_eq!(value.len(), 1),
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
			value: vec![Expr::Binary {
				op: BinaryOp::Mul,
				lhs: Box::new(ident("x")),
				rhs: Box::new(int_lit(2)),
				span: Span::default(),
			}],
			count: Box::new(int_lit(10)),
			span: Span::default(),
		});

		let result = desugarer.desugar_expr(expr).inspect_err(|e| eprintln!("{e}"));
		assert!(result.is_ok());
		let output = result.unwrap();

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
	fn test_unlabeled_loop_gets_label()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Loop {
			label: None,
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
				// Loop should have a generated label
				assert!(label.is_some());
				let loop_label = label.unwrap();
				assert!(loop_label.starts_with("#__loop_"));

				// Break should have the same label
				if let Stmt::Break { label, .. } = &body.stmts[0] {
					assert_eq!(label, &Some(loop_label));
				} else {
					panic!("Expected break statement");
				}
			}
			_ => panic!("Expected loop"),
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

		match output {
			Stmt::While { label, body, .. } => {
				// While should have a generated label
				assert!(label.is_some());
				let loop_label = label.unwrap();
				assert!(loop_label.starts_with("#__loop_"));

				// Continue should have the same label
				if let Stmt::Continue { label, .. } = &body.stmts[0] {
					assert_eq!(label, &Some(loop_label));
				} else {
					panic!("Expected continue statement");
				}
			}
			_ => panic!("Expected while"),
		}
	}

	#[test]
	fn test_nested_loops_get_different_labels()
	{
		let mut desugarer = Desugarer::new();

		let stmt = Stmt::Loop {
			label: None,
			body: Block {
				stmts: vec![Stmt::Loop {
					label: None,
					body: Block {
						stmts: vec![
							Stmt::Break {
								label: None,
								value: None,
								span: Span::default(),
							},
							Stmt::Continue {
								label: None,
								span: Span::default(),
							},
						],
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
				assert!(outer_label.is_some());
				let outer_lbl = outer_label.unwrap();

				if let Stmt::Loop {
					label: inner_label,
					body: inner_body,
					..
				} = &outer_body.stmts[0]
				{
					assert!(inner_label.is_some());
					let inner_lbl = inner_label.as_ref().unwrap();

					// Labels should be different
					assert_ne!(outer_lbl, *inner_lbl);

					// Break and continue should reference the inner loop
					if let Stmt::Break { label, .. } = &inner_body.stmts[0] {
						assert_eq!(label, inner_label);
					}

					if let Stmt::Continue { label, .. } = &inner_body.stmts[1] {
						assert_eq!(label, inner_label);
					}
				} else {
					panic!("Expected inner loop");
				}
			}
			_ => panic!("Expected outer loop"),
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
}
