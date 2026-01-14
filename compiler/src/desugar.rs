mod tests;

use crate::{
	CompileError,
	lexer::{Span, Spanned},
	parser::{
		ArrayLiteral, Block, BlockContent, CallType, DirectiveNode, Expr, FuncBound, FunctionDecl, FunctionSignature,
		Ident, ImplDecl, ImplItem, NamespaceDecl, Param, Path, Pattern, Program, RangeExpr, Stmt, SwitchArm,
		SwitchBody, TopLevelDecl, TraitDecl, TraitItem, Type, TypeCore, VariableDecl, WhereBound, WhereConstraint,
	},
	source_map::SourceIndex,
};

#[derive(Debug, Default)]
pub struct Desugarer
{
	tmp_counter: usize,
	source_index: SourceIndex,
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
	pub source_index: SourceIndex,
	pub kind: DesugarErrorKind,
	pub context: Vec<String>,
}

impl DesugarError
{
	pub const fn new(span: Span, kind: DesugarErrorKind, source_index: SourceIndex) -> Self
	{
		return Self {
			span,
			source_index,
			kind,
			context: Vec::new(),
		};
	}

	pub fn with_context(mut self, ctx: impl Into<String>) -> Self
	{
		self.context.push(ctx.into());
		return self;
	}

	pub fn invalid_constructor_type(span: Span, reason: impl Into<String>, source_index: SourceIndex) -> Self
	{
		return Self::new(
			span,
			DesugarErrorKind::InvalidConstructorType { reason: reason.into() },
			source_index,
		);
	}

	pub fn invalid_pattern(span: Span, reason: impl Into<String>, source_index: SourceIndex) -> Self
	{
		return Self::new(
			span,
			DesugarErrorKind::InvalidPattern { reason: reason.into() },
			source_index,
		);
	}

	pub fn generic(span: Span, message: impl Into<String>, source_index: SourceIndex) -> Self
	{
		return Self::new(
			span,
			DesugarErrorKind::Generic {
				message: message.into(),
			},
			source_index,
		);
	}

	pub fn write(&self, f: &mut impl std::fmt::Write, source_map: &crate::source_map::SourceMap) -> std::fmt::Result
	{
		return write!(
			f,
			"{}",
			self.span
				.format_error(&source_map.get(self.source_index).src, &format!("{self}"))
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

		let mut new_params: Vec<Param> = Vec::new();
		let mut body_stmts: Vec<Stmt> = Vec::new();

		for param in func.signature.params {
			let param_span: Span = param.span();
			match param.pattern {
				Pattern::TypedIdentifier { .. } => {
					new_params.push(param);
				}

				other_pattern => {
					let temp: Ident = self.gen_temp("param");

					new_params.push(Param {
						pattern: Pattern::TypedIdentifier {
							path: Path::simple(vec![temp.clone()], param_span),
							ty: param.ty.clone(),
							call_constructor: None,
							span: param_span,
						},
						mutable: param.mutable,
						ty: param.ty,
						span: param_span,
					});

					body_stmts.push(Stmt::VariableDecl(VariableDecl {
						pattern: other_pattern,
						init: Some(Expr::Identifier {
							path: Path::simple(vec![temp], param_span),
							span: param_span,
						}),
						comp_const: false,
						span: param_span,
					}));
				}
			}
		}

		func.signature.params = new_params;
		func.signature = self.desugar_function_signature(func.signature)?;

		if let Some(body) = func.body {
			let mut new_body: Block = body;
			body_stmts.extend(new_body.stmts);
			new_body.stmts = body_stmts;
			func.body = Some(self.desugar_block(new_body)?);
		}

		return Ok(func);
	}

	fn desugar_function_signature(&self, mut func_sig: FunctionSignature) -> Result<FunctionSignature, CompileError>
	{
		let generics_with_bounds: Vec<&String> = func_sig
			.generics
			.iter()
			.filter(|g| return g.has_bounds())
			.map(|g| return &g.name)
			.collect();

		let heap_generics_with_bounds: Vec<&String> = func_sig
			.heap_generics
			.iter()
			.filter(|g| return g.has_bounds())
			.map(|g| return &g.name)
			.collect();

		for where_constraint in &func_sig.where_clause {
			let mentioned_types: Vec<Ident> = get_mentioned_type_params(&where_constraint.ty);

			let mentioned_in_args: Vec<Ident> = where_constraint
				.type_args
				.iter()
				.flat_map(|ty| return get_mentioned_type_params_in_type(ty))
				.collect();

			let all_mentioned: Vec<Ident> = mentioned_types.into_iter().chain(mentioned_in_args).collect();

			for type_param in all_mentioned {
				if generics_with_bounds.contains(&&type_param) {
					return Err(CompileError::DesugarError(DesugarError::generic(
						where_constraint.span,
						format!(
							"type parameter `{}` has bounds in generic parameter list but is also used in where clause. \
								 Move all bounds for `{}` to the where clause instead.",
							type_param, type_param
						),
						self.source_index,
					)));
				}

				if heap_generics_with_bounds.contains(&&type_param) {
					return Err(CompileError::DesugarError(DesugarError::generic(
						where_constraint.span,
						format!(
							"heap generic type parameter `{}` has bounds in generic parameter list but is also used in where clause. \
								 Move all bounds for `{}` to the where clause instead.",
							type_param, type_param
						),
						self.source_index,
					)));
				}
			}
		}

		for generic in &func_sig.generics {
			if generic.bounds.is_empty() {
				continue;
			}

			func_sig.where_clause.push(WhereConstraint {
				ty: Path {
					segments: vec![generic.name.clone()],
					generics: Vec::new(),
					span: generic.span,
				},
				bounds: generic.bounds.clone(),
				type_args: Vec::new(),
				span: generic.span,
			});
		}

		for generic in &mut func_sig.generics {
			generic.bounds.clear();
		}

		for generic in &func_sig.heap_generics {
			if generic.bounds.is_empty() {
				continue;
			}

			func_sig.where_clause.push(WhereConstraint {
				ty: Path {
					segments: vec![generic.name.clone()],
					generics: Vec::new(),
					span: generic.span,
				},
				bounds: generic.bounds.clone(),
				type_args: Vec::new(),
				span: generic.span,
			});
		}

		for generic in &mut func_sig.heap_generics {
			generic.bounds.clear();
		}

		return Ok(func_sig);
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
		impl_decl = self.desugar_impl_generics(impl_decl)?;

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

	fn desugar_impl_generics(&self, mut impl_decl: ImplDecl) -> Result<ImplDecl, CompileError>
	{
		let generics_with_bounds: Vec<&String> = impl_decl
			.generics
			.iter()
			.filter(|g| return g.has_bounds())
			.map(|g| return &g.name)
			.collect();

		for where_constraint in &impl_decl.where_clause {
			let mentioned_types = get_mentioned_type_params(&where_constraint.ty);

			let mentioned_in_args: Vec<String> = where_constraint
				.type_args
				.iter()
				.flat_map(|ty| return get_mentioned_type_params_in_type(ty))
				.collect();

			let all_mentioned: Vec<String> = mentioned_types.into_iter().chain(mentioned_in_args).collect();

			for type_param in all_mentioned {
				if generics_with_bounds.contains(&&type_param) {
					return Err(CompileError::DesugarError(DesugarError::generic(
						where_constraint.span,
						format!(
							"type parameter `{}` has bounds in generic parameter list but is also used in where clause. \
								 Move all bounds for `{}` to the where clause instead.",
							type_param, type_param
						),
						self.source_index,
					)));
				}
			}
		}

		for generic in &impl_decl.generics {
			if generic.bounds.is_empty() {
				continue;
			}

			impl_decl.where_clause.push(WhereConstraint {
				ty: Path {
					segments: vec![generic.name.clone()],
					generics: Vec::new(),
					span: generic.span,
				},
				bounds: generic.bounds.clone(),
				type_args: Vec::new(),
				span: generic.span,
			});
		}

		for generic in &mut impl_decl.generics {
			generic.bounds.clear();
		}

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
				pattern,
				iter,
				body,
				span,
			} => self.desugar_for_loop(label, pattern, iter, body, span)?,

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
			} => self.desugar_while_loop(label, cond, body, span)?,

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
		pattern: Pattern,
		iter: Expr,
		body: Block,
		span: Span,
	) -> Result<Stmt, CompileError>
	{
		let iter_temp: Ident = self.gen_temp("loop");
		let iter_span: Span = iter.span();
		let pattern_span: Span = pattern.span();

		let desugared_iter: Expr = self.desugar_expr(iter)?;

		let actual_label: Ident = self.push_loop(label);
		let desugared_body: Block = self.desugar_block(body)?;

		let desugared_pattern: Pattern = self.desugar_pattern(pattern)?;

		let iter_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![iter_temp.clone()], iter_span),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], iter_span),
						generics: vec![],
					}),
					span: iter_span,
				},
				call_constructor: None,
				span: iter_span,
			},
			init: Some(desugared_iter),
			comp_const: false,
			span: iter_span,
		});

		let next_call: Expr = Expr::Call {
			callee: Box::new(Expr::Field {
				base: Box::new(Expr::Identifier {
					path: Path::simple(vec![iter_temp], iter_span),
					span: iter_span,
				}),
				name: "next".to_string(),
				span: iter_span,
			}),
			call_type: CallType::CompilerHeap,
			named_generics: Vec::new(),
			args: vec![],
			span: iter_span,
		};

		let some_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Variant {
				path: Path::simple(vec!["Some".to_string()], pattern_span),
				args: vec![desugared_pattern],
				span: pattern_span,
			},
			body: SwitchBody::Block(desugared_body),
			span,
		};

		let some_false_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Variant {
				path: Path::simple(vec!["Some".to_string()], pattern_span),
				args: vec![Pattern::Wildcard { span: pattern_span }],
				span: pattern_span,
			},
			body: SwitchBody::Block(Block {
				stmts: vec![Stmt::Continue {
					label: Some(actual_label.clone()),
					span: pattern_span,
				}],
				tail_expr: None,
				span: pattern_span,
			}),
			span: pattern_span,
		};

		let none_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Variant {
				path: Path::simple(vec!["None".to_string()], pattern_span),
				args: vec![],
				span: pattern_span,
			},
			body: SwitchBody::Block(Block {
				stmts: vec![Stmt::Break {
					label: Some(actual_label.clone()),
					value: None,
					span: pattern_span,
				}],
				tail_expr: None,
				span: pattern_span,
			}),
			span: pattern_span,
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(next_call),
			arms: vec![some_arm, some_false_arm, none_arm],
			span,
		};

		let loop_stmt: Stmt = Stmt::Loop {
			label: Some(actual_label),
			body: Block {
				stmts: vec![Stmt::Expr(switch_expr)],
				tail_expr: None,
				span,
			},
			span,
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
		let pattern_span: Span = pattern.span();

		let desugared_expr: Expr = self.desugar_expr(expr)?;
		let desugared_then: Block = self.desugar_block(then_block)?;

		let temp_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![temp_var.clone()], expr_span),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], expr_span),
						generics: vec![],
					}),
					span: expr_span,
				},
				call_constructor: None,
				span: expr_span,
			},
			init: Some(desugared_expr),
			comp_const: false,
			span: expr_span,
		});

		let match_arm: SwitchArm = SwitchArm {
			pattern: self.desugar_pattern(pattern)?,
			body: SwitchBody::Block(desugared_then),
			span: pattern_span,
		};

		let else_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard { span: pattern_span },
			body: else_branch.map_or_else(
				|| {
					return Ok(SwitchBody::Block(Block {
						stmts: vec![],
						tail_expr: None,
						span: pattern_span,
					}));
				},
				|else_stmt| {
					let stmt_span = else_stmt.span();
					return Ok(SwitchBody::Block(Block {
						stmts: vec![self.desugar_stmt(*else_stmt)?],
						tail_expr: None,
						span: stmt_span,
					}));
				},
			)?,
			span: pattern_span,
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(Expr::Identifier {
				path: Path::simple(vec![temp_var], expr_span),
				span: expr_span,
			}),
			arms: vec![match_arm, else_arm],
			span,
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
		let pattern_span: Span = pattern.span();

		let desugared_expr: Expr = self.desugar_expr(expr)?;

		let actual_label: Ident = self.push_loop(label);
		let desugared_body: Block = self.desugar_block(body)?;

		let temp_decl = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![temp_var.clone()], expr_span),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], expr_span),
						generics: vec![],
					}),
					span: expr_span,
				},
				call_constructor: None,
				span: expr_span,
			},
			init: Some(desugared_expr),
			comp_const: false,
			span: expr_span,
		});

		let match_arm: SwitchArm = SwitchArm {
			pattern: self.desugar_pattern(pattern)?,
			body: SwitchBody::Block(desugared_body),
			span: pattern_span,
		};

		let break_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard { span: pattern_span },
			body: SwitchBody::Block(Block {
				stmts: vec![Stmt::Break {
					label: Some(actual_label.clone()),
					value: None,
					span: pattern_span,
				}],
				tail_expr: None,
				span: pattern_span,
			}),
			span: pattern_span,
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(Expr::Identifier {
				path: Path::simple(vec![temp_var], expr_span),
				span: expr_span,
			}),
			arms: vec![match_arm, break_arm],
			span,
		};

		let result = Stmt::Loop {
			label: Some(actual_label),
			body: Block {
				stmts: vec![temp_decl, Stmt::Expr(switch_expr)],
				tail_expr: None,
				span,
			},
			span,
		};

		self.pop_loop();

		return Ok(result);
	}

	fn desugar_while_loop(
		&mut self,
		label: Option<String>,
		cond: Expr,
		body: Block,
		span: Span,
	) -> Result<Stmt, CompileError>
	{
		let cond_span: Span = cond.span();

		let desugared_cond: Expr = self.desugar_expr(cond)?;

		let actual_label: Ident = self.push_loop(label);
		let desugared_body: Block = self.desugar_block(body)?;

		let negated_cond = Expr::Unary {
			op: crate::parser::UnaryOp::Not,
			expr: Box::new(desugared_cond),
			span: cond_span,
		};

		let if_break: Stmt = Stmt::If {
			cond: negated_cond,
			then_block: Block {
				stmts: vec![Stmt::Break {
					label: Some(actual_label.clone()),
					value: None,
					span: cond_span,
				}],
				tail_expr: None,
				span: cond_span,
			},
			else_branch: None,
			span: cond_span,
		};

		let mut loop_body_stmts: Vec<Stmt> = vec![if_break];
		loop_body_stmts.extend(desugared_body.stmts);

		let result = Stmt::Loop {
			label: Some(actual_label),
			body: Block {
				stmts: loop_body_stmts,
				tail_expr: desugared_body.tail_expr,
				span,
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
			Pattern::TypedIdentifier { call_constructor, .. } => call_constructor.is_some() && var.init.is_none(),
			_ => false,
		};

		if needs_constructor
			&& let Pattern::TypedIdentifier {
				ty, call_constructor, ..
			} = &var.pattern
			&& call_constructor.is_some()
		{
			var.init = Some(self.type_to_constructor_call(
				ty,
				call_constructor.expect("Because of the checks before this, this should not be none"),
			)?);

			if let Pattern::TypedIdentifier { path, ty, span, .. } = var.pattern.clone() {
				var.pattern = Pattern::TypedIdentifier {
					path,
					ty,
					call_constructor: None,
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

			Expr::Call {
				callee,
				call_type,
				named_generics,
				args,
				span,
			} => Expr::Call {
				callee: Box::new(self.desugar_expr(*callee)?),
				call_type,
				named_generics,
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

			Expr::Range(range_expr) => self.desugar_range(range_expr)?,

			Expr::Identifier { .. } | Expr::Literal { .. } | Expr::Default { .. } => expr,
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
		let pattern_span: Span = pattern.span();

		let desugared_expr: Expr = self.desugar_expr(expr)?;
		let desugared_then: Block = self.desugar_block(then_block)?;

		let temp_decl = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![temp_var.clone()], expr_span),
				ty: Type {
					modifiers: vec![],
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], expr_span),
						generics: vec![],
					}),
					span: expr_span,
				},
				call_constructor: None,
				span: expr_span,
			},
			init: Some(desugared_expr),
			comp_const: false,
			span: expr_span,
		});

		let match_arm: SwitchArm = SwitchArm {
			pattern: self.desugar_pattern(pattern)?,
			body: SwitchBody::Block(desugared_then),
			span: pattern_span,
		};

		let else_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard { span: pattern_span },
			body: else_branch.map_or_else(
				|| {
					return Ok(SwitchBody::Block(Block {
						stmts: vec![],
						tail_expr: None,
						span: pattern_span,
					}));
				},
				|else_expr| {
					let desugared_else = self.desugar_expr(*else_expr)?;

					return Ok(match desugared_else {
						Expr::Block(block) => SwitchBody::Block(*block),
						other_expr => {
							let other_span = other_expr.span();
							SwitchBody::Block(Block {
								stmts: vec![],
								tail_expr: Some(Box::new(other_expr)),
								span: other_span,
							})
						}
					});
				},
			)?,
			span: pattern_span,
		};

		let switch_expr: Expr = Expr::Switch {
			expr: Box::new(Expr::Identifier {
				path: Path::simple(vec![temp_var], expr_span),
				span: expr_span,
			}),
			arms: vec![match_arm, else_arm],
			span,
		};

		return Ok(Expr::Block(Box::new(Block {
			stmts: vec![temp_decl],
			tail_expr: Some(Box::new(switch_expr)),
			span,
		})));
	}

	#[allow(clippy::result_large_err)]
	fn type_to_constructor_call(&self, ty: &Type, call_type: CallType) -> Result<Expr, CompileError>
	{
		let mut path = match ty.core.as_ref() {
			TypeCore::Base { path, .. } => path.clone(),
			_ => {
				return Err(CompileError::DesugarError(DesugarError::invalid_constructor_type(
					ty.span(),
					"only basic types can have a constructor call",
					self.source_index,
				)));
			}
		};

		path.segments.push("create".to_string());

		return Ok(Expr::Call {
			callee: Box::new(Expr::Identifier { path, span: ty.span }),
			call_type,
			named_generics: Vec::new(),
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
				value: Box::new(self.desugar_expr(*value)?),
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

	fn desugar_range(&mut self, expr: RangeExpr) -> Result<Expr, CompileError>
	{
		fn call(path: &[&str], args: Vec<Expr>, span: Span) -> Expr
		{
			return Expr::Call {
				callee: Box::new(Expr::Identifier {
					path: Path {
						segments: path.iter().map(|s| return s.to_string()).collect(),
						generics: Vec::new(),
						span,
					},
					span,
				}),
				call_type: CallType::Regular,
				named_generics: Vec::new(),
				args,
				span,
			};
		}
		let span: Span = expr.span;

		let start: Option<Expr> = expr.start.map(|x| return self.desugar_expr(*x)).transpose()?;
		let end: Option<Expr> = expr.end.map(|x| return self.desugar_expr(*x)).transpose()?;

		return Ok(match (start, end, expr.inclusive) {
			// a..b
			(Some(a), Some(b), false) => call(&["Range", "new"], vec![a, b], span),

			// a..=b
			(Some(a), Some(b), true) => call(&["RangeInclusive", "new"], vec![a, b], span),

			// a..
			(Some(a), None, _) => call(&["RangeFrom", "new"], vec![a], span),

			// ..b
			(None, Some(b), false) => call(&["RangeTo", "new"], vec![b], span),

			// ..=b
			(None, Some(b), true) => call(&["RangeToInclusive", "new"], vec![b], span),

			// ..
			(None, None, _) => call(&["RangeFull", "new"], vec![], span),
		});
	}
}

fn get_mentioned_type_params(path: &Path) -> Vec<String>
{
	let mut result = Vec::new();

	if path.segments.len() == 1 && path.generics.is_empty() {
		result.push(path.segments[0].clone());
	}

	for generic_type in &path.generics {
		result.extend(get_mentioned_type_params_in_type(generic_type));
	}

	return result;
}

fn get_mentioned_type_params_in_type(ty: &Type) -> Vec<String>
{
	match ty.core.as_ref() {
		TypeCore::Base { path, generics } => {
			let mut result = Vec::new();

			if path.segments.len() == 1 && generics.is_empty() && path.generics.is_empty() {
				result.push(path.segments[0].clone());
			}

			for generic_type in generics {
				result.extend(get_mentioned_type_params_in_type(generic_type));
			}

			for generic_type in &path.generics {
				result.extend(get_mentioned_type_params_in_type(generic_type));
			}

			return result;
		}
		TypeCore::Reference { inner, .. } | TypeCore::Mutable { inner } | TypeCore::Pointer { inner } => {
			return get_mentioned_type_params_in_type_core(inner);
		}
		TypeCore::Array { inner, size: _ } => {
			let result = get_mentioned_type_params_in_type_core(inner);

			return result;
		}
		TypeCore::Tuple(types) => return types.iter().flat_map(get_mentioned_type_params_in_type).collect(),
		TypeCore::ImplTrait { bounds } => {
			return bounds
				.iter()
				.flat_map(|bound| match bound {
					WhereBound::Path(path) => return get_mentioned_type_params(path),
					WhereBound::Func(func_bound) => match func_bound {
						FuncBound::Fn { args, ret } => {
							let mut result: Vec<String> =
								args.iter().flat_map(get_mentioned_type_params_in_type).collect();

							if let Some(ret_ty) = ret {
								result.extend(get_mentioned_type_params_in_type(ret_ty));
							}

							return result;
						}
					},
				})
				.collect();
		}
	}
}

fn get_mentioned_type_params_in_type_core(core: &TypeCore) -> Vec<String>
{
	match core {
		TypeCore::Base { path, generics } => {
			let mut result = Vec::new();

			if path.segments.len() == 1 && generics.is_empty() && path.generics.is_empty() {
				result.push(path.segments[0].clone());
			}

			for generic_type in generics {
				result.extend(get_mentioned_type_params_in_type(generic_type));
			}

			for generic_type in &path.generics {
				result.extend(get_mentioned_type_params_in_type(generic_type));
			}

			return result;
		}
		TypeCore::Reference { inner, .. }
		| TypeCore::Mutable { inner }
		| TypeCore::Pointer { inner }
		| TypeCore::Array { inner, .. } => return get_mentioned_type_params_in_type_core(inner),
		TypeCore::Tuple(types) => return types.iter().flat_map(get_mentioned_type_params_in_type).collect(),
		TypeCore::ImplTrait { .. } => return Vec::new(), // Already handled above
	}
}
