#![allow(clippy::unused_self)]

#[cfg(test)]
#[path = "../../tests/desugar/tests.rs"]
mod tests;

use leaf_proc::{Spanned, compiler_bug};

use std::fmt::Debug;

use crate::{
	diagnostics::{CompileDiagnostic, DiagnosticBuilder, ErrorCode},
	lexer::{Span, Spanned},
	parser::{
		AST, ArrayLiteral, AssignOp, Block, BlockContent, CallType, Directive, DirectiveNode, Expr, FuncBound,
		FunctionDecl, FunctionSignature, GenericArg, GenericParam, Ident, ImplDecl, ImplItem, ModuleDecl, ModuleKind,
		Param, Path, PathSegment, Pattern, RangeExpr, Stmt, SwitchArm, SwitchBody, TopLevelBlock, TopLevelDecl,
		TraitDecl, TraitItem, Type, TypeCore, VariableDecl, WhereBound, WhereConstraint, extract_type_from_pattern,
	},
	source_map::SourceIndex,
};

/// The root node of the Desugared Abstract Syntax Tree.
///
/// # Fields
/// * `top_level_block` - The real programm
/// * `source_index` - The source index of the file
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, PartialEq)]
pub struct DesugaredAST
{
	pub top_level_block: TopLevelBlock,
	pub source_index: SourceIndex,
}

// impl TryFrom<AST> for DesugaredAST
// {
// 	type Error = DesugarError;
// 	fn try_from(value: AST) -> Result<Self, Self::Error>
// 	{
// 		return desugar_program(value);
// 	}
// }

impl std::fmt::Display for DesugaredAST
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return write!(f, "{}", self.top_level_block);
	}
}

impl Spanned for DesugaredAST
{
	fn span(&self) -> Span
	{
		return self.top_level_block.span();
	}
}

#[derive(Debug)]
struct Desugarer
{
	tmp_counter: usize,
	loop_stack: Vec<String>,
	diagnostics: Vec<DiagnosticBuilder>,
}

/// Types of errors that can occur during desugaring.
///
/// Categorizes different semantic errors that prevent desugaring from
/// completing successfully. Unlike parse errors, these occur when the
/// AST is syntactically valid but semantically problematic.
#[derive(Debug, Clone)]
pub enum DesugarErrorKind
{
	/// A constructor call was attempted on an invalid type.
	///
	/// Constructor calls (using `()` or `!()`) are not valid on references and pointers.
	InvalidConstructorType
	{
		reason: String
	},

	/// A pattern is malformed or used in an invalid context.
	///
	/// This can occur when patterns are too complex for their context,
	/// or when they contain constructs that aren't supported.
	#[allow(unused)]
	InvalidPattern
	{
		reason: String
	},

	/// A generic error with a custom message.
	///
	/// Used for errors that don't fit other categories.
	Generic
	{
		message: String
	},
}

#[derive(Debug, Clone, Spanned)]
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

	#[allow(unused)]
	pub fn with_context(mut self, ctx: impl Into<String>) -> Self
	{
		self.context.push(ctx.into());
		return self;
	}

	pub fn invalid_constructor_type(span: Span, reason: impl Into<String>) -> Self
	{
		return Self::new(span, DesugarErrorKind::InvalidConstructorType { reason: reason.into() });
	}

	#[allow(unused)]
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
				write!(f, "invalid constructor type: {reason}")?;
			}
			DesugarErrorKind::InvalidPattern { reason } => {
				write!(f, "invalid pattern: {reason}")?;
			}
			DesugarErrorKind::Generic { message } => {
				write!(f, "{message}")?;
			}
		}

		if !self.context.is_empty() {
			write!(f, "\n  while desugaring: {}", self.context.join(" → "))?;
		}

		return Ok(());
	}
}

impl std::error::Error for DesugarError {}

impl CompileDiagnostic for DesugarError
{
	fn build(&self) -> DiagnosticBuilder
	{
		let mut diag = match &self.kind {
			DesugarErrorKind::InvalidConstructorType { reason } => {
				DiagnosticBuilder::error(format!("invalid constructor type: {reason}"))
					.code(ErrorCode::DesugarInvalidConstructorType)
			}

			DesugarErrorKind::InvalidPattern { reason } => {
				DiagnosticBuilder::error(format!("invalid pattern: {reason}")).code(ErrorCode::DesugarInvalidPattern)
			}

			DesugarErrorKind::Generic { message } => {
				DiagnosticBuilder::error(message.clone()).code(ErrorCode::DesugarGeneric)
			}
		};

		// Primary label
		diag = diag.primary(self.span(), None);

		// Context stack
		for ctx in &self.context {
			diag = diag.note(format!("while desugaring: {ctx}"));
		}

		return diag;
	}
}

impl From<DesugarError> for crate::CompileError
{
	fn from(value: DesugarError) -> Self
	{
		return crate::CompileError::Desugar(value);
	}
}

impl Desugarer
{
	#[allow(unused)]
	const fn new() -> Self
	{
		return Desugarer {
			tmp_counter: 0,
			loop_stack: Vec::new(),
			diagnostics: Vec::new(),
		};
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
		let actual_label: String = label.unwrap_or_else(|| return self.gen_loop_label());
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

	fn desugar_program(&mut self, program: AST) -> DesugaredAST
	{
		let top_lvl: TopLevelBlock = self.desugar_top_level_block(program.top_level_block);

		#[allow(clippy::debug_assert_with_mut_call)]
		{
			debug_assert_eq!(top_lvl, self.desugar_top_level_block(top_lvl.clone()));
		}

		return DesugaredAST {
			top_level_block: top_lvl,
			source_index: program.source_index,
		};
	}

	fn desugar_top_level_block(&mut self, top_level_block: TopLevelBlock) -> TopLevelBlock
	{
		let items: Vec<TopLevelDecl> = top_level_block
			.items
			.into_iter()
			.map(|item| return self.desugar_top_level_decl(item))
			.collect::<Vec<_>>();

		return TopLevelBlock {
			items,
			span: top_level_block.span,
		};
	}

	fn desugar_top_level_decl(&mut self, decl: TopLevelDecl) -> TopLevelDecl
	{
		return match decl {
			TopLevelDecl::Function(func) => TopLevelDecl::Function(self.desugar_function(func)),
			TopLevelDecl::Module(ns) => TopLevelDecl::Module(self.desugar_module(ns)),
			TopLevelDecl::Impl(impl_decl) => TopLevelDecl::Impl(self.desugar_impl(impl_decl)),
			TopLevelDecl::Trait(trait_decl) => TopLevelDecl::Trait(self.desugar_trait(trait_decl)),
			TopLevelDecl::Directive(d) => TopLevelDecl::Directive(self.desugar_directive_node(d)),
			TopLevelDecl::VariableDecl(var) => TopLevelDecl::VariableDecl(self.desugar_variable_decl(var)),
			TopLevelDecl::Struct(s) => TopLevelDecl::Struct(s),
			TopLevelDecl::Union(u) => TopLevelDecl::Union(u),
			TopLevelDecl::Enum(e) => TopLevelDecl::Enum(e),
			TopLevelDecl::Variant(v) => TopLevelDecl::Variant(v),
			TopLevelDecl::TypeAlias(t) => TopLevelDecl::TypeAlias(t),
		};
	}

	fn desugar_function(&mut self, mut func: FunctionDecl) -> FunctionDecl
	{
		debug_assert!(self.loop_stack.is_empty(), "loop_stack should be empty");

		let mut new_params: Vec<Param> = Vec::new();
		let mut body_stmts: Vec<Stmt> = Vec::new();

		for param in func.signature.params {
			let param_span: Span = param.span();

			if param.variadic {
				new_params.push(param);
				continue;
			}

			match param.pattern {
				Pattern::TypedIdentifier { .. } => {
					new_params.push(param);
				}

				other_pattern => {
					let temp: Ident = self.gen_temp("param");

					new_params.push(Param {
						pattern: Pattern::TypedIdentifier {
							path: Path::simple(vec![temp.clone()], param_span),
							modifiers: Vec::new(),
							ty: param.ty.clone(),
							call_constructor: None,
							span: param_span,
							mutable: false,
						},
						variadic: param.variadic,
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
						docs: None,
						span: param_span,
					}));
				}
			}
		}

		func.signature.params = new_params;
		func.signature = self.desugar_function_signature(func.signature);

		if let Some(body) = func.body {
			let mut new_body: Block = body;
			body_stmts.extend(new_body.stmts);
			new_body.stmts = body_stmts;
			func.body = Some(self.desugar_block(new_body));
		}

		return func;
	}

	fn desugar_function_signature(&mut self, mut func_sig: FunctionSignature) -> FunctionSignature
	{
		let mut impl_trait_counter: usize = 0;
		let mut new_generics: Vec<GenericParam> = Vec::new();

		for param in &mut func_sig.params {
			if let Some(new_generic) = Self::desugar_impl_trait_in_param(param, &mut impl_trait_counter) {
				new_generics.push(new_generic);
			}
		}

		new_generics.extend(func_sig.generics);
		func_sig.generics = new_generics;

		if let Some(new_generic) = Self::desugar_impl_trait_in_type(&mut func_sig.return_type, &mut impl_trait_counter)
		{
			func_sig.generics.push(new_generic);
		}

		let generics_with_bounds: Vec<(&String, Span)> = func_sig
			.generics
			.iter()
			.filter(|g| return g.has_bounds())
			.map(|g| return (&g.name, g.span()))
			.collect();

		for where_constraint in &func_sig.where_clause {
			let mentioned_types: Vec<Ident> = get_mentioned_type_params(&where_constraint.ty);

			let mentioned_in_args: Vec<Ident> = where_constraint
				.type_args
				.iter()
				.flat_map(get_mentioned_type_params_in_type)
				.collect();

			let all_mentioned: Vec<Ident> = mentioned_types.into_iter().chain(mentioned_in_args).collect();

			for type_param in all_mentioned {
				if let Some((_, generic_span)) = generics_with_bounds
					.iter()
					.find(|(name, _)| return *name == &type_param)
				{
					self.diagnostics.push(
						DesugarError::generic(
							*generic_span,
							format!(
								"type parameter `{}` has bounds in generic parameter list but is also used in where clause. \
									Move all bounds for `{}` to the where clause instead.",
								type_param, type_param
							),
						)
						.build()
						.secondary(
							where_constraint.span(),
							"second constraint here in the where clause".to_string(),
						)
						.note("during desugaring")
						.help("move all the constraints to the where clause"),
					);
				}
			}
		}

		for generic in &func_sig.generics {
			if generic.bounds.is_empty() {
				continue;
			}

			func_sig.where_clause.push(WhereConstraint {
				ty: Path {
					segments: vec![PathSegment {
						name: generic.name.clone(),
						generics: Vec::new(),
						span: generic.span,
					}],
					glob: false,
					global: false,
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

		return func_sig;
	}

	fn desugar_impl_trait_in_param(param: &mut Param, counter: &mut usize) -> Option<GenericParam>
	{
		let generic_param: Option<GenericParam> = Self::desugar_impl_trait_in_type(&mut param.ty, counter);

		if let Pattern::TypedIdentifier { ty, .. } = &mut param.pattern {
			*ty = param.ty.clone();
		}

		return generic_param;
	}

	fn desugar_impl_trait_in_type(ty: &mut Type, counter: &mut usize) -> Option<GenericParam>
	{
		match ty.core.as_mut() {
			TypeCore::ImplTrait { bounds } => {
				let generic_name: String = format!("#__ImplTrait{}", counter);
				*counter += 1;

				let generic_param = GenericParam {
					name: generic_name.clone(),
					bounds: bounds.clone(),
					span: ty.span,
				};

				*ty.core = TypeCore::Base {
					path: Path {
						segments: vec![PathSegment {
							name: generic_name,
							generics: Vec::new(),
							span: ty.span,
						}],
						glob: false,
						global: false,
						span: ty.span,
					},
					generics: Vec::new(),
				};

				return Some(generic_param);
			}
			TypeCore::Reference { inner, .. } | TypeCore::Mutable { inner } | TypeCore::Pointer { inner, .. } => {
				let mut inner_ty: Type = Type {
					core: inner.clone(),
					span: ty.span,
				};
				let result: Option<GenericParam> = Self::desugar_impl_trait_in_type(&mut inner_ty, counter);
				*inner = inner_ty.core;
				return result;
			}
			TypeCore::Array { inner, .. } => {
				let mut inner_ty = Type {
					core: inner.clone(),
					span: ty.span,
				};
				let result: Option<GenericParam> = Self::desugar_impl_trait_in_type(&mut inner_ty, counter);
				*inner = inner_ty.core;
				return result;
			}
			TypeCore::Tuple(types) => {
				for tuple_ty in types.iter_mut() {
					Self::desugar_impl_trait_in_type(tuple_ty, counter)?;
				}
				return None;
			}
			TypeCore::Base { generics, .. } => {
				for gen_ty in generics.iter_mut() {
					Self::desugar_impl_trait_in_type(gen_ty, counter)?;
				}
				return None;
			}
		}
	}

	fn desugar_module(&mut self, mut ns: ModuleDecl) -> ModuleDecl
	{
		match &mut ns.kind {
			ModuleKind::Inline(body) => {
				*body = self.desugar_top_level_block(std::mem::take(body));
			}
			ModuleKind::External => {}
		}
		return ns;
	}

	fn desugar_impl(&mut self, mut impl_decl: ImplDecl) -> ImplDecl
	{
		impl_decl = self.desugar_impl_generics(impl_decl);

		impl_decl.body = impl_decl
			.body
			.into_iter()
			.map(|item| {
				return match item {
					ImplItem::Function(func) => ImplItem::Function(self.desugar_function(func)),
					ImplItem::TypeAlias(t) => ImplItem::TypeAlias(t),
					ImplItem::AssocType(t) => ImplItem::AssocType(t),
					ImplItem::Const(c) => ImplItem::Const(self.desugar_variable_decl(c)),
				};
			})
			.collect::<Vec<_>>();
		return impl_decl;
	}

	fn desugar_impl_generics(&mut self, mut impl_decl: ImplDecl) -> ImplDecl
	{
		let generics_with_bounds: Vec<(&String, Span)> = impl_decl
			.generics
			.iter()
			.filter(|g| return g.has_bounds())
			.map(|g| return (&g.name, g.span()))
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
				if let Some((_, generic_span)) = generics_with_bounds
					.iter()
					.find(|(name, _)| return *name == &type_param)
				{
					self.diagnostics.push(
						DesugarError::generic(
							*generic_span,
							format!(
								"type parameter `{}` has bounds in generic parameter list but is also used in where clause. \
									Move all bounds for `{}` to the where clause instead.",
								type_param, type_param
							),
						)
						.build()
						.secondary(
							where_constraint.span(),
							"second constraint here in the where clause".to_string(),
						)
						.note("during desugaring")
						.help("move the all the constraints to the where clause"),
					);
				}
			}
		}

		for generic in &impl_decl.generics {
			if generic.bounds.is_empty() {
				continue;
			}

			impl_decl.where_clause.push(WhereConstraint {
				ty: Path {
					segments: vec![PathSegment {
						name: generic.name.clone(),
						generics: Vec::new(),
						span: generic.span,
					}],
					glob: false,
					global: false,
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

		return impl_decl;
	}

	fn desugar_trait(&mut self, mut trait_decl: TraitDecl) -> TraitDecl
	{
		trait_decl.items = trait_decl
			.items
			.into_iter()
			.map(|item| -> TraitItem {
				return match item {
					TraitItem::Function(func) => {
						debug_assert!(self.loop_stack.is_empty());
						TraitItem::Function(self.desugar_function(func))
					}
					TraitItem::TypeAlias(t) => TraitItem::TypeAlias(t),
					TraitItem::AssocType(t) => TraitItem::AssocType(t),
					TraitItem::Const(c) => TraitItem::Const(self.desugar_variable_decl(c)),
				};
			})
			.collect::<Vec<_>>();
		return trait_decl;
	}

	fn desugar_directive_node(&mut self, mut directive: DirectiveNode) -> DirectiveNode
	{
		directive.body = directive.body.map(|body| return self.desugar_block_content(body));
		return directive;
	}

	fn desugar_block(&mut self, block: Block) -> Block
	{
		let mut stmts: Vec<Stmt> = Vec::new();

		for stmt in block.stmts {
			match stmt {
				Stmt::VariableDecl(var) => {
					let needs_complex_desugar: bool = match &var.pattern {
						Pattern::Struct { .. } | Pattern::Variant { .. } => true,
						Pattern::Tuple { patterns, .. } => patterns.len() > 1 || Self::has_nested_patterns(patterns),
						_ => false,
					};

					if needs_complex_desugar {
						let span: Span = var.span;
						let comp_const: bool = var.comp_const;
						let init: Expr = if let Some(i) = var.init {
							i
						} else {
							self.diagnostics
								.push(DesugarError::generic(span, "complex pattern requires initializer").build());
							Expr::Identifier {
								path: Path::simple(Vec::new(), span),
								span,
							}
						};

						let var_decls = self.desugar_pattern_to_statements(var.pattern, init, span, comp_const);

						for var_decl in var_decls {
							stmts.push(var_decl);
						}
					} else {
						stmts.push(Stmt::VariableDecl(self.desugar_variable_decl(var)));
					}
				}

				other_stmt => {
					stmts.push(self.desugar_stmt(other_stmt));
				}
			}
		}

		let tail_expr = block.tail_expr.map(|expr| return Box::new(self.desugar_expr(*expr)));

		return Block {
			stmts,
			tail_expr,
			span: block.span,
		};
	}

	fn desugar_block_content(&mut self, content: BlockContent) -> BlockContent
	{
		return match content {
			BlockContent::Block(block) => BlockContent::Block(self.desugar_block(block)),
			BlockContent::TopLevelBlock(block) => BlockContent::TopLevelBlock(self.desugar_top_level_block(block)),
		};
	}

	fn desugar_stmt(&mut self, stmt: Stmt) -> Stmt
	{
		return match stmt {
			Stmt::For {
				label,
				pattern,
				iter,
				body,
				span,
			} => self.desugar_for_loop(label, pattern, iter, body, span),

			Stmt::If {
				cond,
				then_block,
				else_branch,
				span,
			} => Stmt::If {
				cond: self.desugar_expr(cond),
				then_block: self.desugar_block(then_block),
				else_branch: else_branch.map(|stmt| -> Box<Stmt> { return Box::new(self.desugar_stmt(*stmt)) }),
				span,
			},

			Stmt::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
				span,
			} => self.desugar_if_var(pattern, expr, then_block, else_branch, span),

			Stmt::While {
				label,
				cond,
				body,
				span,
			} => self.desugar_while_loop(label, cond, body, span),

			Stmt::Loop { label, body, span } => {
				let actual_label = self.push_loop(label);
				let desugared = Stmt::Loop {
					label: Some(actual_label),
					body: self.desugar_block(body),
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
			} => self.desugar_while_var_loop(label, pattern, expr, body, span),

			Stmt::VariableDecl(var) => {
				let needs_complex_desugar = match &var.pattern {
					Pattern::Struct { .. } | Pattern::Variant { .. } => true,
					Pattern::Tuple { patterns, .. } => patterns.len() > 1 && Self::has_nested_patterns(patterns),
					_ => false,
				};

				if needs_complex_desugar {
					let span: Span = var.span;
					let comp_const: bool = var.comp_const;
					let init: Expr = if let Some(i) = var.init {
						i
					} else {
						self.diagnostics
							.push(DesugarError::generic(span, "complex pattern requires initializer").build());
						Expr::Identifier {
							path: Path::simple(Vec::new(), span),
							span,
						}
					};

					let stmts: Vec<Stmt> = self.desugar_pattern_to_statements(var.pattern, init, span, comp_const);

					Stmt::Block(Block {
						stmts,
						tail_expr: None,
						span,
					})
				} else {
					Stmt::VariableDecl(self.desugar_variable_decl(var))
				}
			}

			Stmt::Assignment {
				target,
				op,
				value,
				span,
			} => {
				let needs_complex_desugar: bool = match &target {
					Expr::Tuple { elements, .. } => {
						elements.len() > 1 || !elements.iter().all(|e| matches!(e, Expr::Identifier { .. }))
					}
					Expr::StructInit { .. } => true,
					_ => false,
				};

				if needs_complex_desugar && matches!(op, AssignOp::Assign) {
					let desugared_value: Expr = self.desugar_expr(value);

					let temp: Ident = self.gen_temp("assign");
					let value_span: Span = desugared_value.span();

					let temp_decl: Stmt = Stmt::VariableDecl(VariableDecl {
						pattern: Pattern::TypedIdentifier {
							path: Path::simple(vec![temp.clone()], value_span),
							modifiers: Vec::new(),
							ty: Type {
								core: Box::new(TypeCore::Base {
									path: Path::simple(vec!["_".to_string()], value_span),
									generics: vec![],
								}),
								span: value_span,
							},
							call_constructor: None,
							span: value_span,
							mutable: false,
						},
						docs: None,
						init: Some(desugared_value),
						comp_const: false,
						span: value_span,
					});

					let assignments: Vec<Stmt> = self.desugar_assignment_target(
						target,
						Expr::Identifier {
							path: Path::simple(vec![temp], value_span),
							span: value_span,
						},
						span,
					);

					let mut stmts: Vec<Stmt> = vec![temp_decl];
					stmts.extend(assignments);

					Stmt::Block(Block {
						stmts,
						tail_expr: None,
						span,
					})
				} else {
					Stmt::Assignment {
						target: self.desugar_expr(target),
						op,
						value: self.desugar_expr(value),
						span,
					}
				}
			}

			Stmt::Return { value, span } => Stmt::Return {
				value: value.map(|e| return self.desugar_expr(e)),
				span,
			},

			Stmt::Expr(expr) => Stmt::Expr(self.desugar_expr(expr)),

			Stmt::Break { label, value, span } => {
				let actual_label = label.or_else(|| return self.current_loop().cloned());
				Stmt::Break {
					label: actual_label,
					value: value.map(|v| return self.desugar_expr(v)),
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

			Stmt::Unsafe(block) => Stmt::Unsafe(self.desugar_block(block)),
			Stmt::Block(block) => Stmt::Block(self.desugar_block(block)),

			Stmt::Directive(directive) => Stmt::Directive(self.desugar_directive_node(directive)),

			Stmt::Delete { expr, span } => Stmt::Delete {
				expr: self.desugar_expr(expr),
				span,
			},
		};
	}

	fn desugar_for_loop(&mut self, label: Option<String>, pattern: Pattern, iter: Expr, body: Block, span: Span)
	-> Stmt
	{
		let iter_temp: Ident = self.gen_temp("loop");
		let iter_span: Span = iter.span();
		let pattern_span: Span = pattern.span();

		let desugared_iter: Expr = self.desugar_expr(iter);

		let actual_label: Ident = self.push_loop(label);
		let desugared_body: Block = self.desugar_block(body);

		let desugared_pattern: Pattern = Self::desugar_pattern(pattern);

		let item_type: Type = extract_type_from_pattern(&desugared_pattern).unwrap_or_else(|| {
			return Type {
				core: Box::new(TypeCore::Base {
					path: Path::simple(vec!["_".to_string()], pattern_span),
					generics: vec![],
				}),
				span: pattern_span,
			};
		});

		let iterator_type: Type = Type {
			core: Box::new(TypeCore::ImplTrait {
				bounds: vec![WhereBound::Path {
					path: Path {
						segments: vec![
							PathSegment {
								name: "core".to_string(),
								generics: Vec::new(),
								span: iter_span,
							},
							PathSegment {
								name: "iterator".to_string(),
								generics: Vec::new(),
								span: iter_span,
							},
							PathSegment {
								name: "Iterator".to_string(),
								generics: Vec::new(),
								span: iter_span,
							},
						],
						span: iter_span,
						global: true,
						glob: false,
					},
					args: vec![GenericArg::Binding {
						name: "Item".to_string(),
						ty: item_type,
						span: pattern_span,
					}],
				}],
			}),
			span: iter_span,
		};

		let iter_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![iter_temp.clone()], iter_span),
				modifiers: vec![],
				ty: iterator_type,
				call_constructor: None,
				span: iter_span,
				mutable: false,
			},
			docs: None,
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
				name: Path::simple(vec!["next".to_string()], Span::default()),
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
				args: vec![Pattern::Wildcard {
					span: pattern_span,
					ty: None,
				}],
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

		return Stmt::Block(Block {
			stmts: vec![iter_decl, loop_stmt],
			tail_expr: None,
			span,
		});
	}

	fn desugar_if_var(
		&mut self,
		pattern: Pattern,
		expr: Expr,
		then_block: Block,
		else_branch: Option<Box<Stmt>>,
		span: Span,
	) -> Stmt
	{
		let temp_var: Ident = self.gen_temp("ifvar");
		let expr_span: Span = expr.span();
		let pattern_span: Span = pattern.span();

		let desugared_expr: Expr = self.desugar_expr(expr);
		let desugared_then: Block = self.desugar_block(then_block);

		let temp_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![temp_var.clone()], expr_span),
				modifiers: Vec::new(),
				ty: Type {
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], expr_span),
						generics: vec![],
					}),
					span: expr_span,
				},
				call_constructor: None,
				span: expr_span,
				mutable: false,
			},
			docs: None,
			init: Some(desugared_expr),
			comp_const: false,
			span: expr_span,
		});

		let match_arm: SwitchArm = SwitchArm {
			pattern: Self::desugar_pattern(pattern),
			body: SwitchBody::Block(desugared_then),
			span: pattern_span,
		};

		let else_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard {
				span: pattern_span,
				ty: None,
			},
			body: else_branch.map_or_else(
				|| {
					return SwitchBody::Block(Block {
						stmts: vec![],
						tail_expr: None,
						span: pattern_span,
					});
				},
				|else_stmt| {
					let stmt_span = else_stmt.span();
					return SwitchBody::Block(Block {
						stmts: vec![self.desugar_stmt(*else_stmt)],
						tail_expr: None,
						span: stmt_span,
					});
				},
			),
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

		return Stmt::Block(Block {
			stmts: vec![temp_decl, Stmt::Expr(switch_expr)],
			tail_expr: None,
			span,
		});
	}

	fn desugar_while_var_loop(
		&mut self,
		label: Option<String>,
		pattern: Pattern,
		expr: Expr,
		body: Block,
		span: Span,
	) -> Stmt
	{
		let temp_var: Ident = self.gen_temp("whilevar");
		let expr_span: Span = expr.span();
		let pattern_span: Span = pattern.span();

		let desugared_expr: Expr = self.desugar_expr(expr);

		let actual_label: Ident = self.push_loop(label);
		let desugared_body: Block = self.desugar_block(body);

		let temp_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![temp_var.clone()], expr_span),
				modifiers: Vec::new(),
				ty: Type {
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], expr_span),
						generics: vec![],
					}),
					span: expr_span,
				},
				call_constructor: None,
				span: expr_span,
				mutable: false,
			},
			docs: None,
			init: Some(desugared_expr),
			comp_const: false,
			span: expr_span,
		});

		let match_arm: SwitchArm = SwitchArm {
			pattern: Self::desugar_pattern(pattern),
			body: SwitchBody::Block(desugared_body),
			span: pattern_span,
		};

		let break_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard {
				span: pattern_span,
				ty: None,
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

		return result;
	}

	fn desugar_while_loop(&mut self, label: Option<String>, cond: Expr, body: Block, span: Span) -> Stmt
	{
		let cond_span: Span = cond.span();

		let desugared_cond: Expr = self.desugar_expr(cond);

		let actual_label: Ident = self.push_loop(label);
		let desugared_body: Block = self.desugar_block(body);

		let negated_cond: Expr = Expr::Unary {
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

		let result: Stmt = Stmt::Loop {
			label: Some(actual_label),
			body: Block {
				stmts: loop_body_stmts,
				tail_expr: desugared_body.tail_expr,
				span,
			},
			span,
		};

		self.pop_loop();

		return result;
	}

	fn desugar_variable_decl(&mut self, mut var: VariableDecl) -> VariableDecl
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
			));

			if let Pattern::TypedIdentifier {
				path,
				ty,
				span,
				modifiers,
				call_constructor: _,
				mutable,
			} = var.pattern.clone()
			{
				var.pattern = Pattern::TypedIdentifier {
					path,
					modifiers,
					ty,
					call_constructor: None,
					mutable,
					span,
				};
			}
		}

		let needs_complex_desugar: bool = match &var.pattern {
			Pattern::Struct { .. } | Pattern::Variant { .. } => true,
			Pattern::Tuple { patterns, .. } => patterns.len() > 1 && Self::has_nested_patterns(patterns),
			_ => false,
		};

		if needs_complex_desugar {
			return self.desugar_complex_pattern_binding(var);
		}

		var.init = var.init.map(|init| return self.desugar_expr(init));
		var.pattern = Self::desugar_pattern(var.pattern);
		return var;
	}

	fn desugar_expr(&mut self, expr: Expr) -> Expr
	{
		return match expr {
			Expr::Unary { op, expr, span } => Expr::Unary {
				op,
				expr: Box::new(self.desugar_expr(*expr)),
				span,
			},

			Expr::Binary { op, lhs, rhs, span } => Expr::Binary {
				op,
				lhs: Box::new(self.desugar_expr(*lhs)),
				rhs: Box::new(self.desugar_expr(*rhs)),
				span,
			},

			Expr::Cast { ty, expr, span } => Expr::Cast {
				ty,
				expr: Box::new(self.desugar_expr(*expr)),
				span,
			},

			Expr::Call {
				callee,
				call_type,
				named_generics,
				args,
				span,
			} => Expr::Call {
				callee: Box::new(self.desugar_expr(*callee)),
				call_type,
				named_generics: {
					named_generics
						.into_iter()
						.map(|(n, e)| return (n, self.desugar_expr(e)))
						.collect()
				},
				args: args
					.into_iter()
					.map(|arg| return self.desugar_expr(arg))
					.collect::<Vec<_>>(),
				span,
			},

			Expr::Field { base, name, span } => Expr::Field {
				base: Box::new(self.desugar_expr(*base)),
				name,
				span,
			},

			Expr::Index { base, index, span } => Expr::Index {
				base: Box::new(self.desugar_expr(*base)),
				index: Box::new(self.desugar_expr(*index)),
				span,
			},

			Expr::Tuple { elements, span } => Expr::Tuple {
				elements: elements
					.into_iter()
					.map(|e| return self.desugar_expr(e))
					.collect::<Vec<_>>(),
				span,
			},

			Expr::Array(array_lit) => Expr::Array(self.desugar_array_literal(array_lit)),

			Expr::StructInit {
				path,
				fields,
				span,
				base,
				has_rest,
			} => {
				let desugared_fields: Vec<(Ident, Expr)> = fields
					.into_iter()
					.map(|(name, expr)| {
						return (name, self.desugar_expr(expr));
					})
					.collect::<Vec<_>>();

				let desugared_base = base.map(|expr| return self.desugar_expr(*expr)).map(Box::new);

				Expr::StructInit {
					path,
					fields: desugared_fields,
					base: desugared_base,
					span,
					has_rest,
				}
			}
			Expr::Block(block) => Expr::Block(Box::new(self.desugar_block(*block))),

			Expr::UnsafeBlock(block) => Expr::UnsafeBlock(Box::new(self.desugar_block(*block))),

			Expr::Switch { expr, arms, span } => Expr::Switch {
				expr: Box::new(self.desugar_expr(*expr)),
				arms: arms
					.into_iter()
					.map(|arm| return self.desugar_switch_arm(arm))
					.collect::<Vec<_>>(),
				span,
			},

			Expr::If {
				cond,
				then_block,
				else_branch,
				span,
			} => self.desugar_if_expr(*cond, then_block, else_branch, span),

			Expr::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
				span,
			} => self.desugar_if_var_expr(pattern, *expr, then_block, else_branch, span),

			Expr::Loop { label, body, span } => {
				let actual_label: String = self.push_loop(label);
				let desugared: Expr = Expr::Loop {
					label: Some(actual_label),
					body: Box::new(self.desugar_block(*body)),
					span,
				};
				self.pop_loop();
				desugared
			}

			Expr::Range(range_expr) => self.desugar_range(range_expr),

			Expr::Identifier { .. } | Expr::Literal { .. } | Expr::Default { .. } => expr,
		};
	}

	fn desugar_if_expr(&mut self, cond: Expr, then_block: Block, else_branch: Option<Box<Expr>>, span: Span) -> Expr
	{
		return Expr::If {
			cond: Box::new(self.desugar_expr(cond)),
			then_block: self.desugar_block(then_block),
			else_branch: else_branch.map(|e| return Box::new(self.desugar_expr(*e))),
			span,
		};
	}

	fn desugar_if_var_expr(
		&mut self,
		pattern: Pattern,
		expr: Expr,
		then_block: Block,
		else_branch: Option<Box<Expr>>,
		span: Span,
	) -> Expr
	{
		let temp_var: Ident = self.gen_temp("ifvar_expr");
		let expr_span: Span = expr.span();
		let pattern_span: Span = pattern.span();

		let desugared_expr: Expr = self.desugar_expr(expr);
		let desugared_then: Block = self.desugar_block(then_block);

		let temp_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![temp_var.clone()], expr_span),
				modifiers: Vec::new(),
				ty: Type {
					core: Box::new(TypeCore::Base {
						path: Path::simple(vec!["_".to_string()], expr_span),
						generics: vec![],
					}),
					span: expr_span,
				},
				call_constructor: None,
				span: expr_span,
				mutable: false,
			},
			docs: None,
			init: Some(desugared_expr),
			comp_const: false,
			span: expr_span,
		});

		let match_arm: SwitchArm = SwitchArm {
			pattern: Self::desugar_pattern(pattern),
			body: SwitchBody::Block(desugared_then),
			span: pattern_span,
		};

		let else_arm: SwitchArm = SwitchArm {
			pattern: Pattern::Wildcard {
				span: pattern_span,
				ty: None,
			},
			body: else_branch.map_or_else(
				|| {
					return SwitchBody::Block(Block {
						stmts: vec![],
						tail_expr: None,
						span: pattern_span,
					});
				},
				|else_expr| {
					let desugared_else = self.desugar_expr(*else_expr);

					return match desugared_else {
						Expr::Block(block) => SwitchBody::Block(*block),
						other_expr => {
							let other_span: Span = other_expr.span();
							SwitchBody::Block(Block {
								stmts: vec![],
								tail_expr: Some(Box::new(other_expr)),
								span: other_span,
							})
						}
					};
				},
			),
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

		return Expr::Block(Box::new(Block {
			stmts: vec![temp_decl],
			tail_expr: Some(Box::new(switch_expr)),
			span,
		}));
	}

	fn type_to_constructor_call(&mut self, ty: &Type, call_type: CallType) -> Expr
	{
		let span: Span = ty.span();

		match ty.core.as_ref() {
			TypeCore::Base { path, generics } => {
				let mut constructor_path = path.clone();

				if !generics.is_empty()
					&& let Some(last_segment) = constructor_path.segments.last_mut()
				{
					last_segment.generics.clone_from(generics);
				}

				constructor_path.segments.push(PathSegment {
					name: "create".to_string(),
					generics: Vec::new(),
					span,
				});

				return Expr::Call {
					callee: Box::new(Expr::Identifier {
						path: constructor_path,
						span,
					}),
					call_type,
					named_generics: Vec::new(),
					args: vec![],
					span,
				};
			}

			TypeCore::Reference { .. } => {
				self.diagnostics.push(
					DesugarError::invalid_constructor_type(
						span,
						"cannot call constructor on reference types - references must point to existing values",
					)
					.build(),
				);
				return Expr::Identifier {
					path: Path::simple(Vec::new(), span),
					span,
				};
			}

			TypeCore::Pointer { .. } => {
				self.diagnostics.push(
					DesugarError::invalid_constructor_type(
						span,
						"cannot call constructor on pointer types - pointers must point to existing values",
					)
					.build(),
				);
				return Expr::Identifier {
					path: Path::simple(Vec::new(), span),
					span,
				};
			}

			TypeCore::Mutable { inner } => {
				let inner_type: Type = Type {
					core: inner.clone(),
					span,
				};

				return self.type_to_constructor_call(&inner_type, call_type);
			}

			TypeCore::Array { inner, size } => {
				let inner_type: Type = Type {
					core: inner.clone(),
					span,
				};

				let element_constructor: Expr = self.type_to_constructor_call(&inner_type, call_type);

				if let Some(size_expr) = size {
					return Expr::Array(ArrayLiteral::Repeat {
						value: Box::new(element_constructor),
						count: size_expr.clone(),
						span,
					});
				}
				return Expr::Array(ArrayLiteral::List {
					elements: Vec::new(),
					span,
				});
			}

			TypeCore::Tuple(types) => {
				let mut element_constructors: Vec<Expr> = Vec::new();

				for tuple_ty in types {
					let element_constructor: Expr = self.type_to_constructor_call(tuple_ty, call_type);
					element_constructors.push(element_constructor);
				}

				return Expr::Tuple {
					elements: element_constructors,
					span,
				};
			}

			TypeCore::ImplTrait { .. } => {
				self.diagnostics.push(
					DesugarError::invalid_constructor_type(
						span,
						"cannot call constructor on 'impl Trait' types - they must be concrete types",
					)
					.build(),
				);
				return Expr::Identifier {
					path: Path::simple(Vec::new(), span),
					span,
				};
			}
		}
	}

	fn desugar_array_literal(&mut self, array_lit: ArrayLiteral) -> ArrayLiteral
	{
		return match array_lit {
			ArrayLiteral::List { elements, span } => ArrayLiteral::List {
				elements: elements
					.into_iter()
					.map(|e| return self.desugar_expr(e))
					.collect::<Vec<_>>(),
				span,
			},
			ArrayLiteral::Repeat { value, count, span } => ArrayLiteral::Repeat {
				value: Box::new(self.desugar_expr(*value)),
				count: Box::new(self.desugar_expr(*count)),
				span,
			},
		};
	}

	fn desugar_switch_arm(&mut self, arm: SwitchArm) -> SwitchArm
	{
		return SwitchArm {
			pattern: Self::desugar_pattern(arm.pattern),
			body: match arm.body {
				SwitchBody::Expr(expr) => SwitchBody::Expr(self.desugar_expr(expr)),
				SwitchBody::Block(block) => SwitchBody::Block(self.desugar_block(block)),
			},
			span: arm.span,
		};
	}

	fn desugar_pattern(pattern: Pattern) -> Pattern
	{
		let expanded: Vec<Pattern> = Self::expand_or_patterns(pattern);

		let pattern: Pattern = if expanded.len() > 1 {
			let span: Span = expanded[0].span();
			Pattern::Or {
				patterns: expanded,
				span,
			}
		} else {
			expanded
				.into_iter()
				.next()
				.expect("expand_or_patterns always returns at least one pattern")
		};

		return match pattern {
			Pattern::Wildcard { span, ty } => Pattern::Wildcard { span, ty },
			Pattern::Literal { value, span } => Pattern::Literal { value, span },
			Pattern::TypedIdentifier {
				path,
				modifiers,
				ty,
				call_constructor,
				span,
				mutable,
			} => Pattern::TypedIdentifier {
				path,
				modifiers,
				ty,
				call_constructor,
				span,
				mutable,
			},

			Pattern::Variant { path, args, span } => Pattern::Variant {
				path,
				args: args
					.into_iter()
					.map(|p| return Self::desugar_pattern(p))
					.collect::<Vec<_>>(),
				span,
			},

			Pattern::Tuple { patterns, span } => {
				let desugared: Vec<Pattern> = patterns
					.into_iter()
					.map(|p| return Self::desugar_pattern(p))
					.collect::<Vec<_>>();

				if desugared.len() == 1 {
					desugared.into_iter().next().expect("len == 1, so should not error")
				} else {
					Pattern::Tuple {
						patterns: desugared,
						span,
					}
				}
			}

			Pattern::Struct {
				path,
				fields,
				span,
				has_rest,
			} => Pattern::Struct {
				path,
				fields: fields
					.into_iter()
					.map(|(name, pat)| {
						return (name, Self::desugar_pattern(pat));
					})
					.collect::<Vec<_>>(),
				span,
				has_rest,
			},

			Pattern::Range(range) => Pattern::Range(range),

			Pattern::Or { patterns, span } => {
				let flattened: Vec<Pattern> = patterns
					.into_iter()
					.map(|p| return Self::desugar_pattern(p))
					.collect::<Vec<_>>();

				if flattened.len() == 1 {
					flattened.into_iter().next().expect("len == 1, so should not error")
				} else {
					Pattern::Or {
						patterns: flattened,
						span,
					}
				}
			}
		};
	}

	fn desugar_range(&mut self, expr: RangeExpr) -> Expr
	{
		fn call(path: &[&str], args: Vec<Expr>, span: Span) -> Expr
		{
			return Expr::Call {
				callee: Box::new(Expr::Identifier {
					path: Path {
						segments: path
							.iter()
							.map(|s| {
								return PathSegment {
									name: s.to_string(),
									generics: Vec::new(),
									span,
								};
							})
							.collect(),
						glob: false,
						global: true,
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

		let start: Option<Expr> = expr.start.map(|x| return self.desugar_expr(*x));
		let end: Option<Expr> = expr.end.map(|x| return self.desugar_expr(*x));

		return match (start, end, expr.inclusive) {
			// a..b
			(Some(a), Some(b), false) => call(&["core", "ranges", "Range", "new"], vec![a, b], span),

			// a..=b
			(Some(a), Some(b), true) => call(&["core", "ranges", "RangeInclusive", "new"], vec![a, b], span),

			// a..
			(Some(a), None, _) => call(&["core", "ranges", "RangeFrom", "new"], vec![a], span),

			// ..b
			(None, Some(b), false) => call(&["core", "ranges", "RangeTo", "new"], vec![b], span),

			// ..=b
			(None, Some(b), true) => call(&["core", "ranges", "RangeToInclusive", "new"], vec![b], span),

			// ..
			(None, None, _) => call(&["core", "ranges", "RangeFull", "new"], vec![], span),
		};
	}

	fn has_nested_patterns(patterns: &[Pattern]) -> bool
	{
		return patterns
			.iter()
			.any(|p| return !matches!(p, Pattern::TypedIdentifier { .. } | Pattern::Wildcard { .. }));
	}

	fn desugar_complex_pattern_binding(&mut self, var: VariableDecl) -> VariableDecl
	{
		let span: Span = var.span;
		let comp_const: bool = var.comp_const;
		let init: Expr = if let Some(i) = var.init {
			i
		} else {
			self.diagnostics
				.push(DesugarError::generic(span, "complex pattern requires initializer").build());
			Expr::Identifier {
				path: Path::simple(Vec::new(), span),
				span,
			}
		};

		let stmts: Vec<Stmt> = self.desugar_pattern_to_statements(var.pattern, init, span, comp_const);

		if let Some(Stmt::VariableDecl(var_decl)) = stmts.into_iter().next() {
			return var_decl;
		}

		self.diagnostics.push(compiler_bug!(
			Span::default(),
			"desugar_pattern_to_statements should always return at least one statement"
		));
		return VariableDecl {
			pattern: Pattern::Wildcard { span, ty: None },
			init: None,
			comp_const,
			docs: None,
			span,
		};
	}

	fn desugar_pattern_to_statements(&mut self, pattern: Pattern, init: Expr, span: Span, comp_const: bool)
	-> Vec<Stmt>
	{
		let mut statements: Vec<Stmt> = Vec::new();

		let is_simple_binding: bool = matches!(pattern, Pattern::TypedIdentifier { .. });

		if is_simple_binding {
			if let Pattern::TypedIdentifier {
				path,
				modifiers,
				ty,
				call_constructor,
				span: id_span,
				mutable,
			} = pattern
			{
				statements.push(Stmt::VariableDecl(VariableDecl {
					pattern: Pattern::TypedIdentifier {
						path,
						modifiers,
						ty,
						call_constructor,
						span: id_span,
						mutable,
					},
					init: Some(self.desugar_expr(init)),
					comp_const,
					docs: None,
					span: id_span,
				}));
			}
			return statements;
		}

		let temp_type: Type = if let Some(t) = extract_type_from_pattern(&pattern) {
			t
		} else {
			// todo!()
			self.diagnostics
				.push(DesugarError::generic(pattern.span(), "cannot extract type from pattern").build());
			Type {
				core: Box::new(TypeCore::Base {
					path: Path::simple(Vec::new(), span),
					generics: Vec::new(),
				}),
				span,
			}
		};

		let temp: String = self.gen_temp("pattern");
		let temp_span: Span = init.span();

		let temp_decl: Stmt = Stmt::VariableDecl(VariableDecl {
			pattern: Pattern::TypedIdentifier {
				path: Path::simple(vec![temp.clone()], temp_span),
				modifiers: Vec::new(),
				ty: temp_type,
				call_constructor: None,
				span: temp_span,
				mutable: false,
			},
			init: Some(self.desugar_expr(init)),
			comp_const,
			docs: None,
			span,
		});

		statements.push(temp_decl);

		match pattern {
			Pattern::Struct {
				path,
				fields,
				span: pattern_span,
				has_rest,
			} => {
				let field_names: Vec<String> = fields.iter().map(|(name, _)| return name.clone()).collect();

				let validation_directive = Stmt::Directive(DirectiveNode {
					directive: Directive::ValidateStructPattern {
						struct_path: path,
						pattern_fields: field_names,
						has_rest,
					},
					body: None,
					span: pattern_span,
				});

				statements.push(validation_directive);

				for (field_name, field_pattern) in fields {
					if let Pattern::Wildcard { ty, span } = field_pattern {
						if let Some(wildcard_ty) = ty {
							let validation_directive = Stmt::Directive(DirectiveNode {
								directive: Directive::ValidateType {
									ty: wildcard_ty,
									expr: Expr::Identifier {
										path: Path::simple(vec![temp.clone()], temp_span),
										span: temp_span,
									},
								},
								body: None,
								span,
							});

							statements.push(validation_directive);
						}
						continue;
					}

					let field_expr: Expr = Expr::Field {
						base: Box::new(Expr::Identifier {
							path: Path::simple(vec![temp.clone()], temp_span),
							span: temp_span,
						}),
						name: Path::simple(vec![field_name.clone()], Span::default()),
						span: pattern_span,
					};

					let nested_stmts: Vec<Stmt> =
						self.desugar_pattern_to_statements(field_pattern, field_expr, pattern_span, comp_const);

					statements.extend(nested_stmts);
				}
			}

			Pattern::Tuple {
				patterns,
				span: pattern_span,
			} => {
				for (i, elem_pattern) in patterns.into_iter().enumerate() {
					let index_expr = Expr::Field {
						base: Box::new(Expr::Identifier {
							path: Path::simple(vec![temp.clone()], temp_span),
							span: temp_span,
						}),
						name: Path::simple(vec![i.to_string()], Span::default()),
						span: pattern_span,
					};

					let nested_stmts =
						self.desugar_pattern_to_statements(elem_pattern, index_expr, pattern_span, comp_const);

					statements.extend(nested_stmts);
				}
			}

			Pattern::Wildcard { ty, span } => {
				if let Some(wildcard_ty) = ty {
					let validation_directive = Stmt::Directive(DirectiveNode {
						directive: Directive::ValidateType {
							ty: wildcard_ty,
							expr: Expr::Identifier {
								path: Path::simple(vec![temp], temp_span),
								span: temp_span,
							},
						},
						body: None,
						span,
					});

					statements.push(validation_directive);
				}
			}

			Pattern::TypedIdentifier { span, .. } => {
				self.diagnostics.push(compiler_bug!(
					span,
					"TypedIdentifier should be handled in the early return"
				));
			}

			Pattern::Variant { .. } => {
				self.diagnostics.push(
					DesugarError::generic(
						span,
						"variant patterns in var bindings not yet supported - use switch instead",
					)
					.build(),
				);
			}

			_ => {
				self.diagnostics
					.push(DesugarError::generic(span, "unsupported pattern type in var binding").build());
			}
		}

		return statements;
	}

	fn expand_or_patterns(pattern: Pattern) -> Vec<Pattern>
	{
		match pattern {
			Pattern::Or { patterns, .. } => {
				return patterns
					.into_iter()
					.flat_map(|p| return Self::expand_or_patterns(p))
					.collect();
			}

			Pattern::Variant { path, args, span } => {
				let expanded_args: Vec<Vec<Pattern>> = args
					.into_iter()
					.map(|arg| return Self::expand_or_patterns(arg))
					.collect();

				return Self::cartesian_product_patterns(expanded_args)
					.into_iter()
					.map(|args| {
						return Pattern::Variant {
							path: path.clone(),
							args,
							span,
						};
					})
					.collect();
			}

			Pattern::Tuple { patterns, span } => {
				let expanded: Vec<Vec<Pattern>> = patterns
					.into_iter()
					.map(|p| return Self::expand_or_patterns(p))
					.collect();

				return Self::cartesian_product_patterns(expanded)
					.into_iter()
					.map(|patterns| return Pattern::Tuple { patterns, span })
					.collect();
			}

			Pattern::Struct {
				path,
				fields,
				span,
				has_rest,
			} => {
				let field_names: Vec<Ident> = fields.iter().map(|(name, _)| return name.clone()).collect();
				let field_patterns: Vec<Vec<Pattern>> = fields
					.into_iter()
					.map(|(_, pat)| return Self::expand_or_patterns(pat))
					.collect();

				let expanded_field_sets: Vec<Vec<Pattern>> = Self::cartesian_product_patterns(field_patterns);

				return expanded_field_sets
					.into_iter()
					.map(|expanded_patterns| {
						let fields = field_names
							.iter()
							.zip(expanded_patterns)
							.map(|(name, pat)| return (name.clone(), pat))
							.collect();

						return Pattern::Struct {
							path: path.clone(),
							fields,
							span,
							has_rest,
						};
					})
					.collect();
			}

			other => return vec![other],
		}
	}

	fn cartesian_product_patterns(lists: Vec<Vec<Pattern>>) -> Vec<Vec<Pattern>>
	{
		if lists.is_empty() {
			return vec![vec![]];
		}

		let mut result = vec![vec![]];

		for list in lists {
			let mut new_result = Vec::new();
			for existing in &result {
				for item in &list {
					let mut new_combo = existing.clone();
					new_combo.push(item.clone());
					new_result.push(new_combo);
				}
			}
			result = new_result;
		}

		return result;
	}

	fn desugar_assignment_target(&mut self, target: Expr, source: Expr, span: Span) -> Vec<Stmt>
	{
		let mut statements = Vec::new();

		match target {
			Expr::Tuple {
				elements,
				span: tuple_span,
			} => {
				for (i, elem) in elements.into_iter().enumerate() {
					let index_expr = Expr::Field {
						base: Box::new(source.clone()),
						name: Path::simple(vec![i.to_string()], Span::default()),
						span: tuple_span,
					};

					let nested_stmts: Vec<Stmt> = self.desugar_assignment_target(elem, index_expr, span);
					statements.extend(nested_stmts);
				}
			}

			Expr::StructInit {
				path,
				fields,
				span: struct_span,
				base,
				has_rest,
			} => {
				assert!(base.is_none());
				let field_names: Vec<String> = fields.iter().map(|(name, _)| return name.clone()).collect();

				let validation_directive = Stmt::Directive(DirectiveNode {
					directive: Directive::ValidateStructPattern {
						struct_path: path,
						pattern_fields: field_names,
						has_rest,
					},
					body: None,
					span: struct_span,
				});

				statements.push(validation_directive);
				for (field_name, field_expr) in fields {
					let field_access = Expr::Field {
						base: Box::new(source.clone()),
						name: Path::simple(vec![field_name], Span::default()),
						span: struct_span,
					};

					let nested_stmts: Vec<Stmt> = self.desugar_assignment_target(field_expr, field_access, span);
					statements.extend(nested_stmts);
				}
			}

			other_expr => {
				statements.push(Stmt::Assignment {
					target: self.desugar_expr(other_expr),
					op: AssignOp::Assign,
					value: self.desugar_expr(source),
					span,
				});
			}
		}

		return statements;
	}
}

fn get_mentioned_type_params(path: &Path) -> Vec<String>
{
	let mut result = Vec::new();

	if path.segments.len() == 1 && path.segments[0].generics.is_empty() {
		result.push(path.segments[0].name.clone());
	}

	for segment in &path.segments {
		for generic_type in &segment.generics {
			result.extend(get_mentioned_type_params_in_type(generic_type));
		}
	}

	return result;
}

fn get_mentioned_type_params_in_type(ty: &Type) -> Vec<String>
{
	match ty.core.as_ref() {
		TypeCore::Base { path, generics } => {
			let mut result = Vec::new();

			if path.segments.len() == 1 && generics.is_empty() && path.segments[0].generics.is_empty() {
				result.push(path.segments[0].name.clone());
			}

			for generic_type in generics {
				result.extend(get_mentioned_type_params_in_type(generic_type));
			}

			for segment in &path.segments {
				for generic_type in &segment.generics {
					result.extend(get_mentioned_type_params_in_type(generic_type));
				}
			}

			return result;
		}
		TypeCore::Reference { inner, .. }
		| TypeCore::Mutable { inner }
		| TypeCore::Pointer { inner, .. }
		| TypeCore::Array { inner, size: _ } => {
			return get_mentioned_type_params_in_type_core(inner);
		}
		TypeCore::Tuple(types) => return types.iter().flat_map(get_mentioned_type_params_in_type).collect(),
		TypeCore::ImplTrait { bounds } => {
			return bounds
				.iter()
				.flat_map(|bound| match bound {
					WhereBound::Path { path, args } => {
						let mut result = get_mentioned_type_params(path);

						for arg in args {
							match arg {
								GenericArg::Type(ty) | GenericArg::Binding { ty, .. } => {
									result.extend(get_mentioned_type_params_in_type(ty));
								}
							}
						}

						return result;
					}

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

			if path.segments.len() == 1 && generics.is_empty() && path.segments[0].generics.is_empty() {
				result.push(path.segments[0].name.clone());
			}

			for generic_type in generics {
				result.extend(get_mentioned_type_params_in_type(generic_type));
			}

			for segment in &path.segments {
				for generic_type in &segment.generics {
					result.extend(get_mentioned_type_params_in_type(generic_type));
				}
			}

			return result;
		}
		TypeCore::Reference { inner, .. }
		| TypeCore::Mutable { inner }
		| TypeCore::Pointer { inner, .. }
		| TypeCore::Array { inner, .. } => return get_mentioned_type_params_in_type_core(inner),
		TypeCore::Tuple(types) => return types.iter().flat_map(get_mentioned_type_params_in_type).collect(),
		TypeCore::ImplTrait { .. } => return Vec::new(),
	}
}

/// Desugars a complete program AST into its simplified form.
///
/// This is the main entry point for desugaring. It transforms high-level syntax
/// constructs into equivalent primitive forms, making later compiler stages simpler.
///
/// # Arguments
/// * `program` - The parsed program AST to desugar
///
/// # Returns
/// * `Ok(DesugaredAST)` - The fully desugared program
/// * `Err(DesugarError)` - If desugaring encounters a semantic error
///
/// # Errors
/// Returns an error if:
/// - A constructor call is attempted on a reference or pointer type
/// - A complex pattern binding lacks an initializer
/// - A variant pattern is used in a variable binding (use `switch` instead)
/// - A type parameter appears in both the generic list (with bounds) and the where clause
///
/// # Examples
/// ```ignore
/// use crate::desugar::desugar_program;
///
/// let desugared = desugar_program(parsed_program)?;
/// ```
pub fn desugar_program(program: AST) -> Result<(DesugaredAST, Vec<DiagnosticBuilder>), Vec<DiagnosticBuilder>>
{
	let mut desugarer: Desugarer = Desugarer::new();
	let desugared_program = desugarer.desugar_program(program);
	if desugarer
		.diagnostics
		.iter()
		.any(|d| return d.severity.should_stop_compiling())
	{
		return Err(desugarer.diagnostics);
	}
	return Ok((desugared_program, desugarer.diagnostics));
}
