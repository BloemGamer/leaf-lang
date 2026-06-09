#[cfg(test)]
#[path = "../../tests/mir/tests.rs"]
mod tests;

mod display;

use std::collections::HashMap;

use leaf_proc::{CompileErrorKind, Spanned, compiler_bug};

use crate::{
	diagnostics::{CompileDiagnostic, CompileError, DiagnosticBuilder, ErrorCode},
	lexer::{IntSign, IntSize, IntType, Span, Spanned, StringFlags},
	name_resolution::ResolvedPathKind,
	parser::{AssignOp, BinaryOp, CallType, Literal, UnaryOp},
	source_map::SourceIndex,
	symbol_collection::{GlobalSymbolTable, SymbolId},
	type_analysis::{
		Primitive, Ty, TypedArrayLiteral, TypedBlock, TypedEnumDecl, TypedExpr, TypedExprKind, TypedFunctionDecl,
		TypedImplItem, TypedModule, TypedPattern, TypedStmt, TypedStructDecl, TypedSwitchBody, TypedTopLevelDecl,
		TypedTraitItem, TypedTypeAliasDecl, TypedUnionDecl, TypedVariableDecl, TypedVariantDecl, intrinsics::Intrinsic,
	},
};

#[derive(Debug, Clone, Spanned)]
pub struct MirError
{
	pub span: Span,
	pub kind: MirErrorKind,
}

#[derive(Debug, Clone, CompileErrorKind)]
#[compile_error_variant(CompileError::Mir)]
pub enum MirErrorKind
{
	#[error_msg("undefined label `'{label}`")]
	#[error_code(ErrorCode::MirUndefinedLabel)]
	UndefinedLabel
	{
		label: String
	},
}

/// A local variable or temporary within a `MirBody`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);

/// A basic block within a `MirBody`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone)]
pub struct MirModule
{
	pub path: Vec<String>,
	pub source_index: SourceIndex,
	pub items: Vec<MirItem>,
}

#[derive(Debug, Clone)]
pub enum MirItem
{
	Function(MirFunction),
	Global(MirGlobal),
	// Structs/enums/traits don't emit MIR bodies but we keep their metadata
	// around for codegen (layout, vtables, etc.).
	TypeDef(MirTypeDef),
}

#[derive(Debug, Clone)]
pub struct MirGlobal
{
	pub symbol: SymbolId,
	pub name: String,
	pub ty: Ty,
	pub init: MirOperand,
	pub mutable: bool,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MirTypeDef
{
	pub symbol: SymbolId,
	pub name: String,
	pub kind: MirTypeDefKind,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub enum MirTypeDefKind
{
	Struct
	{
		fields: Vec<(String, Ty)>
	},
	Union
	{
		fields: Vec<(String, Ty)>
	},
	Enum
	{
		variants: Vec<(String, Option<MirOperand>)>,
	},
	Variant
	{
		members: Vec<(String, Option<Ty>)>
	},
	TypeAlias
	{
		ty: Ty
	},
}

#[derive(Debug, Clone)]
pub struct MirFunction
{
	pub symbol: SymbolId,
	pub name: String,
	pub call_type: CallType,
	/// Parameters are the first `params.len()` locals in `body.locals`.
	pub params: Vec<MirParam>,
	pub return_ty: Ty,
	/// `None` for extern / trait declarations without a body.
	pub body: Option<MirBody>,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MirParam
{
	pub local: LocalId,
	pub name: String,
	pub ty: Ty,
	pub mutable: bool,
}

/// The control-flow graph for a single function body.
#[derive(Debug, Clone)]
pub struct MirBody
{
	/// All locals, indexed by `LocalId`. Params occupy `0..param_count`.
	pub locals: Vec<MirLocal>,
	pub param_count: usize,
	/// All basic blocks, indexed by `BlockId`. Block 0 is the entry block.
	pub blocks: Vec<MirBasicBlock>,
	/// The return local - every `return` stores into this before branching to
	/// the implicit exit block. `None` for `-> ()` / `-> !` functions.
	pub return_local: Option<LocalId>,
}

impl MirBody
{
	pub const fn entry_block(&self) -> BlockId
	{
		return BlockId(0);
	}

	pub fn local(&self, id: LocalId) -> &MirLocal
	{
		return &self.locals[id.0 as usize];
	}

	pub fn block(&self, id: BlockId) -> &MirBasicBlock
	{
		return &self.blocks[id.0 as usize];
	}
}

#[derive(Debug, Clone)]
pub struct MirLocal
{
	pub id: LocalId,
	pub ty: Ty,
	pub name: Option<String>,
	pub mutable: bool,
	/// Compiler-generated temporaries vs user-declared variables.
	pub is_temp: bool,
	pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MirBasicBlock
{
	pub id: BlockId,
	pub stmts: Vec<MirStmt>,
	pub terminator: MirTerminator,
}

/// A single side-effecting step that does not alter control flow.
#[derive(Debug, Clone)]
pub enum MirStmt
{
	/// `place = rvalue`
	Assign
	{
		place: MirPlace,
		rvalue: MirRvalue,
		span: Span,
	},

	/// A call whose return value is discarded (or is `()`).
	Call
	{
		callee: MirCallee,
		args: Vec<MirOperand>,
		span: Span,
	},

	/// `delete expr` - call destructor
	Delete
	{
		operand: MirOperand, span: Span
	},

	/// No-op; useful as a placeholder while building.
	Nop,
}

/// Every basic block ends with exactly one terminator.
#[derive(Debug, Clone)]
pub enum MirTerminator
{
	/// Unconditional jump.
	Goto
	{
		target: BlockId
	},

	/// `if cond { goto then_block } else { goto else_block }`
	Branch
	{
		cond: MirOperand,
		then_block: BlockId,
		else_block: BlockId,
	},

	/// Call that produces a value and resumes at `next`.
	CallAndContinue
	{
		callee: MirCallee,
		args: Vec<MirOperand>,
		dest: MirPlace,
		next: BlockId,
		/// Target block for unwinding (not yet used; placeholder for later, maybe).
		unwind: Option<BlockId>,
		span: Span,
	},

	/// Function returns. Value is already stored in `MirBody::return_local`.
	Return,

	/// Loops forever (diverges). Equivalent to `loop {}`.
	Unreachable,

	/// Switch / match dispatch.
	Switch
	{
		scrutinee: MirOperand,
		arms: Vec<MirSwitchArm>,
		otherwise: BlockId,
	},
}

#[derive(Debug, Clone)]
pub struct MirSwitchArm
{
	pub value: MirOperand,
	pub target: BlockId,
}

/// A location that can be read from or written to.
#[derive(Debug, Clone)]
pub struct MirPlace
{
	pub base: MirPlaceBase,
	pub projections: Vec<MirProjection>,
	pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum MirPlaceBase
{
	Local(LocalId),
	Global(SymbolId),
}

#[derive(Debug, Clone)]
pub enum MirProjection
{
	/// `base.field_name`
	Field
	{
		name: String, ty: Ty
	},
	/// `base[index]` - index must already be materialised into a local.
	Index
	{
		index: LocalId, ty: Ty
	},
	/// `*base`
	Deref,
}

/// A value that can be used as an input to an rvalue or call.
#[derive(Debug, Clone)]
pub enum MirOperand
{
	Copy(MirPlace),
	Move(MirPlace),
	Const(MirLiteral),
}

impl MirOperand
{
	pub const DUMMY: MirOperand = MirOperand::Const(MirLiteral {
		value: MirLiteralValue::Undef,
		ty: Ty::Never,
	});

	pub const fn ty(&self) -> &Ty
	{
		match self {
			MirOperand::Copy(p) | MirOperand::Move(p) => return &p.ty,
			MirOperand::Const(c) => return &c.ty,
		}
	}
}

/// A literal constant value with its type.
#[derive(Debug, Clone)]
pub struct MirLiteral
{
	pub value: MirLiteralValue,
	pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum MirLiteralValue
{
	Literal(Literal),
	ZeroInit,
	/// No explicit value provided; the backend assigns the next sequential
	/// discriminant (only meaningful inside `MirTypeDefKind::Enum`).
	Undef,
}

/// A pure computation that produces a value and can appear on the RHS of an
/// assignment.
#[derive(Debug, Clone)]
pub enum MirRvalue
{
	/// Use an operand as a value directly (copy / move / const).
	Use(MirOperand),

	Unary
	{
		op: UnaryOp,
		operand: MirOperand,
	},

	Binary
	{
		op: BinaryOp,
		lhs: MirOperand,
		rhs: MirOperand,
	},

	Cast
	{
		ty: Ty,
		operand: MirOperand,
	},

	/// Take the address of a place: `&place` or `&mut place`.
	Ref
	{
		mutable: bool,
		place: MirPlace,
	},

	/// Pointer arithmetic / raw address-of.
	RawPtr
	{
		mutable: bool,
		place: MirPlace,
	},

	/// Struct / union / variant literal. All field values are operands
	/// (i.e. already-computed locals or constants).
	Aggregate
	{
		kind: MirAggregateKind,
		fields: Vec<(String, MirOperand)>,
	},

	/// `[a, b, c]` - all elements are operands.
	Array
	{
		elements: Vec<MirOperand>,
		elem_ty: Ty,
	},

	/// `[val; count]`
	ArrayRepeat
	{
		value: MirOperand,
		count: MirOperand,
		elem_ty: Ty,
	},

	/// `(a, b, c)` - all elements are operands.
	Tuple(Vec<MirOperand>),

	/// Range literal - produces a range struct. Both bounds are optional
	/// operands (already-computed locals or constants).
	Range
	{
		start: Option<MirOperand>,
		end: Option<MirOperand>,
		inclusive: bool,
		elem_ty: Ty,
	},

	Discriminant(MirPlace),
}

#[derive(Debug, Clone)]
pub enum MirAggregateKind
{
	Struct(SymbolId),
	Union(SymbolId),
	/// Variant member constructor.
	VariantMember
	{
		parent: SymbolId,
		member: String,
	},
	Tuple,
}

/// What is being called.
#[derive(Debug, Clone)]
pub enum MirCallee
{
	/// Direct call to a known function symbol.
	Direct(SymbolId),

	/// Indirect call through a function pointer / closure stored in a local.
	Indirect(LocalId),

	/// Compiler intrinsic (maps 1-to-1 from `TypedExprKind::InternalCall`).
	Intrinsic(crate::type_analysis::intrinsics::Intrinsic),
}

struct LoopContext
{
	label: String,
	break_target: BlockId,
	continue_target: BlockId,
	result_local: Option<LocalId>,
}

struct BodyBuilder
{
	locals: Vec<MirLocal>,
	blocks: Vec<MirBasicBlock>,
	current_block: BlockId,
	return_local: Option<LocalId>,
	local_map: HashMap<SymbolId, LocalId>,
	loop_stack: Vec<LoopContext>,
}

impl BodyBuilder
{
	fn new(locals: Vec<MirLocal>) -> Self
	{
		let entry = MirBasicBlock {
			id: BlockId(0),
			stmts: Vec::new(),
			terminator: MirTerminator::Unreachable,
		};
		return Self {
			locals,
			blocks: vec![entry],
			current_block: BlockId(0),
			return_local: None,
			local_map: HashMap::new(),
			loop_stack: Vec::new(),
		};
	}

	fn alloc_local(&mut self, ty: Ty, name: Option<String>, mutable: bool, span: Span) -> LocalId
	{
		#[allow(clippy::cast_possible_truncation)]
		let id = LocalId(self.locals.len() as u32);
		self.locals.push(MirLocal {
			id,
			ty,
			name: name.clone(),
			mutable,
			is_temp: name.is_none(),
			span,
		});
		return id;
	}

	fn alloc_block(&mut self) -> BlockId
	{
		#[allow(clippy::cast_possible_truncation)]
		let id = BlockId(self.blocks.len() as u32);
		self.blocks.push(MirBasicBlock {
			id,
			stmts: Vec::new(),
			terminator: MirTerminator::Unreachable,
		});
		return id;
	}

	fn push_stmt(&mut self, stmt: MirStmt)
	{
		self.blocks[self.current_block.0 as usize].stmts.push(stmt);
	}

	fn set_terminator(&mut self, term: MirTerminator)
	{
		self.blocks[self.current_block.0 as usize].terminator = term;
	}

	fn finish(self, param_count: usize) -> MirBody
	{
		return MirBody {
			locals: self.locals,
			param_count,
			blocks: self.blocks,
			return_local: self.return_local,
		};
	}
}

struct MirLowerer<'a>
{
	diagnostics: Vec<DiagnosticBuilder>,
	global: &'a GlobalSymbolTable,
	module: &'a TypedModule,
}

impl<'a> MirLowerer<'a>
{
	const fn new(global: &'a GlobalSymbolTable, module: &'a TypedModule) -> Self
	{
		return Self {
			diagnostics: Vec::new(),
			global,
			module,
		};
	}

	fn copy_or_move(&self, place: MirPlace) -> MirOperand
	{
		if place
			.ty
			.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
		{
			MirOperand::Copy(place)
		} else {
			MirOperand::Move(place)
		}
	}

	fn lower_module(&mut self, module: &TypedModule) -> MirModule
	{
		let mut items = Vec::new();

		for decl in &module.ast.top_level_block.items {
			self.lower_top_level_decl(decl, &mut items);
		}

		return MirModule {
			path: module.path.clone(),
			source_index: module.ast.source_index,
			items,
		};
	}

	fn lower_top_level_decl(&mut self, decl: &TypedTopLevelDecl, out: &mut Vec<MirItem>)
	{
		match decl {
			TypedTopLevelDecl::Function(f) => {
				out.push(MirItem::Function(self.lower_function(f)));
			}
			TypedTopLevelDecl::VariableDecl(v) => {
				out.push(MirItem::Global(self.lower_global(v)));
			}
			TypedTopLevelDecl::Struct(s) => {
				out.push(MirItem::TypeDef(Self::lower_struct(s)));
			}
			TypedTopLevelDecl::Union(u) => {
				out.push(MirItem::TypeDef(Self::lower_union(u)));
			}
			TypedTopLevelDecl::Enum(e) => {
				out.push(MirItem::TypeDef(self.lower_enum(e)));
			}
			TypedTopLevelDecl::Variant(v) => {
				out.push(MirItem::TypeDef(Self::lower_variant(v)));
			}
			TypedTopLevelDecl::TypeAlias(t) => {
				out.push(MirItem::TypeDef(Self::lower_type_alias(t)));
			}
			TypedTopLevelDecl::Impl(i) => {
				for item in &i.items {
					match item {
						TypedImplItem::Function(f) => {
							out.push(MirItem::Function(self.lower_function(f)));
						}
						TypedImplItem::Const(v) => {
							out.push(MirItem::Global(self.lower_global(v)));
						}
						TypedImplItem::TypeAlias(t) => {
							out.push(MirItem::TypeDef(Self::lower_type_alias(t)));
						}
						TypedImplItem::AssocType(_) => {
							// Associated types are purely a type-level construct;
							// nothing to emit in MIR.
						}
					}
				}
			}
			TypedTopLevelDecl::Trait(t) => {
				// Only lower the default method bodies.
				for item in &t.items {
					if let TypedTraitItem::Function(f) = item
						&& f.body.is_some()
					{
						out.push(MirItem::Function(self.lower_function(f)));
					}
				}
			}
			TypedTopLevelDecl::Module(m) => {
				if let Some(body) = &m.resolved_body {
					for inner in &body.items {
						self.lower_top_level_decl(inner, out);
					}
				}
			}
			// Directives have no MIR representation.
			TypedTopLevelDecl::Directive(_) => {}
		}
	}

	fn lower_function(&mut self, f: &TypedFunctionDecl) -> MirFunction
	{
		let mut locals: Vec<MirLocal> = Vec::new();
		let mut params: Vec<MirParam> = Vec::new();

		for param in &f.signature.params {
			#[allow(clippy::cast_possible_truncation)]
			let id: LocalId = LocalId(locals.len() as u32);
			locals.push(MirLocal {
				id,
				ty: param.ty.clone(),
				name: Some(param.name.clone()),
				mutable: param.mutable,
				is_temp: false,
				span: param.span,
			});
			params.push(MirParam {
				local: id,
				name: param.name.clone(),
				ty: param.ty.clone(),
				mutable: param.mutable,
			});
		}

		let body = f.body.as_ref().map(|b| {
			let mut builder = BodyBuilder::new(locals.clone());

			if !matches!(f.signature.return_type, Ty::Unit | Ty::Never) {
				let ret_id = builder.alloc_local(
					f.signature.return_type.clone(),
					Some("#return".to_string()),
					true,
					f.span,
				);
				builder.return_local = Some(ret_id);
			}

			self.lower_block_into(&mut builder, b);

			builder.set_terminator(MirTerminator::Return);

			return builder.finish(params.len());
		});

		return MirFunction {
			symbol: f.resolved_name,
			name: f.signature.name.clone(),
			call_type: f.signature.call_type,
			params,
			return_ty: f.signature.return_type.clone(),
			body,
			span: f.span,
		};
	}

	fn lower_global(&mut self, v: &TypedVariableDecl) -> MirGlobal
	{
		return MirGlobal {
			symbol: v.resolved_name,
			name: v.name.clone(),
			ty: v.ty.clone(),
			init: v.init.as_ref().map_or_else(
				|| {
					return MirOperand::Const(MirLiteral {
						value: MirLiteralValue::Undef,
						ty: v.ty.clone(),
					});
				},
				|init| return self.lower_expr_as_operand(init),
			),
			mutable: v.mutable,
			span: v.span,
		};
	}

	fn lower_block_into(&mut self, builder: &mut BodyBuilder, block: &TypedBlock)
	{
		for stmt in &block.stmts {
			self.lower_stmt_into(builder, stmt);
		}
		if let Some(tail_expr) = &block.tail_expr {
			// Value is discarded
			// a block as a stmt should doesn't give back anything
			self.lower_expr_into(builder, tail_expr);
		}
	}

	fn lower_stmt_into(&mut self, builder: &mut BodyBuilder, stmt: &TypedStmt)
	{
		match stmt {
			TypedStmt::VariableDecl(var) => {
				let var_id: LocalId =
					builder.alloc_local(var.ty.clone(), Some(var.name.clone()), var.mutable, var.span());

				builder.local_map.insert(var.resolved_name, var_id);

				if let Some(init) = &var.init {
					let operand = self.lower_expr_into(builder, init);
					builder.push_stmt(MirStmt::Assign {
						place: MirPlace {
							base: MirPlaceBase::Local(var_id),
							projections: Vec::new(),
							ty: var.ty.clone(),
						},
						rvalue: MirRvalue::Use(operand),
						span: var.span(),
					});
				}
			}
			TypedStmt::Assignment {
				target,
				op,
				value,
				span,
			} => {
				if !matches!(op, AssignOp::Assign) {
					self.diagnostics.push(compiler_bug!(
						*span,
						"the type resolution should have changed all the other assignments to function calls"
					));
					return;
				}

				let place = self.lower_expr_as_place(builder, target);
				let operand = self.lower_expr_into(builder, value);

				builder.push_stmt(MirStmt::Assign {
					place,
					rvalue: MirRvalue::Use(operand),
					span: *span,
				});
			}
			TypedStmt::Return { value, span } => {
				if let Some(val) = value {
					let operand = self.lower_expr_into(builder, val);
					if let Some(ret_local) = builder.return_local {
						builder.push_stmt(MirStmt::Assign {
							place: MirPlace {
								base: MirPlaceBase::Local(ret_local),
								projections: Vec::new(),
								ty: operand.ty().clone(),
							},
							rvalue: MirRvalue::Use(operand),
							span: *span,
						});
					}
				}

				builder.set_terminator(MirTerminator::Return);

				// allocate a new block for dead code, but it should still be checked ect
				let dead: BlockId = builder.alloc_block();
				builder.current_block = dead;
			}
			TypedStmt::Expr(typed_expr) => {
				self.lower_expr_into(builder, typed_expr);
			}
			TypedStmt::Break { label, value, span } => {
				let (break_block, result_local) =
					if let Some(ct) = builder.loop_stack.iter().rev().find(|ctx| return ctx.label == *label) {
						(ct.break_target, ct.result_local)
					} else {
						self.diagnostics.push(
							MirError {
								span: *span,
								kind: MirErrorKind::UndefinedLabel { label: label.clone() },
							}
							.build(),
						);
						(BlockId(0), None)
					};

				if let Some(val) = value {
					let operand = self.lower_expr_into(builder, val);
					if let Some(nresult_local) = result_local {
						builder.push_stmt(MirStmt::Assign {
							place: MirPlace {
								base: MirPlaceBase::Local(nresult_local),
								projections: Vec::new(),
								ty: operand.ty().clone(),
							},
							rvalue: MirRvalue::Use(operand),
							span: *span,
						});
					}
				}

				builder.set_terminator(MirTerminator::Goto { target: break_block });

				let dead: BlockId = builder.alloc_block();
				builder.current_block = dead;
			}
			TypedStmt::Continue { label, span } => {
				let continue_block =
					if let Some(ct) = builder.loop_stack.iter().rev().find(|ctx| return ctx.label == *label) {
						ct.continue_target
					} else {
						self.diagnostics.push(
							MirError {
								span: *span,
								kind: MirErrorKind::UndefinedLabel { label: label.clone() },
							}
							.build(),
						);
						BlockId(0)
					};
				builder.set_terminator(MirTerminator::Goto { target: continue_block });

				// allocate a new block for dead code, but it should still be checked ect
				let dead: BlockId = builder.alloc_block();
				builder.current_block = dead;
			}
			TypedStmt::If {
				cond,
				then_block,
				else_branch,
				span,
			} => {
				let cond_operand: MirOperand = self.lower_expr_into(builder, cond);

				let then_bb: BlockId = builder.alloc_block();
				let else_bb: BlockId = builder.alloc_block();
				let merge_bb: BlockId = builder.alloc_block();

				builder.set_terminator(MirTerminator::Branch {
					cond: cond_operand,
					then_block: then_bb,
					else_block: else_bb,
				});

				builder.current_block = then_bb;
				self.lower_block_into(builder, then_block);
				builder.set_terminator(MirTerminator::Goto { target: merge_bb });

				builder.current_block = else_bb;
				if let Some(else_block) = else_branch {
					self.lower_stmt_into(builder, else_block);
				}
				builder.set_terminator(MirTerminator::Goto { target: merge_bb });

				builder.current_block = merge_bb;
			}
			TypedStmt::Loop { label, body, span } => {
				let loop_bb: BlockId = builder.alloc_block();
				let exit_loop_bb: BlockId = builder.alloc_block();

				builder.loop_stack.push(LoopContext {
					label: label.clone(),
					break_target: exit_loop_bb,
					continue_target: loop_bb,
					result_local: None,
				});

				builder.current_block = loop_bb;

				self.lower_block_into(builder, body);

				builder.set_terminator(MirTerminator::Goto { target: loop_bb });

				builder.current_block = exit_loop_bb;
			}
			TypedStmt::Delete { expr, span } => {
				let del_op: MirOperand = self.lower_expr_into(builder, expr);
				builder.push_stmt(MirStmt::Delete {
					operand: del_op,
					span: *span,
				});
			}
			TypedStmt::Unsafe(typed_block) | TypedStmt::Block(typed_block) => {
				self.lower_block_into(builder, typed_block);
			}
			TypedStmt::Directive(_) => {
				// Directive should not generate any MIR I think, well, when meta programming is introduced probably it should, but not for now
			}
			TypedStmt::Pending(span) => self.diagnostics.push(compiler_bug!(
				*span,
				"no TypedStmt::Pending should be able to leak to the MIR generation"
			)),
		}
	}

	fn lower_expr_into(&mut self, builder: &mut BodyBuilder, expr: &TypedExpr) -> MirOperand
	{
		return match &expr.kind {
			TypedExprKind::Identifier { path } => match &path.kind {
				ResolvedPathKind::Resolved(sym) => {
					let place: MirPlace = if let Some(&local_id) = builder.local_map.get(sym) {
						MirPlace {
							base: MirPlaceBase::Local(local_id),
							projections: Vec::new(),
							ty: expr.ty.clone(),
						}
					} else {
						MirPlace {
							base: MirPlaceBase::Global(*sym),
							projections: Vec::new(),
							ty: expr.ty.clone(),
						}
					};
					if expr
						.ty
						.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
					{
						MirOperand::Copy(place)
					} else {
						MirOperand::Move(place)
					}
				}
				ResolvedPathKind::AssocItem { base, .. } => {
					let place: MirPlace = MirPlace {
						base: MirPlaceBase::Global(*base),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					};
					if expr
						.ty
						.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
					{
						MirOperand::Copy(place)
					} else {
						MirOperand::Move(place)
					}
				}
				ResolvedPathKind::Primitive(_) => {
					self.diagnostics.push(compiler_bug!(
						expr.span,
						"primitive type used as value expression in MIR lowering"
					));
					MirOperand::Const(MirLiteral {
						value: MirLiteralValue::Undef,
						ty: expr.ty.clone(),
					})
				}
			},
			TypedExprKind::Literal { value } => MirOperand::Const(MirLiteral {
				value: MirLiteralValue::Literal(value.clone()),
				ty: expr.ty.clone(),
			}),
			TypedExprKind::Default { heap_call: _ } => {
				self.diagnostics.push(compiler_bug!(
					Span::default(),
					"`TypedExprKind::Default` should not be leaking to the MIR lowering"
				)); // TODO: fix the span I guess
				MirOperand::Const(MirLiteral {
					value: MirLiteralValue::Undef,
					ty: Ty::Never,
				})
			}
			TypedExprKind::Unary { op, expr: u_expr } => {
				let operand: MirOperand = self.lower_expr_into(builder, u_expr);
				let tmp: LocalId = builder.alloc_local(u_expr.ty.clone(), None, false, u_expr.span());

				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(tmp),
						projections: Vec::new(),
						ty: u_expr.ty.clone(),
					},
					rvalue: MirRvalue::Unary { op: *op, operand },
					span: u_expr.span(),
				});

				let place: MirPlace = MirPlace {
					base: MirPlaceBase::Local(tmp),
					projections: Vec::new(),
					ty: expr.ty.clone(),
				};

				if expr
					.ty
					.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
				{
					MirOperand::Copy(place)
				} else {
					MirOperand::Move(place)
				}
			}

			TypedExprKind::Binary { op, lhs, rhs } => {
				let lhs_operand: MirOperand = self.lower_expr_into(builder, lhs);
				let rhs_operand: MirOperand = self.lower_expr_into(builder, rhs);
				let tmp: LocalId = builder.alloc_local(expr.ty.clone(), None, false, expr.span());

				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(tmp),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					},
					rvalue: MirRvalue::Binary {
						op: *op,
						lhs: lhs_operand,
						rhs: rhs_operand,
					},
					span: expr.span(),
				});

				let place: MirPlace = MirPlace {
					base: MirPlaceBase::Local(tmp),
					projections: Vec::new(),
					ty: expr.ty.clone(),
				};

				if expr
					.ty
					.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
				{
					MirOperand::Copy(place)
				} else {
					MirOperand::Move(place)
				}
			}
			TypedExprKind::Cast { ty, expr: c_expr } => {
				let operand: MirOperand = self.lower_expr_into(builder, c_expr);
				let tmp: LocalId = builder.alloc_local(c_expr.ty.clone(), None, false, c_expr.span());

				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(tmp),
						projections: Vec::new(),
						ty: c_expr.ty.clone(),
					},
					rvalue: MirRvalue::Cast {
						ty: ty.clone(),
						operand,
					},
					span: c_expr.span(),
				});

				let place: MirPlace = MirPlace {
					base: MirPlaceBase::Local(tmp),
					projections: Vec::new(),
					ty: c_expr.ty.clone(),
				};

				if c_expr
					.ty
					.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
				{
					MirOperand::Copy(place)
				} else {
					MirOperand::Move(place)
				}
			}
			TypedExprKind::Call {
				callee,
				call_type: _,
				named_generics: _,
				args,
			} => {
				let lowered_args: Vec<MirOperand> =
					args.iter().map(|a| return self.lower_expr_into(builder, a)).collect();

				let lowered_callee: MirCallee = match &callee.kind {
					TypedExprKind::Identifier { path } => match &path.kind {
						ResolvedPathKind::Resolved(sym) => {
							// TODO: when implementing function pointers, lambda/closures ect, this should change to a check and make MirCallee::Indirect() for that call
							MirCallee::Direct(*sym)
						}
						ResolvedPathKind::AssocItem { base, .. } => MirCallee::Direct(*base),
						ResolvedPathKind::Primitive(_) => {
							self.diagnostics
								.push(compiler_bug!(callee.span, "primitive used as callee in MIR lowering"));
							return MirOperand::Const(MirLiteral {
								value: MirLiteralValue::Undef,
								ty: expr.ty.clone(),
							});
						}
					},
					TypedExprKind::InternalCall { intrinsic } => MirCallee::Intrinsic(intrinsic.clone()),

					_ => {
						let operand: MirOperand = self.lower_expr_into(builder, callee);
						let temp: LocalId = builder.alloc_local(callee.ty.clone(), None, false, callee.span());
						builder.push_stmt(MirStmt::Assign {
							place: MirPlace {
								base: MirPlaceBase::Local(temp),
								projections: Vec::new(),
								ty: callee.ty.clone(),
							},
							rvalue: MirRvalue::Use(operand),
							span: callee.span(),
						});
						MirCallee::Indirect(temp)
					}
				};

				let next_bb: BlockId = builder.alloc_block();

				if matches!(expr.ty, Ty::Unit | Ty::Never) {
					let dummy: LocalId = builder.alloc_local(expr.ty.clone(), None, false, expr.span());
					builder.set_terminator(MirTerminator::CallAndContinue {
						callee: lowered_callee,
						args: lowered_args,
						dest: MirPlace {
							base: MirPlaceBase::Local(dummy),
							projections: Vec::new(),
							ty: expr.ty.clone(),
						},
						next: next_bb,
						unwind: None,
						span: expr.span(),
					});
					builder.current_block = next_bb;
					MirOperand::Const(MirLiteral {
						value: MirLiteralValue::ZeroInit,
						ty: expr.ty.clone(),
					})
				} else {
					let temp: LocalId = builder.alloc_local(expr.ty.clone(), None, false, expr.span());
					builder.set_terminator(MirTerminator::CallAndContinue {
						callee: lowered_callee,
						args: lowered_args,
						dest: MirPlace {
							base: MirPlaceBase::Local(temp),
							projections: Vec::new(),
							ty: expr.ty.clone(),
						},
						next: next_bb,
						unwind: None,
						span: expr.span(),
					});
					builder.current_block = next_bb;
					if expr
						.ty
						.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
					{
						MirOperand::Copy(MirPlace {
							base: MirPlaceBase::Local(temp),
							projections: Vec::new(),
							ty: expr.ty.clone(),
						})
					} else {
						MirOperand::Move(MirPlace {
							base: MirPlaceBase::Local(temp),
							projections: Vec::new(),
							ty: expr.ty.clone(),
						})
					}
				}
			}
			TypedExprKind::InternalCall { intrinsic: _ } => {
				// TypedExprKind::InternalCall is only allowed to be used by `TypedExprKind::InternalCall`, outside of here should be caught earlier
				self.diagnostics.push(compiler_bug!(
					expr.span,
					"`TypedExprKind::InternalCall` appeared outside of a `TypedExprKind::Call` expression in MIR lowering"
				));
				MirOperand::Const(MirLiteral {
					value: MirLiteralValue::Undef,
					ty: expr.ty.clone(),
				})
			}
			TypedExprKind::Field { base, name } => {
				let base_place: MirPlace = self.lower_expr_as_place(builder, base);
				let place: MirPlace = MirPlace {
					ty: expr.ty.clone(),
					projections: {
						let mut projs: Vec<MirProjection> = base_place.projections.clone();
						projs.push(MirProjection::Field {
							name: name.clone(),
							ty: expr.ty.clone(),
						});
						projs
					},
					base: base_place.base,
				};
				if expr
					.ty
					.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
				{
					MirOperand::Copy(place)
				} else {
					MirOperand::Move(place)
				}
			}
			TypedExprKind::Index { base, index } => {
				let base_place: MirPlace = self.lower_expr_as_place(builder, base);
				let index_operand: MirOperand = self.lower_expr_into(builder, index);
				let index_local: LocalId = builder.alloc_local(index.ty.clone(), None, false, index.span());
				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(index_local),
						projections: Vec::new(),
						ty: index.ty.clone(),
					},
					rvalue: MirRvalue::Use(index_operand),
					span: index.span(),
				});
				let place: MirPlace = MirPlace {
					ty: expr.ty.clone(),
					projections: {
						let mut projs: Vec<MirProjection> = base_place.projections.clone();
						projs.push(MirProjection::Index {
							index: index_local,
							ty: expr.ty.clone(),
						});
						projs
					},
					base: base_place.base,
				};
				if expr
					.ty
					.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
				{
					MirOperand::Copy(place)
				} else {
					MirOperand::Move(place)
				}
			}
			TypedExprKind::Range(_) => {
				self.diagnostics.push(compiler_bug!(
					expr.span(),
					"`TypedExprKind::Range` should be removed by the desugarer"
				));
				MirOperand::DUMMY
			}
			TypedExprKind::Tuple { elements } => {
				let operands: Vec<MirOperand> = elements
					.iter()
					.map(|e| return self.lower_expr_into(builder, e))
					.collect();
				let temp: LocalId = builder.alloc_local(expr.ty.clone(), None, false, expr.span());
				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(temp),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					},
					rvalue: MirRvalue::Tuple(operands),
					span: expr.span(),
				});
				if expr
					.ty
					.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
				{
					MirOperand::Copy(MirPlace {
						base: MirPlaceBase::Local(temp),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					})
				} else {
					MirOperand::Move(MirPlace {
						base: MirPlaceBase::Local(temp),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					})
				}
			}
			TypedExprKind::Array(array_literal) => {
				let elem_ty: Ty = if let Ty::Array { inner, .. } = &expr.ty {
					*inner.clone()
				} else {
					self.diagnostics.push(compiler_bug!(
						expr.span,
						"array literal does not have array type in MIR lowering"
					));
					Ty::Unit
				};
				let temp: LocalId = builder.alloc_local(expr.ty.clone(), None, false, expr.span());
				let rvalue: MirRvalue = match array_literal {
					TypedArrayLiteral::List { elements, .. } => {
						let operands: Vec<MirOperand> = elements
							.iter()
							.map(|e| return self.lower_expr_into(builder, e))
							.collect();
						MirRvalue::Array {
							elements: operands,
							elem_ty,
						}
					}
					TypedArrayLiteral::Repeat { value, count, .. } => {
						let value_operand: MirOperand = self.lower_expr_into(builder, value);
						let count_operand: MirOperand = self.lower_expr_into(builder, count);
						MirRvalue::ArrayRepeat {
							value: value_operand,
							count: count_operand,
							elem_ty,
						}
					}
				};
				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(temp),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					},
					rvalue,
					span: expr.span(),
				});
				if expr
					.ty
					.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
				{
					MirOperand::Copy(MirPlace {
						base: MirPlaceBase::Local(temp),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					})
				} else {
					MirOperand::Move(MirPlace {
						base: MirPlaceBase::Local(temp),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					})
				}
			}
			TypedExprKind::StructInit {
				path,
				fields,
				base,
				has_rest,
			} => {
				if *has_rest || base.is_some() {
					self.diagnostics.push(compiler_bug!(
						expr.span(),
						"base should always be None, and has_rest has to be false"
					));
				}
				let symbol: SymbolId = match &path.kind {
					ResolvedPathKind::Resolved(sym) => *sym,
					ResolvedPathKind::AssocItem { base, .. } => *base,
					ResolvedPathKind::Primitive(_) => {
						self.diagnostics.push(compiler_bug!(
							expr.span,
							"primitive type used as struct init path in MIR lowering"
						));
						return MirOperand::Const(MirLiteral {
							value: MirLiteralValue::Undef,
							ty: expr.ty.clone(),
						});
					}
				};

				let lowered_fields: Vec<(String, MirOperand)> = fields
					.iter()
					.map(|f| return (f.0.clone(), self.lower_expr_into(builder, &f.1)))
					.collect();

				let temp: LocalId = builder.alloc_local(expr.ty.clone(), None, false, expr.span());
				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(temp),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					},
					rvalue: MirRvalue::Aggregate {
						kind: MirAggregateKind::Struct(symbol),
						fields: lowered_fields,
					},
					span: expr.span(),
				});

				if expr
					.ty
					.implements_copy(&self.module.caches.trait_impls, self.module.caches.copy_sym)
				{
					MirOperand::Copy(MirPlace {
						base: MirPlaceBase::Local(temp),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					})
				} else {
					MirOperand::Move(MirPlace {
						base: MirPlaceBase::Local(temp),
						projections: Vec::new(),
						ty: expr.ty.clone(),
					})
				}
			}
			TypedExprKind::Block(typed_block) | TypedExprKind::UnsafeBlock(typed_block) => {
				for stmt in &typed_block.stmts {
					self.lower_stmt_into(builder, stmt);
				}

				typed_block.tail_expr.as_ref().map_or(
					MirOperand::Const(MirLiteral {
						value: MirLiteralValue::ZeroInit,
						ty: Ty::Unit,
					}),
					|tail| return self.lower_expr_into(builder, tail),
				)
			}
			TypedExprKind::Switch { expr: scrutinee, arms } => {
				let scrutinee_operand: MirOperand = self.lower_expr_into(builder, scrutinee);

				let scrutinee_local: LocalId = builder.alloc_local(scrutinee.ty.clone(), None, false, scrutinee.span());
				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(scrutinee_local),
						projections: Vec::new(),
						ty: scrutinee.ty.clone(),
					},
					rvalue: MirRvalue::Use(scrutinee_operand),
					span: scrutinee.span(),
				});

				let result_local: Option<LocalId> = if matches!(expr.ty, Ty::Unit | Ty::Never) {
					None
				} else {
					Some(builder.alloc_local(expr.ty.clone(), None, false, expr.span()))
				};

				let merge_bb: BlockId = builder.alloc_block();
				let unreachable_bb: BlockId = builder.alloc_block();

				let mut next_test_bb: BlockId = builder.current_block;

				for (i, arm) in arms.iter().enumerate() {
					let is_last: bool = i == arms.len() - 1;
					let body_bb: BlockId = builder.alloc_block();
					let fail_bb: BlockId = if is_last { unreachable_bb } else { builder.alloc_block() };
					builder.current_block = next_test_bb;
					self.lower_pattern_test(builder, &arm.pattern, scrutinee_local, &scrutinee.ty, body_bb, fail_bb);

					builder.current_block = body_bb;

					self.lower_pattern_bindings(builder, &arm.pattern, scrutinee_local);

					let arm_operand: MirOperand = match &arm.body {
						TypedSwitchBody::Expr(e) => self.lower_expr_into(builder, e),
						TypedSwitchBody::Block(b) => {
							for stmt in &b.stmts {
								self.lower_stmt_into(builder, stmt);
							}
							b.tail_expr.as_ref().map_or(
								MirOperand::Const(MirLiteral {
									value: MirLiteralValue::ZeroInit,
									ty: Ty::Unit,
								}),
								|tail| return self.lower_expr_into(builder, tail),
							)
						}
					};

					if let Some(res) = result_local {
						builder.push_stmt(MirStmt::Assign {
							place: MirPlace {
								base: MirPlaceBase::Local(res),
								projections: Vec::new(),
								ty: expr.ty.clone(),
							},
							rvalue: MirRvalue::Use(arm_operand),
							span: arm.span,
						});
					}
					builder.set_terminator(MirTerminator::Goto { target: merge_bb });
					next_test_bb = fail_bb;
				}

				builder.current_block = unreachable_bb;
				// TODO: the checker does not validate if patterns are exausted, so inserting a panic for the default value, should be removed if this check is implemented
				{
					let panic_bb: BlockId = builder.alloc_block();
					let base: MirPlaceBase =
						MirPlaceBase::Local(builder.alloc_local(Ty::Unit, None, false, expr.span()));

					builder.set_terminator(MirTerminator::CallAndContinue {
						callee: MirCallee::Intrinsic(Intrinsic::Panic),
						args: vec![MirOperand::Const(MirLiteral {
							value: MirLiteralValue::Literal(Literal::String {
								value: "non-exhaustive match".to_string(),
								flags: StringFlags::NONE,
								span: scrutinee.span(),
							}),
							ty: Ty::Primitive(Primitive::Str),
						})],
						dest: MirPlace {
							base,
							projections: Vec::new(),
							ty: Ty::Unit,
						},
						next: panic_bb,
						unwind: None,
						span: expr.span(),
					});
					builder.current_block = panic_bb;
					builder.set_terminator(MirTerminator::Unreachable);
				}

				builder.current_block = merge_bb;

				result_local.map_or(
					MirOperand::Const(MirLiteral {
						value: MirLiteralValue::ZeroInit,
						ty: Ty::Unit,
					}),
					|res| {
						let place: MirPlace = MirPlace {
							base: MirPlaceBase::Local(res),
							projections: Vec::new(),
							ty: expr.ty.clone(),
						};
						return self.copy_or_move(place);
					},
				)
			}

			TypedExprKind::If {
				cond,
				then_block,
				else_branch,
			} => {
				let cond_operand: MirOperand = self.lower_expr_into(builder, cond);

				let then_bb: BlockId = builder.alloc_block();
				let else_bb: BlockId = builder.alloc_block();
				let merge_bb: BlockId = builder.alloc_block();

				let result_local: Option<LocalId> = if matches!(expr.ty, Ty::Unit | Ty::Never) {
					None
				} else {
					Some(builder.alloc_local(expr.ty.clone(), None, false, expr.span()))
				};

				builder.set_terminator(MirTerminator::Branch {
					cond: cond_operand,
					then_block: then_bb,
					else_block: else_bb,
				});

				builder.current_block = then_bb;
				for stmt in &then_block.stmts {
					self.lower_stmt_into(builder, stmt);
				}
				if let Some(res) = result_local
					&& let Some(tail) = &then_block.tail_expr
				{
					let then_operand = self.lower_expr_into(builder, tail);
					builder.push_stmt(MirStmt::Assign {
						place: MirPlace {
							base: MirPlaceBase::Local(res),
							projections: Vec::new(),
							ty: expr.ty.clone(),
						},
						rvalue: MirRvalue::Use(then_operand),
						span: expr.span(),
					});
				}
				builder.set_terminator(MirTerminator::Goto { target: merge_bb });

				builder.current_block = else_bb;
				if let Some(else_expr) = else_branch {
					let else_operand = self.lower_expr_into(builder, else_expr);
					if let Some(res) = result_local {
						builder.push_stmt(MirStmt::Assign {
							place: MirPlace {
								base: MirPlaceBase::Local(res),
								projections: Vec::new(),
								ty: expr.ty.clone(),
							},
							rvalue: MirRvalue::Use(else_operand),
							span: expr.span(),
						});
					}
				}
				builder.set_terminator(MirTerminator::Goto { target: merge_bb });

				builder.current_block = merge_bb;

				result_local.map_or(
					MirOperand::Const(MirLiteral {
						value: MirLiteralValue::ZeroInit,
						ty: Ty::Unit,
					}),
					|res| {
						let place = MirPlace {
							base: MirPlaceBase::Local(res),
							projections: Vec::new(),
							ty: expr.ty.clone(),
						};
						return self.copy_or_move(place);
					},
				)
			}

			TypedExprKind::Loop { label, body } => {
				let loop_bb: BlockId = builder.alloc_block();
				let exit_bb: BlockId = builder.alloc_block();

				let result_local: Option<LocalId> = if matches!(expr.ty, Ty::Unit | Ty::Never) {
					None
				} else {
					Some(builder.alloc_local(expr.ty.clone(), None, false, expr.span()))
				};

				builder.set_terminator(MirTerminator::Goto { target: loop_bb });

				builder.loop_stack.push(LoopContext {
					label: label.clone(),
					break_target: exit_bb,
					continue_target: loop_bb,
					result_local,
				});

				builder.current_block = loop_bb;
				self.lower_block_into(builder, body);
				builder.set_terminator(MirTerminator::Goto { target: loop_bb });

				builder.loop_stack.pop();

				builder.current_block = exit_bb;

				result_local.map_or(
					MirOperand::Const(MirLiteral {
						value: MirLiteralValue::ZeroInit,
						ty: Ty::Unit,
					}),
					|res| {
						let place: MirPlace = MirPlace {
							base: MirPlaceBase::Local(res),
							projections: Vec::new(),
							ty: expr.ty.clone(),
						};
						return self.copy_or_move(place);
					},
				)
			}
		};
	}

	fn lower_pattern_test(
		&mut self,
		builder: &mut BodyBuilder,
		pattern: &TypedPattern,
		scrutinee_local: LocalId,
		scrutinee_ty: &Ty,
		success: BlockId,
		fail: BlockId,
	)
	{
		match pattern {
			TypedPattern::Wildcard { .. } | TypedPattern::TypedIdentifier { .. } => {
				builder.set_terminator(MirTerminator::Goto { target: success });
			}

			TypedPattern::Literal { value, ty, span } => {
				let scrutinee_operand: MirOperand = self.copy_or_move(MirPlace {
					base: MirPlaceBase::Local(scrutinee_local),
					projections: Vec::new(),
					ty: scrutinee_ty.clone(),
				});
				let pattern_operand: MirOperand = MirOperand::Const(MirLiteral {
					value: MirLiteralValue::Literal(value.clone()),
					ty: ty.clone(),
				});
				let cmp_local: LocalId = builder.alloc_local(Ty::Primitive(Primitive::Bool), None, false, *span);
				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(cmp_local),
						projections: Vec::new(),
						ty: Ty::Primitive(Primitive::Bool),
					},
					rvalue: MirRvalue::Binary {
						op: BinaryOp::Eq,
						lhs: scrutinee_operand,
						rhs: pattern_operand,
					},
					span: *span,
				});
				builder.set_terminator(MirTerminator::Branch {
					cond: MirOperand::Copy(MirPlace {
						base: MirPlaceBase::Local(cmp_local),
						projections: Vec::new(),
						ty: Ty::Primitive(Primitive::Bool),
					}),
					then_block: success,
					else_block: fail,
				});
			}

			TypedPattern::Range(range_expr) => {
				let span: Span = range_expr.span();
				let scrutinee_place: MirPlace = MirPlace {
					base: MirPlaceBase::Local(scrutinee_local),
					projections: Vec::new(),
					ty: scrutinee_ty.clone(),
				};

				let check_upper_bb: BlockId = builder.alloc_block();

				if let Some(start) = &range_expr.start {
					let start_operand: MirOperand = self.lower_expr_as_operand(start);
					let ge_local: LocalId = builder.alloc_local(Ty::Primitive(Primitive::Bool), None, false, span);
					builder.push_stmt(MirStmt::Assign {
						place: MirPlace {
							base: MirPlaceBase::Local(ge_local),
							projections: Vec::new(),
							ty: Ty::Primitive(Primitive::Bool),
						},
						rvalue: MirRvalue::Binary {
							op: BinaryOp::Ge,
							lhs: MirOperand::Copy(scrutinee_place.clone()),
							rhs: start_operand,
						},
						span,
					});
					builder.set_terminator(MirTerminator::Branch {
						cond: MirOperand::Copy(MirPlace {
							base: MirPlaceBase::Local(ge_local),
							projections: Vec::new(),
							ty: Ty::Primitive(Primitive::Bool),
						}),
						then_block: check_upper_bb,
						else_block: fail,
					});
				} else {
					builder.set_terminator(MirTerminator::Goto { target: check_upper_bb });
				}
				builder.current_block = check_upper_bb;

				if let Some(end) = &range_expr.end {
					let end_operand: MirOperand = self.lower_expr_as_operand(end);
					let le_local: LocalId = builder.alloc_local(Ty::Primitive(Primitive::Bool), None, false, span);
					let upper_op: BinaryOp = if range_expr.inclusive {
						BinaryOp::Le
					} else {
						BinaryOp::Lt
					};
					builder.push_stmt(MirStmt::Assign {
						place: MirPlace {
							base: MirPlaceBase::Local(le_local),
							projections: Vec::new(),
							ty: Ty::Primitive(Primitive::Bool),
						},
						rvalue: MirRvalue::Binary {
							op: upper_op,
							lhs: MirOperand::Copy(scrutinee_place),
							rhs: end_operand,
						},
						span,
					});
					builder.set_terminator(MirTerminator::Branch {
						cond: MirOperand::Copy(MirPlace {
							base: MirPlaceBase::Local(le_local),
							projections: Vec::new(),
							ty: Ty::Primitive(Primitive::Bool),
						}),
						then_block: success,
						else_block: fail,
					});
				} else {
					builder.set_terminator(MirTerminator::Goto { target: success });
				}
			}

			TypedPattern::Or { patterns, .. } => {
				let mut next_bb: BlockId = builder.current_block;
				for (i, alt) in patterns.iter().enumerate() {
					builder.current_block = next_bb;
					let alt_fail: BlockId = if i == patterns.len() - 1 {
						fail
					} else {
						builder.alloc_block()
					};
					self.lower_pattern_test(builder, alt, scrutinee_local, scrutinee_ty, success, alt_fail);
					next_bb = alt_fail;
				}
			}

			TypedPattern::Tuple { patterns, ty, span } => {
				let mut chain: Vec<(BlockId, BlockId, &TypedPattern, usize)> = Vec::new();
				let mut next_success: BlockId = success;
				for (idx, sub) in patterns.iter().enumerate().rev() {
					let test_bb: BlockId = if idx == 0 {
						builder.current_block
					} else {
						builder.alloc_block()
					};
					chain.push((test_bb, next_success, sub, idx));
					next_success = test_bb;
				}
				chain.reverse();
				for (test_bb, sub_success, sub_pattern, idx) in chain {
					builder.current_block = test_bb;
					let elem_ty: Ty = sub_pattern.ty().clone();
					let elem_local: LocalId = builder.alloc_local(elem_ty.clone(), None, false, sub_pattern.span());
					builder.push_stmt(MirStmt::Assign {
						place: MirPlace {
							base: MirPlaceBase::Local(elem_local),
							projections: Vec::new(),
							ty: elem_ty.clone(),
						},
						rvalue: MirRvalue::Use(MirOperand::Copy(MirPlace {
							base: MirPlaceBase::Local(scrutinee_local),
							projections: vec![MirProjection::Field {
								name: idx.to_string(),
								ty: elem_ty.clone(),
							}],
							ty: elem_ty.clone(),
						})),
						span: sub_pattern.span(),
					});
					self.lower_pattern_test(builder, sub_pattern, elem_local, &elem_ty, sub_success, fail);
				}
			}

			TypedPattern::Variant {
				path,
				args,
				ty: _,
				span,
			} => {
				const USIZE_TY: Ty = Ty::Primitive(Primitive::Int(IntType {
					bits: IntSize::Size,
					sign: IntSign::Unsigned,
				}));

				let tag_local: LocalId = builder.alloc_local(USIZE_TY.clone(), None, false, *span);
				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(tag_local),
						projections: Vec::new(),
						ty: USIZE_TY.clone(),
					},
					rvalue: MirRvalue::Discriminant(MirPlace {
						base: MirPlaceBase::Local(scrutinee_local),
						projections: Vec::new(),
						ty: scrutinee_ty.clone(),
					}),
					span: *span,
				});

				let variant_sym: SymbolId = match &path.kind {
					ResolvedPathKind::Resolved(s) => *s,
					ResolvedPathKind::AssocItem { base, .. } => *base,
					ResolvedPathKind::Primitive(_) => {
						self.diagnostics
							.push(compiler_bug!(*span, "primitives can't be variants I think"));
						builder.set_terminator(MirTerminator::Goto { target: fail });
						return;
					}
				};

				let discriminant: usize = self.variant_discriminant(variant_sym);
				let expected_tag: MirOperand = MirOperand::Const(MirLiteral {
					value: MirLiteralValue::Literal(Literal::Int {
						value: discriminant.to_string(),
						base: crate::lexer::IntBase::Decimal,
						ty: None,
						span: *span,
					}),
					ty: USIZE_TY.clone(),
				});

				let eq_local: LocalId = builder.alloc_local(Ty::Primitive(Primitive::Bool), None, false, *span);
				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(eq_local),
						projections: Vec::new(),
						ty: Ty::Primitive(Primitive::Bool),
					},
					rvalue: MirRvalue::Binary {
						op: BinaryOp::Eq,
						lhs: MirOperand::Copy(MirPlace {
							base: MirPlaceBase::Local(tag_local),
							projections: Vec::new(),
							ty: USIZE_TY,
						}),
						rhs: expected_tag,
					},
					span: *span,
				});

				let sub_check_bb: BlockId = if args.is_empty() {
					success
				} else {
					builder.alloc_block()
				};

				builder.set_terminator(MirTerminator::Branch {
					cond: MirOperand::Copy(MirPlace {
						base: MirPlaceBase::Local(eq_local),
						projections: Vec::new(),
						ty: Ty::Primitive(Primitive::Bool),
					}),
					then_block: sub_check_bb,
					else_block: fail,
				});

				if !args.is_empty() {
					let mut test_blocks: Vec<BlockId> = vec![sub_check_bb];
					for _ in 1..args.len() {
						test_blocks.push(builder.alloc_block());
					}

					for (idx, (sub_pattern, &test_bb)) in args.iter().zip(test_blocks.iter()).enumerate() {
						let sub_success: BlockId = if idx == args.len() - 1 {
							success
						} else {
							test_blocks[idx + 1]
						};

						builder.current_block = test_bb;

						let elem_ty: Ty = sub_pattern.ty().clone();
						let elem_local: LocalId = builder.alloc_local(elem_ty.clone(), None, false, sub_pattern.span());
						builder.push_stmt(MirStmt::Assign {
							place: MirPlace {
								base: MirPlaceBase::Local(elem_local),
								projections: Vec::new(),
								ty: elem_ty.clone(),
							},
							rvalue: MirRvalue::Use(self.copy_or_move(MirPlace {
								base: MirPlaceBase::Local(scrutinee_local),
								projections: vec![MirProjection::Field {
									name: idx.to_string(),
									ty: elem_ty.clone(),
								}],
								ty: elem_ty.clone(),
							})),
							span: sub_pattern.span(),
						});

						self.lower_pattern_test(builder, sub_pattern, elem_local, &elem_ty, sub_success, fail);
					}
				}
			}

			TypedPattern::Struct {
				path: _,
				fields,
				has_rest: _,
				ty: _,
				span: _,
			} => {
				let mut next_success: BlockId = success;
				let mut chain: Vec<(BlockId, BlockId, &TypedPattern, String)> = Vec::new();
				for (idx, (field_name, sub)) in fields.iter().enumerate().rev() {
					let test_bb: BlockId = if idx == 0 {
						builder.current_block
					} else {
						builder.alloc_block()
					};
					chain.push((test_bb, next_success, sub, field_name.clone()));
					next_success = test_bb;
				}
				if chain.is_empty() {
					builder.set_terminator(MirTerminator::Goto { target: success });
					return;
				}
				chain.reverse();
				for (test_bb, sub_success, sub_pattern, field_name) in chain {
					builder.current_block = test_bb;
					let field_ty: Ty = sub_pattern.ty().clone();
					let field_local: LocalId = builder.alloc_local(field_ty.clone(), None, false, sub_pattern.span());
					builder.push_stmt(MirStmt::Assign {
						place: MirPlace {
							base: MirPlaceBase::Local(field_local),
							projections: Vec::new(),
							ty: field_ty.clone(),
						},
						rvalue: MirRvalue::Use(MirOperand::Copy(MirPlace {
							base: MirPlaceBase::Local(scrutinee_local),
							projections: vec![MirProjection::Field {
								name: field_name.clone(),
								ty: field_ty.clone(),
							}],
							ty: field_ty.clone(),
						})),
						span: sub_pattern.span(),
					});
					self.lower_pattern_test(builder, sub_pattern, field_local, &field_ty, sub_success, fail);
				}
			}
		}
	}

	fn variant_discriminant(&mut self, variant_sym: SymbolId) -> usize
	{
		let sym: &crate::symbol_collection::Symbol = self.global.symbol(variant_sym);
		let parent_scope: &crate::symbol_collection::Scope = self.global.scope(sym.scope);
		return parent_scope
			.symbols
			.iter()
			.position(|&s| return s == variant_sym)
			.unwrap_or_else(|| {
				self.diagnostics.push(compiler_bug!(
					Span::default(),
					"variant symbol not found in parent scope"
				));
				return 0;
			});
	}

	fn lower_pattern_bindings(&mut self, builder: &mut BodyBuilder, pattern: &TypedPattern, scrutinee_local: LocalId)
	{
		match pattern {
			TypedPattern::TypedIdentifier {
				symbol,
				name,
				ty,
				mutable,
				span,
			} => {
				let local: LocalId = builder.alloc_local(ty.clone(), Some(name.clone()), *mutable, *span);
				builder.local_map.insert(*symbol, local);
				let operand: MirOperand = self.copy_or_move(MirPlace {
					base: MirPlaceBase::Local(scrutinee_local),
					projections: Vec::new(),
					ty: ty.clone(),
				});
				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(local),
						projections: Vec::new(),
						ty: ty.clone(),
					},
					rvalue: MirRvalue::Use(operand),
					span: *span,
				});
			}

			TypedPattern::Wildcard { .. } | TypedPattern::Literal { .. } | TypedPattern::Range(_) => {}

			TypedPattern::Or { patterns, .. } => {
				if let Some(first) = patterns.first() {
					self.lower_pattern_bindings(builder, first, scrutinee_local);
				}
			}

			TypedPattern::Tuple { patterns, .. } => {
				for (idx, sub) in patterns.iter().enumerate() {
					let elem_ty: Ty = sub.ty().clone();
					let elem_local: LocalId = builder.alloc_local(elem_ty.clone(), None, false, sub.span());
					let operand: MirOperand = self.copy_or_move(MirPlace {
						base: MirPlaceBase::Local(scrutinee_local),
						projections: vec![MirProjection::Field {
							name: idx.to_string(),
							ty: elem_ty.clone(),
						}],
						ty: elem_ty.clone(),
					});
					builder.push_stmt(MirStmt::Assign {
						place: MirPlace {
							base: MirPlaceBase::Local(elem_local),
							projections: Vec::new(),
							ty: elem_ty.clone(),
						},
						rvalue: MirRvalue::Use(operand),
						span: sub.span(),
					});
					self.lower_pattern_bindings(builder, sub, elem_local);
				}
			}

			TypedPattern::Variant { args, .. } => {
				for (idx, sub) in args.iter().enumerate() {
					let elem_ty: Ty = sub.ty().clone();
					let elem_local: LocalId = builder.alloc_local(elem_ty.clone(), None, false, sub.span());
					let operand: MirOperand = self.copy_or_move(MirPlace {
						base: MirPlaceBase::Local(scrutinee_local),
						projections: vec![MirProjection::Field {
							name: idx.to_string(),
							ty: elem_ty.clone(),
						}],
						ty: elem_ty.clone(),
					});
					builder.push_stmt(MirStmt::Assign {
						place: MirPlace {
							base: MirPlaceBase::Local(elem_local),
							projections: Vec::new(),
							ty: elem_ty.clone(),
						},
						rvalue: MirRvalue::Use(operand),
						span: sub.span(),
					});
					self.lower_pattern_bindings(builder, sub, elem_local);
				}
			}

			TypedPattern::Struct { fields, .. } => {
				for (field_name, sub) in fields {
					let field_ty: Ty = sub.ty().clone();
					let field_local: LocalId = builder.alloc_local(field_ty.clone(), None, false, sub.span());
					let operand: MirOperand = self.copy_or_move(MirPlace {
						base: MirPlaceBase::Local(scrutinee_local),
						projections: vec![MirProjection::Field {
							name: field_name.clone(),
							ty: field_ty.clone(),
						}],
						ty: field_ty.clone(),
					});
					builder.push_stmt(MirStmt::Assign {
						place: MirPlace {
							base: MirPlaceBase::Local(field_local),
							projections: Vec::new(),
							ty: field_ty.clone(),
						},
						rvalue: MirRvalue::Use(operand),
						span: sub.span(),
					});
					self.lower_pattern_bindings(builder, sub, field_local);
				}
			}
		}
	}

	fn lower_expr_as_place(&mut self, builder: &mut BodyBuilder, expr: &TypedExpr) -> MirPlace
	{
		let operand: MirOperand = self.lower_expr_into(builder, expr);
		match operand {
			MirOperand::Copy(place) | MirOperand::Move(place) => return place,
			MirOperand::Const(_) => {
				// Constants can't be assigned to directly; materialize into a temp first
				let ty: Ty = operand.ty().clone();
				let temp: LocalId = builder.alloc_local(ty.clone(), None, false, expr.span());
				builder.push_stmt(MirStmt::Assign {
					place: MirPlace {
						base: MirPlaceBase::Local(temp),
						projections: Vec::new(),
						ty: ty.clone(),
					},
					rvalue: MirRvalue::Use(operand),
					span: expr.span(),
				});
				return MirPlace {
					base: MirPlaceBase::Local(temp),
					projections: Vec::new(),
					ty,
				};
			}
		}
	}

	fn lower_expr_as_operand(&mut self, e: &TypedExpr) -> MirOperand
	{
		let mut builder: BodyBuilder = BodyBuilder::new(Vec::new());
		return self.lower_expr_into(&mut builder, e);
	}

	fn lower_struct(s: &TypedStructDecl) -> MirTypeDef
	{
		return MirTypeDef {
			symbol: s.resolved_name,
			name: s.name.clone(),
			kind: MirTypeDefKind::Struct {
				fields: s.fields.iter().map(|f| return (f.name.clone(), f.ty.clone())).collect(),
			},
			span: s.span,
		};
	}

	fn lower_union(u: &TypedUnionDecl) -> MirTypeDef
	{
		return MirTypeDef {
			symbol: u.resolved_name,
			name: u.name.clone(),
			kind: MirTypeDefKind::Union {
				fields: u.fields.iter().map(|f| return (f.name.clone(), f.ty.clone())).collect(),
			},
			span: u.span,
		};
	}

	fn lower_enum(&mut self, e: &TypedEnumDecl) -> MirTypeDef
	{
		return MirTypeDef {
			symbol: e.resolved_name,
			name: e.name.clone(),
			kind: MirTypeDefKind::Enum {
				variants: e
					.variants
					.iter()
					.map(|v| {
						let c = v.value.as_ref().map(|te| return self.lower_expr_as_operand(te));
						return (v.name.clone(), c);
					})
					.collect(),
			},
			span: e.span,
		};
	}

	fn lower_variant(v: &TypedVariantDecl) -> MirTypeDef
	{
		return MirTypeDef {
			symbol: v.resolved_name,
			name: v.name.clone(),
			kind: MirTypeDefKind::Variant {
				members: v
					.variants
					.iter()
					.map(|m| return (m.name.clone(), m.ty.clone()))
					.collect(),
			},
			span: v.span,
		};
	}

	fn lower_type_alias(t: &TypedTypeAliasDecl) -> MirTypeDef
	{
		return MirTypeDef {
			symbol: t.resolved_name,
			name: t.name.clone(),
			kind: MirTypeDefKind::TypeAlias { ty: t.ty.clone() },
			span: t.span,
		};
	}
}

pub fn lower_module(
	module: &TypedModule,
	global: &GlobalSymbolTable,
) -> Result<(MirModule, Vec<DiagnosticBuilder>), Vec<DiagnosticBuilder>>
{
	let mut lowerer = MirLowerer::new(global, module);

	let mir_mod: MirModule = lowerer.lower_module(module);

	if lowerer
		.diagnostics
		.iter()
		.any(|d| return d.severity.should_stop_compiling())
	{
		return Err(lowerer.diagnostics);
	}
	return Ok((mir_mod, lowerer.diagnostics));
}
