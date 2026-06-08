mod display;

use std::collections::HashMap;

use leaf_proc::{CompileErrorKind, Spanned, compiler_bug};

use crate::{
	diagnostics::{CompileDiagnostic, CompileError, DiagnosticBuilder, ErrorCode},
	lexer::{Span, Spanned},
	parser::{AssignOp, BinaryOp, CallType, Literal, UnaryOp},
	source_map::SourceIndex,
	symbol_collection::{GlobalSymbolTable, SymbolId},
	type_analysis::{
		Ty, TypedBlock, TypedEnumDecl, TypedExpr, TypedFunctionDecl, TypedImplItem, TypedModule, TypedStmt,
		TypedStructDecl, TypedTopLevelDecl, TypedTraitItem, TypedTypeAliasDecl, TypedUnionDecl, TypedVariableDecl,
		TypedVariantDecl,
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
		op: UnaryOp, operand: MirOperand
	},

	Binary
	{
		op: BinaryOp,
		lhs: MirOperand,
		rhs: MirOperand,
	},

	Cast
	{
		ty: Ty, operand: MirOperand
	},

	/// Take the address of a place: `&place` or `&mut place`.
	Ref
	{
		mutable: bool, place: MirPlace
	},

	/// Pointer arithmetic / raw address-of.
	RawPtr
	{
		mutable: bool, place: MirPlace
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
		elements: Vec<MirOperand>, elem_ty: Ty
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
}

impl<'a> MirLowerer<'a>
{
	const fn new(global: &'a GlobalSymbolTable) -> Self
	{
		return Self {
			diagnostics: Vec::new(),
			global,
		};
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
					Some("__return".to_string()),
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
			todo!("lower_block_into: Some(block.tail_expr)")
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
			TypedStmt::Expr(typed_expr) => todo!(),
			TypedStmt::Break { label, value, span } => todo!(),
			TypedStmt::Continue { label, span } => {
				let continue_block = match builder.loop_stack.iter().rev().find(|ctx| return ctx.label == *label) {
					Some(ct) => ct.continue_target,
					None => {
						self.diagnostics.push(
							MirError {
								span: *span,
								kind: MirErrorKind::UndefinedLabel { label: label.clone() },
							}
							.build(),
						);
						BlockId(0)
					}
				};
			}
			TypedStmt::If {
				cond,
				then_block,
				else_branch,
				span,
			} => todo!(),
			TypedStmt::Loop { label, body, span } => todo!(),
			TypedStmt::Delete { expr, span } => todo!(),
			TypedStmt::Unsafe(typed_block) | TypedStmt::Block(typed_block) => todo!(),
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
		todo!("lower_expr_into")
	}

	fn lower_expr_as_place(&mut self, builder: &mut BodyBuilder, expr: &TypedExpr) -> MirPlace
	{
		todo!("lower_expr_as_place")
	}

	fn lower_expr_as_operand(&mut self, e: &TypedExpr) -> MirOperand
	{
		todo!("lower_expr_as_operand");
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
						let c = v.value.as_ref().map(|te| self.lower_expr_as_operand(te));
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
	let mut lowerer = MirLowerer::new(global);

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
