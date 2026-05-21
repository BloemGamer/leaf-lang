#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::needless_pass_by_ref_mut)]
#![allow(clippy::unused_self)]

mod tests;

use std::fmt;

use ignorable::PartialEq;

use leaf_proc::Spanned;

use crate::{
	desugar::DesugaredAST,
	diagnostics::{CompileDiagnostic, CompileError, DiagnosticBuilder, ErrorCode},
	lexer::{Span, Spanned},
	parser::{
		self, AssignOp, AssocTypeDecl, BinaryOp, CallType, EnumDecl, FunctionSignature, GenericArg, Ident, ImplDecl,
		Literal, ModuleDecl, ModuleKind, Path, PathSegment, RangeExpr, StructDecl, TopLevelDecl, TraitDecl,
		TypeAliasDecl, TypeCore, UnaryOp, UnionDecl, VariableDecl, WhereBound, WhereConstraint,
	},
	source_map::SourceIndex,
	symbol_collection::{
		GlobalSymbolTable, LocalSymbolTable, Scope, ScopeId, Symbol, SymbolId, SymbolKind, Visibility,
	},
	type_analysis::{self, Ty, intrinsics::Intrinsic},
};

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedPath
{
	pub original: Path,
	pub kind: ResolvedPathKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedPathKind
{
	Resolved(SymbolId),
	AssocItem
	{
		base: SymbolId,
		member: String,
	},
	Primitive(type_analysis::Ty),
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedType
{
	pub core: Box<ResolvedTypeCore>,
	#[ignored(PartialEq)]
	pub span: Span,
}

enum ResolvedPathResult
{
	Full(SymbolId),
	Assoc
	{
		base: SymbolId,
		member: String,
	},
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedTypeCore
{
	Base
	{
		path: ResolvedPath,
		generics: Vec<ResolvedType>,
	},
	/// Primitive / unresolved single-segment name (e.g. `i32`, `bool`, `_`).
	/// The resolver tolerates missing single-segment names to allow primitives.
	/// But probably should be removed later
	Primitive
	{
		name: String,
		generics: Vec<ResolvedType>,
	},
	Reference
	{
		mutable: bool,
		inner: Box<ResolvedTypeCore>,
	},
	Mutable
	{
		inner: Box<ResolvedTypeCore>,
	},
	Pointer
	{
		mutable: bool,
		inner: Box<ResolvedTypeCore>,
	},
	Array
	{
		inner: Box<ResolvedTypeCore>,
		size: Option<Box<ResolvedExpr>>,
	},
	Tuple(Vec<ResolvedType>),
	ImplTrait
	{
		bounds: Vec<ResolvedWhereBound>,
	},
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Spanned)]
pub struct ResolvedWhereConstraint
{
	pub ty: String,
	pub bounds: Vec<ResolvedWhereBound>,
	pub type_args: Vec<ResolvedType>,
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedWhereBound
{
	Path
	{
		path: ResolvedPath,
		args: Vec<ResolvedGenericArg>,
	},
	Func(ResolvedFuncBound),
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Spanned)]
pub enum ResolvedGenericArg
{
	Type(ResolvedType),
	Binding
	{
		name: String,
		ty: ResolvedType,
		span: Span,
	},
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedFuncBound
{
	Fn
	{
		args: Vec<ResolvedType>,
		ret: Option<ResolvedType>,
	},
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub enum ResolvedPattern
{
	Wildcard
	{
		ty: Option<ResolvedType>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Literal
	{
		value: Literal,
		#[ignored(PartialEq)]
		span: Span,
	},
	TypedIdentifier
	{
		symbol: SymbolId,
		name: String,
		ty: ResolvedType,
		mutable: bool,
		#[ignored(PartialEq)]
		span: Span,
	},
	Variant
	{
		path: ResolvedPath,
		args: Vec<ResolvedPattern>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Tuple
	{
		patterns: Vec<ResolvedPattern>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Struct
	{
		path: ResolvedPath,
		fields: Vec<(String, ResolvedPattern)>,
		has_rest: bool,
		#[ignored(PartialEq)]
		span: Span,
	},
	Range(ResolvedRangeExpr),
	Or
	{
		patterns: Vec<ResolvedPattern>,
		#[ignored(PartialEq)]
		span: Span,
	},
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedRangeExpr
{
	pub start: Option<Box<ResolvedExpr>>,
	pub end: Option<Box<ResolvedExpr>>,
	pub inclusive: bool,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub enum ResolvedArrayLiteral
{
	List
	{
		elements: Vec<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Repeat
	{
		value: Box<ResolvedExpr>,
		count: Box<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub enum ResolvedExpr
{
	/// A resolved identifier reference.
	Identifier
	{
		path: ResolvedPath,
		#[ignored(PartialEq)]
		span: Span,
	},
	/// An identifier that could not be resolved (e.g. a local not yet declared
	/// at the point of use — tolerated during this pass).
	UnresolvedIdentifier
	{
		path: Path,
		#[ignored(PartialEq)]
		span: Span,
	},
	/// For a later pass, when type info is known ect, specificly for impl statements
	AssocPath
	{
		base: ResolvedPath,
		member: PathSegment,
		#[ignored(PartialEq)]
		span: Span,
	},
	AssocSelf
	{
		member: PathSegment,
		#[ignored(PartialEq)]
		span: Span,
	},
	InternalCall
	{
		intrinsic: Intrinsic,
		#[ignored(PartialEq)]
		span: Span,
	},
	Literal
	{
		value: Literal,
		#[ignored(PartialEq)]
		span: Span,
	},
	Default
	{
		heap_call: CallType,
		#[ignored(PartialEq)]
		span: Span,
	},
	Unary
	{
		op: UnaryOp,
		expr: Box<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Binary
	{
		op: BinaryOp,
		lhs: Box<ResolvedExpr>,
		rhs: Box<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Cast
	{
		ty: ResolvedType,
		expr: Box<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Call
	{
		callee: Box<ResolvedExpr>,
		call_type: CallType,
		named_generics: Vec<(String, ResolvedType)>,
		args: Vec<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Field
	{
		base: Box<ResolvedExpr>,
		name: String,
		#[ignored(PartialEq)]
		span: Span,
	},
	Index
	{
		base: Box<ResolvedExpr>,
		index: Box<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Range(ResolvedRangeExpr),
	Tuple
	{
		elements: Vec<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Array(ResolvedArrayLiteral),
	StructInit
	{
		path: ResolvedPath,
		fields: Vec<(String, ResolvedExpr)>,
		base: Option<Box<ResolvedExpr>>,
		has_rest: bool,
		#[ignored(PartialEq)]
		span: Span,
	},
	Block(Box<ResolvedBlock>),
	UnsafeBlock(Box<ResolvedBlock>),
	Switch
	{
		expr: Box<ResolvedExpr>,
		arms: Vec<ResolvedSwitchArm>,
		#[ignored(PartialEq)]
		span: Span,
	},
	If
	{
		cond: Box<ResolvedExpr>,
		then_block: ResolvedBlock,
		else_branch: Option<Box<ResolvedExpr>>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Loop
	{
		label: Option<String>,
		body: Box<ResolvedBlock>,
		#[ignored(PartialEq)]
		span: Span,
	},
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedBlock
{
	pub stmts: Vec<ResolvedStmt>,
	pub tail_expr: Option<Box<ResolvedExpr>>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedSwitchArm
{
	pub pattern: ResolvedPattern,
	pub body: ResolvedSwitchBody,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused, clippy::large_enum_variant)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub enum ResolvedSwitchBody
{
	Expr(ResolvedExpr),
	Block(ResolvedBlock),
}

#[allow(unused, clippy::large_enum_variant)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub enum ResolvedStmt
{
	VariableDecl(ResolvedVariableDecl),
	Assignment
	{
		target: ResolvedExpr,
		op: AssignOp,
		value: ResolvedExpr,
		#[ignored(PartialEq)]
		span: Span,
	},
	Return
	{
		value: Option<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Expr(ResolvedExpr),
	Break
	{
		label: Option<String>,
		value: Option<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Continue
	{
		label: Option<String>,
		#[ignored(PartialEq)]
		span: Span,
	},
	If
	{
		cond: ResolvedExpr,
		then_block: ResolvedBlock,
		else_branch: Option<Box<ResolvedStmt>>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Loop
	{
		label: Option<String>,
		body: ResolvedBlock,
		#[ignored(PartialEq)]
		span: Span,
	},
	Delete
	{
		expr: ResolvedExpr,
		#[ignored(PartialEq)]
		span: Span,
	},
	Unsafe(ResolvedBlock),
	Block(ResolvedBlock),
	Directive(ResolvedDirectiveNode),
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedDirectiveNode
{
	pub directive: ResolvedDirective,
	/// for now will always be `None`
	pub body: Option<ResolvedBlock>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedDirective
{
	Import
	{
		import: String, visibility: Visibility
	},
	Use
	{
		use_path: Path, visibility: Visibility
	},
	Custom
	{
		name: String,
		params: Vec<parser::DirectiveParam>,
	},
	ValidateStructPattern
	{
		struct_path: ResolvedPath,
		pattern_fields: Vec<String>,
		has_rest: bool,
	},
	ValidateType
	{
		ty: ResolvedType, expr: ResolvedExpr
	},
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedParam
{
	pub symbol: SymbolId,
	pub name: String,
	pub ty: ResolvedType,
	pub mutable: bool,
	pub variadic: bool,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedFunctionSignature
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<(Ident, Span)>,
	pub heap_generics: Vec<ResolvedGenericHeapParam>,
	pub call_type: CallType,
	pub params: Vec<ResolvedParam>,
	pub return_type: ResolvedType,
	pub where_clause: Vec<ResolvedWhereConstraint>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedAST
{
	pub top_level_block: ResolvedTopLevelBlock,
	pub source_index: SourceIndex,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedTopLevelBlock
{
	pub items: Vec<ResolvedTopLevelDecl>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub enum ResolvedTopLevelDecl
{
	Function(ResolvedFunctionDecl),
	VariableDecl(ResolvedVariableDecl),
	Struct(ResolvedStructDecl),
	Union(ResolvedUnionDecl),
	Enum(ResolvedEnumDecl),
	Variant(ResolvedVariantDecl),
	TypeAlias(ResolvedTypeAliasDecl),
	Trait(ResolvedTraitDecl),
	Module(ResolvedModuleDecl),
	Impl(ResolvedImplDecl),
	Directive(ResolvedDirectiveNode),
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedFunctionDecl
{
	pub resolved_name: SymbolId,
	pub signature: ResolvedFunctionSignature,
	pub body: Option<ResolvedBlock>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedVariableDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub ty: ResolvedType,
	pub init: Option<ResolvedExpr>,
	pub comp_const: bool,
	pub mutable: bool,
	pub modifiers: Vec<parser::Modifier>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedStructField
{
	pub name: String,
	pub ty: ResolvedType,
	pub default_value: Option<ResolvedExpr>,
	pub modifiers: Vec<parser::Modifier>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedStructDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub fields: Vec<ResolvedStructField>,
	pub where_clause: Vec<ResolvedWhereConstraint>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedUnionField
{
	pub name: String,
	pub ty: ResolvedType,
	pub modifiers: Vec<parser::Modifier>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedUnionDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub fields: Vec<ResolvedUnionField>,
	pub where_clause: Vec<ResolvedWhereConstraint>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedEnumVariant
{
	pub name: String,
	pub value: Option<ResolvedExpr>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedEnumDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub variants: Vec<ResolvedEnumVariant>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedVariantMember
{
	pub name: String,
	pub ty: Option<ResolvedType>,
	pub value: Option<ResolvedExpr>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedVariantDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub variants: Vec<ResolvedVariantMember>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedTypeAliasDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub ty: ResolvedType,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedAssocTypeDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub ty: Option<ResolvedType>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub enum ResolvedTraitItem
{
	Function(ResolvedFunctionDecl),
	TypeAlias(ResolvedTypeAliasDecl),
	AssocType(ResolvedAssocTypeDecl),
	Const(ResolvedVariableDecl),
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedTraitDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub super_traits: Vec<ResolvedWhereBound>,
	pub items: Vec<ResolvedTraitItem>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedModuleDecl
{
	pub resolved_name: SymbolId,
	pub name: String,
	pub modifiers: Vec<parser::Modifier>,
	pub resolved_body: Option<ResolvedTopLevelBlock>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub enum ResolvedImplItem
{
	Function(ResolvedFunctionDecl),
	TypeAlias(ResolvedTypeAliasDecl),
	AssocType(ResolvedAssocTypeDecl),
	Const(ResolvedVariableDecl),
}

#[allow(unused)]
#[derive(Debug, Clone, Spanned, PartialEq)]
pub struct ResolvedImplDecl
{
	pub resolved_target: ResolvedPath,
	pub resolved_trait: Option<ResolvedPath>,
	pub modifiers: Vec<parser::Modifier>,
	pub generics: Vec<parser::GenericParam>,
	pub where_clause: Vec<ResolvedWhereConstraint>,
	pub items: Vec<ResolvedImplItem>,
	#[ignored(PartialEq)]
	pub docs: Option<parser::DocsComment>,
	#[ignored(PartialEq)]
	pub span: Span,
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedModule
{
	pub path: Vec<String>,
	pub ast: ResolvedAST,
	pub symbols: LocalSymbolTable,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedGenericHeapKind
{
	Forwarded,
	Forced(ResolvedType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedGenericHeapParam
{
	pub name: Ident,
	pub kind: ResolvedGenericHeapKind,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NameResolutionErrorKind
{
	UnresolvedPath
	{
		path: Path
	},

	ShadowedVariable
	{
		name: String, first_definition: Span
	},

	PrivateSymbol
	{
		path: Path
	},

	UnresolvedUseTarget
	{
		path: Path
	},

	AmbiguousName
	{
		name: String, candidates: Vec<Path>
	},
}

#[derive(Debug, Clone, Spanned)]
pub struct NameResolutionError
{
	pub span: Span,
	pub kind: NameResolutionErrorKind,
	pub context: Vec<String>,
}

#[allow(unused)]
impl NameResolutionError
{
	pub const fn new(span: Span, kind: NameResolutionErrorKind) -> Self
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

	pub const fn unresolved_path(span: Span, path: Path) -> Self
	{
		return Self::new(span, NameResolutionErrorKind::UnresolvedPath { path });
	}

	pub fn shadowed_variable(span: Span, name: impl Into<String>, first_definition: Span) -> Self
	{
		return Self::new(
			span,
			NameResolutionErrorKind::ShadowedVariable {
				name: name.into(),
				first_definition,
			},
		);
	}

	pub const fn private_symbol(span: Span, path: Path) -> Self
	{
		return Self::new(span, NameResolutionErrorKind::PrivateSymbol { path });
	}

	pub const fn unresolved_use_target(span: Span, path: Path) -> Self
	{
		return Self::new(span, NameResolutionErrorKind::UnresolvedUseTarget { path });
	}

	pub fn ambiguous_name(span: Span, name: impl Into<String>, candidates: Vec<Path>) -> Self
	{
		return Self::new(
			span,
			NameResolutionErrorKind::AmbiguousName {
				name: name.into(),
				candidates,
			},
		);
	}
}

impl fmt::Display for NameResolutionError
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match &self.kind {
			NameResolutionErrorKind::UnresolvedPath { path } => {
				write!(f, "unresolved path `{}`", path)
			}

			NameResolutionErrorKind::ShadowedVariable { name, first_definition } => {
				write!(
					f,
					"variable `{}` shadows an existing binding (first at {:?}, again at {:?})",
					name, first_definition, self.span
				)
			}

			NameResolutionErrorKind::PrivateSymbol { path } => {
				write!(f, "symbol `{}` is private", path)
			}

			NameResolutionErrorKind::UnresolvedUseTarget { path } => {
				write!(f, "`@use` target `{}` does not exist", path)
			}

			NameResolutionErrorKind::AmbiguousName { name, .. } => {
				write!(f, "ambiguous name `{}`", name)
			}
		};
	}
}

impl std::error::Error for NameResolutionError {}

impl CompileDiagnostic for NameResolutionError
{
	fn build(&self) -> DiagnosticBuilder
	{
		let mut diag = match &self.kind {
			NameResolutionErrorKind::UnresolvedPath { path } => {
				DiagnosticBuilder::error(format!("unresolved path `{}`", path))
					.code(ErrorCode::NameResolutionUnresolvedPath)
					.primary(self.span, None)
			}

			NameResolutionErrorKind::ShadowedVariable { name, first_definition } => {
				DiagnosticBuilder::error(format!("variable `{name}` shadows an existing binding"))
					.code(ErrorCode::NameResolutionShadowedVariable)
					.primary(self.span, Some("second definition".into()))
					.secondary(*first_definition, Some("first defined here".into()))
			}

			NameResolutionErrorKind::PrivateSymbol { path } => {
				DiagnosticBuilder::error(format!("symbol `{}` is private", path))
					.code(ErrorCode::NameResolutionPrivateSymbol)
					.primary(self.span, None)
			}

			NameResolutionErrorKind::UnresolvedUseTarget { path } => {
				DiagnosticBuilder::error(format!("`@use` target `{}` does not exist", path))
					.code(ErrorCode::NameResolutionUnresolvedUseTarget)
					.primary(self.span, None)
			}

			NameResolutionErrorKind::AmbiguousName { name, candidates } => {
				let mut d = DiagnosticBuilder::error(format!("ambiguous name `{name}`"))
					.code(ErrorCode::NameResolutionAmbiguousName)
					.primary(self.span, None);

				for cand in candidates {
					d = d.note(format!("could refer to `{}`", cand));
				}

				d
			}
		};

		for ctx in &self.context {
			diag = diag.note(format!("while resolving names: {ctx}"));
		}
		return diag;
	}
}

impl From<NameResolutionError> for CompileError
{
	fn from(e: NameResolutionError) -> Self
	{
		return CompileError::NameResolution(e);
	}
}

struct Resolver<'a>
{
	global: &'a GlobalSymbolTable,
	modules: &'a [(Vec<String>, DesugaredAST, LocalSymbolTable)],
	symbols: &'a LocalSymbolTable,
	current_scope: ScopeId,
	trait_scope: Option<ScopeId>,
	use_imports: Vec<UseImport>,
	anon_scope_idx: usize,
	scope_offset: usize,
	self_sym: Option<SymbolId>,
	in_expr_context: bool,
}

#[allow(unused)]
#[derive(Debug, Clone)]
struct UseImport
{
	alias: Vec<String>,
	target_path: Vec<String>,
	visibility: Visibility,
	glob: bool,
}

#[derive(PartialEq, Eq)]
enum Bool
{
	True,
	False,
}

impl<'a> Resolver<'a>
{
	const fn new(
		global: &'a GlobalSymbolTable,
		modules: &'a [(Vec<String>, DesugaredAST, LocalSymbolTable)],
		symbols: &'a LocalSymbolTable,
		scope_offset: usize,
	) -> Self
	{
		return Self {
			global,
			modules,
			symbols,
			trait_scope: None,
			current_scope: ScopeId(symbols.root.0 + scope_offset),
			use_imports: Vec::new(),
			anon_scope_idx: 0,
			scope_offset,
			self_sym: None,
			in_expr_context: false,
		};
	}

	fn find_in_scope(&self, scope_id: ScopeId, name: &str) -> Option<SymbolId>
	{
		return self
			.global
			.scope(scope_id)
			.symbols
			.iter()
			.find(|&&id| return self.global.symbol(id).name == name)
			.copied();
	}

	fn find_in_scope_chain(&self, start: ScopeId, name: &str) -> Option<(SymbolId, ScopeId)>
	{
		let mut scope_id: ScopeId = start;
		loop {
			let scope: &Scope = self.global.scope(scope_id);

			let blocked = self.in_expr_context && self.trait_scope == Some(scope_id);

			if !blocked {
				for &sym_id in &scope.symbols {
					if self.global.symbol(sym_id).name == name {
						return Some((sym_id, scope_id));
					}
				}
			}

			match scope.parent {
				Some(parent) => scope_id = parent,
				None => return None,
			}
		}
	}

	fn find_in_scope_chain_expr(&self, start: ScopeId, name: &str) -> Option<(SymbolId, ScopeId)>
	{
		let mut scope_id: ScopeId = start;
		loop {
			let scope: &Scope = self.global.scope(scope_id);

			let blocked = self.trait_scope == Some(scope_id);

			if !blocked {
				for &sym_id in &scope.symbols {
					if self.global.symbol(sym_id).name == name {
						return Some((sym_id, scope_id));
					}
				}
			}

			match scope.parent {
				Some(parent) => scope_id = parent,
				None => return None,
			}
		}
	}

	fn find_introduced_scope(&self, sym_id: SymbolId) -> Option<ScopeId>
	{
		return self.global.symbol(sym_id).introduced_scope;
	}

	fn find_self_member_kind(&self, name: &str) -> Option<&'static str>
	{
		let self_sym = self.self_sym?;

		if let Some(scope_id) = self.global.symbol(self_sym).introduced_scope {
			let scope = self.global.scope(scope_id);
			for &child_id in &scope.symbols {
				let child = self.global.symbol(child_id);
				if child.name == name {
					match child.kind {
						SymbolKind::AssocType | SymbolKind::TypeAlias => {
							return Some("associated type");
						}
						SymbolKind::Function { .. } => {
							return Some("method");
						}
						_ => {}
					}
				}
			}
		}

		if let Some(trait_scope) = self.trait_scope {
			let scope = self.global.scope(trait_scope);
			for &child_id in &scope.symbols {
				let child = self.global.symbol(child_id);
				if child.name == name && matches!(child.kind, SymbolKind::Function { .. }) {
					return Some("method");
				}
			}
		}

		return None;
	}

	fn is_descendant_of(&self, mut scope: ScopeId, ancestor: ScopeId) -> bool
	{
		loop {
			if scope == ancestor {
				return true;
			}
			match self.global.scope(scope).parent {
				Some(parent) => scope = parent,
				None => return false,
			}
		}
	}

	fn next_anon_scope(&mut self) -> Option<ScopeId>
	{
		let idx: usize = self.anon_scope_idx;
		self.anon_scope_idx += 1;
		return self
			.symbols
			.anon_scopes
			.get(idx)
			.map(|&local| return ScopeId(local.0 + self.scope_offset)); // ← convert to global
	}

	fn resolve_path(&self, path: &Path, span: Span) -> Result<ResolvedPathResult, NameResolutionError>
	{
		let segments: &Vec<parser::PathSegment> = &path.segments;
		if segments.is_empty() {
			return Err(NameResolutionError {
				span,
				kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
				context: Vec::new(),
			});
		}
		if !path.global
			&& segments.len() == 1
			&& segments[0].name == "Self"
			&& segments[0].generics.is_empty()
			&& let Some(sym_id) = self.self_sym
		{
			return Ok(ResolvedPathResult::Full(sym_id));
		}
		if !path.global
			&& segments.len() >= 2
			&& segments[0].name == "Self"
			&& segments[0].generics.is_empty()
			&& let Some(self_sym) = self.self_sym
		{
			return Ok(ResolvedPathResult::Assoc {
				base: self_sym,
				member: segments[1].name.clone(),
			});
		}

		let first_name: &String = &segments[0].name;

		if path.global {
			let mut current_sym_id: SymbolId = self
				.find_sym_in_global_scope(self.global.root, first_name)
				.or_else(|| {
					if let Some(&root_mod_scope) = self.global.module_roots.get(&[] as &[String])
						&& let Some(sym_id) = self.find_sym_in_global_scope(root_mod_scope, first_name)
					{
						return Some(sym_id);
					}
					for &root in self.global.module_roots.values() {
						if let Some(sym_id) = self.find_sym_in_global_scope(root, first_name) {
							return Some(sym_id);
						}
					}
					return None;
				})
				.ok_or_else(|| {
					return NameResolutionError {
						span,
						kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
						context: Vec::new(),
					};
				})?;

			for (i, seg) in segments[1..].iter().enumerate() {
				let name: &String = &seg.name;
				let _prefix: Vec<String> = segments[..=i + 1].iter().map(|s| return s.name.clone()).collect();

				let search_scope: ScopeId = self
					.scope_for_path_prefix(&segments[..=i].iter().map(|s| return s.name.clone()).collect::<Vec<_>>())
					.ok_or_else(|| {
						return NameResolutionError {
							span,
							kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
							context: Vec::new(),
						};
					})?;

				let maybe_sym: Option<SymbolId> = self.find_sym_in_global_scope(search_scope, name);

				let Some(sym_id) = maybe_sym else {
					let current_sym = self.global.symbol(current_sym_id);
					if matches!(
						current_sym.kind,
						SymbolKind::Struct
							| SymbolKind::Union | SymbolKind::Enum
							| SymbolKind::Variant | SymbolKind::TypeAlias
							| SymbolKind::AssocType
					) {
						return Ok(ResolvedPathResult::Assoc {
							base: current_sym_id,
							member: name.clone(),
						});
					}
					return Err(NameResolutionError {
						span,
						kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
						context: Vec::new(),
					});
				};

				let sym: &Symbol = self.global.symbol(sym_id);
				if sym.visibility == Visibility::Private && !self.is_descendant_of(self.current_scope, search_scope) {
					return Err(NameResolutionError {
						span,
						kind: NameResolutionErrorKind::PrivateSymbol { path: path.clone() },
						context: Vec::new(),
					});
				}
				current_sym_id = sym_id;
			}
			return Ok(ResolvedPathResult::Full(current_sym_id));
		}

		if let Some((sym_id, full_consumed, current_module_path)) =
			self.resolve_first_via_use(first_name, span, path)?
		{
			if segments.len() == 1
				&& let Some((local_sym, _)) = self.find_in_scope_chain(self.current_scope, first_name)
				&& local_sym != sym_id
			{
				let use_path = self
					.use_imports
					.iter()
					.find(|imp| return imp.alias.last().map(String::as_str) == Some(first_name))
					.map_or_else(
						|| return path.clone(),
						|imp| return Path::simple(imp.target_path.clone(), span),
					);

				return Err(NameResolutionError::ambiguous_name(
					span,
					first_name.clone(),
					vec![Path::simple(vec![first_name.to_owned()], span), use_path],
				));
			}

			if full_consumed || segments.len() == 1 {
				return Ok(ResolvedPathResult::Full(sym_id));
			}

			let search_scope: ScopeId = current_module_path
				.as_deref()
				.and_then(|p| return self.global.module_roots.get(p).copied())
				.or_else(|| return self.find_introduced_scope(sym_id))
				.ok_or_else(|| {
					return NameResolutionError {
						span,
						kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
						context: Vec::new(),
					};
				})?;

			let mut cur_sym: SymbolId = sym_id;
			let mut cur_scope: ScopeId = search_scope;
			let mut abs_path: Vec<String> = current_module_path.unwrap_or_else(|| {
				return self
					.global
					.module_roots
					.iter()
					.find_map(|(p, &root)| {
						return if self.find_introduced_scope(sym_id) == Some(root) {
							Some(p.clone())
						} else {
							None
						};
					})
					.unwrap_or_else(|| vec![self.global.symbol(sym_id).name.clone()]);
			});

			for seg in &segments[1..] {
				let name: &String = &seg.name;

				let maybe_sym: Option<SymbolId> = self.find_sym_in_global_scope(cur_scope, name);

				let Some(next_sym_id) = maybe_sym else {
					let current_sym = self.global.symbol(cur_sym);
					if matches!(
						current_sym.kind,
						SymbolKind::Struct
							| SymbolKind::Union | SymbolKind::Enum
							| SymbolKind::Variant | SymbolKind::TypeAlias
							| SymbolKind::AssocType
					) {
						return Ok(ResolvedPathResult::Assoc {
							base: cur_sym,
							member: name.clone(),
						});
					}
					return Err(NameResolutionError {
						span,
						kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
						context: Vec::new(),
					});
				};

				let sym: &Symbol = self.global.symbol(next_sym_id);
				if sym.visibility == Visibility::Private && !self.is_descendant_of(self.current_scope, cur_scope) {
					return Err(NameResolutionError {
						span,
						kind: NameResolutionErrorKind::PrivateSymbol { path: path.clone() },
						context: Vec::new(),
					});
				}

				abs_path.push(name.clone());
				cur_scope = if let Some(&mod_root) = self.global.module_roots.get(abs_path.as_slice()) {
					mod_root
				} else if let Some(sc) = self.find_introduced_scope(next_sym_id)
					&& !self.global.scope(sc).symbols.is_empty()
				{
					sc
				} else {
					cur_scope
				};

				cur_sym = next_sym_id;
			}

			return Ok(ResolvedPathResult::Full(cur_sym));
		}

		let (mut current_sym_id, _) = self
			.find_in_scope_chain(self.current_scope, first_name)
			.ok_or_else(|| {
				return NameResolutionError {
					span,
					kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
					context: Vec::new(),
				};
			})?;

		let mut current_abs_path: Option<Vec<String>> = {
			let sym: &Symbol = self.global.symbol(current_sym_id);
			self.global
				.module_roots
				.iter()
				.find_map(|(p, &root)| {
					return if p.last().map(String::as_str) == Some(sym.name.as_str())
						&& sym.introduced_scope == Some(root)
					{
						Some(p.clone())
					} else {
						None
					};
				})
				.or_else(|| {
					let candidate: Vec<String> = vec![sym.name.clone()];
					return if self.scope_for_path_prefix(&candidate).is_some() {
						Some(candidate)
					} else {
						None
					};
				})
		};

		for seg in &segments[1..] {
			let name: &String = &seg.name;

			let next_abs_path: Option<Vec<String>> = current_abs_path.as_ref().map(|p| {
				let mut np: Vec<String> = p.clone();
				np.push(name.clone());
				return np;
			});

			let search_scope_opt: Option<ScopeId> = current_abs_path.as_ref().map_or_else(
				|| return self.find_introduced_scope(current_sym_id),
				|abs| return self.scope_for_path_prefix(abs),
			);

			let Some(search_scope) = search_scope_opt else {
				let current_sym = self.global.symbol(current_sym_id);
				if matches!(
					current_sym.kind,
					SymbolKind::Struct
						| SymbolKind::Union
						| SymbolKind::Enum | SymbolKind::Variant
						| SymbolKind::TypeAlias
						| SymbolKind::AssocType
				) {
					return Ok(ResolvedPathResult::Assoc {
						base: current_sym_id,
						member: name.clone(),
					});
				}
				return Err(NameResolutionError {
					span,
					kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
					context: Vec::new(),
				});
			};

			let maybe_sym: Option<SymbolId> = self.find_sym_in_global_scope(search_scope, name);

			let Some(sym_id) = maybe_sym else {
				let current_sym: &Symbol = self.global.symbol(current_sym_id);
				if matches!(
					current_sym.kind,
					SymbolKind::Struct
						| SymbolKind::Union
						| SymbolKind::Enum | SymbolKind::Variant
						| SymbolKind::TypeAlias
						| SymbolKind::AssocType
				) {
					return Ok(ResolvedPathResult::Assoc {
						base: current_sym_id,
						member: name.clone(),
					});
				}
				return Err(NameResolutionError {
					span,
					kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
					context: Vec::new(),
				});
			};

			let sym: &Symbol = self.global.symbol(sym_id);
			if sym.visibility == Visibility::Private && !self.is_descendant_of(self.current_scope, search_scope) {
				return Err(NameResolutionError {
					span,
					kind: NameResolutionErrorKind::PrivateSymbol { path: path.clone() },
					context: Vec::new(),
				});
			}

			current_abs_path = next_abs_path;
			current_sym_id = sym_id;
		}

		return Ok(ResolvedPathResult::Full(current_sym_id));
	}

	fn resolve_path_or_primitive(&self, path: &Path, span: Span) -> Result<ResolvedPath, NameResolutionError>
	{
		if !path.global && path.segments.len() == 1 {
			match self.resolve_path_full(path, span) {
				Ok(rp) => return Ok(rp),
				Err(e) => {
					let name = &path.segments[0].name;
					if matches!(
						name.as_str(),
						"bool"
							| "char" | "i8" | "i16"
							| "i32" | "i64" | "i128"
							| "u8" | "u16" | "u32"
							| "u64" | "u128" | "f32"
							| "f64" | "isize" | "usize"
							| "str" | "()" | "!"
					) {
						return Ok(ResolvedPath {
							original: path.clone(),
							kind: ResolvedPathKind::Primitive(
								Ty::from_primitive_name(name)
									.expect("the function before should have filtered this out"),
							),
						});
					}
					return Err(e);
				}
			}
		}
		return self.resolve_path_full(path, span);
	}

	fn resolve_path_full(&self, path: &Path, span: Span) -> Result<ResolvedPath, NameResolutionError>
	{
		return Ok(match self.resolve_path(path, span)? {
			ResolvedPathResult::Full(symbol) => ResolvedPath {
				original: path.clone(),
				kind: ResolvedPathKind::Resolved(symbol),
			},
			ResolvedPathResult::Assoc { base, member } => ResolvedPath {
				original: path.clone(),
				kind: ResolvedPathKind::AssocItem { base, member },
			},
		});
	}

	fn collect_use_directives(&mut self, items: &[TopLevelDecl]) -> Result<(), NameResolutionError>
	{
		use crate::parser::Directive;

		for decl in items {
			let TopLevelDecl::Directive(node) = decl else { continue };

			match &node.directive {
				Directive::Use {
					use_path, visibility, ..
				} => {
					let segments: Vec<String> = use_path.segments.iter().map(|s| return s.name.clone()).collect();

					let should_validate: bool = use_path.global || segments.len() > 1;

					if should_validate {
						self.resolve_absolute_path(&segments, use_path.span(), use_path)?;
					}
					if use_path.glob {
						self.use_imports.push(UseImport {
							alias: Vec::new(),
							target_path: segments,
							glob: true,
							visibility: *visibility,
						});
					} else {
						let alias: Vec<String> = segments.last().cloned().map(|n| vec![n]).unwrap_or_default();
						self.use_imports.push(UseImport {
							alias,
							target_path: segments,
							glob: false,
							visibility: *visibility,
						});
					}
				}
				Directive::Import { import, visibility, .. } => {
					let logical_path: Vec<String> = import
						.trim_end_matches(".leaf")
						.trim_end_matches(".rs")
						.split(['/', '\\', ':'])
						.filter(|s| return !s.is_empty())
						.map(str::to_owned)
						.collect();
					if !logical_path.is_empty() {
						self.use_imports.push(UseImport {
							alias: Vec::new(),
							target_path: logical_path,
							glob: true,
							visibility: *visibility,
						});
					}
				}
				_ => {}
			}
		}
		return Ok(());
	}

	#[allow(clippy::type_complexity)]
	fn resolve_first_via_use(
		&self,
		name: &str,
		span: Span,
		original_path: &Path,
	) -> Result<Option<(SymbolId, bool, Option<Vec<String>>)>, NameResolutionError>
	{
		let path_segs: Vec<&str> = original_path.segments.iter().map(|s| return s.name.as_str()).collect();

		for import in &self.use_imports {
			if import.glob {
				if let Some(sym_id) = self.resolve_use_glob(&import.target_path, name) {
					return Ok(Some((sym_id, true, None)));
				}
				continue;
			}

			let alias_name: &str = match import.alias.last() {
				Some(a) => a.as_str(),
				None => continue,
			};

			if alias_name == name {
				let sym_id: SymbolId = self.resolve_absolute_path(&import.target_path, span, original_path)?;
				return Ok(Some((sym_id, false, Some(import.target_path.clone()))));
			}

			if path_segs.len() > 1 && import.target_path.len() >= path_segs.len() {
				let suffix: &[String] = &import.target_path[import.target_path.len() - path_segs.len()..];
				if suffix.iter().zip(&path_segs).all(|(a, b)| return a.as_str() == *b) {
					let sym_id: SymbolId = self.resolve_absolute_path(&import.target_path, span, original_path)?;
					return Ok(Some((sym_id, true, None)));
				}
			}
		}
		return Ok(None);
	}

	fn resolve_use_glob(&self, target_path: &[String], name: &str) -> Option<SymbolId>
	{
		if let Some(&root) = self.global.module_roots.get(target_path) {
			return self.resolve_in_module_surface(target_path, root, name, 8);
		}

		if target_path.len() == 1 {
			let type_name: &String = &target_path[0];
			for &mod_root in self.global.module_roots.values() {
				if let Some(sym_id) = self.find_sym_in_global_scope(mod_root, type_name) {
					let sym: &Symbol = self.global.symbol(sym_id);
					if matches!(
						sym.kind,
						SymbolKind::Variant
							| SymbolKind::Enum | SymbolKind::Struct
							| SymbolKind::TypeAlias
							| SymbolKind::AssocType
					) && let Some(introduced) = sym.introduced_scope
						&& let Some(found) = self
							.global
							.scope(introduced)
							.symbols
							.iter()
							.find(|&&id| {
								let s = self.global.symbol(id);
								return s.name == name
									&& matches!(s.visibility, Visibility::Public | Visibility::Export);
							})
							.copied()
					{
						return Some(found);
					}
				}
			}
		}

		for prefix_len in (1..=target_path.len()).rev() {
			let prefix: &[String] = &target_path[..prefix_len];
			let Some(&root_scope) = self.global.module_roots.get(prefix) else {
				continue;
			};

			let mut scope: ScopeId = root_scope;
			let mut ok: Bool = Bool::True;
			for seg in &target_path[prefix_len..] {
				let sym_id: SymbolId = self.find_sym_in_global_scope(scope, seg)?;
				if let Some(s) = self.global.symbol(sym_id).introduced_scope {
					scope = s;
				} else {
					ok = Bool::False;
					break;
				}
			}
			if ok == Bool::False {
				continue;
			}

			return self
				.global
				.scope(scope)
				.symbols
				.iter()
				.find(|&&id| {
					let sym = self.global.symbol(id);
					return sym.name == name && matches!(sym.visibility, Visibility::Public | Visibility::Export);
				})
				.copied();
		}

		return None;
	}

	fn resolve_in_module_surface(
		&self,
		module_path: &[String],
		root_scope: ScopeId,
		name: &str,
		depth: usize,
	) -> Option<SymbolId>
	{
		if depth == 0 {
			return None;
		}

		if let Some(sym_id) = self
			.global
			.scope(root_scope)
			.symbols
			.iter()
			.find(|&&id| {
				let sym = self.global.symbol(id);
				return sym.name == name && matches!(sym.visibility, Visibility::Public | Visibility::Export);
			})
			.copied()
		{
			return Some(sym_id);
		}

		let (_, ast, _) = self
			.modules
			.iter()
			.find(|(p, _, _)| return p.as_slice() == module_path)?;

		for decl in &ast.top_level_block.items {
			let parser::TopLevelDecl::Directive(node) = decl else {
				continue;
			};
			let parser::Directive::Use {
				use_path, visibility, ..
			} = &node.directive
			else {
				continue;
			};
			if !matches!(*visibility, Visibility::Public | Visibility::Export) {
				continue;
			}

			let target: Vec<String> = use_path.segments.iter().map(|s| return s.name.clone()).collect();

			if use_path.glob {
				if let Some(&inner_root) = self.global.module_roots.get(&target) {
					if let Some(sym_id) = self.resolve_in_module_surface(&target, inner_root, name, depth - 1) {
						return Some(sym_id);
					}
				} else {
					let mut scope: ScopeId = root_scope;
					let mut ok: Bool = Bool::True;
					for seg in &target {
						let sym_id: SymbolId = if let Some(id) = self.find_sym_in_global_scope(scope, seg) {
							id
						} else {
							let mut found = None;
							'outer: for prefix_len in (1..=target.len()).rev() {
								let prefix = &target[..prefix_len];
								let Some(&mod_root) = self.global.module_roots.get(prefix) else {
									continue;
								};
								found = Some(mod_root);
								break 'outer;
							}
							if let Some(s) = found {
								scope = s;
								break;
							}
							ok = Bool::False;
							break;
						};
						if let Some(s) = self.global.symbol(sym_id).introduced_scope {
							scope = s;
						} else {
							ok = Bool::False;
							break;
						}
					}

					if ok == Bool::True
						&& let Some(sym_id) = self
							.global
							.scope(scope)
							.symbols
							.iter()
							.find(|&&id| {
								let sym = self.global.symbol(id);
								return sym.name == name
									&& matches!(sym.visibility, Visibility::Public | Visibility::Export);
							})
							.copied()
					{
						return Some(sym_id);
					}
				}
			} else if target.last().map(String::as_str) == Some(name)
				&& let Some(sym_id) = self.resolve_named_reexport(&target)
			{
				return Some(sym_id);
			}
		}

		return None;
	}

	fn resolve_named_reexport(&self, target: &[String]) -> Option<SymbolId>
	{
		for prefix_len in (1..=target.len()).rev() {
			let prefix: &[String] = &target[..prefix_len];
			let Some(&root_scope) = self.global.module_roots.get(prefix) else {
				continue;
			};

			if prefix_len == target.len() {
				let name: &String = target.last()?;
				for &mod_root in self.global.module_roots.values() {
					if let Some(sym_id) = self.find_sym_in_global_scope(mod_root, name) {
						return Some(sym_id);
					}
				}
				return None;
			}

			let remaining: &[String] = &target[prefix_len..];
			let mut scope: ScopeId = root_scope;
			let mut last_sym: Option<SymbolId> = None;

			for seg in remaining {
				if let Some(sym_id) = self.find_sym_in_global_scope(scope, seg) {
					if let Some(sc) = self.global.symbol(sym_id).introduced_scope {
						scope = sc;
					}
					last_sym = Some(sym_id);
				} else {
					if let Some(sym_id) = self.resolve_in_module_surface(prefix, scope, seg, 4) {
						last_sym = Some(sym_id);
					}
					break;
				}
			}

			if let Some(sym_id) = last_sym {
				return Some(sym_id);
			}
		}

		let name: &String = target.last()?;
		for &root in self.global.module_roots.values() {
			if let Some(sym_id) = self.find_sym_in_global_scope(root, name)
				&& matches!(
					self.global.symbol(sym_id).visibility,
					Visibility::Public | Visibility::Export
				) {
				return Some(sym_id);
			}
		}
		return None;
	}

	fn resolve_absolute_path(
		&self,
		segments: &[String],
		span: Span,
		original_path: &Path,
	) -> Result<SymbolId, NameResolutionError>
	{
		for prefix_len in (1..=segments.len()).rev() {
			let prefix: &[String] = &segments[..prefix_len];
			if let Some(&root_scope) = self.global.module_roots.get(prefix) {
				if prefix_len == segments.len() {
					let mod_name: &String = segments.last().expect("");
					if let Some(&parent_root) = self.global.module_roots.get(&segments[..prefix_len - 1])
						&& let Some(sym_id) = self.find_sym_in_global_scope(parent_root, mod_name)
					{
						let sym = self.global.symbol(sym_id);
						if sym.visibility == Visibility::Private
							&& !self.is_descendant_of(self.current_scope, sym.scope)
						{
							return Err(NameResolutionError {
								span,
								kind: NameResolutionErrorKind::PrivateSymbol {
									path: original_path.clone(),
								},
								context: Vec::new(),
							});
						}
						return Ok(sym_id);
					}
					if let Some(sym_id) = self.global.symbols.iter().enumerate().find_map(|(i, sym)| {
						return if sym.name == *mod_name && sym.introduced_scope == Some(root_scope) {
							Some(SymbolId(i))
						} else {
							None
						};
					}) {
						let sym: &Symbol = self.global.symbol(sym_id);
						if sym.visibility == Visibility::Private
							&& !self.is_descendant_of(self.current_scope, sym.scope)
						{
							return Err(NameResolutionError {
								span,
								kind: NameResolutionErrorKind::PrivateSymbol {
									path: original_path.clone(),
								},
								context: Vec::new(),
							});
						}
						return Ok(sym_id);
					}
				} else {
					let remaining: &[String] = &segments[prefix_len..];
					let mut scope: ScopeId = root_scope;
					let mut last_sym: Option<SymbolId> = None;
					for (i, seg) in remaining.iter().enumerate() {
						let is_last: bool = i == remaining.len() - 1;

						let sym_id: SymbolId = if is_last {
							let module_path = &segments[..prefix_len];
							self.resolve_in_module_surface(module_path, scope, seg, 8)
								.ok_or_else(|| {
									return NameResolutionError {
										span,
										kind: NameResolutionErrorKind::UnresolvedUseTarget {
											path: original_path.clone(),
										},
										context: Vec::new(),
									};
								})?
						} else {
							self.find_sym_in_global_scope(scope, seg).ok_or_else(|| {
								return NameResolutionError {
									span,
									kind: NameResolutionErrorKind::UnresolvedUseTarget {
										path: original_path.clone(),
									},
									context: Vec::new(),
								};
							})?
						};

						let sym: &Symbol = self.global.symbol(sym_id);
						if sym.visibility == Visibility::Private
							&& !self.is_descendant_of(self.current_scope, sym.scope)
						{
							return Err(NameResolutionError {
								span,
								kind: NameResolutionErrorKind::PrivateSymbol {
									path: original_path.clone(),
								},
								context: Vec::new(),
							});
						}
						if !is_last {
							scope = sym.introduced_scope.ok_or_else(|| {
								return NameResolutionError {
									span,
									kind: NameResolutionErrorKind::UnresolvedUseTarget {
										path: original_path.clone(),
									},
									context: Vec::new(),
								};
							})?;
						}
						last_sym = Some(sym_id);
					}
					if let Some(id) = last_sym {
						return Ok(id);
					}
				}
			}
		}

		if let Some(parent_scope) = self.scope_for_path_prefix(&segments[..segments.len() - 1]) {
			let last = segments.last().expect("");
			if let Some(sym_id) = self.find_sym_in_global_scope(parent_scope, last) {
				let sym = self.global.symbol(sym_id);
				if sym.visibility == Visibility::Private && !self.is_descendant_of(self.current_scope, sym.scope) {
					return Err(NameResolutionError {
						span,
						kind: NameResolutionErrorKind::PrivateSymbol {
							path: original_path.clone(),
						},
						context: Vec::new(),
					});
				}
				return Ok(sym_id);
			}
		}

		return Err(NameResolutionError {
			span,
			kind: NameResolutionErrorKind::UnresolvedUseTarget {
				path: original_path.clone(),
			},
			context: Vec::new(),
		});
	}

	fn find_sym_in_global_scope(&self, scope_id: ScopeId, name: &str) -> Option<SymbolId>
	{
		return self
			.global
			.scope(scope_id)
			.symbols
			.iter()
			.find(|&&id| return self.global.symbol(id).name == name)
			.copied();
	}

	fn scope_for_path_prefix(&self, prefix: &[String]) -> Option<ScopeId>
	{
		if let Some(&root) = self.global.module_roots.get(prefix) {
			return Some(root);
		}
		if prefix.is_empty() {
			return Some(self.global.root);
		}
		let parent_scope: ScopeId = self.scope_for_path_prefix(&prefix[..prefix.len() - 1])?;
		let name: &String = &prefix[prefix.len() - 1];

		let sym_id: SymbolId = self.find_sym_in_global_scope(parent_scope, name).or_else(|| {
			if parent_scope == self.global.root {
				for &mod_root in self.global.module_roots.values() {
					if let Some(id) = self.find_sym_in_global_scope(mod_root, name) {
						return Some(id);
					}
				}
			}
			return None;
		})?;

		let introduced: ScopeId = self.global.symbol(sym_id).introduced_scope?;
		if !self.global.scope(introduced).symbols.is_empty() {
			return Some(introduced);
		}
		return None;
	}

	fn resolve_top_level_block(
		&mut self,
		block: &parser::TopLevelBlock,
	) -> Result<ResolvedTopLevelBlock, NameResolutionError>
	{
		self.collect_use_directives(&block.items)?;

		let mut items: Vec<ResolvedTopLevelDecl> = Vec::new();
		for decl in &block.items {
			items.push(self.resolve_top_level_decl(decl)?);
		}
		return Ok(ResolvedTopLevelBlock {
			items,
			span: block.span,
		});
	}

	fn resolve_top_level_decl(&mut self, decl: &TopLevelDecl) -> Result<ResolvedTopLevelDecl, NameResolutionError>
	{
		return Ok(match decl {
			TopLevelDecl::Function(f) => ResolvedTopLevelDecl::Function(self.resolve_function_decl(f)?),
			TopLevelDecl::VariableDecl(v) => ResolvedTopLevelDecl::VariableDecl(self.resolve_variable_decl(v)?),
			TopLevelDecl::Struct(s) => ResolvedTopLevelDecl::Struct(self.resolve_struct_decl(s)?),
			TopLevelDecl::Union(u) => ResolvedTopLevelDecl::Union(self.resolve_union_decl(u)?),
			TopLevelDecl::Enum(e) => ResolvedTopLevelDecl::Enum(self.resolve_enum_decl(e)?),
			TopLevelDecl::Variant(v) => ResolvedTopLevelDecl::Variant(self.resolve_variant_decl(v)?),
			TopLevelDecl::TypeAlias(t) => ResolvedTopLevelDecl::TypeAlias(self.resolve_type_alias_decl(t)?),
			TopLevelDecl::Trait(t) => ResolvedTopLevelDecl::Trait(self.resolve_trait_decl(t)?),
			TopLevelDecl::Module(m) => ResolvedTopLevelDecl::Module(self.resolve_module_decl(m)?),
			TopLevelDecl::Impl(i) => ResolvedTopLevelDecl::Impl(self.resolve_impl_decl(i)?),
			TopLevelDecl::Directive(d) => ResolvedTopLevelDecl::Directive(self.resolve_directive_node(d)?),
		});
	}

	fn resolve_type(&mut self, ty: &parser::Type) -> Result<ResolvedType, NameResolutionError>
	{
		let core = self.resolve_type_core(ty.core.as_ref(), ty.span)?;
		return Ok(ResolvedType {
			core: Box::new(core),
			span: ty.span,
		});
	}

	fn resolve_type_core(&mut self, core: &TypeCore, span: Span) -> Result<ResolvedTypeCore, NameResolutionError>
	{
		return Ok(match core {
			TypeCore::Base { path, generics } => {
				if path.len() == 1 && path.segments[0].name == "Self" {
					return Ok(ResolvedTypeCore::Primitive {
						name: "Self".to_string(),
						generics: Vec::new(),
					});
				}
				if !path.global && path.segments.len() == 1 {
					let name = &path.segments[0].name;
					if name != "Self"
						&& let Some(kind_str) = self.find_self_member_kind(name)
					{
						return Err(NameResolutionError {
							span,
							kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
							context: vec![format!(
								"`{name}` is a {kind_str} of `Self`; write `Self::{name}` to reference it"
							)],
						});
					}
				}
				if !path.global && path.segments.len() >= 2 && path.segments[0].name == "Self" {
					let resolved_generics = generics
						.iter()
						.map(|g| return self.resolve_type(g))
						.collect::<Result<_, _>>()?;
					let member = path.segments[1].name.clone();
					if let Some(self_sym) = self.self_sym {
						return Ok(ResolvedTypeCore::Base {
							path: ResolvedPath {
								original: path.clone(),
								kind: ResolvedPathKind::AssocItem { base: self_sym, member },
							},
							generics: resolved_generics,
						});
					}
				}
				let resolved_generics: Vec<ResolvedType> = generics
					.iter()
					.map(|g| return self.resolve_type(g))
					.collect::<Result<_, _>>()?;

				if path.segments.len() > 1 || path.global {
					let rp = self.resolve_path_full(path, span)?;
					ResolvedTypeCore::Base {
						path: rp,
						generics: resolved_generics,
					}
				} else {
					match self.resolve_path_full(path, span) {
						Ok(rp) => ResolvedTypeCore::Base {
							path: rp,
							generics: resolved_generics,
						},
						Err(_) => ResolvedTypeCore::Primitive {
							name: path.segments[0].name.clone(),
							generics: resolved_generics,
						},
					}
				}
			}
			TypeCore::Reference { mutable, inner } => ResolvedTypeCore::Reference {
				mutable: *mutable,
				inner: Box::new(self.resolve_type_core(inner, span)?),
			},
			TypeCore::Mutable { inner } => ResolvedTypeCore::Mutable {
				inner: Box::new(self.resolve_type_core(inner, span)?),
			},
			TypeCore::Pointer { mutable, inner } => ResolvedTypeCore::Pointer {
				mutable: *mutable,
				inner: Box::new(self.resolve_type_core(inner, span)?),
			},
			TypeCore::Array { inner, size } => {
				let resolved_inner = self.resolve_type_core(inner, span)?;
				let resolved_size = size.as_ref().map(|e| return self.resolve_expr(e)).transpose()?;
				ResolvedTypeCore::Array {
					inner: Box::new(resolved_inner),
					size: resolved_size.map(Box::new),
				}
			}
			TypeCore::Tuple(types) => ResolvedTypeCore::Tuple(
				types
					.iter()
					.map(|t| return self.resolve_type(t))
					.collect::<Result<_, _>>()?,
			),
			TypeCore::ImplTrait { bounds } => ResolvedTypeCore::ImplTrait {
				bounds: bounds
					.iter()
					.map(|b| return self.resolve_where_bound(b))
					.collect::<Result<_, _>>()?,
			},
		});
	}

	fn resolve_where_constraint(
		&mut self,
		constraint: &WhereConstraint,
	) -> Result<ResolvedWhereConstraint, NameResolutionError>
	{
		let bounds = constraint
			.bounds
			.iter()
			.map(|b| return self.resolve_where_bound(b))
			.collect::<Result<_, _>>()?;
		let type_args = constraint
			.type_args
			.iter()
			.map(|t| return self.resolve_type(t))
			.collect::<Result<_, _>>()?;
		return Ok(ResolvedWhereConstraint {
			ty: constraint
				.ty
				.segments
				.iter()
				.map(|s| return s.name.as_str())
				.collect::<Vec<_>>()
				.join("::"),
			bounds,
			type_args,
			span: constraint.span,
		});
	}

	fn resolve_where_bound(&mut self, bound: &WhereBound) -> Result<ResolvedWhereBound, NameResolutionError>
	{
		return Ok(match bound {
			WhereBound::Path { path, args } => {
				let rp = self.resolve_path_full(path, path.span())?;
				let resolved_args = args
					.iter()
					.map(|arg| match arg {
						GenericArg::Type(ty) => return self.resolve_type(ty).map(ResolvedGenericArg::Type),
						GenericArg::Binding { name, ty, span } => {
							return self.resolve_type(ty).map(|rty| {
								return ResolvedGenericArg::Binding {
									name: name.clone(),
									ty: rty,
									span: *span,
								};
							});
						}
					})
					.collect::<Result<_, _>>()?;
				ResolvedWhereBound::Path {
					path: rp,
					args: resolved_args,
				}
			}
			WhereBound::Func(fb) => {
				use parser::FuncBound;
				match fb {
					FuncBound::Fn { args, ret } => {
						let rargs = args
							.iter()
							.map(|t| return self.resolve_type(t))
							.collect::<Result<_, _>>()?;
						let rret = ret.as_ref().map(|t| return self.resolve_type(t)).transpose()?;
						ResolvedWhereBound::Func(ResolvedFuncBound::Fn { args: rargs, ret: rret })
					}
				}
			}
		});
	}

	fn resolve_pattern(&mut self, pattern: &parser::Pattern) -> Result<ResolvedPattern, NameResolutionError>
	{
		return Ok(match pattern {
			parser::Pattern::Wildcard { ty, span } => ResolvedPattern::Wildcard {
				ty: ty.as_ref().map(|t| return self.resolve_type(t)).transpose()?,
				span: *span,
			},

			parser::Pattern::Literal { value, span } => ResolvedPattern::Literal {
				value: value.clone(),
				span: *span,
			},

			parser::Pattern::TypedIdentifier {
				path,
				ty,
				mutable,
				span,
				..
			} => {
				let name: String = path.segments[0].name.clone();
				let symbol: SymbolId = self
					.find_in_scope_chain(self.current_scope, &name)
					.map(|(id, _)| return id)
					.ok_or_else(|| {
						return NameResolutionError {
							span: *span,
							kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
							context: Vec::new(),
						};
					})?;
				let resolved_ty = self.resolve_type(ty)?;
				ResolvedPattern::TypedIdentifier {
					symbol,
					name,
					ty: resolved_ty,
					mutable: *mutable,
					span: *span,
				}
			}

			parser::Pattern::Variant { path, args, span } => {
				let rp: ResolvedPath = self.resolve_path_full(path, *span)?;
				let rargs: Vec<ResolvedPattern> = args
					.iter()
					.map(|p| return self.resolve_pattern(p))
					.collect::<Result<_, _>>()?;
				ResolvedPattern::Variant {
					path: rp,
					args: rargs,
					span: *span,
				}
			}

			parser::Pattern::Tuple { patterns, span } => {
				let rp: Vec<ResolvedPattern> = patterns
					.iter()
					.map(|p| return self.resolve_pattern(p))
					.collect::<Result<_, _>>()?;
				ResolvedPattern::Tuple {
					patterns: rp,
					span: *span,
				}
			}

			parser::Pattern::Struct {
				path,
				fields,
				has_rest,
				span,
			} => {
				let rp: ResolvedPath = self.resolve_path_full(path, *span)?;
				let rfields: Vec<(String, ResolvedPattern)> = fields
					.iter()
					.map(|(name, pat)| return self.resolve_pattern(pat).map(|rp| return (name.clone(), rp)))
					.collect::<Result<_, _>>()?;
				ResolvedPattern::Struct {
					path: rp,
					fields: rfields,
					has_rest: *has_rest,
					span: *span,
				}
			}

			parser::Pattern::Range(re) => ResolvedPattern::Range(self.resolve_range_expr(re)?),

			parser::Pattern::Or { patterns, span } => {
				let rp: Vec<ResolvedPattern> = patterns
					.iter()
					.map(|p| return self.resolve_pattern(p))
					.collect::<Result<_, _>>()?;
				ResolvedPattern::Or {
					patterns: rp,
					span: *span,
				}
			}
		});
	}

	fn resolve_range_expr(&mut self, re: &RangeExpr) -> Result<ResolvedRangeExpr, NameResolutionError>
	{
		return Ok(ResolvedRangeExpr {
			start: re
				.start
				.as_ref()
				.map(|e| return self.resolve_expr(e))
				.transpose()?
				.map(Box::new),
			end: re
				.end
				.as_ref()
				.map(|e| return self.resolve_expr(e))
				.transpose()?
				.map(Box::new),
			inclusive: re.inclusive,
			span: re.span,
		});
	}

	fn resolve_expr(&mut self, expr: &parser::Expr) -> Result<ResolvedExpr, NameResolutionError>
	{
		let prev_in_expr = self.in_expr_context;
		self.in_expr_context = true;
		let result = self.resolve_expr_inner(expr);
		self.in_expr_context = prev_in_expr;
		return result;
	}

	fn resolve_expr_inner(&mut self, expr: &parser::Expr) -> Result<ResolvedExpr, NameResolutionError>
	{
		use parser::{ArrayLiteral, Expr};

		return Ok(match expr {
			Expr::Identifier { path, span } => {
				if !path.global && path.segments.len() == 1 && path.segments[0].name.starts_with('#') {
					let name: &String = &path.segments[0].name;
					if let Some(intrinsic) = Intrinsic::from_name(name) {
						return Ok(ResolvedExpr::InternalCall { intrinsic, span: *span });
					}
				}

				if let Ok(rp) = self.resolve_path_full(path, *span) {
					ResolvedExpr::Identifier { path: rp, span: *span }
				} else {
					if matches!(
						path,
						Path {
							segments,
							glob: false,
							global: false,
							..
						} if matches!(
							&segments[0],
							PathSegment { name, generics, .. }
								if name == "Self" && generics.is_empty()
						)
					) {
						return Ok(ResolvedExpr::AssocSelf {
							member: path.segments[1].clone(),
							span: path.span(),
						});
					}

					if !path.global && path.segments.len() == 2 && path.segments[0].generics.is_empty() {
						let base_name = &path.segments[0].name;
						let is_generic_param =
							self.find_in_scope_chain(self.current_scope, base_name)
								.is_some_and(|(sym_id, _)| {
									return matches!(self.global.symbol(sym_id).kind, SymbolKind::GenericParam);
								});

						if is_generic_param
							&& let Ok(base_path) = self.resolve_path_full(
								&Path::simple(vec![base_name.clone()], path.segments[0].span),
								path.segments[0].span,
							) {
							return Ok(ResolvedExpr::AssocPath {
								base: base_path,
								member: path.segments[1].clone(),
								span: *span,
							});
						}
					}

					if !path.global && path.segments.len() == 1 {
						let name = &path.segments[0].name;
						if let Some(kind_str) = self.find_self_member_kind(name) {
							return Err(NameResolutionError {
								span: *span,
								kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
								context: vec![format!(
									"`{name}` is a {kind_str} of `Self`; write `Self::{name}` to reference it"
								)],
							});
						}
					}

					ResolvedExpr::UnresolvedIdentifier {
						path: path.clone(),
						span: *span,
					}
				}
			}

			Expr::Literal { value, span } => ResolvedExpr::Literal {
				value: value.clone(),
				span: *span,
			},

			Expr::Default { heap_call, span } => ResolvedExpr::Default {
				heap_call: *heap_call,
				span: *span,
			},

			Expr::Unary { op, expr, span } => ResolvedExpr::Unary {
				op: op.clone(),
				expr: Box::new(self.resolve_expr(expr)?),
				span: *span,
			},

			Expr::Binary { op, lhs, rhs, span } => ResolvedExpr::Binary {
				op: op.clone(),
				lhs: Box::new(self.resolve_expr(lhs)?),
				rhs: Box::new(self.resolve_expr(rhs)?),
				span: *span,
			},

			Expr::Cast { ty, expr, span } => ResolvedExpr::Cast {
				ty: self.resolve_type(ty)?,
				expr: Box::new(self.resolve_expr(expr)?),
				span: *span,
			},

			Expr::Call {
				callee,
				call_type,
				named_generics,
				args,
				span,
			} => {
				let rcallee: ResolvedExpr = self.resolve_expr(callee)?;
				let rng: Vec<(String, ResolvedType)> = named_generics
					.iter()
					.map(|(name, ty)| return self.resolve_type(ty).map(|rt| return (name.clone(), rt)))
					.collect::<Result<_, _>>()?;
				let rargs = args
					.iter()
					.map(|a| return self.resolve_expr(a))
					.collect::<Result<_, _>>()?;
				ResolvedExpr::Call {
					callee: Box::new(rcallee),
					call_type: *call_type,
					named_generics: rng,
					args: rargs,
					span: *span,
				}
			}

			Expr::Field { base, name, span } => {
				let field_name: String = name
					.segments
					.iter()
					.map(|s| return s.name.as_str())
					.collect::<Vec<_>>()
					.join("::");
				ResolvedExpr::Field {
					base: Box::new(self.resolve_expr(base)?),
					name: field_name,
					span: *span,
				}
			}

			Expr::Index { base, index, span } => ResolvedExpr::Index {
				base: Box::new(self.resolve_expr(base)?),
				index: Box::new(self.resolve_expr(index)?),
				span: *span,
			},

			Expr::Range(re) => ResolvedExpr::Range(self.resolve_range_expr(re)?),

			Expr::Tuple { elements, span } => ResolvedExpr::Tuple {
				elements: elements
					.iter()
					.map(|e| return self.resolve_expr(e))
					.collect::<Result<_, _>>()?,
				span: *span,
			},

			Expr::Array(arr) => ResolvedExpr::Array(match arr {
				ArrayLiteral::List { elements, span } => ResolvedArrayLiteral::List {
					elements: elements
						.iter()
						.map(|e| return self.resolve_expr(e))
						.collect::<Result<_, _>>()?,
					span: *span,
				},
				ArrayLiteral::Repeat { value, count, span } => ResolvedArrayLiteral::Repeat {
					value: Box::new(self.resolve_expr(value)?),
					count: Box::new(self.resolve_expr(count)?),
					span: *span,
				},
			}),

			Expr::StructInit {
				path,
				fields,
				base,
				has_rest,
				span,
			} => {
				let rp: ResolvedPath = self.resolve_path_full(path, *span)?;
				let rfields: Vec<(String, ResolvedExpr)> = fields
					.iter()
					.map(|(name, e)| return self.resolve_expr(e).map(|re| return (name.clone(), re)))
					.collect::<Result<_, _>>()?;
				let rbase = base
					.as_ref()
					.map(|e| return self.resolve_expr(e))
					.transpose()?
					.map(Box::new);
				ResolvedExpr::StructInit {
					path: rp,
					fields: rfields,
					base: rbase,
					has_rest: *has_rest,
					span: *span,
				}
			}

			Expr::Block(block) => ResolvedExpr::Block(Box::new(self.resolve_scoped_block(block)?)),

			Expr::UnsafeBlock(block) => ResolvedExpr::UnsafeBlock(Box::new(self.resolve_scoped_block(block)?)),

			Expr::Switch { expr, arms, span } => {
				let rexpr: ResolvedExpr = self.resolve_expr(expr)?;
				let mut rarms: Vec<ResolvedSwitchArm> = Vec::new();
				for arm in arms {
					let arm_scope: Option<ScopeId> = self.next_anon_scope();
					let prev: ScopeId = self.current_scope;
					if let Some(sc) = arm_scope {
						self.current_scope = sc;
					}
					let rpat = self.resolve_pattern(&arm.pattern)?;
					let rbody = match &arm.body {
						parser::SwitchBody::Expr(e) => ResolvedSwitchBody::Expr(self.resolve_expr(e)?),
						parser::SwitchBody::Block(b) => {
							let block = self.resolve_block_contents(b)?;
							ResolvedSwitchBody::Block(block)
						}
					};
					self.current_scope = prev;
					rarms.push(ResolvedSwitchArm {
						pattern: rpat,
						body: rbody,
						span: arm.span(),
					});
				}
				ResolvedExpr::Switch {
					expr: Box::new(rexpr),
					arms: rarms,
					span: *span,
				}
			}

			Expr::If {
				cond,
				then_block,
				else_branch,
				span,
			} => {
				let rcond: ResolvedExpr = self.resolve_expr(cond)?;
				let then_scope: Option<ScopeId> = self.next_anon_scope();
				let prev: ScopeId = self.current_scope;
				if let Some(sc) = then_scope {
					self.current_scope = sc;
				}
				let rthen: ResolvedBlock = self.resolve_block_contents(then_block)?;
				self.current_scope = prev;

				let relse: Option<Box<ResolvedExpr>> = if let Some(e) = else_branch {
					let else_scope = match e.as_ref() {
						Expr::Block(_b) | Expr::UnsafeBlock(_b) => self.next_anon_scope(),
						_ => None,
					};
					let prev: ScopeId = self.current_scope;
					if let Some(sc) = else_scope {
						self.current_scope = sc;
					}
					let re: ResolvedExpr = self.resolve_expr(e)?;
					self.current_scope = prev;
					Some(Box::new(re))
				} else {
					None
				};
				ResolvedExpr::If {
					cond: Box::new(rcond),
					then_block: rthen,
					else_branch: relse,
					span: *span,
				}
			}

			Expr::IfVar { .. } => {
				unreachable!("should have been covered by the desugarer");
			}

			Expr::Loop { label, body, span } => {
				let loop_scope: Option<ScopeId> = self.next_anon_scope();
				let prev: ScopeId = self.current_scope;
				if let Some(sc) = loop_scope {
					self.current_scope = sc;
				}
				let rbody: ResolvedBlock = self.resolve_block_contents(body)?;
				self.current_scope = prev;
				ResolvedExpr::Loop {
					label: label.clone(),
					body: Box::new(rbody),
					span: *span,
				}
			}
		});
	}

	fn resolve_block_contents(&mut self, block: &parser::Block) -> Result<ResolvedBlock, NameResolutionError>
	{
		let mut stmts: Vec<ResolvedStmt> = Vec::new();
		for stmt in &block.stmts {
			stmts.push(self.resolve_stmt(stmt)?);
		}
		let tail: Option<ResolvedExpr> = block
			.tail_expr
			.as_ref()
			.map(|e| return self.resolve_expr(e))
			.transpose()?;
		return Ok(ResolvedBlock {
			stmts,
			tail_expr: tail.map(Box::new),
			span: block.span,
		});
	}

	fn resolve_scoped_block(&mut self, block: &parser::Block) -> Result<ResolvedBlock, NameResolutionError>
	{
		let prev: ScopeId = self.current_scope;
		let found = self.next_anon_scope();
		if let Some(sc) = found {
			self.current_scope = sc;
		}
		let result: ResolvedBlock = self.resolve_block_contents(block)?;
		self.current_scope = prev;
		return Ok(result);
	}

	fn resolve_function_body(&mut self, block: &parser::Block) -> Result<ResolvedBlock, NameResolutionError>
	{
		return self.resolve_block_contents(block);
	}

	fn resolve_stmt(&mut self, stmt: &parser::Stmt) -> Result<ResolvedStmt, NameResolutionError>
	{
		use parser::Stmt;

		return Ok(match stmt {
			Stmt::VariableDecl(var) => ResolvedStmt::VariableDecl(self.resolve_variable_decl(var)?),

			Stmt::Assignment {
				target,
				op,
				value,
				span,
			} => ResolvedStmt::Assignment {
				target: self.resolve_expr(target)?,
				op: op.clone(),
				value: self.resolve_expr(value)?,
				span: *span,
			},

			Stmt::Return { value, span } => ResolvedStmt::Return {
				value: value.as_ref().map(|e| return self.resolve_expr(e)).transpose()?,
				span: *span,
			},

			Stmt::Expr(e) => ResolvedStmt::Expr(self.resolve_expr(e)?),

			Stmt::Break { label, value, span } => ResolvedStmt::Break {
				label: label.clone(),
				value: value.as_ref().map(|e| return self.resolve_expr(e)).transpose()?,
				span: *span,
			},

			Stmt::Continue { label, span } => ResolvedStmt::Continue {
				label: label.clone(),
				span: *span,
			},

			Stmt::Directive(d) => ResolvedStmt::Directive(self.resolve_directive_node(d)?),

			Stmt::If {
				cond,
				then_block,
				else_branch,
				span,
			} => {
				let rcond: ResolvedExpr = self.resolve_expr(cond)?;

				let then_scope: Option<ScopeId> = self.next_anon_scope();
				let prev: ScopeId = self.current_scope;
				if let Some(sc) = then_scope {
					self.current_scope = sc;
				}
				let rthen: ResolvedBlock = self.resolve_block_contents(then_block)?;
				self.current_scope = prev;

				let relse: Option<Box<ResolvedStmt>> = if let Some(el) = else_branch {
					let else_scope: Option<ScopeId> = match &**el {
						Stmt::Block(_b) => self.next_anon_scope(),
						_ => None,
					};
					let prev: ScopeId = self.current_scope;
					if let Some(sc) = else_scope {
						self.current_scope = sc;
					}
					let rs: ResolvedStmt = self.resolve_stmt(el)?;
					self.current_scope = prev;
					Some(Box::new(rs))
				} else {
					None
				};

				ResolvedStmt::If {
					cond: rcond,
					then_block: rthen,
					else_branch: relse,
					span: *span,
				}
			}

			Stmt::IfVar { .. } | Stmt::While { .. } | Stmt::WhileVarLoop { .. } | Stmt::For { .. } => {
				unreachable!("desugarer should have handeled this");
			}

			Stmt::Loop { label, body, span } => {
				let loop_scope: Option<ScopeId> = self.next_anon_scope();
				let prev: ScopeId = self.current_scope;
				if let Some(sc) = loop_scope {
					self.current_scope = sc;
				}
				let rbody: ResolvedBlock = self.resolve_block_contents(body)?;
				self.current_scope = prev;
				ResolvedStmt::Loop {
					label: label.clone(),
					body: rbody,
					span: *span,
				}
			}

			Stmt::Delete { expr, span } => ResolvedStmt::Delete {
				expr: self.resolve_expr(expr)?,
				span: *span,
			},

			Stmt::Unsafe(block) => ResolvedStmt::Unsafe(self.resolve_scoped_block(block)?),
			Stmt::Block(block) => ResolvedStmt::Block(self.resolve_scoped_block(block)?),
		});
	}

	fn resolve_directive_node(
		&mut self,
		node: &parser::DirectiveNode,
	) -> Result<ResolvedDirectiveNode, NameResolutionError>
	{
		if node.body.is_some() {
			unimplemented!("directive bodies are not yet supported in name resolution");
		}

		let directive = match &node.directive {
			parser::Directive::Import { import, visibility, .. } => ResolvedDirective::Import {
				import: import.clone(),
				visibility: *visibility,
			},
			parser::Directive::Use {
				use_path, visibility, ..
			} => ResolvedDirective::Use {
				use_path: use_path.clone(),
				visibility: *visibility,
			},
			parser::Directive::Custom { name, params } => ResolvedDirective::Custom {
				name: name.clone(),
				params: params.clone(),
			},
			parser::Directive::ValidateStructPattern {
				struct_path,
				pattern_fields,
				has_rest,
			} => {
				let rp = self.resolve_path_full(struct_path, struct_path.span())?;
				ResolvedDirective::ValidateStructPattern {
					struct_path: rp,
					pattern_fields: pattern_fields.clone(),
					has_rest: *has_rest,
				}
			}
			parser::Directive::ValidateType { ty, expr } => ResolvedDirective::ValidateType {
				ty: self.resolve_type(ty)?,
				expr: self.resolve_expr(expr)?,
			},
		};

		return Ok(ResolvedDirectiveNode {
			directive,
			body: None,
			span: node.span,
		});
	}

	fn resolve_function_decl(
		&mut self,
		func: &parser::FunctionDecl,
	) -> Result<ResolvedFunctionDecl, NameResolutionError>
	{
		let sig: &FunctionSignature = &func.signature;
		let name_str: &str = sig
			.name
			.segments
			.first()
			.expect("parser guarantees at least one segment")
			.name
			.as_str();

		let resolved_name: SymbolId = self
			.find_in_scope_chain(self.current_scope, name_str)
			.ok_or_else(|| {
				return NameResolutionError {
					span: sig.name.span(),
					kind: NameResolutionErrorKind::UnresolvedPath { path: sig.name.clone() },
					context: Vec::new(),
				};
			})?
			.0;

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev: ScopeId = self.current_scope;
		self.current_scope = body_scope;

		let mut resolved_params: Vec<ResolvedParam> = Vec::new();
		for param in &sig.params {
			if param.variadic {
				break;
				//let ty: ResolvedType = self.resolve_type(&param.ty)?;
				//resolved_params.push(ResolvedParam {
				//	symbol: SymbolId(usize::MAX),
				//	name: String::from("..."),
				//	ty,
				//	mutable: false,
				//	variadic: true,
				//	span: param.span(),
				//});
				//unimplemented!("variadic arguments are not yet allowed");
			}
			let (param_name, param_span, param_mutable) = match &param.pattern {
				parser::Pattern::TypedIdentifier {
					path, span, mutable, ..
				} => (path.segments[0].name.clone(), *span, *mutable),
				_ => unreachable!("desugarer guarantees TypedIdentifier for params"),
			};
			let param_sym: SymbolId = self.find_in_scope(body_scope, &param_name).ok_or_else(|| {
				return NameResolutionError {
					span: param_span,
					kind: NameResolutionErrorKind::UnresolvedPath {
						path: Path::simple(vec![param_name.clone()], param_span),
					},
					context: Vec::new(),
				};
			})?;
			let ty: ResolvedType = self.resolve_type(&param.ty)?;
			resolved_params.push(ResolvedParam {
				symbol: param_sym,
				name: param_name,
				ty,
				mutable: param_mutable,
				variadic: false,
				span: param_span,
			});
		}

		let return_type: ResolvedType = self.resolve_type(&sig.return_type)?;
		let where_clause: Vec<ResolvedWhereConstraint> = sig
			.where_clause
			.iter()
			.map(|c| return self.resolve_where_constraint(c))
			.collect::<Result<_, _>>()?;

		let heap_generics: Vec<ResolvedGenericHeapParam> = sig
			.heap_generics
			.iter()
			.map(|hp| {
				let kind = match &hp.kind {
					parser::HeapGenericKind::Forwarded => ResolvedGenericHeapKind::Forwarded,
					parser::HeapGenericKind::Forced(ty) => ResolvedGenericHeapKind::Forced(self.resolve_type(ty)?),
				};
				return Ok(ResolvedGenericHeapParam {
					name: hp.name.clone(),
					kind,
					span: hp.span,
				});
			})
			.collect::<Result<_, NameResolutionError>>()?;

		let resolved_sig: ResolvedFunctionSignature = ResolvedFunctionSignature {
			resolved_name,
			name: sig
				.name
				.segments
				.iter()
				.map(|s| return s.name.as_str())
				.collect::<Vec<_>>()
				.join("::"),
			modifiers: sig.modifiers.clone(),
			generics: sig
				.generics
				.iter()
				.map(|g| {
					if !g.bounds.is_empty() {
						unreachable!("desugarer should have handeled this");
					}
					return (g.name.clone(), g.span);
				})
				.collect(),
			heap_generics,
			call_type: sig.call_type,
			params: resolved_params,
			return_type,
			where_clause,
			span: sig.span(),
		};

		let body: Option<ResolvedBlock> = func
			.body
			.as_ref()
			.map(|b| return self.resolve_function_body(b))
			.transpose()?;

		self.current_scope = prev;

		return Ok(ResolvedFunctionDecl {
			resolved_name,
			signature: resolved_sig,
			body,
			docs: func.docs.clone(),
			span: func.span(),
		});
	}

	fn resolve_variable_decl(&mut self, var: &VariableDecl) -> Result<ResolvedVariableDecl, NameResolutionError>
	{
		let (name_str, var_span, mutable) = match &var.pattern {
			parser::Pattern::TypedIdentifier {
				path, span, mutable, ..
			} => (
				path.segments
					.first()
					.expect("desugarer guarantees single segment")
					.name
					.clone(),
				*span,
				*mutable,
			),
			_ => unreachable!("desugarer guarantees TypedIdentifier"),
		};

		let mut check_scope: ScopeId = self.current_scope;
		loop {
			let count: Vec<SymbolId> = self
				.global
				.scope(check_scope)
				.symbols
				.iter()
				.filter(|&&id| return self.global.symbol(id).name == name_str)
				.copied()
				.collect();

			let threshold: usize = if check_scope == self.current_scope { 2 } else { 1 };

			if count.len() >= threshold {
				return Err(NameResolutionError {
					span: var_span,
					kind: NameResolutionErrorKind::ShadowedVariable {
						name: name_str,
						first_definition: self.global.symbol(count[threshold - 1]).def_span,
					},
					context: Vec::new(),
				});
			}

			match self.global.scope(check_scope).parent {
				Some(parent) => check_scope = parent,
				None => break,
			}
		}

		let ty: ResolvedType = match &var.pattern {
			parser::Pattern::TypedIdentifier { ty, .. } => self.resolve_type(ty)?,
			_ => unreachable!(),
		};

		let modifiers: Vec<parser::Modifier> = match &var.pattern {
			parser::Pattern::TypedIdentifier { modifiers, .. } => modifiers.clone(),
			_ => unreachable!(),
		};

		let init: Option<ResolvedExpr> = var.init.as_ref().map(|e| return self.resolve_expr(e)).transpose()?;

		let resolved_name: SymbolId = self
			.find_in_scope_chain(self.current_scope, &name_str)
			.ok_or_else(|| {
				return NameResolutionError {
					span: var_span,
					kind: NameResolutionErrorKind::UnresolvedPath {
						path: Path::simple(vec![name_str.clone()], var_span),
					},
					context: Vec::new(),
				};
			})?
			.0;

		return Ok(ResolvedVariableDecl {
			resolved_name,
			name: name_str,
			ty,
			init,
			comp_const: var.comp_const,
			mutable,
			modifiers,
			docs: var.docs.clone(),
			span: var.span(),
		});
	}

	fn resolve_struct_decl(&mut self, s: &StructDecl) -> Result<ResolvedStructDecl, NameResolutionError>
	{
		let name_str: &str = s.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self
			.find_in_scope_chain(self.current_scope, name_str)
			.ok_or_else(|| {
				return NameResolutionError {
					span: s.name.span(),
					kind: NameResolutionErrorKind::UnresolvedPath { path: s.name.clone() },
					context: Vec::new(),
				};
			})?
			.0;

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev = self.current_scope;
		self.current_scope = body_scope;

		let fields: Vec<ResolvedStructField> = s
			.fields
			.iter()
			.map(|f| {
				let ty = self.resolve_type(&f.ty)?;
				let default_value = f
					.default_value
					.as_ref()
					.map(|e| return self.resolve_expr(e))
					.transpose()?;
				return Ok(ResolvedStructField {
					name: f.name.clone(),
					ty,
					default_value,
					modifiers: f.modifiers.clone(),
					docs: f.docs.clone(),
					span: f.span(),
				});
			})
			.collect::<Result<_, NameResolutionError>>()?;

		let where_clause: Vec<ResolvedWhereConstraint> = s
			.where_clause
			.iter()
			.map(|c| return self.resolve_where_constraint(c))
			.collect::<Result<_, _>>()?;

		self.current_scope = prev;

		return Ok(ResolvedStructDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: s.modifiers.clone(),
			generics: s.generics.clone(),
			fields,
			where_clause,
			docs: s.docs.clone(),
			span: s.span(),
		});
	}

	fn resolve_union_decl(&mut self, u: &UnionDecl) -> Result<ResolvedUnionDecl, NameResolutionError>
	{
		let name_str: &str = u.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self
			.find_in_scope_chain(self.current_scope, name_str)
			.ok_or_else(|| {
				return NameResolutionError {
					span: u.name.span(),
					kind: NameResolutionErrorKind::UnresolvedPath { path: u.name.clone() },
					context: Vec::new(),
				};
			})?
			.0;

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev: ScopeId = self.current_scope;
		self.current_scope = body_scope;

		let fields: Vec<ResolvedUnionField> = u
			.fields
			.iter()
			.map(|f| {
				let ty = self.resolve_type(&f.ty)?;
				return Ok(ResolvedUnionField {
					name: f.name.clone(),
					ty,
					modifiers: f.modifiers.clone(),
					docs: f.docs.clone(),
					span: f.span(),
				});
			})
			.collect::<Result<_, NameResolutionError>>()?;

		let where_clause: Vec<ResolvedWhereConstraint> = u
			.where_clause
			.iter()
			.map(|c| return self.resolve_where_constraint(c))
			.collect::<Result<_, _>>()?;

		self.current_scope = prev;

		return Ok(ResolvedUnionDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: u.modifiers.clone(),
			generics: u.generics.clone(),
			fields,
			where_clause,
			docs: u.docs.clone(),
			span: u.span(),
		});
	}

	fn resolve_enum_decl(&mut self, e: &EnumDecl) -> Result<ResolvedEnumDecl, NameResolutionError>
	{
		let name_str: &str = e.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self
			.find_in_scope_chain(self.current_scope, name_str)
			.ok_or_else(|| {
				return NameResolutionError {
					span: e.name.span(),
					kind: NameResolutionErrorKind::UnresolvedPath { path: e.name.clone() },
					context: Vec::new(),
				};
			})?
			.0;

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev: ScopeId = self.current_scope;
		self.current_scope = body_scope;

		let variants: Vec<ResolvedEnumVariant> = e
			.variants
			.iter()
			.map(|v| {
				let value = v
					.value
					.as_ref()
					.map(|expr| return self.resolve_expr(expr))
					.transpose()?;
				return Ok(ResolvedEnumVariant {
					name: v.name.clone(),
					value,
					docs: v.docs.clone(),
					span: v.span(),
				});
			})
			.collect::<Result<_, NameResolutionError>>()?;

		self.current_scope = prev;

		return Ok(ResolvedEnumDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: e.modifiers.clone(),
			generics: e.generics.clone(),
			variants,
			docs: e.docs.clone(),
			span: e.span(),
		});
	}

	fn resolve_variant_decl(&mut self, v: &parser::VariantDecl) -> Result<ResolvedVariantDecl, NameResolutionError>
	{
		let name_str: &str = v.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self
			.find_in_scope_chain(self.current_scope, name_str)
			.ok_or_else(|| {
				return NameResolutionError {
					span: v.name.span(),
					kind: NameResolutionErrorKind::UnresolvedPath { path: v.name.clone() },
					context: Vec::new(),
				};
			})?
			.0;

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev: ScopeId = self.current_scope;
		self.current_scope = body_scope;

		let variants: Vec<ResolvedVariantMember> = v
			.variants
			.iter()
			.map(|m| {
				let ty = m.ty.as_ref().map(|t| return self.resolve_type(t)).transpose()?;
				let value = m.value.as_ref().map(|e| return self.resolve_expr(e)).transpose()?;
				return Ok(ResolvedVariantMember {
					name: m.name.clone(),
					ty,
					value,
					docs: m.docs.clone(),
					span: m.span(),
				});
			})
			.collect::<Result<_, NameResolutionError>>()?;

		self.current_scope = prev;

		return Ok(ResolvedVariantDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: v.modifiers.clone(),
			generics: v.generics.clone(),
			variants,
			docs: v.docs.clone(),
			span: v.span(),
		});
	}

	fn resolve_type_alias_decl(&mut self, t: &TypeAliasDecl) -> Result<ResolvedTypeAliasDecl, NameResolutionError>
	{
		let name_str: &str = t.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self
			.find_in_scope_chain(self.current_scope, name_str)
			.ok_or_else(|| {
				return NameResolutionError {
					span: t.name.span(),
					kind: NameResolutionErrorKind::UnresolvedPath { path: t.name.clone() },
					context: Vec::new(),
				};
			})?
			.0;

		let ty: ResolvedType = self.resolve_type(&t.ty)?;

		return Ok(ResolvedTypeAliasDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: t.modifiers.clone(),
			generics: t.generics.clone(),
			ty,
			docs: t.docs.clone(),
			span: t.span(),
		});
	}

	fn resolve_assoc_type_decl(&mut self, t: &AssocTypeDecl) -> Result<ResolvedAssocTypeDecl, NameResolutionError>
	{
		let name_str: &str = t.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self
			.find_in_scope_chain(self.current_scope, name_str)
			.ok_or_else(|| {
				return NameResolutionError {
					span: t.name.span(),
					kind: NameResolutionErrorKind::UnresolvedPath { path: t.name.clone() },
					context: Vec::new(),
				};
			})?
			.0;

		let ty: Option<ResolvedType> = if let Some(pty) = &t.ty {
			Some(self.resolve_type(pty)?)
		} else {
			None
		};

		return Ok(ResolvedAssocTypeDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: t.modifiers.clone(),
			generics: t.generics.clone(),
			ty,
			docs: t.docs.clone(),
			span: t.span(),
		});
	}

	fn resolve_trait_decl(&mut self, t: &TraitDecl) -> Result<ResolvedTraitDecl, NameResolutionError>
	{
		let name_str: &str = t.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self
			.find_in_scope_chain(self.current_scope, name_str)
			.ok_or_else(|| {
				return NameResolutionError {
					span: t.name.span(),
					kind: NameResolutionErrorKind::UnresolvedPath { path: t.name.clone() },
					context: Vec::new(),
				};
			})?
			.0;

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev: ScopeId = self.current_scope;
		self.current_scope = body_scope;

		let prev_trait_scope: Option<ScopeId> = self.trait_scope.replace(body_scope);
		let prev_self_sym: Option<SymbolId> = self.self_sym.replace(resolved_name);

		let super_traits: Vec<ResolvedWhereBound> = t
			.super_traits
			.iter()
			.map(|b| return self.resolve_where_bound(b))
			.collect::<Result<_, _>>()?;

		let items: Vec<ResolvedTraitItem> = t
			.items
			.iter()
			.map(|item| match item {
				parser::TraitItem::Function(f) => {
					return self.resolve_function_decl(f).map(ResolvedTraitItem::Function);
				}
				parser::TraitItem::TypeAlias(ta) => {
					return self.resolve_type_alias_decl(ta).map(ResolvedTraitItem::TypeAlias);
				}
				parser::TraitItem::AssocType(ta) => {
					return self.resolve_assoc_type_decl(ta).map(ResolvedTraitItem::AssocType);
				}
				parser::TraitItem::Const(var) => return self.resolve_variable_decl(var).map(ResolvedTraitItem::Const),
			})
			.collect::<Result<_, _>>()?;

		self.current_scope = prev;
		self.trait_scope = prev_trait_scope;
		self.self_sym = prev_self_sym;

		return Ok(ResolvedTraitDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: t.modifiers.clone(),
			generics: t.generics.clone(),
			super_traits,
			items,
			docs: t.docs.clone(),
			span: t.span(),
		});
	}

	fn resolve_module_decl(&mut self, m: &ModuleDecl) -> Result<ResolvedModuleDecl, NameResolutionError>
	{
		let name_str: &str = m.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self
			.find_in_scope_chain(self.current_scope, name_str)
			.ok_or_else(|| {
				return NameResolutionError {
					span: m.name.span(),
					kind: NameResolutionErrorKind::UnresolvedPath { path: m.name.clone() },
					context: Vec::new(),
				};
			})?
			.0;

		let resolved_body: Option<ResolvedTopLevelBlock> = match &m.kind {
			ModuleKind::Inline(body) => {
				let body_scope = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
				let saved_imports: Vec<UseImport> = std::mem::take(&mut self.use_imports);
				let prev: ScopeId = self.current_scope;
				self.current_scope = body_scope;
				let resolved: ResolvedTopLevelBlock = self.resolve_top_level_block(body)?;
				self.current_scope = prev;
				self.use_imports = saved_imports;
				Some(resolved)
			}
			ModuleKind::External => None,
		};

		return Ok(ResolvedModuleDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: m.modifiers.clone(),
			resolved_body,
			docs: m.docs.clone(),
			span: m.span(),
		});
	}

	fn resolve_impl_decl(&mut self, i: &ImplDecl) -> Result<ResolvedImplDecl, NameResolutionError>
	{
		let body_scope: Option<ScopeId> = self.next_anon_scope();
		let prev: ScopeId = self.current_scope;
		if let Some(sc) = body_scope {
			self.current_scope = sc;
		}

		let resolved_target: ResolvedPath = self.resolve_path_or_primitive(&i.target.path, i.target.span())?;
		let resolved_trait: Option<ResolvedPath> = i
			.trait_path
			.as_ref()
			.map(|tp| return self.resolve_path_or_primitive(&tp.path, tp.span()))
			.transpose()?;

		let prev_self_sym: Option<SymbolId> = match &resolved_target.kind {
			ResolvedPathKind::Resolved(id) => self.self_sym.replace(*id),
			ResolvedPathKind::AssocItem { base, .. } => self.self_sym.replace(*base),
			ResolvedPathKind::Primitive(_) => self.self_sym.take(),
		};

		let where_clause: Vec<ResolvedWhereConstraint> = i
			.where_clause
			.iter()
			.map(|c| return self.resolve_where_constraint(c))
			.collect::<Result<_, _>>()?;

		let items: Vec<ResolvedImplItem> = i
			.body
			.iter()
			.map(|item| match item {
				parser::ImplItem::Function(f) => return self.resolve_function_decl(f).map(ResolvedImplItem::Function),
				parser::ImplItem::TypeAlias(ta) => {
					return self.resolve_type_alias_decl(ta).map(ResolvedImplItem::TypeAlias);
				}
				parser::ImplItem::AssocType(ta) => {
					return self.resolve_assoc_type_decl(ta).map(ResolvedImplItem::AssocType);
				}
				parser::ImplItem::Const(var) => return self.resolve_variable_decl(var).map(ResolvedImplItem::Const),
			})
			.collect::<Result<_, _>>()?;

		self.current_scope = prev;
		self.self_sym = prev_self_sym;

		return Ok(ResolvedImplDecl {
			resolved_target,
			resolved_trait,
			modifiers: i.modifiers.clone(),
			generics: i.generics.clone(),
			where_clause,
			items,
			docs: i.docs.clone(),
			span: i.span(),
		});
	}
}

pub fn resolve_names(
	logical_path: &[String],
	ast: &DesugaredAST,
	symbols: &LocalSymbolTable,
	global: &GlobalSymbolTable,
	modules: &[(Vec<String>, DesugaredAST, LocalSymbolTable)],
) -> Result<ResolvedModule, CompileError>
{
	let scope_offset: usize = global.module_roots.get(logical_path).copied().map_or(0, |s| return s.0);

	let mut resolver: Resolver<'_> = Resolver::new(global, modules, symbols, scope_offset);

	let resolved_block: ResolvedTopLevelBlock = resolver
		.resolve_top_level_block(&ast.top_level_block)
		.map_err(CompileError::NameResolution)?;

	let span: Span = resolved_block.span;

	return Ok(ResolvedModule {
		path: logical_path.to_vec(),
		ast: ResolvedAST {
			span,
			top_level_block: resolved_block,
			source_index: ast.source_index,
		},
		symbols: symbols.clone(),
	});
}

use crate::parser::{IndentWriter, write_docs};

impl fmt::Display for ResolvedAST
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "{}", self.top_level_block);
	}
}

impl fmt::Display for ResolvedTopLevelBlock
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		let mut w = IndentWriter::new();
		for item in &self.items {
			write_resolved_top_level_decl(f, &mut w, item)?;
			writeln!(f)?;
		}
		return Ok(());
	}
}

impl fmt::Display for ResolvedPath
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match &self.kind {
			ResolvedPathKind::Resolved(sym) => write!(f, "{} /* #{} */", self.original, sym.0),
			ResolvedPathKind::AssocItem { base, member } => {
				write!(f, "{} /* #{} */::{}  /* assoc */", self.original, base.0, member)
			}
			ResolvedPathKind::Primitive(name) => write!(f, "{} /* primitive */", name),
		};
	}
}

impl fmt::Display for ResolvedType
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "{}", self.core);
	}
}

impl fmt::Display for ResolvedTypeCore
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			ResolvedTypeCore::Base { path, generics } => {
				write!(f, "{}", path)?;
				if !generics.is_empty() {
					write!(f, "<")?;
					for (i, g) in generics.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", g)?;
					}
					write!(f, ">")?;
				}
				return Ok(());
			}
			ResolvedTypeCore::Primitive { name, generics } => {
				write!(f, "{}", name)?;
				if !generics.is_empty() {
					write!(f, "<")?;
					for (i, g) in generics.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", g)?;
					}
					write!(f, ">")?;
				}
				return Ok(());
			}
			ResolvedTypeCore::Reference { mutable, inner } => {
				write!(f, "&")?;
				if *mutable {
					write!(f, "mut ")?;
				}
				return write!(f, "{}", inner);
			}
			ResolvedTypeCore::Mutable { inner } => return write!(f, "mut {}", inner),
			ResolvedTypeCore::Pointer { mutable, inner } => {
				write!(f, "*")?;
				if *mutable {
					write!(f, "mut ")?;
				}
				return write!(f, "{}", inner);
			}
			ResolvedTypeCore::Array { inner, size } => {
				write!(f, "[{}", inner)?;
				if let Some(s) = size {
					write!(f, "; {}", s)?;
				}
				return write!(f, "]");
			}
			ResolvedTypeCore::Tuple(types) => {
				write!(f, "(")?;
				for (i, ty) in types.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", ty)?;
				}
				return write!(f, ")");
			}
			ResolvedTypeCore::ImplTrait { bounds } => {
				write!(f, "impl ")?;
				for (i, b) in bounds.iter().enumerate() {
					if i > 0 {
						write!(f, " + ")?;
					}
					write!(f, "{}", b)?;
				}
				return Ok(());
			}
		}
	}
}

impl fmt::Display for ResolvedWhereBound
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			ResolvedWhereBound::Path { path, args } => {
				write!(f, "{}", path)?;
				if !args.is_empty() {
					write!(f, "<")?;
					for (i, a) in args.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", a)?;
					}
					write!(f, ">")?;
				}
				Ok(())
			}
			ResolvedWhereBound::Func(fb) => write!(f, "{}", fb),
		};
	}
}

impl fmt::Display for ResolvedFuncBound
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			ResolvedFuncBound::Fn { args, ret } => {
				write!(f, "Fn(")?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", a)?;
				}
				write!(f, ")")?;
				if let Some(ty) = ret {
					write!(f, " -> {}", ty)?;
				}
				Ok(())
			}
		};
	}
}

impl fmt::Display for ResolvedGenericArg
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			ResolvedGenericArg::Type(ty) => write!(f, "{}", ty),
			ResolvedGenericArg::Binding { name, ty, .. } => write!(f, "{} = {}", name, ty),
		};
	}
}

impl fmt::Display for ResolvedWhereConstraint
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.ty)?;
		if !self.type_args.is_empty() {
			write!(f, "<")?;
			for (i, a) in self.type_args.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}", a)?;
			}
			write!(f, ">")?;
		}
		write!(f, ": ")?;
		for (i, b) in self.bounds.iter().enumerate() {
			if i > 0 {
				write!(f, " + ")?;
			}
			write!(f, "{}", b)?;
		}
		return Ok(());
	}
}

impl fmt::Display for ResolvedRangeExpr
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		if let Some(s) = &self.start {
			write!(f, "{}", s)?;
		}
		if self.inclusive {
			write!(f, "..=")?;
		} else {
			write!(f, "..")?;
		}
		if let Some(e) = &self.end {
			write!(f, "{}", e)?;
		}
		return Ok(());
	}
}

impl fmt::Display for ResolvedPattern
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			ResolvedPattern::Wildcard { ty, .. } => {
				write!(f, "_")?;
				if let Some(t) = ty {
					write!(f, ": {}", t)?;
				}
				return Ok(());
			}
			ResolvedPattern::Literal { value, .. } => return write!(f, "{}", value),
			ResolvedPattern::TypedIdentifier { name, ty, mutable, .. } => {
				if *mutable {
					write!(f, "mut ")?;
				}
				return write!(f, "{}: {}", name, ty);
			}
			ResolvedPattern::Variant { path, args, .. } => {
				write!(f, "{}", path)?;
				if !args.is_empty() {
					write!(f, "(")?;
					for (i, a) in args.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", a)?;
					}
					write!(f, ")")?;
				}
				return Ok(());
			}
			ResolvedPattern::Tuple { patterns, .. } => {
				write!(f, "(")?;
				for (i, p) in patterns.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", p)?;
				}
				return write!(f, ")");
			}
			ResolvedPattern::Struct {
				path, fields, has_rest, ..
			} => {
				write!(f, "{} {{", path)?;
				for (i, (name, pat)) in fields.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{} -> {}", name, pat)?;
				}
				if *has_rest {
					if !fields.is_empty() {
						write!(f, ", ")?;
					}
					write!(f, "..")?;
				}
				return write!(f, "}}");
			}
			ResolvedPattern::Range(re) => return write!(f, "{}", re),
			ResolvedPattern::Or { patterns, .. } => {
				for (i, p) in patterns.iter().enumerate() {
					if i > 0 {
						write!(f, " | ")?;
					}
					write!(f, "{}", p)?;
				}
				return Ok(());
			}
		}
	}
}

impl fmt::Display for ResolvedExpr
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		let mut w = IndentWriter::new();
		return write_resolved_expr(f, &mut w, self);
	}
}

pub fn write_resolved_top_level_decl(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	decl: &ResolvedTopLevelDecl,
) -> fmt::Result
{
	match decl {
		ResolvedTopLevelDecl::Function(func) => return write_resolved_function_decl(f, w, func),
		ResolvedTopLevelDecl::VariableDecl(var) => {
			write_resolved_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		ResolvedTopLevelDecl::Struct(s) => return write_resolved_struct_decl(f, w, s),
		ResolvedTopLevelDecl::Union(u) => return write_resolved_union_decl(f, w, u),
		ResolvedTopLevelDecl::Enum(e) => return write_resolved_enum_decl(f, w, e),
		ResolvedTopLevelDecl::Variant(v) => return write_resolved_variant_decl(f, w, v),
		ResolvedTopLevelDecl::TypeAlias(t) => {
			write_resolved_type_alias_decl(f, w, t)?;
			return write!(f, ";");
		}
		ResolvedTopLevelDecl::Trait(t) => return write_resolved_trait_decl(f, w, t),
		ResolvedTopLevelDecl::Module(m) => return write_resolved_module_decl(f, w, m),
		ResolvedTopLevelDecl::Impl(i) => return write_resolved_impl_decl(f, w, i),
		ResolvedTopLevelDecl::Directive(d) => return write!(f, "{};", d),
	}
}

pub fn write_resolved_function_decl(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	func: &ResolvedFunctionDecl,
) -> fmt::Result
{
	write_docs(f, w, &func.docs)?;
	write_resolved_function_signature(f, w, &func.signature)?;
	if let Some(body) = &func.body {
		write!(f, " ")?;
		write_resolved_block(f, w, body)?;
	} else {
		write!(f, ";")?;
	}
	return Ok(());
}

pub fn write_resolved_function_signature(
	f: &mut fmt::Formatter<'_>,
	_w: &mut IndentWriter,
	sig: &ResolvedFunctionSignature,
) -> fmt::Result
{
	for m in &sig.modifiers {
		write!(f, "{} ", m)?;
	}

	write!(f, "fn")?;
	match sig.call_type {
		CallType::UserHeap => write!(f, "!")?,
		CallType::UserMaybeHeap | CallType::CompilerHeap => write!(f, "?")?,
		CallType::Regular => {}
	}

	if !sig.heap_generics.is_empty() {
		write!(f, "<")?;
		for (i, g) in sig.heap_generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", g)?;
		}
		write!(f, ">")?;
	}

	write!(f, " {}", sig.name)?;

	if !sig.generics.is_empty() {
		write!(f, "<")?;
		for (i, (name, _span)) in sig.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", name)?;
		}
		write!(f, ">")?;
	}

	write!(f, "(")?;
	for (i, param) in sig.params.iter().enumerate() {
		if i > 0 {
			write!(f, ", ")?;
		}
		write_resolved_param(f, param)?;
	}
	write!(f, ")")?;

	write!(f, " -> {}", sig.return_type)?;

	if !sig.where_clause.is_empty() {
		write!(f, " where ")?;
		for (i, c) in sig.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", c)?;
		}
	}

	return Ok(());
}

impl std::fmt::Display for ResolvedGenericHeapParam
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.name)?;
		if let ResolvedGenericHeapKind::Forced(fg) = &self.kind {
			write!(f, " = {}", fg)?;
		}
		return Ok(());
	}
}

fn write_resolved_param(f: &mut fmt::Formatter<'_>, param: &ResolvedParam) -> fmt::Result
{
	if param.variadic {
		return write!(f, "...");
	}
	if param.mutable {
		write!(f, "mut ")?;
	}
	return write!(f, "{}: {}", param.name, param.ty);
}

pub fn write_resolved_variable_decl(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	var: &ResolvedVariableDecl,
) -> fmt::Result
{
	if var.comp_const {
		write!(f, "const ")?;
	} else {
		write!(f, "var ")?;
	}
	write!(f, "{}: {}", var.name, var.ty)?;
	if let Some(init) = &var.init {
		write!(f, " = ")?;
		write_resolved_expr(f, w, init)?;
	}
	return Ok(());
}

pub fn write_resolved_struct_decl(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	s: &ResolvedStructDecl,
) -> fmt::Result
{
	write_docs(f, w, &s.docs)?;
	for m in &s.modifiers {
		write!(f, "{} ", m)?;
	}
	write!(f, "struct {}", s.name)?;
	write_generic_params(f, &s.generics)?;
	write_resolved_where_clause(f, &s.where_clause)?;
	writeln!(f, " {{")?;
	w.indent();
	for field in &s.fields {
		write_resolved_struct_field(f, w, field)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_resolved_struct_field(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	field: &ResolvedStructField,
) -> fmt::Result
{
	write_docs(f, w, &field.docs)?;
	w.write_indent(f)?;
	write!(f, "{}: {}", field.name, field.ty)?;
	if let Some(dv) = &field.default_value {
		write!(f, " = ")?;
		write_resolved_expr(f, w, dv)?;
	}
	return writeln!(f, ",");
}

pub fn write_resolved_union_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, u: &ResolvedUnionDecl)
-> fmt::Result
{
	write_docs(f, w, &u.docs)?;
	for m in &u.modifiers {
		write!(f, "{} ", m)?;
	}
	write!(f, "union {}", u.name)?;
	write_generic_params(f, &u.generics)?;
	write_resolved_where_clause(f, &u.where_clause)?;
	writeln!(f, " {{")?;
	w.indent();
	for field in &u.fields {
		write_resolved_union_field(f, w, field)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_resolved_union_field(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	field: &ResolvedUnionField,
) -> fmt::Result
{
	write_docs(f, w, &field.docs)?;
	w.write_indent(f)?;
	return writeln!(f, "{}: {},", field.name, field.ty);
}

pub fn write_resolved_enum_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, e: &ResolvedEnumDecl) -> fmt::Result
{
	write_docs(f, w, &e.docs)?;
	for m in &e.modifiers {
		write!(f, "{} ", m)?;
	}
	write!(f, "enum {}", e.name)?;
	write_generic_params(f, &e.generics)?;
	writeln!(f, " {{")?;
	w.indent();
	for variant in &e.variants {
		write_resolved_enum_variant(f, w, variant)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_resolved_enum_variant(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, v: &ResolvedEnumVariant)
-> fmt::Result
{
	write_docs(f, w, &v.docs)?;
	w.write_indent(f)?;
	write!(f, "{}", v.name)?;
	if let Some(val) = &v.value {
		write!(f, " = ")?;
		write_resolved_expr(f, w, val)?;
	}
	return writeln!(f, ",");
}

pub fn write_resolved_variant_decl(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	v: &ResolvedVariantDecl,
) -> fmt::Result
{
	write_docs(f, w, &v.docs)?;
	for m in &v.modifiers {
		write!(f, "{} ", m)?;
	}
	write!(f, "variant {}", v.name)?;
	write_generic_params(f, &v.generics)?;
	writeln!(f, " {{")?;
	w.indent();
	for member in &v.variants {
		write_resolved_variant_member(f, w, member)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_resolved_variant_member(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	m: &ResolvedVariantMember,
) -> fmt::Result
{
	write_docs(f, w, &m.docs)?;
	w.write_indent(f)?;
	write!(f, "{}", m.name)?;
	if let Some(ty) = &m.ty {
		write!(f, "({})", ty)?;
	}
	if let Some(val) = &m.value {
		write!(f, " = ")?;
		write_resolved_expr(f, w, val)?;
	}
	return writeln!(f, ",");
}

pub fn write_resolved_type_alias_decl(
	f: &mut fmt::Formatter<'_>,
	w: &IndentWriter,
	t: &ResolvedTypeAliasDecl,
) -> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for m in &t.modifiers {
		write!(f, "{} ", m)?;
	}
	write!(f, "type {}", t.name)?;
	write_generic_params(f, &t.generics)?;
	return write!(f, " = {}", t.ty);
}

pub fn write_resolved_assoc_type_decl(
	f: &mut fmt::Formatter<'_>,
	w: &IndentWriter,
	t: &ResolvedAssocTypeDecl,
) -> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for m in &t.modifiers {
		write!(f, "{} ", m)?;
	}
	write!(f, "type {}", t.name)?;
	write_generic_params(f, &t.generics)?;
	if let Some(ty) = &t.ty {
		write!(f, " = {}", ty)?;
	}
	return Ok(());
}

pub fn write_resolved_trait_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, t: &ResolvedTraitDecl)
-> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for m in &t.modifiers {
		write!(f, "{} ", m)?;
	}
	write!(f, "trait {}", t.name)?;
	write_generic_params(f, &t.generics)?;
	if !t.super_traits.is_empty() {
		write!(f, ": ")?;
		for (i, st) in t.super_traits.iter().enumerate() {
			if i > 0 {
				write!(f, " + ")?;
			}
			write!(f, "{}", st)?;
		}
	}
	writeln!(f, " {{")?;
	w.indent();
	for item in &t.items {
		w.write_indent(f)?;
		write_resolved_trait_item(f, w, item)?;
		writeln!(f)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_resolved_trait_item(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	item: &ResolvedTraitItem,
) -> fmt::Result
{
	match item {
		ResolvedTraitItem::Function(func) => return write_resolved_function_decl(f, w, func),
		ResolvedTraitItem::TypeAlias(ta) => {
			write_resolved_type_alias_decl(f, w, ta)?;
			return write!(f, ";");
		}
		ResolvedTraitItem::AssocType(ta) => {
			write_resolved_assoc_type_decl(f, w, ta)?;
			return write!(f, ";");
		}
		ResolvedTraitItem::Const(var) => {
			write_resolved_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
	}
}

pub fn write_resolved_module_decl(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	m: &ResolvedModuleDecl,
) -> fmt::Result
{
	write_docs(f, w, &m.docs)?;
	for modifier in &m.modifiers {
		write!(f, "{} ", modifier)?;
	}
	write!(f, "module {}", m.name)?;
	if let Some(body) = &m.resolved_body {
		writeln!(f, " {{")?;
		w.indent();
		for item in &body.items {
			w.write_indent(f)?;
			write_resolved_top_level_decl(f, w, item)?;
			writeln!(f)?;
			writeln!(f)?;
		}
		w.dedent();
		w.write_indent(f)?;
		write!(f, "}}")?;
	} else {
		write!(f, ";")?;
	}
	return Ok(());
}

pub fn write_resolved_impl_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, i: &ResolvedImplDecl) -> fmt::Result
{
	write_docs(f, w, &i.docs)?;
	for m in &i.modifiers {
		write!(f, "{} ", m)?;
	}
	write!(f, "impl")?;
	write_generic_params(f, &i.generics)?;
	if let Some(tr) = &i.resolved_trait {
		write!(f, " {} for", tr)?;
	}
	write!(f, " {}", i.resolved_target)?;
	write_resolved_where_clause(f, &i.where_clause)?;
	writeln!(f, " {{")?;
	w.indent();
	for item in &i.items {
		w.write_indent(f)?;
		write_resolved_impl_item(f, w, item)?;
		writeln!(f)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_resolved_impl_item(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	item: &ResolvedImplItem,
) -> fmt::Result
{
	match item {
		ResolvedImplItem::Function(func) => return write_resolved_function_decl(f, w, func),
		ResolvedImplItem::TypeAlias(ta) => {
			write_resolved_type_alias_decl(f, w, ta)?;
			return write!(f, ";");
		}
		ResolvedImplItem::AssocType(ta) => {
			write_resolved_assoc_type_decl(f, w, ta)?;
			return write!(f, ";");
		}
		ResolvedImplItem::Const(var) => {
			write_resolved_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
	}
}

pub fn write_resolved_block(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, block: &ResolvedBlock) -> fmt::Result
{
	writeln!(f, "{{")?;
	w.indent();
	for stmt in &block.stmts {
		write_resolved_stmt(f, w, stmt)?;
		writeln!(f)?;
	}
	if let Some(tail) = &block.tail_expr {
		w.write_indent(f)?;
		write_resolved_expr(f, w, tail)?;
		writeln!(f)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_resolved_stmt(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, stmt: &ResolvedStmt) -> fmt::Result
{
	w.write_indent(f)?;
	return write_resolved_stmt_no_indent(f, w, stmt);
}

pub fn write_resolved_stmt_no_indent(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	stmt: &ResolvedStmt,
) -> fmt::Result
{
	match stmt {
		ResolvedStmt::VariableDecl(var) => {
			write_resolved_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		ResolvedStmt::Assignment { target, op, value, .. } => {
			write_resolved_expr(f, w, target)?;
			write!(f, " {} ", op)?;
			write_resolved_expr(f, w, value)?;
			return write!(f, ";");
		}
		ResolvedStmt::Return { value, .. } => {
			write!(f, "return")?;
			if let Some(v) = value {
				write!(f, " ")?;
				write_resolved_expr(f, w, v)?;
			}
			return write!(f, ";");
		}
		ResolvedStmt::Expr(expr) => {
			write_resolved_expr(f, w, expr)?;
			return write!(f, ";");
		}
		ResolvedStmt::Break { label, value, .. } => {
			write!(f, "break")?;
			if let Some(lbl) = label {
				write!(f, " '{}", lbl)?;
			}
			if let Some(v) = value {
				write!(f, " ")?;
				write_resolved_expr(f, w, v)?;
			}
			return write!(f, ";");
		}
		ResolvedStmt::Continue { label, .. } => {
			write!(f, "continue")?;
			if let Some(lbl) = label {
				write!(f, " '{}", lbl)?;
			}
			return write!(f, ";");
		}
		ResolvedStmt::If {
			cond,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if ")?;
			write_resolved_expr(f, w, cond)?;
			write!(f, " ")?;
			write_resolved_block(f, w, then_block)?;
			if let Some(else_stmt) = else_branch {
				write!(f, " else ")?;
				write_resolved_stmt_no_indent(f, w, else_stmt)?;
			}
			return Ok(());
		}
		ResolvedStmt::Loop { label, body, .. } => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "loop ")?;
			return write_resolved_block(f, w, body);
		}
		ResolvedStmt::Delete { expr, .. } => {
			write!(f, "delete ")?;
			write_resolved_expr(f, w, expr)?;
			return write!(f, ";");
		}
		ResolvedStmt::Unsafe(block) => {
			write!(f, "unsafe ")?;
			return write_resolved_block(f, w, block);
		}
		ResolvedStmt::Block(block) => return write_resolved_block(f, w, block),
		ResolvedStmt::Directive(d) => {
			write!(f, "{}", d)?;
			if d.body.is_none() {
				write!(f, ";")?;
			}
			return Ok(());
		}
	}
}

pub fn write_resolved_expr(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, expr: &ResolvedExpr) -> fmt::Result
{
	match expr {
		ResolvedExpr::Identifier { path, .. } => return write!(f, "{}", path),
		ResolvedExpr::UnresolvedIdentifier { path, .. } => return write!(f, "/*unresolved*/{}", path),
		ResolvedExpr::Literal { value, .. } => return write!(f, "{}", value),
		ResolvedExpr::AssocPath { base, member, .. } => {
			write!(f, "{}", base)?;
			return write!(f, "::{}", member);
		}
		ResolvedExpr::AssocSelf { member, .. } => {
			write!(f, "Self")?;
			return write!(f, "::{}", member);
		}
		ResolvedExpr::InternalCall { intrinsic, .. } => return write!(f, "{}", intrinsic),
		ResolvedExpr::Default { heap_call, .. } => {
			write!(f, "default")?;
			return match heap_call {
				CallType::Regular => write!(f, "()"),
				CallType::UserHeap => write!(f, "!()"),
				CallType::UserMaybeHeap | CallType::CompilerHeap => write!(f, "?()"),
			};
		}
		ResolvedExpr::Unary { op, expr, .. } => {
			use crate::parser::UnaryOp;
			match op {
				UnaryOp::Neg => write!(f, "-")?,
				UnaryOp::Not => write!(f, "!")?,
				UnaryOp::Deref => write!(f, "*")?,
				UnaryOp::Addr { mutable } => {
					if *mutable {
						write!(f, "&mut ")?;
					} else {
						write!(f, "&")?;
					}
				}
			}
			return write_resolved_expr(f, w, expr);
		}
		ResolvedExpr::Binary { op, lhs, rhs, .. } => {
			write!(f, "(")?;
			write_resolved_expr(f, w, lhs)?;
			write!(f, " {} ", op)?;
			write_resolved_expr(f, w, rhs)?;
			return write!(f, ")");
		}
		ResolvedExpr::Cast { ty, expr, .. } => {
			write!(f, "({}) ", ty)?;
			return write_resolved_expr(f, w, expr);
		}
		ResolvedExpr::Call {
			callee,
			call_type,
			named_generics,
			args,
			..
		} => {
			write_resolved_expr(f, w, callee)?;
			match call_type {
				CallType::UserHeap => write!(f, "!")?,
				CallType::UserMaybeHeap | CallType::CompilerHeap => write!(f, "?")?,
				CallType::Regular => {}
			}
			if !named_generics.is_empty() {
				write!(f, "<")?;
				for (i, (name, ty)) in named_generics.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}: {}", name, ty)?;
				}
				write!(f, ">")?;
			}
			write!(f, "(")?;
			for (i, arg) in args.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_resolved_expr(f, w, arg)?;
			}
			return write!(f, ")");
		}
		ResolvedExpr::Field { base, name, .. } => {
			write_resolved_expr(f, w, base)?;
			return write!(f, ".{}", name);
		}
		ResolvedExpr::Index { base, index, .. } => {
			write_resolved_expr(f, w, base)?;
			write!(f, "[")?;
			write_resolved_expr(f, w, index)?;
			return write!(f, "]");
		}
		ResolvedExpr::Range(re) => return write!(f, "{}", re),
		ResolvedExpr::Tuple { elements, .. } => {
			write!(f, "(")?;
			for (i, e) in elements.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_resolved_expr(f, w, e)?;
			}
			return write!(f, ")");
		}
		ResolvedExpr::Array(arr) => return write_resolved_array_literal(f, w, arr),
		ResolvedExpr::StructInit {
			path,
			fields,
			base,
			has_rest,
			..
		} => {
			write!(f, "{} {{", path)?;
			for (i, (name, expr)) in fields.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{} -> ", name)?;
				write_resolved_expr(f, w, expr)?;
			}
			if let Some(base_expr) = base {
				if !fields.is_empty() {
					write!(f, ", ")?;
				}
				write!(f, "..")?;
				write_resolved_expr(f, w, base_expr)?;
			} else if *has_rest {
				if !fields.is_empty() {
					write!(f, ", ")?;
				}
				write!(f, "..")?;
			}
			return write!(f, "}}");
		}
		ResolvedExpr::Block(block) => return write_resolved_block(f, w, block),
		ResolvedExpr::UnsafeBlock(block) => {
			write!(f, "unsafe ")?;
			return write_resolved_block(f, w, block);
		}
		ResolvedExpr::Switch { expr, arms, .. } => {
			write!(f, "switch ")?;
			write_resolved_expr(f, w, expr)?;
			writeln!(f, " {{")?;
			w.indent();
			for arm in arms {
				write_resolved_switch_arm(f, w, arm)?;
			}
			w.dedent();
			w.write_indent(f)?;
			return write!(f, "}}");
		}
		ResolvedExpr::If {
			cond,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if ")?;
			write_resolved_expr(f, w, cond)?;
			write!(f, " ")?;
			write_resolved_block(f, w, then_block)?;
			if let Some(else_expr) = else_branch {
				write!(f, " else ")?;
				write_resolved_expr(f, w, else_expr)?;
			}
			return Ok(());
		}
		ResolvedExpr::Loop { label, body, .. } => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "loop ")?;
			return write_resolved_block(f, w, body);
		}
	}
}

fn write_resolved_array_literal(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	arr: &ResolvedArrayLiteral,
) -> fmt::Result
{
	match arr {
		ResolvedArrayLiteral::List { elements, .. } => {
			write!(f, "[")?;
			for (i, e) in elements.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_resolved_expr(f, w, e)?;
			}
			return write!(f, "]");
		}
		ResolvedArrayLiteral::Repeat { value, count, .. } => {
			write!(f, "[")?;
			write_resolved_expr(f, w, value)?;
			write!(f, "; ")?;
			write_resolved_expr(f, w, count)?;
			return write!(f, "]");
		}
	}
}

fn write_resolved_switch_arm(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, arm: &ResolvedSwitchArm) -> fmt::Result
{
	w.write_indent(f)?;
	write!(f, "{} => ", arm.pattern)?;
	match &arm.body {
		ResolvedSwitchBody::Expr(expr) => {
			write_resolved_expr(f, w, expr)?;
			return writeln!(f, ",");
		}
		ResolvedSwitchBody::Block(block) => {
			write_resolved_block(f, w, block)?;
			return writeln!(f, ",");
		}
	}
}

fn write_generic_params(f: &mut fmt::Formatter<'_>, generics: &[crate::parser::GenericParam]) -> fmt::Result
{
	if !generics.is_empty() {
		write!(f, "<")?;
		for (i, g) in generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", g)?;
		}
		write!(f, ">")?;
	}
	return Ok(());
}

fn write_resolved_where_clause(f: &mut fmt::Formatter<'_>, clause: &[ResolvedWhereConstraint]) -> fmt::Result
{
	if !clause.is_empty() {
		write!(f, " where ")?;
		for (i, c) in clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", c)?;
		}
	}
	return Ok(());
}

impl fmt::Display for ResolvedDirectiveNode
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "{}", self.directive);
	}
}

impl fmt::Display for ResolvedDirective
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			ResolvedDirective::Import { import, .. } => write!(f, "@import \"{}\"", import),
			ResolvedDirective::Use { use_path, .. } => write!(f, "@use {}", use_path),
			ResolvedDirective::Custom { name, .. } => write!(f, "@{}", name),
			ResolvedDirective::ValidateStructPattern { struct_path, .. } => {
				write!(f, "@validate_struct_pattern {}", struct_path)
			}
			ResolvedDirective::ValidateType { ty, expr, .. } => {
				write!(f, "@validate_type({}, {})", ty, expr)
			}
		};
	}
}
