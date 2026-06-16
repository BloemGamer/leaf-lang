#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::needless_pass_by_ref_mut)]
#![allow(clippy::unused_self)]

pub mod display;
#[cfg(test)]
#[path = "../../tests/name_resolution/tests.rs"]
mod tests;

use std::fmt;

use ignorable::PartialEq;

use leaf_proc::{Spanned, compiler_bug};

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
		GlobalSymbolTable, LocalSymbolTable, Scope, ScopeId, ScopeKind, Symbol, SymbolId, SymbolKind, Visibility,
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
		/// filled in type analysis
		item: SymbolId,
		/// filled in type analysis
		base_type_args: Vec<Ty>,
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
		named_generics: Vec<(String, ResolvedExpr)>,
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
		label: String,
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
		label: String,
		value: Option<ResolvedExpr>,
		#[ignored(PartialEq)]
		span: Span,
	},
	Continue
	{
		label: String,
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
		label: String,
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
	diagnostics: Vec<DiagnosticBuilder>,
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
			diagnostics: Vec::new(),
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

	fn enclosing_module_scope(&self, mut scope: ScopeId) -> ScopeId
	{
		loop {
			let s = self.global.scope(scope);
			if matches!(s.kind, ScopeKind::ModuleInline | ScopeKind::ModuleImport) {
				return scope;
			}
			match s.parent {
				Some(parent) => scope = parent,
				None => return scope, // global root
			}
		}
	}

	fn current_module_path(&self) -> Option<Vec<String>>
	{
		let mod_scope = self.enclosing_module_scope(self.current_scope);
		return self
			.global
			.module_roots
			.iter()
			.find(|(_, s)| return **s == mod_scope)
			.map(|(p, _)| return p.clone());
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
			.map(|&local| return ScopeId(local.0 + self.scope_offset));
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
				let def_module = self.enclosing_module_scope(sym.scope);
				if sym.visibility == Visibility::Private && !self.is_descendant_of(self.current_scope, def_module) {
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
				let def_module = self.enclosing_module_scope(sym.scope);
				if sym.visibility == Visibility::Private && !self.is_descendant_of(self.current_scope, def_module) {
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
			let def_module = self.enclosing_module_scope(sym.scope);
			if sym.visibility == Visibility::Private && !self.is_descendant_of(self.current_scope, def_module) {
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

	fn resolve_path_or_primitive(&mut self, path: &Path, span: Span) -> ResolvedPath
	{
		if !path.global && path.segments.len() == 1 {
			match self.resolve_path_full(path, span) {
				Ok(rp) => return rp,
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
						return ResolvedPath {
							original: path.clone(),
							kind: ResolvedPathKind::Primitive(
								Ty::from_primitive_name(name)
									.expect("the function before should have filtered this out"),
							),
						};
					}
					self.diagnostics.push(e.build());
					return ResolvedPath {
						original: path.clone(),
						kind: ResolvedPathKind::Resolved(SymbolId(usize::MAX)),
					};
				}
			}
		}
		return match self.resolve_path_full(path, span) {
			Ok(p) => p,
			Err(e) => {
				self.diagnostics.push(e.build());
				ResolvedPath {
					original: path.clone(),
					kind: ResolvedPathKind::Resolved(SymbolId(usize::MAX)),
				}
			}
		};
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
				kind: ResolvedPathKind::AssocItem {
					base,
					member,
					item: SymbolId::DUMMY,
					base_type_args: Vec::new(),
				},
			},
		});
	}

	fn collect_use_directives(&mut self, items: &[TopLevelDecl])
	{
		use crate::parser::Directive;

		for decl in items {
			let TopLevelDecl::Directive(node) = decl else { continue };

			match &node.directive {
				Directive::Use {
					use_path, visibility, ..
				} => {
					let raw: Vec<String> = use_path.segments.iter().map(|s| return s.name.clone()).collect();

					let segments: Vec<String> = if use_path.global || raw.is_empty() {
						raw
					} else if let Some((sym, _)) = self.find_in_scope_chain(self.current_scope, &raw[0])
						&& let Some(intro) = self.global.symbol(sym).introduced_scope
						&& let Some((full, _)) = self.global.module_roots.iter().find(|(_, s)| return **s == intro)
					{
						let mut abs = full.clone();
						abs.extend(raw.into_iter().skip(1));
						abs
					} else if self.global.module_roots.contains_key(raw.as_slice()) {
						raw
					} else if let Some(mut base) = self.current_module_path()
						&& !base.is_empty()
					{
						let mut probe = base.clone();
						probe.extend(raw.iter().cloned());
						if self
							.global
							.module_roots
							.contains_key(&probe[..probe.len().saturating_sub(1)])
							|| self.scope_for_path_prefix(&probe).is_some()
						{
							base.extend(raw);
							base
						} else {
							raw
						}
					} else {
						raw
					};

					let should_validate: bool = use_path.global || segments.len() > 1;
					if should_validate && let Err(e) = self.resolve_absolute_path(&segments, use_path.span(), use_path)
					{
						self.diagnostics.push(e.build());
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
			if let Some(sym_id) = self.find_sym_in_global_scope(root, name) {
				return Some(sym_id);
			}
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
				if let Some(sym_id) = self.find_sym_in_global_scope(scope, seg) {
					if let Some(s) = self.global.symbol(sym_id).introduced_scope {
						scope = s;
					}
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
					let mut start_idx: usize = 0;
					for prefix_len in (1..=target.len()).rev() {
						if let Some(&mod_root) = self.global.module_roots.get(&target[..prefix_len]) {
							scope = mod_root;
							start_idx = prefix_len;
							break;
						}
					}

					let mut ok = true;
					for seg in &target[start_idx..] {
						if let Some(sym_id) = self.find_sym_in_global_scope(scope, seg) {
							if let Some(s) = self.global.symbol(sym_id).introduced_scope {
								scope = s;
							} else {
								ok = false;
								break;
							}
						} else {
							ok = false;
							break;
						}
					}

					if ok {
						if let Some(sym_id) = self
							.global
							.scope(scope)
							.symbols
							.iter()
							.find(|&&id| {
								let sym = self.global.symbol(id);
								sym.name == name && matches!(sym.visibility, Visibility::Public | Visibility::Export)
							})
							.copied()
						{
							return Some(sym_id);
						}
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
						let def_module = self.enclosing_module_scope(sym.scope);
						if sym.visibility == Visibility::Private
							&& !self.is_descendant_of(self.current_scope, def_module)
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

	fn resolve_top_level_block(&mut self, block: &parser::TopLevelBlock) -> ResolvedTopLevelBlock
	{
		self.collect_use_directives(&block.items);

		let mut items: Vec<ResolvedTopLevelDecl> = Vec::new();
		for decl in &block.items {
			items.push(self.resolve_top_level_decl(decl));
		}
		return ResolvedTopLevelBlock {
			items,
			span: block.span,
		};
	}

	fn resolve_top_level_decl(&mut self, decl: &TopLevelDecl) -> ResolvedTopLevelDecl
	{
		return match decl {
			TopLevelDecl::Function(f) => ResolvedTopLevelDecl::Function(self.resolve_function_decl(f)),
			TopLevelDecl::VariableDecl(v) => ResolvedTopLevelDecl::VariableDecl(self.resolve_variable_decl(v)),
			TopLevelDecl::Struct(s) => ResolvedTopLevelDecl::Struct(self.resolve_struct_decl(s)),
			TopLevelDecl::Union(u) => ResolvedTopLevelDecl::Union(self.resolve_union_decl(u)),
			TopLevelDecl::Enum(e) => ResolvedTopLevelDecl::Enum(self.resolve_enum_decl(e)),
			TopLevelDecl::Variant(v) => ResolvedTopLevelDecl::Variant(self.resolve_variant_decl(v)),
			TopLevelDecl::TypeAlias(t) => ResolvedTopLevelDecl::TypeAlias(self.resolve_type_alias_decl(t)),
			TopLevelDecl::Trait(t) => ResolvedTopLevelDecl::Trait(self.resolve_trait_decl(t)),
			TopLevelDecl::Module(m) => ResolvedTopLevelDecl::Module(self.resolve_module_decl(m)),
			TopLevelDecl::Impl(i) => ResolvedTopLevelDecl::Impl(self.resolve_impl_decl(i)),
			TopLevelDecl::Directive(d) => ResolvedTopLevelDecl::Directive(self.resolve_directive_node(d)),
		};
	}

	fn resolve_type(&mut self, ty: &parser::Type) -> ResolvedType
	{
		let core = self.resolve_type_core(ty.core.as_ref(), ty.span);
		return ResolvedType {
			core: Box::new(core),
			span: ty.span,
		};
	}

	fn resolve_type_core(&mut self, core: &TypeCore, span: Span) -> ResolvedTypeCore
	{
		return match core {
			TypeCore::Base { path, generics } => {
				if path.len() == 1 && path.segments[0].name == "Self" {
					return ResolvedTypeCore::Primitive {
						name: "Self".to_string(),
						generics: Vec::new(),
					};
				}
				if !path.global && path.segments.len() == 1 {
					let name = &path.segments[0].name;
					if name != "Self"
						&& let Some(kind_str) = self.find_self_member_kind(name)
					{
						self.diagnostics.push(
							NameResolutionError {
								span,
								kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
								context: vec![format!(
									"`{name}` is a {kind_str} of `Self`; write `Self::{name}` to reference it"
								)],
							}
							.build(),
						);
					}
				}
				if !path.global && path.segments.len() >= 2 && path.segments[0].name == "Self" {
					let resolved_generics = generics.iter().map(|g| return self.resolve_type(g)).collect();
					let member = path.segments[1].name.clone();
					if let Some(self_sym) = self.self_sym {
						return ResolvedTypeCore::Base {
							path: ResolvedPath {
								original: path.clone(),
								kind: ResolvedPathKind::AssocItem {
									base: self_sym,
									member,
									item: SymbolId::DUMMY,
									base_type_args: Vec::new(),
								},
							},
							generics: resolved_generics,
						};
					}
				}
				let resolved_generics: Vec<ResolvedType> =
					generics.iter().map(|g| return self.resolve_type(g)).collect();

				if path.segments.len() > 1 || path.global {
					let rp = self.resolve_path_or_primitive(path, span);
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
				inner: Box::new(self.resolve_type_core(inner, span)),
			},
			TypeCore::Mutable { inner } => ResolvedTypeCore::Mutable {
				inner: Box::new(self.resolve_type_core(inner, span)),
			},
			TypeCore::Pointer { mutable, inner } => ResolvedTypeCore::Pointer {
				mutable: *mutable,
				inner: Box::new(self.resolve_type_core(inner, span)),
			},
			TypeCore::Array { inner, size } => {
				let resolved_inner = self.resolve_type_core(inner, span);
				let resolved_size = size.as_ref().map(|e| return self.resolve_expr(e));
				ResolvedTypeCore::Array {
					inner: Box::new(resolved_inner),
					size: resolved_size.map(Box::new),
				}
			}
			TypeCore::Tuple(types) => {
				ResolvedTypeCore::Tuple(types.iter().map(|t| return self.resolve_type(t)).collect())
			}
			TypeCore::ImplTrait { bounds } => ResolvedTypeCore::ImplTrait {
				bounds: bounds.iter().map(|b| return self.resolve_where_bound(b)).collect(),
			},
		};
	}

	fn resolve_where_constraint(&mut self, constraint: &WhereConstraint) -> ResolvedWhereConstraint
	{
		let bounds = constraint
			.bounds
			.iter()
			.map(|b| return self.resolve_where_bound(b))
			.collect();
		let type_args = constraint
			.type_args
			.iter()
			.map(|t| return self.resolve_type(t))
			.collect();
		return ResolvedWhereConstraint {
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
		};
	}

	fn resolve_where_bound(&mut self, bound: &WhereBound) -> ResolvedWhereBound
	{
		return match bound {
			WhereBound::Path { path, args } => {
				let rp = self.resolve_path_or_primitive(path, path.span());
				let resolved_args = args
					.iter()
					.map(|arg| {
						return match arg {
							GenericArg::Type(ty) => ResolvedGenericArg::Type(self.resolve_type(ty)),
							GenericArg::Binding { name, ty, span } => ResolvedGenericArg::Binding {
								name: name.clone(),
								ty: self.resolve_type(ty),
								span: *span,
							},
						};
					})
					.collect();
				ResolvedWhereBound::Path {
					path: rp,
					args: resolved_args,
				}
			}
			WhereBound::Func(fb) => {
				use parser::FuncBound;
				match fb {
					FuncBound::Fn { args, ret } => {
						let rargs = args.iter().map(|t| return self.resolve_type(t)).collect();
						let rret = ret.as_ref().map(|t| return self.resolve_type(t));
						ResolvedWhereBound::Func(ResolvedFuncBound::Fn { args: rargs, ret: rret })
					}
				}
			}
		};
	}

	fn resolve_pattern(&mut self, pattern: &parser::Pattern) -> ResolvedPattern
	{
		return match pattern {
			parser::Pattern::Wildcard { ty, span } => ResolvedPattern::Wildcard {
				ty: ty.as_ref().map(|t| return self.resolve_type(t)),
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
				let symbol: SymbolId = self.find_in_scope_chain(self.current_scope, &name).map_or_else(
					|| {
						self.diagnostics.push(
							NameResolutionError {
								span: *span,
								kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
								context: Vec::new(),
							}
							.build(),
						);
						return SymbolId(usize::MAX);
					},
					|(id, _)| return id,
				);
				let resolved_ty = self.resolve_type(ty);
				ResolvedPattern::TypedIdentifier {
					symbol,
					name,
					ty: resolved_ty,
					mutable: *mutable,
					span: *span,
				}
			}

			parser::Pattern::Variant { path, args, span } => {
				let rp: ResolvedPath = self.resolve_path_or_primitive(path, *span);
				let rargs: Vec<ResolvedPattern> = args.iter().map(|p| return self.resolve_pattern(p)).collect();
				ResolvedPattern::Variant {
					path: rp,
					args: rargs,
					span: *span,
				}
			}

			parser::Pattern::Tuple { patterns, span } => {
				let rp: Vec<ResolvedPattern> = patterns.iter().map(|p| return self.resolve_pattern(p)).collect();
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
				let rp: ResolvedPath = self.resolve_path_or_primitive(path, *span);
				let rfields: Vec<(String, ResolvedPattern)> = fields
					.iter()
					.map(|(name, pat)| return (name.clone(), self.resolve_pattern(pat)))
					.collect();
				ResolvedPattern::Struct {
					path: rp,
					fields: rfields,
					has_rest: *has_rest,
					span: *span,
				}
			}

			parser::Pattern::Range(re) => ResolvedPattern::Range(self.resolve_range_expr(re)),

			parser::Pattern::Or { patterns, span } => {
				let rp: Vec<ResolvedPattern> = patterns.iter().map(|p| return self.resolve_pattern(p)).collect();
				ResolvedPattern::Or {
					patterns: rp,
					span: *span,
				}
			}
		};
	}

	fn resolve_range_expr(&mut self, re: &RangeExpr) -> ResolvedRangeExpr
	{
		return ResolvedRangeExpr {
			start: re.start.as_ref().map(|e| return self.resolve_expr(e)).map(Box::new),
			end: re.end.as_ref().map(|e| return self.resolve_expr(e)).map(Box::new),
			inclusive: re.inclusive,
			span: re.span,
		};
	}

	fn resolve_expr(&mut self, expr: &parser::Expr) -> ResolvedExpr
	{
		let prev_in_expr = self.in_expr_context;
		self.in_expr_context = true;
		let result = self.resolve_expr_inner(expr);
		self.in_expr_context = prev_in_expr;
		return result;
	}

	fn resolve_expr_inner(&mut self, expr: &parser::Expr) -> ResolvedExpr
	{
		use parser::{ArrayLiteral, Expr};

		return match expr {
			Expr::Identifier { path, span } => {
				if !path.global && path.segments.len() == 1 && path.segments[0].name.starts_with('#') {
					let name: &String = &path.segments[0].name;
					if let Some(intrinsic) = Intrinsic::from_name(name) {
						return ResolvedExpr::InternalCall { intrinsic, span: *span };
					}
				}

				match self.resolve_path_full(path, *span) {
					Ok(rp) => ResolvedExpr::Identifier { path: rp, span: *span },
					Err(e) => {
						if !matches!(e.kind, NameResolutionErrorKind::UnresolvedPath { .. }) {
							self.diagnostics.push(e.build());
						}
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
							return ResolvedExpr::AssocSelf {
								member: path.segments[1].clone(),
								span: path.span(),
							};
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
								return ResolvedExpr::AssocPath {
									base: base_path,
									member: path.segments[1].clone(),
									span: *span,
								};
							}
						}

						if !path.global && path.segments.len() == 1 {
							let name = &path.segments[0].name;
							if let Some(kind_str) = self.find_self_member_kind(name) {
								self.diagnostics.push(
									NameResolutionError {
										span: *span,
										kind: NameResolutionErrorKind::UnresolvedPath { path: path.clone() },
										context: vec![format!(
											"`{name}` is a {kind_str} of `Self`; write `Self::{name}` to reference it"
										)],
									}
									.build(),
								);
							}
						}

						ResolvedExpr::UnresolvedIdentifier {
							path: path.clone(),
							span: *span,
						}
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
				op: *op,
				expr: Box::new(self.resolve_expr(expr)),
				span: *span,
			},

			Expr::Binary { op, lhs, rhs, span } => ResolvedExpr::Binary {
				op: *op,
				lhs: Box::new(self.resolve_expr(lhs)),
				rhs: Box::new(self.resolve_expr(rhs)),
				span: *span,
			},

			Expr::Cast { ty, expr, span } => ResolvedExpr::Cast {
				ty: self.resolve_type(ty),
				expr: Box::new(self.resolve_expr(expr)),
				span: *span,
			},

			Expr::Call {
				callee,
				call_type,
				named_generics,
				args,
				span,
			} => {
				let rcallee: ResolvedExpr = self.resolve_expr(callee);
				let rng: Vec<(String, ResolvedExpr)> = named_generics
					.iter()
					.map(|(name, expr)| return (name.clone(), self.resolve_expr(expr)))
					.collect();
				let rargs = args.iter().map(|a| return self.resolve_expr(a)).collect();
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
					base: Box::new(self.resolve_expr(base)),
					name: field_name,
					span: *span,
				}
			}

			Expr::Index { base, index, span } => ResolvedExpr::Index {
				base: Box::new(self.resolve_expr(base)),
				index: Box::new(self.resolve_expr(index)),
				span: *span,
			},

			Expr::Range(re) => ResolvedExpr::Range(self.resolve_range_expr(re)),

			Expr::Tuple { elements, span } => ResolvedExpr::Tuple {
				elements: elements.iter().map(|e| return self.resolve_expr(e)).collect(),
				span: *span,
			},

			Expr::Array(arr) => ResolvedExpr::Array(match arr {
				ArrayLiteral::List { elements, span } => ResolvedArrayLiteral::List {
					elements: elements.iter().map(|e| return self.resolve_expr(e)).collect(),
					span: *span,
				},
				ArrayLiteral::Repeat { value, count, span } => ResolvedArrayLiteral::Repeat {
					value: Box::new(self.resolve_expr(value)),
					count: Box::new(self.resolve_expr(count)),
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
				let rp: ResolvedPath = self.resolve_path_or_primitive(path, *span);
				let rfields: Vec<(String, ResolvedExpr)> = fields
					.iter()
					.map(|(name, e)| return (name.clone(), self.resolve_expr(e)))
					.collect();
				let rbase = base.as_ref().map(|e| return Box::new(self.resolve_expr(e)));
				ResolvedExpr::StructInit {
					path: rp,
					fields: rfields,
					base: rbase,
					has_rest: *has_rest,
					span: *span,
				}
			}

			Expr::Block(block) => ResolvedExpr::Block(Box::new(self.resolve_scoped_block(block))),

			Expr::UnsafeBlock(block) => ResolvedExpr::UnsafeBlock(Box::new(self.resolve_scoped_block(block))),

			Expr::Switch { expr, arms, span } => {
				let rexpr: ResolvedExpr = self.resolve_expr(expr);
				let mut rarms: Vec<ResolvedSwitchArm> = Vec::new();
				for arm in arms {
					let arm_scope: Option<ScopeId> = self.next_anon_scope();
					let prev: ScopeId = self.current_scope;
					if let Some(sc) = arm_scope {
						self.current_scope = sc;
					}
					let rpat = self.resolve_pattern(&arm.pattern);
					let rbody = match &arm.body {
						parser::SwitchBody::Expr(e) => ResolvedSwitchBody::Expr(self.resolve_expr(e)),
						parser::SwitchBody::Block(b) => {
							let block = self.resolve_block_contents(b);
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
				let rcond: ResolvedExpr = self.resolve_expr(cond);
				let then_scope: Option<ScopeId> = self.next_anon_scope();
				let prev: ScopeId = self.current_scope;
				if let Some(sc) = then_scope {
					self.current_scope = sc;
				}
				let rthen: ResolvedBlock = self.resolve_block_contents(then_block);
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
					let re: ResolvedExpr = self.resolve_expr(e);
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
				let rbody: ResolvedBlock = self.resolve_block_contents(body);
				self.current_scope = prev;
				ResolvedExpr::Loop {
					label: if let Some(l) = label {
						l.clone()
					} else {
						self.diagnostics
							.push(compiler_bug!(*span, "desugarer should have given loop a label"));
						"ERROR".to_string()
					},
					body: Box::new(rbody),
					span: *span,
				}
			}
		};
	}

	fn resolve_block_contents(&mut self, block: &parser::Block) -> ResolvedBlock
	{
		let mut stmts: Vec<ResolvedStmt> = Vec::new();
		for stmt in &block.stmts {
			stmts.push(self.resolve_stmt(stmt));
		}
		let tail: Option<ResolvedExpr> = block.tail_expr.as_ref().map(|e| return self.resolve_expr(e));
		return ResolvedBlock {
			stmts,
			tail_expr: tail.map(Box::new),
			span: block.span,
		};
	}

	fn resolve_scoped_block(&mut self, block: &parser::Block) -> ResolvedBlock
	{
		let prev: ScopeId = self.current_scope;
		let found = self.next_anon_scope();
		if let Some(sc) = found {
			self.current_scope = sc;
		}
		let result: ResolvedBlock = self.resolve_block_contents(block);
		self.current_scope = prev;
		return result;
	}

	fn resolve_function_body(&mut self, block: &parser::Block) -> ResolvedBlock
	{
		return self.resolve_block_contents(block);
	}

	fn resolve_stmt(&mut self, stmt: &parser::Stmt) -> ResolvedStmt
	{
		use parser::Stmt;

		return match stmt {
			Stmt::VariableDecl(var) => ResolvedStmt::VariableDecl(self.resolve_variable_decl(var)),

			Stmt::Assignment {
				target,
				op,
				value,
				span,
			} => ResolvedStmt::Assignment {
				target: self.resolve_expr(target),
				op: *op,
				value: self.resolve_expr(value),
				span: *span,
			},

			Stmt::Return { value, span } => ResolvedStmt::Return {
				value: value.as_ref().map(|e| return self.resolve_expr(e)),
				span: *span,
			},

			Stmt::Expr(e) => ResolvedStmt::Expr(self.resolve_expr(e)),

			Stmt::Break { label, value, span } => {
				let l = if let Some(l) = label.clone() {
					l
				} else {
					self.diagnostics.push(compiler_bug!(
						*span,
						"desugarer should always give labels to Stmt::Break"
					));
					"COMPILER_BUG".to_string()
				};
				ResolvedStmt::Break {
					label: l,
					value: value.as_ref().map(|e| return self.resolve_expr(e)),
					span: *span,
				}
			}

			Stmt::Continue { label, span } => {
				let l = if let Some(l) = label.clone() {
					l
				} else {
					self.diagnostics.push(compiler_bug!(
						*span,
						"desugarer should always give labels to Stmt::Continue"
					));
					"COMPILER_BUG".to_string()
				};
				ResolvedStmt::Continue { label: l, span: *span }
			}

			Stmt::Directive(d) => ResolvedStmt::Directive(self.resolve_directive_node(d)),

			Stmt::If {
				cond,
				then_block,
				else_branch,
				span,
			} => {
				let rcond: ResolvedExpr = self.resolve_expr(cond);

				let then_scope: Option<ScopeId> = self.next_anon_scope();
				let prev: ScopeId = self.current_scope;
				if let Some(sc) = then_scope {
					self.current_scope = sc;
				}
				let rthen: ResolvedBlock = self.resolve_block_contents(then_block);
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
					let rs: ResolvedStmt = self.resolve_stmt(el);
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
				let l = if let Some(l) = label.clone() {
					l
				} else {
					self.diagnostics.push(compiler_bug!(
						*span,
						"desugarer should always give labels to Stmt::Continue"
					));
					"COMPILER_BUG".to_string()
				};
				let loop_scope: Option<ScopeId> = self.next_anon_scope();
				let prev: ScopeId = self.current_scope;
				if let Some(sc) = loop_scope {
					self.current_scope = sc;
				}
				let rbody: ResolvedBlock = self.resolve_block_contents(body);
				self.current_scope = prev;
				ResolvedStmt::Loop {
					label: l,
					body: rbody,
					span: *span,
				}
			}

			Stmt::Delete { expr, span } => ResolvedStmt::Delete {
				expr: self.resolve_expr(expr),
				span: *span,
			},

			Stmt::Unsafe(block) => ResolvedStmt::Unsafe(self.resolve_scoped_block(block)),
			Stmt::Block(block) => ResolvedStmt::Block(self.resolve_scoped_block(block)),
		};
	}

	fn resolve_directive_node(&mut self, node: &parser::DirectiveNode) -> ResolvedDirectiveNode
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
			parser::Directive::MangleName { .. } => {
				todo!("can't be an expression or something?? Check if this indeed will not be hit")
			}
			parser::Directive::Custom { name, params } => ResolvedDirective::Custom {
				name: name.clone(),
				params: params.clone(),
			},
			parser::Directive::ValidateStructPattern {
				struct_path,
				pattern_fields,
				has_rest,
			} => {
				let rp = self.resolve_path_or_primitive(struct_path, struct_path.span());
				ResolvedDirective::ValidateStructPattern {
					struct_path: rp,
					pattern_fields: pattern_fields.clone(),
					has_rest: *has_rest,
				}
			}
			parser::Directive::ValidateType { ty, expr } => ResolvedDirective::ValidateType {
				ty: self.resolve_type(ty),
				expr: self.resolve_expr(expr),
			},
		};

		return ResolvedDirectiveNode {
			directive,
			body: None,
			span: node.span,
		};
	}

	fn resolve_function_decl(&mut self, func: &parser::FunctionDecl) -> ResolvedFunctionDecl
	{
		let sig: &FunctionSignature = &func.signature;
		let name_str: &str = sig
			.name
			.segments
			.first()
			.expect("parser guarantees at least one segment")
			.name
			.as_str();

		let resolved_name: SymbolId = self.find_in_scope_chain(self.current_scope, name_str).map_or_else(
			|| {
				self.diagnostics.push(
					NameResolutionError {
						span: sig.name.span(),
						kind: NameResolutionErrorKind::UnresolvedPath { path: sig.name.clone() },
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			},
			|(id, _)| return id,
		);

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev: ScopeId = self.current_scope;
		self.current_scope = body_scope;

		let mut resolved_params: Vec<ResolvedParam> = Vec::new();
		for param in &sig.params {
			if param.variadic {
				break;
			}
			let (param_name, param_span, param_mutable) = match &param.pattern {
				parser::Pattern::TypedIdentifier {
					path, span, mutable, ..
				} => (path.segments[0].name.clone(), *span, *mutable),
				_ => unreachable!("desugarer guarantees TypedIdentifier for params"),
			};
			let param_sym: SymbolId = self.find_in_scope(body_scope, &param_name).unwrap_or_else(|| {
				self.diagnostics.push(
					NameResolutionError {
						span: param_span,
						kind: NameResolutionErrorKind::UnresolvedPath {
							path: Path::simple(vec![param_name.clone()], param_span),
						},
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			});
			let ty: ResolvedType = self.resolve_type(&param.ty);
			resolved_params.push(ResolvedParam {
				symbol: param_sym,
				name: param_name,
				ty,
				mutable: param_mutable,
				variadic: false,
				span: param_span,
			});
		}

		let return_type: ResolvedType = self.resolve_type(&sig.return_type);
		let where_clause: Vec<ResolvedWhereConstraint> = sig
			.where_clause
			.iter()
			.map(|c| return self.resolve_where_constraint(c))
			.collect();

		let heap_generics: Vec<ResolvedGenericHeapParam> = sig
			.heap_generics
			.iter()
			.map(|hp| {
				let kind = match &hp.kind {
					parser::HeapGenericKind::Forwarded => ResolvedGenericHeapKind::Forwarded,
					parser::HeapGenericKind::Forced(ty) => ResolvedGenericHeapKind::Forced(self.resolve_type(ty)),
				};
				return ResolvedGenericHeapParam {
					name: hp.name.clone(),
					kind,
					span: hp.span,
				};
			})
			.collect();

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

		let body: Option<ResolvedBlock> = func.body.as_ref().map(|b| return self.resolve_function_body(b));

		self.current_scope = prev;

		return ResolvedFunctionDecl {
			resolved_name,
			signature: resolved_sig,
			body,
			docs: func.docs.clone(),
			span: func.span(),
		};
	}

	fn resolve_variable_decl(&mut self, var: &VariableDecl) -> ResolvedVariableDecl
	{
		if let parser::Pattern::Wildcard { ty, span } = &var.pattern {
			let nty = ty.as_ref().map_or_else(
				|| {
					return ResolvedType {
						core: Box::new(ResolvedTypeCore::Primitive {
							name: "_".to_string(),
							generics: Vec::new(),
						}),
						span: *span,
					};
				},
				|t| return self.resolve_type(t),
			);
			let init = var.init.as_ref().map(|e| return self.resolve_expr(e));
			return ResolvedVariableDecl {
				resolved_name: SymbolId(usize::MAX),
				name: "_".to_string(),
				ty: nty,
				init,
				comp_const: var.comp_const,
				mutable: false,
				modifiers: Vec::new(),
				docs: var.docs.clone(),
				span: var.span(),
			};
		}

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
				self.diagnostics.push(
					NameResolutionError {
						span: var_span,
						kind: NameResolutionErrorKind::ShadowedVariable {
							name: name_str.clone(),
							first_definition: self.global.symbol(count[threshold - 1]).def_span,
						},
						context: Vec::new(),
					}
					.build(),
				);
			}

			match self.global.scope(check_scope).parent {
				Some(parent) => check_scope = parent,
				None => break,
			}
		}

		let ty: ResolvedType = match &var.pattern {
			parser::Pattern::TypedIdentifier { ty, .. } => self.resolve_type(ty),
			_ => unreachable!(),
		};

		let modifiers: Vec<parser::Modifier> = match &var.pattern {
			parser::Pattern::TypedIdentifier { modifiers, .. } => modifiers.clone(),
			_ => unreachable!(),
		};

		let init: Option<ResolvedExpr> = var.init.as_ref().map(|e| return self.resolve_expr(e));

		let resolved_name: SymbolId = self.find_in_scope_chain(self.current_scope, &name_str).map_or_else(
			|| {
				self.diagnostics.push(
					NameResolutionError {
						span: var_span,
						kind: NameResolutionErrorKind::UnresolvedPath {
							path: Path::simple(vec![name_str.clone()], var_span),
						},
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			},
			|(id, _)| return id,
		);

		return ResolvedVariableDecl {
			resolved_name,
			name: name_str,
			ty,
			init,
			comp_const: var.comp_const,
			mutable,
			modifiers,
			docs: var.docs.clone(),
			span: var.span(),
		};
	}

	fn resolve_struct_decl(&mut self, s: &StructDecl) -> ResolvedStructDecl
	{
		let name_str: &str = s.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self.find_in_scope_chain(self.current_scope, name_str).map_or_else(
			|| {
				self.diagnostics.push(
					NameResolutionError {
						span: s.name.span(),
						kind: NameResolutionErrorKind::UnresolvedPath { path: s.name.clone() },
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			},
			|(id, _)| return id,
		);

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev = self.current_scope;
		self.current_scope = body_scope;

		let fields: Vec<ResolvedStructField> = s
			.fields
			.iter()
			.map(|f| {
				let ty = self.resolve_type(&f.ty);
				let default_value = f.default_value.as_ref().map(|e| return self.resolve_expr(e));
				return ResolvedStructField {
					name: f.name.clone(),
					ty,
					default_value,
					modifiers: f.modifiers.clone(),
					docs: f.docs.clone(),
					span: f.span(),
				};
			})
			.collect();

		let where_clause: Vec<ResolvedWhereConstraint> = s
			.where_clause
			.iter()
			.map(|c| return self.resolve_where_constraint(c))
			.collect();

		self.current_scope = prev;

		return ResolvedStructDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: s.modifiers.clone(),
			generics: s.generics.clone(),
			fields,
			where_clause,
			docs: s.docs.clone(),
			span: s.span(),
		};
	}

	fn resolve_union_decl(&mut self, u: &UnionDecl) -> ResolvedUnionDecl
	{
		let name_str: &str = u.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self.find_in_scope_chain(self.current_scope, name_str).map_or_else(
			|| {
				self.diagnostics.push(
					NameResolutionError {
						span: u.name.span(),
						kind: NameResolutionErrorKind::UnresolvedPath { path: u.name.clone() },
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			},
			|(id, _)| return id,
		);

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev: ScopeId = self.current_scope;
		self.current_scope = body_scope;

		let fields: Vec<ResolvedUnionField> = u
			.fields
			.iter()
			.map(|f| {
				let ty = self.resolve_type(&f.ty);
				return ResolvedUnionField {
					name: f.name.clone(),
					ty,
					modifiers: f.modifiers.clone(),
					docs: f.docs.clone(),
					span: f.span(),
				};
			})
			.collect();

		let where_clause: Vec<ResolvedWhereConstraint> = u
			.where_clause
			.iter()
			.map(|c| return self.resolve_where_constraint(c))
			.collect();

		self.current_scope = prev;

		return ResolvedUnionDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: u.modifiers.clone(),
			generics: u.generics.clone(),
			fields,
			where_clause,
			docs: u.docs.clone(),
			span: u.span(),
		};
	}

	fn resolve_enum_decl(&mut self, e: &EnumDecl) -> ResolvedEnumDecl
	{
		let name_str: &str = e.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self.find_in_scope_chain(self.current_scope, name_str).map_or_else(
			|| {
				self.diagnostics.push(
					NameResolutionError {
						span: e.name.span(),
						kind: NameResolutionErrorKind::UnresolvedPath { path: e.name.clone() },
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			},
			|(id, _)| return id,
		);

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev: ScopeId = self.current_scope;
		self.current_scope = body_scope;

		let variants: Vec<ResolvedEnumVariant> = e
			.variants
			.iter()
			.map(|v| {
				let value = v.value.as_ref().map(|expr| return self.resolve_expr(expr));
				return ResolvedEnumVariant {
					name: v.name.clone(),
					value,
					docs: v.docs.clone(),
					span: v.span(),
				};
			})
			.collect();

		self.current_scope = prev;

		return ResolvedEnumDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: e.modifiers.clone(),
			generics: e.generics.clone(),
			variants,
			docs: e.docs.clone(),
			span: e.span(),
		};
	}

	fn resolve_variant_decl(&mut self, v: &parser::VariantDecl) -> ResolvedVariantDecl
	{
		let name_str: &str = v.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self.find_in_scope_chain(self.current_scope, name_str).map_or_else(
			|| {
				self.diagnostics.push(
					NameResolutionError {
						span: v.name.span(),
						kind: NameResolutionErrorKind::UnresolvedPath { path: v.name.clone() },
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			},
			|(id, _)| return id,
		);

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev: ScopeId = self.current_scope;
		self.current_scope = body_scope;

		let variants: Vec<ResolvedVariantMember> = v
			.variants
			.iter()
			.map(|m| {
				let ty = m.ty.as_ref().map(|t| return self.resolve_type(t));
				let value = m.value.as_ref().map(|e| return self.resolve_expr(e));
				return ResolvedVariantMember {
					name: m.name.clone(),
					ty,
					value,
					docs: m.docs.clone(),
					span: m.span(),
				};
			})
			.collect();

		self.current_scope = prev;

		return ResolvedVariantDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: v.modifiers.clone(),
			generics: v.generics.clone(),
			variants,
			docs: v.docs.clone(),
			span: v.span(),
		};
	}

	fn resolve_type_alias_decl(&mut self, t: &TypeAliasDecl) -> ResolvedTypeAliasDecl
	{
		let name_str: &str = t.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self.find_in_scope_chain(self.current_scope, name_str).map_or_else(
			|| {
				self.diagnostics.push(
					NameResolutionError {
						span: t.name.span(),
						kind: NameResolutionErrorKind::UnresolvedPath { path: t.name.clone() },
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			},
			|(id, _)| return id,
		);

		let ty: ResolvedType = self.resolve_type(&t.ty);

		return ResolvedTypeAliasDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: t.modifiers.clone(),
			generics: t.generics.clone(),
			ty,
			docs: t.docs.clone(),
			span: t.span(),
		};
	}

	fn resolve_assoc_type_decl(&mut self, t: &AssocTypeDecl) -> ResolvedAssocTypeDecl
	{
		let name_str: &str = t.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self.find_in_scope_chain(self.current_scope, name_str).map_or_else(
			|| {
				self.diagnostics.push(
					NameResolutionError {
						span: t.name.span(),
						kind: NameResolutionErrorKind::UnresolvedPath { path: t.name.clone() },
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			},
			|(id, _)| return id,
		);

		let ty: Option<ResolvedType> = t.ty.as_ref().map(|pty| return self.resolve_type(pty));

		return ResolvedAssocTypeDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: t.modifiers.clone(),
			generics: t.generics.clone(),
			ty,
			docs: t.docs.clone(),
			span: t.span(),
		};
	}

	fn resolve_trait_decl(&mut self, t: &TraitDecl) -> ResolvedTraitDecl
	{
		let name_str: &str = t.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self.find_in_scope_chain(self.current_scope, name_str).map_or_else(
			|| {
				self.diagnostics.push(
					NameResolutionError {
						span: t.name.span(),
						kind: NameResolutionErrorKind::UnresolvedPath { path: t.name.clone() },
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			},
			|(id, _)| return id,
		);

		let body_scope: ScopeId = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
		let prev: ScopeId = self.current_scope;
		self.current_scope = body_scope;

		let prev_trait_scope: Option<ScopeId> = self.trait_scope.replace(body_scope);
		let prev_self_sym: Option<SymbolId> = self.self_sym.replace(resolved_name);

		let super_traits: Vec<ResolvedWhereBound> = t
			.super_traits
			.iter()
			.map(|b| return self.resolve_where_bound(b))
			.collect();

		let items: Vec<ResolvedTraitItem> = t
			.items
			.iter()
			.map(|item| {
				return match item {
					parser::TraitItem::Function(f) => ResolvedTraitItem::Function(self.resolve_function_decl(f)),
					parser::TraitItem::TypeAlias(ta) => ResolvedTraitItem::TypeAlias(self.resolve_type_alias_decl(ta)),
					parser::TraitItem::AssocType(ta) => ResolvedTraitItem::AssocType(self.resolve_assoc_type_decl(ta)),
					parser::TraitItem::Const(var) => ResolvedTraitItem::Const(self.resolve_variable_decl(var)),
				};
			})
			.collect();

		self.current_scope = prev;
		self.trait_scope = prev_trait_scope;
		self.self_sym = prev_self_sym;

		return ResolvedTraitDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: t.modifiers.clone(),
			generics: t.generics.clone(),
			super_traits,
			items,
			docs: t.docs.clone(),
			span: t.span(),
		};
	}

	fn resolve_module_decl(&mut self, m: &ModuleDecl) -> ResolvedModuleDecl
	{
		let name_str: &str = m.name.segments[0].name.as_str();
		let resolved_name: SymbolId = self.find_in_scope_chain(self.current_scope, name_str).map_or_else(
			|| {
				self.diagnostics.push(
					NameResolutionError {
						span: m.name.span(),
						kind: NameResolutionErrorKind::UnresolvedPath { path: m.name.clone() },
						context: Vec::new(),
					}
					.build(),
				);
				return SymbolId(usize::MAX);
			},
			|(id, _)| return id,
		);

		let resolved_body: Option<ResolvedTopLevelBlock> = match &m.kind {
			ModuleKind::Inline(body) => {
				let body_scope = self.find_introduced_scope(resolved_name).unwrap_or(self.current_scope);
				let saved_imports: Vec<UseImport> = std::mem::take(&mut self.use_imports);
				let prev: ScopeId = self.current_scope;
				self.current_scope = body_scope;
				let resolved: ResolvedTopLevelBlock = self.resolve_top_level_block(body);
				self.current_scope = prev;
				self.use_imports = saved_imports;
				Some(resolved)
			}
			ModuleKind::External => None,
		};

		return ResolvedModuleDecl {
			resolved_name,
			name: name_str.to_owned(),
			modifiers: m.modifiers.clone(),
			resolved_body,
			docs: m.docs.clone(),
			span: m.span(),
		};
	}

	fn resolve_impl_decl(&mut self, i: &ImplDecl) -> ResolvedImplDecl
	{
		let body_scope: Option<ScopeId> = self.next_anon_scope();
		let prev: ScopeId = self.current_scope;
		if let Some(sc) = body_scope {
			self.current_scope = sc;
		}

		let resolved_target: ResolvedPath = self.resolve_path_or_primitive(&i.target.path, i.target.span());
		let resolved_trait: Option<ResolvedPath> = i
			.trait_path
			.as_ref()
			.map(|tp| return self.resolve_path_or_primitive(&tp.path, tp.span()));

		let prev_self_sym: Option<SymbolId> = match &resolved_target.kind {
			ResolvedPathKind::Resolved(id) => self.self_sym.replace(*id),
			ResolvedPathKind::AssocItem { base, .. } => self.self_sym.replace(*base),
			ResolvedPathKind::Primitive(_) => self.self_sym.take(),
		};

		let prev_trait_scope: Option<ScopeId> = self.trait_scope.replace(body_scope.unwrap_or(prev));

		let where_clause: Vec<ResolvedWhereConstraint> = i
			.where_clause
			.iter()
			.map(|c| return self.resolve_where_constraint(c))
			.collect();

		let items: Vec<ResolvedImplItem> = i
			.body
			.iter()
			.map(|item| {
				return match item {
					parser::ImplItem::Function(f) => ResolvedImplItem::Function(self.resolve_function_decl(f)),
					parser::ImplItem::TypeAlias(ta) => ResolvedImplItem::TypeAlias(self.resolve_type_alias_decl(ta)),
					parser::ImplItem::AssocType(ta) => ResolvedImplItem::AssocType(self.resolve_assoc_type_decl(ta)),
					parser::ImplItem::Const(var) => ResolvedImplItem::Const(self.resolve_variable_decl(var)),
				};
			})
			.collect();

		self.current_scope = prev;
		self.self_sym = prev_self_sym;
		self.trait_scope = prev_trait_scope;

		return ResolvedImplDecl {
			resolved_target,
			resolved_trait,
			modifiers: i.modifiers.clone(),
			generics: i.generics.clone(),
			where_clause,
			items,
			docs: i.docs.clone(),
			span: i.span(),
		};
	}
}

pub fn resolve_names(
	logical_path: &[String],
	ast: &DesugaredAST,
	symbols: &LocalSymbolTable,
	global: &GlobalSymbolTable,
	modules: &[(Vec<String>, DesugaredAST, LocalSymbolTable)],
) -> Result<(ResolvedModule, Vec<DiagnosticBuilder>), Vec<DiagnosticBuilder>>
{
	let scope_offset: usize = global.module_roots.get(logical_path).copied().map_or(0, |s| return s.0);

	let mut resolver: Resolver<'_> = Resolver::new(global, modules, symbols, scope_offset);

	let resolved_block: ResolvedTopLevelBlock = resolver.resolve_top_level_block(&ast.top_level_block);

	let span: Span = resolved_block.span;

	if resolver
		.diagnostics
		.iter()
		.any(|d| return d.severity.should_stop_compiling())
	{
		return Err(resolver.diagnostics);
	}

	return Ok((
		ResolvedModule {
			path: logical_path.to_vec(),
			ast: ResolvedAST {
				span,
				top_level_block: resolved_block,
				source_index: ast.source_index,
			},
			symbols: symbols.clone(),
		},
		resolver.diagnostics,
	));
}
