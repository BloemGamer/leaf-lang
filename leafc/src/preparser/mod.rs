use leaf_proc::Spanned;

use crate::{
	diagnostics::Diagnostic,
	lexer::LexerSlice,
	util::span::{Span, SpanNoEq, Spanned},
};

pub mod parse;

pub struct PreParsedUnitResult<'s>
{
	pub pre_parsed_unit: PreParsedUnit<'s>,
	pub diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct PreParsedUnit<'s>
{
	pub span: SpanNoEq,

	pub docs: Docs<'s>,
	pub attributes: Vec<Attribute<'s>>,

	pub modules: Vec<ModuleDecl<'s>>,
	pub types: Vec<TypeDecl<'s>>,
	pub interfaces: Vec<InterfaceDecl<'s>>,
	pub variables: Vec<VariableDecl<'s>>,
	pub functions: Vec<FunctionDecl<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct DeclMeta<'s>
{
	pub span: SpanNoEq,

	pub docs: Docs<'s>,
	pub attributes: Vec<Attribute<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct Docs<'s>
{
	pub docs: Vec<&'s str>,

	/// span will be [`Span::DUMMY`] if `docs.is_empty()`
	pub span: SpanNoEq,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct Attribute<'s>
{
	pub span: SpanNoEq,

	pub path: Path<'s>,
	pub args: Vec<AttributeArg<'s>>,
}

#[derive(Clone, PartialEq)]
pub enum AttributeArg<'s>
{
	Path(Path<'s>),
	String(&'s str),
	Integer(&'s str),
	Bool(bool),

	Expression(LexerSlice<'s>),
}

#[derive(Clone, PartialEq, Spanned)]
pub struct ModuleDecl<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub unit: PreParsedUnit<'s>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct FunctionDecl<'s>
{
	pub span: SpanNoEq,

	pub signature: FunctionSignature<'s>,

	pub body: LexerSlice<'s>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct FunctionSignature<'s>
{
	pub meta: DeclMeta<'s>,

	pub span: SpanNoEq,

	pub name: &'s str,

	pub generics: Vec<GenericParam<'s>>,

	pub effects: Vec<Effect<'s>>,

	pub params: Vec<Param<'s>>,

	pub return_type: Type<'s>,

	pub where_clauses: Vec<WhereClause<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct Param<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: Option<&'s str>,

	pub ty: Type<'s>,

	pub default: Option<LexerSlice<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct Effect<'s>
{
	pub span: SpanNoEq,

	pub path: EffectKind<'s>,
}

#[derive(Clone, PartialEq)]
pub enum EffectKind<'s>
{
	/// [IO]
	IO,
	/// [Global}
	Global,
	/// [FFI]
	FFI,

	/// [_]
	Ignore,
	/// [?ident]
	Generic(&'s str),
	/// [?]
	GenericImply,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct GenericParam<'s>
{
	pub span: SpanNoEq,

	pub name: &'s str,

	pub bounds: Vec<TypeBound<'s>>,

	pub default: Option<Type<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct TypeBound<'s>
{
	pub span: SpanNoEq,

	pub path: Path<'s>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct WhereClause<'s>
{
	pub span: SpanNoEq,

	pub ty: Type<'s>,

	pub bounds: Vec<TypeBound<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct Type<'s>
{
	pub span: SpanNoEq,

	pub kind: TypeKind<'s>,
}

#[derive(Clone, PartialEq)]
pub enum TypeKind<'s>
{
	Path(Path<'s>),

	Applied
	{
		path: Path<'s>,
		arguments: Vec<Type<'s>>,
	},

	SelfType,

	Unit,

	Tuple(Vec<Type<'s>>),

	Array
	{
		element: Box<Type<'s>>,
		size: LexerSlice<'s>,
	},

	Slice(Box<Type<'s>>),

	Reference
	{
		lifetime: Option<Lifetime<'s>>,
		mutable: bool,
		ty: Box<Type<'s>>,
	},

	Pointer
	{
		mutable: bool,
		ty: Box<Type<'s>>,
	},

	Function
	{
		params: Vec<Type<'s>>,
		return_type: Box<Type<'s>>,
	},

	Impl
	{
		bounds: Vec<TypeBound<'s>>,
	},
}

#[derive(Clone, PartialEq, Spanned)]
pub struct Lifetime<'s>
{
	pub span: SpanNoEq,

	pub name: &'s str,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct Path<'s>
{
	pub span: SpanNoEq,

	pub segments: Vec<PathSegment<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct PathSegment<'s>
{
	pub span: SpanNoEq,

	pub name: &'s str,

	pub generic_arguments: Vec<Type<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct TypeDecl<'s>
{
	pub span: SpanNoEq,

	pub kind: TypeDeclKind<'s>,
}

#[derive(Clone, PartialEq)]
pub enum TypeDeclKind<'s>
{
	Alias(AliasType<'s>),

	Variant(VariantType<'s>),

	Enum(EnumType<'s>),

	Struct(StructType<'s>),

	Union(UnionType<'s>),
}

#[derive(Clone, PartialEq, Spanned)]
pub struct AliasType<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub generics: Vec<GenericParam<'s>>,

	pub ty: Type<'s>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct VariantType<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub generics: Vec<GenericParam<'s>>,

	pub variants: Vec<Variant<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct Variant<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub fields: VariantFields<'s>,
}

#[derive(Clone, PartialEq)]
pub enum VariantFields<'s>
{
	Unit,

	Tuple(Vec<VariantField<'s>>),

	Struct(Vec<Field<'s>>),
}

#[derive(Clone, PartialEq, Spanned)]
pub struct VariantField<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub ty: Type<'s>,

	pub discriminant: Option<LexerSlice<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct EnumType<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub generics: Vec<GenericParam<'s>>,

	pub variants: Vec<EnumVariant<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct EnumVariant<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub discriminant: Option<LexerSlice<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct StructType<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub generics: Vec<GenericParam<'s>>,

	pub fields: Vec<Field<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct Field<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub ty: Type<'s>,

	pub default: Option<LexerSlice<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct UnionType<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub generics: Vec<GenericParam<'s>>,

	pub fields: Vec<UnionField<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct UnionField<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub ty: Type<'s>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct InterfaceDecl<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub generics: Vec<GenericParam<'s>>,

	pub types: Vec<AssociatedType<'s>>,

	pub functions: Vec<FunctionDecl<'s>>,

	pub where_clauses: Vec<WhereClause<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct AssociatedType<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub bounds: Vec<TypeBound<'s>>,

	pub default: Option<Type<'s>>,
}

#[derive(Clone, PartialEq, Spanned)]
pub struct VariableDecl<'s>
{
	pub span: SpanNoEq,

	pub meta: DeclMeta<'s>,

	pub name: &'s str,

	pub mutable: bool,

	pub ty: Option<Type<'s>>,

	pub initializer: LexerSlice<'s>,
}
