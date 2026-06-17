use std::fmt;

use crate::{
	name_resolution::{
		CallType, ResolvedArrayLiteral, ResolvedAssocTypeDecl, ResolvedBlock, ResolvedDirective, ResolvedDirectiveNode,
		ResolvedEnumDecl, ResolvedEnumVariant, ResolvedExpr, ResolvedFuncBound, ResolvedFunctionDecl,
		ResolvedFunctionSignature, ResolvedGenericArg, ResolvedGenericHeapKind, ResolvedGenericHeapParam, ResolvedHIR,
		ResolvedImplDecl, ResolvedImplItem, ResolvedModuleDecl, ResolvedParam, ResolvedPath, ResolvedPathKind,
		ResolvedPattern, ResolvedRangeExpr, ResolvedStmt, ResolvedStructDecl, ResolvedStructField, ResolvedSwitchArm,
		ResolvedSwitchBody, ResolvedTopLevelBlock, ResolvedTopLevelDecl, ResolvedTraitDecl, ResolvedTraitItem,
		ResolvedType, ResolvedTypeAliasDecl, ResolvedTypeCore, ResolvedUnionDecl, ResolvedUnionField,
		ResolvedVariableDecl, ResolvedVariantDecl, ResolvedVariantMember, ResolvedWhereBound, ResolvedWhereConstraint,
	},
	parser::display::write_docs,
	utils::indent_writer::IndentWriter,
};

impl fmt::Display for ResolvedHIR
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
			ResolvedPathKind::AssocItem { base, member, .. } => {
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
			write!(f, " '{label}")?;
			if let Some(v) = value {
				write!(f, " ")?;
				write_resolved_expr(f, w, v)?;
			}
			return write!(f, ";");
		}
		ResolvedStmt::Continue { label, .. } => {
			write!(f, "continue")?;
			write!(f, " '{label}")?;
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
			write!(f, " '{label}")?;
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
			write!(f, "'{}: ", label)?;
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
