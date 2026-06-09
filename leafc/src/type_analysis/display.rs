use std::fmt;

use crate::{
	lexer::{IntSign, IntSize, IntType},
	parser::display::write_docs,
	type_analysis::{
		Primitive, Ty, TyBound, TyDisplay, TyGenericArg, TypedAST, TypedArrayLiteral, TypedAssocTypeDecl, TypedBlock,
		TypedDirective, TypedDirectiveNode, TypedEnumDecl, TypedEnumVariant, TypedExpr, TypedExprKind,
		TypedFunctionDecl, TypedFunctionSignature, TypedImplDecl, TypedImplItem, TypedModuleDecl, TypedParam,
		TypedPattern, TypedRangeExpr, TypedStmt, TypedStructDecl, TypedStructField, TypedSwitchArm, TypedSwitchBody,
		TypedTopLevelBlock, TypedTopLevelDecl, TypedTraitDecl, TypedTraitItem, TypedTypeAliasDecl, TypedUnionDecl,
		TypedUnionField, TypedVariableDecl, TypedVariantDecl, TypedVariantMember, TypedWhereConstraint,
	},
	utils::indent_writer::IndentWriter,
};

impl fmt::Display for TyDisplay<'_>
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return self.fmt_ty(f, self.ty);
	}
}

impl fmt::Display for TyGenericArg
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			TyGenericArg::Type(ty) => return write!(f, "{ty}"),
			TyGenericArg::Binding { name, ty } => return write!(f, "{name} = {ty}"),
		}
	}
}

impl fmt::Display for TyBound
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			TyBound::Trait { symbol, args } => {
				write!(f, "#{}", symbol.0)?;
				if !args.is_empty() {
					write!(f, "<")?;
					for (i, a) in args.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{a}")?;
					}
					write!(f, ">")?;
				}
				return Ok(());
			}
			TyBound::Fn { args, ret } => {
				write!(f, "Fn(")?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{a}")?;
				}
				return write!(f, ") -> {ret}");
			}
		}
	}
}

impl fmt::Display for Primitive
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			Primitive::Bool => write!(f, "bool"),
			Primitive::Char => write!(f, "char"),
			Primitive::Int(IntType {
				sign: IntSign::Signed,
				bits: IntSize::Fixed(n),
			}) => write!(f, "i{n}"),
			Primitive::Int(IntType {
				sign: IntSign::Unsigned,
				bits: IntSize::Fixed(n),
			}) => write!(f, "u{n}"),
			Primitive::Int(IntType {
				sign: IntSign::Signed,
				bits: IntSize::Size,
			}) => write!(f, "isize"),
			Primitive::Int(IntType {
				sign: IntSign::Unsigned,
				bits: IntSize::Size,
			}) => write!(f, "usize"),
			Primitive::F32 => write!(f, "f32"),
			Primitive::F64 => write!(f, "f64"),
			Primitive::Str => write!(f, "str"),
			Primitive::CStr => write!(f, "cstr"),
		};
	}
}
impl fmt::Display for Ty
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			Ty::Primitive(p) => return write!(f, "{}", p),
			Ty::Unit => return write!(f, "()"),
			Ty::Never => return write!(f, "!"),
			Ty::Infer => return write!(f, "_"),
			Ty::Named { symbol, generics } => {
				write!(f, "#{}", symbol.0)?;
				if !generics.is_empty() {
					write!(f, "<")?;
					for (i, g) in generics.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{g}")?;
					}
					write!(f, ">")?;
				}
				return Ok(());
			}
			Ty::Reference { mutable, inner } => {
				write!(f, "&")?;
				if *mutable {
					write!(f, "mut ")?;
				}
				return write!(f, "{inner}");
			}
			Ty::Mutable { inner } => return write!(f, "mut {inner}"),
			Ty::Pointer { mutable, inner } => {
				write!(f, "*")?;
				if *mutable {
					write!(f, "mut ")?;
				}
				return write!(f, "{inner}");
			}

			Ty::Array { inner, size } => {
				write!(f, "[{inner}")?;
				if let Some(n) = size {
					write!(f, "; {n}")?;
				}
				return write!(f, "]");
			}
			Ty::Tuple(ts) => {
				write!(f, "(")?;
				for (i, t) in ts.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{t}")?;
				}
				return write!(f, ")");
			}
			Ty::Generic { name, bounds } => {
				write!(f, "{name}")?;
				if !bounds.is_empty() {
					write!(f, ": ")?;
					for (i, b) in bounds.iter().enumerate() {
						if i > 0 {
							write!(f, " + ")?;
						}
						write!(f, "{b}")?;
					}
				}
				return Ok(());
			}
			Ty::ImplTrait { bounds, .. } => {
				write!(f, "impl ")?;
				for (i, b) in bounds.iter().enumerate() {
					if i > 0 {
						write!(f, " + ")?;
					}
					write!(f, "{b}")?;
				}
				return Ok(());
			}
			Ty::SelfTy => return write!(f, "Self"),
		}
	}
}

impl fmt::Display for TypedAST
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "{}", self.top_level_block);
	}
}

impl fmt::Display for TypedTopLevelBlock
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		let mut w = IndentWriter::new();
		for item in &self.items {
			write_typed_top_level_decl(f, &mut w, item)?;
			writeln!(f)?;
		}
		return Ok(());
	}
}

pub fn write_typed_top_level_decl(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	decl: &TypedTopLevelDecl,
) -> fmt::Result
{
	match decl {
		TypedTopLevelDecl::Function(func) => return write_typed_function_decl(f, w, func),
		TypedTopLevelDecl::VariableDecl(var) => {
			write_typed_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		TypedTopLevelDecl::Struct(s) => return write_typed_struct_decl(f, w, s),
		TypedTopLevelDecl::Union(u) => return write_typed_union_decl(f, w, u),
		TypedTopLevelDecl::Enum(e) => return write_typed_enum_decl(f, w, e),
		TypedTopLevelDecl::Variant(v) => return write_typed_variant_decl(f, w, v),
		TypedTopLevelDecl::TypeAlias(t) => {
			write_typed_type_alias_decl(f, w, t)?;
			return write!(f, ";");
		}
		TypedTopLevelDecl::Trait(t) => return write_typed_trait_decl(f, w, t),
		TypedTopLevelDecl::Module(m) => return write_typed_module_decl(f, w, m),
		TypedTopLevelDecl::Impl(i) => return write_typed_impl_decl(f, w, i),
		TypedTopLevelDecl::Directive(d) => {
			write_typed_directive_node(f, w, d)?;
			if d.body.is_none() {
				write!(f, ";")?;
			}
			return Ok(());
		}
	}
}

pub fn write_typed_function_decl(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	func: &TypedFunctionDecl,
) -> fmt::Result
{
	write_docs(f, w, &func.docs)?;
	write_typed_function_signature(f, w, &func.signature)?;
	if let Some(body) = &func.body {
		write!(f, " ")?;
		write_typed_block(f, w, body)?;
	} else {
		write!(f, ";")?;
	}
	return Ok(());
}

pub fn write_typed_function_signature(
	f: &mut fmt::Formatter<'_>,
	_w: &mut IndentWriter,
	sig: &TypedFunctionSignature,
) -> fmt::Result
{
	for m in &sig.modifiers {
		write!(f, "{m} ")?;
	}
	write!(f, "fn")?;
	match sig.call_type {
		crate::parser::CallType::UserHeap => write!(f, "!")?,
		crate::parser::CallType::UserMaybeHeap | crate::parser::CallType::CompilerHeap => write!(f, "?")?,
		crate::parser::CallType::Regular => {}
	}
	if !sig.heap_generics.is_empty() {
		write!(f, "<")?;
		for (i, g) in sig.heap_generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{g}")?;
		}
		write!(f, ">")?;
	}
	write!(f, " {} /* #{} */", sig.name, sig.resolved_name.0)?;
	if !sig.generics.is_empty() {
		write!(f, "<")?;
		for (i, (name, _)) in sig.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{name}")?;
		}
		write!(f, ">")?;
	}
	write!(f, "(")?;
	for (i, param) in sig.params.iter().enumerate() {
		if i > 0 {
			write!(f, ", ")?;
		}
		write_typed_param(f, param)?;
	}
	write!(f, ") -> {}", sig.return_type)?;
	if !sig.where_clause.is_empty() {
		write!(f, " where ")?;
		for (i, c) in sig.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{c}")?;
		}
	}
	return Ok(());
}

fn write_typed_param(f: &mut fmt::Formatter<'_>, param: &TypedParam) -> fmt::Result
{
	if param.variadic {
		return write!(f, "...");
	}
	if param.mutable {
		write!(f, "mut ")?;
	}
	return write!(f, "{} /* #{} */: {}", param.name, param.symbol.0, param.ty);
}

pub fn write_typed_variable_decl(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	var: &TypedVariableDecl,
) -> fmt::Result
{
	write_docs(f, w, &var.docs)?;
	if var.comp_const {
		write!(f, "const ")?;
	} else {
		write!(f, "var ")?;
	}
	if var.mutable {
		write!(f, "mut ")?;
	}
	write!(f, "{} /* #{} */: {}", var.name, var.resolved_name.0, var.ty)?;
	if let Some(init) = &var.init {
		write!(f, " = ")?;
		write_typed_expr(f, w, init)?;
	}
	return Ok(());
}

pub fn write_typed_struct_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, s: &TypedStructDecl) -> fmt::Result
{
	write_docs(f, w, &s.docs)?;
	for m in &s.modifiers {
		write!(f, "{m} ")?;
	}
	write!(f, "struct {} /* #{} */", s.name, s.resolved_name.0)?;
	write_generic_params_typed(f, &s.generics)?;
	write_typed_where_clause(f, &s.where_clause)?;
	writeln!(f, " {{")?;
	w.indent();
	for field in &s.fields {
		write_typed_struct_field(f, w, field)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_typed_struct_field(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, field: &TypedStructField) -> fmt::Result
{
	write_docs(f, w, &field.docs)?;
	w.write_indent(f)?;
	write!(f, "{}: {}", field.name, field.ty)?;
	if let Some(dv) = &field.default_value {
		write!(f, " = ")?;
		write_typed_expr(f, w, dv)?;
	}
	return writeln!(f, ",");
}

pub fn write_typed_union_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, u: &TypedUnionDecl) -> fmt::Result
{
	write_docs(f, w, &u.docs)?;
	for m in &u.modifiers {
		write!(f, "{m} ")?;
	}
	write!(f, "union {} /* #{} */", u.name, u.resolved_name.0)?;
	write_generic_params_typed(f, &u.generics)?;
	write_typed_where_clause(f, &u.where_clause)?;
	writeln!(f, " {{")?;
	w.indent();
	for field in &u.fields {
		write_typed_union_field(f, w, field)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_typed_union_field(f: &mut fmt::Formatter<'_>, w: &IndentWriter, field: &TypedUnionField) -> fmt::Result
{
	write_docs(f, w, &field.docs)?;
	w.write_indent(f)?;
	return writeln!(f, "{}: {},", field.name, field.ty);
}

pub fn write_typed_enum_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, e: &TypedEnumDecl) -> fmt::Result
{
	write_docs(f, w, &e.docs)?;
	for m in &e.modifiers {
		write!(f, "{m} ")?;
	}
	write!(f, "enum {} /* #{} */", e.name, e.resolved_name.0)?;
	write_generic_params_typed(f, &e.generics)?;
	writeln!(f, " {{")?;
	w.indent();
	for variant in &e.variants {
		write_typed_enum_variant(f, w, variant)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_typed_enum_variant(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, v: &TypedEnumVariant) -> fmt::Result
{
	write_docs(f, w, &v.docs)?;
	w.write_indent(f)?;
	write!(f, "{}", v.name)?;
	if let Some(val) = &v.value {
		write!(f, " = ")?;
		write_typed_expr(f, w, val)?;
	}
	return writeln!(f, ",");
}

pub fn write_typed_variant_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, v: &TypedVariantDecl) -> fmt::Result
{
	write_docs(f, w, &v.docs)?;
	for m in &v.modifiers {
		write!(f, "{m} ")?;
	}
	write!(f, "variant {} /* #{} */", v.name, v.resolved_name.0)?;
	write_generic_params_typed(f, &v.generics)?;
	writeln!(f, " {{")?;
	w.indent();
	for member in &v.variants {
		write_typed_variant_member(f, w, member)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

fn write_typed_variant_member(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, m: &TypedVariantMember) -> fmt::Result
{
	write_docs(f, w, &m.docs)?;
	w.write_indent(f)?;
	write!(f, "{}", m.name)?;
	if let Some(ty) = &m.ty {
		write!(f, "({ty})")?;
	}
	if let Some(val) = &m.value {
		write!(f, " = ")?;
		write_typed_expr(f, w, val)?;
	}
	return writeln!(f, ",");
}

pub fn write_typed_type_alias_decl(f: &mut fmt::Formatter<'_>, w: &IndentWriter, t: &TypedTypeAliasDecl)
-> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for m in &t.modifiers {
		write!(f, "{m} ")?;
	}
	write!(f, "type {} /* #{} */", t.name, t.resolved_name.0)?;
	write_generic_params_typed(f, &t.generics)?;
	return write!(f, " = {}", t.ty);
}

pub fn write_typed_assoc_type_decl(f: &mut fmt::Formatter<'_>, w: &IndentWriter, t: &TypedAssocTypeDecl)
-> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for m in &t.modifiers {
		write!(f, "{m} ")?;
	}
	write!(f, "type {} /* #{} */", t.name, t.resolved_name.0)?;
	write_generic_params_typed(f, &t.generics)?;
	if let Some(ty) = &t.ty {
		write!(f, " = {ty}")?;
	}
	return Ok(());
}

pub fn write_typed_trait_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, t: &TypedTraitDecl) -> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for m in &t.modifiers {
		write!(f, "{m} ")?;
	}
	write!(f, "trait {} /* #{} */", t.name, t.resolved_name.0)?;
	write_generic_params_typed(f, &t.generics)?;
	if !t.super_traits.is_empty() {
		write!(f, ": ")?;
		for (i, st) in t.super_traits.iter().enumerate() {
			if i > 0 {
				write!(f, " + ")?;
			}
			write!(f, "{st}")?;
		}
	}
	writeln!(f, " {{")?;
	w.indent();
	for item in &t.items {
		w.write_indent(f)?;
		write_typed_trait_item(f, w, item)?;
		writeln!(f)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_typed_trait_item(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, item: &TypedTraitItem) -> fmt::Result
{
	match item {
		TypedTraitItem::Function(func) => return write_typed_function_decl(f, w, func),
		TypedTraitItem::TypeAlias(ta) => {
			write_typed_type_alias_decl(f, w, ta)?;
			return write!(f, ";");
		}
		TypedTraitItem::AssocType(ta) => {
			write_typed_assoc_type_decl(f, w, ta)?;
			return write!(f, ";");
		}
		TypedTraitItem::Const(var) => {
			write_typed_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
	}
}

pub fn write_typed_module_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, m: &TypedModuleDecl) -> fmt::Result
{
	write_docs(f, w, &m.docs)?;
	for modifier in &m.modifiers {
		write!(f, "{modifier} ")?;
	}
	write!(f, "module {} /* #{} */", m.name, m.resolved_name.0)?;
	if let Some(body) = &m.resolved_body {
		writeln!(f, " {{")?;
		w.indent();
		for item in &body.items {
			w.write_indent(f)?;
			write_typed_top_level_decl(f, w, item)?;
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

pub fn write_typed_impl_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, i: &TypedImplDecl) -> fmt::Result
{
	write_docs(f, w, &i.docs)?;
	for m in &i.modifiers {
		write!(f, "{m} ")?;
	}
	write!(f, "impl")?;
	write_generic_params_typed(f, &i.generics)?;
	if let Some(tr) = &i.resolved_trait {
		write!(f, " {tr} for")?;
	}
	write!(f, " {}", i.resolved_target)?;
	write_typed_where_clause(f, &i.where_clause)?;
	writeln!(f, " {{")?;
	w.indent();
	for item in &i.items {
		w.write_indent(f)?;
		write_typed_impl_item(f, w, item)?;
		writeln!(f)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_typed_impl_item(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, item: &TypedImplItem) -> fmt::Result
{
	match item {
		TypedImplItem::Function(func) => return write_typed_function_decl(f, w, func),
		TypedImplItem::TypeAlias(ta) => {
			write_typed_type_alias_decl(f, w, ta)?;
			return write!(f, ";");
		}
		TypedImplItem::AssocType(ta) => {
			write_typed_assoc_type_decl(f, w, ta)?;
			return write!(f, ";");
		}
		TypedImplItem::Const(var) => {
			write_typed_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
	}
}

pub fn write_typed_block(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, block: &TypedBlock) -> fmt::Result
{
	writeln!(f, "{{ /* : {} */", block.ty)?;
	w.indent();
	for stmt in &block.stmts {
		write_typed_stmt(f, w, stmt)?;
		writeln!(f)?;
	}
	if let Some(tail) = &block.tail_expr {
		w.write_indent(f)?;
		write_typed_expr(f, w, tail)?;
		writeln!(f, " /* : {} */", tail.ty)?;
	}
	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_typed_stmt(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, stmt: &TypedStmt) -> fmt::Result
{
	w.write_indent(f)?;
	return write_typed_stmt_no_indent(f, w, stmt);
}

pub fn write_typed_stmt_no_indent(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, stmt: &TypedStmt) -> fmt::Result
{
	match stmt {
		TypedStmt::VariableDecl(var) => {
			write_typed_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		TypedStmt::Assignment { target, op, value, .. } => {
			write_typed_expr(f, w, target)?;
			write!(f, " {op} ")?;
			write_typed_expr(f, w, value)?;
			return write!(f, ";");
		}
		TypedStmt::Return { value, .. } => {
			write!(f, "return")?;
			if let Some(v) = value {
				write!(f, " ")?;
				write_typed_expr(f, w, v)?;
				write!(f, " /* : {} */", v.ty)?;
			}
			return write!(f, ";");
		}
		TypedStmt::Expr(expr) => {
			write_typed_expr(f, w, expr)?;
			return write!(f, " /* : {} */;", expr.ty);
		}
		TypedStmt::Break { label, value, .. } => {
			write!(f, "break")?;
			write!(f, " '{label}")?;
			if let Some(v) = value {
				write!(f, " ")?;
				write_typed_expr(f, w, v)?;
				write!(f, " /* : {} */", v.ty)?;
			}
			return write!(f, ";");
		}
		TypedStmt::Continue { label, .. } => {
			write!(f, "continue")?;
			write!(f, " '{label}")?;
			return write!(f, ";");
		}
		TypedStmt::If {
			cond,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if ")?;
			write_typed_expr(f, w, cond)?;
			write!(f, " ")?;
			write_typed_block(f, w, then_block)?;
			if let Some(else_stmt) = else_branch {
				write!(f, " else ")?;
				write_typed_stmt_no_indent(f, w, else_stmt)?;
			}
			return Ok(());
		}
		TypedStmt::Loop { label, body, .. } => {
			write!(f, " '{label}")?;
			write!(f, "loop ")?;
			return write_typed_block(f, w, body);
		}
		TypedStmt::Delete { expr, .. } => {
			write!(f, "delete ")?;
			write_typed_expr(f, w, expr)?;
			return write!(f, ";");
		}
		TypedStmt::Unsafe(block) => {
			write!(f, "unsafe ")?;
			return write_typed_block(f, w, block);
		}
		TypedStmt::Block(block) => return write_typed_block(f, w, block),
		TypedStmt::Directive(d) => {
			write_typed_directive_node(f, w, d)?;
			if d.body.is_none() {
				write!(f, ";")?;
			}
			return Ok(());
		}
		TypedStmt::Pending(_) => return write!(f, "<TYPED-STMT-PENDING>"),
	}
}

pub fn write_typed_expr(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, expr: &TypedExpr) -> fmt::Result
{
	return write_typed_expr_kind(f, w, &expr.kind);
}

fn write_typed_expr_kind(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, kind: &TypedExprKind) -> fmt::Result
{
	match kind {
		TypedExprKind::Identifier { path } => return write!(f, "{path}"),
		TypedExprKind::Literal { value } => return write!(f, "{value}"),
		TypedExprKind::Default { heap_call } => {
			write!(f, "default")?;
			match heap_call {
				crate::parser::CallType::Regular => return write!(f, "()"),
				crate::parser::CallType::UserHeap => return write!(f, "!()"),
				crate::parser::CallType::UserMaybeHeap | crate::parser::CallType::CompilerHeap => {
					return write!(f, "?()");
				}
			}
		}
		TypedExprKind::Unary { op, expr } => {
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
			write_typed_expr(f, w, expr)?;
			return write!(f, " /* : {} */", expr.ty);
		}
		TypedExprKind::Binary { op, lhs, rhs } => {
			write!(f, "(")?;
			write_typed_expr(f, w, lhs)?;
			write!(f, " /* : {} */ {op} ", lhs.ty)?;
			write_typed_expr(f, w, rhs)?;
			return write!(f, " /* : {} */)", rhs.ty);
		}
		TypedExprKind::Cast { ty, expr } => {
			write!(f, "({ty}) ")?;
			return write_typed_expr(f, w, expr);
		}
		TypedExprKind::InternalCall { intrinsic } => return write!(f, "{intrinsic}"),
		TypedExprKind::Call {
			callee,
			call_type,
			named_generics,
			args,
		} => {
			write_typed_expr(f, w, callee)?;
			write!(f, " /* : {} */", callee.ty)?;
			match call_type {
				crate::parser::CallType::UserHeap => write!(f, "!")?,
				crate::parser::CallType::UserMaybeHeap | crate::parser::CallType::CompilerHeap => write!(f, "?")?,
				crate::parser::CallType::Regular => {}
			}
			if !named_generics.is_empty() {
				write!(f, "<")?;
				for (i, (name, ty)) in named_generics.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{name}: {ty}")?;
				}
				write!(f, ">")?;
			}
			write!(f, "(")?;
			for (i, arg) in args.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_typed_expr(f, w, arg)?;
				write!(f, " /* : {} */", arg.ty)?;
			}
			return write!(f, ")");
		}
		TypedExprKind::Field { base, name } => {
			write_typed_expr(f, w, base)?;
			return write!(f, " /* : {} */.{name}", base.ty);
		}
		TypedExprKind::Index { base, index } => {
			write_typed_expr(f, w, base)?;
			write!(f, "[")?;
			write_typed_expr(f, w, index)?;
			return write!(f, " /* : {} */]", index.ty);
		}
		TypedExprKind::Range(re) => return write_typed_range_expr(f, w, re),
		TypedExprKind::Tuple { elements } => {
			write!(f, "(")?;
			for (i, e) in elements.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_typed_expr(f, w, e)?;
				write!(f, " /* : {} */", e.ty)?;
			}
			return write!(f, ")");
		}
		TypedExprKind::Array(arr) => return write_typed_array_literal(f, w, arr),
		TypedExprKind::StructInit {
			path,
			fields,
			base,
			has_rest,
		} => {
			write!(f, "{path} {{")?;
			for (i, (name, expr)) in fields.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{name} -> ")?;
				write_typed_expr(f, w, expr)?;
				write!(f, " /* : {} */", expr.ty)?;
			}
			if let Some(base_expr) = base {
				if !fields.is_empty() {
					write!(f, ", ")?;
				}
				write!(f, "..")?;
				write_typed_expr(f, w, base_expr)?;
			} else if *has_rest {
				if !fields.is_empty() {
					write!(f, ", ")?;
				}
				write!(f, "..")?;
			}
			return write!(f, "}}");
		}
		TypedExprKind::Block(block) => return write_typed_block(f, w, block),
		TypedExprKind::UnsafeBlock(block) => {
			write!(f, "unsafe ")?;
			return write_typed_block(f, w, block);
		}
		TypedExprKind::Switch { expr, arms } => {
			write!(f, "switch ")?;
			write_typed_expr(f, w, expr)?;
			write!(f, " /* : {} */ ", expr.ty)?;
			writeln!(f, "{{")?;
			w.indent();
			for arm in arms {
				write_typed_switch_arm(f, w, arm)?;
			}
			w.dedent();
			w.write_indent(f)?;
			return write!(f, "}}");
		}
		TypedExprKind::If {
			cond,
			then_block,
			else_branch,
		} => {
			write!(f, "if ")?;
			write_typed_expr(f, w, cond)?;
			write!(f, " ")?;
			write_typed_block(f, w, then_block)?;
			if let Some(else_expr) = else_branch {
				write!(f, " else ")?;
				write_typed_expr(f, w, else_expr)?;
				write!(f, " /* : {} */", else_expr.ty)?;
			}
			return Ok(());
		}
		TypedExprKind::Loop { label, body } => {
			write!(f, "'{label}: ")?;
			write!(f, "loop ")?;
			return write_typed_block(f, w, body);
		}
	}
}

fn write_typed_range_expr(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, re: &TypedRangeExpr) -> fmt::Result
{
	if let Some(s) = &re.start {
		write_typed_expr(f, w, s)?;
		write!(f, " /* : {} */", s.ty)?;
	}
	if re.inclusive {
		write!(f, "..=")?;
	} else {
		write!(f, "..")?;
	}
	if let Some(e) = &re.end {
		write_typed_expr(f, w, e)?;
		write!(f, " /* : {} */", e.ty)?;
	}
	return Ok(());
}

fn write_typed_array_literal(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, arr: &TypedArrayLiteral) -> fmt::Result
{
	match arr {
		TypedArrayLiteral::List { elements, .. } => {
			write!(f, "[")?;
			for (i, e) in elements.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_typed_expr(f, w, e)?;
				write!(f, " /* : {} */", e.ty)?;
			}
			return write!(f, "]");
		}
		TypedArrayLiteral::Repeat { value, count, .. } => {
			write!(f, "[")?;
			write_typed_expr(f, w, value)?;
			write!(f, " /* : {} */; ", value.ty)?;
			write_typed_expr(f, w, count)?;
			return write!(f, " /* : {} */]", count.ty);
		}
	}
}

fn write_typed_switch_arm(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, arm: &TypedSwitchArm) -> fmt::Result
{
	w.write_indent(f)?;
	write!(f, "{} => ", arm.pattern)?;
	match &arm.body {
		TypedSwitchBody::Expr(expr) => {
			write_typed_expr(f, w, expr)?;
			return writeln!(f, " /* : {} */,", expr.ty);
		}
		TypedSwitchBody::Block(block) => {
			write_typed_block(f, w, block)?;
			return writeln!(f, ",");
		}
	}
}

impl fmt::Display for TypedPattern
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			TypedPattern::Wildcard { ty, .. } => return write!(f, "_: {ty}"),
			TypedPattern::Literal { value, ty, .. } => return write!(f, "{value}: {ty}"),
			TypedPattern::TypedIdentifier {
				name,
				ty,
				mutable,
				symbol,
				..
			} => {
				if *mutable {
					write!(f, "mut ")?;
				}
				return write!(f, "{name} /* #{} */: {ty}", symbol.0);
			}
			TypedPattern::Variant { path, args, ty, .. } => {
				write!(f, "{path}")?;
				if !args.is_empty() {
					write!(f, "(")?;
					for (i, a) in args.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{a}")?;
					}
					write!(f, ")")?;
				}
				return write!(f, " /* : {ty} */");
			}
			TypedPattern::Tuple { patterns, ty, .. } => {
				write!(f, "(")?;
				for (i, p) in patterns.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{p}")?;
				}
				return write!(f, "): {ty}");
			}
			TypedPattern::Struct {
				path,
				fields,
				has_rest,
				ty,
				..
			} => {
				write!(f, "{path} /* : {ty} */ {{")?;
				for (i, (name, pat)) in fields.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{name} -> {pat}")?;
				}
				if *has_rest {
					if !fields.is_empty() {
						write!(f, ", ")?;
					}
					write!(f, "..")?;
				}
				return write!(f, "}}");
			}
			TypedPattern::Range(re) => {
				if let Some(s) = &re.start {
					write!(f, "{s}")?;
				}
				if re.inclusive {
					write!(f, "..=")?;
				} else {
					write!(f, "..")?;
				}
				if let Some(e) = &re.end {
					write!(f, "{e}")?;
				}
				return write!(f, " /* : {} */", re.ty);
			}
			TypedPattern::Or { patterns, ty, .. } => {
				for (i, p) in patterns.iter().enumerate() {
					if i > 0 {
						write!(f, " | ")?;
					}
					write!(f, "{p}")?;
				}
				return write!(f, " /* : {ty} */");
			}
		}
	}
}

impl fmt::Display for TypedExpr
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		let mut w = IndentWriter::new();
		write_typed_expr(f, &mut w, self)?;
		return write!(f, " /* : {} */", self.ty);
	}
}

pub fn write_typed_directive_node(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	node: &TypedDirectiveNode,
) -> fmt::Result
{
	write_typed_directive(f, w, &node.directive)?;
	if let Some(body) = &node.body {
		write!(f, " ")?;
		write_typed_block(f, w, body)?;
	}
	return Ok(());
}

fn write_typed_directive(f: &mut fmt::Formatter<'_>, _w: &mut IndentWriter, directive: &TypedDirective) -> fmt::Result
{
	match directive {
		TypedDirective::Import { import, .. } => return write!(f, "@import \"{import}\""),
		TypedDirective::Use { use_path, .. } => return write!(f, "@use {use_path}"),
		TypedDirective::Custom { name, .. } => return write!(f, "@{name}"),
		TypedDirective::ValidateStructPattern { struct_path, .. } => {
			return write!(f, "@validate_struct_pattern {struct_path}");
		}
		TypedDirective::ValidateType { ty, expr } => return write!(f, "@validate_type({ty}, {expr})"),
	}
}

impl fmt::Display for TypedWhereConstraint
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
				write!(f, "{a}")?;
			}
			write!(f, ">")?;
		}
		write!(f, ": ")?;
		for (i, b) in self.bounds.iter().enumerate() {
			if i > 0 {
				write!(f, " + ")?;
			}
			write!(f, "{b}")?;
		}
		return Ok(());
	}
}

fn write_generic_params_typed(f: &mut fmt::Formatter<'_>, generics: &[crate::parser::GenericParam]) -> fmt::Result
{
	if !generics.is_empty() {
		write!(f, "<")?;
		for (i, g) in generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{g}")?;
		}
		write!(f, ">")?;
	}
	return Ok(());
}

fn write_typed_where_clause(f: &mut fmt::Formatter<'_>, clause: &[TypedWhereConstraint]) -> fmt::Result
{
	if !clause.is_empty() {
		write!(f, " where ")?;
		for (i, c) in clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{c}")?;
		}
	}
	return Ok(());
}
