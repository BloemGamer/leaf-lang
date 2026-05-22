use std::fmt;

use crate::{
	parser::{
		AST, ArrayLiteral, AssignOp, AssocTypeDecl, BinaryOp, Block, BlockContent, CallType, Directive, DirectiveNode,
		DirectiveParam, DocsComment, EnumDecl, Expected, Expr, ExternLanguage, FuncBound, FunctionDecl,
		FunctionSignature, GenericArg, GenericHeapParam, GenericParam, HeapGenericKind, ImplDecl, ImplItem, ImplTarget,
		Literal, Modifier, ModuleDecl, ModuleKind, Param, Path, PathSegment, Pattern, RangeExpr, Stmt, StructDecl,
		SwitchArm, SwitchBody, TopLevelBlock, TopLevelDecl, TraitDecl, TraitItem, Type, TypeAliasDecl, TypeCore,
		UnaryOp, UnionDecl, VariableDecl, VariantDecl, WhereBound, WhereConstraint,
	},
	utils::indent_writer::IndentWriter,
};

impl std::fmt::Display for Expected
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		return match self {
			Expected::Token(tk) => write!(f, "{:?}", tk),
			Expected::Identifier => write!(f, "identifier"),
			Expected::Type => write!(f, "type"),
			Expected::Pattern => write!(f, "pattern"),
			Expected::Expression => write!(f, "expression"),
			Expected::OneOf(tokens) => {
				write!(f, "one of: ")?;
				for (i, tk) in tokens.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{:?}", tk)?;
				}
				Ok(())
			}
			Expected::Description(s) => write!(f, "{}", s),
		};
	}
}

impl fmt::Display for AST
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "{}", self.top_level_block);
	}
}

impl fmt::Display for TopLevelBlock
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		let mut writer: IndentWriter = IndentWriter::new();
		for item in &self.items {
			write_top_level_decl(f, &mut writer, item)?;
			writeln!(f)?; // Add blank line between top-level items
		}
		return Ok(());
	}
}

pub fn write_top_level_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, decl: &TopLevelDecl) -> fmt::Result
{
	match decl {
		TopLevelDecl::Function(func) => return write_function_decl(f, w, func),
		TopLevelDecl::VariableDecl(var) => {
			write_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		TopLevelDecl::Struct(s) => return write_struct_decl(f, w, s),
		TopLevelDecl::Union(u) => return write_union_decl(f, w, u),
		TopLevelDecl::Enum(e) => return write_enum_decl(f, w, e),
		TopLevelDecl::Variant(v) => return write_variant_decl(f, w, v),
		TopLevelDecl::TypeAlias(t) => {
			write_type_alias_decl(f, w, t)?;
			return write!(f, ";");
		}
		TopLevelDecl::Trait(t) => return write_trait_decl(f, w, t),
		TopLevelDecl::Module(n) => return write_module_decl(f, w, n),
		TopLevelDecl::Impl(i) => return write_impl_decl(f, w, i),
		TopLevelDecl::Directive(d) => return write!(f, "{};", d),
	}
}

impl fmt::Display for Modifier
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			Modifier::Pub => return write!(f, "pub"),
			Modifier::Export => return write!(f, "export"),
			Modifier::Unsafe => return write!(f, "unsafe"),
			Modifier::Inline => return write!(f, "inline"),
			Modifier::Const => return write!(f, "const"),
			Modifier::Volatile => return write!(f, "volatile"),
			Modifier::Mut => return write!(f, "mut"),
			Modifier::Extern(lang) => {
				write!(f, "extern")?;
				if let Some(l) = lang {
					write!(f, "({l})")?;
				}
				return Ok(());
			}
			Modifier::Directive(d) => return write!(f, "{}", d),
		}
	}
}

impl fmt::Display for ExternLanguage
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			ExternLanguage::C => write!(f, "C")?,
			ExternLanguage::Leaf => write!(f, "Leaf")?,
		}
		return Ok(());
	}
}

impl std::fmt::Display for DirectiveNode
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "{}", self.directive)?;

		if let Some(body) = &self.body {
			write!(f, " ")?;
			match body {
				BlockContent::Block(_block) => {
					todo!()
				}
				BlockContent::TopLevelBlock(top_level) => {
					write!(f, "{{ {} items }}", top_level.items.len())?;
				}
			}
		}

		return Ok(());
	}
}

impl std::fmt::Display for Path
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		if self.global {
			write!(f, "::")?;
		}
		for (i, segment) in self.segments.iter().enumerate() {
			if i > 0 {
				write!(f, "::")?;
			}
			write!(f, "{}", segment.name)?;
			if !segment.generics.is_empty() {
				write!(f, "::<")?;
				for (i, g) in segment.generics.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", g)?;
				}
				write!(f, ">")?;
			}
		}
		if self.glob {
			if !self.segments.is_empty() {
				write!(f, "::")?;
			}
			write!(f, "*")?;
		}
		return Ok(());
	}
}

impl std::fmt::Display for PathSegment
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.name)?;

		if !self.generics.is_empty() {
			write!(f, "::<")?;
			for (i, g) in self.generics.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}", g)?;
			}
			write!(f, ">")?;
		}
		return Ok(());
	}
}

impl std::fmt::Display for Directive
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		match self {
			Directive::Import {
				modifers,
				import,
				visibility: _,
			} => {
				for m in modifers {
					write!(f, "{} ", m)?;
				}
				return write!(f, "@import \"{}\"", import);
			}
			Directive::Use {
				modifers,
				use_path,
				visibility: _,
			} => {
				for m in modifers {
					write!(f, "{} ", m)?;
				}
				write!(f, "@use ")?;
				write!(f, "{}", use_path)?;
				return Ok(());
			}
			Directive::Custom { name, params } => {
				write!(f, "@{}", name)?;
				if !params.is_empty() {
					write!(f, "(")?;
					for (i, arg) in params.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", arg)?;
					}
					write!(f, ")")?;
				}
				return Ok(());
			}
			Directive::ValidateStructPattern {
				struct_path,
				pattern_fields,
				has_rest,
			} => {
				write!(f, "@#validate_struct_pattern({struct_path}{{")?;
				for (i, p) in pattern_fields.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", p)?;
				}
				if *has_rest {
					if !pattern_fields.is_empty() {
						write!(f, ", ")?;
					}
					write!(f, "..")?;
				}
				return write!(f, "}})");
			}
			Directive::ValidateType { ty, expr } => {
				return write!(f, "@#validate_type({} == #typeof({}))", ty, expr);
			}
		}
	}
}

impl fmt::Display for DirectiveParam
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			DirectiveParam::Literal(lit) => write!(f, "{}", lit),
			DirectiveParam::Identifier(ident) => write!(f, "{}", ident),
			DirectiveParam::Named { name, arg } => write!(f, "{} = {}", name, arg),
		};
	}
}

pub fn write_function_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, func: &FunctionDecl) -> fmt::Result
{
	write_docs(f, w, &func.docs)?;
	write_function_signature(f, w, &func.signature)?;

	if let Some(body) = &func.body {
		write!(f, " ")?;
		write_block(f, w, body)?;
	} else {
		write!(f, ";")?;
	}

	return Ok(());
}

pub fn write_function_signature(
	f: &mut fmt::Formatter<'_>,
	_w: &mut IndentWriter,
	sig: &FunctionSignature,
) -> fmt::Result
{
	for modifier in &sig.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "fn")?;

	match sig.call_type {
		CallType::UserHeap => write!(f, "!")?,
		CallType::UserMaybeHeap | CallType::CompilerHeap => write!(f, "?")?,
		CallType::Regular => {}
	}

	if !sig.heap_generics.is_empty() {
		write!(f, "<")?;
		for (i, generic_param) in sig.heap_generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic_param)?;
		}
		write!(f, ">")?;
	}

	write!(f, " {}", sig.name)?;

	if !sig.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in sig.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	write!(f, "(")?;
	for (i, param) in sig.params.iter().enumerate() {
		if i > 0 {
			write!(f, ", ")?;
		}
		write!(f, "{}", param)?;
	}
	write!(f, ")")?;

	write!(f, " -> {}", sig.return_type)?;

	if !sig.where_clause.is_empty() {
		write!(f, " where ")?;
		for (i, constraint) in sig.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}

	return Ok(());
}

impl fmt::Display for Param
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		if self.variadic {
			return write!(f, "...");
		}

		return write!(f, "{}", self.pattern);
	}
}

impl fmt::Display for GenericParam
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.name)?;
		if !self.bounds.is_empty() {
			write!(f, ": ")?;
			for (i, bound) in self.bounds.iter().enumerate() {
				if i > 0 {
					write!(f, " + ")?;
				}
				write!(f, "{}", bound)?;
			}
		}
		return Ok(());
	}
}

impl fmt::Display for GenericHeapParam
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.name)?;
		if let HeapGenericKind::Forced(fg) = &self.kind {
			write!(f, " = {}", fg)?;
		}
		return Ok(());
	}
}

impl fmt::Display for Type
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "{}", self.core);
	}
}

impl fmt::Display for TypeCore
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			TypeCore::Base { path, generics } => {
				write!(f, "{}", path)?;
				if !generics.is_empty() {
					write!(f, "<")?;
					for (i, generic) in generics.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", generic)?;
					}
					write!(f, ">")?;
				}
				return Ok(());
			}
			TypeCore::Reference { mutable, inner } => {
				write!(f, "&")?;
				if *mutable {
					write!(f, "mut ")?;
				}
				return write!(f, "{}", inner);
			}
			TypeCore::Mutable { inner } => {
				return write!(f, "mut {}", inner);
			}
			TypeCore::Pointer { mutable, inner } => {
				write!(f, "*")?;
				if *mutable {
					write!(f, "mut ")?;
				}
				return write!(f, "{}", inner);
			}
			TypeCore::Array { inner, size } => {
				write!(f, "[")?;
				write!(f, "{}", inner)?;
				if let Some(s) = size {
					write!(f, "; {}", s)?;
				}
				return write!(f, "]");
			}
			TypeCore::Tuple(types) => {
				write!(f, "(")?;
				for (i, ty) in types.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", ty)?;
				}
				return write!(f, ")");
			}
			TypeCore::ImplTrait { bounds } => {
				write!(f, "impl ")?;
				for (i, bound) in bounds.iter().enumerate() {
					if i > 0 {
						write!(f, " + ")?;
					}
					write!(f, "{}", bound)?;
				}
				return Ok(());
			}
		}
	}
}

pub fn write_variable_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, var: &VariableDecl) -> fmt::Result
{
	if var.comp_const {
		write!(f, "const ")?;
	} else {
		write!(f, "var ")?;
	}

	write!(f, "{}", var.pattern)?;

	if let Some(init) = &var.init {
		write!(f, " = ")?;
		write_expr(f, w, init)?;
	}

	return Ok(());
}

impl fmt::Display for Pattern
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			Pattern::Wildcard { ty, .. } => {
				write!(f, "_")?;
				if let Some(t) = ty {
					write!(f, ": {}", t)?;
				}
				return Ok(());
			}
			Pattern::Literal { value: lit, .. } => return write!(f, "{}", lit),
			Pattern::TypedIdentifier {
				path,
				modifiers,
				ty,
				call_constructor,
				mutable,
				..
			} => {
				for modifier in modifiers {
					write!(f, "{} ", modifier)?;
				}
				if *mutable {
					write!(f, "mut ")?;
				}
				write!(f, "{}: {}", path, ty)?;
				if let Some(ct) = call_constructor {
					match ct {
						CallType::Regular => write!(f, "()")?,
						CallType::UserHeap => write!(f, "!()")?,
						CallType::UserMaybeHeap | CallType::CompilerHeap => write!(f, "?()")?,
					}
				}
				return Ok(());
			}
			Pattern::Variant { path, args, .. } => {
				write!(f, "{}", path)?;
				if !args.is_empty() {
					write!(f, "(")?;
					for (i, arg) in args.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", arg)?;
					}
					write!(f, ")")?;
				}
				return Ok(());
			}
			Pattern::Tuple { patterns, .. } => {
				write!(f, "(")?;
				for (i, pat) in patterns.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", pat)?;
				}
				return write!(f, ")");
			}
			Pattern::Struct {
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
			Pattern::Range(range) => return write!(f, "{}", range),
			Pattern::Or { patterns, .. } => {
				for (i, pat) in patterns.iter().enumerate() {
					if i > 0 {
						write!(f, " | ")?;
					}
					write!(f, "{}", pat)?;
				}
				return Ok(());
			}
		}
	}
}

impl fmt::Display for Expr
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			Expr::Identifier { path, .. } => return write!(f, "{}", path),
			Expr::Literal { value: lit, .. } => return write!(f, "{}", lit),
			Expr::Default { heap_call, .. } => {
				return {
					write!(f, "default")?;
					match *heap_call {
						CallType::Regular => write!(f, "()"),
						CallType::UserHeap => write!(f, "!()"),
						CallType::UserMaybeHeap | CallType::CompilerHeap => write!(f, "?()"),
					}
				};
			}
			Expr::Unary { op, expr, .. } => match op {
				UnaryOp::Neg => return write!(f, "-{}", expr),
				UnaryOp::Not => return write!(f, "!{}", expr),
				UnaryOp::Deref => return write!(f, "*{}", expr),
				UnaryOp::Addr { mutable } => {
					if *mutable {
						return write!(f, "&mut {}", expr);
					}
					return write!(f, "&{}", expr);
				}
			},
			Expr::Binary { op, lhs, rhs, .. } => return write!(f, "({} {} {})", lhs, op, rhs),
			Expr::Cast { ty, expr, .. } => return write!(f, "({}) {}", ty, expr),
			Expr::Call {
				callee,
				call_type,
				named_generics,
				args,
				..
			} => {
				write!(f, "{}", callee)?;

				match call_type {
					CallType::UserHeap => write!(f, "!")?,
					CallType::CompilerHeap | CallType::UserMaybeHeap => write!(f, "?")?,
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
					write!(f, "{}", arg)?;
				}
				return write!(f, ")");
			}
			Expr::Field { base, name, .. } => return write!(f, "{}.{}", base, name),
			Expr::Index { base, index, .. } => return write!(f, "{}[{}]", base, index),
			Expr::Range(range) => return write!(f, "{}", range),
			Expr::Tuple { elements: exprs, .. } => {
				write!(f, "(")?;
				for (i, expr) in exprs.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", expr)?;
				}
				return write!(f, ")");
			}
			Expr::Array(arr) => return write!(f, "{}", arr),
			Expr::StructInit {
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
					write!(f, "{} -> {}", name, expr)?;
				}
				if let Some(base_expr) = base {
					if !fields.is_empty() {
						write!(f, ", ")?;
					}
					write!(f, "..{}", base_expr)?;
				} else if *has_rest {
					if !fields.is_empty() {
						write!(f, ", ")?;
					}
					write!(f, "..")?;
				}
				return write!(f, "}}");
			}
			Expr::Block(block) => {
				let mut w = IndentWriter::new();
				return write_block(f, &mut w, block);
			}
			Expr::UnsafeBlock(block) => {
				write!(f, "unsafe ")?;
				let mut w = IndentWriter::new();
				return write_block(f, &mut w, block);
			}
			Expr::Switch { expr, arms, .. } => {
				let mut w = IndentWriter::new();
				return write_switch(f, &mut w, expr, arms);
			}
			Expr::If {
				cond,
				then_block,
				else_branch,
				..
			} => {
				write!(f, "if {} ", cond)?;
				let mut w = IndentWriter::new();
				write_block(f, &mut w, then_block)?;
				if let Some(else_expr) = else_branch {
					write!(f, " else ")?;
					match else_expr.as_ref() {
						Expr::Block(b) => write_block(f, &mut w, b)?,
						_ => write!(f, "{}", else_expr)?,
					}
				}
				return Ok(());
			}

			Expr::IfVar {
				pattern,
				expr,
				then_block,
				else_branch,
				..
			} => {
				write!(f, "if var {} = {} ", pattern, expr)?;
				let mut w = IndentWriter::new();
				write_block(f, &mut w, then_block)?;
				if let Some(else_expr) = else_branch {
					write!(f, " else ")?;
					match else_expr.as_ref() {
						Expr::Block(b) => write_block(f, &mut w, b)?,
						_ => write!(f, "{}", else_expr)?,
					}
				}
				return Ok(());
			}
			Expr::Loop { label, body, .. } => {
				if let Some(lbl) = label {
					write!(f, "'{}: ", lbl)?;
				}
				write!(f, "loop ")?;
				let mut w = IndentWriter::new();
				return write_block(f, &mut w, body);
			}
		}
	}
}

pub fn write_switch(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, expr: &Expr, arms: &[SwitchArm]) -> fmt::Result
{
	write!(f, "switch ")?;
	write_expr(f, w, expr)?;
	writeln!(f, " {{")?;
	w.indent();

	for arm in arms {
		w.write_indent(f)?;
		write!(f, "{} => ", arm.pattern)?;
		match &arm.body {
			SwitchBody::Expr(expr) => {
				write_expr(f, w, expr)?;
				writeln!(f, ",")?;
			}
			SwitchBody::Block(b) => {
				write_block(f, w, b)?;
				writeln!(f, ",")?;
			}
		}
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

impl fmt::Display for Literal
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			Literal::Int {
				value,
				base,
				ty,
				span: _,
			} => {
				write!(f, "{}{}", base, value)?;
				if let Some(t) = ty {
					write!(f, "{}", t)?;
				}
				return Ok(());
			}
			Literal::Float { value, bits, span: _ } => {
				write!(f, "{}", value)?;
				if let Some(b) = bits {
					write!(f, "{}", b)?;
				}
				return Ok(());
			}
			Literal::Bool { value: b, span: _ } => return write!(f, "{}", b),
			Literal::String {
				value: s,
				flags,
				span: _,
			} => return write!(f, "{}{:?}", flags, s),
			Literal::Char { value: c, span: _ } => return write!(f, "{:?}", c),
		}
	}
}

impl fmt::Display for ArrayLiteral
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			ArrayLiteral::List { elements, .. } => {
				write!(f, "[")?;
				for (i, expr) in elements.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{}", expr)?;
				}
				return write!(f, "]");
			}
			ArrayLiteral::Repeat { value, count, .. } => {
				write!(f, "[")?;
				write!(f, "{}", value)?;
				return write!(f, "; {}]", count);
			}
		}
	}
}

impl fmt::Display for BinaryOp
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			BinaryOp::LogicalOr => return write!(f, "||"),
			BinaryOp::LogicalAnd => return write!(f, "&&"),
			BinaryOp::Eq => return write!(f, "=="),
			BinaryOp::Ne => return write!(f, "!="),
			BinaryOp::Lt => return write!(f, "<"),
			BinaryOp::Gt => return write!(f, ">"),
			BinaryOp::Le => return write!(f, "<="),
			BinaryOp::Ge => return write!(f, ">="),
			BinaryOp::Add => return write!(f, "+"),
			BinaryOp::Sub => return write!(f, "-"),
			BinaryOp::Mul => return write!(f, "*"),
			BinaryOp::Div => return write!(f, "/"),
			BinaryOp::Mod => return write!(f, "%"),
			BinaryOp::BitAnd => return write!(f, "&"),
			BinaryOp::BitOr => return write!(f, "|"),
			BinaryOp::BitXor => return write!(f, "^"),
			BinaryOp::Shl => return write!(f, "<<"),
			BinaryOp::Shr => return write!(f, ">>"),
		}
	}
}

impl fmt::Display for RangeExpr
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		if let Some(start) = &self.start {
			write!(f, "{}", start)?;
		}

		if self.inclusive {
			write!(f, "..=")?;
		} else {
			write!(f, "..")?;
		}

		if let Some(end) = &self.end {
			write!(f, "{}", end)?;
		}

		return Ok(());
	}
}

pub fn write_block(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, block: &Block) -> fmt::Result
{
	writeln!(f, "{{")?;
	w.indent();

	for stmt in &block.stmts {
		write_stmt(f, w, stmt)?;
		writeln!(f)?;
	}

	if let Some(tail) = &block.tail_expr {
		w.write_indent(f)?;
		write_expr(f, w, tail)?;
		writeln!(f)?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_expr(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, expr: &Expr) -> fmt::Result
{
	match expr {
		Expr::Switch {
			expr: switch_expr,
			arms,
			..
		} => return write_switch(f, w, switch_expr, arms),
		Expr::Block(block) => return write_block(f, w, block),
		Expr::UnsafeBlock(block) => {
			write!(f, "unsafe ")?;
			return write_block(f, w, block);
		}
		Expr::If {
			cond,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if ")?;
			write_expr(f, w, cond)?;
			write!(f, " ")?;
			write_block(f, w, then_block)?;
			if let Some(else_stmt) = else_branch {
				write!(f, " else ")?;
				write_expr(f, w, else_stmt)?;
			}
			return Ok(());
		}
		Expr::IfVar {
			pattern,
			expr,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if var {} = ", pattern)?;

			write_expr(f, w, expr)?;
			write!(f, " ")?;
			write_block(f, w, then_block)?;
			if let Some(else_stmt) = else_branch {
				write!(f, " else ")?;
				write_expr(f, w, else_stmt)?;
			}
			return Ok(());
		}
		Expr::Loop { label, body, .. } => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "loop ")?;
			return write_block(f, w, body);
		}
		_ => return write!(f, "{}", expr),
	}
}

pub fn write_stmt(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, stmt: &Stmt) -> fmt::Result
{
	w.write_indent(f)?;
	return write_stmt_no_indent(f, w, stmt);
}

pub fn write_stmt_no_indent(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, stmt: &Stmt) -> fmt::Result
{
	match stmt {
		Stmt::VariableDecl(var) => {
			write_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		Stmt::Assignment { target, op, value, .. } => {
			write_expr(f, w, target)?;
			write!(f, " {} ", op)?;
			write_expr(f, w, value)?;
			return write!(f, ";");
		}
		Stmt::Return { value, .. } => {
			write!(f, "return")?;
			if let Some(e) = value {
				write!(f, " ")?;
				write_expr(f, w, e)?;
			}
			return write!(f, ";");
		}
		Stmt::Expr(expr) => match expr {
			Expr::Switch { expr, arms, .. } => {
				write_switch(f, w, expr, arms)?;
				return write!(f, ";");
			}
			Expr::Block(block) => {
				write_block(f, w, block)?;
				return write!(f, ";");
			}
			_ => {
				write_expr(f, w, expr)?;
				return write!(f, ";");
			}
		},
		Stmt::Break { label, value, .. } => {
			write!(f, "break")?;
			if let Some(lbl) = label {
				write!(f, " '{}", lbl)?;
			}
			if let Some(val) = value {
				write!(f, " ")?;
				write_expr(f, w, val)?;
			}
			return write!(f, ";");
		}
		Stmt::Continue { label, .. } => {
			write!(f, "continue")?;
			if let Some(lbl) = label {
				write!(f, " '{}", lbl)?;
			}
			return write!(f, ";");
		}
		Stmt::If {
			cond,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if ")?;
			write_expr(f, w, cond)?;
			write!(f, " ")?;
			write_block(f, w, then_block)?;
			if let Some(else_stmt) = else_branch {
				write!(f, " else ")?;
				write_stmt_no_indent(f, w, else_stmt)?;
			}
			return Ok(());
		}
		Stmt::IfVar {
			pattern,
			expr,
			then_block,
			else_branch,
			..
		} => {
			write!(f, "if var {} = ", pattern)?;
			write_expr(f, w, expr)?;
			write!(f, " ")?;
			write_block(f, w, then_block)?;
			if let Some(else_stmt) = else_branch {
				write!(f, " else ")?;
				write_stmt_no_indent(f, w, else_stmt)?;
			}
			return Ok(());
		}
		Stmt::While { label, cond, body, .. } => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "while ")?;
			write_expr(f, w, cond)?;
			write!(f, " ")?;
			return write_block(f, w, body);
		}
		Stmt::Loop { label, body, .. } => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "loop ")?;
			return write_block(f, w, body);
		}
		Stmt::WhileVarLoop {
			label,
			pattern,
			expr,
			body,
			..
		} => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "while var {} = ", pattern)?;
			write_expr(f, w, expr)?;
			write!(f, " ")?;
			return write_block(f, w, body);
		}
		Stmt::For {
			label,
			pattern,
			iter,
			body,
			..
		} => {
			if let Some(lbl) = label {
				write!(f, "'{}: ", lbl)?;
			}
			write!(f, "for {} in ", pattern)?;
			write_expr(f, w, iter)?;
			write!(f, " ")?;
			return write_block(f, w, body);
		}
		Stmt::Delete { expr, .. } => {
			write!(f, "delete ")?;
			write_expr(f, w, expr)?;
			return write!(f, ";");
		}
		Stmt::Unsafe(block) => {
			write!(f, "unsafe ")?;
			return write_block(f, w, block);
		}
		Stmt::Block(block) => return write_block(f, w, block),
		Stmt::Directive(directive_node) => {
			write!(f, "{}", directive_node)?;
			if directive_node.body.is_none() {
				write!(f, ";")?;
			}
			return Ok(());
		}
	}
}

impl fmt::Display for AssignOp
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		match self {
			AssignOp::Assign => return write!(f, "="),
			AssignOp::AddAssign => return write!(f, "+="),
			AssignOp::SubAssign => return write!(f, "-="),
			AssignOp::MulAssign => return write!(f, "*="),
			AssignOp::DivAssign => return write!(f, "/="),
			AssignOp::ModAssign => return write!(f, "%="),
			AssignOp::AndAssign => return write!(f, "&="),
			AssignOp::OrAssign => return write!(f, "|="),
			AssignOp::XorAssign => return write!(f, "^="),
			AssignOp::ShlAssign => return write!(f, "<<="),
			AssignOp::ShrAssign => return write!(f, ">>="),
		}
	}
}

pub fn write_struct_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, s: &StructDecl) -> fmt::Result
{
	write_docs(f, w, &s.docs)?;
	for modifier in &s.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "struct {}", s.name)?;
	if !s.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in s.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if !s.where_clause.is_empty() {
		write!(f, "\nwhere ")?;
		for (i, constraint) in s.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}

	writeln!(f, " {{")?;
	w.indent();

	for field in &s.fields {
		write_docs(f, w, &field.docs)?;
		w.write_indent(f)?;
		write!(f, "{}: {}", field.name, field.ty)?;
		if let Some(default) = &field.default_value {
			write!(f, " = {}", default)?;
		}
		writeln!(f, ",")?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_union_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, u: &UnionDecl) -> fmt::Result
{
	write_docs(f, w, &u.docs)?;
	for modifier in &u.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "union {}", u.name)?;
	if !u.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in u.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if !u.where_clause.is_empty() {
		write!(f, "\nwhere ")?;
		for (i, constraint) in u.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}
	writeln!(f, " {{")?;

	w.indent();

	for field in &u.fields {
		write_docs(f, w, &field.docs)?;
		w.write_indent(f)?;
		writeln!(f, "{}: {},", field.name, field.ty)?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_enum_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, e: &EnumDecl) -> fmt::Result
{
	write_docs(f, w, &e.docs)?;
	for modifier in &e.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "enum {}", e.name)?;
	if !e.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in e.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if !e.where_clause.is_empty() {
		write!(f, "\nwhere ")?;
		for (i, constraint) in e.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}

	writeln!(f, " {{")?;
	w.indent();

	for variant in &e.variants {
		write_docs(f, w, &variant.docs)?;
		w.write_indent(f)?;
		if let Some(val) = &variant.value {
			write!(f, "{} = ", variant.name)?;
			write_expr(f, w, val)?;
		} else {
			writeln!(f, "{},", variant.name)?;
		}
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_variant_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, v: &VariantDecl) -> fmt::Result
{
	write_docs(f, w, &v.docs)?;
	for modifier in &v.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "variant {}", v.name)?;
	if !v.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in v.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if !v.where_clause.is_empty() {
		write!(f, "\nwhere\n")?;
		for (i, constraint) in v.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}

	writeln!(f, " {{")?;
	w.indent();

	for member in &v.variants {
		write_docs(f, w, &member.docs)?;
		w.write_indent(f)?;
		write!(f, "{}", member.name)?;
		if let Some(t) = &member.ty {
			write!(f, "({})", t)?;
		}
		if let Some(val) = &member.value {
			write!(f, " = ")?;
			write_expr(f, w, val)?;
		}
		writeln!(f, ",")?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_type_alias_decl(f: &mut fmt::Formatter<'_>, w: &IndentWriter, t: &TypeAliasDecl) -> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for modifier in &t.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "type {}", t.name)?;

	if !t.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in t.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	return write!(f, " = {}", t.ty);
}

pub fn write_type_assoc_type(f: &mut fmt::Formatter<'_>, w: &IndentWriter, t: &AssocTypeDecl) -> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for modifier in &t.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "type {}", t.name)?;

	if !t.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in t.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if let Some(ty) = &t.ty {
		write!(f, " = {}", ty)?;
	}
	return Ok(());
}

pub fn write_module_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, n: &ModuleDecl) -> fmt::Result
{
	write_docs(f, w, &n.docs)?;
	for modifier in &n.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "module {}", n.name)?;

	match &n.kind {
		ModuleKind::Inline(inline) => {
			writeln!(f, " {{")?;
			w.indent();
			for item in &inline.items {
				w.write_indent(f)?;
				write_top_level_decl(f, w, item)?;
				writeln!(f)?;
				writeln!(f)?;
			}
			w.dedent();
			w.write_indent(f)?;
			write!(f, "}}")?;
		}
		ModuleKind::External => {
			write!(f, ";")?;
		}
	}
	return Ok(());
}

pub fn write_trait_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, t: &TraitDecl) -> fmt::Result
{
	write_docs(f, w, &t.docs)?;
	for modifier in &t.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "trait {}", t.name)?;

	if !t.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in t.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

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
		write_trait_item(f, w, item)?;
		writeln!(f)?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_trait_item(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, item: &TraitItem) -> fmt::Result
{
	match item {
		TraitItem::Function(func) => {
			return write_function_decl(f, w, func);
		}
		TraitItem::TypeAlias(ta) => {
			write_type_alias_decl(f, w, ta)?;
			return write!(f, ";");
		}
		TraitItem::Const(var) => {
			write_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		TraitItem::AssocType(ty) => {
			write_type_assoc_type(f, w, ty)?;
			return write!(f, ";");
		}
	}
}

pub fn write_impl_decl(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, i: &ImplDecl) -> fmt::Result
{
	write_docs(f, w, &i.docs)?;
	for modifier in &i.modifiers {
		write!(f, "{} ", modifier)?;
	}

	write!(f, "impl")?;

	if !i.generics.is_empty() {
		write!(f, "<")?;
		for (i, generic) in i.generics.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", generic)?;
		}
		write!(f, ">")?;
	}

	if let Some(trait_path) = &i.trait_path {
		write!(f, " {}", trait_path)?;
		write!(f, " for")?;
	}

	write!(f, " {}", i.target)?;

	if !i.where_clause.is_empty() {
		write!(f, " where ")?;
		for (i, constraint) in i.where_clause.iter().enumerate() {
			if i > 0 {
				write!(f, ", ")?;
			}
			write!(f, "{}", constraint)?;
		}
	}

	writeln!(f, " {{")?;
	w.indent();

	for item in &i.body {
		w.write_indent(f)?;
		write_impl_item(f, w, item)?;
		writeln!(f)?;
	}

	w.dedent();
	w.write_indent(f)?;
	return write!(f, "}}");
}

pub fn write_impl_item(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, item: &ImplItem) -> fmt::Result
{
	match item {
		ImplItem::Function(func) => return write_function_decl(f, w, func),
		ImplItem::TypeAlias(ta) => {
			write_type_alias_decl(f, w, ta)?;
			return write!(f, ";");
		}
		ImplItem::Const(var) => {
			write_variable_decl(f, w, var)?;
			return write!(f, ";");
		}
		ImplItem::AssocType(ty) => {
			write_type_assoc_type(f, w, ty)?;
			return write!(f, ";");
		}
	}
}

impl fmt::Display for ImplTarget
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.path)?;

		if !self.generics.is_empty() {
			write!(f, "<")?;
			for (i, generic) in self.generics.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}", generic)?;
			}
			write!(f, ">")?;
		}

		return Ok(());
	}
}

impl fmt::Display for WhereConstraint
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		write!(f, "{}", self.ty)?;

		if !self.type_args.is_empty() {
			write!(f, "<")?;
			for (i, arg) in self.type_args.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}", arg)?;
			}
			write!(f, ">")?;
		}

		write!(f, ": ")?;
		for (i, bound) in self.bounds.iter().enumerate() {
			if i > 0 {
				write!(f, " + ")?;
			}
			write!(f, "{}", bound)?;
		}
		return Ok(());
	}
}

impl fmt::Display for WhereBound
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			WhereBound::Path { path, args } => {
				write!(f, "{}", path)?;
				if !args.is_empty() {
					write!(f, "<")?;
					for (i, arg) in args.iter().enumerate() {
						if i > 0 {
							write!(f, ", ")?;
						}
						write!(f, "{}", arg)?;
					}
					write!(f, ">")?;
				}
				Ok(())
			}
			WhereBound::Func(func_bound) => {
				write!(f, "{}", func_bound)
			}
		};
	}
}

impl fmt::Display for FuncBound
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			FuncBound::Fn { args, ret } => {
				write!(f, "Fn(")?;
				for (i, a) in args.iter().enumerate() {
					if i > 0 {
						write!(f, ", ")?;
					}
					write!(f, "{},", a)?;
					write!(f, ")")?;
				}
				if let Some(ty) = ret {
					write!(f, "-> {}", ty)?;
				}
				Ok(())
			}
		};
	}
}

impl fmt::Display for GenericArg
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return match self {
			GenericArg::Type(ty) => write!(f, "{}", ty),
			GenericArg::Binding { name, ty, .. } => write!(f, "{} = {}", name, ty),
		};
	}
}

#[allow(clippy::ref_option)]
pub fn write_docs(f: &mut fmt::Formatter<'_>, w: &IndentWriter, docs: &Option<DocsComment>) -> fmt::Result
{
	if let Some(doc) = docs {
		for line in doc.content.lines() {
			w.write_indent(f)?;
			writeln!(f, "///{}", line)?;
		}
	}
	return Ok(());
}

impl fmt::Display for DocsComment
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		return write!(f, "///{}", self.content);
	}
}
