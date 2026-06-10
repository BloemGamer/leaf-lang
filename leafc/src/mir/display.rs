use std::fmt;

use crate::parser::CallType;
use crate::{type_analysis::TyBound, utils::indent_writer::IndentWriter};

use super::{
	ConstBodyId, MirAggregateKind, MirBasicBlock, MirBody, MirCallee, MirConstBody, MirFunction, MirGlobal, MirItem,
	MirLiteral, MirLiteralValue, MirLocal, MirModule, MirOperand, MirPlace, MirPlaceBase, MirProjection, MirRvalue,
	MirStmt, MirSwitchArm, MirTerminator, MirTypeDef, MirTypeDefKind,
};

impl fmt::Display for MirModule
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
	{
		writeln!(
			f,
			"-------------------------------------------------------\n::{} =>",
			self.path.join("::"),
		)?;
		let mut w = IndentWriter::new();
		for item in &self.items {
			write_mir_item(f, &mut w, item)?;
			writeln!(f)?;
		}

		if !self.const_bodies.is_empty() {
			writeln!(f, "// ---- const bodies ----")?;
			for (i, cb) in self.const_bodies.iter().enumerate() {
				#[allow(clippy::cast_possible_truncation)]
				write_mir_const_body(f, &mut w, ConstBodyId(i as u32), cb)?;
				writeln!(f)?;
			}
		}
		return Ok(());
	}
}

pub fn write_mir_const_body(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	id: ConstBodyId,
	cb: &MirConstBody,
) -> fmt::Result
{
	writeln!(
		f,
		"const#{} -> _{}: {} {{",
		id.0,
		cb.result.0,
		cb.body.local(cb.result).ty
	)?;
	w.indent();
	write_mir_body(f, w, &cb.body)?;
	w.dedent();
	return writeln!(f, "}}");
}

pub fn write_mir_item(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, item: &MirItem) -> fmt::Result
{
	match item {
		MirItem::Function(func) => return write_mir_function(f, w, func),
		MirItem::Global(global) => return write_mir_global(f, w, global),
		MirItem::TypeDef(typedef) => return write_mir_typedef(f, w, typedef),
	}
}

pub fn write_mir_global(f: &mut fmt::Formatter<'_>, _w: &mut IndentWriter, global: &MirGlobal) -> fmt::Result
{
	write!(f, "global ")?;
	if global.mutable {
		write!(f, "mut ")?;
	}
	return writeln!(
		f,
		"{}: {} = const#{};  // {:?}",
		global.name, global.ty, global.init.0, global.symbol
	);
}

fn write_generic_param_list(f: &mut fmt::Formatter<'_>, generics: &[crate::parser::GenericParam]) -> fmt::Result
{
	if generics.is_empty() {
		return Ok(());
	}
	write!(f, "<")?;
	for (i, g) in generics.iter().enumerate() {
		if i > 0 {
			write!(f, ", ")?;
		}
		write!(f, "{}", g.name)?;
	}
	return write!(f, ">");
}

fn write_fn_generic_list(
	f: &mut fmt::Formatter<'_>,
	generics: &[(crate::parser::Ident, crate::lexer::Span)],
	heap_generics: &[crate::name_resolution::ResolvedGenericHeapParam],
) -> fmt::Result
{
	if generics.is_empty() && heap_generics.is_empty() {
		return Ok(());
	}
	write!(f, "<")?;
	let mut first = true;
	for (name, _) in generics {
		if !first {
			write!(f, ", ")?;
		}
		write!(f, "{}", name)?;
		first = false;
	}
	for hp in heap_generics {
		if !first {
			write!(f, ", ")?;
		}
		// Heap generics get a leading `'` so they're visually distinct from
		// regular type params.
		write!(f, "'{}", hp.name)?;
		first = false;
	}
	return write!(f, ">");
}

fn write_ty_bound(f: &mut fmt::Formatter<'_>, b: &TyBound) -> fmt::Result
{
	match b {
		TyBound::Trait { symbol, .. } => return write!(f, "{:?}", symbol),
		TyBound::Fn { args, ret } => {
			write!(f, "Fn(")?;
			for (i, a) in args.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}", a)?;
			}
			return write!(f, ") -> {}", ret);
		}
	}
}

fn write_where_clause(
	f: &mut fmt::Formatter<'_>,
	w: &IndentWriter,
	where_clause: &[crate::type_analysis::TypedWhereConstraint],
) -> fmt::Result
{
	if where_clause.is_empty() {
		return Ok(());
	}
	writeln!(f)?;
	w.write_indent(f)?;
	write!(f, "where")?;
	for (i, c) in where_clause.iter().enumerate() {
		if i > 0 {
			write!(f, ",")?;
		}
		writeln!(f)?;
		w.write_indent(f)?;
		write!(f, "  {}: ", c.ty)?;
		for (j, b) in c.bounds.iter().enumerate() {
			if j > 0 {
				write!(f, " + ")?;
			}
			write_ty_bound(f, b)?;
		}
	}
	return Ok(());
}

pub fn write_mir_typedef(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, typedef: &MirTypeDef) -> fmt::Result
{
	match &typedef.kind {
		MirTypeDefKind::Struct { fields } => {
			write!(f, "struct {}", typedef.name)?;
			write_generic_param_list(f, &typedef.generics)?;
			write_where_clause(f, w, &typedef.where_clause)?;
			writeln!(f, " {{  // {:?}", typedef.symbol)?;
			w.indent();
			for (name, ty) in fields {
				w.write_indent(f)?;
				writeln!(f, "{}: {},", name, ty)?;
			}
			w.dedent();
			return writeln!(f, "}}");
		}
		MirTypeDefKind::Union { fields } => {
			write!(f, "union {}", typedef.name)?;
			write_generic_param_list(f, &typedef.generics)?;
			write_where_clause(f, w, &typedef.where_clause)?;
			writeln!(f, " {{  // {:?}", typedef.symbol)?;
			w.indent();
			for (name, ty) in fields {
				w.write_indent(f)?;
				writeln!(f, "{}: {},", name, ty)?;
			}
			w.dedent();
			return writeln!(f, "}}");
		}
		MirTypeDefKind::Enum { variants } => {
			write!(f, "enum {}", typedef.name)?;
			write_generic_param_list(f, &typedef.generics)?;
			writeln!(f, " {{  // {:?}", typedef.symbol)?;
			w.indent();
			for (name, value) in variants {
				w.write_indent(f)?;
				if let Some(id) = value {
					writeln!(f, "{} = const#{},", name, id.0)?;
				} else {
					writeln!(f, "{},", name)?;
				}
			}
			w.dedent();
			return writeln!(f, "}}");
		}
		MirTypeDefKind::Variant { members } => {
			write!(f, "variant {}", typedef.name)?;
			write_generic_param_list(f, &typedef.generics)?;
			writeln!(f, " {{  // {:?}", typedef.symbol)?;
			w.indent();
			for (name, ty) in members {
				w.write_indent(f)?;
				if let Some(t) = ty {
					writeln!(f, "{}({}),", name, t)?;
				} else {
					writeln!(f, "{},", name)?;
				}
			}
			w.dedent();
			return writeln!(f, "}}");
		}
		MirTypeDefKind::TypeAlias { ty } => {
			write!(f, "type {}", typedef.name)?;
			write_generic_param_list(f, &typedef.generics)?;
			return writeln!(f, " = {};  // {:?}", ty, typedef.symbol);
		}
	}
}

pub fn write_mir_function(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, func: &MirFunction) -> fmt::Result
{
	write!(f, "fn")?;
	match func.call_type {
		CallType::UserHeap => write!(f, "!")?,
		CallType::UserMaybeHeap | CallType::CompilerHeap => write!(f, "?")?,
		CallType::Regular => {}
	}
	write!(f, " {}", func.name)?;
	write_fn_generic_list(f, &func.generics, &func.heap_generics)?;
	write!(f, "(")?;
	for (i, param) in func.params.iter().enumerate() {
		if i > 0 {
			write!(f, ", ")?;
		}
		if param.mutable {
			write!(f, "mut ")?;
		}
		write!(f, "{}: {} [_{:?}]", param.name, param.ty, param.local.0)?;
	}
	write!(f, ") -> {}", func.return_ty)?;
	write_where_clause(f, w, &func.where_clause)?;
	writeln!(f, "  // {:?}", func.symbol)?;

	match &func.body {
		None => {
			return writeln!(f, ";");
		}
		Some(body) => {
			writeln!(f, "{{")?;
			w.indent();
			write_mir_body(f, w, body)?;
			w.dedent();
			return writeln!(f, "}}");
		}
	}
}

pub fn write_mir_body(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, body: &MirBody) -> fmt::Result
{
	w.write_indent(f)?;
	writeln!(f, "// locals:")?;
	for local in &body.locals {
		write_mir_local_decl(f, w, local, body.param_count)?;
	}
	if let Some(ret) = body.return_local {
		w.write_indent(f)?;
		writeln!(f, "// return => _{}", ret.0)?;
	}
	writeln!(f)?;

	for block in &body.blocks {
		write_mir_block(f, w, block)?;
		writeln!(f)?;
	}
	return Ok(());
}

fn write_mir_local_decl(
	f: &mut fmt::Formatter<'_>,
	w: &IndentWriter,
	local: &MirLocal,
	param_count: usize,
) -> fmt::Result
{
	w.write_indent(f)?;
	let kind = if (local.id.0 as usize) < param_count {
		"param"
	} else if local.is_temp {
		"temp "
	} else {
		"let  "
	};
	write!(f, "{} _{}", kind, local.id.0)?;
	if let Some(name) = &local.name {
		write!(f, " /* {} */", name)?;
	}
	if local.mutable {
		write!(f, " mut")?;
	}
	return writeln!(f, ": {};", local.ty);
}

pub fn write_mir_block(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, block: &MirBasicBlock) -> fmt::Result
{
	w.write_indent(f)?;
	writeln!(f, "bb{}: {{", block.id.0)?;
	w.indent();

	for stmt in &block.stmts {
		write_mir_stmt(f, w, stmt)?;
	}

	write_mir_terminator(f, w, &block.terminator)?;

	w.dedent();
	w.write_indent(f)?;
	return writeln!(f, "}}");
}

fn write_call_generics(
	f: &mut fmt::Formatter<'_>,
	type_args: &[crate::type_analysis::Ty],
	named_generics: &[(String, crate::type_analysis::Ty)],
) -> fmt::Result
{
	if type_args.is_empty() && named_generics.is_empty() {
		return Ok(());
	}
	write!(f, "::<")?;
	let mut first = true;
	for ta in type_args {
		if !first {
			write!(f, ", ")?;
		}
		write!(f, "{}", ta)?;
		first = false;
	}
	for (name, ty) in named_generics {
		if !first {
			write!(f, ", ")?;
		}
		write!(f, "{} = {}", name, ty)?;
		first = false;
	}
	return write!(f, ">");
}

pub fn write_mir_stmt(f: &mut fmt::Formatter<'_>, w: &IndentWriter, stmt: &MirStmt) -> fmt::Result
{
	w.write_indent(f)?;
	match stmt {
		MirStmt::Assign { place, rvalue, .. } => {
			write_mir_place(f, place)?;
			write!(f, " = ")?;
			write_mir_rvalue(f, rvalue)?;
			return writeln!(f, ";");
		}
		MirStmt::Call {
			callee,
			type_args,
			named_generics,
			args,
			..
		} => {
			write_mir_callee(f, callee)?;
			write_call_generics(f, type_args, named_generics)?;
			write!(f, "(")?;
			for (i, arg) in args.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_mir_operand(f, arg)?;
			}
			return writeln!(f, ");");
		}
		MirStmt::Delete { operand, .. } => {
			write!(f, "delete ")?;
			write_mir_operand(f, operand)?;
			return writeln!(f, ";");
		}
		MirStmt::Nop => {
			return writeln!(f, "nop;");
		}
	}
}

pub fn write_mir_terminator(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, term: &MirTerminator) -> fmt::Result
{
	w.write_indent(f)?;
	match term {
		MirTerminator::Goto { target } => {
			return writeln!(f, "goto bb{};", target.0);
		}
		MirTerminator::Branch {
			cond,
			then_block,
			else_block,
		} => {
			write!(f, "if ")?;
			write_mir_operand(f, cond)?;
			return writeln!(f, " {{ goto bb{} }} else {{ goto bb{} }}", then_block.0, else_block.0);
		}
		MirTerminator::CallAndContinue {
			callee,
			type_args,
			named_generics,
			args,
			dest,
			next,
			unwind,
			..
		} => {
			write_mir_place(f, dest)?;
			write!(f, " = ")?;
			write_mir_callee(f, callee)?;
			write_call_generics(f, type_args, named_generics)?;
			write!(f, "(")?;
			for (i, arg) in args.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_mir_operand(f, arg)?;
			}
			write!(f, ") -> bb{}", next.0)?;
			if let Some(uw) = unwind {
				write!(f, " unwind bb{}", uw.0)?;
			}
			return writeln!(f, ";");
		}
		MirTerminator::Return => {
			return writeln!(f, "return;");
		}
		MirTerminator::Unreachable => {
			return writeln!(f, "unreachable;");
		}
		MirTerminator::Switch {
			scrutinee,
			arms,
			otherwise,
		} => {
			write!(f, "switch ")?;
			write_mir_operand(f, scrutinee)?;
			writeln!(f, " {{")?;
			w.indent();
			for arm in arms {
				write_mir_switch_arm(f, w, arm)?;
			}
			w.write_indent(f)?;
			writeln!(f, "_ => bb{},", otherwise.0)?;
			w.dedent();
			w.write_indent(f)?;
			return writeln!(f, "}}");
		}
	}
}

fn write_mir_switch_arm(f: &mut fmt::Formatter<'_>, w: &IndentWriter, arm: &MirSwitchArm) -> fmt::Result
{
	w.write_indent(f)?;
	write_mir_operand(f, &arm.value)?;
	return writeln!(f, " => bb{},", arm.target.0);
}

pub fn write_mir_place(f: &mut fmt::Formatter<'_>, place: &MirPlace) -> fmt::Result
{
	match &place.base {
		MirPlaceBase::Local(id) => write!(f, "_{}", id.0)?,
		MirPlaceBase::Global(sym) => write!(f, "global({:?})", sym)?,
	}
	for proj in &place.projections {
		match proj {
			MirProjection::Field { name, .. } => write!(f, ".{}", name)?,
			MirProjection::Index { index, .. } => write!(f, "[_{}]", index.0)?,
			MirProjection::Deref => write!(f, ".*")?,
		}
	}
	return Ok(());
}

pub fn write_mir_operand(f: &mut fmt::Formatter<'_>, operand: &MirOperand) -> fmt::Result
{
	match operand {
		MirOperand::Copy(place) => {
			write!(f, "copy ")?;
			return write_mir_place(f, place);
		}
		MirOperand::Move(place) => {
			write!(f, "move ")?;
			return write_mir_place(f, place);
		}
		MirOperand::Const(lit) => return write_mir_literal(f, lit),
	}
}

pub fn write_mir_literal(f: &mut fmt::Formatter<'_>, lit: &MirLiteral) -> fmt::Result
{
	match &lit.value {
		MirLiteralValue::Literal(l) => write!(f, "{}", l)?,
		MirLiteralValue::ZeroInit => write!(f, "zeroinit")?,
		MirLiteralValue::Undef => write!(f, "undef")?,
		MirLiteralValue::ConstBody(id) => write!(f, "const#{}", id.0)?,
	}
	return write!(f, ": {}", lit.ty);
}

pub fn write_mir_rvalue(f: &mut fmt::Formatter<'_>, rvalue: &MirRvalue) -> fmt::Result
{
	match rvalue {
		MirRvalue::Use(op) => return write_mir_operand(f, op),

		MirRvalue::Unary { op, operand } => {
			write!(f, "{}", op)?;
			return write_mir_operand(f, operand);
		}

		MirRvalue::Binary { op, lhs, rhs } => {
			write!(f, "(")?;
			write_mir_operand(f, lhs)?;
			write!(f, " {} ", op)?;
			write_mir_operand(f, rhs)?;
			return write!(f, ")");
		}

		MirRvalue::Cast { ty, operand } => {
			write!(f, "({}) ", ty)?;
			return write_mir_operand(f, operand);
		}

		MirRvalue::Ref { mutable, place } => {
			if *mutable {
				write!(f, "&mut ")?;
			} else {
				write!(f, "&")?;
			}
			return write_mir_place(f, place);
		}

		MirRvalue::RawPtr { mutable, place } => {
			if *mutable {
				write!(f, "*mut ")?;
			} else {
				write!(f, "*const ")?;
			}
			return write_mir_place(f, place);
		}

		MirRvalue::Aggregate { kind, fields } => {
			match kind {
				MirAggregateKind::Struct(sym) => write!(f, "struct({:?})", sym)?,
				MirAggregateKind::Union(sym) => write!(f, "union({:?})", sym)?,
				MirAggregateKind::VariantMember { parent, member } => write!(f, "variant({:?}::{})", parent, member)?,
				MirAggregateKind::Tuple => write!(f, "tuple")?,
			}
			write!(f, " {{")?;
			for (i, (name, op)) in fields.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}: ", name)?;
				write_mir_operand(f, op)?;
			}
			return write!(f, "}}");
		}

		MirRvalue::Array { elements, elem_ty } => {
			write!(f, "[{}: ", elem_ty)?;
			for (i, el) in elements.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_mir_operand(f, el)?;
			}
			return write!(f, "]");
		}

		MirRvalue::ArrayRepeat { value, count, elem_ty } => {
			write!(f, "[{}: ", elem_ty)?;
			write_mir_operand(f, value)?;
			write!(f, "; ")?;
			write_mir_operand(f, count)?;
			return write!(f, "]");
		}

		MirRvalue::Tuple(elements) => {
			write!(f, "(")?;
			for (i, el) in elements.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_mir_operand(f, el)?;
			}
			return write!(f, ")");
		}

		MirRvalue::Discriminant(place) => {
			write!(f, "discriminant(")?;
			write_mir_place(f, place)?;
			return write!(f, ")");
		}
	}
}

pub fn write_mir_callee(f: &mut fmt::Formatter<'_>, callee: &MirCallee) -> fmt::Result
{
	match callee {
		MirCallee::Direct(sym) => return write!(f, "{:?}", sym),
		MirCallee::Indirect(local) => return write!(f, "(*_{})", local.0),
		MirCallee::Intrinsic(intrinsic) => return write!(f, "{}", intrinsic),
	}
}
