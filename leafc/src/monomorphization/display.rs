use std::fmt;

use crate::utils::indent_writer::IndentWriter;

use super::{
	ConstBodyId, MonoAggregateKind, MonoBasicBlock, MonoBody, MonoCallee, MonoConstBody, MonoFunction, MonoGlobal,
	MonoItem, MonoLiteral, MonoLocal, MonoModule, MonoOperand, MonoPlace, MonoPlaceBase, MonoProjection, MonoRvalue,
	MonoStmt, MonoSwitchArm, MonoTerminator, MonoTypeDef, MonoTypeDefKind,
};

impl fmt::Display for MonoModule
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
			write_mono_item(f, &mut w, item)?;
			writeln!(f)?;
		}

		if !self.const_bodies.is_empty() {
			writeln!(f, "// ---- const bodies ----")?;
			for (i, cb) in self.const_bodies.iter().enumerate() {
				#[allow(clippy::cast_possible_truncation)]
				write_mono_const_body(f, &mut w, ConstBodyId(i as u32), cb)?;
				writeln!(f)?;
			}
		}
		return Ok(());
	}
}

pub fn write_mono_const_body(
	f: &mut fmt::Formatter<'_>,
	w: &mut IndentWriter,
	id: ConstBodyId,
	cb: &MonoConstBody,
) -> fmt::Result
{
	let result_local = cb.body.locals.iter().find(|l| return l.id == cb.result);
	let ty_str = result_local.map_or_else(|| return "?".to_string(), |l| format!("{:?}", l.ty));
	writeln!(f, "const#{} -> _{}: {} {{", id.0, cb.result.0, ty_str)?;
	w.indent();
	write_mono_body(f, w, &cb.body)?;
	w.dedent();
	return writeln!(f, "}}");
}

pub fn write_mono_item(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, item: &MonoItem) -> fmt::Result
{
	match item {
		MonoItem::Function(func) => return write_mono_function(f, w, func),
		MonoItem::Global(global) => return write_mono_global(f, w, global),
		MonoItem::TypeDef(typedef) => return write_mono_typedef(f, w, typedef),
	}
}

pub fn write_mono_global(f: &mut fmt::Formatter<'_>, _w: &mut IndentWriter, global: &MonoGlobal) -> fmt::Result
{
	write!(f, "global ")?;
	if global.mutable {
		write!(f, "mut ")?;
	}
	return writeln!(
		f,
		"{} [{}]: {:?} = const#{};  // {:?}",
		global.mangled_name, global.mangled_name, global.ty, global.init.0, global.symbol
	);
}

pub fn write_mono_typedef(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, typedef: &MonoTypeDef) -> fmt::Result
{
	match &typedef.kind {
		MonoTypeDefKind::Struct { fields } => {
			writeln!(f, "struct {} {{  // {:?}", typedef.mangled_name, typedef.symbol)?;
			w.indent();
			for (name, ty) in fields {
				w.write_indent(f)?;
				writeln!(f, "{}: {:?},", name, ty)?;
			}
			w.dedent();
			return writeln!(f, "}}");
		}
		MonoTypeDefKind::Union { fields } => {
			writeln!(f, "union {} {{  // {:?}", typedef.mangled_name, typedef.symbol)?;
			w.indent();
			for (name, ty) in fields {
				w.write_indent(f)?;
				writeln!(f, "{}: {:?},", name, ty)?;
			}
			w.dedent();
			return writeln!(f, "}}");
		}
		MonoTypeDefKind::Enum { variants } => {
			writeln!(f, "enum {} {{  // {:?}", typedef.mangled_name, typedef.symbol)?;
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
		MonoTypeDefKind::Variant { members } => {
			writeln!(f, "variant {} {{  // {:?}", typedef.mangled_name, typedef.symbol)?;
			w.indent();
			for (name, ty) in members {
				w.write_indent(f)?;
				if let Some(t) = ty {
					writeln!(f, "{}({:?}),", name, t)?;
				} else {
					writeln!(f, "{},  // zst", name)?;
				}
			}
			w.dedent();
			return writeln!(f, "}}");
		}
		MonoTypeDefKind::TypeAlias { ty } => {
			return writeln!(f, "type {} = {:?};  // {:?}", typedef.mangled_name, ty, typedef.symbol);
		}
	}
}

pub fn write_mono_function(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, func: &MonoFunction) -> fmt::Result
{
	write!(f, "fn {}", func.mangled_name)?;
	write!(f, "(")?;
	for (i, param) in func.params.iter().enumerate() {
		if i > 0 {
			write!(f, ", ")?;
		}
		if param.mutable {
			write!(f, "mut ")?;
		}
		write!(f, "{}: {:?} [_{:?}]", param.name, param.ty, param.local.0)?;
	}
	write!(f, ") -> ")?;
	match &func.return_ty {
		Some(ty) => write!(f, "{:?}", ty)?,
		None => write!(f, "void")?,
	}
	writeln!(f, "  // {:?}", func.symbol)?;

	match &func.body {
		None => {
			return writeln!(f, ";");
		}
		Some(body) => {
			writeln!(f, "{{")?;
			w.indent();
			write_mono_body(f, w, body)?;
			w.dedent();
			return writeln!(f, "}}");
		}
	}
}

pub fn write_mono_body(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, body: &MonoBody) -> fmt::Result
{
	w.write_indent(f)?;
	writeln!(f, "// locals:")?;
	for local in &body.locals {
		write_mono_local_decl(f, w, local, body.param_count)?;
	}
	if let Some(ret) = body.return_local {
		w.write_indent(f)?;
		writeln!(f, "// return => _{}", ret.0)?;
	}
	writeln!(f)?;

	for block in &body.blocks {
		write_mono_block(f, w, block)?;
		writeln!(f)?;
	}
	return Ok(());
}

fn write_mono_local_decl(
	f: &mut fmt::Formatter<'_>,
	w: &IndentWriter,
	local: &MonoLocal,
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
	return writeln!(f, ": {:?};", local.ty);
}

pub fn write_mono_block(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, block: &MonoBasicBlock) -> fmt::Result
{
	w.write_indent(f)?;
	writeln!(f, "bb{}: {{", block.id.0)?;
	w.indent();

	for stmt in &block.stmts {
		write_mono_stmt(f, w, stmt)?;
	}

	write_mono_terminator(f, w, &block.terminator)?;

	w.dedent();
	w.write_indent(f)?;
	return writeln!(f, "}}");
}

pub fn write_mono_stmt(f: &mut fmt::Formatter<'_>, w: &IndentWriter, stmt: &MonoStmt) -> fmt::Result
{
	w.write_indent(f)?;
	match stmt {
		MonoStmt::Assign { place, rvalue, .. } => {
			write_mono_place(f, place)?;
			write!(f, " = ")?;
			write_mono_rvalue(f, rvalue)?;
			return writeln!(f, ";");
		}
		MonoStmt::Call { callee, args, .. } => {
			write_mono_callee(f, callee)?;
			write!(f, "(")?;
			for (i, arg) in args.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_mono_operand(f, arg)?;
			}
			return writeln!(f, ");");
		}
		MonoStmt::Delete { operand, .. } => {
			write!(f, "delete ")?;
			write_mono_operand(f, operand)?;
			return writeln!(f, ";");
		}
		MonoStmt::Nop => {
			return writeln!(f, "nop;");
		}
	}
}

pub fn write_mono_terminator(f: &mut fmt::Formatter<'_>, w: &mut IndentWriter, term: &MonoTerminator) -> fmt::Result
{
	w.write_indent(f)?;
	match term {
		MonoTerminator::Goto { target } => {
			return writeln!(f, "goto bb{};", target.0);
		}
		MonoTerminator::Branch {
			cond,
			then_block,
			else_block,
		} => {
			write!(f, "if ")?;
			write_mono_operand(f, cond)?;
			return writeln!(f, " {{ goto bb{} }} else {{ goto bb{} }}", then_block.0, else_block.0);
		}
		MonoTerminator::CallAndContinue {
			callee,
			args,
			dest,
			next,
			unwind,
			..
		} => {
			write_mono_place(f, dest)?;
			write!(f, " = ")?;
			write_mono_callee(f, callee)?;
			write!(f, "(")?;
			for (i, arg) in args.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_mono_operand(f, arg)?;
			}
			write!(f, ") -> bb{}", next.0)?;
			if let Some(uw) = unwind {
				write!(f, " unwind bb{}", uw.0)?;
			}
			return writeln!(f, ";");
		}
		MonoTerminator::Return => {
			return writeln!(f, "return;");
		}
		MonoTerminator::Unreachable => {
			return writeln!(f, "unreachable;");
		}
		MonoTerminator::Switch {
			scrutinee,
			arms,
			otherwise,
		} => {
			write!(f, "switch ")?;
			write_mono_operand(f, scrutinee)?;
			writeln!(f, " {{")?;
			w.indent();
			for arm in arms {
				write_mono_switch_arm(f, w, arm)?;
			}
			w.write_indent(f)?;
			writeln!(f, "_ => bb{},", otherwise.0)?;
			w.dedent();
			w.write_indent(f)?;
			return writeln!(f, "}}");
		}
	}
}

fn write_mono_switch_arm(f: &mut fmt::Formatter<'_>, w: &IndentWriter, arm: &MonoSwitchArm) -> fmt::Result
{
	w.write_indent(f)?;
	write_mono_operand(f, &arm.value)?;
	return writeln!(f, " => bb{},", arm.target.0);
}

pub fn write_mono_place(f: &mut fmt::Formatter<'_>, place: &MonoPlace) -> fmt::Result
{
	match &place.base {
		MonoPlaceBase::Local(id) => write!(f, "_{}", id.0)?,
		MonoPlaceBase::Global(sym) => write!(f, "global({:?})", sym)?,
	}
	for proj in &place.projections {
		match proj {
			MonoProjection::Field { name, .. } => write!(f, ".{}", name)?,
			MonoProjection::Index { index, .. } => write!(f, "[_{}]", index.0)?,
			MonoProjection::Deref => write!(f, ".*")?,
		}
	}
	return Ok(());
}

pub fn write_mono_operand(f: &mut fmt::Formatter<'_>, operand: &MonoOperand) -> fmt::Result
{
	match operand {
		MonoOperand::Copy(place) => {
			write!(f, "copy ")?;
			return write_mono_place(f, place);
		}
		MonoOperand::Move(place) => {
			write!(f, "move ")?;
			return write_mono_place(f, place);
		}
		MonoOperand::Const(lit) => return write_mono_literal(f, lit),
	}
}

pub fn write_mono_literal(f: &mut fmt::Formatter<'_>, lit: &MonoLiteral) -> fmt::Result
{
	use crate::mir::MirLiteralValue;
	match &lit.value {
		MirLiteralValue::Literal(l) => write!(f, "{}", l)?,
		MirLiteralValue::ZeroInit => write!(f, "zeroinit")?,
		MirLiteralValue::Undef => write!(f, "undef")?,
		MirLiteralValue::ConstBody(id) => write!(f, "const#{}", id.0)?,
	}
	return write!(f, ": {:?}", lit.ty);
}

pub fn write_mono_rvalue(f: &mut fmt::Formatter<'_>, rvalue: &MonoRvalue) -> fmt::Result
{
	match rvalue {
		MonoRvalue::Use(op) => return write_mono_operand(f, op),

		MonoRvalue::Unary { op, operand } => {
			write!(f, "{}", op)?;
			return write_mono_operand(f, operand);
		}

		MonoRvalue::Binary { op, lhs, rhs } => {
			write!(f, "(")?;
			write_mono_operand(f, lhs)?;
			write!(f, " {} ", op)?;
			write_mono_operand(f, rhs)?;
			return write!(f, ")");
		}

		MonoRvalue::Cast { ty, operand } => {
			write!(f, "({:?}) ", ty)?;
			return write_mono_operand(f, operand);
		}

		MonoRvalue::Ref { mutable, place } => {
			if *mutable {
				write!(f, "&mut ")?;
			} else {
				write!(f, "&")?;
			}
			return write_mono_place(f, place);
		}

		MonoRvalue::RawPtr { mutable, place } => {
			if *mutable {
				write!(f, "*mut ")?;
			} else {
				write!(f, "*const ")?;
			}
			return write_mono_place(f, place);
		}

		MonoRvalue::Aggregate { kind, fields } => {
			match kind {
				MonoAggregateKind::Struct { symbol, mangled_name } => write!(f, "struct({})", mangled_name)?,
				MonoAggregateKind::Union { symbol, mangled_name } => write!(f, "union({})", mangled_name)?,
				MonoAggregateKind::VariantMember {
					parent,
					member,
					parent_mangled,
				} => write!(f, "variant({}::{})", parent_mangled, member)?,
				MonoAggregateKind::Tuple => write!(f, "tuple")?,
			}
			write!(f, " {{")?;
			for (i, (name, op)) in fields.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write!(f, "{}: ", name)?;
				write_mono_operand(f, op)?;
			}
			return write!(f, "}}");
		}

		MonoRvalue::Array { elements, elem_ty } => {
			write!(f, "[{:?}: ", elem_ty)?;
			for (i, el) in elements.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_mono_operand(f, el)?;
			}
			return write!(f, "]");
		}

		MonoRvalue::ArrayRepeat { value, count, elem_ty } => {
			write!(f, "[{:?}: ", elem_ty)?;
			write_mono_operand(f, value)?;
			write!(f, "; ")?;
			writeln!(f, "const#{},", count.0)?;
			return write!(f, "]");
		}

		MonoRvalue::Tuple(elements) => {
			write!(f, "(")?;
			for (i, el) in elements.iter().enumerate() {
				if i > 0 {
					write!(f, ", ")?;
				}
				write_mono_operand(f, el)?;
			}
			return write!(f, ")");
		}

		MonoRvalue::Discriminant(place) => {
			write!(f, "discriminant(")?;
			write_mono_place(f, place)?;
			return write!(f, ")");
		}
	}
}

pub fn write_mono_callee(f: &mut fmt::Formatter<'_>, callee: &MonoCallee) -> fmt::Result
{
	match callee {
		MonoCallee::Direct {
			symbol,
			type_args,
			mangled_name,
		} => return write!(f, "{}", mangled_name),
		MonoCallee::Indirect(local) => return write!(f, "(*_{})", local.0),
		MonoCallee::Intrinsic(intrinsic) => return write!(f, "{}", intrinsic),
	}
}
