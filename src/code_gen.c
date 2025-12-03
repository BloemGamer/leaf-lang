#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "basic_types.h"
#include "code_gen.h"
#include "parser.h"
#include "tokens.h"
#include "utils.h"

#define TMP_VAR_PREFIX "sl_tmp_"
static constexpr i64 MAX_BUFFER_SIZE = 64;

static constexpr bool GEN_LINE = true;

static void gen_code(CodeGen* code_gen, AST* ast);

static void gen_ast_block(CodeGen code_gen[static 1], Block block, const char* add_before_trailing_expr);
static void gen_ast_func_def(CodeGen code_gen[static 1], FuncDef func_def);
static void gen_ast_var_def(CodeGen code_gen[static 1], VarDef var_def);
static void gen_ast_literal(CodeGen code_gen[static 1], Literal literal);
static void gen_ast_break_stmt(CodeGen code_gen[static 1]);
static void gen_ast_continue_stmt(CodeGen code_gen[static 1]);
static void gen_ast_return_stmt(CodeGen code_gen[static 1], ReturnStmt return_stmt);
static void gen_ast_struct_def(CodeGen code_gen[static 1], StructDef struct_def);
static void gen_ast_union_def(CodeGen code_gen[static 1], UnionDef union_def);
static void gen_ast_enum_def(CodeGen code_gen[static 1], EnumDef enum_def);
static void gen_ast_member_access(CodeGen code_gen[static 1], MemberAccess member_access);
static void gen_ast_func_call(CodeGen code_gen[static 1], FuncCall func_call);
static void gen_ast_identifier(CodeGen code_gen[static 1], Identifier identifier);
static void gen_ast_binary_expr(CodeGen code_gen[static 1], BinaryExpr binary_expr);
static void gen_ast_unary_expr(CodeGen code_gen[static 1], UnaryExpr unary_expr);
static void gen_ast_cast_expr(CodeGen code_gen[static 1], CastExpr cast_expr);
static void gen_ast_cast_block(CodeGen code_gen[static 1], CastExpr cast_expr, const char* add_before_trailing_expr);
static void gen_ast_index_expr(CodeGen code_gen[static 1], IndexExpr index_expr);
static void gen_ast_array_init(CodeGen code_gen[static 1], ArrayInit array_init);
static void gen_ast_if_expr(CodeGen code_gen[static 1], IfExpr if_expr, const char* add_before_trailing_expr);
static void gen_ast_while_expr(CodeGen code_gen[static 1], WhileExpr while_expr);
static void gen_ast_for_expr(CodeGen code_gen[static 1], ForExpr for_expr);
static void gen_ast_struct_init(CodeGen code_gen[static 1], StructInit struct_init);
static void gen_ast_message(CodeGen code_gen[static 1], Message message);

static void gen_type(CodeBlock* code_block, VarType var_type);
static void gen_var_def(CodeGen code_gen[static 1], VarDef var_def);
static void gen_var_def_with_const(CodeGen code_gen[static 1], VarDef var_def, bool const_var);
static void gen_func_signature(CodeGen code_gen[static 1], FuncDef func_def);

static CodeBlock* get_code_block(CodeGen code_gen[static 1]);
static bool casted_block(CastExpr cast_expr);
static void str_cat(CodeBlock* code_block, const char* str);
static void str_cat_escaped(CodeBlock* code_block, const char* str);

static void free_code_block(CodeBlock code_block);

static void emit_line_directive(CodeGen* code_gen, Pos pos);

CodeGen generate_code(AST* ast, const char* filename)
{
	CodeGen code_gen = {.current_block = CODE_BLOCK_NONE, .source_filename = filename};

	str_cat(&code_gen.includes, "#include <assert.h>\n");
	str_cat(&code_gen.includes, "#include <basic_types.h>\n");
	gen_code(&code_gen, ast);

	return code_gen;
}

static void gen_code(CodeGen* code_gen, AST* ast)
{
	if (ast == nullptr)
	{
		return;
	}
	if (code_gen == nullptr)
	{
		assert(code_gen != nullptr);
	}

	switch (ast->type)
	{
		case AST_VAR_DEF:
			emit_line_directive(code_gen, ast->pos);
			gen_ast_var_def(code_gen, ast->node.var_def);
			return;
		case AST_FUNC_DEF:
			emit_line_directive(code_gen, ast->pos);
			gen_ast_func_def(code_gen, ast->node.func_def);
			return;
		case AST_IF_EXPR:
			emit_line_directive(code_gen, ast->pos);
			gen_ast_if_expr(code_gen, ast->node.if_expr, nullptr);
			return;
		case AST_WHILE_EXPR:
			emit_line_directive(code_gen, ast->pos);
			gen_ast_while_expr(code_gen, ast->node.while_expr);
			return;
		case AST_FOR_EXPR:
			emit_line_directive(code_gen, ast->pos);
			gen_ast_for_expr(code_gen, ast->node.for_expr);
			return;
		case AST_RETURN_STMT:
			emit_line_directive(code_gen, ast->pos);
			gen_ast_return_stmt(code_gen, ast->node.return_stmt);
			return;
		case AST_BREAK_STMT:
			emit_line_directive(code_gen, ast->pos);
			gen_ast_break_stmt(code_gen);
			return;
		case AST_CONTINUE_STMT:
			emit_line_directive(code_gen, ast->pos);
			gen_ast_continue_stmt(code_gen);
			return;

		case AST_FUNC_CALL:
			if (code_gen->current_block == CODE_BLOCK_CODE)
			{
				emit_line_directive(code_gen, ast->pos);
			}
			gen_ast_func_call(code_gen, ast->node.func_call);
			return;

		case AST_BLOCK:
			gen_ast_block(code_gen, ast->node.block, nullptr);
			return;
		case AST_LITERAL:
			gen_ast_literal(code_gen, ast->node.literal);
			return;
		case AST_STRUCT_DEF:
			gen_ast_struct_def(code_gen, ast->node.struct_def);
			return;
		case AST_UNION_DEF:
			gen_ast_union_def(code_gen, ast->node.union_def);
			return;
		case AST_ENUM_DEF:
			gen_ast_enum_def(code_gen, ast->node.enum_def);
			return;
		case AST_MEMBER_ACCESS:
			gen_ast_member_access(code_gen, ast->node.member_access);
			return;
		case AST_IDENTIFIER:
			gen_ast_identifier(code_gen, ast->node.identifier);
			return;
		case AST_BINARY_EXPR:
			gen_ast_binary_expr(code_gen, ast->node.binary_expr);
			return;
		case AST_UNARY:
			gen_ast_unary_expr(code_gen, ast->node.unary_expr);
			return;
		case AST_CAST_EXPR:
			gen_ast_cast_expr(code_gen, ast->node.cast_expr);
			return;
		case AST_INDEX_EXPR:
			gen_ast_index_expr(code_gen, ast->node.index_expr);
			return;
		case AST_ARRAY_INIT:
			gen_ast_array_init(code_gen, ast->node.array_init);
			return;
		case AST_RANGE_EXPR:
			assert(false && "code gen: range expression should not be used outside of for statement");
			return;
		case AST_STRUCT_INIT:
			gen_ast_struct_init(code_gen, ast->node.struct_init);
			return;
		case AST_MESSAGE:
			gen_ast_message(code_gen, ast->node.message);
			return;
	}
	assert(false && "code gen: not yet implemented");
}

static void gen_ast_block(CodeGen code_gen[static 1], Block block, const char* add_before_trailing_expr)
{
	CodeBlock* code_block = get_code_block(code_gen);

	if (!block.global && !code_gen->skip_brace)
	{
		code_block = get_code_block(code_gen);
		str_cat(code_block, "{");
	}
	bool prev_global_block = code_gen->global_block;
	code_gen->global_block = block.global;
	bool prev_skip_brace = code_gen->skip_brace;
	code_gen->skip_brace = false;
	for (usize i = 0; i < block.statement_count; i++)
	{
		code_block = get_code_block(code_gen);
		gen_code(code_gen, block.statements[i]);
		switch (block.statements[i]->type)
		{
			case AST_IDENTIFIER:
			case AST_MEMBER_ACCESS:
			case AST_ARRAY_INIT:
			case AST_BREAK_STMT:
			case AST_CONTINUE_STMT:
			case AST_RETURN_STMT:
			case AST_BINARY_EXPR:
			case AST_FUNC_CALL:
			case AST_LITERAL:
			case AST_RANGE_EXPR:
			case AST_CAST_EXPR:
			case AST_STRUCT_INIT:
				str_cat(code_block, ";");
				break;
			case AST_STRUCT_DEF:
			case AST_UNION_DEF:
			case AST_ENUM_DEF:
			case AST_VAR_DEF:
			case AST_FUNC_DEF:
			case AST_INDEX_EXPR:
			case AST_IF_EXPR:
			case AST_FOR_EXPR:
			case AST_WHILE_EXPR:
			case AST_BLOCK:
			case AST_UNARY:
			case AST_MESSAGE:
				break;
		}
	}
	code_gen->global_block = prev_global_block;

	if (add_before_trailing_expr != nullptr)
	{
		assert(block.trailing_expr != nullptr);
		str_cat(code_block, add_before_trailing_expr);

		switch (block.trailing_expr->type)
		{
			case AST_IDENTIFIER:
			case AST_MEMBER_ACCESS:
			case AST_ARRAY_INIT:
			case AST_BREAK_STMT:
			case AST_CONTINUE_STMT:
			case AST_RETURN_STMT:
			case AST_BINARY_EXPR:
			case AST_FUNC_CALL:
			case AST_LITERAL:
			case AST_RANGE_EXPR:
			case AST_CAST_EXPR:
			case AST_STRUCT_INIT:
			case AST_STRUCT_DEF:
			case AST_UNION_DEF:
			case AST_ENUM_DEF:
			case AST_VAR_DEF:
			case AST_FUNC_DEF:
			case AST_INDEX_EXPR:
			case AST_UNARY:
			case AST_MESSAGE:
			case AST_FOR_EXPR:
			case AST_WHILE_EXPR:
				gen_code(code_gen, block.trailing_expr);
				for (usize i = 0; i < code_gen->close_paren_count; i++)
				{
					str_cat(code_block, ")");
				}
				str_cat(code_block, ";");
				break;
			case AST_IF_EXPR:
			case AST_BLOCK:
				gen_ast_block(code_gen, block.trailing_expr->node.block, add_before_trailing_expr);
				break;
		}
	}
	else if (block.trailing_expr != nullptr)
	{
		code_block = get_code_block(code_gen);
		gen_code(code_gen, block.trailing_expr);
		switch (block.trailing_expr->type)
		{
			case AST_IDENTIFIER:
			case AST_MEMBER_ACCESS:
			case AST_ARRAY_INIT:
			case AST_BREAK_STMT:
			case AST_CONTINUE_STMT:
			case AST_RETURN_STMT:
			case AST_BINARY_EXPR:
			case AST_FUNC_CALL:
			case AST_LITERAL:
			case AST_RANGE_EXPR:
			case AST_CAST_EXPR:
			case AST_STRUCT_INIT:
				str_cat(code_block, ";");
				break;
			case AST_STRUCT_DEF:
			case AST_UNION_DEF:
			case AST_ENUM_DEF:
			case AST_VAR_DEF:
			case AST_FUNC_DEF:
			case AST_INDEX_EXPR:
			case AST_IF_EXPR:
			case AST_FOR_EXPR:
			case AST_WHILE_EXPR:
			case AST_BLOCK:
			case AST_UNARY:
			case AST_MESSAGE:
				break;
		}
	}

	if (!block.global && !prev_skip_brace)
	{
		str_cat(code_block, "}");
	}
	code_gen->skip_brace = prev_skip_brace;
}

static void gen_ast_func_def(CodeGen code_gen[static 1], FuncDef func_def)
{
	CodeBlock* code_block = nullptr;

	code_gen->current_block = CODE_BLOCK_PRIV_FUNCTIONS;
	code_block = &code_gen->priv_functions;
	for (usize i = 0; i < func_def.modifier_count; i++)
	{
		if (func_def.modifiers[i].token_type == token_type_pub)
		{
			code_gen->current_block = CODE_BLOCK_PUB_FUNCTIONS;
			code_block = &code_gen->pub_functions;
			break;
		}
	}

	if (strcmp(func_def.name, "main") != 0)
	{
		if (code_gen->current_block == CODE_BLOCK_PRIV_FUNCTIONS)
		{
			str_cat(code_block, "static ");
		}
		else
		{
			str_cat(code_block, "extern ");
		}
		gen_func_signature(code_gen, func_def);
		str_cat(code_block, ";");
		if (code_gen->current_block == CODE_BLOCK_PRIV_FUNCTIONS)
		{
			str_cat(&code_gen->code, "static ");
		}
	}

	code_gen->current_block = CODE_BLOCK_CODE;
	gen_func_signature(code_gen, func_def);

	gen_code(code_gen, func_def.body);

	code_gen->current_block = CODE_BLOCK_NONE;
}

static void gen_ast_var_def(CodeGen code_gen[static 1], VarDef var_def)
{
	CodeBlock* code_block = nullptr;

	if (code_gen->global_block)
	{
		code_gen->current_block = CODE_BLOCK_PRIV_VARS;
		code_block = &code_gen->priv_vars;
		for (usize i = 0; i < var_def.modifier_count; i++)
		{
			if (var_def.modifiers[i].token_type == token_type_pub)
			{
				code_gen->current_block = CODE_BLOCK_PUB_VARS;
				code_block = &code_gen->pub_vars;
				break;
			}
		}
	}
	else
	{
		code_block = get_code_block(code_gen);
		if (code_block == nullptr)
		{
			code_block = &code_gen->code;
			code_gen->current_block = CODE_BLOCK_CODE;
		}
	}
	char tmp_var[MAX_BUFFER_SIZE] = {};
	if (var_def.equals != nullptr && (var_def.equals->type == AST_BLOCK || var_def.equals->type == AST_IF_EXPR))
	{
		assert(code_gen->current_block == CODE_BLOCK_CODE,
			   "(for now) you can't have a global variable with a block or if initialiser");
		int tmp_var_len = snprintf(tmp_var, MAX_BUFFER_SIZE - 1, TMP_VAR_PREFIX "ret_%" PRIu32, code_gen->tmp_num);
		code_gen->tmp_num++;

		gen_type(code_block, var_def.type);
		str_cat(code_block, " ");
		str_cat(code_block, tmp_var);
		str_cat(code_block, ";");
		strcat(tmp_var, "=");
		if (var_def.equals->type == AST_BLOCK)
		{
			gen_ast_block(code_gen, var_def.equals->node.block, tmp_var);
		}
		else if (var_def.equals->type == AST_IF_EXPR)
		{
			gen_ast_if_expr(code_gen, var_def.equals->node.if_expr, tmp_var);
		}
		tmp_var[tmp_var_len] = '\0';
	}
	if (code_gen->current_block == CODE_BLOCK_PUB_VARS)
	{
		str_cat(code_block, "extern ");
		gen_var_def(code_gen, var_def);
		str_cat(code_block, ";");
		code_gen->current_block = CODE_BLOCK_PRIV_VARS;
		code_block = get_code_block(code_gen);
	}
	else if (code_gen->current_block == CODE_BLOCK_PRIV_VARS)
	{
		str_cat(code_block, "static ");
	}
	gen_var_def(code_gen, var_def);

	if (var_def.equals == nullptr)
	{
		str_cat(code_block, ";");
		code_gen->current_block = CODE_BLOCK_NONE;
		return;
	}
	str_cat(code_block, "=");

	if (var_def.equals != nullptr && (var_def.equals->type == AST_BLOCK || var_def.equals->type == AST_IF_EXPR))
	{
		str_cat(code_block, tmp_var);
	}
	else
	{
		gen_code(code_gen, var_def.equals);
	}

	if (!code_gen->no_semicolon)
	{
		str_cat(code_block, ";");
	}
	code_gen->current_block = CODE_BLOCK_NONE;
}

static void gen_ast_literal(CodeGen code_gen[static 1], Literal literal)
{
	Token token = literal.literal;
	CodeBlock* code_block = get_code_block(code_gen);
	if (code_block == nullptr)
	{
		code_block = &code_gen->code;
	}
	char buffer[MAX_BUFFER_SIZE] = {0};
	switch (token.token_type)
	{
		case token_type_string:
			str_cat(code_block, "\"");
			str_cat_escaped(code_block, token.str_val);
			str_cat(code_block, "\"");
			break;
		case token_type_number:
			(void)snprintf(buffer, MAX_BUFFER_SIZE - 1, "%" PRId64, token.num_val);
			str_cat(code_block, buffer);
			break;
		case token_type_char:
			(void)snprintf(buffer, MAX_BUFFER_SIZE - 1, "0x%02x", token.char_val);
			str_cat(code_block, buffer);
			break;
		case token_type_float:
			(void)snprintf(buffer, MAX_BUFFER_SIZE - 1, "%lf", token.float_val);
			str_cat(code_block, buffer);
			break;
		case token_type_true:
			str_cat(code_block, "true");
			break;
		case token_type_false:
			str_cat(code_block, "false");
			break;
		default:
			assert(false && "code gen: not an literal");
	}
}

static void gen_ast_break_stmt(CodeGen code_gen[static 1])
{
	str_cat(&code_gen->code, "break");
}

static void gen_ast_continue_stmt(CodeGen code_gen[static 1])
{
	str_cat(&code_gen->code, "continue");
}

static void gen_ast_return_stmt(CodeGen code_gen[static 1], ReturnStmt return_stmt)
{
	str_cat(&code_gen->code, "return ");
	gen_code(code_gen, return_stmt.return_stmt);
}

static void gen_ast_struct_def(CodeGen code_gen[static 1], StructDef struct_def)
{
	CodeBlock* code_block = nullptr;

	code_gen->current_block = CODE_BLOCK_PRIV_TYPES;
	code_block = &code_gen->priv_types;
	for (usize i = 0; i < struct_def.modifier_count; i++)
	{
		if (struct_def.modifiers[i].token_type == token_type_pub)
		{
			code_gen->current_block = CODE_BLOCK_PUB_TYPES;
			code_block = &code_gen->pub_types;
			break;
		}
	}
	if (code_gen->current_block == CODE_BLOCK_PUB_TYPES)
	{
		str_cat(&code_gen->pub_typedefs, "typedef struct ");
		str_cat(&code_gen->pub_typedefs, struct_def.name);
		str_cat(&code_gen->pub_typedefs, " ");
		str_cat(&code_gen->pub_typedefs, struct_def.name);
		str_cat(&code_gen->pub_typedefs, ";");
	}
	else
	{
		str_cat(&code_gen->priv_typedefs, "typedef struct ");
		str_cat(&code_gen->priv_typedefs, struct_def.name);
		str_cat(&code_gen->priv_typedefs, " ");
		str_cat(&code_gen->priv_typedefs, struct_def.name);
		str_cat(&code_gen->priv_typedefs, ";");
	}
	str_cat(code_block, "struct ");
	str_cat(code_block, struct_def.name);
	str_cat(code_block, "{");

	for (usize i = 0; i < struct_def.member_count; i++)
	{
		assert(struct_def.members[i]->type == AST_VAR_DEF);
		gen_var_def_with_const(code_gen, struct_def.members[i]->node.var_def, false);
		str_cat(code_block, ";");
	}

	str_cat(code_block, "}");
	if (!code_gen->no_semicolon)
	{
		str_cat(code_block, ";");
	}
	code_gen->current_block = CODE_BLOCK_NONE;
}

static void gen_ast_union_def(CodeGen code_gen[static 1], UnionDef union_def)
{
	CodeBlock* code_block = nullptr;

	code_gen->current_block = CODE_BLOCK_PRIV_TYPES;
	code_block = &code_gen->priv_types;
	for (usize i = 0; i < union_def.modifier_count; i++)
	{
		if (union_def.modifiers[i].token_type == token_type_pub)
		{
			code_gen->current_block = CODE_BLOCK_PUB_TYPES;
			code_block = &code_gen->pub_types;
			break;
		}
	}
	if (code_gen->current_block == CODE_BLOCK_PUB_TYPES)
	{
		str_cat(&code_gen->pub_typedefs, "typedef union ");
		str_cat(&code_gen->pub_typedefs, union_def.name);
		str_cat(&code_gen->pub_typedefs, " ");
		str_cat(&code_gen->pub_typedefs, union_def.name);
		str_cat(&code_gen->pub_typedefs, ";");
	}
	else
	{
		str_cat(&code_gen->priv_typedefs, "typedef union ");
		str_cat(&code_gen->priv_typedefs, union_def.name);
		str_cat(&code_gen->priv_typedefs, " ");
		str_cat(&code_gen->priv_typedefs, union_def.name);
		str_cat(&code_gen->priv_typedefs, ";");
	}
	str_cat(code_block, "union ");
	str_cat(code_block, union_def.name);
	str_cat(code_block, "{");

	for (usize i = 0; i < union_def.member_count; i++)
	{
		assert(union_def.members[i]->type == AST_VAR_DEF);
		gen_var_def_with_const(code_gen, union_def.members[i]->node.var_def, false);
		str_cat(code_block, ";");
	}

	str_cat(code_block, "}");
	if (!code_gen->no_semicolon)
	{
		str_cat(code_block, ";");
	}
	code_gen->current_block = CODE_BLOCK_NONE;
}

static void gen_ast_enum_def(CodeGen code_gen[static 1], EnumDef enum_def)
{
	CodeBlock* code_block = nullptr;

	code_gen->current_block = CODE_BLOCK_PRIV_TYPES;
	code_block = &code_gen->priv_types;
	for (usize i = 0; i < enum_def.modifier_count; i++)
	{
		if (enum_def.modifiers[i].token_type == token_type_pub)
		{
			code_gen->current_block = CODE_BLOCK_PUB_TYPES;
			code_block = &code_gen->pub_types;
			break;
		}
	}
	if (code_gen->current_block == CODE_BLOCK_PUB_TYPES)
	{
		str_cat(&code_gen->pub_typedefs, "typedef enum ");
		str_cat(&code_gen->pub_typedefs, enum_def.name);
		str_cat(&code_gen->pub_typedefs, " ");
		str_cat(&code_gen->pub_typedefs, enum_def.name);
		str_cat(&code_gen->pub_typedefs, ";");
	}
	else
	{
		str_cat(&code_gen->priv_typedefs, "typedef enum ");
		str_cat(&code_gen->priv_typedefs, enum_def.name);
		str_cat(&code_gen->priv_typedefs, " ");
		str_cat(&code_gen->priv_typedefs, enum_def.name);
		str_cat(&code_gen->priv_typedefs, ";");
	}
	str_cat(code_block, "enum ");
	str_cat(code_block, enum_def.name);
	if (enum_def.type != nullptr)
	{
		str_cat(code_block, ":");
		str_cat(code_block, enum_def.type);
	}
	str_cat(code_block, "{");
	for (usize i = 0; i < enum_def.member_count; i++)
	{
		str_cat(code_block, enum_def.members[i].name);
		if (enum_def.members[i].has_value)
		{
			str_cat(code_block, "=");
			char buffer[MAX_BUFFER_SIZE] = {0};
			(void)snprintf(buffer, MAX_BUFFER_SIZE - 1, "%" PRId64, enum_def.members[i].value);
			str_cat(code_block, buffer);
		}
		str_cat(code_block, ",");
	}
	str_cat(code_block, "}");
	if (!code_gen->no_semicolon)
	{
		str_cat(code_block, ";");
	}
	code_gen->current_block = CODE_BLOCK_NONE;
}

static void gen_ast_member_access(CodeGen code_gen[static 1], MemberAccess member_access)
{
	CodeBlock* code_block = get_code_block(code_gen);
	gen_code(code_gen, member_access.left);
	if (member_access.direct)
	{
		str_cat(code_block, ".");
	}
	else
	{
		str_cat(code_block, "->");
	}
	gen_code(code_gen, member_access.right);
}

static void gen_ast_func_call(CodeGen code_gen[static 1], FuncCall func_call)
{
	CodeBlock* code_block = get_code_block(code_gen);
	gen_code(code_gen, func_call.callee);
	str_cat(code_block, "(");
	if (func_call.arg_count > 0)
	{
		gen_code(code_gen, func_call.args[0]);
	}
	for (usize i = 1; i < func_call.arg_count; i++)
	{
		str_cat(code_block, ",");
		gen_code(code_gen, func_call.args[i]);
	}
	str_cat(code_block, ")");
}

static void gen_ast_identifier(CodeGen code_gen[static 1], Identifier identifier)
{
	CodeBlock* code_block = get_code_block(code_gen);
	str_cat(code_block, identifier.identifier.str_val);
}

static void gen_ast_binary_expr(CodeGen code_gen[static 1], BinaryExpr binary_expr)
{
	CodeBlock* code_block = get_code_block(code_gen);

	if (strchr(TOKENS_STR_IDENT[binary_expr.op.token_type], '=') &&
		(binary_expr.right->type == AST_BLOCK || binary_expr.right->type == AST_IF_EXPR ||
		 (binary_expr.right->type == AST_CAST_EXPR && casted_block(binary_expr.right->node.cast_expr)))) // NOLINT
	{
		char tmp_var[MAX_BUFFER_SIZE] = {};
		int tmp_var_len = snprintf(tmp_var, MAX_BUFFER_SIZE - 1, TMP_VAR_PREFIX "ret_%" PRIu32, code_gen->tmp_num);
		code_gen->tmp_num++;

		str_cat(code_block, "typeof_unqual(");
		gen_code(code_gen, binary_expr.left);
		str_cat(code_block, ") ");
		str_cat(code_block, tmp_var);
		str_cat(code_block, ";");

		strcat(tmp_var, "=");
		if (binary_expr.right->type == AST_BLOCK)
		{
			gen_ast_block(code_gen, binary_expr.right->node.block, tmp_var);
		}
		else if (binary_expr.right->type == AST_IF_EXPR)
		{
			gen_ast_if_expr(code_gen, binary_expr.right->node.if_expr, tmp_var);
		}
		else if (binary_expr.right->type == AST_CAST_EXPR)
		{
			gen_ast_cast_block(code_gen, binary_expr.right->node.cast_expr, tmp_var);
		}
		tmp_var[tmp_var_len] = '\0';

		gen_code(code_gen, binary_expr.left);
		str_cat(code_block, TOKENS_STR_IDENT[binary_expr.op.token_type]);
		str_cat(code_block, tmp_var);
	}
	else
	{

		str_cat(code_block, "(");
		gen_code(code_gen, binary_expr.left);
		str_cat(code_block, TOKENS_STR_IDENT[binary_expr.op.token_type]);
		gen_code(code_gen, binary_expr.right);
		str_cat(code_block, ")");
	}
}

static void gen_ast_unary_expr(CodeGen code_gen[static 1], UnaryExpr unary_expr)
{
	CodeBlock* code_block = get_code_block(code_gen);

	str_cat(code_block, "(");
	str_cat(code_block, TOKENS_STR_IDENT[unary_expr.op.token_type]);
	gen_code(code_gen, unary_expr.rhs);
	str_cat(code_block, ")");
}

static void gen_ast_cast_expr(CodeGen code_gen[static 1], CastExpr cast_expr)
{
	CodeBlock* code_block = get_code_block(code_gen);

	str_cat(code_block, "((");
	gen_var_def(code_gen, cast_expr.target_type);
	str_cat(code_block, ")");
	gen_code(code_gen, cast_expr.expr);
	str_cat(code_block, ")");
}

static void gen_ast_cast_block_internal(CodeGen code_gen[static 1], CastExpr cast_expr, char** add_before_trailing_expr,
										usize* cap, usize* len, usize* paren_count)
{
	usize cast_len = 2;
	cast_len += strlen(cast_expr.target_type.type.name);
	for (usize i = 0; i < cast_expr.target_type.type.pointer_count; i++)
	{
		if (cast_expr.target_type.type.pointer_types[i] == pointer_type_const)
		{
			cast_len += strlen("* const");
		}
		else
		{
			cast_len += 1;
		}
	}
	cast_len += 1;

	while (*len + cast_len + 1 > *cap)
	{
		*cap *= 2;
		*add_before_trailing_expr = (char*)realloc(*add_before_trailing_expr, *cap);
		assert(*add_before_trailing_expr != nullptr);
	}

	strcat(*add_before_trailing_expr, "((");
	strcat(*add_before_trailing_expr, cast_expr.target_type.type.name);
	for (usize i = 0; i < cast_expr.target_type.type.pointer_count; i++)
	{
		if (cast_expr.target_type.type.pointer_types[i] == pointer_type_const)
		{
			strcat(*add_before_trailing_expr, "* const");
		}
		else
		{
			strcat(*add_before_trailing_expr, "*");
		}
	}
	strcat(*add_before_trailing_expr, ")");
	*len = strlen(*add_before_trailing_expr);
	(*paren_count)++;

	if (cast_expr.expr->type == AST_CAST_EXPR)
	{
		CastExpr inner_cast = cast_expr.expr->node.cast_expr;
		gen_ast_cast_block_internal(code_gen, inner_cast, add_before_trailing_expr, cap, len, paren_count);
	}
}

static void gen_ast_cast_block(CodeGen code_gen[static 1], CastExpr cast_expr, const char* add_before_trailing_expr)
{
	usize cap = MAX_BUFFER_SIZE;
	char* before_trailing_expr = (char*)malloc(cap);

	if (add_before_trailing_expr != nullptr)
	{
		strcpy(before_trailing_expr, add_before_trailing_expr);
	}
	else
	{
		before_trailing_expr[0] = '\0';
	}

	usize len = strlen(before_trailing_expr);
	usize paren_count = 0;

	gen_ast_cast_block_internal(code_gen, cast_expr, &before_trailing_expr, &cap, &len, &paren_count);

	code_gen->close_paren_count = paren_count;

	AST* deepest_expr = cast_expr.expr;
	while (deepest_expr->type == AST_CAST_EXPR)
	{
		deepest_expr = deepest_expr->node.cast_expr.expr;
	}

	if (deepest_expr->type == AST_BLOCK)
	{
		gen_ast_block(code_gen, deepest_expr->node.block, before_trailing_expr);
	}
	else if (deepest_expr->type == AST_IF_EXPR)
	{
		gen_ast_if_expr(code_gen, deepest_expr->node.if_expr, before_trailing_expr);
	}
	else
	{
		assert(false && "not an expected type");
	}

	code_gen->close_paren_count = 0;

	free(before_trailing_expr);
}

static void gen_ast_index_expr(CodeGen code_gen[static 1], IndexExpr index_expr)
{
	CodeBlock* code_block = get_code_block(code_gen);
	str_cat(code_block, "(");
	gen_code(code_gen, index_expr.left);
	str_cat(code_block, "[");
	gen_code(code_gen, index_expr.index);
	str_cat(code_block, "])");
}

static void gen_ast_array_init(CodeGen code_gen[static 1], ArrayInit array_init)
{
	CodeBlock* code_block = get_code_block(code_gen);
	if (array_init.is_sized)
	{
		str_cat(code_block, "{");
		assert(array_init.size_expr->type == AST_LITERAL);
		assert(array_init.size_expr->node.literal.literal.token_type == token_type_number);
		for (i64 i = 0; i < array_init.size_expr->node.literal.literal.num_val; i++)
		{
			for (usize j = 0; j < array_init.element_count; j++)
			{
				gen_code(code_gen, array_init.elements[j]);
				str_cat(code_block, ",");
			}
		}
		str_cat(code_block, "}");
	}
	else
	{
		str_cat(code_block, "{");
		for (usize i = 0; i < array_init.element_count; i++)
		{
			gen_code(code_gen, array_init.elements[i]);
			str_cat(code_block, ",");
		}
		str_cat(code_block, "}");
	}
}

static void gen_ast_if_expr(CodeGen code_gen[static 1], IfExpr if_expr, const char* add_before_trailing_expr)
{
	CodeBlock* code_block = get_code_block(code_gen);

	str_cat(code_block, "if(");
	gen_code(code_gen, if_expr.condition);
	str_cat(code_block, ")");
	if (add_before_trailing_expr == nullptr)
	{
		gen_code(code_gen, if_expr.then_block);
		str_cat(code_block, "else ");
		gen_code(code_gen, if_expr.else_block);
	}
	else
	{
		assert(if_expr.then_block != nullptr);
		assert(if_expr.else_block != nullptr);
		assert(if_expr.then_block->type == AST_BLOCK);
		assert(if_expr.else_block->type == AST_BLOCK || if_expr.else_block->type == AST_IF_EXPR);
		gen_ast_block(code_gen, if_expr.then_block->node.block, add_before_trailing_expr);
		if (if_expr.else_block->type == AST_BLOCK)
		{
			gen_ast_block(code_gen, if_expr.else_block->node.block, add_before_trailing_expr);
		}
		else if (if_expr.else_block->type == AST_IF_EXPR)
		{
			gen_ast_if_expr(code_gen, if_expr.else_block->node.if_expr, add_before_trailing_expr);
		}
	}
}

static void gen_ast_while_expr(CodeGen code_gen[static 1], WhileExpr while_expr)
{
	CodeBlock* code_block = get_code_block(code_gen);

	str_cat(code_block, "while(");
	gen_code(code_gen, while_expr.condition);
	str_cat(code_block, ")");
	gen_code(code_gen, while_expr.then_block);
}

static void gen_ast_for_expr(CodeGen code_gen[static 1], ForExpr for_expr)
{
	CodeBlock* code_block = get_code_block(code_gen);

	if (for_expr.style == FOR_STYLE_C)
	{
		str_cat(code_block, "for(");
		code_gen->no_semicolon = true;
		gen_code(code_gen, for_expr.c_style.init);
		str_cat(code_block, ";");
		gen_code(code_gen, for_expr.c_style.condition);
		str_cat(code_block, ";");
		gen_code(code_gen, for_expr.c_style.increment);
		code_gen->no_semicolon = false;
		str_cat(code_block, ")");

		gen_code(code_gen, for_expr.body);
	}
	else // Rust-style
	{
		char tmp_start[MAX_BUFFER_SIZE] = {};
		char tmp_end[MAX_BUFFER_SIZE] = {};
		char tmp_dir[MAX_BUFFER_SIZE] = {};
		char tmp_iter[MAX_BUFFER_SIZE] = {};

		int tmp_start_len =
			snprintf(tmp_start, MAX_BUFFER_SIZE - 1, TMP_VAR_PREFIX "start_%" PRIu32, code_gen->tmp_num);
		int tmp_end_len = snprintf(tmp_end, MAX_BUFFER_SIZE - 1, TMP_VAR_PREFIX "end_%" PRIu32, code_gen->tmp_num);
		(void)snprintf(tmp_dir, MAX_BUFFER_SIZE - 1, TMP_VAR_PREFIX "dir_%" PRIu32, code_gen->tmp_num);
		(void)snprintf(tmp_iter, MAX_BUFFER_SIZE - 1, TMP_VAR_PREFIX "iter_%" PRIu32, code_gen->tmp_num);
		code_gen->tmp_num++;

		assert(for_expr.rust_style.iterable->type == AST_RANGE_EXPR);
		RangeExpr range_expr = for_expr.rust_style.iterable->node.range_expr;

		if (range_expr.start->type == AST_BLOCK || range_expr.start->type == AST_IF_EXPR)
		{
			gen_type(code_block, for_expr.rust_style.var_def.type);
			str_cat(code_block, " ");
			str_cat(code_block, tmp_start);
			str_cat(code_block, ";");

			tmp_start[tmp_start_len] = '=';

			if (range_expr.start->type == AST_BLOCK)
			{
				gen_ast_block(code_gen, range_expr.start->node.block, tmp_start);
			}
			else if (range_expr.start->type == AST_IF_EXPR)
			{
				gen_ast_if_expr(code_gen, range_expr.start->node.if_expr, tmp_start);
			}

			tmp_start[tmp_start_len] = '\0';
		}
		else
		{
			gen_type(code_block, for_expr.rust_style.var_def.type);
			str_cat(code_block, " ");
			str_cat(code_block, tmp_start);
			str_cat(code_block, "=");
			gen_code(code_gen, range_expr.start);
			str_cat(code_block, ";");
		}
		if (range_expr.end->type == AST_BLOCK || range_expr.end->type == AST_IF_EXPR)
		{
			gen_type(code_block, for_expr.rust_style.var_def.type);
			str_cat(code_block, " ");
			str_cat(code_block, tmp_end);
			str_cat(code_block, ";");

			tmp_end[tmp_end_len] = '=';

			if (range_expr.end->type == AST_BLOCK)
			{
				gen_ast_block(code_gen, range_expr.end->node.block, tmp_end);
			}
			else if (range_expr.end->type == AST_IF_EXPR)
			{
				gen_ast_if_expr(code_gen, range_expr.end->node.if_expr, tmp_end);
			}

			tmp_end[tmp_end_len] = '\0';
		}
		else
		{
			gen_type(code_block, for_expr.rust_style.var_def.type);
			str_cat(code_block, " ");
			str_cat(code_block, tmp_end);
			str_cat(code_block, "=");
			gen_code(code_gen, range_expr.end);
			str_cat(code_block, ";");
		}

		str_cat(code_block, "bool ");
		str_cat(code_block, tmp_dir);
		str_cat(code_block, "=");
		str_cat(code_block, tmp_start);
		str_cat(code_block, "<");
		str_cat(code_block, tmp_end);
		str_cat(code_block, ";");

		str_cat(code_block, "for(");

		// for (x; _; _)
		gen_type(code_block, for_expr.rust_style.var_def.type);
		str_cat(code_block, " ");
		str_cat(code_block, tmp_iter);
		str_cat(code_block, "=");
		str_cat(code_block, tmp_start);

		str_cat(code_block, ";");

		// for (_; x; _)
		str_cat(code_block, "(");
		str_cat(code_block, tmp_dir);
		str_cat(code_block, "?");
		str_cat(code_block, tmp_iter);
		if (range_expr.inclusive)
		{
			str_cat(code_block, "<=");
		}
		else
		{
			str_cat(code_block, "<");
		}
		str_cat(code_block, tmp_end);
		str_cat(code_block, ":");
		str_cat(code_block, tmp_iter);
		if (range_expr.inclusive)
		{
			str_cat(code_block, ">=");
		}
		else
		{
			str_cat(code_block, ">");
		}
		str_cat(code_block, tmp_end);
		str_cat(code_block, ")");

		str_cat(code_block, ";");

		// for (_; _; x)
		str_cat(code_block, "(");
		str_cat(code_block, tmp_dir);
		str_cat(code_block, "?");
		str_cat(code_block, tmp_iter);
		str_cat(code_block, "++");
		str_cat(code_block, ":");
		str_cat(code_block, tmp_iter);
		str_cat(code_block, "--)");

		str_cat(code_block, ")");

		str_cat(code_block, "{");
		gen_var_def(code_gen, for_expr.rust_style.var_def);
		str_cat(code_block, "=");
		str_cat(code_block, tmp_iter);
		str_cat(code_block, ";");
		code_gen->skip_brace = true;
		gen_code(code_gen, for_expr.body);
		code_gen->skip_brace = false;
		str_cat(code_block, "}");
	}
}

static void gen_ast_struct_init(CodeGen code_gen[static 1], StructInit struct_init)
{
	CodeBlock* code_block = get_code_block(code_gen);
	str_cat(code_block, "{");
	for (usize i = 0; i < struct_init.field_count; i++)
	{
		str_cat(code_block, ".");
		const StructFieldInit field = struct_init.fields[i];
		str_cat(code_block, field.field_name);
		str_cat(code_block, "=");
		gen_code(code_gen, field.value);
		str_cat(code_block, ",");
	}
	str_cat(code_block, "}");
}

static void gen_ast_message(CodeGen code_gen[static 1], Message message)
{
	CodeBlock* code_block = get_code_block(code_gen);

	switch (message.msg)
	{
		case msg_invalid:
			assert(false && "invalid message");
		case msg_import:
			str_cat(&code_gen->includes, "#include \"");
			str_cat(&code_gen->includes, message.import.import);
			str_cat(&code_gen->includes, "\"\n");
			break;
		case msg_c_type:
			break;
		case msg_include:
			str_cat(code_block, "#include \"");
			str_cat(code_block, message.import.import);
			str_cat(code_block, "\"\n");
			break;
		case msg_use:
		case msg_include_str:
			assert(false && "code_gen: not yet implemented");
	}
}

static void gen_type(CodeBlock* code_block, VarType var_type)
{
	str_cat(code_block, var_type.name);
	for (usize i = 0; i < var_type.pointer_count; i++)
	{
		if (var_type.pointer_types[i] == pointer_type_const)
		{
			str_cat(code_block, "* const");
		}
		else
		{
			str_cat(code_block, "*");
		}
	}
}

static void gen_var_def(CodeGen code_gen[static 1], VarDef var_def)
{
	gen_var_def_with_const(code_gen, var_def, true);
}

static void gen_var_def_with_const(CodeGen code_gen[static 1], VarDef var_def, bool const_var)
{
	CodeBlock* code_block = get_code_block(code_gen);

	debug_assert(code_block != nullptr);
	{
		for (usize i = 0; i < var_def.modifier_count; i++)
		{
			switch (var_def.modifiers[i].token_type)
			{
				case token_type_mut:
					const_var = false;
					break;
				case token_type_pub:
					break;
				case token_type_static:
					if (code_gen->global_block)
					{
						assert(false, "Static is not supported on global variables\n Static variables are the default "
									  "if you don't add pub for globals");
					}
					str_cat(code_block, "static ");
					break;
				case token_type_volatile:
					str_cat(code_block, "volatile ");
					break;
				case token_type_const:
					str_cat(code_block, "constexpr ");
					break;
				default:
					assert(false, "not a supported token: %s", token_to_string(var_def.modifiers[i].token_type))
			}
		}

		if (const_var)
		{
			str_cat(code_block, "const ");
		}
	}
	gen_type(code_block, var_def.type);
	str_cat(code_block, " ");
	str_cat(code_block, var_def.name);

	for (usize i = 0; i < var_def.type.array_count; i++)
	{
		str_cat(code_block, "[");
		gen_code(code_gen, var_def.type.array_sizes[i]);
		str_cat(code_block, "]");
	}
}

static void gen_func_signature(CodeGen code_gen[static 1], FuncDef func_def)
{
	CodeBlock* code_block = get_code_block(code_gen);
	for (usize i = 0; i < func_def.modifier_count; i++)
	{
		switch (func_def.modifiers[i].token_type)
		{
			case token_type_pub:
				break;
			case token_type_static:
				assert(false, "Static is not supported on global variables\n Static variables are the default "
							  "if you don't add pub for globals");
				break;
			case token_type_volatile:
				str_cat(code_block, "volatile ");
				break;
			case token_type_const:
				assert(false, "const functions not yet supported");
				break;
			default:
				assert(false, "not a supported token%s", token_to_string(func_def.modifiers[i].token_type))
		}
	}
	if (func_def.return_type.type.name != nullptr)
	{
		gen_type(code_block, func_def.return_type.type);
	}
	else
	{
		str_cat(code_block, "void");
	}

	str_cat(code_block, " ");
	str_cat(code_block, func_def.name);
	str_cat(code_block, "(");
	if (func_def.param_count > 0)
	{
		gen_var_def(code_gen, func_def.params[0]->node.var_def);
	}
	for (usize i = 1; i < func_def.param_count; i++)
	{
		str_cat(code_block, ",");
		gen_var_def(code_gen, func_def.params[i]->node.var_def);
	}
	str_cat(code_block, ")");
}

static CodeBlock* get_code_block(CodeGen code_gen[static 1])
{
	switch (code_gen->current_block)
	{
		case CODE_BLOCK_INCLUDES:
			return &code_gen->includes;
		case CODE_BLOCK_PRIV_TYPES:
			return &code_gen->priv_types;
		case CODE_BLOCK_PRIV_FUNCTIONS:
			return &code_gen->priv_functions;
		case CODE_BLOCK_PRIV_VARS:
			return &code_gen->priv_vars;
		case CODE_BLOCK_PUB_TYPES:
			return &code_gen->pub_types;
		case CODE_BLOCK_PUB_FUNCTIONS:
			return &code_gen->pub_functions;
		case CODE_BLOCK_PUB_VARS:
			return &code_gen->pub_vars;
		case CODE_BLOCK_CODE:
		case CODE_BLOCK_NONE:
			return &code_gen->code;
	}
	assert(false);
}

static bool casted_block(CastExpr cast_expr)
{
	while (cast_expr.expr->type == AST_CAST_EXPR)
	{
		cast_expr = cast_expr.expr->node.cast_expr;
	}
	return (cast_expr.expr->type == AST_BLOCK || cast_expr.expr->type == AST_IF_EXPR);
}

static void str_cat(CodeBlock* code_block, const char* str)
{
	if (str == nullptr)
	{
		return;
	}
	usize str_len = strlen(str);
	if (code_block->len + str_len + 1 >= code_block->cap)
	{
		while (code_block->len + str_len + 1 >= code_block->cap)
		{
			code_block->cap = MAX(code_block->cap, 1);
			code_block->cap *= 2;
		}
		code_block->code = (char*)realloc((void*)code_block->code, code_block->cap); // NOLINT
		assert(code_block != nullptr);
	}
	memcpy((void*)(code_block->code + code_block->len), (void*)str, str_len);
	code_block->len += str_len;
	code_block->code[code_block->len] = '\0';
}

static void str_cat_escaped(CodeBlock* code_block, const char* str)
{
	for (const char* ptr = str; *ptr; ptr++)
	{
		switch (*ptr)
		{
			case '\a':
				str_cat(code_block, "\\a");
				break;
			case '\b':
				str_cat(code_block, "\\b");
				break;
			case '\f':
				str_cat(code_block, "\\f");
				break;
			case '\n':
				str_cat(code_block, "\\n");
				break;
			case '\r':
				str_cat(code_block, "\\r");
				break;
			case '\t':
				str_cat(code_block, "\\t");
				break;
			case '\v':
				str_cat(code_block, "\\v");
				break;
			case '\\':
				str_cat(code_block, "\\\\");
				break;
			case '\'':
				str_cat(code_block, "\\\'");
				break;
			case '\"':
				str_cat(code_block, "\\\"");
				break;
			case '0':
				str_cat(code_block, "\\0");
				break;
			default:
				if (*ptr >= 0x20 && *ptr < 0x7E)
				{
					char buf[2] = {*ptr, 0};
					str_cat(code_block, buf);
				}
				else
				{
					char buf[8];
					(void)snprintf(buf, sizeof(buf), "\\x%02x", (unsigned char)*ptr);
					str_cat(code_block, buf);
				}
		}
	}
}

NewFiles code_gen_to_files(const CodeGen* code_gen, const char* file_name)
{
	NewFiles files = {.c_file = nullptr, .h_file = nullptr};
	const char s_lang_header[] = "/* Generated with S-lang */\n";
	const char pragma[] = "\n#pragma once\n";
	const char ifdef_cpp[] = "\n#ifdef __cplusplus\nextern \"C\"{\n#endif\n";
	const char ifndef_cpp[] = "\n#ifdef __cplusplus\n}\n#endif\n";

	if (code_gen == nullptr || file_name == nullptr)
	{
		return files;
	}

	usize file_name_len = strlen(file_name);

	usize c_file_len = 0;
	c_file_len += sizeof(s_lang_header) - 1;
	c_file_len += sizeof("\n#include \"") - 1 + file_name_len + sizeof(".h\"\n") - 1;
	c_file_len += code_gen->includes.len;
	c_file_len += code_gen->priv_typedefs.len;
	c_file_len += code_gen->priv_types.len;
	c_file_len += code_gen->priv_vars.len;
	c_file_len += code_gen->priv_functions.len;
	c_file_len += code_gen->code.len;

	usize h_file_len = 0;
	h_file_len += sizeof(s_lang_header) - 1;
	h_file_len += sizeof(pragma) - 1;
	h_file_len += sizeof(ifdef_cpp) - 1;
	h_file_len += code_gen->pub_typedefs.len;
	h_file_len += code_gen->pub_types.len;
	h_file_len += code_gen->pub_vars.len;
	h_file_len += code_gen->pub_functions.len;
	h_file_len += sizeof(ifndef_cpp) - 1;

	files.c_file = (char*)malloc(c_file_len + 1);
	if (!files.c_file)
	{
		return files;
	}

	files.h_file = (char*)malloc(h_file_len + 1);
	if (!files.h_file)
	{
		free(files.c_file);
		files.c_file = nullptr;
		return files;
	}

	usize c_offset = 0;

	memcpy(files.c_file + c_offset, s_lang_header, sizeof(s_lang_header) - 1);
	c_offset += sizeof(s_lang_header) - 1;

	memcpy(files.c_file + c_offset, "\n#include \"", sizeof("\n#include \"") - 1);
	c_offset += sizeof("\n#include \"") - 1;

	memcpy(files.c_file + c_offset, file_name, file_name_len);
	c_offset += file_name_len;

	memcpy(files.c_file + c_offset, ".h\"\n", sizeof(".h\"\n") - 1);
	c_offset += sizeof(".h\"\n") - 1;

	if (code_gen->includes.code)
	{
		memcpy(files.c_file + c_offset, code_gen->includes.code, code_gen->includes.len);
		c_offset += code_gen->includes.len;
	}

	if (code_gen->priv_typedefs.code)
	{
		memcpy(files.c_file + c_offset, code_gen->priv_typedefs.code, code_gen->priv_typedefs.len);
		c_offset += code_gen->priv_typedefs.len;
	}

	if (code_gen->priv_types.code)
	{
		memcpy(files.c_file + c_offset, code_gen->priv_types.code, code_gen->priv_types.len);
		c_offset += code_gen->priv_types.len;
	}

	if (code_gen->priv_vars.code)
	{
		memcpy(files.c_file + c_offset, code_gen->priv_vars.code, code_gen->priv_vars.len);
		c_offset += code_gen->priv_vars.len;
	}

	if (code_gen->priv_functions.code)
	{
		memcpy(files.c_file + c_offset, code_gen->priv_functions.code, code_gen->priv_functions.len);
		c_offset += code_gen->priv_functions.len;
	}

	if (code_gen->code.code)
	{
		memcpy(files.c_file + c_offset, code_gen->code.code, code_gen->code.len);
		c_offset += code_gen->code.len;
	}

	files.c_file[c_offset] = '\0';

	usize h_offset = 0;

	memcpy(files.h_file + h_offset, s_lang_header, sizeof(s_lang_header) - 1);
	h_offset += sizeof(s_lang_header) - 1;

	memcpy(files.h_file + h_offset, pragma, sizeof(pragma) - 1);
	h_offset += sizeof(pragma) - 1;

	memcpy(files.h_file + h_offset, ifdef_cpp, sizeof(ifdef_cpp) - 1);
	h_offset += sizeof(ifdef_cpp) - 1;

	if (code_gen->pub_typedefs.code)
	{
		memcpy(files.h_file + h_offset, code_gen->pub_typedefs.code, code_gen->pub_typedefs.len);
		h_offset += code_gen->pub_typedefs.len;
	}

	if (code_gen->pub_types.code)
	{
		memcpy(files.h_file + h_offset, code_gen->pub_types.code, code_gen->pub_types.len);
		h_offset += code_gen->pub_types.len;
	}

	if (code_gen->pub_vars.code)
	{
		memcpy(files.h_file + h_offset, code_gen->pub_vars.code, code_gen->pub_vars.len);
		h_offset += code_gen->pub_vars.len;
	}

	if (code_gen->pub_functions.code)
	{
		memcpy(files.h_file + h_offset, code_gen->pub_functions.code, code_gen->pub_functions.len);
		h_offset += code_gen->pub_functions.len;
	}

	memcpy(files.h_file + h_offset, ifndef_cpp, sizeof(ifndef_cpp) - 1);
	h_offset += sizeof(ifndef_cpp) - 1;

	files.h_file[h_offset] = '\0';

	return files;
}

void code_gen_free_code_gen(CodeGen code_gen)
{
	free_code_block(code_gen.includes);
	free_code_block(code_gen.priv_typedefs);
	free_code_block(code_gen.priv_types);
	free_code_block(code_gen.priv_functions);
	free_code_block(code_gen.priv_vars);
	free_code_block(code_gen.pub_functions);
	free_code_block(code_gen.pub_typedefs);
	free_code_block(code_gen.pub_types);
	free_code_block(code_gen.pub_vars);
	free_code_block(code_gen.code);
}

void code_gen_free_new_files(NewFiles new_files)
{
	free((void*)new_files.h_file);
	free((void*)new_files.c_file);
}

static void free_code_block(CodeBlock code_block)
{
	if (code_block.code != nullptr)
	{
		free((void*)code_block.code);
	}
}

static void emit_line_directive(CodeGen* code_gen, Pos pos)
{
	if (!GEN_LINE)
	{
		return;
	}

	CodeBlock* code_block = get_code_block(code_gen);
	if (!code_block)
	{
		return;
	}

	if (code_gen->current_block != CODE_BLOCK_CODE && code_gen->current_block != CODE_BLOCK_PRIV_FUNCTIONS &&
		code_gen->current_block != CODE_BLOCK_PUB_FUNCTIONS)
	{
		return;
	}

	if (code_gen->current_line != pos.line)
	{

		char line_buf[256];
		if (code_gen->source_filename)
		{
			(void)snprintf(line_buf, sizeof(line_buf), "\n#line %zu \"%s\"\n", pos.line, code_gen->source_filename);
		}
		else
		{
			(void)snprintf(line_buf, sizeof(line_buf), "\n#line %zu\n", pos.line);
		}
		str_cat(code_block, line_buf);

		code_gen->current_line = pos.line;
	}
}
