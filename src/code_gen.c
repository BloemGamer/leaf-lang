#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "basic_types.h"
#include "code_gen.h"
#include "parser.h"
#include "tokens.h"
#include "utils.h"

static void gen_code(CodeGen* code_gen, AST* ast);

static void gen_ast_block(CodeGen* code_gen, AST* ast, const char* tmp_var);
static void gen_ast_func_def(CodeGen* code_gen, AST* ast);
static void gen_ast_var_def(CodeGen* code_gen, AST* ast);
static void gen_ast_literal(CodeGen* code_gen, AST* ast);
static void gen_ast_break_stmt(CodeGen* code_gen, AST* ast);
static void gen_ast_continue_stmt(CodeGen* code_gen, AST* ast);
static void gen_ast_return_stmt(CodeGen* code_gen, AST* ast);
static void gen_ast_struct_def(CodeGen* code_gen, AST* ast);
static void gen_ast_union_def(CodeGen* code_gen, AST* ast);
static void gen_ast_enum_def(CodeGen* code_gen, AST* ast);
static void gen_ast_member_access(CodeGen* code_gen, AST* ast);
static void gen_ast_func_call(CodeGen* code_gen, AST* ast);
static void gen_ast_identifier(CodeGen* code_gen, AST* ast);
static void gen_ast_binary_expr(CodeGen* code_gen, AST* ast);
static void gen_ast_unary_expr(CodeGen* code_gen, AST* ast);
static void gen_ast_cast_expr(CodeGen* code_gen, AST* ast);
static void gen_ast_index_expr(CodeGen* code_gen, AST* ast);
static void gen_ast_array_init(CodeGen* code_gen, AST* ast);
static void gen_ast_if_expr(CodeGen* code_gen, AST* ast, const char* tmp_var);
static void gen_ast_while_expr(CodeGen* code_gen, AST* ast);
static void gen_ast_for_expr(CodeGen* code_gen, AST* ast);
static void gen_ast_struct_init(CodeGen* code_gen, AST* ast);
static void gen_ast_message(CodeGen* code_gen, AST* ast);

static void gen_type(CodeBlock* code_block, VarType var_type);
static void gen_var_def(CodeGen* code_gen, VarDef var_def);
static void gen_var_def_with_const(CodeGen* code_gen, VarDef var_def, bool const_var);
static void gen_func_signature(CodeGen* code_gen, FuncDef func_def);

static CodeBlock* get_code_block(CodeGen* code_gen);
static void str_cat(CodeBlock* code_block, const char* str);

static void free_code_block(CodeBlock code_block);

CodeGen generate_code(AST* ast)
{
	CodeGen code_gen = {.current_block = CODE_BLOCK_NONE};

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
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = nullptr;
	CodeBlockType saved_block = CODE_BLOCK_NONE;
	char buffer[64] = {0};

	switch (ast->type)
	{
		case AST_BLOCK:
			gen_ast_block(code_gen, ast, nullptr);
			return;
		case AST_FUNC_DEF:
			gen_ast_func_def(code_gen, ast);
			return;
		case AST_VAR_DEF:
			gen_ast_var_def(code_gen, ast);
			return;
		case AST_LITERAL:
			gen_ast_literal(code_gen, ast);
			return;
		case AST_BREAK_STMT:
			gen_ast_break_stmt(code_gen, ast);
			return;
		case AST_CONTINUE_STMT:
			gen_ast_continue_stmt(code_gen, ast);
			return;
		case AST_RETURN_STMT:
			gen_ast_return_stmt(code_gen, ast);
			return;
		case AST_STRUCT_DEF:
			gen_ast_struct_def(code_gen, ast);
			return;
		case AST_UNION_DEF:
			gen_ast_union_def(code_gen, ast);
			return;
		case AST_ENUM_DEF:
			gen_ast_enum_def(code_gen, ast);
			return;
		case AST_MEMBER_ACCESS:
			gen_ast_member_access(code_gen, ast);
			return;
		case AST_FUNC_CALL:
			gen_ast_func_call(code_gen, ast);
			return;
		case AST_IDENTIFIER:
			gen_ast_identifier(code_gen, ast);
			return;
		case AST_BINARY_EXPR:
			gen_ast_binary_expr(code_gen, ast);
			return;
		case AST_UNARY:
			gen_ast_unary_expr(code_gen, ast);
			return;
		case AST_CAST_EXPR:
			gen_ast_cast_expr(code_gen, ast);
			return;
		case AST_INDEX_EXPR:
			gen_ast_index_expr(code_gen, ast);
			return;
		case AST_ARRAY_INIT:
			gen_ast_array_init(code_gen, ast);
			return;
		case AST_IF_EXPR:
			gen_ast_if_expr(code_gen, ast, nullptr);
			return;
		case AST_WHILE_EXPR:
			gen_ast_while_expr(code_gen, ast);
			return;
		case AST_FOR_EXPR:
			gen_ast_for_expr(code_gen, ast);
			return;
		case AST_RANGE_EXPR:
			assert(false && "code gen: range expression should not be used outside of for statement");
			return;
		case AST_STRUCT_INIT:
			gen_ast_struct_init(code_gen, ast);
			return;
		case AST_MESSAGE:
			gen_ast_message(code_gen, ast);
			return;
	}
	assert(false && "code gen: not yet implemented");
}

static void gen_ast_block(CodeGen* code_gen, AST* ast, const char* tmp_var)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);

	if (!node.block.global && !code_gen->skip_brace)
	{
		bool prev_global_block = code_gen->global_block;
		code_block = get_code_block(code_gen);

		str_cat(code_block, "{");
	}
	bool prev_global_block = code_gen->global_block;
	code_gen->global_block = node.block.global;
	bool prev_skip_brace = code_gen->skip_brace;
	code_gen->skip_brace = false;
	for (usize i = 0; i < node.block.statement_count; i++) // NOLINT
	{
		code_block = get_code_block(code_gen);
		gen_code(code_gen, node.block.statements[i]);
		switch (node.block.statements[i]->type)
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

	if (tmp_var != nullptr)
	{
		assert(node.block.trailing_expr != nullptr);
		str_cat(code_block, tmp_var);
		str_cat(code_block, "=");

		switch (node.block.trailing_expr->type)
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
				gen_code(code_gen, node.block.trailing_expr);
				str_cat(code_block, ";");
				break;
			case AST_IF_EXPR:
			case AST_BLOCK:
				gen_ast_block(code_gen, node.block.trailing_expr, tmp_var);
				break;
		}
	}
	else if (node.block.trailing_expr != nullptr)
	{
		code_block = get_code_block(code_gen);
		gen_code(code_gen, node.block.trailing_expr);
		switch (node.block.trailing_expr->type)
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

	if (!node.block.global && !prev_skip_brace)
	{
		str_cat(code_block, "}");
	}
	code_gen->skip_brace = prev_skip_brace;
}

static void gen_ast_func_def(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = nullptr;

	code_gen->current_block = CODE_BLOCK_PRIV_FUNCTIONS;
	code_block = &code_gen->priv_functions;
	for (usize i = 0; i < node.func_def.modifier_count; i++) // NOLINT
	{
		if (node.func_def.modifiers[i].token_type == token_type_pub)
		{
			code_gen->current_block = CODE_BLOCK_PUB_FUNCTIONS;
			code_block = &code_gen->pub_functions;
			break;
		}
	}

	if (strcmp(node.func_def.name, "main") != 0)
	{
		if (code_gen->current_block == CODE_BLOCK_PRIV_FUNCTIONS)
		{
			str_cat(code_block, "static ");
		}
		else
		{
			str_cat(code_block, "extern ");
		}
		gen_func_signature(code_gen, node.func_def);
		str_cat(code_block, ";");
		if (code_gen->current_block == CODE_BLOCK_PRIV_FUNCTIONS)
		{
			str_cat(&code_gen->code, "static ");
		}
	}

	code_gen->current_block = CODE_BLOCK_CODE;
	gen_func_signature(code_gen, node.func_def);

	gen_code(code_gen, node.func_def.body);

	code_gen->current_block = CODE_BLOCK_NONE;
}

static void gen_ast_var_def(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = nullptr;

	if (code_gen->global_block)
	{
		code_gen->current_block = CODE_BLOCK_PRIV_VARS;
		code_block = &code_gen->priv_vars;
		for (usize i = 0; i < node.var_def.modifier_count; i++) // NOLINT
		{
			if (node.var_def.modifiers[i].token_type == token_type_pub)
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
	char tmp_var[64] = {};
	if (node.var_def.equals != nullptr &&
		(node.var_def.equals->type == AST_BLOCK || node.var_def.equals->type == AST_IF_EXPR))
	{
		(void)snprintf(tmp_var, 63, "sl_tmp_ret_%" PRIu32, code_gen->tmp_num);
		code_gen->tmp_num++;

		gen_type(code_block, node.var_def.type);
		str_cat(code_block, " ");
		str_cat(code_block, tmp_var);
		str_cat(code_block, ";");
		if (node.var_def.equals->type == AST_BLOCK)
		{
			gen_ast_block(code_gen, node.var_def.equals, tmp_var);
		}
		else if (node.var_def.equals->type == AST_IF_EXPR)
		{
			gen_ast_if_expr(code_gen, node.var_def.equals, tmp_var);
		}
	}
	if (code_gen->current_block == CODE_BLOCK_PRIV_VARS)
	{
		str_cat(code_block, "static ");
	}
	else if (code_gen->current_block == CODE_BLOCK_PUB_VARS)
	{
		str_cat(code_block, "extern ");
	}
	gen_var_def(code_gen, node.var_def);

	if (node.var_def.equals == nullptr)
	{
		str_cat(code_block, ";");
		code_gen->current_block = CODE_BLOCK_NONE;
		return;
	}
	str_cat(code_block, "=");

	if (node.var_def.equals != nullptr &&
		(node.var_def.equals->type == AST_BLOCK || node.var_def.equals->type == AST_IF_EXPR))
	{
		str_cat(code_block, tmp_var);
	}
	else
	{
		gen_code(code_gen, node.var_def.equals);
	}

	if (!code_gen->no_semicolon)
	{
		str_cat(code_block, ";");
	}
	code_gen->current_block = CODE_BLOCK_NONE;
}

static void gen_ast_literal(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	Token token = node.literal.literal;
	CodeBlock* code_block = get_code_block(code_gen);
	if (code_block == nullptr)
	{
		code_block = &code_gen->code;
	}
	char buffer[64] = {0};
	switch (token.token_type)
	{
		case token_type_string:
			str_cat(code_block, "\"");
			str_cat(code_block, token.str_val);
			str_cat(code_block, "\"");
			break;
		case token_type_number:
			(void)snprintf(buffer, 63, "%" PRId64, token.num_val);
			str_cat(code_block, buffer);
			break;
		case token_type_char:
			(void)snprintf(buffer, 63, "'%c'", token.char_val);
			str_cat(code_block, buffer);
			break;
		case token_type_float:
			(void)snprintf(buffer, 63, "%lf", token.float_val);
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

static void gen_ast_break_stmt(CodeGen* code_gen, AST* ast)
{
	(void)ast; // will use the ast prob later
	str_cat(&code_gen->code, "break");
}

static void gen_ast_continue_stmt(CodeGen* code_gen, AST* ast)
{
	(void)ast; // will use the ast prob later
	str_cat(&code_gen->code, "continue");
}

static void gen_ast_return_stmt(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	str_cat(&code_gen->code, "return ");
	gen_code(code_gen, node.return_stmt.return_stmt);
}

static void gen_ast_struct_def(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = nullptr;

	code_gen->current_block = CODE_BLOCK_PRIV_TYPES;
	code_block = &code_gen->priv_types;
	for (usize i = 0; i < node.struct_def.modifier_count; i++) // NOLINT
	{
		if (node.struct_def.modifiers[i].token_type == token_type_pub)
		{
			code_gen->current_block = CODE_BLOCK_PUB_TYPES;
			code_block = &code_gen->pub_types;
			break;
		}
	}
	str_cat(code_block, "typedef struct ");
	str_cat(code_block, node.struct_def.name);
	str_cat(code_block, "{");

	for (usize i = 0; i < node.struct_def.member_count; i++) // NOLINT
	{
		assert(node.struct_def.members[i]->type == AST_VAR_DEF);
		gen_var_def_with_const(code_gen, node.struct_def.members[i]->node.var_def, false);
		str_cat(code_block, ";");
	}

	str_cat(code_block, "}");
	str_cat(code_block, node.struct_def.name);
	if (!code_gen->no_semicolon)
	{
		str_cat(code_block, ";");
	}
	code_gen->current_block = CODE_BLOCK_NONE;
}

static void gen_ast_union_def(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = nullptr;

	code_gen->current_block = CODE_BLOCK_PRIV_TYPES;
	code_block = &code_gen->priv_types;
	for (usize i = 0; i < node.union_def.modifier_count; i++) // NOLINT
	{
		if (node.union_def.modifiers[i].token_type == token_type_pub)
		{
			code_gen->current_block = CODE_BLOCK_PUB_TYPES;
			code_block = &code_gen->pub_types;
			break;
		}
	}
	str_cat(code_block, "typedef union ");
	str_cat(code_block, node.union_def.name);
	str_cat(code_block, "{");

	for (usize i = 0; i < node.union_def.member_count; i++) // NOLINT
	{
		assert(node.union_def.members[i]->type == AST_VAR_DEF);
		gen_var_def_with_const(code_gen, node.union_def.members[i]->node.var_def, false);
		str_cat(code_block, ";");
	}

	str_cat(code_block, "}");
	str_cat(code_block, node.union_def.name);
	if (!code_gen->no_semicolon)
	{
		str_cat(code_block, ";");
	}
	code_gen->current_block = CODE_BLOCK_NONE;
}

static void gen_ast_enum_def(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = nullptr;

	code_gen->current_block = CODE_BLOCK_PRIV_TYPES;
	code_block = &code_gen->priv_types;
	for (usize i = 0; i < node.enum_def.modifier_count; i++) // NOLINT
	{
		if (node.enum_def.modifiers[i].token_type == token_type_pub)
		{
			code_gen->current_block = CODE_BLOCK_PUB_TYPES;
			code_block = &code_gen->pub_types;
			break;
		}
	}
	str_cat(code_block, "typedef enum ");
	if (node.enum_def.type != nullptr)
	{
		str_cat(code_block, ":");
		str_cat(code_block, node.enum_def.type);
	}
	str_cat(code_block, "{");
	for (usize i = 0; i < node.enum_def.member_count; i++) // NOLINT
	{
		str_cat(code_block, node.enum_def.members[i].name);
		if (node.enum_def.members[i].has_value)
		{
			str_cat(code_block, "=");
			char buffer[64] = {0};
			(void)snprintf(buffer, 63, "%" PRId64, node.enum_def.members[i].value);
			str_cat(code_block, buffer);
		}
		str_cat(code_block, ",");
	}
	str_cat(code_block, "}");
	str_cat(code_block, node.enum_def.name);
	if (!code_gen->no_semicolon)
	{
		str_cat(code_block, ";");
	}
	code_gen->current_block = CODE_BLOCK_NONE;
}

static void gen_ast_member_access(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);
	gen_code(code_gen, node.member_access.left);
	if (node.member_access.direct)
	{
		str_cat(code_block, ".");
	}
	else
	{
		str_cat(code_block, "->");
	}
	gen_code(code_gen, node.member_access.right);
}

static void gen_ast_func_call(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);
	gen_code(code_gen, node.func_call.callee);
	str_cat(code_block, "(");
	if (node.func_call.arg_count > 0)
	{
		gen_code(code_gen, node.func_call.args[0]);
	}
	for (usize i = 1; i < node.func_call.arg_count; i++) // NOLINT
	{
		str_cat(code_block, ",");
		gen_code(code_gen, node.func_call.args[i]);
	}
	str_cat(code_block, ")");
}

static void gen_ast_identifier(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);
	str_cat(code_block, node.identifier.identifier.str_val);
}

static void gen_ast_binary_expr(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);

	str_cat(code_block, "(");
	gen_code(code_gen, node.binary_expr.left);
	str_cat(code_block, TOKENS_STR_IDENT[node.binary_expr.op.token_type]);
	gen_code(code_gen, node.binary_expr.right);
	str_cat(code_block, ")");
}

static void gen_ast_unary_expr(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);

	str_cat(code_block, "(");
	str_cat(code_block, TOKENS_STR_IDENT[node.unary_expr.op.token_type]);
	gen_code(code_gen, node.unary_expr.rhs);
	str_cat(code_block, ")");
}

static void gen_ast_cast_expr(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);

	str_cat(code_block, "((");
	gen_var_def(code_gen, node.cast_expr.target_type);
	str_cat(code_block, ")");
	gen_code(code_gen, node.cast_expr.expr);
	str_cat(code_block, ")");
}

static void gen_ast_index_expr(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);
	str_cat(code_block, "(");
	gen_code(code_gen, node.index_expr.left);
	str_cat(code_block, "[");
	gen_code(code_gen, node.index_expr.index);
	str_cat(code_block, "])");
}

static void gen_ast_array_init(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);
	if (node.array_init.is_sized)
	{
		str_cat(code_block, "{");
		assert(node.array_init.size_expr->type == AST_LITERAL);
		assert(node.array_init.size_expr->node.literal.literal.token_type == token_type_number);
		for (usize i = 0; i < node.array_init.size_expr->node.literal.literal.num_val; i++) // NOLINT
		{
			for (usize j = 0; j < node.array_init.element_count; j++) // NOLINT
			{
				gen_code(code_gen, node.array_init.elements[j]);
				str_cat(code_block, ",");
			}
		}
		str_cat(code_block, "}");
	}
	else
	{
		str_cat(code_block, "{");
		for (usize i = 0; i < node.array_init.element_count; i++) // NOLINT
		{
			gen_code(code_gen, node.array_init.elements[i]);
			str_cat(code_block, ",");
		}
		str_cat(code_block, "}");
	}
}

static void gen_ast_if_expr(CodeGen* code_gen, AST* ast, const char* tmp_var)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);

	str_cat(code_block, "if(");
	gen_code(code_gen, node.if_expr.condition);
	str_cat(code_block, ")");
	if (tmp_var == nullptr)
	{
		gen_code(code_gen, node.if_expr.then_block);
		str_cat(code_block, "else ");
		gen_code(code_gen, node.if_expr.else_block);
	}
	else
	{
		assert(node.if_expr.then_block->type == AST_BLOCK);
		assert(node.if_expr.else_block->type == AST_BLOCK || node.if_expr.else_block->type == AST_IF_EXPR);
		gen_ast_block(code_gen, node.if_expr.then_block, tmp_var);
		if (node.if_expr.else_block->type == AST_BLOCK)
		{
			gen_ast_block(code_gen, node.if_expr.else_block, tmp_var);
		}
		else if (node.if_expr.else_block->type == AST_IF_EXPR)
		{
			gen_ast_if_expr(code_gen, node.if_expr.else_block, tmp_var);
		}
	}
}

static void gen_ast_while_expr(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);

	str_cat(code_block, "while(");
	gen_code(code_gen, node.while_expr.condition);
	str_cat(code_block, ")");
	gen_code(code_gen, node.while_expr.then_block);
}

static void gen_ast_for_expr(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);

	if (node.for_expr.style == FOR_STYLE_C)
	{
		str_cat(code_block, "for(");
		code_gen->no_semicolon = true;
		gen_code(code_gen, node.for_expr.c_style.init);
		str_cat(code_block, ";");
		gen_code(code_gen, node.for_expr.c_style.condition);
		str_cat(code_block, ";");
		gen_code(code_gen, node.for_expr.c_style.increment);
		code_gen->no_semicolon = false;
		str_cat(code_block, ")");

		gen_code(code_gen, node.for_expr.body);
	}
	else // Rust-style
	{
		char tmp_end[64] = {};
		char tmp_start[64] = {};
		char tmp_dir[64] = {};
		char tmp_iter[64] = {};
		(void)snprintf(tmp_end, 63, "sl_tmp_end_%" PRIu32, code_gen->tmp_num);
		(void)snprintf(tmp_start, 63, "sl_tmp_start_%" PRIu32, code_gen->tmp_num);
		(void)snprintf(tmp_dir, 63, "sl_tmp_dir_%" PRIu32, code_gen->tmp_num);
		(void)snprintf(tmp_iter, 63, "sl_tmp_iter_%" PRIu32, code_gen->tmp_num);
		code_gen->tmp_num++;

		gen_type(code_block, node.for_expr.rust_style.var_def.type);
		str_cat(code_block, " ");
		str_cat(code_block, tmp_end);
		str_cat(code_block, "=");
		gen_code(code_gen, node.for_expr.rust_style.iterable->node.range_expr.end);
		str_cat(code_block, ";");

		gen_type(code_block, node.for_expr.rust_style.var_def.type);
		str_cat(code_block, " ");
		str_cat(code_block, tmp_start);
		str_cat(code_block, "=");
		gen_code(code_gen, node.for_expr.rust_style.iterable->node.range_expr.start);
		str_cat(code_block, ";");

		str_cat(code_block, "bool ");
		str_cat(code_block, tmp_dir);
		str_cat(code_block, "=");
		str_cat(code_block, tmp_start);
		str_cat(code_block, "<");
		str_cat(code_block, tmp_end);
		str_cat(code_block, ";");

		str_cat(code_block, "for(");

		// for (x; _; _)
		gen_type(code_block, node.for_expr.rust_style.var_def.type);
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
		if (node.for_expr.rust_style.iterable->node.range_expr.inclusive)
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
		if (node.for_expr.rust_style.iterable->node.range_expr.inclusive)
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
		gen_var_def(code_gen, node.for_expr.rust_style.var_def);
		str_cat(code_block, "=");
		str_cat(code_block, tmp_iter);
		str_cat(code_block, ";");
		code_gen->skip_brace = true;
		gen_code(code_gen, node.for_expr.body);
		code_gen->skip_brace = false;
		str_cat(code_block, "}");
	}
}

static void gen_ast_struct_init(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);
	str_cat(code_block, "{");
	for (usize i = 0; i < node.struct_init.field_count; i++) // NOLINT
	{
		str_cat(code_block, ".");
		const StructFieldInit field = node.struct_init.fields[i];
		str_cat(code_block, field.field_name);
		str_cat(code_block, "=");
		gen_code(code_gen, field.value);
		str_cat(code_block, ",");
	}
	str_cat(code_block, "}");
}

static void gen_ast_message(CodeGen* code_gen, AST* ast)
{
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = get_code_block(code_gen);

	switch (node.message.msg)
	{
		case msg_invalid:
			assert(false && "invalid message");
		case msg_embed:
			str_cat(&code_gen->includes, "#include \"");
			str_cat(&code_gen->includes, node.message.import.import);
			str_cat(&code_gen->includes, "\"\n");
			break;
		case msg_c_type:
			break;
		case msg_include:
			str_cat(code_block, "#include \"");
			str_cat(code_block, node.message.import.import);
			str_cat(code_block, "\"\n");
			break;
		case msg_import:
		case msg_include_str:
			assert(false && "code_gen: not yet implemented");
	}
}

static void gen_type(CodeBlock* code_block, VarType var_type)
{
	str_cat(code_block, var_type.name);
	for (usize i = 0; i < var_type.pointer_count; i++) // NOLINT
	{
		str_cat(code_block, "*");
	}
}

static void gen_var_def(CodeGen* code_gen, VarDef var_def)
{
	gen_var_def_with_const(code_gen, var_def, true);
}

static void gen_var_def_with_const(CodeGen* code_gen, VarDef var_def, bool const_var)
{
	CodeBlock* code_block = get_code_block(code_gen);

	debug_assert(code_block != nullptr);
	{
		for (usize i = 0; i < var_def.modifier_count; i++) // NOLINT
		{
			if (var_def.modifiers[i].token_type == token_type_mut)
			{
				const_var = false;
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

	for (usize i = 0; i < var_def.type.array_count; i++) // NOLINT
	{
		str_cat(code_block, "[");
		gen_code(code_gen, var_def.type.array_sizes[i]);
		str_cat(code_block, "]");
	}
}

static void gen_func_signature(CodeGen* code_gen, FuncDef func_def)
{
	CodeBlock* code_block = get_code_block(code_gen);
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
	for (usize i = 1; i < func_def.param_count; i++) // NOLINT
	{
		str_cat(code_block, ",");
		gen_var_def(code_gen, func_def.params[i]->node.var_def);
	}
	str_cat(code_block, ")");
}

static CodeBlock* get_code_block(CodeGen* code_gen)
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

static void str_cat(CodeBlock* code_block, const char* str)
{
	if (str == nullptr)
	{
		return;
	}
	usize str_len = strlen(str);
	if (code_block->len + str_len + 1 >= code_block->cap)
	{
		while (code_block->len + str_len + 1 >= code_block->cap) // NOLINT
		{
			code_block->cap = MAX(code_block->cap, 1);
			code_block->cap *= 2;
		}
		code_block->code = (char*)realloc((void*)code_block->code, code_block->cap); // NOLINT
	}
	memcpy((void*)code_block->code + code_block->len, (void*)str, str_len);
	code_block->len += str_len;
	code_block->code[code_block->len] = '\0';
}

NewFiles code_gen_to_files(const CodeGen* code_gen, char* file_name)
{
	NewFiles files = {.c_file = nullptr, .h_file = nullptr};
	const char s_lang_header[] = "/* Generated with S-lang */\n";

	if (code_gen == nullptr)
	{
		return files;
	}
	if (file_name == nullptr)
	{
		return files;
	}

	usize file_name_len = strlen(file_name);

	usize c_file_len = 0;
	c_file_len += strlen(s_lang_header);
	c_file_len += strlen("#include \"") + file_name_len + strlen(".h\"\n");
	c_file_len += code_gen->includes.len;
	c_file_len += code_gen->priv_types.len;
	c_file_len += code_gen->priv_vars.len;
	c_file_len += code_gen->priv_functions.len;
	c_file_len += code_gen->code.len;

	usize h_file_len = 0;
	h_file_len += strlen(s_lang_header);
	h_file_len += code_gen->pub_types.len;
	h_file_len += code_gen->pub_vars.len;
	h_file_len += code_gen->pub_functions.len;

	files.c_file = (char*)malloc(c_file_len + 1);
	if (files.c_file == nullptr)
	{
		return files;
	}

	files.h_file = (char*)malloc(h_file_len + 1);
	if (files.h_file == nullptr)
	{
		free(files.c_file);
		files.c_file = nullptr;
		return files;
	}

	usize c_offset = 0;

	memcpy(files.c_file + c_offset, s_lang_header, strlen(s_lang_header));
	c_offset += strlen(s_lang_header);

	memcpy(files.c_file + c_offset, "#include \"", strlen("#include \""));
	c_offset += strlen("#include \"");
	memcpy(files.c_file + c_offset, file_name, file_name_len);
	c_offset += file_name_len;
	memcpy(files.c_file + c_offset, ".h\"\n", strlen(".h\"\n"));
	c_offset += strlen(".h\"\n");

	if (code_gen->includes.code != nullptr)
	{
		memcpy(files.c_file + c_offset, code_gen->includes.code, code_gen->includes.len);
		c_offset += code_gen->includes.len;
	}

	if (code_gen->priv_types.code != nullptr)
	{
		memcpy(files.c_file + c_offset, code_gen->priv_types.code, code_gen->priv_types.len);
		c_offset += code_gen->priv_types.len;
	}

	if (code_gen->priv_vars.code != nullptr)
	{
		memcpy(files.c_file + c_offset, code_gen->priv_vars.code, code_gen->priv_vars.len);
		c_offset += code_gen->priv_vars.len;
	}

	if (code_gen->priv_functions.code != nullptr)
	{
		memcpy(files.c_file + c_offset, code_gen->priv_functions.code, code_gen->priv_functions.len);
		c_offset += code_gen->priv_functions.len;
	}

	if (code_gen->code.code != nullptr)
	{
		memcpy(files.c_file + c_offset, code_gen->code.code, code_gen->code.len);
		c_offset += code_gen->code.len;
	}

	files.c_file[c_offset] = '\0';

	usize h_offset = 0;

	memcpy(files.h_file + h_offset, s_lang_header, strlen(s_lang_header));
	h_offset += strlen(s_lang_header);

	if (code_gen->pub_types.code != nullptr)
	{
		memcpy(files.h_file + h_offset, code_gen->pub_types.code, code_gen->pub_types.len);
		h_offset += code_gen->pub_types.len;
	}

	if (code_gen->pub_vars.code != nullptr)
	{
		memcpy(files.h_file + h_offset, code_gen->pub_vars.code, code_gen->pub_vars.len);
		h_offset += code_gen->pub_vars.len;
	}

	if (code_gen->pub_functions.code != nullptr)
	{
		memcpy(files.h_file + h_offset, code_gen->pub_functions.code, code_gen->pub_functions.len);
		h_offset += code_gen->pub_functions.len;
	}

	files.h_file[h_offset] = '\0';

	return files;
}

void code_gen_free_code_gen(CodeGen code_gen)
{
	free_code_block(code_gen.includes);
	free_code_block(code_gen.priv_types);
	free_code_block(code_gen.priv_functions);
	free_code_block(code_gen.priv_vars);
	free_code_block(code_gen.pub_functions);
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
