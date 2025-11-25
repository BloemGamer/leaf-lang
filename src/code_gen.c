#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "basic_types.h"
#include "code_gen.h"
#include "parser.h"
#include "tokens.h"
#include "utils.h"

typedef struct
{
	char* code;
	usize len;
	usize cap;
} CodeGen;

static void gen_code(AST* ast, CodeGen* code_gen);

static void gen_type(CodeGen* code_gen, VarType var_type);
static void gen_var_def(CodeGen* code_gen, VarDef var_def);

static void str_cat(CodeGen* code_gen, const char* str);

char* generate_code(AST* ast)
{
	CodeGen code_gen = {.code = nullptr};

	str_cat(&code_gen, "/* Code generated with S-lang */\n\n");
	str_cat(&code_gen, "#include <assert.h>\n");
	str_cat(&code_gen, "#include <basic_types.h>\n");
	gen_code(ast, &code_gen);

	return code_gen.code;
}

static void gen_code(AST* ast, CodeGen* code_gen)
{
	if (ast == nullptr)
	{
		return;
	}
	typeof(ast->node) node = ast->node;
	switch (ast->type)
	{
		case AST_BLOCK:
			if (!node.block.global)
			{
				str_cat(code_gen, "{");
			}

			for (usize i = 0; i < node.block.statement_count; i++) // NOLINT
			{
				gen_code(node.block.statements[i], code_gen);
			}

			if (!node.block.global)
			{
				str_cat(code_gen, "}");
			}
			return;
		case AST_FUNC_DEF:
			gen_type(code_gen, node.func_def.return_type.type);
			str_cat(code_gen, " ");
			str_cat(code_gen, node.func_def.name);
			str_cat(code_gen, "(");
			if (node.func_def.param_count > 0)
			{
				gen_var_def(code_gen, node.func_def.params[0]->node.var_def);
			}
			for (usize i = 1; i < node.func_def.param_count; i++) // NOLINT
			{
				str_cat(code_gen, ",");
				gen_var_def(code_gen, node.func_def.params[i]->node.var_def);
			}
			str_cat(code_gen, ")");
			gen_code(node.func_def.body, code_gen);
			return;
		case AST_VAR_DEF:
			gen_var_def(code_gen, node.var_def);

			if (node.var_def.equals == nullptr)
			{
				return;
			}
			str_cat(code_gen, "=");
			gen_code(node.var_def.equals, code_gen);

			str_cat(code_gen, ";");
			return;
		case AST_LITERAL:
			Token token = node.literal.literal;
			char buffer[64] = {0};
			switch (token.token_type)
			{
				case token_type_string:
					str_cat(code_gen, token.str_val);
					break;
				case token_type_number:
					(void)snprintf(buffer, 63, "%" PRId64, token.num_val);
					str_cat(code_gen, buffer);
					break;
				case token_type_char:
					(void)snprintf(buffer, 63, "%c", token.char_val);
					str_cat(code_gen, buffer);
					break;
				case token_type_float:
					(void)snprintf(buffer, 63, "%lf", token.float_val);
					str_cat(code_gen, buffer);
					break;
				default:
					assert(false && "code gen: not an literal");
			}
			return;
		case AST_BREAK_STMT:
			str_cat(code_gen, "break;");
			return;
		case AST_CONTINUE_STMT:
			str_cat(code_gen, "continue;");
			return;
		case AST_RETURN_STMT:
			str_cat(code_gen, "return ");
			gen_code(node.return_stmt.return_stmt, code_gen);
			str_cat(code_gen, ";");
			return;
		case AST_STRUCT_DEF:
			str_cat(code_gen, "typedef struct ");
			str_cat(code_gen, node.struct_def.name);
			str_cat(code_gen, "{");

			for (usize i = 0; i < node.struct_def.member_count; i++) // NOLINT
			{
				assert(node.struct_def.members[i]->type == AST_VAR_DEF);
				gen_code(node.struct_def.members[i], code_gen);
				str_cat(code_gen, ";");
			}

			str_cat(code_gen, "}");
			str_cat(code_gen, node.struct_def.name);
			str_cat(code_gen, ";");
		case AST_UNION_DEF:
			str_cat(code_gen, "typedef union ");
			str_cat(code_gen, node.union_def.name);
			str_cat(code_gen, "{");

			for (usize i = 0; i < node.union_def.member_count; i++) // NOLINT
			{
				assert(node.union_def.members[i]->type == AST_VAR_DEF);
				gen_code(node.union_def.members[i], code_gen);
				str_cat(code_gen, ";");
			}

			str_cat(code_gen, "}");
			str_cat(code_gen, node.union_def.name);
			str_cat(code_gen, ";");
		case AST_ENUM_DEF:
			str_cat(code_gen, "typedef enum ");
			if (node.enum_def.type != nullptr)
			{
				str_cat(code_gen, ":");
				str_cat(code_gen, node.enum_def.type);
			}
			str_cat(code_gen, "{");
			for (usize i = 0; i < node.enum_def.member_count; i++) // NOLINT
			{
				str_cat(code_gen, node.enum_def.members[i].name);
				if (node.enum_def.members[i].has_value)
				{
					str_cat(code_gen, "=");
					char buffer[64] = {0};
					(void)snprintf(buffer, 63, "%" PRId64, node.enum_def.members[i].value);
					str_cat(code_gen, buffer);
				}
				str_cat(code_gen, ",");
			}
			str_cat(code_gen, "}");
			str_cat(code_gen, node.enum_def.name);
			str_cat(code_gen, ";");
	}
	// assert(false && "code gen: not yet implemented");
}

static void gen_type(CodeGen* code_gen, VarType var_type)
{
	str_cat(code_gen, var_type.name);
	for (usize i = 0; i < var_type.pointer_count; i++) // NOLINT
	{
		str_cat(code_gen, "*");
	}
	if (var_type.name == nullptr)
	{
		return;
	}
}

static void gen_var_def(CodeGen* code_gen, VarDef var_def)
{
	gen_type(code_gen, var_def.type);

	str_cat(code_gen, " ");
	str_cat(code_gen, var_def.name);

	for (usize i = 0; i < var_def.type.array_count; i++) // NOLINT
	{
		str_cat(code_gen, "[");
		gen_code(var_def.type.array_sizes[i], code_gen);
		str_cat(code_gen, "]");
	}
}

static void str_cat(CodeGen* code_gen, const char* str)
{
	if (str == nullptr)
	{
		return;
	}
	usize str_len = strlen(str);
	if (code_gen->len + str_len + 1 >= code_gen->cap)
	{
		while (code_gen->len + str_len + 1 >= code_gen->cap) // NOLINT
		{
			code_gen->cap = MAX(code_gen->cap, 1);
			code_gen->cap *= 2;
		}
		code_gen->code = (char*)realloc((void*)code_gen->code, code_gen->cap); // NOLINT
	}
	memcpy((void*)code_gen->code + code_gen->len, (void*)str, str_len);
	code_gen->len += str_len;
	code_gen->code[code_gen->len] = '\0';
}
