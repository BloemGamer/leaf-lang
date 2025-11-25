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

static void gen_type(CodeBlock* code_block, VarType var_type);
static void gen_var_def(CodeGen* code_gen, VarDef var_def);
static void gen_func_signature(CodeGen* code_gen, FuncDef func_def);

static CodeBlock* get_code_block(CodeGen* code_gen);

static void str_cat(CodeBlock* code_block, const char* str);

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
	typeof(ast->node) node = ast->node;
	CodeBlock* code_block = nullptr;
	CodeBlockType saved_block = CODE_BLOCK_NONE;
	char buffer[64] = {0};

	switch (ast->type)
	{
		case AST_BLOCK:
			if (!node.block.global)
			{
				code_block = get_code_block(code_gen);
				if (code_block == nullptr)
				{
					code_block = &code_gen->code;
				}
				str_cat(code_block, "{");
			}
			bool prev_global_block = code_gen->global_block;
			code_gen->global_block = node.block.global;
			for (usize i = 0; i < node.block.statement_count; i++) // NOLINT
			{
				gen_code(code_gen, node.block.statements[i]);
			}
			code_gen->global_block = prev_global_block;

			if (!node.block.global)
			{
				str_cat(code_block, "}");
			}
			return;
		case AST_FUNC_DEF:
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

			// Generate function declaration in functions block
			gen_func_signature(code_gen, node.func_def);
			str_cat(code_block, ";"); // Add semicolon for declaration

			// Generate function implementation in code block
			code_gen->current_block = CODE_BLOCK_CODE;
			gen_func_signature(code_gen, node.func_def);

			// Generate the function body
			gen_code(code_gen, node.func_def.body);

			code_gen->current_block = CODE_BLOCK_NONE;
			return;
		case AST_VAR_DEF:
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
				code_block = get_code_block(code_gen); // FIX: Get current block instead of always using code
				if (code_block == nullptr)			   // FIX: Fallback for struct members
				{
					code_block = &code_gen->code;
					code_gen->current_block = CODE_BLOCK_CODE;
				}
			}
			gen_var_def(code_gen, node.var_def);

			if (node.var_def.equals == nullptr)
			{
				str_cat(code_block, ";"); // FIX: Add semicolon for declarations without initialization
				code_gen->current_block = CODE_BLOCK_NONE;
				return;
			}
			str_cat(code_block, "="); // FIX: Use code_block instead of always code_gen->code
			gen_code(code_gen, node.var_def.equals);

			str_cat(code_block, ";"); // FIX: Use code_block instead of always code_gen->code
			code_gen->current_block = CODE_BLOCK_NONE;
			return;
		case AST_LITERAL:
			Token token = node.literal.literal;
			code_block = get_code_block(code_gen);
			if (code_block == nullptr)
			{
				code_block = &code_gen->code;
			}
			switch (token.token_type)
			{
				case token_type_string:
					str_cat(code_block, token.str_val);
					break;
				case token_type_number:
					(void)snprintf(buffer, 63, "%" PRId64, token.num_val);
					str_cat(code_block, buffer);
					break;
				case token_type_char:
					(void)snprintf(buffer, 63, "%c", token.char_val);
					str_cat(code_block, buffer);
					break;
				case token_type_float:
					(void)snprintf(buffer, 63, "%lf", token.float_val);
					str_cat(code_block, buffer);
					break;
				default:
					assert(false && "code gen: not an literal");
			}
			return;
		case AST_BREAK_STMT:
			str_cat(&code_gen->code, "break;");
			return;
		case AST_CONTINUE_STMT:
			str_cat(&code_gen->code, "continue;");
			return;
		case AST_RETURN_STMT:
			str_cat(&code_gen->code, "return ");
			gen_code(code_gen, node.return_stmt.return_stmt);
			str_cat(&code_gen->code, ";");
			return;
		case AST_STRUCT_DEF:
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
				gen_var_def(code_gen, node.struct_def.members[i]->node.var_def);
				str_cat(code_block, ";");
			}

			str_cat(code_block, "}");
			str_cat(code_block, node.struct_def.name);
			str_cat(code_block, ";");
			code_gen->current_block = CODE_BLOCK_NONE;
			return;
		case AST_UNION_DEF:
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
				gen_var_def(code_gen, node.union_def.members[i]->node.var_def);
				str_cat(code_block, ";");
			}

			str_cat(code_block, "}");
			str_cat(code_block, node.union_def.name);
			str_cat(code_block, ";");
			code_gen->current_block = CODE_BLOCK_NONE;
			return;
		case AST_ENUM_DEF:
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
			str_cat(code_block, ";");
			code_gen->current_block = CODE_BLOCK_NONE;
			return;
	}
	// assert(false && "code gen: not yet implemented");
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
	CodeBlock* code_block = get_code_block(code_gen);

	debug_assert(code_block != nullptr);
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
	gen_type(code_block, func_def.return_type.type);
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
			return &code_gen->code;
		case CODE_BLOCK_NONE:
			return nullptr;
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
