#pragma once

#include "basic_types.h"
#include "parser.h"

typedef struct
{
	char* code;
	usize len;
	usize cap;
} CodeBlock;

typedef enum
{
	CODE_BLOCK_NONE = 0,
	CODE_BLOCK_INCLUDES,
	CODE_BLOCK_PRIV_TYPES,
	CODE_BLOCK_PRIV_FUNCTIONS,
	CODE_BLOCK_PRIV_VARS,
	CODE_BLOCK_PUB_TYPES,
	CODE_BLOCK_PUB_FUNCTIONS,
	CODE_BLOCK_PUB_VARS,
	CODE_BLOCK_CODE
} CodeBlockType;

typedef struct
{
	CodeBlock includes;
	CodeBlock priv_typedefs;
	CodeBlock priv_types;
	CodeBlock priv_functions;
	CodeBlock priv_vars;
	CodeBlock pub_functions;
	CodeBlock pub_typedefs;
	CodeBlock pub_types;
	CodeBlock pub_vars;
	CodeBlock code;
	CodeBlockType current_block;
	usize close_paren_count;
	u32 tmp_num;
	bool global_block;
	bool skip_brace;
	bool no_semicolon;
	const char* source_filename;
	usize current_line;
	bool line_directive_needed;
} CodeGen;

typedef struct
{
	char* c_file;
	char* h_file;
} NewFiles;

extern CodeGen generate_code(AST* ast, const char* filename);

extern NewFiles code_gen_to_files(const CodeGen* code_gen, const char* file_name);
extern void code_gen_free_code_gen(CodeGen code_gen);
extern void code_gen_free_new_files(NewFiles new_files);
