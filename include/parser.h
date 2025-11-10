#pragma once

#include "tokens.h"
#include "utils.h"

typedef struct AbstractSyntaxTree AST;

typedef struct [[gnu::aligned(64)]]
{
	char* name;
	char* type;
	Token* modifiers;
	usize modifier_count;
	AST* ast;
} VarDef;

typedef struct [[gnu::aligned(128)]]
{
	char* name;
	char* type;
	Token* modifiers;
	usize modifier_count;
	AST** template_types;
	usize template_count;
	AST** params;
	usize param_count;
	Token return_type;
	AST* body;
} FuncDef;

/// also used for enums and unions
typedef struct [[gnu::aligned(32)]]
{
	char* name;
	char* type;
	AST** members;
	usize param_count;
} StructDef;

typedef struct [[gnu::aligned(64)]]
{
	Token op;
	AST* left;
	AST* right;
} BinaryExpr;

typedef struct [[gnu::aligned(32)]]
{
	Token literal;
} Literal;

/// wil also be used for messages
typedef struct [[gnu::aligned(32)]]
{
	Token identefier;
} Identefier;

typedef struct [[gnu::aligned(16)]]
{
	AST** statements;
	usize satement_count;
} Block;

typedef struct [[gnu::aligned(128)]] AbstractSyntaxTree
{
	enum
	{
		AST_VAR_DEF,
		AST_FUNC_DEF,
		AST_STRUCT_DEF,
		AST_BINARY_EXPR,
		AST_LITERAL,
		AST_IDENTEFIER,
		AST_BLOCK
	} type;

	AST* tree;

	union
	{
		VarDef var_def;
		FuncDef func_def;
		StructDef stuct_def;
		BinaryExpr binary_expr;
		Literal literal;
		Identefier identefier;
		Block block;
	} node;
} AST;

/// parses the Token array given by the lexer
/// the TokenTree should be freed with free_token_tree
[[gnu::warn_unused_result]]
AST* parse(const Token* tokens);

/// will be implemented when the parser is done
void free_token_tree(AST* token_tree);
