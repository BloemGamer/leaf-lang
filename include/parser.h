#pragma once

#include "tokens.h"
#include "utils.h"

typedef struct AbstractSyntaxTree AST;

typedef struct [[gnu::aligned(32)]]
{
	char* name;
	usize amount_pointer;
	usize amount_array;
} VarType;

typedef struct [[gnu::aligned(32)]]
{
	char* name;
	bool has_value;
	i64 value;
} EnumType;

typedef struct [[gnu::aligned(64)]]
{
	char* name;
	VarType type;
	Token* modifiers;
	usize modifier_count;
	AST* equals;
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

typedef struct [[gnu::aligned(32)]]
{
	char* name;
	AST** members;
	usize member_count;
} StructDef;

typedef struct [[gnu::aligned(32)]]
{
	char* name;
	AST** members;
	usize member_count;
} UnionDef;

typedef struct [[gnu::aligned(32)]]
{
	char* name;
	char* type;
	EnumType* members;
	usize member_count;
} EnumDef;

typedef struct [[gnu::aligned(32)]]
{
	AST* callee;
	AST** args;
	usize arg_count;
} FuncCall;

typedef struct [[gnu::aligned(16)]]
{
	AST* left;
	AST* right;
} MemberAccess;

typedef struct [[gnu::aligned(64)]]
{
	Token op;
	AST* left;
	AST* right;
} BinaryExpr;

typedef struct [[gnu::aligned(16)]]
{
	AST* left;
	AST* index;
} IndexExpr;

typedef struct [[gnu::aligned(32)]]
{
	Token literal;
} Literal;

/// wil also be used for messages
typedef struct [[gnu::aligned(32)]]
{
	Token identifier;
} Identifier;

/// At least for now also used for the global scope
typedef struct [[gnu::aligned(16)]]
{
	AST** statements;
	usize statement_count;
} Block;

typedef struct [[gnu::aligned(128)]] AbstractSyntaxTree
{
	enum
	{
		AST_VAR_DEF,
		AST_FUNC_DEF,
		AST_STRUCT_DEF,
		AST_UNION_DEF,
		AST_ENUM_DEF,
		AST_FUNC_CALL,
		AST_MEMBER_ACCESS,
		AST_BINARY_EXPR,
		AST_INDEX_EXPR,
		AST_LITERAL,
		AST_IDENTIFIER,
		AST_BLOCK,
	} type;

	union
	{
		VarDef var_def;				// done
		FuncDef func_def;			// done
		StructDef struct_def;		// done
		UnionDef union_def;			// done
		EnumDef enum_def;			// done
		FuncCall func_call;			// binary_expr
		MemberAccess member_access; // binary_expr
		BinaryExpr binary_expr;		// done
		IndexExpr index_expr;		// binary_expr
		Literal literal;			// binary_expr
		Identifier identifier;		// binary_expr
		Block block;				// done
	} node;
} AST;

/// parses the Token array given by the lexer
/// the TokenTree should be freed with free_token_tree
[[gnu::warn_unused_result]]
AST* parse(const Token* tokens);

/// will be implemented when the parser is done
void free_token_tree(AST* token_tree);

void parse_print(const AST* ast);
