#pragma once

#include "tokens.h"
#include "utils.h"

typedef struct AbstractSyntaxTree AST;

typedef enum
{
	pointer_type_none = 0,
	pointer_type_const,
	pointer_type_mut,
} PointerType;

typedef struct [[gnu::aligned(64)]]
{
	char* name;
	PointerType* pointer_types;
	usize pointer_count;
	AST** array_sizes;
	usize array_count;
} VarType;

typedef struct [[gnu::aligned(32)]]
{
	char* name;
	bool has_value;
	i64 value;
} EnumType;

typedef struct [[gnu::aligned(128)]]
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
	Token* modifiers;
	usize modifier_count;
	AST** template_types;
	usize template_count;
	AST** params;
	usize param_count;
	VarDef return_type;
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
typedef struct [[gnu::aligned(32)]]
{
	AST** statements;
	usize statement_count;
	AST* trailing_expr;
} Block;

typedef struct // NOLINT
{
	AST* condition;
	AST* then_block;
	AST* else_block; // Can be NULL, another IF_EXPR, or a BLOCK
} IfExpr;

typedef struct // NOLINT
{
	AST* condition;
	AST* then_block;
} WhileExpr;

typedef struct // NOLINT
{
	AST* return_stmt;
} ReturnStmt;

typedef struct // NOLINT
{
	enum
	{
		msg_invallid = 0,
		msg_embed,
		msg_import,
		msg_include,
		msg_include_str,
		msg_c_type,
	} msg;

	struct // NOLINT
	{
		enum
		{
			import_type_system,
			import_type_user,
		} type;
		char* import;
	} import;

	struct
	{
		char* type;
	} c_type;

} Message;

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

		AST_IF_EXPR,
		AST_WHILE_EXPR,
		AST_FOR_EXPR,
		AST_RETURN_STMT,
		AST_BREAK_STMT,
		AST_CONTINUE_STMT,

		AST_MESSAGE,
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

		IfExpr if_expr;			// done
		WhileExpr while_expr;	// done
		ReturnStmt return_stmt; // done

		Message message;
	} node;
} AST;

/// parses the Token array given by the lexer
/// the TokenTree should be freed with free_token_tree
[[gnu::warn_unused_result]]
AST* parse(const Token* tokens);

/// will be implemented when the parser is done
void free_token_tree(AST* token_tree);

void parse_print(const AST* ast);
