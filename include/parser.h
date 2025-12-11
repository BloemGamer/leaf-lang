#pragma once

#include "basic_types.h"
#include "tokens.h"

typedef struct AbstractSyntaxTree ASTToken;

typedef enum
{
	pointer_type_none = 0,
	pointer_type_const,
	pointer_type_mut,
} PointerType;

typedef struct
{
	char* name;
	PointerType* pointer_types;
	usize pointer_count;
	ASTToken** array_sizes;
	usize array_count;
} VarType;

typedef struct
{
	char* name;
	Token* modifiers;
	usize modifier_count;
	bool has_value;
	i64 value;
} EnumType;

typedef struct
{
	char* name;
	VarType type;
	Token* modifiers;
	usize modifier_count;
	ASTToken* equals;
} VarDef;

typedef struct
{
	char* name;
	Token* modifiers;
	usize modifier_count;
	ASTToken** template_types;
	usize template_count;
	ASTToken** params;
	usize param_count;
	VarDef return_type;
	ASTToken* body;
} FuncDef;

typedef struct
{
	char* name;
	Token* modifiers;
	usize modifier_count;
	ASTToken** members;
	usize member_count;
} StructDef;

typedef struct
{
	char* name;
	Token* modifiers;
	usize modifier_count;
	ASTToken** members;
	usize member_count;
} UnionDef;

typedef struct
{
	char* name;
	char* type;
	Token* modifiers;
	usize modifier_count;
	EnumType* members;
	usize member_count;
} EnumDef;

typedef struct
{
	ASTToken* callee;
	ASTToken** args;
	usize arg_count;
} FuncCall;

typedef struct
{
	ASTToken* left;
	ASTToken* right;
	bool direct; // true means with a . false with a ->
} MemberAccess;

typedef struct
{
	Token op;
	ASTToken* left;
	ASTToken* right;
} BinaryExpr;

typedef struct
{
	ASTToken* left;
	ASTToken* index;
} IndexExpr;

typedef struct
{
	Token literal;
} Literal;

typedef struct
{
	Token identifier;
} Identifier;

/// At least for now also used for the global scope
typedef struct
{
	ASTToken** statements;
	usize statement_count;
	ASTToken* trailing_expr;
	bool global;
} Block;

typedef struct // NOLINT
{
	ASTToken* condition;
	ASTToken* then_block;
	ASTToken* else_block;
} IfExpr;

typedef struct // NOLINT
{
	ASTToken* condition;
	ASTToken* then_block;
} WhileExpr;

typedef struct // NOLINT
{
	enum
	{
		FOR_STYLE_RUST,
		FOR_STYLE_C
	} style;

	// Rust-style: for x in iterable
	struct
	{
		VarDef var_def;
		ASTToken* iterable;
	} rust_style;

	// C-style: for (init; cond; incr)
	struct
	{
		ASTToken* init;
		ASTToken* condition;
		ASTToken* increment;
	} c_style;

	ASTToken* body;
} ForExpr;

typedef struct
{
	ASTToken* start;
	ASTToken* end;
	bool inclusive;
} RangeExpr;

typedef struct
{
	Token op;
	ASTToken* rhs;
} UnaryExpr;

typedef struct // NOLINT
{
	ASTToken* return_stmt;
} ReturnStmt;

typedef struct
{
	ASTToken** elements;
	usize element_count;
	ASTToken* size_expr; // For [type; size] syntax
	bool is_sized;
} ArrayInit;

typedef struct
{
	char* field_name;
	ASTToken* value;
} StructFieldInit;

typedef struct
{
	char* struct_name;
	VarType type;
	StructFieldInit* fields;
	usize field_count;
} StructInit;

typedef struct
{
	VarDef target_type;
	ASTToken* expr;
} CastExpr;

typedef struct // NOLINT
{
	enum
	{
		msg_invalid = 0,
		msg_import,
		msg_use,
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

typedef struct AbstractSyntaxTree
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
		AST_RANGE_EXPR,
		AST_UNARY,

		AST_ARRAY_INIT,
		AST_STRUCT_INIT,
		AST_CAST_EXPR,

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
		ForExpr for_expr;		// done
		ReturnStmt return_stmt; // done
		RangeExpr range_expr;	// done
		UnaryExpr unary_expr;	// done

		ArrayInit array_init;
		StructInit struct_init;
		CastExpr cast_expr;

		Message message;
	} node;
	Pos pos;
} ASTToken;

typedef struct
{
	ASTToken* ast;
	usize warnings;
	usize errors;
} AST;

/// parses the Token array given by the lexer
/// the TokenTree should be freed with free_token_tree
[[gnu::warn_unused_result]]
AST parse(const Token* tokens);

void free_token_tree_token(ASTToken* ast);
void free_token_tree(AST ast);

void parse_print(AST ast);
