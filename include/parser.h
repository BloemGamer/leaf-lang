#pragma once

#include "basic_types.h"
#include "tokens.h"

typedef struct AbstractSyntaxTree AST;

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
	AST** array_sizes;
	usize array_count;
} VarType;

typedef struct
{
	char* name;
	bool has_value;
	i64 value;
} EnumType;

typedef struct
{
	char* name;
	VarType type;
	Token* modifiers;
	usize modifier_count;
	AST* equals;
} VarDef;

typedef struct
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

typedef struct
{
	char* name;
	Token* modifiers;
	usize modifier_count;
	AST** members;
	usize member_count;
} StructDef;

typedef struct
{
	char* name;
	Token* modifiers;
	usize modifier_count;
	AST** members;
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
	AST* callee;
	AST** args;
	usize arg_count;
} FuncCall;

typedef struct
{
	AST* left;
	AST* right;
	bool direct; // true means with a . false with a ->
} MemberAccess;

typedef struct
{
	Token op;
	AST* left;
	AST* right;
} BinaryExpr;

typedef struct
{
	AST* left;
	AST* index;
} IndexExpr;

typedef struct
{
	Token literal;
} Literal;

/// wil also be used for messages
typedef struct
{
	Token identifier;
} Identifier;

/// At least for now also used for the global scope
typedef struct
{
	AST** statements;
	usize statement_count;
	AST* trailing_expr;
	bool global;
} Block;

typedef struct // NOLINT
{
	AST* condition;
	AST* then_block;
	AST* else_block;
} IfExpr;

typedef struct // NOLINT
{
	AST* condition;
	AST* then_block;
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
		AST* iterable;
	} rust_style;

	// C-style: for (init; cond; incr)
	struct
	{
		AST* init;
		AST* condition;
		AST* increment;
	} c_style;

	AST* body;
} ForExpr;

typedef struct
{
	AST* start;
	AST* end;
} RangeExpr;

typedef struct
{
	Token const* op;
	AST* rhs;
} UnaryExpr;

typedef struct // NOLINT
{
	AST* return_stmt;
} ReturnStmt;

typedef struct
{
	AST** elements;
	usize element_count;
	AST* size_expr; // For [type; size] syntax
	bool is_sized;
} ArrayInit;

typedef struct
{
	char* field_name;
	AST* value;
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
	AST* expr;
} CastExpr;

typedef struct // NOLINT
{
	enum
	{
		msg_invalid = 0,
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
} AST;

/// parses the Token array given by the lexer
/// the TokenTree should be freed with free_token_tree
[[gnu::warn_unused_result]]
AST* parse(const Token* tokens);

/// will be implemented when the parser is done
void free_token_tree(AST* ast);

void parse_print(const AST* ast);
