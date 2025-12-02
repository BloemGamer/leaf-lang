#include "lexer.h"
#include "parser.h"
#include "tokens.h"
#include <stdio.h>
#include <string.h>

#define ASSERT(condition, message)                                                                    \
	do                                                                                                \
	{                                                                                                 \
		if (!(condition))                                                                             \
		{                                                                                             \
			(void)fprintf(stderr, "ASSERTION FAILED: %s\n  at %s:%d\n", message, __FILE__, __LINE__); \
			return 1;                                                                                 \
		}                                                                                             \
	} while (0)

#define ASSERT_EQ(actual, expected, message) ASSERT((actual) == (expected), message)

#define ASSERT_STR_EQ(actual, expected, message) ASSERT(strcmp(actual, expected) == 0, message)

#define ASSERT_NOT_NULL(ptr, message) ASSERT((ptr) != nullptr, message)

// Helper function to parse input
static AST* parse_input(const char* input)
{
	Token* tokens = lex(input);
	AST* ast = parse(tokens);
	lex_free(tokens);
	return ast;
}

// Test: Empty input
int test_empty_input(void)
{
	AST* ast = parse_input("");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");
	ASSERT_EQ(ast->type, AST_BLOCK, "Should be a block");
	ASSERT_EQ(ast->node.block.statement_count, 0, "Should have no statements");
	free_token_tree(ast);
	return 0;
}

// Test: Simple variable declaration
int test_simple_var_decl(void)
{
	AST* ast = parse_input("i32 x = 42;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");
	ASSERT_EQ(ast->type, AST_BLOCK, "Should be a block");
	ASSERT_EQ(ast->node.block.statement_count, 1, "Should have one statement");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->type, AST_VAR_DEF, "Should be a variable definition");
	ASSERT_STR_EQ(var_def->node.var_def.name, "x", "Variable name should be x");
	ASSERT_STR_EQ(var_def->node.var_def.type.name, "i32", "Type should be i32");
	ASSERT_NOT_NULL(var_def->node.var_def.equals, "Should have initializer");

	free_token_tree(ast);
	return 0;
}

// Test: Variable with pointer
int test_var_with_pointer(void)
{
	AST* ast = parse_input("i32* ptr = 0;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->type, AST_VAR_DEF, "Should be a variable definition");
	ASSERT_EQ(var_def->node.var_def.type.pointer_count, 1, "Should have one pointer level");
	ASSERT_EQ(var_def->node.var_def.type.pointer_types[0], pointer_type_mut, "Should be mutable pointer");

	free_token_tree(ast);
	return 0;
}

// Test: Variable with const reference
int test_var_with_const_ref(void)
{
	AST* ast = parse_input("i32& ref = x;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->type, AST_VAR_DEF, "Should be a variable definition");
	ASSERT_EQ(var_def->node.var_def.type.pointer_count, 1, "Should have one reference level");
	ASSERT_EQ(var_def->node.var_def.type.pointer_types[0], pointer_type_const, "Should be const reference");

	free_token_tree(ast);
	return 0;
}

// Test: Array declaration
int test_array_decl(void)
{
	AST* ast = parse_input("i32 arr[10] = 0;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->type, AST_VAR_DEF, "Should be a variable definition");
	ASSERT_EQ(var_def->node.var_def.type.array_count, 1, "Should have one array dimension");
	ASSERT_NOT_NULL(var_def->node.var_def.type.array_sizes[0], "Should have array size");

	free_token_tree(ast);
	return 0;
}

// Test: Simple function definition
int test_simple_func_def(void)
{
	AST* ast = parse_input("fn add(i32 a, i32 b) -> i32 { return a + b; }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* func_def = ast->node.block.statements[0];
	ASSERT_EQ(func_def->type, AST_FUNC_DEF, "Should be a function definition");
	ASSERT_STR_EQ(func_def->node.func_def.name, "add", "Function name should be add");
	ASSERT_EQ(func_def->node.func_def.param_count, 2, "Should have two parameters");
	ASSERT_STR_EQ(func_def->node.func_def.return_type.type.name, "i32", "Return type should be i32");
	ASSERT_NOT_NULL(func_def->node.func_def.body, "Should have a body");

	free_token_tree(ast);
	return 0;
}

// Test: Function with no parameters
int test_func_no_params(void)
{
	AST* ast = parse_input("fn main() -> i32 { return 0; }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* func_def = ast->node.block.statements[0];
	ASSERT_EQ(func_def->type, AST_FUNC_DEF, "Should be a function definition");
	ASSERT_EQ(func_def->node.func_def.param_count, 0, "Should have no parameters");

	free_token_tree(ast);
	return 0;
}

// Test: Function with void return (no arrow)
int test_func_void_return(void)
{
	AST* ast = parse_input("fn print() { }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* func_def = ast->node.block.statements[0];
	ASSERT_EQ(func_def->type, AST_FUNC_DEF, "Should be a function definition");
	ASSERT_NOT_NULL(func_def->node.func_def.body, "Should have a body");

	free_token_tree(ast);
	return 0;
}

// Test: Struct definition
int test_struct_def(void)
{
	AST* ast = parse_input("struct Point { i32 x, i32 y }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* struct_def = ast->node.block.statements[0];
	ASSERT_EQ(struct_def->type, AST_STRUCT_DEF, "Should be a struct definition");
	ASSERT_STR_EQ(struct_def->node.struct_def.name, "Point", "Struct name should be Point");
	ASSERT_EQ(struct_def->node.struct_def.member_count, 2, "Should have two members");

	free_token_tree(ast);
	return 0;
}

// Test: Union definition
int test_union_def(void)
{
	AST* ast = parse_input("union Value { i32 i, f32 f }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* union_def = ast->node.block.statements[0];
	ASSERT_EQ(union_def->type, AST_UNION_DEF, "Should be a union definition");
	ASSERT_STR_EQ(union_def->node.union_def.name, "Value", "Union name should be Value");
	ASSERT_EQ(union_def->node.union_def.member_count, 2, "Should have two members");

	free_token_tree(ast);
	return 0;
}

// Test: Enum definition
int test_enum_def(void)
{
	AST* ast = parse_input("enum Color { Red, Green, Blue }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* enum_def = ast->node.block.statements[0];
	ASSERT_EQ(enum_def->type, AST_ENUM_DEF, "Should be an enum definition");
	ASSERT_STR_EQ(enum_def->node.enum_def.name, "Color", "Enum name should be Color");
	ASSERT_EQ(enum_def->node.enum_def.member_count, 3, "Should have three members");

	free_token_tree(ast);
	return 0;
}

// Test: Enum with explicit values
int test_enum_with_values(void)
{
	AST* ast = parse_input("enum Status { Ok = 0, Error = 1 }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* enum_def = ast->node.block.statements[0];
	ASSERT_EQ(enum_def->type, AST_ENUM_DEF, "Should be an enum definition");
	ASSERT_EQ(enum_def->node.enum_def.member_count, 2, "Should have two members");
	ASSERT(enum_def->node.enum_def.members[0].has_value, "First member should have value");
	ASSERT_EQ(enum_def->node.enum_def.members[0].value, 0, "First value should be 0");

	free_token_tree(ast);
	return 0;
}

// Test: Enum with type
int test_enum_with_type(void)
{
	AST* ast = parse_input("enum Status : u8 { Ok, Error }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* enum_def = ast->node.block.statements[0];
	ASSERT_EQ(enum_def->type, AST_ENUM_DEF, "Should be an enum definition");
	ASSERT_NOT_NULL(enum_def->node.enum_def.type, "Should have explicit type");
	ASSERT_STR_EQ(enum_def->node.enum_def.type, "u8", "Type should be u8");

	free_token_tree(ast);
	return 0;
}

// Test: Binary expression
int test_binary_expr(void)
{
	AST* ast = parse_input("i32 x = 1 + 2;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_NOT_NULL(var_def->node.var_def.equals, "Should have initializer");
	ASSERT_EQ(var_def->node.var_def.equals->type, AST_BINARY_EXPR, "Should be binary expression");
	ASSERT_EQ(var_def->node.var_def.equals->node.binary_expr.op.token_type, token_type_plus, "Should be addition");

	free_token_tree(ast);
	return 0;
}

// Test: Function call
int test_func_call(void)
{
	AST* ast = parse_input("print(42);");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* expr = ast->node.block.statements[0];
	ASSERT_EQ(expr->type, AST_FUNC_CALL, "Should be function call");
	ASSERT_EQ(expr->node.func_call.arg_count, 1, "Should have one argument");

	free_token_tree(ast);
	return 0;
}

// Test: Function call with multiple arguments
int test_func_call_multi_args(void)
{
	AST* ast = parse_input("add(1, 2, 3);");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* expr = ast->node.block.statements[0];
	ASSERT_EQ(expr->type, AST_FUNC_CALL, "Should be function call");
	ASSERT_EQ(expr->node.func_call.arg_count, 3, "Should have three arguments");

	free_token_tree(ast);
	return 0;
}

// Test: Member access (dot)
int test_member_access_dot(void)
{
	AST* ast = parse_input("point.x;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* expr = ast->node.block.statements[0];
	ASSERT_EQ(expr->type, AST_MEMBER_ACCESS, "Should be member access");
	ASSERT(expr->node.member_access.direct, "Should be direct access (dot)");

	free_token_tree(ast);
	return 0;
}

// Test: Member access (arrow)
int test_member_access_arrow(void)
{
	AST* ast = parse_input("ptr->x;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* expr = ast->node.block.statements[0];
	ASSERT_EQ(expr->type, AST_MEMBER_ACCESS, "Should be member access");
	ASSERT(!expr->node.member_access.direct, "Should be indirect access (arrow)");

	free_token_tree(ast);
	return 0;
}

// Test: Array indexing
int test_array_index(void)
{
	AST* ast = parse_input("arr[0];");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* expr = ast->node.block.statements[0];
	ASSERT_EQ(expr->type, AST_INDEX_EXPR, "Should be index expression");
	ASSERT_NOT_NULL(expr->node.index_expr.left, "Should have array");
	ASSERT_NOT_NULL(expr->node.index_expr.index, "Should have index");

	free_token_tree(ast);
	return 0;
}

// Test: If statement
int test_if_stmt(void)
{
	AST* ast = parse_input("if x > 0 { return x; }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* if_expr = ast->node.block.statements[0];
	ASSERT_EQ(if_expr->type, AST_IF_EXPR, "Should be if expression");
	ASSERT_NOT_NULL(if_expr->node.if_expr.condition, "Should have condition");
	ASSERT_NOT_NULL(if_expr->node.if_expr.then_block, "Should have then block");

	free_token_tree(ast);
	return 0;
}

// Test: If-else statement
int test_if_else_stmt(void)
{
	AST* ast = parse_input("if x > 0 { return x; } else { return 0; }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* if_expr = ast->node.block.statements[0];
	ASSERT_EQ(if_expr->type, AST_IF_EXPR, "Should be if expression");
	ASSERT_NOT_NULL(if_expr->node.if_expr.else_block, "Should have else block");

	free_token_tree(ast);
	return 0;
}

// Test: While loop
int test_while_loop(void)
{
	AST* ast = parse_input("while x < 10 { x = x + 1; }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* while_expr = ast->node.block.statements[0];
	ASSERT_EQ(while_expr->type, AST_WHILE_EXPR, "Should be while expression");
	ASSERT_NOT_NULL(while_expr->node.while_expr.condition, "Should have condition");
	ASSERT_NOT_NULL(while_expr->node.while_expr.then_block, "Should have body");

	free_token_tree(ast);
	return 0;
}

// Test: C-style for loop
int test_for_loop_c_style(void)
{
	AST* ast = parse_input("for (i32 i = 0; i < 10; i = i + 1) { }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* for_expr = ast->node.block.statements[0];
	ASSERT_EQ(for_expr->type, AST_FOR_EXPR, "Should be for expression");
	ASSERT_EQ(for_expr->node.for_expr.style, FOR_STYLE_C, "Should be C-style");
	ASSERT_NOT_NULL(for_expr->node.for_expr.c_style.init, "Should have init");
	ASSERT_NOT_NULL(for_expr->node.for_expr.c_style.condition, "Should have condition");
	ASSERT_NOT_NULL(for_expr->node.for_expr.c_style.increment, "Should have increment");

	free_token_tree(ast);
	return 0;
}

// Test: Rust-style for loop
int test_for_loop_rust_style(void)
{
	AST* ast = parse_input("for i32 i in 0..10 { }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* for_expr = ast->node.block.statements[0];
	ASSERT_EQ(for_expr->type, AST_FOR_EXPR, "Should be for expression");
	ASSERT_EQ(for_expr->node.for_expr.style, FOR_STYLE_RUST, "Should be Rust-style");
	ASSERT_NOT_NULL(for_expr->node.for_expr.rust_style.iterable, "Should have iterable");

	free_token_tree(ast);
	return 0;
}

// Test: Return statement
int test_return_stmt(void)
{
	AST* ast = parse_input("return 42;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* ret_stmt = ast->node.block.statements[0];
	ASSERT_EQ(ret_stmt->type, AST_RETURN_STMT, "Should be return statement");
	ASSERT_NOT_NULL(ret_stmt->node.return_stmt.return_stmt, "Should have return value");

	free_token_tree(ast);
	return 0;
}

// Test: Break statement
int test_break_stmt(void)
{
	AST* ast = parse_input("break;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* break_stmt = ast->node.block.statements[0];
	ASSERT_EQ(break_stmt->type, AST_BREAK_STMT, "Should be break statement");

	free_token_tree(ast);
	return 0;
}

// Test: Continue statement
int test_continue_stmt(void)
{
	AST* ast = parse_input("continue;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* cont_stmt = ast->node.block.statements[0];
	ASSERT_EQ(cont_stmt->type, AST_CONTINUE_STMT, "Should be continue statement");

	free_token_tree(ast);
	return 0;
}

// Test: Range expression
int test_range_expr(void)
{
	AST* ast = parse_input("i32 x = 0..10;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->node.var_def.equals->type, AST_RANGE_EXPR, "Should be range expression");
	ASSERT(!var_def->node.var_def.equals->node.range_expr.inclusive, "Should be exclusive");

	free_token_tree(ast);
	return 0;
}

// Test: Inclusive range expression
int test_range_expr_inclusive(void)
{
	AST* ast = parse_input("i32 x = 0..=10;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->node.var_def.equals->type, AST_RANGE_EXPR, "Should be range expression");
	ASSERT(var_def->node.var_def.equals->node.range_expr.inclusive, "Should be inclusive");

	free_token_tree(ast);
	return 0;
}

// Test: Array initialization
int test_array_init(void)
{
	AST* ast = parse_input("i32 x = [1, 2, 3];");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->node.var_def.equals->type, AST_ARRAY_INIT, "Should be array init");
	ASSERT_EQ(var_def->node.var_def.equals->node.array_init.element_count, 3, "Should have 3 elements");

	free_token_tree(ast);
	return 0;
}

// Test: Sized array initialization
int test_array_init_sized(void)
{
	AST* ast = parse_input("i32 x = [0; 10];");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->node.var_def.equals->type, AST_ARRAY_INIT, "Should be array init");
	ASSERT(var_def->node.var_def.equals->node.array_init.is_sized, "Should be sized");
	ASSERT_NOT_NULL(var_def->node.var_def.equals->node.array_init.size_expr, "Should have size");

	free_token_tree(ast);
	return 0;
}

// Test: Struct initialization
int test_struct_init(void)
{
	AST* ast = parse_input("@c_type Point\nPoint p = (Point){ .x = 1, .y = 2 };");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[1];
	ASSERT_EQ(var_def->node.var_def.equals->type, AST_STRUCT_INIT, "Should be struct init");
	ASSERT_EQ(var_def->node.var_def.equals->node.struct_init.field_count, 2, "Should have 2 fields");

	free_token_tree(ast);
	return 0;
}

// Test: Cast expression
int test_cast_expr(void)
{
	AST* ast = parse_input("i32 x = (i32)42.5;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->node.var_def.equals->type, AST_CAST_EXPR, "Should be cast expression");
	ASSERT_STR_EQ(var_def->node.var_def.equals->node.cast_expr.target_type.type.name, "i32", "Cast type should be i32");

	free_token_tree(ast);
	return 0;
}

// Test: Unary expression (dereference)
int test_unary_deref(void)
{
	AST* ast = parse_input("i32 x = *ptr;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->node.var_def.equals->type, AST_UNARY, "Should be unary expression");
	ASSERT_EQ(var_def->node.var_def.equals->node.unary_expr.op.token_type, token_type_star, "Should be dereference");

	free_token_tree(ast);
	return 0;
}

// Test: Unary expression (address-of)
int test_unary_address_of(void)
{
	AST* ast = parse_input("i32* ptr = &x;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->node.var_def.equals->type, AST_UNARY, "Should be unary expression");
	ASSERT_EQ(var_def->node.var_def.equals->node.unary_expr.op.token_type, token_type_ampersand,
			  "Should be address-of");

	free_token_tree(ast);
	return 0;
}

// Test: Import message
int test_import_message(void)
{
	AST* ast = parse_input("@import \"file.h\"");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* msg = ast->node.block.statements[0];
	ASSERT_EQ(msg->type, AST_MESSAGE, "Should be message");
	ASSERT_EQ(msg->node.message.msg, msg_import, "Should be import message");
	ASSERT_STR_EQ(msg->node.message.import.import, "file.h", "Import path should match");

	free_token_tree(ast);
	return 0;
}

// Test: C type message
int test_c_type_message(void)
{
	AST* ast = parse_input("@c_type FILE");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* msg = ast->node.block.statements[0];
	ASSERT_EQ(msg->type, AST_MESSAGE, "Should be message");
	ASSERT_EQ(msg->node.message.msg, msg_c_type, "Should be c_type message");
	ASSERT_STR_EQ(msg->node.message.c_type.type, "FILE", "Type should be FILE");

	free_token_tree(ast);
	return 0;
}

// Test: Block with trailing expression
int test_block_trailing_expr(void)
{
	AST* ast = parse_input("{ 42 }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* block = ast->node.block.statements[0];
	ASSERT_EQ(block->type, AST_BLOCK, "Should be block");
	ASSERT_NOT_NULL(block->node.block.trailing_expr, "Should have trailing expression");

	free_token_tree(ast);
	return 0;
}

// Test: Nested blocks
int test_nested_blocks(void)
{
	AST* ast = parse_input("{ { 42; } }");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* outer = ast->node.block.statements[0];
	ASSERT_EQ(outer->type, AST_BLOCK, "Should be block");
	ASSERT_EQ(outer->node.block.statement_count, 0,
			  "Should have 0 statements, because it will be parsed as trailing statement");

	AST* inner = outer->node.block.trailing_expr;
	ASSERT_EQ(inner->type, AST_BLOCK, "Inner should be block");

	free_token_tree(ast);
	return 0;
}

// Test: Complex expression with precedence
int test_precedence(void)
{
	AST* ast = parse_input("i32 x = 1 + 2 * 3;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	AST* expr = var_def->node.var_def.equals;
	ASSERT_EQ(expr->type, AST_BINARY_EXPR, "Should be binary expression");
	ASSERT_EQ(expr->node.binary_expr.op.token_type, token_type_plus, "Top level should be addition");
	ASSERT_EQ(expr->node.binary_expr.right->type, AST_BINARY_EXPR, "Right should be binary expr");
	ASSERT_EQ(expr->node.binary_expr.right->node.binary_expr.op.token_type, token_type_star,
			  "Should be multiplication");

	free_token_tree(ast);
	return 0;
}

// Test: Modifiers
int test_modifiers(void)
{
	AST* ast = parse_input("pub const i32 x = 42;");
	ASSERT_NOT_NULL(ast, "AST should not be nullptr");

	AST* var_def = ast->node.block.statements[0];
	ASSERT_EQ(var_def->node.var_def.modifier_count, 2, "Should have two modifiers");

	free_token_tree(ast);
	return 0;
}

// Main test runner
int main(void)
{
	int failed = 0;
	int total = 0;

#define RUN_TEST(test)                   \
	do                                   \
	{                                    \
		total++;                         \
		printf("Running %s... ", #test); \
		if (test() == 0)                 \
		{                                \
			printf("PASSED\n");          \
		}                                \
		else                             \
		{                                \
			printf("FAILED\n");          \
			failed++;                    \
		}                                \
	} while (0)

	RUN_TEST(test_empty_input);
	RUN_TEST(test_simple_var_decl);
	RUN_TEST(test_var_with_pointer);
	RUN_TEST(test_var_with_const_ref);
	RUN_TEST(test_array_decl);
	RUN_TEST(test_simple_func_def);
	RUN_TEST(test_func_no_params);
	RUN_TEST(test_func_void_return);
	RUN_TEST(test_struct_def);
	RUN_TEST(test_union_def);
	RUN_TEST(test_enum_def);
	RUN_TEST(test_enum_with_values);
	RUN_TEST(test_enum_with_type);
	RUN_TEST(test_binary_expr);
	RUN_TEST(test_func_call);
	RUN_TEST(test_func_call_multi_args);
	RUN_TEST(test_member_access_dot);
	RUN_TEST(test_member_access_arrow);
	RUN_TEST(test_array_index);
	RUN_TEST(test_if_stmt);
	RUN_TEST(test_if_else_stmt);
	RUN_TEST(test_while_loop);
	RUN_TEST(test_for_loop_c_style);
	RUN_TEST(test_for_loop_rust_style);
	RUN_TEST(test_return_stmt);
	RUN_TEST(test_break_stmt);
	RUN_TEST(test_continue_stmt);
	RUN_TEST(test_range_expr);
	RUN_TEST(test_range_expr_inclusive);
	RUN_TEST(test_array_init);
	RUN_TEST(test_array_init_sized);
	RUN_TEST(test_struct_init);
	RUN_TEST(test_cast_expr);
	RUN_TEST(test_unary_deref);
	RUN_TEST(test_unary_address_of);
	RUN_TEST(test_import_message);
	RUN_TEST(test_c_type_message);
	RUN_TEST(test_block_trailing_expr);
	RUN_TEST(test_nested_blocks);
	RUN_TEST(test_precedence);
	RUN_TEST(test_modifiers);
	printf("\n========================================\n");
	printf("Tests run: %d\n", total);
	printf("Tests passed: %d\n", total - failed);
	printf("Tests failed: %d\n", failed);
	printf("========================================\n");

	return failed > 0 ? 1 : 0;
}
