#include "code_gen.h"
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

#define ASSERT_NOT_NULL(ptr, message) ASSERT((ptr) != nullptr, message)

#define ASSERT_STR_CONTAINS(haystack, needle, message) \
	ASSERT(strstr(haystack, needle) != nullptr, message " (needle: " needle ")")

#define ASSERT_STR_NOT_CONTAINS(haystack, needle, message) \
	ASSERT(strstr(haystack, needle) == nullptr, message " (should not contain: " needle ")")

// Helper function to generate code from input
static NewFiles generate_code_from_input(const char* input, const char* filename) // NOLINT
{
	Token* tokens = lex(input);
	AST ast = parse(tokens);
	CodeGen code_gen = generate_code(ast, filename);
	NewFiles files = code_gen_to_files(&code_gen, "test");

	lex_free(tokens);
	free_token_tree(ast);
	code_gen_free_code_gen(code_gen);

	return files;
}

// Test: Empty input
int test_empty_input(void)
{
	NewFiles files = generate_code_from_input("", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");
	ASSERT_NOT_NULL(files.h_file, "H file should not be nullptr");

	ASSERT_STR_CONTAINS(files.h_file, "#pragma once", "Should have pragma once");
	ASSERT_STR_CONTAINS(files.h_file, "\n#ifdef __cplusplus\nextern \"C\"{\n#endif\n",
						"Should have \"extern \"C\"\" for C++");
	ASSERT_STR_CONTAINS(files.h_file, "\n#ifdef __cplusplus\n}\n#endif\n",
						"Should have a closing \"}\" for \"extern \"C\"\" for C++");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Simple variable declaration
int test_simple_var_decl(void)
{
	NewFiles files = generate_code_from_input("i32 x = 42;", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "const i32 x", "Should have const i32 x");
	ASSERT_STR_CONTAINS(files.c_file, "=42", "Should have =42");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Mutable variable declaration
int test_mut_var_decl(void)
{
	NewFiles files = generate_code_from_input("mut i32 x = 42;", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "i32 x", "Should have i32 x");
	ASSERT_STR_NOT_CONTAINS(files.c_file, "const i32 x", "Should not have const");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Variable with pointer
int test_var_with_pointer(void)
{
	NewFiles files = generate_code_from_input("mut i32* ptr = 0;", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "i32*", "Should have i32*");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Variable with const pointer
int test_var_with_const_pointer(void)
{
	NewFiles files = generate_code_from_input("i32& ptr = 0;", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "const i32* const", "Should have i32* const");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Array declaration
int test_array_decl(void)
{
	NewFiles files = generate_code_from_input("i32 arr[10] = 0;", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "const i32 arr[10]", "Should have i32 arr[10]");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Simple function definition
int test_simple_func_def(void)
{
	NewFiles files = generate_code_from_input("fn add(i32 a, i32 b) -> i32 { return a + b; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "i32 add(", "Should have function signature");
	ASSERT_STR_CONTAINS(files.c_file, "const i32 a", "Should have const i32 a parameter");
	ASSERT_STR_CONTAINS(files.c_file, "const i32 b", "Should have const i32 b parameter");
	ASSERT_STR_CONTAINS(files.c_file, "return (a+b)", "Should have return statement");
	ASSERT_STR_CONTAINS(files.c_file, "static", "Private function should be static");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Public function definition
int test_pub_func_def(void)
{
	NewFiles files = generate_code_from_input("pub fn add(i32 a, i32 b) -> i32 { return a + b; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");
	ASSERT_NOT_NULL(files.h_file, "H file should not be nullptr");

	ASSERT_STR_CONTAINS(files.h_file, "extern i32 add(", "Should have function declaration in header");
	ASSERT_STR_CONTAINS(files.c_file, "i32 add(", "Should have function definition in C file");
	ASSERT_STR_NOT_CONTAINS(files.c_file, "static i32 add", "Public function should not be static");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Function with no parameters
int test_func_no_params(void)
{
	NewFiles files = generate_code_from_input("fn main() -> i32 { return 0; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "i32 main()", "Should have main()");
	ASSERT_STR_CONTAINS(files.c_file, "return 0", "Should have return 0");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Function with void return
int test_func_void_return(void)
{
	NewFiles files = generate_code_from_input("fn print() { }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "void print()", "Should have void return type");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Struct definition
int test_struct_def(void)
{
	NewFiles files = generate_code_from_input("struct Point { i32 x, i32 y }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "typedef struct Point Point", "Should have typedef");
	ASSERT_STR_CONTAINS(files.c_file, "struct Point{", "Should have struct Point");
	ASSERT_STR_CONTAINS(files.c_file, "i32 x", "Should have i32 x member");
	ASSERT_STR_CONTAINS(files.c_file, "i32 y", "Should have i32 y member");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Public struct definition
int test_pub_struct_def(void)
{
	NewFiles files = generate_code_from_input("pub struct Point { i32 x, i32 y }", "test.sl");
	ASSERT_NOT_NULL(files.h_file, "H file should not be nullptr");

	ASSERT_STR_CONTAINS(files.h_file, "typedef struct Point Point", "Should have typedef in header");
	ASSERT_STR_CONTAINS(files.h_file, "struct Point{", "Should have struct in header");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Union definition
int test_union_def(void)
{
	NewFiles files = generate_code_from_input("union Value { i32 i, f32 f }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "typedef union Value Value", "Should have typedef");
	ASSERT_STR_CONTAINS(files.c_file, "union Value{", "Should have union Value");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Enum definition
int test_enum_def(void)
{
	NewFiles files = generate_code_from_input("enum Color { Red, Green, Blue }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "typedef enum Color Color", "Should have typedef");
	ASSERT_STR_CONTAINS(files.c_file, "enum Color{", "Should have enum Color");
	ASSERT_STR_CONTAINS(files.c_file, "Red,", "Should have Red");
	ASSERT_STR_CONTAINS(files.c_file, "Green,", "Should have Green");
	ASSERT_STR_CONTAINS(files.c_file, "Blue,", "Should have Blue");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Enum with explicit values
int test_enum_with_values(void)
{
	NewFiles files = generate_code_from_input("enum Status { Ok = 0, Error = 1 }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "Ok=0,", "Should have Ok=0");
	ASSERT_STR_CONTAINS(files.c_file, "Error=1,", "Should have Error=1");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Enum with type
int test_enum_with_type(void)
{
	NewFiles files = generate_code_from_input("enum Status : u8 { Ok, Error }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "enum Status:u8", "Should have enum with type");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Binary expression
int test_binary_expr(void)
{
	NewFiles files = generate_code_from_input("i32 x = 1 + 2 * 3;", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "(1+(2*3))", "Should have correct binary expression");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Function call
int test_func_call(void)
{
	NewFiles files = generate_code_from_input("fn main() { print(42); }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "print(42)", "Should have function call");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Function call with multiple arguments
int test_func_call_multi_args(void)
{
	NewFiles files = generate_code_from_input("fn main() { add(1, 2, 3); }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "add(1,2,3)", "Should have function call with multiple args");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Member access (dot)
int test_member_access_dot(void)
{
	NewFiles files = generate_code_from_input("fn main() { point.x; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "point.x", "Should have member access with dot");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Member access (arrow)
int test_member_access_arrow(void)
{
	NewFiles files = generate_code_from_input("fn main() { ptr->x; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "ptr->x", "Should have member access with arrow");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Array indexing
int test_array_index(void)
{
	NewFiles files = generate_code_from_input("fn main() { arr[0]; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "(arr[0])", "Should have array indexing");

	code_gen_free_new_files(files);
	return 0;
}

// Test: If statement
int test_if_stmt(void)
{
	NewFiles files = generate_code_from_input("fn main() { if x > 0 { return x; } }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "if((x>0))", "Should have if condition");
	ASSERT_STR_CONTAINS(files.c_file, "return x", "Should have return statement");

	code_gen_free_new_files(files);
	return 0;
}

// Test: If-else statement
int test_if_else_stmt(void)
{
	NewFiles files = generate_code_from_input("fn main() { if x > 0 { return x; } else { return 0; } }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "if((x>0))", "Should have if condition");
	ASSERT_STR_CONTAINS(files.c_file, "else", "Should have else");

	code_gen_free_new_files(files);
	return 0;
}

// Test: While loop
int test_while_loop(void)
{
	NewFiles files = generate_code_from_input("fn main() { while x < 10 { x = x + 1; } }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "while((x<10))", "Should have while condition");

	code_gen_free_new_files(files);
	return 0;
}

// Test: C-style for loop
int test_for_loop_c_style(void)
{
	NewFiles files = generate_code_from_input("fn main() { for (i32 i = 0; i < 10; i = i + 1) { } }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "for(", "Should have for loop");
	ASSERT_STR_CONTAINS(files.c_file, "const i32 i=0;", "Should have init");
	ASSERT_STR_CONTAINS(files.c_file, "(i<10);", "Should have condition");
	ASSERT_STR_CONTAINS(files.c_file, "(i=(i+1))", "Should have increment");
	ASSERT_STR_CONTAINS(files.c_file, "for(const i32 i=0;(i<10);(i=(i+1)))", "Should have a full for loop defined");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Rust-style for loop
int test_for_loop_rust_style(void)
{
	NewFiles files = generate_code_from_input("fn main() { for i32 i in 0..10 { } }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "sl_tmp_start", "Should have temp start variable");
	ASSERT_STR_CONTAINS(files.c_file, "sl_tmp_end", "Should have temp end variable");
	ASSERT_STR_CONTAINS(files.c_file, "sl_tmp_dir", "Should have temp direction variable");
	ASSERT_STR_CONTAINS(files.c_file, "for(", "Should have for loop");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Return statement
int test_return_stmt(void)
{
	NewFiles files = generate_code_from_input("fn main() -> i32 { return 42; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "return 42", "Should have return statement");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Break statement
int test_break_stmt(void)
{
	NewFiles files = generate_code_from_input("fn main() { while true { break; } }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "break;", "Should have break statement");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Continue statement
int test_continue_stmt(void)
{
	NewFiles files = generate_code_from_input("fn main() { while true { continue; } }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "continue;", "Should have continue statement");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Array initialization
int test_array_init(void)
{
	NewFiles files = generate_code_from_input("i32 x = [1, 2, 3];", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "{1,2,3,}", "Should have array initialization");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Sized array initialization
int test_array_init_sized(void)
{
	NewFiles files = generate_code_from_input("i32 x = [0; 3];", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "{0,0,0,}", "Should have sized array initialization");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Struct initialization
int test_struct_init(void)
{
	NewFiles files = generate_code_from_input("@c_type Point\nPoint p = (Point){ .x = 1, .y = 2 };", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "{.x=1,.y=2,}", "Should have struct initialization");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Cast expression
int test_cast_expr(void)
{
	NewFiles files = generate_code_from_input("i32 x = (i32)42.5;", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "((const i32 )", "Should have cast");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Unary expression (dereference)
int test_unary_deref(void)
{
	NewFiles files = generate_code_from_input("mut i32 x = *ptr;", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "(*ptr)", "Should have dereference");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Unary expression (address-of)
int test_unary_address_of(void)
{
	NewFiles files = generate_code_from_input("i32* ptr = &x;", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "(&x)", "Should have address-of");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Import message
int test_import_message(void)
{
	NewFiles files = generate_code_from_input("@import \"file.h\"", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "#include \"file.h\"", "Should have include directive");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Literal string
int test_literal_string(void)
{
	NewFiles files = generate_code_from_input("fn main() { \"hello\"; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "\"hello\"", "Should have string literal");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Literal number
int test_literal_number(void)
{
	NewFiles files = generate_code_from_input("fn main() { 42; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "42", "Should have number literal");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Literal boolean
int test_literal_bool(void)
{
	NewFiles files = generate_code_from_input("fn main() { true; false; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "true", "Should have true literal");
	ASSERT_STR_CONTAINS(files.c_file, "false", "Should have false literal");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Static variable modifier
int test_static_var_modifier(void)
{
	NewFiles files = generate_code_from_input("fn main() { static i32 x = 42; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "static", "Should have static keyword");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Public variable
int test_pub_var(void)
{
	NewFiles files = generate_code_from_input("pub i32 x = 42;", "test.sl");
	ASSERT_NOT_NULL(files.h_file, "H file should not be nullptr");

	ASSERT_STR_CONTAINS(files.h_file, "extern", "Should have extern in header");

	code_gen_free_new_files(files);
	return 0;
}

// Test: Block with trailing expression
int test_block_trailing_expr(void)
{
	NewFiles files = generate_code_from_input("fn get_value() -> i32 { 42 }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "42;", "Should have trailing expression");

	code_gen_free_new_files(files);
	return 0;
}

// Test: String escaping
int test_string_escaping(void)
{
	NewFiles files = generate_code_from_input("fn main() { \"hello\\nworld\"; }", "test.sl");
	ASSERT_NOT_NULL(files.c_file, "C file should not be nullptr");

	ASSERT_STR_CONTAINS(files.c_file, "\\n", "Should have escaped newline");

	code_gen_free_new_files(files);
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
	RUN_TEST(test_mut_var_decl);
	RUN_TEST(test_var_with_pointer);
	RUN_TEST(test_var_with_const_pointer);
	RUN_TEST(test_array_decl);
	RUN_TEST(test_simple_func_def);
	RUN_TEST(test_pub_func_def);
	RUN_TEST(test_func_no_params);
	RUN_TEST(test_func_void_return);
	RUN_TEST(test_struct_def);
	RUN_TEST(test_pub_struct_def);
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
	RUN_TEST(test_array_init);
	RUN_TEST(test_array_init_sized);
	RUN_TEST(test_struct_init);
	RUN_TEST(test_cast_expr);
	RUN_TEST(test_unary_deref);
	RUN_TEST(test_unary_address_of);
	RUN_TEST(test_import_message);
	RUN_TEST(test_literal_string);
	RUN_TEST(test_literal_number);
	RUN_TEST(test_literal_bool);
	RUN_TEST(test_static_var_modifier);
	RUN_TEST(test_pub_var);
	RUN_TEST(test_block_trailing_expr);
	RUN_TEST(test_string_escaping);

	printf("\n========================================\n");
	printf("Tests run: %d\n", total);
	printf("Tests passed: %d\n", total - failed);
	printf("Tests failed: %d\n", failed);
	printf("========================================\n");

	return failed > 0 ? 1 : 0;
}
