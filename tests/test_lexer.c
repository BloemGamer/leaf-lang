#include "lexer.h"
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

// Helper function to count tokens
static int count_tokens(Token* tokens)
{
	int count = 0;
	while (tokens[count].token_type != token_type_eof)
	{
		count++;
	}
	return count;
}

// Test: Empty input
int test_empty_input(void)
{
	Token* tokens = lex("");
	ASSERT(tokens != nullptr, "Tokens should not be nullptr");
	ASSERT_EQ(tokens[0].token_type, token_type_sof, "First token should be SOF");
	ASSERT_EQ(tokens[1].token_type, token_type_eof, "Second token should be EOF");
	lex_free(tokens);
	return 0;
}

// Test: nullptr input
int test_null_input(void)
{
	Token* tokens = lex(nullptr);
	ASSERT(tokens != nullptr, "Tokens should not be nullptr");
	ASSERT_EQ(tokens[0].token_type, token_type_sof, "First token should be SOF");
	ASSERT_EQ(tokens[1].token_type, token_type_eof, "Second token should be EOF");
	lex_free(tokens);
	return 0;
}

// Test: Whitespace handling
int test_whitespace(void)
{
	Token* tokens = lex("   \t\n  ");
	ASSERT_EQ(tokens[0].token_type, token_type_sof, "First token should be SOF");
	ASSERT_EQ(tokens[1].token_type, token_type_eof, "Should skip all whitespace");
	lex_free(tokens);
	return 0;
}

// Test: Single line comment
int test_single_line_comment(void)
{
	Token* tokens = lex("// this is a comment\n");
	ASSERT_EQ(tokens[0].token_type, token_type_sof, "First token should be SOF");
	ASSERT_EQ(tokens[1].token_type, token_type_eof, "Comment should be ignored");
	lex_free(tokens);
	return 0;
}

// Test: Multi-line comment
int test_multi_line_comment(void)
{
	Token* tokens = lex("/* this is\na multi-line\ncomment */");
	ASSERT_EQ(tokens[0].token_type, token_type_sof, "First token should be SOF");
	ASSERT_EQ(tokens[1].token_type, token_type_eof, "Comment should be ignored");
	lex_free(tokens);
	return 0;
}

// Test: String literal
int test_string_literal(void)
{
	Token* tokens = lex("\"hello world\"");
	ASSERT_EQ(tokens[0].token_type, token_type_sof, "First token should be SOF");
	ASSERT_EQ(tokens[1].token_type, token_type_string, "Should lex string");
	ASSERT_STR_EQ(tokens[1].str_val, "hello world", "String value should match");
	ASSERT_EQ(tokens[2].token_type, token_type_eof, "Last token should be EOF");
	lex_free(tokens);
	return 0;
}

// Test: String with escape sequences
int test_string_with_escapes(void)
{
	Token* tokens = lex("\"hello\\\"world\"");
	ASSERT_EQ(tokens[1].token_type, token_type_string, "Should lex string with escapes");
	ASSERT_STR_EQ(tokens[1].str_val, "hello\\\"world", "Should preserve escape sequences");
	lex_free(tokens);
	return 0;
}

// Test: Character literal
int test_char_literal(void)
{
	Token* tokens = lex("'a'");
	ASSERT_EQ(tokens[1].token_type, token_type_char, "Should lex char");
	ASSERT_STR_EQ(tokens[1].str_val, "a", "Char value should match");
	lex_free(tokens);
	return 0;
}

// Test: Integer literal
int test_integer_literal(void)
{
	Token* tokens = lex("12345");
	ASSERT_EQ(tokens[1].token_type, token_type_number, "Should lex integer");
	ASSERT_STR_EQ(tokens[1].str_val, "12345", "Number value should match");
	lex_free(tokens);
	return 0;
}

// Test: Float literal
int test_float_literal(void)
{
	Token* tokens = lex("123.456");
	ASSERT_EQ(tokens[1].token_type, token_type_float, "Should lex float");
	ASSERT_STR_EQ(tokens[1].str_val, "123.456", "Float value should match");
	lex_free(tokens);
	return 0;
}

// Test: Hexadecimal number
int test_hex_number(void)
{
	Token* tokens = lex("0xFF");
	ASSERT_EQ(tokens[1].token_type, token_type_number, "Should lex hex number");
	ASSERT_STR_EQ(tokens[1].str_val, "0xFF", "Hex value should match");
	lex_free(tokens);
	return 0;
}

// Test: Binary number
int test_binary_number(void)
{
	Token* tokens = lex("0b1010");
	ASSERT_EQ(tokens[1].token_type, token_type_number, "Should lex binary number");
	ASSERT_STR_EQ(tokens[1].str_val, "0b1010", "Binary value should match");
	lex_free(tokens);
	return 0;
}

// Test: Octal number
int test_octal_number(void)
{
	Token* tokens = lex("0o777");
	ASSERT_EQ(tokens[1].token_type, token_type_number, "Should lex octal number");
	ASSERT_STR_EQ(tokens[1].str_val, "0o777", "Octal value should match");
	lex_free(tokens);
	return 0;
}

// Test: Identifier
int test_identifier(void)
{
	Token* tokens = lex("my_variable");
	ASSERT_EQ(tokens[1].token_type, token_type_identifier, "Should lex identifier");
	ASSERT_STR_EQ(tokens[1].str_val, "my_variable", "Identifier should match");
	lex_free(tokens);
	return 0;
}

// Test: Message token (starts with @)
int test_message_token(void)
{
	Token* tokens = lex("@message");
	ASSERT_EQ(tokens[1].token_type, token_type_message, "Should lex message token");
	ASSERT_STR_EQ(tokens[1].str_val, "@message", "Message should match");
	lex_free(tokens);
	return 0;
}

// Test: Simple operators
int test_simple_operators(void)
{
	Token* tokens = lex("+ - * /");
	int count = count_tokens(tokens);
	ASSERT(count >= 4, "Should have at least 4 operator tokens");
	lex_free(tokens);
	return 0;
}

// Test: Parentheses and braces
int test_delimiters(void)
{
	Token* tokens = lex("(){}[]");
	int count = count_tokens(tokens);
	ASSERT(count >= 6, "Should have 6 delimiter tokens");
	lex_free(tokens);
	return 0;
}

// Test: Mixed tokens
int test_mixed_tokens(void)
{
	Token* tokens = lex("int x = 42;");
	ASSERT_EQ(tokens[0].token_type, token_type_sof, "First should be SOF");
	// Should have: keyword(int), identifier(x), operator(=), number(42), semicolon
	int count = count_tokens(tokens);
	ASSERT(count >= 4, "Should have multiple tokens");
	lex_free(tokens);
	return 0;
}

// Test: Position tracking
int test_position_tracking(void)
{
	Token* tokens = lex("x\ny");
	ASSERT_EQ(tokens[1].pos.line, 1, "First token on line 1");
	ASSERT_EQ(tokens[1].pos.character, 1, "First token at character 1");
	ASSERT_EQ(tokens[2].pos.line, 2, "Second token on line 2");
	ASSERT_EQ(tokens[2].pos.character, 1, "Second token at character 1");
	lex_free(tokens);
	return 0;
}

// Test: Multiple identifiers
int test_multiple_identifiers(void)
{
	Token* tokens = lex("foo bar baz");
	ASSERT_EQ(tokens[1].token_type, token_type_identifier, "First should be identifier");
	ASSERT_EQ(tokens[2].token_type, token_type_identifier, "Second should be identifier");
	ASSERT_EQ(tokens[3].token_type, token_type_identifier, "Third should be identifier");
	ASSERT_STR_EQ(tokens[1].str_val, "foo", "First identifier");
	ASSERT_STR_EQ(tokens[2].str_val, "bar", "Second identifier");
	ASSERT_STR_EQ(tokens[3].str_val, "baz", "Third identifier");
	lex_free(tokens);
	return 0;
}

// Test: Underscore in identifier
int test_identifier_with_underscore(void)
{
	Token* tokens = lex("_private __dunder");
	ASSERT_EQ(tokens[1].token_type, token_type_identifier, "Should lex _private");
	ASSERT_EQ(tokens[2].token_type, token_type_identifier, "Should lex __dunder");
	lex_free(tokens);
	return 0;
}

// Test: Complex expression
int test_complex_expression(void)
{
	Token* tokens = lex("(x + y) * 2.5");
	int count = count_tokens(tokens);
	ASSERT(count >= 7, "Should have multiple tokens for expression");
	lex_free(tokens);
	return 0;
}

// Test: String at end of input (edge case)
int test_string_at_end(void)
{
	Token* tokens = lex("\"test\"");
	ASSERT_EQ(tokens[1].token_type, token_type_string, "Should lex string");
	ASSERT_STR_EQ(tokens[1].str_val, "test", "String value should match");
	lex_free(tokens);
	return 0;
}

// Test: Number followed by identifier
int test_number_then_identifier(void)
{
	Token* tokens = lex("42 answer");
	ASSERT_EQ(tokens[1].token_type, token_type_number, "Should lex number");
	ASSERT_EQ(tokens[2].token_type, token_type_identifier, "Should lex identifier");
	lex_free(tokens);
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
	RUN_TEST(test_null_input);
	RUN_TEST(test_whitespace);
	RUN_TEST(test_single_line_comment);
	RUN_TEST(test_multi_line_comment);
	RUN_TEST(test_string_literal);
	RUN_TEST(test_string_with_escapes);
	RUN_TEST(test_char_literal);
	RUN_TEST(test_integer_literal);
	RUN_TEST(test_float_literal);
	RUN_TEST(test_hex_number);
	RUN_TEST(test_binary_number);
	RUN_TEST(test_octal_number);
	RUN_TEST(test_identifier);
	RUN_TEST(test_message_token);
	RUN_TEST(test_simple_operators);
	RUN_TEST(test_delimiters);
	RUN_TEST(test_mixed_tokens);
	RUN_TEST(test_position_tracking);
	RUN_TEST(test_multiple_identifiers);
	RUN_TEST(test_identifier_with_underscore);
	RUN_TEST(test_complex_expression);
	RUN_TEST(test_string_at_end);
	RUN_TEST(test_number_then_identifier);

	printf("\n========================================\n");
	printf("Tests run: %d\n", total);
	printf("Tests passed: %d\n", total - failed);
	printf("Tests failed: %d\n", failed);
	printf("========================================\n");

	return failed > 0 ? 1 : 0;
}
