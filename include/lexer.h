#include <stdint.h>

typedef enum
{
	token_type_invalid,			// only used for errors
	token_type_eof,				// always appended as the End Of File (Last token, much like string terminator)

	//simple tokens:
	token_type_lparen,			// (
	token_type_rparen,			// )
	token_type_lbrace,			// {
	token_type_rbrace,			// }
	token_type_lsqbracket,		// [
	token_type_rsqbracket,		// ]
	token_type_dot,				// .
	token_type_comma,			// ,
	token_type_plus,			// +
	token_type_minus,			// -
	token_type_star,			// *
	token_type_slash,			// /
	token_type_semicolon,		// ;
	token_type_bang,			// !
	token_type_equal,			// =
	token_type_greater,			// >
	token_type_less,			// <

	//compound tokens:
	token_type_bang_equal,		// !=
	token_type_equal_equal,		// ==
	token_type_greater_equal,	// >=
	token_type_less_equal,		// <=

	//ignored:
	token_type_whitespace,		// \t, \n, \r or <space>
	token_type_comment,			// //

	//literals:
	token_type_string,			// string literal "..."
	token_type_invalid_string,	// invalid string literal "...
	token_type_number,			// any number like: 5 or 5.23
	token_type_char,			// any char like : 'E'

	//identifier:
	token_type_identifier,		// id

	//keywords:
	token_type_if,				// if
	token_type_else,			// else
	token_type_and,				// &&
	token_type_or,				// ||
	token_type_false,			// false
	token_type_true,			// true
	token_type_while,			// while loop
	token_type_for,				// for loop
	token_type_var,				// variable
	token_type_fun,				// function
	token_type_return,			// return

} token_type_t;

typedef struct
{
	token_type_t token_type;
	union
	{
		char *str_val;
		int64_t int_val;
		double float_val;
	} token;

} token_t;


token_type_t *lex(const char *input);
