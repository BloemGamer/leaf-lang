#include <stdint.h>

#define __tokens \
	X(token_type_eof)				/* always appended as the End Of File (Last token, much like string terminator) */ \
	X(token_type_invalid)			/* only used for errors */ \
\
	/* simple tokens: */ \
	X(token_type_lparen)			/* ( */ \
	X(token_type_rparen)			/* ) */ \
	X(token_type_lbrace)			/* { */ \
	X(token_type_rbrace)			/* } */ \
	X(token_type_lsqbracket)		/* [ */ \
	X(token_type_rsqbracket)		/* ] */ \
	X(token_type_dot)				/* . */ \
	X(token_type_comma)				/* , */ \
	X(token_type_plus)				/* + */ \
	X(token_type_minus)				/* - */ \
	X(token_type_star)				/* * */ \
	X(token_type_slash)				/* / */ \
	X(token_type_semicolon)			/* ; */ \
	X(token_type_bang)				/* ! */ \
	X(token_type_equal)				/* = */ \
	X(token_type_greater)			/* > */ \
	X(token_type_less)				/* < */ \
	X(token_type_ampersand)			/* & */ \
	X(token_type_caret)				/* ^ */ \
	X(token_type_pipe)				/* | */ \
	X(token_type_tilda)				/* ~ */ \
	X(token_type_arrow)				/* -> */ \
	X(token_type_lshift)			/* << */ \
	X(token_type_rshift)			/* >> */ \
\
	/*ompound tokens: */ \
	X(token_type_bang_equal)		/* != */ \
	X(token_type_equal_equal)		/* == */ \
	X(token_type_greater_equal)		/* >= */ \
	X(token_type_less_equal)		/* <= */ \
\
	/*ignored:*/ \
	X(token_type_whitespace)		/* \t, \n, \r or <space> */ \
	X(token_type_comment)			/* // (both comments) */ \
\
	/*literals: */ \
	X(token_type_string)			/* string literal "..." */ \
	X(token_type_invalid_string)	/* invalid string literal "... EOF */ \
	X(token_type_number)			/* any number like: 5 or 5.23 */ \
	X(token_type_char)				/* any char like : 'E' */ \
\
	/*identifier: */ \
	X(token_type_identifier)		/* id */ \
\
	/*keywords: */ \
	X(token_type_if)				/* if */ \
	X(token_type_else)				/* else */ \
	X(token_type_and)				/* && */ \
	X(token_type_or)				/* || */ \
	X(token_type_false)				/* false */ \
	X(token_type_true)				/* true */ \
	X(token_type_while)				/* while loop */ \
	X(token_type_for)				/* for loop */ \
	X(token_type_var)				/* variable */ \
	X(token_type_fun)				/* function */ \
	X(token_type_return)			/* return */


typedef enum
{
#define X(x) x,
	__tokens
#undef X
	// token_type_eof,				// always appended as the End Of File (Last token, much like string terminator)
	// token_type_invalid,			// only used for errors
	//
	// //simple tokens:
	// token_type_lparen,			// (
	// token_type_rparen,			// )
	// token_type_lbrace,			// {
	// token_type_rbrace,			// }
	// token_type_lsqbracket,		// [
	// token_type_rsqbracket,		// ]
	// token_type_dot,				// .
	// token_type_comma,			// ,
	// token_type_plus,			// +
	// token_type_minus,			// -
	// token_type_star,			// *
	// token_type_slash,			// /
	// token_type_semicolon,		// ;
	// token_type_bang,			// !
	// token_type_equal,			// =
	// token_type_greater,			// >
	// token_type_less,			// <
	// token_type_ampersand,		// &
	// token_type_caret,			// ^
	// token_type_pipe,			// |
	// token_type_tilda,			// ~
	//
	// //compound tokens:
	// token_type_bang_equal,		// !=
	// token_type_equal_equal,		// ==
	// token_type_greater_equal,	// >=
	// token_type_less_equal,		// <=
	//
	// //ignored:
	// token_type_whitespace,		// \t, \n, \r or <space>
	// token_type_comment,			// //
	//
	// //literals:
	// token_type_string,			// string literal "..."
	// token_type_invalid_string,	// invalid string literal "... EOF
	// token_type_number,			// any number like: 5 or 5.23
	// token_type_char,			// any char like : 'E'
	//
	// //identifier:
	// token_type_identifier,		// id
	//
	// //keywords:
	// token_type_if,				// if
	// token_type_else,			// else
	// token_type_and,				// &&
	// token_type_or,				// ||
	// token_type_false,			// false
	// token_type_true,			// true
	// token_type_while,			// while loop
	// token_type_for,				// for loop
	// token_type_var,				// variable
	// token_type_fun,				// function
	// token_type_return,			// return

} Token_type;

typedef struct
{
	Token_type token_type;
	union
	{
		char *str_val;
		int64_t int_val;
		double float_val;
	} token;

} Token;


/// Returns an array of Token, that always ends with token_type_eof, the input can be deallocated if you want
[[gnu::warn_unused_result]]
Token *lex(const char *input);
char *token_to_string(Token *token);
