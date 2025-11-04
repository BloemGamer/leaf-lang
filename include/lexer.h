#include <stdint.h>


typedef struct {}__token_keyword;
typedef struct {}__token_simple;
typedef struct {}__token_litteral;
typedef struct {}__token_identefier;
typedef struct {}__token_ignored;
typedef struct {}__token_invalid;
typedef struct {}__token_eof;

#define SECOND(a, b, ...) b
#define X_HELPER(a, b, c) X(a, b, c)
#define Y(a, b, ...) X_HELPER(a, b, SECOND(a, ##__VA_ARGS__, void))

#define __tokens \
	Y(token_type_eof, __token_eof)							/* always appended as the End Of File (Last token, much like string terminator) */ \
	Y(token_type_invalid, __token_invallid)					/* only used for errors */ \
\
	/* simple tokens: */ \
	Y(token_type_lparen, __token_simple)					/* ( */ \
	Y(token_type_rparen, __token_simple)					/* ) */ \
	Y(token_type_lbrace, __token_simple)					/* { */ \
	Y(token_type_rbrace, __token_simple)					/* } */ \
	Y(token_type_lsqbracket, __token_simple)				/* [ */ \
	Y(token_type_rsqbracket, __token_simple)				/* ] */ \
	Y(token_type_dot, __token_simple)						/* . */ \
	Y(token_type_comma, __token_simple)						/* , */ \
	Y(token_type_plus, __token_simple)						/* + */ \
	Y(token_type_minus, __token_simple)						/* - */ \
	Y(token_type_star, __token_simple)						/* * */ \
	Y(token_type_slash, __token_simple)						/* / */ \
	Y(token_type_semicolon, __token_simple)					/* ; */ \
	Y(token_type_bang, __token_simple)						/* ! */ \
	Y(token_type_equal, __token_simple)						/* = */ \
	Y(token_type_greater, __token_simple)					/* > */ \
	Y(token_type_less, __token_simple)						/* < */ \
	Y(token_type_ampersand, __token_simple)					/* & */ \
	Y(token_type_caret, __token_simple)						/* ^ */ \
	Y(token_type_pipe, __token_simple)						/* | */ \
	Y(token_type_tilda, __token_simple)						/* ~ */ \
	Y(token_type_arrow, __token_simple)						/* -> */ \
	Y(token_type_lshift, __token_simple)					/* << */ \
	Y(token_type_rshift, __token_simple)					/* >> */ \
	Y(token_type_and, __token_simple)						/* && */ \
	Y(token_type_or, __token_simple)						/* || */ \
	/*ompound tokens: */ \
	Y(token_type_bang_equal, __token_simple)				/* != */ \
	Y(token_type_equal_equal, __token_simple)				/* == */ \
	Y(token_type_greater_equal, __token_simple)				/* >= */ \
	Y(token_type_less_equal, __token_simple)				/* <= */ \
	/*ignored:*/ \
	Y(token_type_whitespace, __token_ignored)				/* \t, \n, \r or <space> */ \
	Y(token_type_comment, __token_ignored)					/* // (both comments) */ \
\
	/*literals: */ \
	Y(token_type_string, __token_literal, char*)			/* string literal "..." */ \
	Y(token_type_invalid_string, __token_literal, char*)	/* invalid string literal "... EOF */ \
	Y(token_type_number, __token_literal, int64_t)			/* any number like: 5 or 5.23 */ \
	Y(token_type_char, __token_literal, char)				/* any char like : 'E' */ \
\
	/*identifier: */ \
	Y(token_type_identifier, __token_identefier, char*)		/* id */ \
	Y(token_type_type, __token_identefier, char*)			/* type */ \
\
	/*keywords: */ \
	Y(token_type_if, __token_keyword)						/* if */ \
	Y(token_type_else, __token_keyword)						/* else */ \
	Y(token_type_true, __token_keyword)						/* true */ \
	Y(token_type_false, __token_keyword)					/* false */ \
	Y(token_type_while, __token_keyword)					/* while */ \
	Y(token_type_for, __token_keyword)						/* for */ \
	Y(token_type_fun, __token_keyword)						/* fn */ \
	Y(token_type_struct, __token_keyword)					/* struct */ \
	Y(token_type_enum, __token_keyword)						/* enum */ \
	Y(token_type_return, __token_keyword)					/* return */

typedef enum
{
#define X(x, _a, _b) x,
	__tokens
#undef X
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

/// Returns a pointer to a string, these should not be freed
char *token_to_string(Token *token);
