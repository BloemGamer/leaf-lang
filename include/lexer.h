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
	X(token_type_and)				/* && */ \
	X(token_type_or)				/* || */ \
	/*ompound tokens: */ \
	X(token_type_bang_equal)		/* != */ \
	X(token_type_equal_equal)		/* == */ \
	X(token_type_greater_equal)		/* >= */ \
	X(token_type_less_equal)		/* <= */ \
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
	X(token_type_type)				/* type */ \
\
	/*keywords: */ \
	X(token_type_if)				/* if */ \
	X(token_type_else)				/* else */ \
	X(token_type_true)				/* true */ \
	X(token_type_false)				/* false */ \
	X(token_type_while)				/* while */ \
	X(token_type_for)				/* for */ \
	X(token_type_fun)				/* fn */ \
	X(token_type_return)			/* return */


typedef enum
{
#define X(x) x,
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
char *token_to_string(Token *token);
