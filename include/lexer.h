#include <stdint.h>

#define SECOND(a, b, ...) a, b)
#define Y(a, ...) X(a, SECOND(__VA_ARGS__, void))

#define __tokens \
	Y(token_type_eof)					/* always appended as the End Of File (Last token, much like string terminator) */ \
	Y(token_type_invalid)				/* only used for errors */ \
\
	/* simple tokens: */ \
	Y(token_type_lparen)				/* ( */ \
	Y(token_type_rparen)				/* ) */ \
	Y(token_type_lbrace)				/* { */ \
	Y(token_type_rbrace)				/* } */ \
	Y(token_type_lsqbracket)			/* [ */ \
	Y(token_type_rsqbracket)			/* ] */ \
	Y(token_type_dot)					/* . */ \
	Y(token_type_comma)					/* , */ \
	Y(token_type_plus)					/* + */ \
	Y(token_type_minus)					/* - */ \
	Y(token_type_star)					/* * */ \
	Y(token_type_slash)					/* / */ \
	Y(token_type_semicolon)				/* ; */ \
	Y(token_type_bang)					/* ! */ \
	Y(token_type_equal)					/* = */ \
	Y(token_type_greater)				/* > */ \
	Y(token_type_less)					/* < */ \
	Y(token_type_ampersand)				/* & */ \
	Y(token_type_caret)					/* ^ */ \
	Y(token_type_pipe)					/* | */ \
	Y(token_type_tilda)					/* ~ */ \
	Y(token_type_arrow)					/* -> */ \
	Y(token_type_lshift)				/* << */ \
	Y(token_type_rshift)				/* >> */ \
	Y(token_type_and)					/* && */ \
	Y(token_type_or)					/* || */ \
	/*ompound tokens: */ \
	Y(token_type_bang_equal)			/* != */ \
	Y(token_type_equal_equal)			/* == */ \
	Y(token_type_greater_equal)			/* >= */ \
	Y(token_type_less_equal)			/* <= */ \
	/*ignored:*/ \
	Y(token_type_whitespace)			/* \t, \n, \r or <space> */ \
	Y(token_type_comment)				/* // (both comments) */ \
\
	/*literals: */ \
	Y(token_type_string, char*)				/* string literal "..." */ \
	Y(token_type_invalid_string, char*)		/* invalid string literal "... EOF */ \
	Y(token_type_number, int64_t)			/* any number like: 5 or 5.23 */ \
	Y(token_type_char, char)				/* any char like : 'E' */ \
\
	/*identifier: */ \
	Y(token_type_identifier, char*)		/* id */ \
	Y(token_type_type, char*)			/* type */ \
\
	/*keywords: */ \
	Y(token_type_if)					/* if */ \
	Y(token_type_else)					/* else */ \
	Y(token_type_true)					/* true */ \
	Y(token_type_false)					/* false */ \
	Y(token_type_while)					/* while */ \
	Y(token_type_for)					/* for */ \
	Y(token_type_fun)					/* fn */ \
	Y(token_type_struct)				/* struct */ \
	Y(token_type_enum)					/* enum */ \
	Y(token_type_return)				/* return */

typedef enum
{
#define X(x, _) x,
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


#define test(x) _Generic((x)0, \
	void: "void", \
	default: "defualt" ) \

char *t = test(void);

/// Returns an array of Token, that always ends with token_type_eof, the input can be deallocated if you want
[[gnu::warn_unused_result]]
Token *lex(const char *input);

/// Returns a pointer to a string, these should not be freed
char *token_to_string(Token *token);
