#include <stdint.h>
#include <string.h>


typedef struct {}__token_keyword;
typedef struct {}__token_simple;
typedef struct {}__token_literal;
typedef struct {}__token_identefier;
typedef struct {}__token_ignored;
typedef struct {}__token_invalid;
typedef struct {}__token_eof;



#define __KEYWORDS \
	X_KEY(if)		/* if */ \
	X_KEY(else)		/* else */ \
	X_KEY(true)		/* true */ \
	X_KEY(false)	/* false */ \
	X_KEY(while)	/* while */ \
	X_KEY(for)		/* for */ \
	X_KEY(fn)		/* fn */ \
	X_KEY(struct)	/* struct */ \
	X_KEY(enum)		/* enum */ \
	X_KEY(return)	/* return */

#define X_KEY(x) #x,
	constexpr char KEYWORDS[][100] = { __KEYWORDS};
#undef X_KEY

#define SECOND(a, b, ...) b
#define X_HELPER(a, b, c) X(a, b, c)
#define Y(a, b, ...) X_HELPER(a, b, SECOND(a, ##__VA_ARGS__, void))

#define X_KEY(x) Y(token_type_##x, __token_keyword)
#define __tokens \
	Y(token_type_eof, __token_eof)							/* always appended as the End Of File (Last token, much like string terminator) */ \
	Y(token_type_invalid, __token_invalid)					/* only used for errors */ \
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
	__KEYWORDS

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
