#pragma once

#include <stddef.h>
#include <stdint.h>



#define __TOKENS_SIMPLE_SINGLE \
	Y(lparen, '(')				/* ( */ \
	Y(rparen, ')')				/* ) */ \
	Y(lbrace, '{')				/* { */ \
	Y(rbrace, '}')				/* } */ \
	Y(lsqbracket, '[')			/* [ */ \
	Y(rsqbracket, ']')			/* ] */ \
	Y(dot, '.')					/* . */ \
	Y(comma, ',')				/* , */ \
	Y(plus, '+')				/* + */ \
	Y(minus, '-')				/* - */ \
	Y(star, '*')				/* * */ \
	Y(slash, '/')				/* / */ \
	Y(semicolon, ';')			/* ; */ \
	Y(bang, '!')				/* ! */ \
	Y(equal, '=')				/* = */ \
	Y(greater, '>')				/* > */ \
	Y(less, '<')				/* < */ \
	Y(ampersand, '&')			/* & */ \
	Y(caret, '^')				/* ^ */ \
	Y(pipe, '|')				/* | */ \
	Y(tilda, '~')				/* ~ */

#define __TOKENS_SIMPLE_DOUBLE \
	Y(arrow, '-','>')			/* -> */ \
	Y(lshift, '<','<')			/* << */ \
	Y(rshift, '>','>')			/* >> */ \
	Y(and, '&','&')				/* && */ \
	Y(or, '|','|')				/* || */ \
	/* compound tokens: */ \
	Y(bang_equal, '!','=')		/* != */ \
	Y(equal_equal, '=','=')		/* == */ \
	Y(greater_equal, '>','=')	/* >= */ \
	Y(less_equal, '<','=')		/* <= */

#define __TOKENS_SIMPLE \
	__TOKENS_SIMPLE_SINGLE \
	__TOKENS_SIMPLE_DOUBLE


#define __TOKENS_KEYWORD \
	X(if)					/* if */ \
	X(else)					/* else */ \
	X(true)					/* true */ \
	X(false)				/* false */ \
	X(while)				/* while */ \
	X(for)					/* for */ \
	X(fn)					/* fn */ \
	X(struct)				/* struct */ \
	X(enum)					/* enum */ \
	X(return)				/* return */

#define __TOKENS_IGNORED	 \
	X(whitespace)			/* \t, \n, \r or <space> */ \
	X(comment)				/* // (both comments) */ \

#define __TOKENS_LITERALS \
	X(string)				/* string literal "..." */ \
	X(invalid_string)		/* invalid string literal "... EOF */ \
	X(number)				/* any number like: 5 or 5.23 */ \
	X(char)					/* any char like : 'E' */ \

#define __TOKENS_IDENTEFIERS \
	X(identifier)			/* id */ \
	X(type)					/* type */ \

#define __TOKENS_MISC \
	X(eof)					/* always appended as the End Of File (Last token, much like string terminator) */ \
	X(invalid)				/* only used for errors */ \


#define __TOKENS \
	__TOKENS_SIMPLE \
	__TOKENS_KEYWORD \
	__TOKENS_IGNORED \
	__TOKENS_LITERALS \
	__TOKENS_IDENTEFIERS \
	__TOKENS_MISC

#define X(x) token_type_##x,
#define Y(x, ...) X(x)
typedef enum
{
	__TOKENS
} TokenType;
#undef X
#undef Y

typedef struct
{
	size_t line;
	size_t character;
} Pos;

typedef struct
{
	TokenType token_type;
	char *str_val;
	Pos pos;
} Token;

#define X(x) token_type_##x,
#define Y(x, ...) X(x)
constexpr TokenType TOKENS_TYPES_SIMPLE[] = { __TOKENS_SIMPLE };
constexpr TokenType TOKENS_TYPES_KEYWORD[] = { __TOKENS_KEYWORD };
constexpr TokenType TOKENS_TYPES_INGORED[] = { __TOKENS_IGNORED };
constexpr TokenType TOKENS_TYPES_LETERALS[] = { __TOKENS_LITERALS };
constexpr TokenType TOKENS_TYPES_IDENTEFIERS[] = { __TOKENS_IDENTEFIERS };
constexpr TokenType TOKENS_TYPES[] = { __TOKENS };
#undef X
#undef Y

#define __TOKENS_STR_LEN 15
#define X(x) _Static_assert(__TOKENS_STR_LEN >= sizeof(#x), "__TOKENS_STR_LEN too small");
#define Y(x, ...) X(x)
__TOKENS
#undef Y
#undef X

#define X(x) #x,
#define Y(x, ...) X(x)
constexpr char TOKENS_STR_PR_SIMPLE[][__TOKENS_STR_LEN] = { __TOKENS_SIMPLE };
constexpr char TOKENS_STR_PR_KEYWORD[][__TOKENS_STR_LEN] = { __TOKENS_KEYWORD };
constexpr char TOKENS_STR_PR_INGORED[][__TOKENS_STR_LEN] = { __TOKENS_IGNORED };
constexpr char TOKENS_STR_PR_LETERALS[][__TOKENS_STR_LEN] = { __TOKENS_LITERALS };
constexpr char TOKENS_STR_PR_IDENTEFIERS[][__TOKENS_STR_LEN] = { __TOKENS_IDENTEFIERS };
constexpr char TOKENS_STR_PR[][__TOKENS_STR_LEN] = { __TOKENS };
#undef X
#undef Y

#define X(x) #x,
#define Y(_, ...) { __VA_ARGS__, '\0' },
constexpr char TOKENS_STR_IDENT_SIMPLE[][__TOKENS_STR_LEN] = { __TOKENS_SIMPLE };
constexpr char TOKENS_STR_IDENT_KEYWORD[][__TOKENS_STR_LEN] = { __TOKENS_KEYWORD };
constexpr char TOKENS_STR_IDENT_INGORED[][__TOKENS_STR_LEN] = { __TOKENS_IGNORED };
constexpr char TOKENS_STR_IDENT_LETERALS[][__TOKENS_STR_LEN] = { __TOKENS_LITERALS };
constexpr char TOKENS_STR_IDENT_IDENTEFIERS[][__TOKENS_STR_LEN] = { __TOKENS_IDENTEFIERS };
constexpr char TOKENS_STR_IDENT[][__TOKENS_STR_LEN] = { __TOKENS };
#undef X
#undef Y

#undef __TOKENS_STR_LEN


#define Y(_, x, ...) x,
constexpr char TOKENS_STOP[] = { ' ', '\t', '\r', '\n', __TOKENS_SIMPLE '\0'};
#undef Y

#define X(x) token_type_##x,
constexpr TokenType __TOKENS_LEX_FREE[] = { __TOKENS_IDENTEFIERS __TOKENS_LITERALS};
#undef X

#undef __TOKENS_SIMPLE
#undef __TOKENS_KEYWORD
#undef __TOKENS_IGNORED
#undef __TOKENS_LITERALS
#undef __TOKENS_IDENTEFIERS
#undef __TOKENS_MISC

#undef __TOKENS


/// Returns an array of Token, that always ends with token_type_eof, the input can be deallocated if you want
/// The return array should be freed with lex_free
[[gnu::warn_unused_result]]
Token *lex(const char *input);

/// This one does not yet exist
void lex_free(Token *tokens);

/// Returns a pointer to a string, these should not be freed
const char *token_to_string(Token *token);
