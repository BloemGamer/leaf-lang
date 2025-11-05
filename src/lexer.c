#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <alloca.h>
#include <ctype.h>

#include "assert.h"
#include "lexer.h"

#define MAX2(a, b) ((a) > (b)) ? (a) : (b)

typedef struct
{
	Token token;
	size_t size;
} TokenResult;

typedef struct
{
	size_t size;
	char char_val;
} CharToken;


static void add_token(const Token next, Token **tokens, size_t *token_size, size_t *token_max_size) __attribute((nonnull(2, 3, 4)));
static TokenResult string_to_token(const char *input);
static TokenResult string_to_ignored_token(const char *input);
static TokenResult string_to_simple_token(const char *input);
static TokenResult string_to_keyword_token(const char *input);
static TokenResult string_to_literals_token(const char *input);
static CharToken escape_to_char(const char* escape);

Token *lex(const char *input)
{
	Token *tokens = NULL;
	size_t token_size = 0;
	size_t token_max_size = 0;


	while(*input != '\0')
	{
		const TokenResult next_token = string_to_token(input);
		input += next_token.size;
		if(next_token.token.token_type == token_type_whitespace || next_token.token.token_type == token_type_comment)
		{ continue; }
		if(next_token.token.token_type == token_type_invalid) // tmp
		{ continue; }
		add_token(next_token.token, &tokens, &token_size, &token_max_size);
	}
	Token token = { 0 };
	token.token_type = token_type_eof;
	add_token(token, &tokens, &token_size, &token_max_size);


	return tokens;
}

static void add_token(const Token next, Token **tokens, size_t *token_size, size_t *token_max_size)
{
	if(*token_size >= *token_max_size)
	{
		if(*token_max_size == 0) { *token_max_size += 1; }
		*token_max_size *= 2;
		*tokens = reallocarray(*tokens, *token_max_size, sizeof(Token));
	}
	(*tokens)[*token_size] = next;
	*token_size += 1;
}

static TokenResult string_to_token(const char *input)
{
	TokenResult token_res = { 0 };

	token_res = string_to_ignored_token(input);
	if(token_res.size != 0) { return token_res; }

	token_res = string_to_simple_token(input);
	if(token_res.size != 0) { return token_res; }

	token_res = string_to_keyword_token(input);
	if(token_res.size != 0) { return token_res; }

	token_res = string_to_literals_token(input);
	if(token_res.size != 0) { return token_res; }

	token_res.token.token_type = token_type_invalid;
	token_res.size = 1;
	return token_res;
}

static TokenResult string_to_ignored_token(const char *input)
{
	TokenResult token_res = { 0 };

	if(isspace(input[0]))
	{
		size_t i = 1;
		while(isspace(input[i])) { i++; }

		token_res.token.token_type = token_type_whitespace;
		token_res.size = i;
		return token_res;
	}
	else if(input[0] == '/' && input [1] == '/')
	{
		char *endline = strchrnul(input, '\n');

		token_res.token.token_type = token_type_comment;
		token_res.size = endline - input;
		return token_res;
	}
	else if(input[0] == '/' && input [1] == '*')
	{
		char *endline = strstr(input, "*/");
		if(endline == NULL)
		{
			assert(false && "error: unterminated comment\n  198 | /*");
		} else {
			token_res.size = endline - input;
		}
		token_res.token.token_type = token_type_comment;


		return token_res;
	}
	token_res.size = 0;
	return token_res;
}

static TokenResult string_to_simple_token(const char *input)
{
	TokenResult token_res = { 0 };

	if(isalnum(input[0]) != false)
	{
		token_res.size = 0;
		return token_res;
	}

	constexpr size_t amount_tokens = sizeof(TOKENS_TYPES_SIMPLE) / sizeof(TOKENS_TYPES_SIMPLE[0]);

	for(size_t i = 0; i < amount_tokens; i++)
	{
		if(input[0] == TOKENS_STR_IDENT_SIMPLE[i][0] && input[1] == TOKENS_STR_IDENT_SIMPLE[i][1])
		{
			token_res.token.token_type = TOKENS_TYPES_SIMPLE[i];
			token_res.size = 2;
			return token_res;
		}
	}
	for(size_t i = 0; i < amount_tokens; i++)
	{
		if(input[0] == TOKENS_STR_IDENT_SIMPLE[i][0])
		{
			token_res.token.token_type = TOKENS_TYPES_SIMPLE[i];
			token_res.size = 1;
			return token_res;
		}
	}

	token_res.size = 0;
	return token_res;
}

static TokenResult string_to_keyword_token(const char *input)
{
	TokenResult token_res = { 0 };

	if(isalpha(input[0]) == false)
	{
		token_res.size = 0;
		return token_res;
	}

	constexpr size_t buffer_size = sizeof(TOKENS_STR_IDENT_KEYWORD[0]);
	constexpr size_t amount_tokens = sizeof(TOKENS_TYPES_KEYWORD) / sizeof(TOKENS_TYPES_KEYWORD[0]);
	char buffer[buffer_size] = { 0 };

	buffer[0] = input[0];
	size_t len;
	for(len = 1; len < buffer_size; len++)
	{
		if(isalnum(input[len]) != false)
		{
			buffer[len] = input[len];
		} else {
			break;
		}
	}
	buffer[len] = '\0';

	for(size_t i = 0; i < amount_tokens; i++)
	{
		if(strcmp(buffer, TOKENS_STR_IDENT_KEYWORD[i]) == 0)
		{
			token_res.token.token_type = TOKENS_TYPES_KEYWORD[i];
			token_res.size = len;
			return token_res;
		}
	}
	token_res.size = 0;
	return token_res;
}

static TokenResult string_to_literals_token(const char *input)
{
	TokenResult token_res = { 0 };

	if(input[0] == '"') // strings
	{
		const char *end_str = input;
		while(true)
		{
			end_str = strstr(end_str + 1, "\"");
			if(end_str == NULL)
			{
				token_res.size = 1;
				token_res.token.token_type = token_type_invalid_string;
				return token_res;
			}
			else if(end_str[-1] == '\\')
			{
				continue;
			}
			else
			{
				token_res.size = end_str - input + 1;
				size_t str_size = token_res.size - 2;
				token_res.token.token_type = token_type_string;
				token_res.token.token.str_val = malloc((str_size) * sizeof(char));
				memcpy(token_res.token.token.str_val, input + 1, str_size);
				token_res.token.token.str_val[str_size] = '\0';
				return token_res;
			}
		}

	}
	if(input[0] == '\'')
	{
		if(input[1] == '\'')
		{
			assert(false && "Nothing in char place, or not terminated \'");
		}
		if(input[2] == '\'' && !(input[1] == '\\')) // normal character
		{
			token_res.token.token_type = token_type_char;
			token_res.token.token.char_val = input[1];
			token_res.size = 3;
			return token_res;
		}
		token_res.token.token_type = token_type_char;
		token_res.size = 4;
		CharToken c;
		if((c = escape_to_char(input + 1)).size != 0)
		{
			token_res.token.token_type = token_type_char;
			token_res.token.token.char_val = c.char_val;
			token_res.size = c.size + 2;
			return token_res;
		} else {
			assert(false && "Not implemented");
		}


	}

	token_res.size = 0;
	return token_res;
}

static CharToken escape_to_char(const char* escape)
{
	CharToken ret = { 0 };
	ret.size = 0;
	if(escape[0] != '\\')
		return ret;
	switch(escape[1])
	{
		case 'a':
			ret.char_val = '\a';
			ret.size = 1;
			return ret;
		case 'b':
			ret.char_val = '\b';
			ret.size = 1;
			return ret;
		case 'e':
			ret.char_val = '\e';
			ret.size = 1;
			return ret;
		case 'f':
			ret.char_val = '\f';
			ret.size = 1;
			return ret;
		case 'n':
			ret.char_val = '\n';
			ret.size = 1;
			return ret;
		case 'r':
			ret.char_val = '\r';
			ret.size = 1;
			return ret;
		case 't':
			ret.char_val = '\t';
			ret.size = 1;
			return ret;
		case 'v':
			ret.char_val = '\v';
			ret.size = 1;
			return ret;
		case '\\':
			ret.char_val = '\\';
			ret.size = 1;
			return ret;
		case '\'':
			ret.char_val = '\'';
			ret.size = 1;
			return ret;
		case '"':
			ret.char_val = '\"';
			ret.size = 1;
			return ret;
		case '?':
			ret.char_val = '\?';
			ret.size = 1;
			return ret;
		case 'x':
			{
				char c_ret = 0;
				size_t i;
				for(i = 0; i < 2; i++)
				{
					c_ret <<= 4;
					char c = escape[2 + i];
					if(c >= '0' && c <= '9')
					{
						c_ret |= c - '0';
					}
					else if(c >= 'A' && c <= 'F')
					{
						c_ret |= c - 'A';
					}
					else if(c >= 'a' && c <= 'f')
					{
						c_ret |= c - 'a';
					}
					else
					{
						break;
					}
				}
				if(i == 0)
				{
					assert(false && "No valid character after `\\x`");
				}
				ret.size = i;
				ret.char_val = c_ret;
				return ret;
			}
		case '0'...'7':
			{
				char c_ret = 0;
				size_t i;
				for(i = 0; i < 2; i++)
				{
					c_ret <<= 2;
					char c = escape[2 + i];
					if(c >= '0' && c <= '7')
					{
						c_ret |= c - '0';
					}
					else
					{
						break;
					}
				}
				if(i == 0)
				{
					assert(false && "No valid character after `\\`");
				}
				ret.size = i;
				ret.char_val = c_ret;
				return ret;
			}
		default:
			assert(false && "not implemented or does not exist");
	}
	return ret;
}

const char *token_to_string(Token *token)
{
	return TOKENS_STR_PR[token->token_type];
}
