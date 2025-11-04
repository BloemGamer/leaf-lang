#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <alloca.h>
#include <ctype.h>

#include "lexer.h"

#define MAX2(a, b) ((a) > (b)) ? (a) : (b)

typedef struct
{
	Token token;
	size_t size;
} TokenResult;

static TokenResult string_to_token(const char *input);
static TokenResult string_to_simple_token(const char *input);
static TokenResult string_to_keyword_token(const char *input);
static void add_token(const Token next, Token **tokens, size_t *token_size, size_t *token_max_size) __attribute((nonnull(2, 3, 4)));
static size_t max(size_t arg1, ...);

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

	token_res = string_to_simple_token(input);
	if(token_res.size != 0) { return token_res; }

	token_res = string_to_keyword_token(input);
	if(token_res.size != 0) { return token_res; }

	token_res.token.token_type = token_type_invalid;
	token_res.size = 1;
	return token_res;
}

static TokenResult string_to_simple_token(const char *input)
{
	TokenResult token_res = { 0 };

	if(isalpha(input[0]) != false)
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

const char *token_to_string(Token *token)
{
	return TOKENS_STR_PR[token->token_type];
}
