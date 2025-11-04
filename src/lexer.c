#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"

typedef struct
{
	Token token;
	size_t size;
} TokenResult;

static TokenResult string_to_token(const char *input);
static TokenResult string_to_simple_token(const char *input);
static void add_token(const Token next, Token **tokens, size_t *token_size, size_t *token_max_size) __attribute((nonnull(2, 3, 4)));

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

	token_res.token.token_type = token_type_invalid;
	return token_res;
}


static TokenResult string_to_simple_token(const char *input)
{
	TokenResult token_res = { 0 };

	switch(input[0])
	{
		case '(': { token_res.token.token_type = token_type_lparen; token_res.size = 1; return token_res; }
		case ')': { token_res.token.token_type = token_type_rparen; token_res.size = 1; return token_res; }
		case '{': { token_res.token.token_type = token_type_lbrace; token_res.size = 1; return token_res; }
		case '}': { token_res.token.token_type = token_type_rbrace; token_res.size = 1; return token_res; }
		case '[': { token_res.token.token_type = token_type_lsqbracket; token_res.size = 1; return token_res; }
		case ']': { token_res.token.token_type = token_type_rsqbracket; token_res.size = 1; return token_res; }
		case '.': { token_res.token.token_type = token_type_dot; token_res.size = 1; return token_res; }
		case ',': { token_res.token.token_type = token_type_comma; token_res.size = 1; return token_res; }
		case '+': { token_res.token.token_type = token_type_plus; token_res.size = 1; return token_res; }
		case '-':
		{
			if(input[1] == '>')
			{
				token_res.token.token_type = token_type_arrow;
				token_res.size = 2;
			} else {
				token_res.token.token_type = token_type_minus;
				token_res.size = 1;
			}
			return token_res;
		}
		case '*': { token_res.token.token_type = token_type_star; token_res.size = 1; return token_res; }
		case '/':
		{
			if(input[1] == '/') // line comments
			{
				token_res.token.token_type = token_type_comment;
				size_t i = 2;
				while(input[i++] != '\n') {};
				token_res.size = i;
			}else if(input[1] == '*') /* block comments, note, prob does not what it should do if a end comment is in a string */
			{
				token_res.token.token_type = token_type_comment;
				const char *end_comment = strstr(input, "*/");
				token_res.size = end_comment - input;
			} else {
				token_res.token.token_type = token_type_slash;
				token_res.size = 1;
			}
			return token_res;
		}
		case ';': { token_res.token.token_type = token_type_semicolon; token_res.size = 1; return token_res; }
		case '!':
		{
			if(input[1] == '=')
			{
				token_res.token.token_type = token_type_bang_equal;
				token_res.size = 2;
			} else {
				token_res.token.token_type = token_type_bang;
				token_res.size = 1;
			}
			return token_res;
		}
		case '=':
		{
			if(input[1] == '=')
			{
				token_res.token.token_type = token_type_equal_equal;
				token_res.size = 2;
			} else {
				token_res.token.token_type = token_type_equal;
				token_res.size = 1;
			}
			return token_res;
		}
		case '<':
		{
			if(input[1] == '=')
			{
				token_res.token.token_type = token_type_less_equal;
				token_res.size = 2;
			} else if(input[1] == '<') {
				token_res.token.token_type = token_type_lshift;
				token_res.size = 2;
			} else {
				token_res.token.token_type = token_type_less;
				token_res.size = 1;
			}
			return token_res;
		}
		case '>':
		{
			if(input[1] == '=')
			{
				token_res.token.token_type = token_type_greater_equal;
				token_res.size = 2;
			} else if(input[1] == '>') {
				token_res.token.token_type = token_type_rshift;
				token_res.size = 2;
			} else {
				token_res.token.token_type = token_type_greater;
				token_res.size = 1;
			}
			return token_res;
		}
		case '&':
		{
			if(input[1] == '&')
			{
				token_res.token.token_type = token_type_and;
				token_res.size = 2;
			} else {
				token_res.token.token_type = token_type_ampersand;
				token_res.size = 1;
			}
			return token_res;
		}
		case '^': { token_res.token.token_type = token_type_caret; token_res.size = 1; return token_res; }
		case '|':
		{
			if(input[1] == '|')
			{
				token_res.token.token_type = token_type_or;
				token_res.size = 2;
			} else {
				token_res.token.token_type = token_type_pipe;
				token_res.size = 1;
			}
			return token_res;
		}
		case '~': { token_res.token.token_type = token_type_caret; token_res.size = 1; return token_res; }
		case ' ':
		case '\t':
		case '\n': { token_res.token.token_type = token_type_whitespace; token_res.size = 1; return token_res; }
		default: break;
	}



	token_res.size = 0;
	return token_res;
}


char *token_to_string(Token *token)
{
	switch(token->token_type)
	{
#define X(x, _) case x: return #x;
		__tokens;
#undef X
		default:
			fprintf(stderr, "Could not find token_type: %d", token->token_type);
			exit(EXIT_FAILURE);
	}
}
