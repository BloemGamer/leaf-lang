#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"


static Token string_to_token(const char *input);
static void add_token(const Token next, Token **tokens, size_t *token_size, size_t *token_max_size) __attribute((nonnull(2, 3, 4)));

Token *lex(const char *input)
{
	Token *tokens = NULL;
	size_t token_size = 0;
	size_t token_max_size = 0;


	while(*input != '\0')
	{
		const Token next_token = string_to_token(input);
		input++;
		if(next_token.token_type == token_type_whitespace || next_token.token_type == token_type_comment)
		{ continue; }
		if(next_token.token_type == token_type_invalid) // tmp
		{ continue; }
		add_token(next_token, &tokens, &token_size, &token_max_size);
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

static Token string_to_token(const char *input)
{
	Token token = { 0 };

	// one of 2 chars, that are predetermined
	switch(input[0])
	{
		case '(': { token.token_type = token_type_lparen; return token; }
		case ')': { token.token_type = token_type_rparen; return token; }
		case '{': { token.token_type = token_type_lbrace; return token; }
		case '}': { token.token_type = token_type_rbrace; return token; }
		case '[': { token.token_type = token_type_lsqbracket; return token; }
		case ']': { token.token_type = token_type_rsqbracket; return token; }
		case '.': { token.token_type = token_type_dot; return token; }
		case ',': { token.token_type = token_type_comma; return token; }
		case '+': { token.token_type = token_type_plus; return token; }
		case '-':
		{
			if(input[1] == '>')
			{
				token.token_type = token_type_arrow;
			} else {
				token.token_type = token_type_minus;
			}
			return token;
		}
		case '*': { token.token_type = token_type_star; return token; }
		case '/':
		{
			if(input[1] == '/')
			{
				token.token_type = token_type_comment;
			} else {
				token.token_type = token_type_slash;
			}
			return token;
		}
		case ';': { token.token_type = token_type_semicolon; return token; }
		case '!':
		{
			if(input[1] == '=')
			{
				token.token_type = token_type_bang_equal;
			} else {
				token.token_type = token_type_bang;
			}
			return token;
		}
		case '=':
		{
			if(input[1] == '=')
			{
				token.token_type = token_type_equal_equal;
			} else {
				token.token_type = token_type_equal;
			}
			return token;
		}
		case '<':
		{
			if(input[1] == '=')
			{
				token.token_type = token_type_less_equal;
			} else {
				token.token_type = token_type_less;
			}
			return token;
		}
		case '>':
		{
			if(input[1] == '=')
			{
				token.token_type = token_type_greater_equal;
			} else {
				token.token_type = token_type_greater;
			}
			return token;
		}
		case '&':
		{
			if(input[1] == '&')
			{
				token.token_type = token_type_and;
			} else {
				token.token_type = token_type_ampersand;
			}
			return token;
		}
		case '^': { token.token_type = token_type_caret; return token; }
		case '|':
		{
			if(input[1] == '|')
			{
				token.token_type = token_type_or;
			} else {
				token.token_type = token_type_pipe;
			}
			return token;
		}
		case '~': { token.token_type = token_type_caret; return token; }
		case ' ':
		case '\t':
		case '\n': { token.token_type = token_type_whitespace; return token; }
	}



	token.token_type = token_type_invalid;
	return token;
}


char *token_to_string(Token *token)
{
	switch(token->token_type)
	{
#define X(x) case x: return #x;
		__tokens;
#undef X

		default:
			fprintf(stderr, "Could not find token_type: %d", token->token_type);
			exit(EXIT_FAILURE);
	}
}
