#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <alloca.h>
#include <ctype.h>

#include "log.h"
#include "assert.h"
#include "lexer.h"

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
static Pos amount_enters(const char *input, size_t size, Pos pos);
static TokenResult string_to_token(const char *input);
static TokenResult string_to_ignored_token(const char *input);
static TokenResult string_to_simple_token(const char *input);
static TokenResult string_to_keyword_token(const char *input);
static TokenResult string_to_literals_token(const char *input);
static TokenResult string_to_identefier_token(const char *input);



Token *lex(const char *input)
{
	Token *tokens = NULL;
	size_t token_size = 0;
	size_t token_max_size = 0;
	Pos pos = { .line = 1, .character = 0 };

	while(*input != '\0')
	{
		TokenResult next_token = string_to_token(input);
		pos = amount_enters(input, next_token.size, pos);
		input += next_token.size;
		if(next_token.token.token_type == token_type_whitespace || next_token.token.token_type == token_type_comment)
		{ continue; }
		if(next_token.token.token_type == token_type_invalid) // tmp
		{
			LOG_ERROR(pos, "An invallid token, found: '%c'\n", *input++);
		}
		next_token.token.pos = pos;
		add_token(next_token.token, &tokens, &token_size, &token_max_size);
	}
	Token token = { 0 };
	token.token_type = token_type_eof;
	add_token(token, &tokens, &token_size, &token_max_size);

	if(amount_errors != 0)
	{
		errprintf("Could not finish lexing because of amount errors: %zu\n", amount_errors);
	}
	if(amount_warnings != 0)
	{
		errprintf("Fount amount warnings: %zu\n", amount_errors);
	}
	if(amount_errors != 0)
	{
		exit(EXIT_FAILURE);
	}

	return tokens;
}

static Pos amount_enters(const char *input, size_t size, Pos pos)
{
	for(size_t i = 0; i < size; i++)
	{
		if(input[i] == '\n')
		{
			pos.line++;
			pos.character = 0;
		} else {
			pos.character++;
		}
	}
	return pos;
}

static void add_token(const Token next, Token **tokens, size_t *token_size, size_t *token_max_size)
{
	if(*token_size >= *token_max_size)
	{
		if(*token_max_size == 0) { *token_max_size += 1; }
		*token_max_size *= 2;
		*tokens = (Token*)reallocarray((void*)*tokens, *token_max_size, sizeof(Token));
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

	token_res = string_to_identefier_token(input);
	if(token_res.size != 0) { return token_res; }

	token_res.token.token_type = token_type_invalid;
	token_res.size = 0;
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
		// the next part of the function only works if this returns true, otherwise it will skipt tokens
		// I wanted this to run at compiletime, but I could not make it work sadly
		debug_assert(strlen(TOKENS_STR_IDENT_SIMPLE[i]) <= 2);
	}

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
				token_res.token.str_val = (char*)malloc((str_size + 1) * sizeof(char));
				memcpy(token_res.token.str_val, input + 1, str_size);
				token_res.token.str_val[str_size] = '\0';
				return token_res;
			}
		}
	}
	if(input[0] == '\'')
	{
		const char *end_str = input;
		while(true)
		{
			end_str = strstr(end_str + 1, "\'");
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
				token_res.token.token_type = token_type_char;
				token_res.token.str_val = (char*)malloc((str_size + 1) * sizeof(char));
				memcpy(token_res.token.str_val, input + 1, str_size);
				token_res.token.str_val[str_size] = '\0';
				return token_res;
			}
		}
	}
	if(isdigit(input[0]))
	{
		const char *start_str = input;
		size_t start_str_offset = 0;
		constexpr char num_starts[][3] = { "0b", "0x" };
		for(size_t i = 0; i < sizeof(num_starts) /sizeof(num_starts[0]); i++)
		{
			if(strncmp(num_starts[i], input, 2))
			{
				start_str += 2;
				start_str_offset += 2;
				break;
			}
		}
		size_t num_len = strcspn(start_str, TOKENS_STOP) + start_str_offset;
		token_res.size = num_len;
		token_res.token.token_type = token_type_number;
		token_res.token.str_val = (char*)malloc((num_len) * sizeof(char) + 1);
		memcpy(token_res.token.str_val, input, num_len);
		token_res.token.str_val[num_len] = '\0';
		return token_res;
	}

	token_res.size = 0;
	return token_res;
}

static TokenResult string_to_identefier_token(const char *input)
{
	TokenResult token_res = { 0 };

	if(isalpha(input[0]) == false)
	{
		token_res.size = 0;
		return token_res;
	}

	size_t buffer_size = 1;
	size_t buffer_max_size = 16;
	char *buffer = (char*)malloc(buffer_max_size * sizeof(*buffer));

	buffer[0] = input[0];
	char c;
	while(isalnum(c = input[buffer_size]))
	{
		if(buffer_size >= buffer_max_size)
		{
			buffer_max_size *= 2;
			buffer = (char*)realloc((void*)buffer, buffer_max_size * sizeof(*buffer));
		}
		buffer[buffer_size] = input[buffer_size];
		buffer_size++;
	}
	buffer[buffer_size] = '\0';



	token_res.token.token_type = token_type_identifier;
	token_res.token.str_val = buffer;
	token_res.size = buffer_size;
	return token_res;
}


const char *token_to_string(Token *tokens)
{
	return TOKENS_STR_PR[tokens->token_type];
}

void lex_free(Token *tokens)
{
	Token *tokens_old = tokens;
	Token tok;
	while((tok = *tokens++).token_type != token_type_eof)
	{
		for(size_t i = 0; i < sizeof(__TOKENS_LEX_FREE) / sizeof(__TOKENS_LEX_FREE[0]); i++)
		{
			if(tok.token_type == __TOKENS_LEX_FREE[i])
			{
				free((void*)tok.str_val);
			}
		}
	}
	free((void*)tokens_old);
}
