#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "utils.h"
#include "log.h"
#include "assert.h"
#include "lexer.h"

typedef struct
{
	Token token;
	usize size;
} TokenResult;

typedef struct
{
	usize size;
	char char_val;
} CharToken;


static void add_token(const Token next, Token **tokens, usize *token_size, usize *token_max_size) __attribute((nonnull(2, 3, 4)));
static Pos amount_enters(const char *input, usize size, Pos pos);
static TokenResult string_to_token(const char *input);
static TokenResult string_to_ignored_token(const char *input);
static TokenResult string_to_simple_token(const char *input);
static TokenResult string_to_keyword_token(const char *input);
static TokenResult string_to_literals_token(const char *input);
static TokenResult string_to_identefier_token(const char *input);



Token *lex(const char *input)
{
	Token *tokens = NULL;
	usize token_size = 0;
	usize token_max_size = 0;
	Pos pos = { .line = 1, .character = 1 };

	while(*input != '\0')
	{
		TokenResult next_token = string_to_token(input);
		pos = amount_enters(input, next_token.size, pos);
		input += next_token.size > 0 ? next_token.size : 1;
		if(next_token.token.token_type == token_type_whitespace || next_token.token.token_type == token_type_comment)
		{ continue; }
		if(next_token.token.token_type == token_type_invalid)
		{
			LOG_ERROR(pos, "An invallid token, found: '%c'\n", input[-1]);
		}
		if(next_token.token.token_type == token_type_invalid_comment)
		{
			LOG_ERROR(pos, "An invallid unclosed /*\n");
			break;
		}
		if(next_token.token.token_type == token_type_invalid_string)
		{
			LOG_ERROR(pos, "An invallid unclosed /*\n");
			break;
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

static Pos amount_enters(const char *input, usize size, Pos pos)
{
	for(usize i = 0; i < size; i++)
	{
		if(input[i] == '\n')
		{
			pos.line++;
			pos.character = 1;
		} else {
			pos.character++;
		}
	}
	return pos;
}

static void add_token(const Token next, Token **tokens, usize *token_size, usize *token_max_size)
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
		usize i = 1;
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
			token_res.token.token_type = token_type_invalid_comment;
		} else {
			token_res.size = endline - input;
			token_res.token.token_type = token_type_comment;
		}


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

	constexpr usize amount_tokens = ARRAY_SIZE(TOKENS_TYPES_SIMPLE);

	for(usize i = 0; i < amount_tokens; i++)
	{
		// the next part of the function only works if this returns true, otherwise it will skipt tokens
		// I wanted this to run at compiletime, but I could not make it work sadly
		debug_assert(strlen(TOKENS_STR_IDENT_SIMPLE[i]) <= 2);
	}

	for(usize i = 0; i < amount_tokens; i++)
	{
		if(input[0] == TOKENS_STR_IDENT_SIMPLE[i][0] && input[1] == TOKENS_STR_IDENT_SIMPLE[i][1])
		{
			token_res.token.token_type = TOKENS_TYPES_SIMPLE[i];
			token_res.size = 2;
			return token_res;
		}
	}
	for(usize i = 0; i < amount_tokens; i++)
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

	constexpr usize buffer_size = MAX(sizeof(TOKENS_STR_IDENT_KEYWORD[0]), sizeof(TOKENS_STR_IDENT_MODIFIER[0]));
	constexpr usize amount_tokens_keywords = ARRAY_SIZE(TOKENS_TYPES_KEYWORD);
	constexpr usize amount_tokens_modifier = ARRAY_SIZE(TOKENS_TYPES_MODIFIER);
	char buffer[buffer_size] = { 0 };

	buffer[0] = input[0];
	usize len;
	for(len = 1; len < buffer_size; len++)
	{
		if(isalnum(input[len]) != false || input[len] == '_')
		{
			buffer[len] = input[len];
		} else {
			break;
		}
	}
	buffer[len] = '\0';

	for(usize i = 0; i < amount_tokens_keywords; i++)
	{
		if(strcmp(buffer, TOKENS_STR_IDENT_KEYWORD[i]) == 0)
		{
			token_res.token.token_type = TOKENS_TYPES_KEYWORD[i];
			token_res.size = len;
			return token_res;
		}
	}
	for(usize i = 0; i < amount_tokens_modifier; i++)
	{
		if(strcmp(buffer, TOKENS_STR_IDENT_MODIFIER[i]) == 0)
		{
			token_res.token.token_type = TOKENS_TYPES_MODIFIER[i];
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
				usize str_size = token_res.size - 2;
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
				usize str_size = token_res.size - 2;
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
		usize start_str_offset = 0;
		constexpr char num_starts[][3] = { "0b", "0x" };
		for(usize i = 0; i < ARRAY_SIZE(num_starts); i++)
		{
			if(strncmp(num_starts[i], input, 2))
			{
				start_str += 2;
				start_str_offset += 2;
				break;
			}
		}
		usize num_len = strcspn(start_str, TOKENS_STOP) + start_str_offset;
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

	if(isalpha(input[0]) != false || input[0] == '_')
	{
		token_res.token.token_type = token_type_identifier;
	}
	else if(input[0] == '@')
	{
		token_res.token.token_type = token_type_message;
	}
	else
	{
		token_res.size = 0;
		return token_res;
	}

	usize buffer_size = 1;
	usize buffer_max_size = 16;
	char *buffer = (char*)malloc(buffer_max_size * sizeof(*buffer));

	buffer[0] = input[0];
	char c;
	while(isalnum(c = input[buffer_size]) || input[buffer_size] == '_')
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
		for(usize i = 0; i < ARRAY_SIZE(__TOKENS_LEX_FREE); i++)
		{
			if(tok.token_type == __TOKENS_LEX_FREE[i])
			{
				free((void*)tok.str_val);
			}
		}
	}
	free((void*)tokens_old);
}
