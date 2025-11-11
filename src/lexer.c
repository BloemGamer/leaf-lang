#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "assert.h"
#include "lexer.h"
#include "log.h"
#include "tokens.h"
#include "utils.h"

typedef struct [[gnu::aligned(64)]]
{
	Token token;
	usize size;
} TokenResult;

typedef struct [[gnu::aligned(16)]]
{
	usize size;
	char char_val;
} CharToken;

static void add_token(const Token* next, Token** tokens, usize* token_size, usize* token_max_size)
	__attribute((nonnull(2, 3, 4)));
static Pos amount_enters(const char* input, usize size, Pos pos);
static TokenResult string_to_token(const char* input);
static TokenResult string_to_ignored_token(const char* input);
static TokenResult string_to_simple_token(const char* input);
static TokenResult string_to_keyword_token(const char* input);
static TokenResult string_to_literals_token(const char* input);
static TokenResult string_to_identefier_token(const char* input);

Token* lex(const char* input)
{
	Token* tokens = nullptr;
	usize token_size = 0;
	usize token_max_size = 0;
	Pos pos = {.line = 1, .character = 1};

	{
		Token token = {0};
		token.token_type = token_type_sof;
		add_token(&token, &tokens, &token_size, &token_max_size);
	}
#pragma unroll
	while (*input != '\0')
	{
		TokenResult next_token = string_to_token(input);
		pos = amount_enters(input, next_token.size, pos);
		input += next_token.size > 0 ? next_token.size : 1;
		if (next_token.token.token_type == token_type_whitespace || next_token.token.token_type == token_type_comment)
		{
			continue;
		}
		if (next_token.token.token_type == token_type_invalid)
		{
			LOG_ERROR(pos, "An invallid token, found: '%c'\n", input[-1]);
		}
		if (next_token.token.token_type == token_type_invalid_comment)
		{
			LOG_ERROR(pos, "An invallid unclosed /*\n");
			break;
		}
		if (next_token.token.token_type == token_type_invalid_string)
		{
			LOG_ERROR(pos, "An invallid unclosed /*\n");
			break;
		}
		next_token.token.pos = pos;
		add_token(&next_token.token, &tokens, &token_size, &token_max_size);
	}
	{
		Token token = {0};
		token.token_type = token_type_eof;
		add_token(&token, &tokens, &token_size, &token_max_size);
	}

	if (amount_errors != 0)
	{
		(void)errprintf("Could not finish lexing because of amount errors: %zu\n", amount_errors);
	}
	if (amount_warnings != 0)
	{
		(void)errprintf("Fount amount warnings: %zu\n", amount_errors);
	}
	if (amount_errors != 0)
	{
		exit(EXIT_FAILURE);
	}

	return tokens;
}

static Pos amount_enters(const char* input, usize size, Pos pos)
{
#pragma unroll 2
	for (usize i = 0; i < size; i++)
	{
		if (input[i] == '\n')
		{
			pos.line++;
			pos.character = 1;
		}
		else
		{
			pos.character++;
		}
	}
	return pos;
}

static void add_token(const Token* next, Token** tokens, usize* token_size, usize* token_max_size)
{
	if (*token_size >= *token_max_size)
	{
		if (*token_max_size == 0)
		{
			*token_max_size += 1;
		}
		*token_max_size *= 2;
		*tokens = (Token*)realloc((void*)*tokens, *token_max_size * sizeof(Token));
	}
	(*tokens)[*token_size] = *next;
	*token_size += 1;
}

static TokenResult string_to_token(const char* input)
{
	TokenResult token_res = {0};

	token_res = string_to_ignored_token(input);
	if (token_res.size != 0)
	{
		return token_res;
	}

	token_res = string_to_simple_token(input);
	if (token_res.size != 0)
	{
		return token_res;
	}

	token_res = string_to_keyword_token(input);
	if (token_res.size != 0)
	{
		return token_res;
	}

	token_res = string_to_literals_token(input);
	if (token_res.size != 0)
	{
		return token_res;
	}

	token_res = string_to_identefier_token(input);
	if (token_res.size != 0)
	{
		return token_res;
	}

	token_res.token.token_type = token_type_invalid;
	token_res.size = 0;
	return token_res;
}

static TokenResult string_to_ignored_token(const char* input)
{
	TokenResult token_res = {0};

	if (isspace(input[0]))
	{
		usize i = 1;			  // NOLINT
		while (isspace(input[i])) // NOLINT
		{
			i++;
		}

		token_res.token.token_type = token_type_whitespace;
		token_res.size = i;
		return token_res;
	}
	else if (input[0] == '/' && input[1] == '/') // NOLINT
	{
		char* endline = strchr(input, '\n');
		if (endline == nullptr)
		{
			token_res.size = strlen(input);
		}
		else
		{
			token_res.size = endline - input;
		}

		token_res.token.token_type = token_type_comment;
		return token_res;
	}
	else if (input[0] == '/' && input[1] == '*')
	{
		char* endline = strstr(input, "*/");
		if (endline == NULL)
		{
			token_res.token.token_type = token_type_invalid_comment;
		}
		else
		{
			token_res.size = endline - input;
			token_res.token.token_type = token_type_comment;
		}

		return token_res;
	}
	token_res.size = 0;
	return token_res;
}

static TokenResult string_to_simple_token(const char* input)
{
	TokenResult token_res = {0};

	if (isalnum(input[0]) != false) // NOLINT
	{
		token_res.size = 0;
		return token_res;
	}

	constexpr usize amount_tokens = ARRAY_SIZE(TOKENS_TYPES_SIMPLE);

#pragma unroll
	for (usize i = 0; i < amount_tokens; i++)
	{
		// the next part of the function only works if this returns true, otherwise it will skipt tokens
		// I wanted this to run at compiletime, but I could not make it work sadly
		debug_assert(strlen(TOKENS_STR_IDENT_SIMPLE[i]) <= 2);
	}

#pragma unroll
	for (usize i = 0; i < amount_tokens; i++)
	{
		if (input[0] == TOKENS_STR_IDENT_SIMPLE[i][0] && input[1] == TOKENS_STR_IDENT_SIMPLE[i][1])
		{
			token_res.token.token_type = TOKENS_TYPES_SIMPLE[i];
			token_res.size = 2;
			return token_res;
		}
	}
#pragma unroll
	for (usize i = 0; i < amount_tokens; i++)
	{
		if (input[0] == TOKENS_STR_IDENT_SIMPLE[i][0])
		{
			token_res.token.token_type = TOKENS_TYPES_SIMPLE[i];
			token_res.size = 1;
			return token_res;
		}
	}

	token_res.size = 0;
	return token_res;
}

static TokenResult string_to_keyword_token(const char* input)
{
	TokenResult token_res = {0};

	if (isalpha(input[0]) == false) // NOLINT
	{
		token_res.size = 0;
		return token_res;
	}

	constexpr usize buffer_size = MAX(sizeof(TOKENS_STR_IDENT_KEYWORD[0]), sizeof(TOKENS_STR_IDENT_MODIFIER[0]));
	constexpr usize amount_tokens_keywords = ARRAY_SIZE(TOKENS_TYPES_KEYWORD);
	constexpr usize amount_tokens_modifier = ARRAY_SIZE(TOKENS_TYPES_MODIFIER);
	char buffer[buffer_size] = {0};

	buffer[0] = input[0];
	usize len = 0;

#pragma unroll
	for (len = 1; len < buffer_size; len++)
	{
		if (isalnum(input[len]) != false || input[len] == '_') // NOLINT
		{
			buffer[len] = input[len];
		}
		else
		{
			break;
		}
	}
	buffer[len] = '\0';

#pragma unroll
	for (usize i = 0; i < amount_tokens_keywords; i++)
	{
		if (strcmp(buffer, TOKENS_STR_IDENT_KEYWORD[i]) == 0)
		{
			token_res.token.token_type = TOKENS_TYPES_KEYWORD[i];
			token_res.size = len;
			return token_res;
		}
	}

#pragma unroll
	for (usize i = 0; i < amount_tokens_modifier; i++)
	{
		if (strcmp(buffer, TOKENS_STR_IDENT_MODIFIER[i]) == 0)
		{
			token_res.token.token_type = TOKENS_TYPES_MODIFIER[i];
			token_res.size = len;
			return token_res;
		}
	}
	token_res.size = 0;
	return token_res;
}

static TokenResult string_to_literals_token(const char* input)
{
	TokenResult token_res = {0};

	if (input[0] == '"') // strings
	{
		const char* end_str = input;
		while (true) // NOLINT
		{
			end_str = strchr(end_str + 1, '"');
			if (end_str == nullptr)
			{
				token_res.size = 0;
				token_res.token.token_type = token_type_invalid_string;
				return token_res;
			}
			i64 place = 0;
			i64 count = 0;
			while (end_str[place] == '\\') // NOLINT
			{
				place--;
				count++;
			}
			if ((count % 2) == 0)
			{
				usize total_len = (usize)(end_str - input + 1);
				usize inner_len = total_len - 2;

				token_res.token.str_val = (char*)malloc((inner_len + 1) * sizeof(char));
				if (token_res.token.str_val == nullptr)
				{
					token_res.token.token_type = token_type_invalid;
					token_res.size = 0;
					return token_res;
				}

				token_res.size = total_len;
				token_res.token.token_type = token_type_string;

				memcpy((void*)token_res.token.str_val, (void*)(input + 1), inner_len);
				token_res.token.str_val[inner_len] = '\0';

				return token_res;
			}
		}
	}
	if (input[0] == '\'')
	{
		const char* end_str = input;
		while (true) // NOLINT
		{
			end_str = strchr(end_str + 1, '\'');
			if (end_str == nullptr)
			{
				token_res.size = 0;
				token_res.token.token_type = token_type_invalid_string;
				return token_res;
			}
			i64 place = 0;
			i64 count = 0;
			while (end_str[place] == '\\') // NOLINT
			{
				place--;
				count++;
			}
			if ((count % 2) == 0)
			{
				usize total_len = (usize)(end_str - input + 1);
				usize inner_len = total_len - 2;

				token_res.token.str_val = (char*)malloc((inner_len + 1) * sizeof(char));
				if (token_res.token.str_val == nullptr)
				{
					token_res.token.token_type = token_type_invalid;
					token_res.size = 0;
					return token_res;
				}

				token_res.size = total_len;
				token_res.token.token_type = token_type_char;

				memcpy((void*)token_res.token.str_val, (void*)(input + 1), inner_len);
				token_res.token.str_val[inner_len] = '\0';

				return token_res;
			}
		}
	}
	if (isdigit(input[0]))
	{
		const char* start_str = input;
		usize start_str_offset = 0;
		constexpr char num_starts[][3] = {"0b", "0x", "0o"};
#pragma unroll
		for (usize i = 0; i < ARRAY_SIZE(num_starts); i++)
		{
			if (strncmp(num_starts[i], input, 2) == 0)
			{
				start_str += 2;
				start_str_offset += 2;
				break;
			}
		}
		usize num_len = strcspn(start_str, TOKENS_STOP) + start_str_offset;
		token_res.size = num_len;
		token_res.token.token_type = token_type_number;
		token_res.token.str_val = (char*)malloc(num_len * (sizeof(char) + 1));
		memcpy(token_res.token.str_val, input, num_len);
		token_res.token.str_val[num_len] = '\0';
		return token_res;
	}

	token_res.size = 0;
	return token_res;
}

static TokenResult string_to_identefier_token(const char* input)
{
	TokenResult token_res = {0};

	if (isalpha(input[0]) != false || input[0] == '_') // NOLINT
	{
		token_res.token.token_type = token_type_identifier;
	}
	else if (input[0] == '@')
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
	char* buffer = (char*)malloc(buffer_max_size * sizeof(*buffer));

	buffer[0] = input[0];
	char c;																 // NOLINT
	while (isalnum(c = input[buffer_size]) || input[buffer_size] == '_') // NOLINT
	{
		if (buffer_size >= buffer_max_size)
		{
			buffer_max_size *= 2;
			buffer = (char*)realloc((void*)buffer, buffer_max_size * sizeof(*buffer)); // NOLINT
		}
		buffer[buffer_size] = input[buffer_size];
		buffer_size++;
	}
	buffer[buffer_size] = '\0';

	token_res.token.str_val = buffer;
	token_res.size = buffer_size;
	return token_res;
}

void lex_free(Token* tokens)
{
	Token* tokens_old = tokens;
	Token tok;
	while ((tok = *tokens++).token_type != token_type_eof)
	{
#pragma unroll
		for (usize i = 0; i < ARRAY_SIZE(TOKENS_LEX_FREE); i++)
		{
			if (tok.token_type == TOKENS_LEX_FREE[i])
			{
				free((void*)tok.str_val);
			}
		}
	}
	free((void*)tokens_old);
}
