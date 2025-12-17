#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "assert.h"
#include "basic_types.h"
#include "lexer.h"
#include "log.h"
#include "tokens.h"
#include "utils.h"

typedef struct
{
	const char* tokens;
	usize count;
	usize pos;
	usize amount_errors;
	usize amount_warnings;
} LexerState;

static void add_token(const Token* next, Token** tokens, usize* token_size, usize* token_max_size)
	__attribute((nonnull(2, 3, 4)));
static Pos amount_enters(LexerState lexer_state[static 1], usize size, Pos pos);

static bool string_to_ignored_token(LexerState lexer_state[static 1], Token* out);
static bool string_to_simple_token(LexerState lexer_state[static 1], Token* out);
static bool string_to_keyword_token(LexerState lexer_state[static 1], Token* out);
static bool string_to_literals_token(LexerState lexer_state[static 1], Token* out);
static bool string_to_identifier_token(LexerState lexer_state[static 1], Token* out);

static const char* peek(LexerState* lexer_state);
static const char* peek_count(LexerState* lexer_state, i64 count);
static const char* consume(LexerState* lexer_state);

Token* lex(const char* input)
{
	Token* tokens = nullptr;
	usize token_size = 0;
	usize token_max_size = 0;
	Pos pos = {.line = 1, .character = 1};
	LexerState lexer_state = {
		.tokens = input,
		.count = input ? (usize)strlen(input) : 0,
		.pos = 0,
		.amount_errors = 0,
		.amount_warnings = 0,
	};

	{
		Token token = {0};
		token.token_type = token_type_sof;
		add_token(&token, &tokens, &token_size, &token_max_size);
	}

	while (lexer_state.pos < lexer_state.count)
	{
		const usize start_pos = lexer_state.pos;
		Pos token_start_pos = pos; // Save the position BEFORE consuming

		Token next = {0};
		bool matched = string_to_ignored_token(&lexer_state, &next) || string_to_simple_token(&lexer_state, &next) ||
					   string_to_keyword_token(&lexer_state, &next) || string_to_literals_token(&lexer_state, &next) ||
					   string_to_identifier_token(&lexer_state, &next);

		const usize consumed = lexer_state.pos - start_pos;

		if (consumed > 0)
		{
			pos = amount_enters(&lexer_state, consumed, pos);
		}
		else
		{
			const char* bad_ptr = consume(&lexer_state);
			const char bad = bad_ptr ? *bad_ptr : '\0';
			LOG_ERROR(pos, "Invalid token: '%c'\n", bad); // Use current pos for error
			pos = amount_enters(&lexer_state, 1, pos);
			lexer_state.amount_errors++;
			continue;
		}

		if (!matched)
		{
			continue;
		}

		if (next.token_type == token_type_whitespace || next.token_type == token_type_comment)
		{
			continue;
		}
		if (next.token_type == token_type_invalid)
		{
			LOG_ERROR(token_start_pos, "Invalid token sequence\n"); // Use start position
			lexer_state.amount_errors++;
			continue;
		}
		if (next.token_type == token_type_invalid_comment)
		{
			LOG_ERROR(token_start_pos, "Invalid unclosed /*\n");
			lexer_state.amount_errors++;
			break;
		}
		if (next.token_type == token_type_invalid_string)
		{
			LOG_ERROR(token_start_pos, "Invalid unclosed string\n");
			lexer_state.amount_errors++;
			break;
		}

		next.pos = token_start_pos; // Assign the STARTING position
		add_token(&next, &tokens, &token_size, &token_max_size);
	}

	{
		Token token = {0};
		token.token_type = token_type_eof;
		add_token(&token, &tokens, &token_size, &token_max_size);
	}

	if (lexer_state.amount_warnings != 0)
	{
		(void)errprintf("Found amount warnings: %zu\n", lexer_state.amount_warnings);
	}
	if (lexer_state.amount_errors != 0)
	{
		(void)errprintf("Could not finish lexing because of amount errors: %zu\n", lexer_state.amount_errors);
		exit(EXIT_FAILURE); // NOLINT
	}

	return tokens;
}

static Pos amount_enters(LexerState lexer_state[static 1], usize size, Pos pos)
{
	const usize base = lexer_state->pos - size;
	for (usize i = 0; i < size; i++)
	{
		if ((base + i) >= lexer_state->count)
		{
			break;
		}
		if (lexer_state->tokens[base + i] == '\n')
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
			*token_max_size = 1;
		}
		else
		{
			*token_max_size *= 2;
		}
		*tokens = (Token*)realloc((void*)*tokens, *token_max_size * sizeof(Token));
		assert(*tokens != nullptr);
	}
	(*tokens)[*token_size] = *next;
	*token_size += 1;
}

static bool string_to_ignored_token(LexerState lexer_state[static 1], Token* out)
{
	const char* current = peek(lexer_state);
	if (current && isspace((unsigned char)*current))
	{
		while (current && isspace((unsigned char)*current))
		{
			consume(lexer_state);
			current = peek(lexer_state);
		}
		out->token_type = token_type_whitespace;
		return true;
	}

	if (current && *current == '/' && peek_count(lexer_state, 1) && *peek_count(lexer_state, 1) == '/')
	{
		consume(lexer_state);
		consume(lexer_state);
		while ((current = peek(lexer_state)) && *current != '\n')
		{
			consume(lexer_state);
		}
		out->token_type = token_type_comment;
		return true;
	}

	if (current && *current == '/' && peek_count(lexer_state, 1) && *peek_count(lexer_state, 1) == '*')
	{
		consume(lexer_state);
		consume(lexer_state);
		while (true)
		{
			current = peek(lexer_state);
			if (!current)
			{
				out->token_type = token_type_invalid_comment;
				return true;
			}
			if (*current == '*' && peek_count(lexer_state, 1) && *peek_count(lexer_state, 1) == '/')
			{
				consume(lexer_state);
				consume(lexer_state);
				out->token_type = token_type_comment;
				return true;
			}
			consume(lexer_state);
		}
	}

	return false;
}

static bool string_to_simple_token(LexerState lexer_state[static 1], Token* out)
{
	const char* ch0 = peek(lexer_state);
	if (!ch0)
	{
		return false;
	}
	if (isalnum((unsigned char)*ch0) || *ch0 == '_' || *ch0 == '@' || *ch0 == '"' || *ch0 == '\'')
	{
		return false;
	}

	usize best_len = 0;
	TokenType best_type = token_type_invalid;

	for (usize i = 0; i < ARRAY_SIZE(TOKENS_TYPES_SIMPLE); i++)
	{
		const char* str = TOKENS_STR_IDENT_SIMPLE[i];
		const usize len = (usize)strlen(str);
		if (len == 0)
		{
			continue;
		}

		bool ok = true; // NOLINT
		for (usize j = 0; j < len; j++)
		{
			const char* ch = peek_count(lexer_state, (i64)j); // NOLINT
			if (!ch || *ch != str[j])
			{
				ok = false;
				break;
			}
		}
		if (ok && len > best_len)
		{
			best_len = len;
			best_type = TOKENS_TYPES_SIMPLE[i];
		}
	}

	if (best_len > 0)
	{
		for (usize j = 0; j < best_len; j++)
		{
			consume(lexer_state);
		}
		out->token_type = best_type;
		return true;
	}

	return false;
}

static bool string_to_keyword_token(LexerState lexer_state[static 1], Token* out)
{
	const char* cur0 = peek(lexer_state);
	if (!cur0 || !isalpha((unsigned char)*cur0))
	{
		return false;
	}

	char buffer[64];
	usize len = 0;
	while (true)
	{
		const char* ch = peek_count(lexer_state, (i64)len); // NOLINT
		if (!ch || !(isalnum((unsigned char)*ch) || *ch == '_'))
		{
			break;
		}
		if (len >= sizeof(buffer) - 1)
		{
			break;
		}
		buffer[len++] = *ch;
	}
	buffer[len] = '\0';

	for (usize i = 0; i < ARRAY_SIZE(TOKENS_TYPES_KEYWORD); i++)
	{
		if (strcmp(buffer, TOKENS_STR_IDENT_KEYWORD[i]) == 0)
		{
			for (usize j = 0; j < len; j++)
			{
				consume(lexer_state);
			}
			out->token_type = TOKENS_TYPES_KEYWORD[i];
			return true;
		}
	}

	for (usize i = 0; i < ARRAY_SIZE(TOKENS_TYPES_MODIFIER); i++)
	{
		if (strcmp(buffer, TOKENS_STR_IDENT_MODIFIER[i]) == 0)
		{
			for (usize j = 0; j < len; j++)
			{
				consume(lexer_state);
			}
			out->token_type = TOKENS_TYPES_MODIFIER[i];
			return true;
		}
	}

	return false;
}

static bool string_to_literals_token(LexerState lexer_state[static 1], Token* out)
{
	const char* current = peek(lexer_state);
	if (!current)
	{
		return false;
	}

	if (*current == '"')
	{
		consume(lexer_state);
		const char* start = peek(lexer_state);
		while (true)
		{
			const char* ch = consume(lexer_state); // NOLINT
			if (!ch)
			{
				out->token_type = token_type_invalid_string;
				return true;
			}
			if (*ch == '"')
			{
				const char* back = ch - 1;
				usize esc = 0;
				while (back >= start && *back == '\\')
				{
					esc++;
					back--;
				}
				if ((esc % 2) == 0)
				{
					const usize inner_len = (usize)(ch - start);
					out->str_val = (char*)malloc(inner_len + 1);
					if (out->str_val == nullptr)
					{
						out->token_type = token_type_invalid;
						return true;
					}
					memcpy(out->str_val, start, inner_len);
					out->str_val[inner_len] = '\0';
					out->token_type = token_type_string;
					return true;
				}
			}
		}
	}

	if (*current == '\'')
	{
		consume(lexer_state);
		const char* start = peek(lexer_state);
		while (true)
		{
			const char* ch = consume(lexer_state); // NOLINT
			if (!ch)
			{
				out->token_type = token_type_invalid_string;
				return true;
			}
			if (*ch == '\'')
			{
				const char* back = ch - 1;
				usize esc = 0;
				while (back >= start && *back == '\\')
				{
					esc++;
					back--;
				}
				if ((esc % 2) == 0)
				{
					const usize inner_len = (usize)(ch - start);
					out->str_val = (char*)malloc(inner_len + 1);
					if (out->str_val == nullptr)
					{
						out->token_type = token_type_invalid;
						return true;
					}
					memcpy(out->str_val, start, inner_len);
					out->str_val[inner_len] = '\0';
					out->token_type = token_type_char;
					return true;
				}
			}
		}
	}

	if (isdigit((unsigned char)*current))
	{
		const char* start = current;
		usize offset = 0;

		if (peek_count(lexer_state, 1))
		{
			const char* ch = peek_count(lexer_state, 1); // NOLINT
			if (*start == '0' && (*ch == 'b' || *ch == 'x' || *ch == 'o'))
			{
				offset = 2;
			}
		}

		const char* scan = peek_count(lexer_state, (i64)offset);
		if (!scan)
		{
			scan = start + offset;
		}
		usize inner_len = strcspn(scan, TOKENS_STOP);

		bool is_float = false;
		const char* after_inner = scan + inner_len;
		if ((after_inner < (lexer_state->tokens + lexer_state->count)) && (*after_inner == '.') &&
			(after_inner + 1) < (lexer_state->tokens + lexer_state->count) &&
			isdigit((unsigned char)*(after_inner + 1)))
		{
			is_float = true;
			inner_len += 1 + strcspn(after_inner + 1, TOKENS_STOP);
		}

		const usize total_len = offset + inner_len;
		char* buf = (char*)malloc(total_len + 1);
		assert(buf != nullptr);
		for (usize i = 0; i < total_len; i++)
		{
			const char* ch = consume(lexer_state); // NOLINT
			buf[i] = ch ? *ch : '\0';			   // NOLINT
		}
		buf[total_len] = '\0';

		out->str_val = buf;
		out->token_type = is_float ? token_type_float : token_type_number;
		return true;
	}

	return false;
}

static bool string_to_identifier_token(LexerState lexer_state[static 1], Token* out)
{
	const char* current = peek(lexer_state);
	if (!current)
	{
		return false;
	}

	if (!(isalpha((unsigned char)*current) || *current == '_' || *current == '@'))
	{
		return false;
	}

	usize cap = 16;
	usize len = 0;
	char* buf = (char*)malloc(cap);
	assert(buf != nullptr);

	while ((current = peek(lexer_state)) && (isalnum((unsigned char)*current) || *current == '_' || *current == '@'))
	{
		if (len + 1 >= cap)
		{
			cap *= 2;
			buf = (char*)realloc(buf, cap); // NOLINT
			assert(buf != nullptr);
		}
		buf[len++] = *current;
		consume(lexer_state);
	}
	buf[len] = '\0';

	out->str_val = buf;
	out->token_type = (buf[0] == '@') ? token_type_message : token_type_identifier;
	return true;
}

void lex_free(Token* tokens)
{
	Token* tokens_old = tokens;
	Token tok;
	while ((tok = *tokens++).token_type != token_type_eof)
	{
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

void lex_print(Token* tokens)
{
	Token lex;
	while ((lex = *tokens++).token_type != token_type_eof)
	{
		puts(token_to_string(lex.token_type));
		if (lex.token_type == token_type_string)
		{
			printf("\t\"%s\"\n", lex.str_val);
		}
		if (lex.token_type == token_type_char)
		{
			printf("\t'%s'\n", lex.str_val);
		}
		if (lex.token_type == token_type_number || lex.token_type == token_type_float)
		{
			printf("\t%s\n", lex.str_val);
		}
		if (lex.token_type == token_type_identifier || lex.token_type == token_type_message)
		{
			printf("\t%s\n", lex.str_val);
		}
	}
	puts(token_to_string(lex.token_type));
}

static const char* peek(LexerState* lexer_state)
{
	return lexer_state->pos < lexer_state->count ? &lexer_state->tokens[lexer_state->pos] : nullptr;
}

static const char* peek_count(LexerState* lexer_state, const i64 count)
{
	return ((i64)lexer_state->pos + count) < (i64)lexer_state->count
			   ? &lexer_state->tokens[(i64)lexer_state->pos + count]
			   : nullptr;
}

static const char* consume(LexerState* lexer_state)
{
	return lexer_state->pos < lexer_state->count ? &lexer_state->tokens[lexer_state->pos++] : nullptr;
}
