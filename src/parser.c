#include <stdlib.h>

#include "assert.h"
#include "log.h"
#include "parser.h"
#include "tokens.h"
#include "utils.h"

typedef struct // NOLINT
{
	const Token* tokens;
	usize count;
	usize pos;
} ParserState;

typedef struct // NOLINT
{
	Token* tokens;
	usize count;
} TokenArray;

static AST* parse_decl(ParserState* parser_state);
static AST* parse_fn(ParserState* parser_state);

static const Token* peek(ParserState* parser_state);
static const Token* consume(ParserState* parser_state);
static bool match(ParserState* parser_state, TokenType type);
static bool is_modifier(const TokenType token_type); // NOLINT
static TokenType token_type_search_until(const ParserState* parser_state, const TokenType* reject, usize count);
static TokenArray get_modifiers(ParserState* parser_state);

static AST* parse_decl(ParserState* parser_state)
{
	const TokenType stop[] = {token_type_fn, token_type_equal, token_type_semicolon};
	switch (token_type_search_until(parser_state, stop, ARRAY_SIZE(stop)))
	{
		case token_type_fn:
			return parse_fn(parser_state);
			// case token_type_equal:
			// 	parse_var(parser_state);
			// case token_type_semiclolon:

		default:
			assert(false && "not implemented (yet)");
	}
}

static AST* parse_fn(ParserState* parser_state)
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_FUNC_DEF;

	node->node.func_def.modifiers = get_modifiers(parser_state);

	assert(consume(parser_state)->token_type != token_type_fn);

	Token token = *consume(parser_state);

	return node;
}

AST parse(const Token* tokens)
{
	switch (tokens->token_type)
	{
		default:
			LOG_ERROR(tokens->pos, "Unexpected token: %s\n", token_to_string(tokens->token_type));
			assert(false);
	}
}

static const Token* peek(ParserState* parser_state)
{
	return parser_state->pos < parser_state->count ? &parser_state->tokens[parser_state->pos] : nullptr;
}

static const Token* consume(ParserState* parser_state)
{
	return parser_state->pos < parser_state->count ? &parser_state->tokens[parser_state->pos++] : nullptr;
}

static bool match(ParserState* parser_state, TokenType type)
{
	const Token* t = peek(parser_state); // NOLINT
	if (t && t->token_type == type)
	{
		consume(parser_state);
		return true;
	}
	return false;
}

static bool is_modifier(const TokenType token_type)
{
#pragma unroll
	for (int i = 0; i < ARRAY_SIZE(TOKENS_TYPES_MODIFIER); i++)
	{
		if (token_type == TOKENS_TYPES_MODIFIER[i])
		{
			return true;
		}
	}
	return false;
}

static bool is_identefier(const TokenType token_type)
{
#pragma unroll
	for (int i = 0; i < ARRAY_SIZE(TOKENS_TYPES_IDENTEFIER); i++)
	{
		if (token_type == TOKENS_TYPES_IDENTEFIER[i])
		{
			return true;
		}
	}
	return false;
}

static TokenType token_type_search_until(const ParserState* parser_state, const TokenType* reject, const usize count)
{
	usize pos = 0;
	while (true)
	{
		if (parser_state->tokens[parser_state->pos + pos].token_type == token_type_eof)
		{
			return token_type_eof;
		}
		for (usize i = 0; i < count; i++) // NOLINT
		{
			if (parser_state->tokens[parser_state->pos + pos].token_type == reject[i])
			{
				return reject[i];
			}
		}
		pos++;
	}
}

static TokenArray get_modifiers(ParserState* parser_state)
{
	Token* modifiers = nullptr;
	usize len = 0;
	usize cap = 0;
#pragma unroll 2
	while (true)
	{
		const Token* token = peek(parser_state);
		if (token == nullptr)
		{
			break;
		}
		if (is_modifier(token->token_type))
		{
			if (len >= cap)
			{
				MAX(cap, 1);
				cap *= 2;
				modifiers = (Token*)realloc((void*)modifiers, cap * sizeof(Token)); // NOLINT
				assert(modifiers != nullptr);
			}
			modifiers[len++] = *consume(parser_state);
		}
		else
		{
			break;
		}
	}
	return (TokenArray){modifiers, len};
}
