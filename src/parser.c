#include <assert.h>
#include <stdlib.h>
#include <string.h>

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
static AST* parse_var(ParserState* parser_state);

static const Token* peek(ParserState* parser_state);
static const Token* consume(ParserState* parser_state);
static bool match(ParserState* parser_state, TokenType type);
static bool is_modifier(TokenType token_type);
static TokenType token_type_search_until(const ParserState* parser_state, const TokenType* reject, usize count);
static TokenArray get_modifiers(ParserState* parser_state);

AST* parse(const Token* tokens)
{
	usize count = 0;
	while (tokens[count++].token_type != token_type_eof) // NOLINT
	{
	}
	ParserState parser_state = {
		.tokens = tokens, .count = count, .pos = 0};		  // pos = 1, to counteract the token_type_sof
	while (peek(&parser_state)->token_type == token_type_sof) // NOLINT
	{
		consume(&parser_state);
	}
	return parse_fn(&parser_state);
	// switch (tokens->token_type)
	// {
	// 	default:
	// 		LOG_ERROR(tokens->pos, "Unexpected token: %s\n", token_to_string(tokens->token_type));
	// 		assert(false);
	// }
}

static AST* parse_decl(ParserState* parser_state)
{
	const TokenType stop[] = {token_type_fn, token_type_equal, token_type_semicolon};
	switch (token_type_search_until(parser_state, stop, ARRAY_SIZE(stop)))
	{
		case token_type_fn:
			return parse_fn(parser_state);
		case token_type_equal:
			return parse_var(parser_state);
			// case token_type_semiclolon:

		default:
			assert(false && "not implemented (yet)");
	}
}

static AST* parse_fn(ParserState* parser_state) // NOLINT
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_FUNC_DEF;

	TokenArray mod_arr = get_modifiers(parser_state);
	node->node.func_def.modifiers = mod_arr.tokens;
	node->node.func_def.modifier_count = mod_arr.count;

	assert(consume(parser_state)->token_type == token_type_fn);

	{
		const Token token = *consume(parser_state);
		node->node.func_def.name = strdup(token.str_val);
	}

	// parse templates, //will do this later

	assert(consume(parser_state)->token_type == token_type_lparen);

	{
		Token token;
		if ((token = *peek(parser_state)).token_type == token_type_identifier || // NOLINT
			is_modifier(token.token_type))										 // NOLINT
		{
			AST** params = (AST**)malloc(1 * sizeof(AST*));
			usize cap = 1;
			usize len = 0;
			while (true) // NOLINT
			{
				AST* tmp = parse_var(parser_state);
				if (len >= cap)
				{
					cap *= 2;
					params = (AST**)realloc((void*)params, cap * sizeof(AST*)); // NOLINT
				}
				params[len++] = tmp;
				if (peek(parser_state)->token_type == token_type_comma)
				{
					(void)consume(parser_state);
					continue;
				}
				else if (peek(parser_state)->token_type == token_type_rparen) // NOLINT
				{
					node->node.func_def.params = params;
					node->node.func_def.param_count = len;
					break;
				}
				else
				{
					assert(false && "not an token that should happen");
				}
			}
		}
	}

	assert(consume(parser_state)->token_type == token_type_rparen);

	if (peek(parser_state)->token_type == token_type_lbrace)
	{
		return node;
	}

	assert(consume(parser_state)->token_type == token_type_arrow);

	Token token;
	if ((token = *peek(parser_state)).token_type == token_type_identifier || // NOLINT
		is_modifier(token.token_type))										 // NOLINT
	{
		parse_var(parser_state);
	}

	assert(peek(parser_state)->token_type == token_type_lbrace);
	return node;
}

static AST* parse_var(ParserState* parser_state)
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_VAR_DEF;

	TokenArray mod_arr = get_modifiers(parser_state);
	node->node.func_def.modifiers = mod_arr.tokens;
	node->node.func_def.modifier_count = mod_arr.count;

	{
		const Token token = *consume(parser_state);
		assert(token.token_type == token_type_identifier);
		node->node.func_def.type = strdup(token.str_val);
	}
	{
		const Token token = *consume(parser_state);
		assert(token.token_type == token_type_identifier);
		node->node.func_def.name = strdup(token.str_val);
	}

	if (peek(parser_state)->token_type == token_type_comma)
	{
		return node;
	}
	if (peek(parser_state)->token_type == token_type_rparen)
	{
		return node;
	}

	assert(consume(parser_state)->token_type == token_type_equal_equal);

	return node;
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
				cap = MAX(cap, 1);
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
