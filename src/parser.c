#include "parser.h"
#include "assert.h"
#include "log.h"
#include "tokens.h"
#include "utils.h"

static bool is_modifier(const TokenType token_type); // NOLINT

AST parse(const Token* tokens)
{
	switch (tokens->token_type)
	{
		default:
			LOG_ERROR(tokens->pos, "Unexpected token: %s\n", token_to_string(tokens->token_type));
			assert(false);
	}
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
