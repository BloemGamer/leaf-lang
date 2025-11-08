#include "tokens.h"

const char *token_to_string(const TokenType token)
{
	return TOKENS_STR_PR[token];
}
