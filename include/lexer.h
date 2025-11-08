#pragma once

#include <stddef.h>

#include "tokens.h"




/// Returns an array of Token, that always ends with token_type_eof, the input can be deallocated if you want
/// The return array should be freed with lex_free
[[gnu::warn_unused_result]]
Token *lex(const char *input);

/// This one does not yet exist
void lex_free(Token *tokens);

