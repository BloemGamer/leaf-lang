#pragma once

#include <stddef.h>

#include "tokens.h"

/// Returns an array of Token, that always ends with token_type_eof, the input can be deallocated if you want
/// The return array should be freed with lex_free
[[gnu::warn_unused_result]]
Token* lex(const char* input);

/// Frees the tokens received by lex, do not call free yourself, that will cause memory leaks
void lex_free(Token* tokens);

/// Prints the tokens received by lex
void lex_print(Token* tokens);
