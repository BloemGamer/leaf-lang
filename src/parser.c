#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "parser.h"
#include "utils.h"
#include "log.h"
#include "assert.h"
#include "tokens.h"

typedef struct
{
	AST ast;
	usize size;
} ASTSize;

static ASTSize parse_var(const Token *tokens);
static ASTSize parse_func(const Token *tokens);
static ASTSize parse_struct(const Token *tokens);
static ASTSize parse_binary_expr(const Token *tokens);
static ASTSize parse_literal(const Token *tokens);
static ASTSize parse_identefier(const Token *tokens);
static ASTSize parse_block(const Token *tokens);

static bool is_modifier(const TokenType token_type);


AST parse(const Token *tokens)
{
	switch(tokens->token_type)
	{
		case token_type_fn:
			return parse_func(tokens).ast;
			break;
		default:
			LOG_ERROR(tokens->pos, "Unexpected token: %s\n", token_to_string(tokens->token_type));
			assert(false);
	}

}

static ASTSize parse_var(const Token *tokens) { ASTSize ast = { 0 }; return ast;}

static ASTSize parse_func(const Token *tokens)
{
	const Token *const tokens_old = tokens; // I'm not a fan of this syntax, but it does what it should
	debug_assert(tokens->token_type == token_type_fn);

	ASTSize ast = { .size = 0, .ast ={ .tree = NULL, .node.func_def = { 0 }, .token = { .pos = tokens->pos, .token_type = token_type_fn } } };

	{
		usize modifier_count = 0;
		usize modifier_cap = 0;
		Token *buffer = NULL;
		Token t;
		while(is_modifier((t = *(--tokens)).token_type))
		{
			if(modifier_count >= modifier_cap)
			{
				modifier_cap <<= 1;
				modifier_cap = MAX(modifier_cap, 1);
				buffer = (Token*)reallocarray((void*)buffer, modifier_cap, sizeof(*buffer));
			}
			buffer[modifier_count] = t;
		}
		ast.ast.node.func_def.modifiers = buffer;
		ast.ast.node.func_def.modifier_count = modifier_count;
	}

	tokens = tokens_old;
	tokens++;

	debug_assert(tokens->token_type == token_type_identifier);

	ast.ast.node.func_def.name.str_val = strdup(tokens->str_val);
	ast.ast.node.func_def.name.token_type = token_type_function;

	tokens++;

	if(tokens->token_type == token_type_less)
	{
		assert(false && "doing this later");
	}

	debug_assert(tokens->token_type == token_type_lparen);

	{
		usize argument_count = 0;
		usize argument_cap = 0;
		AST *buffer = NULL;
		ASTSize t;
		while((++tokens)->token_type != token_type_rparen)
		{
			debug_assert(tokens->token_type == token_type_identifier);
				t = parse_var(tokens);
				if(argument_count >= argument_cap)
				{
					argument_cap <<= 1;
					argument_cap = MAX(argument_cap, 1);
					buffer = (AST*)reallocarray((void*)buffer, argument_cap, sizeof(*buffer));
				}
				buffer[argument_count] = t.ast;
		}
		ast.ast.node.func_def.params = buffer;
		ast.ast.node.func_def.param_count = argument_count;
	}

	tokens++;

	if(tokens->token_type != token_type_arrow)
	{
		assert(tokens->token_type == token_type_lbrace);
		auto _ = parse(tokens);
		assert(false && "Do this later");
		// return ast;
	}

	tokens++;

	assert(tokens->token_type == token_type_identifier);




	return ast;
}




static ASTSize parse_struct(const Token *tokens);
static ASTSize parse_binary_expr(const Token *tokens);
static ASTSize parse_literal(const Token *tokens);
static ASTSize parse_identefier(const Token *tokens);
static ASTSize parse_block(const Token *tokens);

static bool is_modifier(const TokenType token_type)
{
	for(int i = 0; i < ARRAY_SIZE(TOKENS_TYPES_MODIFIER); i++)
	{
		if(token_type == TOKENS_TYPES_MODIFIER[i])
		{
			return true;
		}
	}
	return false;
}
