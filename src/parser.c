#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
static AST* parse_var(ParserState* parser_state);
static AST* parse_struct(ParserState* parser_state); // also used for union
static AST* parse_enum(ParserState* parser_state);
static AST* parse_expr(ParserState* parser_state);
static AST* parse_block(ParserState* parser_state);
static AST* parse_statement(ParserState* parser_state);

static AST* parse_precedence(ParserState* parser_state, i32 min_precence);
static AST* parse_prefix(ParserState* parser_state);
static AST* parse_postfix(ParserState* parser_state, AST* left);

static const Token* peek(ParserState* parser_state);
static const Token* consume(ParserState* parser_state);
static bool match(ParserState* parser_state, TokenType type);
static bool is_modifier(TokenType token_type);
static TokenType token_type_search_until(const ParserState* parser_state, const TokenType* reject, usize count);
static TokenArray get_modifiers(ParserState* parser_state);
static i32 precedence(TokenType token_type);

static AST* make_literal(const Token* token);
static AST* make_identifier(const Token* token);
static AST* make_binary(const Token* op, AST* left, AST* right); // NOLINT
static AST* make_call(AST* callee, AST** args, usize arg_count);
static AST* make_member_access(AST* left, AST* right);
static AST* make_index(AST* left, AST* index);

static char* make_string(const Token* token);
static usize unescape(const char* input, char* output, usize output_size, Pos pos); // NOLINT
static char make_char(const Token* token);
static signed char unescape_char(const char* input, Pos pos);
static i64 make_number(const Token* token);
static void print_ast(const AST* ast, int indent);

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
	return parse_block(&parser_state);
}

static AST* parse_decl(ParserState* parser_state) // NOLINT
{
	const TokenType stop[] = {token_type_fn,	 token_type_equal, token_type_enum,
							  token_type_struct, token_type_union, token_type_semicolon};
	switch (token_type_search_until(parser_state, stop, ARRAY_SIZE(stop)))
	{
		case token_type_fn:
			return parse_fn(parser_state);
		case token_type_equal:
			return parse_var(parser_state);
			// case token_type_semiclolon:
		case token_type_struct:
		case token_type_union:
			return parse_struct(parser_state);
		case token_type_enum:
			return parse_enum(parser_state);
		case token_type_eof:
			return nullptr;

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
		node->node.func_def.body = parse_block(parser_state);
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
	node->node.func_def.body = parse_block(parser_state);
	return node;
}

static AST* parse_var(ParserState* parser_state)
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_VAR_DEF;

	TokenArray mod_arr = get_modifiers(parser_state);
	node->node.var_def.modifiers = mod_arr.tokens;
	node->node.var_def.modifier_count = mod_arr.count;

	{
		const Token token = *consume(parser_state);
		assert(token.token_type == token_type_identifier);
		node->node.var_def.type.name = strdup(token.str_val);
	}
	node->node.var_def.type.array_count = 0;
	node->node.var_def.type.pointer_count = 0;
	node->node.var_def.type.pointer_types = nullptr;

	{
		PointerType* pointer_types = nullptr;
		usize cap = 0;
		usize len = 0;

#pragma unroll 2
		while (peek(parser_state)->token_type == token_type_ampersand ||
			   peek(parser_state)->token_type == token_type_star)
		{
			if (cap >= len)
			{
				cap = MAX(cap, 1);
				cap *= 2;
				pointer_types = (PointerType*)realloc((void*)pointer_types, cap * sizeof(PointerType)); // NOLINT
			}
			const Token token = *consume(parser_state);
			pointer_types[len++] = (token.token_type == token_type_ampersand) ? pointer_type_const : pointer_type_mut;
		}
		node->node.var_def.type.pointer_types = pointer_types;
		node->node.var_def.type.pointer_count = len;
	}
	{
		const Token token = *consume(parser_state);
		assert(token.token_type == token_type_identifier);
		node->node.var_def.name = strdup(token.str_val);
	}

	{
		AST** array_sizes = nullptr;
		usize cap = 0;
		usize len = 0;
		while (match(parser_state, token_type_lsqbracket)) // NOLINT
		{
			if (len >= cap)
			{
				cap *= 2;
				array_sizes = (AST**)realloc((void*)array_sizes, cap * sizeof(AST*)); // NOLINT
			}

			if (peek(parser_state)->token_type == token_type_rsqbracket)
			{
				array_sizes[len++] = nullptr;
			}
			else
			{
				array_sizes[len++] = parse_expr(parser_state);
			}
			assert(consume(parser_state)->token_type == token_type_rsqbracket);
		}
	}
	if (peek(parser_state)->token_type == token_type_comma)
	{
		return node;
	}
	if (peek(parser_state)->token_type == token_type_rparen)
	{
		return node;
	}
	if (peek(parser_state)->token_type == token_type_rbrace)
	{
		return node;
	}

	assert(consume(parser_state)->token_type == token_type_equal);

	node->node.var_def.equals = parse_expr(parser_state);

	assert(consume(parser_state)->token_type == token_type_semicolon);

	return node;
}

static AST* parse_struct(ParserState* parser_state) // NOLINT
{
	AST* node = calloc(1, sizeof(AST));
	switch (consume(parser_state)->token_type)
	{
		case token_type_struct:
			node->type = AST_STRUCT_DEF;
			break;
		case token_type_enum:
			node->type = AST_ENUM_DEF;
			break;
		default:
			assert(false);
	}
#define PARSE(_type)                                                                       \
	do                                                                                     \
	{                                                                                      \
		{                                                                                  \
			Token token = *consume(parser_state);                                          \
			assert(token.token_type == token_type_identifier);                             \
		}                                                                                  \
		assert(consume(parser_state)->token_type == token_type_lbrace);                    \
		if (peek(parser_state)->token_type == token_type_rbrace)                           \
		{                                                                                  \
			break;                                                                         \
		}                                                                                  \
		AST** members = nullptr;                                                           \
		usize cap = 0;                                                                     \
		usize len = 0;                                                                     \
		while (true) /* NOLINT */                                                          \
		{                                                                                  \
			if (peek(parser_state)->token_type == token_type_rbrace) /* NOLINT*/           \
			{                                                                              \
				(void)consume(parser_state);                                               \
				node->node._type##_def.members = members;                                  \
				node->node._type##_def.member_count = len;                                 \
				break;                                                                     \
			}                                                                              \
			assert(peek(parser_state)->token_type == token_type_identifier);               \
			AST* tmp = parse_var(parser_state);                                            \
			if (len >= cap)                                                                \
			{                                                                              \
				cap = MAX(cap, 1);                                                         \
				cap *= 2;                                                                  \
				members = (AST**)realloc((void*)members, cap * sizeof(AST*)); /* NOLINT */ \
			}                                                                              \
			members[len++] = tmp;                                                          \
			if (peek(parser_state)->token_type == token_type_comma)                        \
			{                                                                              \
				(void)consume(parser_state);                                               \
				continue;                                                                  \
			}                                                                              \
			else if (peek(parser_state)->token_type == token_type_rbrace) /* NOLINT*/      \
			{                                                                              \
				(void)consume(parser_state);                                               \
				node->node._type##_def.members = members;                                  \
				node->node._type##_def.member_count = len;                                 \
				break;                                                                     \
			}                                                                              \
			else                                                                           \
			{                                                                              \
				assert(false && "not an token that should happen"); /*NOLINT*/             \
			}                                                                              \
		}                                                                                  \
	} while (0)

	switch (node->type)
	{
		case AST_UNION_DEF:
			PARSE(union);
			return node;
		case AST_STRUCT_DEF:
			PARSE(struct);
			return node;
		default:
			assert(false);
	}

#undef PARSE
}

static AST* parse_enum(ParserState* parser_state)
{
	AST* node = calloc(1, sizeof(AST));
	node->type = AST_ENUM_DEF;

	assert(consume(parser_state)->token_type == token_type_enum);

	{
		const Token token = *consume(parser_state);
		assert(token.token_type == token_type_identifier);
		node->node.enum_def.name = strdup(token.str_val);
	}
	if (match(parser_state, token_type_colon))
	{
		{
			const Token token = *consume(parser_state);
			assert(token.token_type == token_type_identifier);
			node->node.enum_def.type = strdup(token.str_val);
		}
	}

	assert(consume(parser_state)->token_type == token_type_lbrace);

	{
		EnumType* members = nullptr;
		usize cap = 0;
		usize len = 0;

		while (true) // NOLINT
		{
			if (peek(parser_state)->token_type == token_type_rbrace) /* NOLINT*/
			{
				node->node.enum_def.members = members;
				node->node.enum_def.member_count = len;
				break;
			}
			if (len >= cap)
			{
				cap *= 2;
				members = (EnumType*)realloc((void*)members, cap * sizeof(EnumType)); /* NOLINT */
				assert(members != nullptr);
			}
			{
				const Token token = *consume(parser_state);
				assert(token.token_type == token_type_identifier);
				members[len].name = strdup(token.str_val);
			}
			if (match(parser_state, token_type_equal))
			{
				{
					const Token token = *consume(parser_state);
					assert(token.token_type == token_type_identifier);
					members[len].value = make_number(&(const Token){
						.str_val = token.str_val, .token_type = token_type_number, .pos = parser_state->pos});
				}
			}
			len++;
			if (peek(parser_state)->token_type == token_type_comma)
			{
				(void)consume(parser_state);
				continue;
			}
			else if (peek(parser_state)->token_type == token_type_rbrace) /* NOLINT*/
			{
				node->node.enum_def.members = members;
				node->node.enum_def.member_count = len;
				break;
			}
		}
	}

	return node;
}

static AST* parse_expr(ParserState* parser_state) // NOLINT
{
	return parse_precedence(parser_state, 1);
}

static AST* parse_block(ParserState* parser_state) // NOLINT
{
	bool global_block = true;
	TokenType end_token = token_type_eof;
	if (match(parser_state,
			  token_type_lbrace)) // add token_type_sof later, but for how the tokens is started, this is for now not
								  // possible
	{
		global_block = false;
		end_token = token_type_rbrace;
	}

	AST* node = calloc(1, sizeof(AST));
	node->type = AST_BLOCK;

	AST** statements = nullptr;
	usize cap = 0;
	usize len = 0;

	while (true) // NOLINT
	{
		if (match(parser_state, end_token))
		{
			node->node.block.statements = statements;
			node->node.block.statement_count = len;
			break;
		}
		if (len >= cap)
		{
			cap = MAX(cap, 1);
			cap *= 2;
			statements = (AST**)realloc((void*)statements, cap * sizeof(AST*)); /* NOLINT */
			assert(statements != nullptr);
		}
		{
			statements[len++] = parse_statement(parser_state);
		}
	}

	return node;
}
static AST* parse_statement(ParserState* parser_state) // NOLINT
{
	switch (peek(parser_state)->token_type)
	{
		case token_type_lbrace:
			return parse_block(parser_state);
		default:
			return parse_decl(parser_state);
	}
}

static AST* parse_precedence(ParserState* parser_state, i32 min_prec) // NOLINT
{
	AST* left = parse_prefix(parser_state);

	left = parse_postfix(parser_state, left);

	while (true) // NOLINT
	{
		const Token* op = peek(parser_state); // NOLINT
		if (!op)
		{
			break;
		}

		int prec = precedence(op->token_type);
		if (prec < min_prec)
		{
			break;
		}

		consume(parser_state);

		AST* right = parse_precedence(parser_state, prec + 1);

		left = make_binary(op, left, right);

		left = parse_postfix(parser_state, left);
	}

	return left;
}

static AST* parse_prefix(ParserState* parser_state) // NOLINT
{
	const Token* t = consume(parser_state); // NOLINT
	if (!t)
	{
		return nullptr;
	}

	switch (t->token_type)
	{
		case token_type_number:
		case token_type_string:
		case token_type_char:
			return make_literal(t);

		case token_type_identifier:
			return make_identifier(t);

		case token_type_lparen:
		{
			AST* expr = parse_expr(parser_state);
			if (!match(parser_state, token_type_rparen))
			{
				assert(false);
				return nullptr;
			}
			return expr;
		}

		default:
			assert(false);
			return nullptr;
	}
}

static AST* parse_postfix(ParserState* parser_state, AST* left) // NOLINT
{
	while (true)
	{
		const Token* t = peek(parser_state); // NOLINT
		if (!t)
		{
			return left;
		}

		switch (t->token_type)
		{
			case token_type_lparen:
			{
				consume(parser_state); // '('
				AST** args = nullptr;
				usize arg_count = 0;

				if (!match(parser_state, token_type_rparen))
				{
					do // NOLINT
					{
						args = (AST**)realloc((void*)args, sizeof(AST*) * (arg_count + 1)); // NOLINT
						args[arg_count++] = parse_expr(parser_state);
					} while (match(parser_state, token_type_comma));

					if (!match(parser_state, token_type_rparen))
					{
						(void)errprintf("Error: expected ')'\n");
						assert(false);
						return left;
					}
				}

				left = make_call(left, args, arg_count);
				break;
			}

			case token_type_dot:
			{
				consume(parser_state); // '.'

				const Token* id = peek(parser_state); // NOLINT
				if (!id || id->token_type != token_type_identifier)
				{
					assert(false);
					return left;
				}
				consume(parser_state);

				AST* ident = make_identifier(id);

				left = make_member_access(left, ident);
				break;
			}

			case token_type_lsqbracket:
			{
				consume(parser_state); // '['
				AST* index_expr = parse_expr(parser_state);
				if (!match(parser_state, token_type_rsqbracket))
				{
					assert(false);
					return left;
				}

				left = make_index(left, index_expr);
				break;
			}

			default:
				return left;
		}
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

static bool is_identifier(const TokenType token_type)
{
#pragma unroll
	for (int i = 0; i < ARRAY_SIZE(TOKENS_TYPES_IDENTIFIER); i++)
	{
		if (token_type == TOKENS_TYPES_IDENTIFIER[i])
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
	while (true) // NOLINT
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

static i32 precedence(TokenType token_type)
{
	switch (token_type)
	{
		case token_type_ampersand:
		case token_type_pipe:
		case token_type_caret:
			return 1;
		case token_type_or:
			return 2;
		case token_type_and:
			return 3;
		case token_type_equal_equal:
		case token_type_bang_equal:
			return 4;
		case token_type_less:
		case token_type_less_equal:
		case token_type_greater:
		case token_type_greater_equal:
			return 5;
		case token_type_plus:
		case token_type_minus:
			return 6;
		case token_type_star:
		case token_type_slash:
			return 7;

		default:
			return 0;
	}
}

AST* make_literal(const Token* token)
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_LITERAL;

	switch (token->token_type)
	{
		case token_type_string:
			node->node.literal.literal.token_type = token_type_string;
			node->node.literal.literal.str_val = make_string(token);
			node->node.literal.literal.pos = token->pos;
			break;
		case token_type_char:
			node->node.literal.literal.token_type = token_type_char;
			node->node.literal.literal.char_val = make_char(token);
			node->node.literal.literal.pos = token->pos;
			break;
		case token_type_number:
			node->node.literal.literal.token_type = token_type_number;
			node->node.literal.literal.num_val = make_number(token);
			node->node.literal.literal.pos = token->pos;
			break;
		default:
			assert(false);
	}

	return node;
}

AST* make_identifier(const Token* token)
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_IDENTIFIER;
	node->node.identifier.identifier = *token;
	node->node.identifier.identifier.str_val = strdup(token->str_val);

	return node;
}

AST* make_binary(const Token* op, AST* left, AST* right) // NOLINT
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_BINARY_EXPR;
	node->node.binary_expr.op = *op;
	node->node.binary_expr.left = left;
	node->node.binary_expr.right = right;

	return node;
}

AST* make_call(AST* callee, AST** args, usize arg_count)
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_FUNC_CALL;
	node->node.func_call.callee = callee;
	node->node.func_call.args = args;
	node->node.func_call.arg_count = arg_count;

	return node;
}

AST* make_member_access(AST* left, AST* right) // NOLINT
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_MEMBER_ACCESS;
	node->node.member_access.left = left;
	node->node.member_access.right = right;

	return node;
}

AST* make_index(AST* left, AST* index) // NOLINT
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_INDEX_EXPR;
	node->node.index_expr.left = left;
	node->node.index_expr.index = index;

	return node;
}

static char* make_string(const Token* token)
{
	usize len = strlen(token->str_val) + 1;
	char* str_ret = calloc(len, sizeof(char));

	(void)unescape(token->str_val, str_ret, len, token->pos);

	return str_ret;
}

static usize unescape(const char* input, char* output, usize output_size, Pos pos) // NOLINT
{
	usize i = 0; // NOLINT
	usize j = 0; // NOLINT

	while (input[i] != '\0' && j < output_size - 1)
	{
		if (input[i] == '\\' && input[i + 1] != '\0')
		{
			i++; // Skip backslash

			switch (input[i])
			{
				case 'a':
					output[j++] = '\a';
					break; // 0x07 Alert
				case 'b':
					output[j++] = '\b';
					break; // 0x08 Backspace
				case 'f':
					output[j++] = '\f';
					break; // 0x0C Form feed
				case 'n':
					output[j++] = '\n';
					break; // 0x0A Newline
				case 'r':
					output[j++] = '\r';
					break; // 0x0D Carriage return
				case 't':
					output[j++] = '\t';
					break; // 0x09 Tab
				case 'v':
					output[j++] = '\v';
					break; // 0x0B Vertical tab
				case '\\':
					output[j++] = '\\';
					break; // 0x5C Backslash
				case '\'':
					output[j++] = '\'';
					break; // 0x27 Single quote
				case '\"':
					output[j++] = '\"';
					break; // 0x22 Double quote
				case '?':
					output[j++] = '\?';
					break; // 0x3F Question mark
				case '0':
					output[j++] = '\0';
					break; // 0x00 Null (simple case)

				// Octal: \nnn (up to 3 digits)
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				{
					int val = input[i] - '0';
					if (input[i + 1] >= '0' && input[i + 1] <= '7')
					{
						val = (val * 8) + (input[++i] - '0');
						if (input[i + 1] >= '0' && input[i + 1] <= '7')
						{
							val = (val * 8) + (input[++i] - '0');
						}
					}
					output[j++] = (char)val;
					break;
				}

				// Hex: \xnn
				case 'x':
				{
					i++; // Skip 'x'
					int val = 0;
					while ((input[i] >= '0' && input[i] <= '9') || (input[i] >= 'a' && input[i] <= 'f') || // NOLINT
						   (input[i] >= 'A' && input[i] <= 'F'))
					{
						if (input[i] >= '0' && input[i] <= '9')
						{
							val = (val * 16) + (input[i] - '0');
						}
						else if (input[i] >= 'a' && input[i] <= 'f')
						{
							val = (val * 16) + (input[i] - 'a' + 10);
						}
						else
						{
							val = (val * 16) + (input[i] - 'A' + 10);
						}
						i++;
					}
					i--; // Back up one since loop will increment
					output[j++] = (char)val;
					break;
				}

				default:
					LOG_WARN(pos, "not a supported escape char: \\%x", input[i]);
					// Unknown escape, keep literal
					output[j++] = input[i];
					break;
			}
			i++;
		}
		else
		{
			output[j++] = input[i++];
		}
	}

	output[j] = '\0';
	return j;
}

static char make_char(const Token* token)
{
	char ch = unescape_char(token->str_val, token->pos); // NOLINT
	return ch;
}

static signed char unescape_char(const char* input, Pos pos)
{
	if (*input != '\\')
	{
		// Not an escape sequence, return the character as-is
		return *input;
	}

	input++; // Skip backslash

	if (*input == '\0')
	{
		// Incomplete escape at end of string
		LOG_WARN(pos, "incomplete escape sequence at end of input");
		debug_assert(false); // this should just not happen
	}

	signed char result = 0;
	switch (*input)
	{
		case 'a':
			result = '\a';
			input++;
			break; // 0x07 Alert
		case 'b':
			result = '\b';
			input++;
			break; // 0x08 Backspace
		case 'f':
			result = '\f';
			input++;
			break; // 0x0C Form feed
		case 'n':
			result = '\n';
			input++;
			break; // 0x0A Newline
		case 'r':
			result = '\r';
			input++;
			break; // 0x0D Carriage return
		case 't':
			result = '\t';
			input++;
			break; // 0x09 Tab
		case 'v':
			result = '\v';
			input++;
			break; // 0x0B Vertical tab
		case '\\':
			result = '\\';
			input++;
			break; // 0x5C Backslash
		case '\'':
			result = '\'';
			input++;
			break; // 0x27 Single quote
		case '\"':
			result = '\"';
			input++;
			break; // 0x22 Double quote
		case '?':
			result = '\?';
			input++;
			break; // 0x3F Question mark
		case '0':
			result = '\0';
			input++;
			break; // 0x00 Null (simple case)

		// Octal: \nnn (up to 3 digits)
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		{
			int val = *input - '0';
			input++;
			if (*input >= '0' && *input <= '7')
			{
				val = (val * 8) + (*input - '0');
				input++;
				if (*input >= '0' && *input <= '7')
				{
					val = (val * 8) + (*input - '0');
					input++;
				}
			}
			result = (signed char)(val);
			break;
		}

		// Hex: \xnn
		case 'x':
		{
			input++; // Skip 'x'
			int val = 0;
			while ((*input >= '0' && *input <= '9') || (*input >= 'a' && *input <= 'f') || // NOLINT
				   (*input >= 'A' && *input <= 'F'))
			{
				if (*input >= '0' && *input <= '9')
				{
					val = (val * 16) + (*input - '0');
				}
				else if (*input >= 'a' && *input <= 'f')
				{
					val = (val * 16) + (*input - 'a' + 10);
				}
				else
				{
					val = (val * 16) + (*input - 'A' + 10);
				}
				input++;
			}
			result = (signed char)(val);
			break;
		}

		default:
			LOG_WARN(pos, "not a supported escape char: \\%x", *input);
			result = *input;
			input++;
			break;
	}

	return result;
}

static i64 make_number(const Token* token)
{
	switch (token->str_val[1])
	{
		case '\0':
			return token->str_val[0] - '0';
		case 'o':
		case 'O':
			return strtoll(token->str_val + 2, nullptr, 8);
		case 'x':
		case 'X':
			return strtoll(token->str_val + 2, nullptr, 16);
		case 'b':
		case 'B':
			return strtoll(token->str_val + 2, nullptr, 2);
		default:
			if (token->str_val[0] == '0')
			{
				LOG_WARN(token->pos,
						 "Numbers starting with '0' are not an octal number here, use '0o' or remove the leading '0'");
			}
			return strtoll(token->str_val, nullptr, 10);
	}
}

void parse_print(const AST* ast)
{
	print_ast(ast, 0);
}

static void print_ast(const AST* ast, int indent) // NOLINT
{
	if (!ast)
	{
		printf("%*sNULL\n", indent * 2, "");
		return;
	}
#define INDENT printf("%*s", indent * 2, "")
	switch (ast->type)
	{
		case AST_VAR_DEF:
			INDENT;
			printf("VarDef: %s\n", ast->node.var_def.name);
			INDENT;
			printf("  Type: %s", ast->node.var_def.type.name);
			for (usize i = 0; i < ast->node.var_def.type.pointer_count; i++) // NOLINT
			{
				if (ast->node.var_def.type.pointer_types[i] == pointer_type_const)
				{
					printf("&");
				}
				else
				{
					printf("*");
				}
			}
			for (usize i = 0; i < ast->node.var_def.type.array_count; i++) // NOLINT
			{
				if (ast->node.var_def.type.array_sizes[i] == nullptr)
				{
					printf("[]");
				}
				else
				{
					print_ast(ast, indent + 1);
				}
			}
			printf("\n");
			if (ast->node.var_def.equals)
			{
				INDENT;
				printf("  Value:\n");
				print_ast(ast->node.var_def.equals, indent + 2);
			}
			break;
		case AST_FUNC_DEF:
			INDENT;
			printf("FuncDef: %s\n", ast->node.func_def.name);
			INDENT;
			printf("  Params (%zu):\n", ast->node.func_def.param_count);
			for (usize i = 0; i < ast->node.func_def.param_count; i++) // NOLINT
			{
				print_ast(ast->node.func_def.params[i], indent + 2);
			}
			INDENT;
			printf("  Body:\n");
			print_ast(ast->node.func_def.body, indent + 2);
			break;
		case AST_BINARY_EXPR:
			INDENT;
			printf("BinaryExpr: %s\n", token_to_string(ast->node.binary_expr.op.token_type));
			INDENT;
			printf("  Left:\n");
			print_ast(ast->node.binary_expr.left, indent + 2);
			INDENT;
			printf("  Right:\n");
			print_ast(ast->node.binary_expr.right, indent + 2);
			break;
		case AST_FUNC_CALL:
			INDENT;
			printf("FuncCall:\n");
			INDENT;
			printf("  Callee:\n");
			print_ast(ast->node.func_call.callee, indent + 2);
			INDENT;
			printf("  Args (%zu):\n", ast->node.func_call.arg_count);
			for (usize i = 0; i < ast->node.func_call.arg_count; i++) // NOLINT
			{
				print_ast(ast->node.func_call.args[i], indent + 2);
			}
			break;
		case AST_LITERAL:
			INDENT;
			printf("Literal: ");
			switch (ast->node.literal.literal.token_type)
			{
				case token_type_string:
					printf("\"%s\"", ast->node.literal.literal.str_val);
					break;
				case token_type_number:
					printf("%ld", ast->node.literal.literal.num_val);
					break;
				case token_type_char:
					printf("'%c'", ast->node.literal.literal.char_val);
					break;
				case token_type_true:
					printf("true");
					break;
				case token_type_false:
					printf("false");
					break;
				default:
					printf("(%s)", token_to_string(ast->node.literal.literal.token_type));
			}
			printf("\n");
			break;
		case AST_IDENTIFIER:
			INDENT;
			printf("Identifier: %s\n", ast->node.identifier.identifier.str_val);
			break;
		case AST_BLOCK:
			INDENT;
			printf("Block (%zu statements):\n", ast->node.block.statement_count);
			for (usize i = 0; i < ast->node.block.statement_count; i++) // NOLINT
			{
				print_ast(ast->node.block.statements[i], indent + 1);
			}
			break;
		case AST_STRUCT_DEF:
			INDENT;
			printf("StructDef: %s\n", ast->node.struct_def.name);
			INDENT;
			printf("  Members (%zu):\n", ast->node.struct_def.member_count);
			for (usize i = 0; i < ast->node.struct_def.member_count; i++) // NOLINT
			{
				print_ast(ast->node.struct_def.members[i], indent + 2);
			}
			break;
		case AST_UNION_DEF:
			INDENT;
			printf("UnionDef: %s\n", ast->node.union_def.name);
			INDENT;
			printf("  Members (%zu):\n", ast->node.union_def.member_count);
			for (usize i = 0; i < ast->node.union_def.member_count; i++) // NOLINT
			{
				print_ast(ast->node.union_def.members[i], indent + 2);
			}
			break;
		case AST_ENUM_DEF:
			INDENT;
			printf("EnumDef: %s (type: %s)\n", ast->node.enum_def.name, ast->node.enum_def.type);
			INDENT;
			printf("  Members (%zu):\n", ast->node.enum_def.member_count);
			for (usize i = 0; i < ast->node.enum_def.member_count; i++) // NOLINT
			{
				INDENT;
				printf("    %s", ast->node.enum_def.members[i].name);
				if (ast->node.enum_def.members[i].has_value)
				{
					printf(" = %ld", ast->node.enum_def.members[i].value);
				}
				printf("\n");
			}
			break;
		case AST_MEMBER_ACCESS:
			INDENT;
			printf("MemberAccess:\n");
			INDENT;
			printf("  Object:\n");
			print_ast(ast->node.member_access.left, indent + 2);
			INDENT;
			printf("  Member:\n");
			print_ast(ast->node.member_access.right, indent + 2);
			break;
		case AST_INDEX_EXPR:
			INDENT;
			printf("IndexExpr:\n");
			INDENT;
			printf("  Array:\n");
			print_ast(ast->node.index_expr.left, indent + 2);
			INDENT;
			printf("  Index:\n");
			print_ast(ast->node.index_expr.index, indent + 2);
			break;
		default:
			INDENT;
			printf("Unknown AST node type: %d\n", ast->type);
	}
#undef INDENT
}
