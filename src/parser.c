#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "basic_types.h"
#include "log.h"
#include "parser.h"
#include "tokens.h"
#include "utils.h"
#include "utils/hash.h"

// macro's for having variable array length
#define varray_make(type, name) \
	type name = nullptr;        \
	usize name##_len = 0;       \
	usize name##_cap = 0

#define varray_push(name, push)                                                    \
	{                                                                              \
		if (name##_len >= name##_cap)                                              \
		{                                                                          \
			name##_cap = MAX(name##_cap, 1);                                       \
			name##_cap *= 2;                                                       \
			name = (typeof(name))realloc((void*)name, name##_cap * sizeof(*name)); \
		}                                                                          \
		name[name##_len++] = push;                                                 \
	}

typedef struct // NOLINT
{
	const Token* tokens;
	usize count;
	usize pos;
	HashStr known_types;
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
static AST* parse_message(ParserState* parser_state);
static AST* parse_if_expr(ParserState* parser_state);
static AST* parse_while_expr(ParserState* parser_state);
static AST* parse_for_expr(ParserState* parser_state);
static AST* parse_return_expr(ParserState* parser_state);
static AST* parse_break_expr(ParserState* parser_state);
static AST* parse_continue_expr(ParserState* parser_state);
static AST* parse_array_init(ParserState* parser_state);
static AST* parse_compound_literal(ParserState* parser_state);
static AST* parse_cast_or_compound(ParserState* parser_state);

static AST* parse_precedence(ParserState* parser_state, i32 min_precence);
static AST* parse_prefix(ParserState* parser_state);
static AST* parse_postfix(ParserState* parser_state, AST* left);

static const Token* peek(ParserState* parser_state);
static const Token* peek_amount(ParserState* parser_state, i64 amount);
static const Token* consume(ParserState* parser_state);
static bool match(ParserState* parser_state, TokenType type);
static void step_back(ParserState* parser_state);
static bool is_modifier(TokenType token_type);
static TokenType token_type_search_until(const ParserState* parser_state, const TokenType* reject, usize count);
static TokenArray get_modifiers(ParserState* parser_state);
static i32 precedence(TokenType token_type);
static VarDef parse_var_def(ParserState* parser_state);

static AST* make_literal(const Token* token);
static AST* make_identifier(const Token* token);
static AST* make_binary(const Token* op, AST* left, AST* right); // NOLINT
static AST* make_call(AST* callee, AST** args, usize arg_count);
static AST* make_member_access(AST* left, AST* right, bool direct);
static AST* make_index(AST* left, AST* index);

static char* make_string(const Token* token);
static usize unescape(const char* input, char* output, usize output_size, Pos pos);
static char make_char(const Token* token);
static signed char unescape_char(const char* input, Pos pos);
static i64 make_number(const Token* token);
static f64 make_float(const Token* token);
static void add_basic_types(ParserState* parser_state);

static void free_var_type(VarType* type);
static void free_enum_type(EnumType* enum_type);
static void free_var_def(VarDef* def);
static void free_func_def(FuncDef* func);

static void parse_print_impl(const AST* ast, usize depth);
static void print_indent(usize depth);
static void print_pointer_types(const PointerType* pointer_types, usize count);
static void print_var_type(const VarType* var_types, usize depth);
static void print_modifiers(const Token* modifiers, usize count, usize depth);
static void print_var_def(const VarDef* var_def, usize depth);
static void print_func_def(const FuncDef* func_def, usize depth);
static void print_struct_def(const StructDef* struct_def, usize depth);
static void print_union_def(const UnionDef* union_def, usize depth);
static void print_enum_def(const EnumDef* enum_def, usize depth);
static void print_func_call(const FuncCall* func_call, usize depth);
static void print_member_access(const MemberAccess* member_access, usize depth);
static void print_binary_expr(const BinaryExpr* binary_expr, usize depth);
static void print_unary_expr(const UnaryExpr* unary_expr, usize depth);
static void print_index_expr(const IndexExpr* index_expr, usize depth);
static void print_literal(const Literal* lit, usize depth);
static void print_identifier(const Identifier* identifier, usize depth);
static void print_block(const Block* block, usize depth);
static void print_message(const Message* message, usize depth);
static void print_if_expr(const IfExpr* if_expr, usize depth);
static void print_while_expr(const WhileExpr* while_expr, usize depth);
static void print_for_expr(const ForExpr* for_expr, usize depth);
static void print_return_expr(const ReturnStmt* return_stmt, usize depth);
static void print_break_expr(usize depth);
static void print_continue_expr(usize depth);
static void print_range_expr(const RangeExpr* range_expr, usize depth);
static void print_array_init(const ArrayInit* array_init, usize depth);
static void print_struct_init(const StructInit* struct_init, usize depth);
static void print_cast_expr(const CastExpr* cast_expr, usize depth);

AST* parse(const Token* tokens)
{
	usize count = 0;
	while (tokens[count++].token_type != token_type_eof) // NOLINT
	{
	}
	ParserState parser_state = {.tokens = tokens, .count = count, .pos = 0, .known_types = hash_str_new(10)};
	add_basic_types(&parser_state);
	while (peek(&parser_state)->token_type == token_type_sof) // NOLINT
	{
		consume(&parser_state);
	}
	AST* ret = parse_block(&parser_state);
	hash_str_free(&parser_state.known_types);
	return ret;
}

static AST* parse_decl(ParserState* parser_state) // NOLINT
{
	const TokenType stop[] = {token_type_fn,	token_type_equal,	  token_type_enum,	 token_type_struct,
							  token_type_union, token_type_semicolon, token_type_message};
	switch (token_type_search_until(parser_state, stop, ARRAY_SIZE(stop)))
	{
		case token_type_fn:
			return parse_fn(parser_state);
		case token_type_equal:
			return parse_var(parser_state);
		case token_type_struct:
		case token_type_union:
			return parse_struct(parser_state);
		case token_type_enum:
			return parse_enum(parser_state);
		case token_type_message:
			return parse_message(parser_state);
		case token_type_eof:
			assert(false && "found eof");

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
			varray_make(AST**, params);
			while (true) // NOLINT
			{
				AST* tmp = parse_var(parser_state);
				varray_push(params, tmp); // NOLINT
				if (peek(parser_state)->token_type == token_type_comma)
				{
					(void)consume(parser_state);
					continue;
				}
				else if (peek(parser_state)->token_type == token_type_rparen) // NOLINT
				{
					node->node.func_def.params = params;
					node->node.func_def.param_count = params_len;
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
		VarDef var_def = parse_var_def(parser_state);
		node->node.func_def.return_type = var_def;
	}

	assert(peek(parser_state)->token_type == token_type_lbrace);
	node->node.func_def.body = parse_block(parser_state);
	return node;
}

static AST* parse_var(ParserState* parser_state) // NOLINT
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_VAR_DEF;

	node->node.var_def = parse_var_def(parser_state);

	{
		const Token token = *consume(parser_state);
		assert(token.token_type == token_type_identifier);
		node->node.var_def.name = strdup(token.str_val);
	}

	{
		varray_make(AST**, array_sizes);
		while (match(parser_state, token_type_lsqbracket)) // NOLINT
		{

			if (peek(parser_state)->token_type == token_type_rsqbracket)
			{
				varray_push(array_sizes, nullptr); // NOLINT
			}
			else
			{
				varray_push(array_sizes, parse_expr(parser_state)); // NOLINT
			}
			assert(consume(parser_state)->token_type == token_type_rsqbracket);
		}
		node->node.var_def.type.array_sizes = array_sizes;
		node->node.var_def.type.array_count = array_sizes_len;
	}
	if (peek(parser_state)->token_type == token_type_comma || peek(parser_state)->token_type == token_type_rparen ||
		peek(parser_state)->token_type == token_type_rbrace)
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
		case token_type_union:
			node->type = AST_UNION_DEF;
			break;
		default:
			assert(false);
	}
#define PARSE(_type)                                                                  \
	do                                                                                \
	{                                                                                 \
		TokenArray mod_arr = get_modifiers(parser_state);                             \
		node->node._type##_def.modifiers = mod_arr.tokens;                            \
		node->node._type##_def.modifier_count = mod_arr.count;                        \
		{                                                                             \
			Token token = *consume(parser_state);                                     \
			assert(token.token_type == token_type_identifier);                        \
			node->node._type##_def.name = strdup(token.str_val);                      \
			hash_str_push(&parser_state->known_types, token.str_val);                 \
		}                                                                             \
		assert(consume(parser_state)->token_type == token_type_lbrace);               \
		if (peek(parser_state)->token_type == token_type_rbrace)                      \
		{                                                                             \
			break;                                                                    \
		}                                                                             \
		varray_make(AST**, members);                                                  \
		while (true) /* NOLINT */                                                     \
		{                                                                             \
			if (peek(parser_state)->token_type == token_type_rbrace) /* NOLINT*/      \
			{                                                                         \
				(void)consume(parser_state);                                          \
				node->node._type##_def.members = members;                             \
				node->node._type##_def.member_count = members_len;                    \
				break;                                                                \
			}                                                                         \
			assert(peek(parser_state)->token_type == token_type_identifier);          \
			AST* tmp = parse_var(parser_state);                                       \
			varray_push(members, tmp); /* NOLINT */                                   \
			if (peek(parser_state)->token_type == token_type_comma)                   \
			{                                                                         \
				(void)consume(parser_state);                                          \
				continue;                                                             \
			}                                                                         \
			else if (peek(parser_state)->token_type == token_type_rbrace) /* NOLINT*/ \
			{                                                                         \
				(void)consume(parser_state);                                          \
				node->node._type##_def.members = members;                             \
				node->node._type##_def.member_count = members_len;                    \
				break;                                                                \
			}                                                                         \
			else                                                                      \
			{                                                                         \
				assert(false && "not an token that should happen"); /*NOLINT*/        \
			}                                                                         \
		}                                                                             \
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

	TokenArray mod_arr = get_modifiers(parser_state);
	node->node.enum_def.modifiers = mod_arr.tokens;
	node->node.enum_def.modifier_count = mod_arr.count;

	assert(consume(parser_state)->token_type == token_type_enum);

	{
		const Token token = *consume(parser_state);
		assert(token.token_type == token_type_identifier);
		node->node.enum_def.name = strdup(token.str_val);
		hash_str_push(&parser_state->known_types, token.str_val);
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
		varray_make(EnumType*, members);

		while (true) // NOLINT
		{
			if (peek(parser_state)->token_type == token_type_rbrace) /* NOLINT*/
			{
				node->node.enum_def.members = members;
				node->node.enum_def.member_count = members_len;
				break;
			}
			EnumType tmp = {nullptr};
			{
				const Token token = *consume(parser_state);
				assert(token.token_type == token_type_identifier);
				tmp.name = strdup(token.str_val);
			}
			if (match(parser_state, token_type_equal))
			{
				{
					const Token token = *consume(parser_state);
					assert(token.token_type == token_type_identifier);
					tmp.value = make_number(
						&(const Token){.str_val = token.str_val, .token_type = token_type_number, .pos = token.pos});
				}
			}
			varray_push(members, tmp); // NOLINT
			if (peek(parser_state)->token_type == token_type_comma)
			{
				(void)consume(parser_state);
				continue;
			}
			else if (peek(parser_state)->token_type == token_type_rbrace) /* NOLINT*/
			{
				node->node.enum_def.members = members;
				node->node.enum_def.member_count = members_len;
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
	node->node.block.global = global_block;

	varray_make(AST**, statements);

	while (true) // NOLINT
	{
		while (match(parser_state, token_type_semicolon)) // NOLINT
		{
		}
		if (match(parser_state, end_token))
		{
			node->node.block.statements = statements;
			node->node.block.statement_count = statements_len;
			node->node.block.trailing_expr = nullptr;
			break;
		}
		{
			usize saved_pos = parser_state->pos;
			AST* tmp = parse_statement(parser_state);

			// Check if next token is closing brace
			if (!global_block && peek(parser_state)->token_type == end_token)
			{
				// Look back to see if we just consumed a semicolon
				// If there was a semicolon, this is NOT a trailing expression
				if (parser_state->pos > 0 &&
					parser_state->tokens[parser_state->pos - 1].token_type != token_type_semicolon)
				{
					(void)consume(parser_state);
					node->node.block.statements = statements;
					node->node.block.statement_count = statements_len;
					node->node.block.trailing_expr = tmp;
					break;
				}
			}
			varray_push(statements, tmp); // NOLINT
		}
	}

	return node;
}
static AST* parse_statement(ParserState* parser_state) // NOLINT
{
	Token token = *peek(parser_state);
	switch (token.token_type)
	{
		case token_type_lbrace:
			return parse_block(parser_state);
		case token_type_identifier:
			if (hash_str_contains(&parser_state->known_types, token.str_val))
			{
				return parse_var(parser_state);
			}
			else
			{
				return parse_expr(parser_state);
			}
		case token_type_fn:
			return parse_fn(parser_state);
		case token_type_struct:
		case token_type_union:
			return parse_struct(parser_state);
		case token_type_enum:
			return parse_enum(parser_state);
		case token_type_message:
			return parse_message(parser_state);
		case token_type_semicolon:
			(void)consume(parser_state);
			return parse_statement(parser_state);
		case token_type_if:
			return parse_if_expr(parser_state);
		case token_type_while:
			return parse_while_expr(parser_state);
		case token_type_return:
			return parse_return_expr(parser_state);
		case token_type_continue:
			return parse_continue_expr(parser_state);
		case token_type_break:
			return parse_break_expr(parser_state);
		case token_type_for:
			return parse_for_expr(parser_state);
		case token_type_number:
		case token_type_string:
		case token_type_char:
		case token_type_float:
		case token_type_true:
		case token_type_false:
			return parse_expr(parser_state);
		default:
			break;
	}
	if (is_modifier(token.token_type))
	{
		return parse_decl(parser_state);
	}
	assert(false && "not an expected type");
}

static AST* parse_message(ParserState* parser_state)
{
	const Token token_g = *consume(parser_state);
	if (strcmp(token_g.str_val, "@embed") == 0)
	{
		AST* node = calloc(1, sizeof(AST));
		node->type = AST_MESSAGE;
		node->node.message.msg = msg_embed;
		const Token token = *consume(parser_state);
		if (token.token_type == token_type_less)
		{
			{
				const Token token_l = *consume(parser_state);
				assert(token_l.token_type == token_type_identifier);

				node->node.message.import.import = strdup(token_l.str_val);
				node->node.message.import.type = import_type_user;
			}
			const Token token_l = *consume(parser_state);
			assert(token_l.token_type == token_type_greater);
		}
		if (token.token_type == token_type_string)
		{
			node->node.message.import.import = strdup(token.str_val);
		}

		return node;
	}
	if (strcmp(token_g.str_val, "@c_type") == 0)
	{
		AST* node = calloc(1, sizeof(AST));
		node->type = AST_MESSAGE;
		node->node.message.msg = msg_c_type;
		const Token token = *consume(parser_state);

		assert(token.token_type == token_type_identifier);
		node->node.message.c_type.type = strdup(token.str_val);

		hash_str_push(&parser_state->known_types, token.str_val);

		return node;
	}
	assert(false && "not (yet) a compiler message");
}

static AST* parse_if_expr(ParserState* parser_state)
{
	AST* node = calloc(1, sizeof(AST));
	node->type = AST_IF_EXPR;

	assert(consume(parser_state)->token_type == token_type_if);

	node->node.if_expr.condition = parse_expr(parser_state);

	assert(peek(parser_state)->token_type == token_type_lbrace);
	node->node.if_expr.then_block = parse_block(parser_state);

	if (match(parser_state, token_type_else))
	{
		if (peek(parser_state)->token_type == token_type_if)
		{
			node->node.if_expr.else_block = parse_if_expr(parser_state);
		}
		else if (peek(parser_state)->token_type == token_type_lbrace)
		{
			node->node.if_expr.else_block = parse_block(parser_state);
		}
		else
		{
			assert(false && "else must be followed by block or if");
		}
	}
	else
	{
		node->node.if_expr.else_block = nullptr;
	}

	return node;
}

static AST* parse_while_expr(ParserState* parser_state) // NOLINT
{
	AST* node = calloc(1, sizeof(AST));
	node->type = AST_WHILE_EXPR;

	assert(consume(parser_state)->token_type == token_type_while);

	node->node.while_expr.condition = parse_expr(parser_state);

	assert(peek(parser_state)->token_type == token_type_lbrace);
	node->node.while_expr.then_block = parse_block(parser_state);

	return node;
}

static AST* parse_for_expr(ParserState* parser_state)
{
	AST* node = calloc(1, sizeof(AST));
	node->type = AST_FOR_EXPR;

	assert(consume(parser_state)->token_type == token_type_for);

	if (peek(parser_state)->token_type == token_type_lparen)
	{
		node->node.for_expr.style = FOR_STYLE_C;

		consume(parser_state); // '('

		if (peek(parser_state)->token_type == token_type_semicolon)
		{
			node->node.for_expr.c_style.init = nullptr;
		}
		else
		{
			Token token = *peek(parser_state);

			if (hash_str_contains(&parser_state->known_types, token.str_val) || is_modifier(token.token_type)) // NOLINT
			{
				node->node.for_expr.c_style.init = parse_var(parser_state);
				// Always takes the semicolon with it
			}
			else
			{
				node->node.for_expr.c_style.init = parse_expr(parser_state);
				assert(peek(parser_state)->token_type == token_type_semicolon);
			}
		}

		if (peek(parser_state)->token_type == token_type_semicolon)
		{
			node->node.for_expr.c_style.condition = nullptr;
		}
		else
		{
			node->node.for_expr.c_style.condition = parse_expr(parser_state);
		}

		assert(consume(parser_state)->token_type == token_type_semicolon);

		if (peek(parser_state)->token_type == token_type_rparen)
		{
			node->node.for_expr.c_style.increment = nullptr;
		}
		else
		{
			node->node.for_expr.c_style.increment = parse_expr(parser_state);
		}

		assert(consume(parser_state)->token_type == token_type_rparen);
	}
	else
	{
		node->node.for_expr.style = FOR_STYLE_RUST;

		node->node.for_expr.rust_style.var_def = parse_var_def(parser_state);

		{
			const Token token = *consume(parser_state);
			assert(token.token_type == token_type_identifier);
			node->node.for_expr.rust_style.var_def.name = strdup(token.str_val);
		}

		assert(consume(parser_state)->token_type == token_type_in);

		node->node.for_expr.rust_style.iterable = parse_expr(parser_state);
	}

	assert(peek(parser_state)->token_type == token_type_lbrace);
	node->node.for_expr.body = parse_block(parser_state);

	return node;
}

static AST* parse_return_expr(ParserState* parser_state) // NOLINT
{
	AST* node = calloc(1, sizeof(AST));
	node->type = AST_RETURN_STMT;

	assert(consume(parser_state)->token_type == token_type_return);

	node->node.return_stmt.return_stmt = parse_expr(parser_state);
	return node;
}

static AST* parse_break_expr(ParserState* parser_state)
{
	AST* node = calloc(1, sizeof(AST));
	node->type = AST_BREAK_STMT;
	assert(consume(parser_state)->token_type == token_type_break);
	return node;
}

static AST* parse_continue_expr(ParserState* parser_state)
{
	AST* node = calloc(1, sizeof(AST));
	node->type = AST_CONTINUE_STMT;
	assert(consume(parser_state)->token_type == token_type_continue);
	return node;
}

static AST* parse_array_init(ParserState* parser_state)
{
	consume(parser_state); // '['

	AST* node = calloc(1, sizeof(AST));
	node->type = AST_ARRAY_INIT;

	if (match(parser_state, token_type_rsqbracket))
	{
		node->node.array_init.elements = nullptr;
		node->node.array_init.element_count = 0;
		node->node.array_init.is_sized = false;
		return node;
	}

	varray_make(AST**, elements);

	AST* first = parse_expr(parser_state);
	varray_push(elements, first); // NOLINT

	if (match(parser_state, token_type_semicolon))
	{
		node->node.array_init.is_sized = true;
		node->node.array_init.size_expr = parse_expr(parser_state);
		node->node.array_init.elements = elements;
		node->node.array_init.element_count = elements_len;
		assert(consume(parser_state)->token_type == token_type_rsqbracket);
		return node;
	}

	node->node.array_init.is_sized = false;
	while (match(parser_state, token_type_comma)) // NOLINT
	{
		varray_push(elements, parse_expr(parser_state)); // NOLINT
	}

	node->node.array_init.elements = elements;
	node->node.array_init.element_count = elements_len;
	assert(consume(parser_state)->token_type == token_type_rsqbracket);
	return node;
}

static AST* parse_compound_literal(ParserState* parser_state)
{
	assert(consume(parser_state)->token_type == token_type_lparen);
	VarDef type_def = parse_var_def(parser_state);
	assert(consume(parser_state)->token_type == token_type_rparen);

	if (peek(parser_state)->token_type != token_type_lbrace)
	{
		AST* node = calloc(1, sizeof(AST));
		node->type = AST_CAST_EXPR;
		node->node.cast_expr.target_type = type_def;
		node->node.cast_expr.expr = parse_prefix(parser_state);
		return node;
	}

	assert(consume(parser_state)->token_type == token_type_lbrace);

	if (match(parser_state, token_type_rbrace))
	{
		AST* node = calloc(1, sizeof(AST));
		node->type = AST_STRUCT_INIT;
		node->node.struct_init.struct_name = type_def.type.name;
		node->node.struct_init.type = type_def.type;
		node->node.struct_init.fields = nullptr;
		node->node.struct_init.field_count = 0;
		return node;
	}

	if (peek(parser_state)->token_type == token_type_dot)
	{
		AST* node = calloc(1, sizeof(AST));
		node->type = AST_STRUCT_INIT;
		node->node.struct_init.struct_name = type_def.type.name;
		node->node.struct_init.type = type_def.type;
		varray_make(StructFieldInit*, fields);
		while (true) // NOLINT
		{
			StructFieldInit field = {.field_name = nullptr, .value = nullptr};
			assert(consume(parser_state)->token_type == token_type_dot);

			const Token* field_token = consume(parser_state);
			assert(field_token->token_type == token_type_identifier);

			field.field_name = strdup(field_token->str_val);
			assert(consume(parser_state)->token_type == token_type_equal);

			field.value = parse_expr(parser_state);
			varray_push(fields, field); // NOLINT

			if (match(parser_state, token_type_comma))
			{
				if (peek(parser_state)->token_type == token_type_rbrace)
				{
					consume(parser_state);
					break;
				}
				continue;
			}
			else if (match(parser_state, token_type_rbrace)) // NOLINT
			{
				break;
			}
			else
			{
				assert(false && "expected ',' or '}'");
			}
		}
		node->node.struct_init.fields = fields;
		node->node.struct_init.field_count = fields_len;
		return node;
	}
	else // NOLINT
	{
		step_back(parser_state);

		AST* node = calloc(1, sizeof(AST));
		node->type = AST_CAST_EXPR;
		node->node.cast_expr.target_type = type_def;
		node->node.cast_expr.expr = parse_block(parser_state);
		return node;
	}
}

static AST* parse_cast_or_compound(ParserState* parser_state)
{
	assert(consume(parser_state)->token_type == token_type_lparen);

	AST* node = calloc(1, sizeof(AST));
	node->type = AST_CAST_EXPR;

	node->node.cast_expr.target_type = parse_var_def(parser_state);

	assert(consume(parser_state)->token_type == token_type_rparen);

	node->node.cast_expr.expr = parse_prefix(parser_state);

	return node;
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

static AST* make_unary(const Token* op, AST* rhs) // NOLINT
{
	AST* node = (AST*)calloc(1, sizeof(AST));
	node->type = AST_UNARY;
	node->node.unary_expr.op = *op;
	node->node.unary_expr.rhs = rhs;
	return node;
}

static AST* parse_prefix(ParserState* parser_state)
{
	const Token* token = consume(parser_state);
	if (!token)
	{
		return nullptr;
	}

	switch (token->token_type)
	{
		case token_type_star:
		case token_type_ampersand:
		{
			AST* rhs = parse_prefix(parser_state);
			return make_unary(token, rhs);
		}

		case token_type_number:
		case token_type_float:
		case token_type_string:
		case token_type_char:
		case token_type_true:
		case token_type_false:
			return make_literal(token);

		case token_type_identifier:
		{
			return make_identifier(token);
		}

		case token_type_lsqbracket:
			step_back(parser_state);
			return parse_array_init(parser_state);

		case token_type_lparen:
		{
			const Token* next_token = peek(parser_state);

			bool could_be_type = false;
			if ((next_token->token_type == token_type_identifier &&
				 hash_str_contains(&parser_state->known_types, next_token->str_val)) || // NOLINT
				(is_modifier(next_token->token_type)))									// NOLINT
			{
				could_be_type = true;
			}

			if (could_be_type)
			{
				step_back(parser_state);
				usize saved_pos = parser_state->pos;
				consume(parser_state);

				while (is_modifier(peek(parser_state)->token_type)) // NOLINT
				{
					consume(parser_state);
				}

				if (peek(parser_state)->token_type == token_type_identifier &&
					hash_str_contains(&parser_state->known_types, peek(parser_state)->str_val)) // NOLINT
				{
					consume(parser_state);

					while (peek(parser_state)->token_type == token_type_star || // NOLINT
						   peek(parser_state)->token_type == token_type_ampersand ||
						   peek(parser_state)->token_type == token_type_and)
					{
						consume(parser_state);
					}

					while (peek(parser_state)->token_type == token_type_lsqbracket)
					{
						(void)consume(parser_state);
						int bracket_depth = 1;
						while (bracket_depth > 0 && peek(parser_state)->token_type != token_type_eof) // NOLINT
						{
							if (peek(parser_state)->token_type == token_type_lsqbracket)
							{
								bracket_depth++;
							}
							if (peek(parser_state)->token_type == token_type_rsqbracket)
							{
								bracket_depth--;
							}
							consume(parser_state);
						}
					}

					if (peek(parser_state)->token_type == token_type_rparen)
					{
						parser_state->pos = saved_pos;

						return parse_compound_literal(parser_state);
					}
				}

				parser_state->pos = saved_pos;
			}

			AST* expr = parse_expr(parser_state);
			if (!match(parser_state, token_type_rparen))
			{
				assert(false && "expected ')'");
				return nullptr;
			}
			return expr;
		}

		case token_type_lbrace:
			step_back(parser_state);
			return parse_block(parser_state);

		case token_type_if:
			step_back(parser_state);
			return parse_if_expr(parser_state);

		default:
			assert(false && "unexpected token in parse_prefix");
			return nullptr;
	}
}

static AST* parse_postfix(ParserState* parser_state, AST* left) // NOLINT
{
	while (true)
	{
		const Token* token = peek(parser_state); // NOLINT
		if (!token)
		{
			return left;
		}

		switch (token->token_type)
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
			case token_type_arrow:
			{
				(void)consume(parser_state); // '.' / '->'

				const Token* id = peek(parser_state); // NOLINT
				if (!id || id->token_type != token_type_identifier)
				{
					assert(false);
					return left;
				}
				(void)consume(parser_state);

				AST* ident = make_identifier(id);

				left = make_member_access(left, ident, (token->token_type == token_type_dot));
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

static const Token* peek_count(ParserState* parser_state, const i64 count)
{
	return parser_state->pos < parser_state->count ? &parser_state->tokens[parser_state->pos + count] : nullptr;
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

static void step_back(ParserState* parser_state)
{
	parser_state->pos--;
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
	varray_make(Token*, modifiers);
	while (true) // NOLINT
	{
		const Token* token = peek(parser_state);
		if (token == nullptr)
		{
			break;
		}
		if (is_modifier(token->token_type))
		{
			varray_push(modifiers, *consume(parser_state)); // NOLINT
		}
		else
		{
			break;
		}
	}
	return (TokenArray){modifiers, modifiers_len};
}

static i32 precedence(TokenType token_type)
{
	switch (token_type)
	{
		case token_type_equal:			 // =
		case token_type_plus_equal:		 // +=
		case token_type_minus_equal:	 // -=
		case token_type_star_equal:		 // *=
		case token_type_slash_equal:	 // /=
		case token_type_mod_equal:		 // %=
		case token_type_pipe_equal:		 // |=
		case token_type_tilda_equal:	 // ~=
		case token_type_ampersand_equal: // &=
		case token_type_caret_equal:	 // ^=
		case token_type_lshift_equal:	 // <<=
		case token_type_rshift_equal:	 // >>=
			return 1;
		case token_type_or: // ||
			return 2;
		case token_type_and: // &&
			return 3;
		case token_type_pipe: // |
			return 4;
		case token_type_caret: // ^
			return 5;
		case token_type_ampersand: // &
			return 6;
		case token_type_equal_equal: // ==
		case token_type_bang_equal:	 // !=
			return 7;
		case token_type_less:		   // <
		case token_type_less_equal:	   // <=
		case token_type_greater:	   // >
		case token_type_greater_equal: // >=
			return 8;
		case token_type_lshift: // <<
		case token_type_rshift: // >>
			return 9;
		case token_type_dot_dot:	   // ..
		case token_type_dot_dot_equal: // ..
			return 10;
		case token_type_plus:  // +
		case token_type_minus: // -
			return 11;
		case token_type_star:  // *
		case token_type_slash: // /
		case token_type_mod:   // %
			return 12;
		default:
			return 0;
	}
}

static VarDef parse_var_def(ParserState* parser_state) // NOLINT
{
	VarDef var_def = {.name = nullptr, .equals = nullptr, .type = nullptr, .modifier_count = 0, .modifiers = nullptr};
	TokenArray mod_arr = get_modifiers(parser_state);
	var_def.modifiers = mod_arr.tokens;
	var_def.modifier_count = mod_arr.count;

	{
		const Token token = *consume(parser_state);
		assert(token.token_type == token_type_identifier);
		assert(hash_str_contains(&parser_state->known_types, token.str_val) == true);
		var_def.type.name = strdup(token.str_val);
	}

	var_def.type.array_count = 0;
	var_def.type.pointer_count = 0;
	var_def.type.pointer_types = nullptr;

	{
		varray_make(PointerType*, pointer_types);

#pragma unroll 2
		while (peek(parser_state)->token_type == token_type_ampersand ||
			   peek(parser_state)->token_type == token_type_star || peek(parser_state)->token_type == token_type_and)
		{
			usize extra_len = 0;
			if (peek(parser_state)->token_type == token_type_and)
			{
				extra_len = 1;
			}
			const Token token = *consume(parser_state);

			if (token.token_type == token_type_and)
			{
				varray_push(pointer_types, pointer_type_const); // NOLINT
				varray_push(pointer_types, pointer_type_const); // NOLINT
			}
			else
			{
				varray_push(pointer_types, // NOLINT
							(token.token_type == token_type_ampersand) ? pointer_type_const : pointer_type_mut);
			}
		}
		var_def.type.pointer_types = pointer_types;
		var_def.type.pointer_count = pointer_types_len;
	}
	return var_def;
}

static AST* make_literal(const Token* token)
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
		case token_type_float:
			node->node.literal.literal.token_type = token_type_float;
			node->node.literal.literal.float_val = make_float(token);
			node->node.literal.literal.pos = token->pos;
			break;
		case token_type_true:
		case token_type_false:
			node->node.literal.literal.token_type = token->token_type;
			node->node.literal.literal.pos = token->pos;
			break;
		default:
			assert(false);
	}

	return node;
}

static AST* make_identifier(const Token* token)
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_IDENTIFIER;
	node->node.identifier.identifier = *token;
	node->node.identifier.identifier.str_val = strdup(token->str_val);

	return node;
}

static AST* make_binary(const Token* op, AST* left, AST* right) // NOLINT
{
	AST* node = calloc(1, sizeof(AST));
	if (op->token_type == token_type_dot_dot || op->token_type == token_type_dot_dot_equal)
	{
		node->type = AST_RANGE_EXPR;
		node->node.range_expr.start = left;
		node->node.range_expr.end = right;
		node->node.range_expr.inclusive = false;
		if (op->token_type == token_type_dot_dot_equal)
		{
			node->node.range_expr.inclusive = true;
		}
	}
	else
	{
		node->type = AST_BINARY_EXPR;
		node->node.binary_expr.op = *op;
		node->node.binary_expr.left = left;
		node->node.binary_expr.right = right;
	}
	return node;
}

static AST* make_call(AST* callee, AST** args, usize arg_count)
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_FUNC_CALL;
	node->node.func_call.callee = callee;
	node->node.func_call.args = args;
	node->node.func_call.arg_count = arg_count;

	return node;
}

static AST* make_member_access(AST* left, AST* right, bool direct) // NOLINT
{
	AST* node = calloc(1, sizeof(AST));

	node->type = AST_MEMBER_ACCESS;
	node->node.member_access.left = left;
	node->node.member_access.right = right;
	node->node.member_access.direct = direct;

	return node;
}

static AST* make_index(AST* left, AST* index) // NOLINT
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
static f64 make_float(const Token* token)
{
	return strtod(token->str_val, nullptr);
}

static void add_basic_types(ParserState* parser_state)
{
#pragma unroll
	for (usize i = 0; i < ARRAY_SIZE(BASIC_TYPES); i++)
	{
		(void)hash_str_push(&parser_state->known_types, BASIC_TYPES[i]);
	}
}

void free_token_tree(AST* ast)
{
	if (!ast)
	{
		return;
	}

	switch (ast->type)
	{
		case AST_VAR_DEF:
			free_var_def(&ast->node.var_def);
			break;

		case AST_FUNC_DEF:
			free_func_def(&ast->node.func_def);
			break;

		case AST_STRUCT_DEF:
			free((void*)ast->node.struct_def.name);
			free((void*)ast->node.struct_def.modifiers);
			if (ast->node.struct_def.members)
			{
				for (usize i = 0; i < ast->node.struct_def.member_count; i++) // NOLINT
				{
					free_token_tree(ast->node.struct_def.members[i]);
				}
				free((void*)ast->node.struct_def.members);
			}
			break;

		case AST_UNION_DEF:
			free((void*)ast->node.union_def.modifiers);
			free((void*)ast->node.union_def.name);
			if (ast->node.union_def.members)
			{
				for (usize i = 0; i < ast->node.union_def.member_count; i++) // NOLINT
				{
					free_token_tree(ast->node.union_def.members[i]);
				}
				free((void*)ast->node.union_def.members);
			}
			break;

		case AST_ENUM_DEF:
			free((void*)ast->node.enum_def.modifiers);
			free((void*)ast->node.enum_def.name);
			if (ast->node.enum_def.members)
			{
				for (usize i = 0; i < ast->node.enum_def.member_count; i++) // NOLINT
				{
					free_enum_type(&ast->node.enum_def.members[i]);
				}
				free((void*)ast->node.enum_def.members);
			}
			free((void*)ast->node.enum_def.type);
			break;

		case AST_FUNC_CALL:
			free_token_tree(ast->node.func_call.callee);
			if (ast->node.func_call.args)
			{
				for (usize i = 0; i < ast->node.func_call.arg_count; i++) // NOLINT
				{
					free_token_tree(ast->node.func_call.args[i]);
				}
				free((void*)ast->node.func_call.args);
			}
			break;

		case AST_MEMBER_ACCESS:
			free_token_tree(ast->node.member_access.left);
			free_token_tree(ast->node.member_access.right);
			break;

		case AST_BINARY_EXPR:
			free_token_tree(ast->node.binary_expr.left);
			free_token_tree(ast->node.binary_expr.right);
			break;

		case AST_INDEX_EXPR:
			free_token_tree(ast->node.index_expr.left);
			free_token_tree(ast->node.index_expr.index);
			break;

		case AST_LITERAL:
			if (ast->node.literal.literal.token_type == token_type_string)
			{
				free((void*)ast->node.literal.literal.str_val);
			}
			break;

		case AST_IDENTIFIER:
			free((void*)ast->node.identifier.identifier.str_val);
			break;

		case AST_BLOCK:
			if (ast->node.block.statements)
			{
				for (usize i = 0; i < ast->node.block.statement_count; i++) // NOLINT
				{
					free_token_tree(ast->node.block.statements[i]);
				}
				free((void*)ast->node.block.statements);
			}
			free_token_tree(ast->node.block.trailing_expr);
			break;

		case AST_IF_EXPR:
			free_token_tree(ast->node.if_expr.condition);
			free_token_tree(ast->node.if_expr.then_block);
			free_token_tree(ast->node.if_expr.else_block);
			break;

		case AST_WHILE_EXPR:
			free_token_tree(ast->node.while_expr.condition);
			free_token_tree(ast->node.while_expr.then_block);
			break;

		case AST_FOR_EXPR:
			if (ast->node.for_expr.style == FOR_STYLE_C)
			{
				free_token_tree(ast->node.for_expr.c_style.init);
				free_token_tree(ast->node.for_expr.c_style.condition);
				free_token_tree(ast->node.for_expr.c_style.increment);
			}
			else if (ast->node.for_expr.style == FOR_STYLE_RUST)
			{
				free_var_def(&ast->node.for_expr.rust_style.var_def);
				free_token_tree(ast->node.for_expr.rust_style.iterable);
			}
			free_token_tree(ast->node.for_expr.body);
			break;

		case AST_RETURN_STMT:
			free_token_tree(ast->node.return_stmt.return_stmt);
			break;

		case AST_BREAK_STMT:
		case AST_CONTINUE_STMT:
			break;

		case AST_MESSAGE:
			switch (ast->node.message.msg)
			{
				case msg_embed:
				case msg_import:
				case msg_include:
				case msg_include_str:
					free((void*)ast->node.message.import.import);
					break;
				case msg_c_type:
					free((void*)ast->node.message.c_type.type);
					break;
				case msg_invalid:
				default:
					assert(false);
			}
			break;

		case AST_RANGE_EXPR:
			free_token_tree(ast->node.range_expr.start);
			free_token_tree(ast->node.range_expr.end);
			break;
		case AST_UNARY:
			free_token_tree(ast->node.unary_expr.rhs);
			break;
		case AST_ARRAY_INIT:
			free_token_tree(ast->node.array_init.size_expr);
			for (usize i = 0; i < ast->node.array_init.element_count; i++) // NOLINT
			{
				free_token_tree(ast->node.array_init.elements[i]);
			}
			free((void*)ast->node.array_init.elements);
			break;
		case AST_STRUCT_INIT:
			free((void*)ast->node.struct_init.struct_name);
			if (ast->node.struct_init.fields != nullptr)
			{
				for (usize i = 0; i < ast->node.struct_init.field_count; i++) // NOLINT
				{
					free_token_tree(ast->node.struct_init.fields[i].value);
				}
				free((void*)ast->node.struct_init.fields);
			}
			break;
		case AST_CAST_EXPR:
			free_var_def(&ast->node.cast_expr.target_type);
			free_token_tree(ast->node.cast_expr.expr);
			break;
	}

	free((void*)ast);
}

static void free_var_type(VarType* type)
{
	if (!type)
	{
		return;
	}
	free(type->name);
	free(type->pointer_types);

	if (type->array_sizes)
	{
		for (usize i = 0; i < type->array_count; i++) // NOLINT
		{
			free_token_tree(type->array_sizes[i]);
		}
		free((void*)type->array_sizes);
	}
}

static void free_enum_type(EnumType* enum_type)
{
	if (!enum_type)
	{
		return;
	}
	free((void*)enum_type->name);
}

static void free_var_def(VarDef* def)
{
	if (!def)
	{
		return;
	}

	free((void*)def->name);
	free((void*)def->modifiers);
	free_var_type(&def->type);
	free_token_tree(def->equals);
}

static void free_func_def(FuncDef* func)
{
	if (!func)
	{
		return;
	}

	free((void*)func->name);
	free((void*)func->modifiers);

	if (func->template_types)
	{
		for (usize i = 0; i < func->template_count; i++) // NOLINT
		{
			free_token_tree(func->template_types[i]);
		}
		free((void*)func->template_types);
	}

	if (func->params)
	{
		for (usize i = 0; i < func->param_count; i++) // NOLINT
		{
			free_token_tree(func->params[i]);
		}
		free((void*)func->params);
	}
	free_var_def(&func->return_type);
	free_token_tree(func->body);
}

void parse_print(const AST* ast)
{
	parse_print_impl(ast, 0);
}

static void parse_print_impl(const AST* ast, const usize depth) // NOLINT
{
	if (!ast)
	{
		print_indent(depth);
		printf("<null>\n");
		return;
	}

	switch (ast->type)
	{
		case AST_VAR_DEF:
			print_var_def(&ast->node.var_def, depth);
			break;
		case AST_FUNC_DEF:
			print_func_def(&ast->node.func_def, depth);
			break;
		case AST_STRUCT_DEF:
			print_struct_def(&ast->node.struct_def, depth);
			break;
		case AST_UNION_DEF:
			print_union_def(&ast->node.union_def, depth);
			break;
		case AST_ENUM_DEF:
			print_enum_def(&ast->node.enum_def, depth);
			break;
		case AST_FUNC_CALL:
			print_func_call(&ast->node.func_call, depth);
			break;
		case AST_MEMBER_ACCESS:
			print_member_access(&ast->node.member_access, depth);
			break;
		case AST_BINARY_EXPR:
			print_binary_expr(&ast->node.binary_expr, depth);
			break;
		case AST_INDEX_EXPR:
			print_index_expr(&ast->node.index_expr, depth);
			break;
		case AST_LITERAL:
			print_literal(&ast->node.literal, depth);
			break;
		case AST_IDENTIFIER:
			print_identifier(&ast->node.identifier, depth);
			break;
		case AST_BLOCK:
			print_block(&ast->node.block, depth);
			break;
		case AST_IF_EXPR:
			print_if_expr(&ast->node.if_expr, depth);
			break;
		case AST_WHILE_EXPR:
			print_while_expr(&ast->node.while_expr, depth);
			break;
		case AST_FOR_EXPR:
			print_for_expr(&ast->node.for_expr, depth);
			break;
		case AST_RETURN_STMT:
			print_return_expr(&ast->node.return_stmt, depth);
			break;
		case AST_BREAK_STMT:
			print_break_expr(depth);
			break;
		case AST_CONTINUE_STMT:
			print_continue_expr(depth);
			break;
		case AST_MESSAGE:
			print_message(&ast->node.message, depth);
			break;
		case AST_RANGE_EXPR:
			print_range_expr(&ast->node.range_expr, depth);
			break;
		case AST_UNARY:
			print_unary_expr(&ast->node.unary_expr, depth);
			break;
		case AST_ARRAY_INIT:
			print_array_init(&ast->node.array_init, depth);
			break;
		case AST_STRUCT_INIT:
			print_struct_init(&ast->node.struct_init, depth);
			break;
		case AST_CAST_EXPR:
			print_cast_expr(&ast->node.cast_expr, depth);
			break;
	}
}

static void print_indent(const usize depth)
{
	for (usize i = 0; i < depth; i++) // NOLINT
	{
		printf("  ");
	}
}

static void print_pointer_types(const PointerType* pointer_types, const usize count)
{
	for (usize i = 0; i < count; i++) // NOLINT
	{
		switch (pointer_types[i])
		{
			case pointer_type_const:
				printf("&");
				break;
			case pointer_type_mut:
				printf("*");
				break;
			case pointer_type_none:
				break;
		}
	}
}

static void print_var_type(const VarType* var_types, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("Type: %s", var_types->name ? var_types->name : "<None>");

	if (var_types->pointer_count > 0)
	{
		printf(" ");
		print_pointer_types(var_types->pointer_types, var_types->pointer_count);
	}

	if (var_types->array_count > 0)
	{
		for (usize i = 0; i < var_types->array_count; i++) // NOLINT
		{
			if (var_types->array_sizes != nullptr)
			{
				printf("[");
				if (var_types->array_sizes[i]->node.binary_expr.op.token_type == token_type_number)
				{
					printf("%" PRId64, var_types->array_sizes[i]->node.binary_expr.op.num_val);
				}
				else
				{
					print_binary_expr(&var_types->array_sizes[i]->node.binary_expr, depth);
					print_indent(depth);
				}
				printf("]");
			}
			else
			{
				printf("[]");
			}
		}
	}
	printf("\n");
}

static void print_modifiers(const Token* modifiers, const usize count, const usize depth) // NOLINT
{
	if (count == 0)
	{
		return;
	}

	print_indent(depth);
	printf("Modifiers: ");
	for (usize i = 0; i < count; i++) // NOLINT
	{
		printf("%s", token_to_string(modifiers[i].token_type));
		if (i < count - 1)
		{
			printf(", ");
		}
	}
	printf("\n");
}

static void print_var_def(const VarDef* var_def, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("VarDef: %s\n", var_def->name ? var_def->name : "<None>");

	print_modifiers(var_def->modifiers, var_def->modifier_count, depth + 1);
	print_var_type(&var_def->type, depth + 1);

	if (var_def->equals)
	{
		print_indent(depth + 1);
		printf("Initializer:\n");
		parse_print_impl(var_def->equals, depth + 2);
	}
}

static void print_func_def(const FuncDef* func_def, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("FuncDef: %s\n", func_def->name ? func_def->name : "<anonymous>");

	print_modifiers(func_def->modifiers, func_def->modifier_count, depth + 1);

	if (func_def->template_count > 0)
	{
		print_indent(depth + 1);
		printf("Templates: %zu\n", func_def->template_count);
		for (usize i = 0; i < func_def->template_count; i++) // NOLINT
		{
			parse_print_impl(func_def->template_types[i], depth + 2);
		}
	}

	if (func_def->param_count > 0)
	{
		print_indent(depth + 1);
		printf("Parameters: %zu\n", func_def->param_count);
		for (usize i = 0; i < func_def->param_count; i++) // NOLINT
		{
			parse_print_impl(func_def->params[i], depth + 2);
		}
	}

	print_indent(depth + 1);
	printf("Return Type:\n");
	print_var_def(&func_def->return_type, depth + 2);

	if (func_def->body)
	{
		print_indent(depth + 1);
		printf("Body:\n");
		parse_print_impl(func_def->body, depth + 2);
	}
}

static void print_struct_def(const StructDef* struct_def, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("StructDef: %s\n", struct_def->name ? struct_def->name : "<anonymous>");

	print_modifiers(struct_def->modifiers, struct_def->modifier_count, depth + 1);

	print_indent(depth + 1);
	printf("Members: %zu\n", struct_def->member_count);
	for (usize i = 0; i < struct_def->member_count; i++) // NOLINT
	{
		parse_print_impl(struct_def->members[i], depth + 2);
	}
}

static void print_union_def(const UnionDef* union_def, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("UnionDef: %s\n", union_def->name ? union_def->name : "<anonymous>");

	print_modifiers(union_def->modifiers, union_def->modifier_count, depth + 1);

	print_indent(depth + 1);
	printf("Members: %zu\n", union_def->member_count);
	for (usize i = 0; i < union_def->member_count; i++) // NOLINT
	{
		parse_print_impl(union_def->members[i], depth + 2);
	}
}

static void print_enum_def(const EnumDef* enum_def, const usize depth)
{
	print_indent(depth);
	printf("EnumDef: %s", enum_def->name ? enum_def->name : "<anonymous>");
	if (enum_def->type)
	{
		printf(" : %s", enum_def->type);
	}
	printf("\n");

	print_modifiers(enum_def->modifiers, enum_def->modifier_count, depth + 1);

	print_indent(depth + 1);
	printf("Members: %zu\n", enum_def->member_count);
	for (usize i = 0; i < enum_def->member_count; i++) // NOLINT
	{
		print_indent(depth + 2);
		printf("%s", enum_def->members[i].name);
		if (enum_def->members[i].has_value)
		{
			printf(" = %" PRId64, enum_def->members[i].value);
		}
		printf("\n");
	}
}

static void print_func_call(const FuncCall* func_call, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("FuncCall:\n");

	print_indent(depth + 1);
	printf("Callee:\n");
	parse_print_impl(func_call->callee, depth + 2);

	if (func_call->arg_count > 0)
	{
		print_indent(depth + 1);
		printf("Arguments: %zu\n", func_call->arg_count);
		for (usize i = 0; i < func_call->arg_count; i++) // NOLINT
		{
			parse_print_impl(func_call->args[i], depth + 2);
		}
	}
}

static void print_member_access(const MemberAccess* member_access, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("MemberAccess:\n");

	print_indent(depth + 1);
	printf("Left:\n");
	parse_print_impl(member_access->left, depth + 2);

	print_indent(depth + 1);
	printf("Right:\n");
	parse_print_impl(member_access->right, depth + 2);

	print_indent(depth + 1);
	printf("Direct: %s\n", member_access->direct ? "true" : "false"); // NOLINT
}

static void print_binary_expr(const BinaryExpr* binary_expr, const usize depth) // NOLINT
{
	print_indent(depth);
#pragma unroll
	for (usize i = 0; i < ARRAY_SIZE(TOKENS_TYPES_LITERAL); i++)
	{
		if (binary_expr->op.token_type == TOKENS_TYPES_LITERAL[i])
		{
			printf("BinaryExpr: %s: ", token_to_string(binary_expr->op.token_type));
			switch (binary_expr->op.token_type)
			{
				case token_type_number:
					printf("%" PRId64 "\n", binary_expr->op.num_val);
					return;
				case token_type_char:
					printf("%c\n", binary_expr->op.char_val);
					return;
				case token_type_string:
					printf("%s\n", binary_expr->op.str_val);
					return;
				default:
					assert(false);
			}
		}
	}
	printf("BinaryExpr: %s\n", token_to_string(binary_expr->op.token_type));

	print_indent(depth + 1);
	printf("Left:\n");
	parse_print_impl(binary_expr->left, depth + 2);

	print_indent(depth + 1);
	printf("Right:\n");
	parse_print_impl(binary_expr->right, depth + 2);
}

static void print_unary_expr(const UnaryExpr* unary_expr, const usize depth)
{
	print_indent(depth);
	printf("Unary:\n");
	print_indent(depth + 1);
	printf("Op: %s\n", token_to_string(unary_expr->op.token_type));
	parse_print_impl(unary_expr->rhs, depth + 1);
}

static void print_index_expr(const IndexExpr* index_expr, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("IndexExpr:\n");

	print_indent(depth + 1);
	printf("Array:\n");
	parse_print_impl(index_expr->left, depth + 2);

	print_indent(depth + 1);
	printf("Index:\n");
	parse_print_impl(index_expr->index, depth + 2);
}

static void print_literal(const Literal* lit, const usize depth)
{
	print_indent(depth);
	switch (lit->literal.token_type)
	{
		case token_type_number:
			printf("Literal: %" PRId64 "\n", lit->literal.num_val);
			break;
		case token_type_float:
			printf("Literal: %.3lf\n", lit->literal.float_val);
			break;
		case token_type_char:
			printf("Literal: %c\n", lit->literal.char_val);
			break;
		case token_type_string:
			printf("Literal: %s\n", lit->literal.str_val);
			break;
		default:
			printf("Literal: %s\n", token_to_string(lit->literal.token_type));
			assert(false);
	}
}

static void print_identifier(const Identifier* identifier, const usize depth)
{
	print_indent(depth);
	printf("Identifier: %s\n", identifier->identifier.str_val);
}

static void print_block(const Block* block, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("Block: %zu statements\n", block->statement_count);

	for (usize i = 0; i < block->statement_count; i++) // NOLINT
	{
		parse_print_impl(block->statements[i], depth + 1);
	}
	print_indent(depth);
	if (block->trailing_expr != nullptr)
	{
		printf("Trailing expression:\n");
		parse_print_impl(block->trailing_expr, depth + 1);
	}
	else
	{
		printf("Trailing expression: <None>\n");
	}
}

static void print_message(const Message* message, const usize depth)
{
	print_indent(depth);
	printf("Message:\n");
	if (message->msg == msg_embed)
	{
		print_indent(depth + 1);
		printf("@embed \"%s\"\n", message->import.import);
	}
	if (message->msg == msg_c_type)
	{
		print_indent(depth + 1);
		printf("@c_type %s\n", message->c_type.type);
	}
}

static void print_if_expr(const IfExpr* if_expr, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("If:\n");
	print_binary_expr(&if_expr->condition->node.binary_expr, depth + 1);
	print_block(&if_expr->then_block->node.block, depth + 1);
	if (if_expr->else_block == nullptr)
	{
		return;
	}
	else if (if_expr->else_block->type == AST_BLOCK) // NOLINT
	{
		print_block(&if_expr->then_block->node.block, depth + 1);
	}
	else if (if_expr->else_block->type == AST_IF_EXPR) // NOLINT
	{
		print_if_expr(&if_expr->else_block->node.if_expr, depth);
	}
}

static void print_while_expr(const WhileExpr* while_expr, const usize depth) // NOLINT
{
	print_indent(depth);
	printf("While:\n");
	print_binary_expr(&while_expr->condition->node.binary_expr, depth + 1);
	print_block(&while_expr->then_block->node.block, depth + 1);
}

static void print_for_expr(const ForExpr* for_expr, usize depth)
{
	print_indent(depth);

	if (for_expr->style == FOR_STYLE_RUST)
	{
		printf("ForExpr (Rust-style):\n");
		print_indent(depth + 1);
		printf("Iterator Variable:\n");
		print_var_def(&for_expr->rust_style.var_def, depth + 2);

		print_indent(depth + 1);
		printf("Iterable:\n");
		parse_print_impl(for_expr->rust_style.iterable, depth + 2);
	}
	else // FOR_STYLE_C
	{
		printf("ForExpr (C-style):\n");

		print_indent(depth + 1);
		printf("Init:\n");
		if (for_expr->c_style.init)
		{
			parse_print_impl(for_expr->c_style.init, depth + 2);
		}
		else
		{
			print_indent(depth + 2);
			printf("<empty>\n");
		}

		print_indent(depth + 1);
		printf("Condition:\n");
		if (for_expr->c_style.condition)
		{
			parse_print_impl(for_expr->c_style.condition, depth + 2);
		}
		else
		{
			print_indent(depth + 2);
			printf("<empty>\n");
		}

		print_indent(depth + 1);
		printf("Increment:\n");
		if (for_expr->c_style.increment)
		{
			parse_print_impl(for_expr->c_style.increment, depth + 2);
		}
		else
		{
			print_indent(depth + 2);
			printf("<empty>\n");
		}
	}

	print_indent(depth + 1);
	printf("Body:\n");
	parse_print_impl(for_expr->body, depth + 2);
}

static void print_return_expr(const ReturnStmt* return_stmt, const usize depth)
{
	print_indent(depth);
	printf("Return:\n");
	print_binary_expr(&return_stmt->return_stmt->node.binary_expr, depth + 1);
}

static void print_break_expr(const usize depth)
{
	print_indent(depth);
	printf("Break:\n");
}

static void print_continue_expr(const usize depth)
{
	print_indent(depth);
	printf("Continue:\n");
}

static void print_range_expr(const RangeExpr* range_expr, const usize depth)
{
	print_indent(depth);
	printf("RangeExpr: ..\n");

	print_indent(depth + 1);
	printf("Start:\n");
	if (range_expr->start)
	{
		parse_print_impl(range_expr->start, depth + 2);
	}
	else
	{
		print_indent(depth + 2);
		printf("<unbounded>\n");
	}

	print_indent(depth + 1);
	printf("End:\n");
	if (range_expr->end)
	{
		parse_print_impl(range_expr->end, depth + 2);
	}
	else
	{
		print_indent(depth + 2);
		printf("<unbounded>\n");
	}
}

static void print_array_init(const ArrayInit* array_init, const usize depth)
{
	print_indent(depth);
	printf("Array Init:\n");
	print_indent(depth + 1);
	printf("[\n");
	for (usize i = 0; i < array_init->element_count; i++) // NOLINT
	{
		parse_print_impl(array_init->elements[i], depth + 2);
	}
	if (array_init->is_sized)
	{
		print_indent(depth + 2);
		printf(";\n");
		parse_print_impl(array_init->size_expr, depth + 2);
	}
	print_indent(depth + 1);
	printf("]\n");
}

static void print_struct_init(const StructInit* struct_init, usize depth)
{
	print_indent(depth);
	printf("StructInit: %s\n", struct_init->struct_name ? struct_init->struct_name : "<anonymous>");

	print_indent(depth + 1);
	printf("Fields: %zu\n", struct_init->field_count);
	for (usize i = 0; i < struct_init->field_count; i++) // NOLINT
	{
		print_indent(depth + 2);
		printf(".%s =\n", struct_init->fields[i].field_name);
		parse_print_impl(struct_init->fields[i].value, depth + 3);
	}
}

static void print_cast_expr(const CastExpr* cast_expr, usize depth)
{
	print_indent(depth);
	printf("CastExpr:\n");

	print_indent(depth + 1);
	printf("Target Type:\n");
	print_var_def(&cast_expr->target_type, depth + 2);

	print_indent(depth + 1);
	printf("Expression:\n");
	parse_print_impl(cast_expr->expr, depth + 2);
}
