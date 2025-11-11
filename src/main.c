#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"
#include "log.h"
#include "parser.h"
#include "tokens.h"

size_t amount_errors = 0;
size_t amount_warnings = 0;

char* read_file_to_str(const char* filename);

int main(int argc, char** argv)
{
	char* filename = nullptr;
	if (argc < 2)
	{
		// fprintf(stderr, "Not enough arguments, call: %s <file>\n", argv[0]);
		// exit(EXIT_FAILURE);
		filename = "slang-test/main.sl";
	}
	else
	{
		filename = argv[1];
	}
	const char* file = read_file_to_str(filename);
	if (file == NULL)
	{
		(void)fprintf(stderr, "Could not open file: %s\n", filename);
		return EXIT_FAILURE;
	}
	// printf("%s", file);
	Token* lexed = lex(file);
	free((void*)file);
	// Token* lexed_old = lexed;
	// Token l;
	// while ((l = *lexed++).token_type != token_type_eof)
	// {
	// 	puts(token_to_string(l.token_type));
	// 	if (l.token_type == token_type_string)
	// 	{
	// 		printf("\t\"%s\"\n", l.str_val);
	// 	}
	// 	if (l.token_type == token_type_char)
	// 	{
	// 		printf("\t'%s'\n", l.str_val);
	// 	}
	// 	if (l.token_type == token_type_number)
	// 	{
	// 		printf("\t%s\n", l.str_val);
	// 	}
	// 	if (l.token_type == token_type_identifier || l.token_type == token_type_message)
	// 	{
	// 		printf("\t%s\n", l.str_val);
	// 	}
	// }
	// lexed = lexed_old;
	// puts(token_to_string(l.token_type));

	AST parsed = *parse(lexed);

	// auto fn_def = parsed.node.func_def;
	//
	// for (int i = 0; i < fn_def.modifier_count; i++)
	// {
	// 	printf("%s", token_to_string(fn_def.modifiers[i].token_type));
	// }
	// printf("fn %s( ", fn_def.name);
	// for (int i = 0; i < fn_def.param_count; i++)
	// {
	// 	printf("%s %s,", fn_def.params[i]->node.var_def.type.name, fn_def.params[i]->node.var_def.name);
	// }
	// printf("\b)");
	// if (fn_def.return_type.str_val != nullptr)
	// {
	// 	printf("-> %s", fn_def.return_type.str_val);
	// }
	// printf("\n");

	lex_free(lexed);
}

char* read_file_to_str(const char* filename)
{
#ifdef _WIN32
	FILE* file = fopen(filename, "rb");
#else // LINUX
	FILE* file = fopen(filename, "rbe");
#endif
	if (file == NULL)
	{
		return nullptr;
	}

	(void)fseek(file, 0, SEEK_END);
	long length = ftell(file);
	(void)fseek(file, 0, SEEK_SET);

	char* buffer = malloc((length + 1) * sizeof(char));
	if (buffer == NULL)
	{
		(void)fclose(file);
		return nullptr;
	}

	(void)fread(buffer, 1, length, file);
	buffer[length] = '\0';

	(void)fclose(file);

	return buffer;
}
