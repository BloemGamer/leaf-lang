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

	// lex_print(lexed);

	AST* parsed = parse(lexed);

	parse_print(parsed);

	lex_free(lexed);

	free_token_tree(parsed);
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

	usize read = fread(buffer, 1, length, file);
	if (read != length)
	{
		if (ferror(file))
		{
			perror("Error reading file");
		}
		else if (feof(file))
		{
			(void)fprintf(stderr, "Unexpected end of file\n");
		}
	}
	buffer[length] = '\0';

	(void)fclose(file);

	return buffer;
}
