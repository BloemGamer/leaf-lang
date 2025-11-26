#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "code_gen.h"
#include "lexer.h"
#include "log.h"
#include "parser.h"
#include "tokens.h"

size_t amount_errors = 0;
size_t amount_warnings = 0;

char* read_file_to_str(const char* filename);
void write_file(char* filename, char* input);

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
	Token* lexed = lex(file);
	free((void*)file);

	AST* parsed = parse(lexed);

	lex_free(lexed);

	CodeGen code = generate_code(parsed);

	free_token_tree(parsed);

	NewFiles files = code_gen_to_files(&code, "main");

	code_gen_free_code_gen(code);

	write_file("slang-test/main.h", files.h_file);
	write_file("slang-test/main.c", files.c_file);

	code_gen_free_new_files(files);
	assert(system("clang-format -i slang-test/main.h") != -1); // NOLINT
	assert(system("clang-format -i slang-test/main.c") != -1); // NOLINT
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

void write_file(char* filename, char* input) // NOLINT
{
#ifdef _WIN32
	FILE* file = fopen(filename, "w");
#else // linux
	FILE* file = fopen(filename, "we");
#endif
	(void)fputs(input, file);
	(void)fclose(file);
}
