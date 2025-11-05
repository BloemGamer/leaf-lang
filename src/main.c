#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"

char *read_file_to_str(const char* filename);

int main(int argc, char **argv)
{
	char *filename = NULL;
	if(argc < 2)
	{
		// fprintf(stderr, "Not enough arguments, call: %s <file>\n", argv[0]);
		// exit(EXIT_FAILURE);
		filename = "slang-test/main.sl";
	} else {
		filename = argv[1];
	}
	const char *file = read_file_to_str(filename);
	if(file == NULL)
	{
		fprintf(stderr, "Could not open file: %s\n", filename);
		exit(EXIT_FAILURE);
	}
	printf("%s", file);
	Token *lexed = lex(file);
	Token l;
	while((l = *lexed++).token_type != token_type_eof)
	{
		puts(token_to_string(&l));
		if(l.token_type == token_type_string)
		{
			printf("\t%s\n",l.token.str_val);
		}
		if(l.token_type == token_type_char)
		{
			printf("\t%c\n",l.token.char_val);
		}
	}
	puts(token_to_string(&l));
}

char *read_file_to_str(const char* filename)
{
	FILE *file = fopen(filename, "rb");
	if(file == NULL) { return NULL; }

	fseek(file, 0, SEEK_END);
	long length = ftell(file);
	fseek(file, 0, SEEK_SET);

	char *buffer = malloc((length + 1) * sizeof(char));
	if(buffer == NULL)
	{
		fclose(file);
		return NULL;
	}

	fread(buffer, 1, length, file);
	buffer[length] = '\0';

	fclose(file);

	return buffer;
}
