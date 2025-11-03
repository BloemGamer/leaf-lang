#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"

char *read_file_to_str(const char* filename);

int main(int argc, char **argv)
{
	if(argc < 2)
	{
		fprintf(stderr, "Not enough arguments, call: %s <file>\n", argv[0]);
		exit(EXIT_FAILURE);
	}
	const char *filename = argv[1];
	const char *file = read_file_to_str(filename);
	if(file == NULL)
	{
		fprintf(stderr, "Could not open file: %s\n", filename);
		exit(EXIT_FAILURE);
	}
	printf("%s", file);
	lex(file);
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
