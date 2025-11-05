#include <stdio.h>

#define LOG_ERROR(pos, format, ...) \
	fprintf(stderr, "\x1B[31m" "ERROR: [line %zu:%zu] "format "\x1B[0m", pos.line, pos.character, ##__VA_ARGS__)

#define LOG_WARN(pos, format, ...) \
	fprintf(stderr, "\x1B[33m" "WARNING: [line %zu:%zu] "format "\x1B[0m", pos.line, pos.character, ##__VA_ARGS__)

