#pragma once

#include <stdio.h>

extern size_t amount_errors;
extern size_t amount_warnings;

#define LOG_ERROR(pos, format, ...) LOG("\x1B[31m", "ERROR", pos, format __VA_OPT__(, ) __VA_ARGS__)

#define LOG_WARN(pos, format, ...) LOG("\x1B[33m", "WARNING", pos, format __VA_OPT__(, ) __VA_ARGS__)

#define LOG(colour_str, log_type_str, pos, format, ...)          \
	(void)fprintf(stderr,                                        \
				  colour_str "[line %zu:%zu] " log_type_str ": " \
							 "\x1B[0m" format,                   \
				  pos.line, pos.character __VA_OPT__(, ) __VA_ARGS__)

#define errprintf(...) fprintf(stderr, __VA_ARGS__)
