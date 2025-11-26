#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>

noreturn static inline void assert_abort(void)
{
	abort();
}

#define assert(expr, ...)                                                                \
	{                                                                                    \
		if (!(expr))                                                                     \
		{                                                                                \
			(void)fprintf(stderr, "Assertion failed: %s\n", #expr);                      \
			/* print extra message only if __VA_ARGS__ exists */                         \
			__VA_OPT__((void)fprintf(stderr, __VA_ARGS__); (void)fprintf(stderr, "\n");) \
			(void)fprintf(stderr, "File: %s\nLine: %d\n", __FILE__, __LINE__);           \
			assert_abort();                                                              \
		}                                                                                \
	}

#ifdef NDEBUG
#define debug_assert(expr, ...) (void)0
#else /* NDEBUG */
#define debug_assert(expr, ...) assert(expr, __VA_ARGS__)
#endif
