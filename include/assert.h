#pragma once

// First include with NDEBUG undefined to get the "always on" version
#ifdef NDEBUG
    #define NDEBUG_WAS_DEFINED
    #undef NDEBUG
#endif

#include_next <assert.h>

// Immediately save the real assert implementation before doing anything else
#ifdef __assert_fail
    #define __assert_fail_always __assert_fail
#else
    // Capture the macro expansion of assert itself
    #define assert_always_impl(expr) assert(expr)
#endif

#ifdef NDEBUG_WAS_DEFINED
    #define NDEBUG
    #undef NDEBUG_WAS_DEFINED
#endif

// Undefine assert
#undef assert

// Define debug_assert based on NDEBUG
#ifdef NDEBUG
    #define debug_assert(expr) ((void)0)
#else
    #define debug_assert(expr) assert_always_impl(expr)
#endif

// Redefine assert to always be active (re-include with NDEBUG off)
#ifdef NDEBUG
    #undef NDEBUG
    #include_next <assert.h>
    #define NDEBUG
#else
    #include_next <assert.h>
#endif
