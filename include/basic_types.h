#pragma once

#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>

#define BASIC_CTYPES \
	X(int64_t, i64)  \
	X(int32_t, i32)  \
	X(int16_t, i16)  \
	X(int8_t, i8)    \
                     \
	X(uint64_t, u64) \
	X(uint32_t, u32) \
	X(uint16_t, u16) \
	X(uint8_t, u8)   \
                     \
	X(long double, f128) \
	X(double, f64)   \
	X(float, f32)    \
                     \
	X(size_t, usize)

#define BASIC_NEW_TYPES \
	Y(String) \
	Y(str)

#define BASIC_SUPPORTED_CTYPES \
	Y(char)                    \
	Y(int)                     \
	Y(void)

// typedefing the basic C types
#define X(c, new) typedef c new;
BASIC_CTYPES
#undef X

// clang-format off
static_assert(sizeof(i64)  ==  64 / 8);
static_assert(sizeof(i32)  ==  32 / 8);
static_assert(sizeof(i16)  ==  16 / 8);
static_assert(sizeof(i8)   ==   8 / 8); // NOLINT

static_assert(sizeof(u64)  ==  64 / 8);
static_assert(sizeof(u32)  ==  32 / 8);
static_assert(sizeof(u16)  ==  16 / 8);
static_assert(sizeof(u8)   ==   8 / 8); // NOLINT

static_assert(sizeof(f128) == 128 / 8);
static_assert(sizeof(f64)  ==  64 / 8);
static_assert(sizeof(f32)  ==  32 / 8);
// clang-format on

#define BASIC_TYPES_STR_LEN 7
#define X(_, x) static_assert(BASIC_TYPES_STR_LEN >= sizeof(#x), "BASIC_TYPES_STR_LEN too small");
#define Y(x) X(0, x)
BASIC_CTYPES
BASIC_NEW_TYPES
BASIC_SUPPORTED_CTYPES
#undef Y
#undef X

#define X(_, x) #x,
#define Y(x) #x,
constexpr char BASIC_TYPES[][BASIC_TYPES_STR_LEN] = {BASIC_CTYPES BASIC_NEW_TYPES BASIC_SUPPORTED_CTYPES};
#undef Y
#undef X

#undef BASIC_CTYPES
#undef BASIC_NEW_TYPES
#undef BASIC_TYPES_STR_LEN
#undef BASIC_SUPPORTED_CTYPES
