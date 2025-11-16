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
	X(size_t, usize)

#define BASIC_NEW_TYPES Y(String)

#define BASIC_SUPPORTED_CTYPES \
	Y(char)                    \
	Y(int)                     \
	Y(void)

// typedefing the basic C types
#define X(c, new) typedef c new;
BASIC_CTYPES
#undef X

#define BASIC_TYPES_STR_LEN 9
#define X(x, _) static_assert(BASIC_TYPES_STR_LEN >= sizeof(#x), "BASIC_TYPES_STR_LEN too small");
#define Y(x) X(x, 0)
BASIC_CTYPES
BASIC_NEW_TYPES
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
