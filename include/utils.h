#pragma once

#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>

#define MAX(a, b) ((a) > (b)) ? (a) : (b)
#define MIN(a, b) ((a) < (b)) ? (a) : (b)
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
