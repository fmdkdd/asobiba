#ifndef COMMON_H
#define COMMON_H

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;

void die(const char *const msg);

#ifdef ENABLE_ASSERT
#define ASSERT(x)
#else
#define ASSERT(x) assert(x)
#endif

#define CHECK(x) do { if (!(x)) assert(false); } while(0)

#define TO16(h,l) ((h) << 8 | (l))

#ifdef BENCH
#define DBG(expr)
#else
#define DBG(expr) expr
#endif

#endif
