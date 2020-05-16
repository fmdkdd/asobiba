#ifndef COMMON_H
#define COMMON_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;

void die(const char *const msg);

#ifdef BENCH
#define DBG(expr)
#else
#define DBG(expr) expr
#endif

#endif
