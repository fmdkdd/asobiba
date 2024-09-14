#ifndef UTILS_H
#define UTILS_H

#include <assert.h>
#include <stddef.h>
#include <stdint.h>

#define K_UNUSED(V) ((void)(V))

typedef size_t usize;
typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t u8;

typedef int64_t s64;
typedef int32_t s32;
typedef int16_t s16;
typedef int8_t s8;

template <typename T, usize N> usize K_ARRAY_SIZE(T const (&array)[N]) {
  K_UNUSED(array);
  return N;
}

#define K_ENSURE(x)                                                            \
  do {                                                                         \
    assert(x);                                                                 \
  } while (0)

#if ENABLE_ASSERT
#define K_ASSERT(x) K_ENSURE(x)
#define K_UNREACHABLE() K_ASSERT(0)
#else
#define K_ASSERT(x)                                                            \
  do {                                                                         \
    (void)sizeof((x));                                                         \
  } while (0)
#define K_UNREACHABLE() __builtin_unreachable()
#endif

#endif
