#ifndef UTILS_H
#define UTILS_H

#include <assert.h>
#include <stddef.h>
#include <stdint.h>

#define UNUSED(V) ((void)(V))

typedef size_t   usize;
typedef uint32_t u32;
typedef int32_t  s32;
typedef uint64_t u64;
typedef int64_t  s64;
typedef uint8_t  u8;

template <typename T, usize N> usize ARRAY_SIZE(T const (&array)[N]) {
  UNUSED(array);
  return N;
}

#define ENSURE(x)                                                              \
  do {                                                                         \
    assert(x);                                                                 \
  } while (0)

#if ENABLE_ASSERT
#define ASSERT(x) ENSURE(x)
#define UNREACHABLE() ASSERT(0)
#else
#define ASSERT(x)                                                              \
  do {                                                                         \
    (void)sizeof((x));                                                         \
  } while (0)
#define UNREACHABLE() __builtin_unreachable()
#endif

#endif
