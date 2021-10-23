#ifndef UTILS_H
#define UTILS_H

#include <assert.h>
#include <stddef.h>
#include <stdint.h>

#define UNUSED(V) ((void)(V))

typedef size_t   usize;
typedef uint32_t u32;
typedef uint64_t u64;
typedef float f32;

template <typename T, usize N> usize ARRAY_SIZE(T const (&array)[N]) {
  UNUSED(array);
  return N;
}

#define STATIC_ASSERT(x) static_assert(x, "")

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
#define UNREACHABLE()
#endif

#endif
