#ifndef ASSERT_H
#define ASSERT_H

#include <assert.h>
#define VERIFY(x) do { assert(x); } while (0)

#if ENABLE_ASSERT
#define ASSERT(x) VERIFY(x)
#define ASSERT_UNREACHABLE() ASSERT(0)
#else
#define ASSERT(x) do { (void) sizeof ((x)); } while (0)
#define ASSERT_UNREACHABLE()
#endif

#endif
