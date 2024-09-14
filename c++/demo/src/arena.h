#ifndef ARENA_H
#define ARENA_H

#include <stdlib.h>

#include "utils.h"

struct Arena {
  u8* m_pBuffer;
  u32 m_size;
  u32 m_capacity;

  void init(u32 capacity) {
    m_pBuffer = (u8*)malloc(capacity);
    K_ENSURE(m_pBuffer != NULL);
    m_capacity = capacity;
    m_size = 0;
  }

  void quit() {
    K_ASSERT(m_pBuffer != NULL);
    free(m_pBuffer);
    m_pBuffer = NULL;
    m_capacity = 0;
    m_size = 0;
  }

  void* push(u32 size) {
    K_ASSERT((m_size + size) <= m_capacity);

    void* p = &m_pBuffer[m_size];
    m_size += size;

    return p;
  }

  void pop(u32 size) {
    K_ASSERT(size <= m_size);

    m_size -= size;
  }
};

#endif
