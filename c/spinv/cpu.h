#ifndef CPU_H
#define CPU_H

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

#define RAM_SIZE 0x10000         // 8080 is 16bit addressing

typedef struct CPU {
  u16 pc, sp;
  u8 a;
  union {
#if   __BYTE_ORDER == __LITTLE_ENDIAN
    struct { u8  c, b, e, d, l, h; };
#elif __BYTE_ORDER == __BIG_ENDIAN
    struct { u8  b, c, d, e, h, l; };
#else
    #error Unspecified endianness
#endif
    struct { u16 bc,   de,   hl; };
  };
  union {
    u8 flags;
    struct {
      u8 z  : 1;  // 1 = zero
      u8 s  : 1;  // 1 = negative
      u8 p  : 1;  // 1 = even
      u8 cy : 1;  // 1 = carry
      u8 ac : 1;  // used for BCD
      u8 _padd: 3;
    };
  };
  u8 ram[RAM_SIZE];
  u8 ports[3];
  u8 shift_offset;
  u16 shift_register;
  bool is_call;
} CPU;

typedef enum {
  Z   = 0x01,
  S   = 0x02,
  P   = 0x04,
  CY  = 0x08,
  AC  = 0x10,
  CYR = 0x20,
} FLAGS;

typedef enum {
 _, A, B, C, D, E, H, L, BC, DE, HL, SP, PSW, D8, D16, ADDR,
} ARGS;

#ifndef BENCH
void print_mnemonic(const char *name, const char *arg1, const char *arg2);
#endif

int cpu_step(CPU *const cpu);
void cpu_interrupt(CPU *const cpu);

#endif
