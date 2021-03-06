#ifndef CPU_H
#define CPU_H

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

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
      u8 _pad: 3;
    };
  };
  u8 ram[RAM_SIZE];
  u8 ports[3];
  u8 shift_offset;
  u16 shift_register;
  int remaining_cycles;
  bool is_call;
  bool interrupts_enabled;
} CPU;

typedef enum {
  Z   = 0x01,
  S   = 0x02,
  P   = 0x04,
  CY  = 0x08,
  CYC = 0x10,
  AC  = 0x20,
  ACC = 0x40,
} FLAGS;

typedef enum {
 _, A, B, C, D, E, H, L, BC, DE, HL, SP, PSW, D8, D16, ADDR,
} ARGS;

void print_mnemonic(const char *name, const char *arg1, const char *arg2);

void cpu_init(CPU *cpu);
int  cpu_step(CPU *cpu);
void cpu_interrupt(CPU *cpu, u8 rst);
void cpu_run_for(CPU *cpu, size_t cycles);
void cpu_emulate_one_frame(CPU *cpu);

#endif
