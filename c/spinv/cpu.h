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

#define DIFF(reg) {                                             \
    if (old->reg != new->reg) printf(#reg ": %02x ", new->reg); \
    else printf("      ");                                      \
  }

#define MAX_MNEMO_LENGTH 13

#ifdef BENCH
#define DBG(expr)
#else
#define DBG(expr) expr
#endif

#define OP(code, name, arg1, arg2, flags, expr)                         \
  case (code): {                                                        \
    DBG(CPU back = *cpu);                                               \
    DBG(u16 old_pc = cpu->pc);                                          \
    DBG(printf("%04x %02x ", cpu->pc, op[0]));                          \
    cpu->pc++;                                                          \
    OP_ARG(arg1);                                                       \
    OP_ARG(arg2);                                                       \
    DBG(if (cpu->pc - old_pc > 1) printf("%02x ", op[1]); else printf("   ")); \
    DBG(if (cpu->pc - old_pc > 2) printf("%02x ", op[2]); else printf("   ")); \
    DBG(print_mnemonic(#name, #arg1, #arg2));                           \
    expr;                                                               \
    if ((flags) & Z)   { cpu->z = (r&0xff) == 0; }                      \
    if ((flags) & S)   { cpu->s = (r >> 7) & 1; }                       \
    if ((flags) & P)   { cpu->p = parity((r&0xff)); }                   \
    if ((flags) & CY)  { cpu->cy = r > 0xff; }                          \
    if ((flags) & CYR) { cpu->cy = r & 1; }                             \
    DBG(diff_state(&back, cpu));                                        \
    DBG(printf("\n"));                                                  \
  }                                                                     \
  break

#define OP_ARG(arg)                                                     \
  switch (arg) {                                                        \
  case _: case A:  case B: case C: case D: case E: case H:              \
  case L: case BC: case DE: case HL: case SP: case PSW: break;          \
  case D8  : d8   = cpu->ram[cpu->pc++]; break;                         \
  case D16 : d16  = TO16(op[2], op[1]); cpu->pc+=2; break;              \
  case ADDR: addr = TO16(op[2], op[1]); cpu->pc+=2; break;              \
  }

#define TO16(h,l) ((h) << 8 | (l))
#define SWAP(a,b) { u8 t = (a); (a) = (b); (b) = t; }
#define R(reg,v) { r = (v); reg = r; }

#define MOV_BLOCK(code, REG, reg)                                       \
  OP(code + 0, MOV,REG,B   , _, { cpu->reg = cpu->b; });                \
  OP(code + 1, MOV,REG,C   , _, { cpu->reg = cpu->c; });                \
  OP(code + 2, MOV,REG,D   , _, { cpu->reg = cpu->d; });                \
  OP(code + 3, MOV,REG,E   , _, { cpu->reg = cpu->e; });                \
  OP(code + 4, MOV,REG,H   , _, { cpu->reg = cpu->h; });                \
  OP(code + 5, MOV,REG,L   , _, { cpu->reg = cpu->l; });                \
  OP(code + 6, MOV,REG,(HL), _, { cpu->reg = cpu->ram[cpu->hl]; });     \
  OP(code + 7, MOV,REG,A   , _, { cpu->reg = cpu->a; });

#define ARITH_BLOCK(code, name, op)                                     \
  OP(code + 0, name,A,B   , Z|S|P|CY|AC, { R(cpu->a, cpu->a op cpu->b); }); \
  OP(code + 1, name,A,C   , Z|S|P|CY|AC, { R(cpu->a, cpu->a op cpu->c); }); \
  OP(code + 2, name,A,D   , Z|S|P|CY|AC, { R(cpu->a, cpu->a op cpu->d); }); \
  OP(code + 3, name,A,E   , Z|S|P|CY|AC, { R(cpu->a, cpu->a op cpu->e); }); \
  OP(code + 4, name,A,H   , Z|S|P|CY|AC, { R(cpu->a, cpu->a op cpu->h); }); \
  OP(code + 5, name,A,L   , Z|S|P|CY|AC, { R(cpu->a, cpu->a op cpu->l); }); \
  OP(code + 6, name,A,(HL), Z|S|P|CY|AC, { R(cpu->a, cpu->a op cpu->ram[cpu->hl]); }); \
  OP(code + 7, name,A,A   , Z|S|P|CY|AC, { R(cpu->a, cpu->a op cpu->a); });

#define WRITE(addr, v)                                                  \
  switch (addr) {                                                       \
  case 0x0000 ... 0x1fff:                                               \
    DBG(printf("\nInvalid write to ROM @%02x\n", (addr)));              \
    cpu->ram[(addr)] = v;                                               \
    break;                                                              \
  case 0x2400 ... 0x3fff:                                               \
    DBG(printf("\nWrite to video RAM @%02x %02x\n", (addr), (v)));      \
    cpu->ram[(addr)] = (v);                                             \
    break;                                                              \
  default:                                                              \
    cpu->ram[(addr)] = (v);                                             \
  }

int cpu_step(CPU *const cpu);
void cpu_interrupt(CPU *const cpu);

#endif
