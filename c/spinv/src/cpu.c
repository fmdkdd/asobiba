#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "cpu.h"
#include "debug.h"

#ifndef BENCH
#define DIFF(reg) {                                             \
    if (old->reg != new->reg) printf(#reg ": %02x ", new->reg); \
    else printf("      ");                                      \
  }

static void diff_state(const CPU *const old, const CPU *const new) {
  printf(" ");
  DIFF(a); DIFF(b); DIFF(c); DIFF(d); DIFF(e); DIFF(h); DIFF(l);
  DIFF(z); DIFF(s); DIFF(p); DIFF(cy); DIFF(ac); DIFF(sp);
}
#endif

#define MAX_MNEMO_LENGTH 13

void print_mnemonic(const char *name, const char *arg1, const char *arg2) {
  char buf[MAX_MNEMO_LENGTH];
  sprintf(buf, "%s", name);
  if (strcmp(arg1, "_")) sprintf(buf+strlen(buf), " %s", arg1);
  if (strcmp(arg2, "_")) sprintf(buf+strlen(buf), ",%s", arg2);
  // Pad the rest
  size_t s = strlen(buf);
  while (s < MAX_MNEMO_LENGTH) {
    sprintf(buf+s, " ");
    ++s;
  }
  printf("%s", buf);
}

// Return 1 if parity of X is odd, 0 if even
static bool parity(u8 x) {
  x ^= x >> 4;
  x ^= x >> 2;
  x ^= x >> 1;
  return x&1;
}

#define OP(code, name, arg1, arg2, cycles, flags, expr)                 \
  case (code): {                                                        \
  cc = (cycles);                                                        \
  DBG(CPU back = *cpu);                                                 \
  DBG(u16 old_pc = cpu->pc);                                            \
  DBG(printf("%04x %02x ", cpu->pc, op[0]));                            \
  cpu->pc++;                                                            \
  OP_ARG(arg1);                                                         \
  OP_ARG(arg2);                                                         \
  DBG(if (cpu->pc - old_pc > 1) printf("%02x ", op[1]); else printf("   ")); \
  DBG(if (cpu->pc - old_pc > 2) printf("%02x ", op[2]); else printf("   ")); \
  DBG(print_mnemonic(#name, #arg1, #arg2));                             \
  expr;                                                                 \
  if ((flags) & Z)   { cpu->z = (r&0xff) == 0; }                        \
  if ((flags) & S)   { cpu->s = (r >> 7) & 1; }                         \
  if ((flags) & P)   { cpu->p = !parity((r&0xff)); }                    \
  if ((flags) & CY)  { cpu->cy = r > 0xff; }                            \
  if ((flags) & CYC) { cpu->cy = 0; }                                   \
  if ((flags) & AC)  { cpu->ac = (prev & 0x10) != (r & 0x10); }         \
  if ((flags) & ACC) { cpu->ac = 0; }                                   \
  DBG(diff_state(&back, cpu));                                          \
  DBG(printf("\n"));                                                    \
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

#define SWAP(a,b) { u8 t = (a); (a) = (b); (b) = t; }
#define R(reg,v) { prev = reg; r = (v); reg = r; }

#define MOV_BLOCK(code, REG, reg)                                       \
  OP(code + 0, MOV,REG,B   , 5, _, { cpu->reg = cpu->b; });             \
  OP(code + 1, MOV,REG,C   , 5, _, { cpu->reg = cpu->c; });             \
  OP(code + 2, MOV,REG,D   , 5, _, { cpu->reg = cpu->d; });             \
  OP(code + 3, MOV,REG,E   , 5, _, { cpu->reg = cpu->e; });             \
  OP(code + 4, MOV,REG,H   , 5, _, { cpu->reg = cpu->h; });             \
  OP(code + 5, MOV,REG,L   , 5, _, { cpu->reg = cpu->l; });             \
  OP(code + 6, MOV,REG,(HL), 7, _, { cpu->reg = cpu->ram[cpu->hl]; });  \
  OP(code + 7, MOV,REG,A   , 5, _, { cpu->reg = cpu->a; });

#define ARITH_BLOCK(code, name, flags, op)                              \
  OP(code + 0, name,A,B   , 4, flags, { R(cpu->a, cpu->a op cpu->b); }); \
  OP(code + 1, name,A,C   , 4, flags, { R(cpu->a, cpu->a op cpu->c); }); \
  OP(code + 2, name,A,D   , 4, flags, { R(cpu->a, cpu->a op cpu->d); }); \
  OP(code + 3, name,A,E   , 4, flags, { R(cpu->a, cpu->a op cpu->e); }); \
  OP(code + 4, name,A,H   , 4, flags, { R(cpu->a, cpu->a op cpu->h); }); \
  OP(code + 5, name,A,L   , 4, flags, { R(cpu->a, cpu->a op cpu->l); }); \
  OP(code + 6, name,A,(HL), 4, flags, { R(cpu->a, cpu->a op cpu->ram[cpu->hl]); }); \
  OP(code + 7, name,A,A   , 4, flags, { R(cpu->a, cpu->a op cpu->a); });

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

void out(CPU *cpu, u8 x) {
  switch (x) {
  case 2:
    cpu->shift_offset = cpu->a;
    break;

  case 4:
    cpu->shift_register >>= 8;
    cpu->shift_register |= cpu->a << 8;
    break;
  }
}

void in(CPU *cpu, u8 x) {
  switch (x) {
  case 1: case 2:
    cpu->a = cpu->ports[x]; break;
  case 3:
    cpu->a = cpu->shift_register >> (8-cpu->shift_offset); break;
  }
}

u16 daa(CPU *cpu) {
  u16 a = cpu->a;
  if ((a & 0xf) > 9 || cpu->ac)
    a += 6;

  if (((a >> 4) & 0xf) > 9 || cpu->cy)
    a += 6 << 4;

  return a;
}

void cpu_init(CPU *cpu) {
  memset(cpu, 0, sizeof(CPU));
  cpu->interrupts_enabled = true;
}

// TODO: eliminate redundancy of Register / Immediate / Memory accesses

int cpu_step(CPU *const cpu) {
  // Fetch and decode
  u8 d8, prev = 0;
  u16 r, d16, addr;

  u8 *op = &cpu->ram[cpu->pc];

  // Count elpased clock cycles (machine states in the 8080 manual)
  u8 cc;

  cpu->is_call = false;

  switch (op[0]) {
  case 0x08: case 0x10: case 0x20:
    // TODO: show immediate values for D8, D16?
    // Or better yet, the output could show values for all arguments
    OP(0x00, NOP, _,_    , 4, _          , {});
    OP(0x01, LXI, BC,D16 ,10, _          , { cpu->bc = d16; });
    OP(0x02, STAX,BC,_   , 7, _          , { WRITE(cpu->bc, cpu->a); });
    OP(0x03, INX, BC,_   , 5, _          , { cpu->bc++; });
    OP(0x04, INR, B,_    , 5, Z|S|P|AC   , { R(cpu->b, cpu->b + 1) });
    OP(0x05, DCR, B,_    , 5, Z|S|P|AC   , { R(cpu->b, cpu->b - 1) });
    OP(0x06, MVI, B,D8   , 7, _          , { cpu->b = d8; });
    OP(0x07, RLC, _,_    , 4, _          , { R(cpu->a, ((cpu->a << 1) | ((cpu->a >> 7)&1))); cpu->cy = r; });
    OP(0x09, DAD, BC,_   ,10, CY         , { R(cpu->hl, cpu->hl + cpu->bc) });
    OP(0x0a, LDAX,BC,_   , 7, _          , { cpu->a = cpu->ram[cpu->bc]; });
    OP(0x0b, DCX, BC,_   , 5, _          , { cpu->bc--; });
    OP(0x0c, INR, C,_    , 5, Z|S|P|AC   , { R(cpu->c, cpu->c + 1) });
    OP(0x0d, DCR, C,_    , 5, Z|S|P|AC   , { R(cpu->c, cpu->c - 1) });
    OP(0x0e, MVI, C,D8   , 7, _          , { cpu->c = d8; });
    OP(0x0f, RRC, _,_    , 4, _          , { R(cpu->a, ((cpu->a << 7)&0x80) | (cpu->a >> 1)); cpu->cy = r >> 7; });
    OP(0x11, LXI, DE,D16 ,10, _          , { cpu->de = d16; });
    OP(0x12, STAX,DE,_   , 7, _          , { WRITE(cpu->de, cpu->a); });
    OP(0x13, INX, DE,_   , 5, _          , { cpu->de++; });
    OP(0x14, INR, D,_    , 5, Z|S|P|AC   , { R(cpu->d, cpu->d + 1); });
    OP(0x15, DCR, D,_    , 5, Z|S|P|AC   , { R(cpu->d, cpu->d - 1); });
    OP(0x16, MVI, D,D8   , 7, _          , { cpu->d = d8; });
    OP(0x17, RAL, _,_    , 4, _          , { R(cpu->a, (cpu->a << 1) | cpu->cy); cpu->cy = prev >> 7; });
    OP(0x19, DAD, DE,_   ,10, CY         , { R(cpu->hl, cpu->hl + cpu->de); });
    OP(0x1a, LDAX,DE,_   , 7, _          , { cpu->a = cpu->ram[cpu->de]; });
    OP(0x1b, DCX, DE,_   , 5, _          , { cpu->de--; });
    OP(0x1c, INR, E,_    , 5, Z|S|P|AC   , { R(cpu->e, cpu->e + 1); });
    OP(0x1d, DCR, E,_    , 5, Z|S|P|AC   , { R(cpu->e, cpu->e - 1); });
    OP(0x1e, MVI, E,D8   , 7, _          , { cpu->e = d8; });
    OP(0x1f, RAR, _,_    , 4, _          , { R(cpu->a, (cpu->cy << 7) | (cpu->a >> 1)); cpu->cy = prev; });
    OP(0x21, LXI, HL,D16 ,10, _          , { cpu->hl = d16; });
    OP(0x22, SHLD,ADDR,_ ,16, _          , { WRITE(addr, cpu->l); WRITE(addr+1, cpu->h); });
    OP(0x23, INX, HL,_   , 5, _          , { cpu->hl++; });
    OP(0x24, INR, H,_    , 5, _          , { R(cpu->h, cpu->h + 1); });
    OP(0x25, DCR, H,_    , 5, _          , { R(cpu->h, cpu->h - 1); });
    OP(0x26, MVI, H,D8   , 7, _          , { cpu->h = d8; });
    OP(0x27, DAA, _,_    , 4, Z|S|P|CY|AC, { R(cpu->a, daa(cpu)); });
    OP(0x29, DAD, HL,_   ,10, CY         , { R(cpu->hl, cpu->hl + cpu->hl) });
    OP(0x2a, LHLD,ADDR,_ ,16, _          , { cpu->l = cpu->ram[addr]; cpu->h = cpu->ram[addr+1]; });
    OP(0x2b, DCX, HL,_   , 5, _          , { cpu->hl--; });
    OP(0x2c, INR, L,_    , 5, _          , { R(cpu->l, cpu->l + 1); });
    OP(0x2d, DCR, L,_    , 5, _          , { R(cpu->l, cpu->l - 1); });
    OP(0x2e, MVI, L,D8   , 7, _          , { cpu->l = d8; });
    OP(0x2f, CMA, _,_    , 4, _          , { cpu->a = ~cpu->a; });
    OP(0x31, LXI, SP,D16 ,10, _          , { cpu->sp = d16; });
    OP(0x32, STA, ADDR,_ ,13, _          , { WRITE(addr, cpu->a); });
    OP(0x33, INX, SP,_   , 5, _          , { cpu->sp++; });
    OP(0x34, INR, (HL),_ ,10, Z|S|P|AC   , { WRITE(cpu->hl, r=cpu->ram[cpu->hl] + 1); });
    OP(0x35, DCR, (HL),_ ,10, Z|S|P|AC   , { WRITE(cpu->hl, r=cpu->ram[cpu->hl] - 1); });
    OP(0x36, MVI, (HL),D8,10, _          , { WRITE(cpu->hl, d8); });
    OP(0x37, STC, _,_    , 4, _          , { cpu->cy = 1; });
    OP(0x39, DAD, SP,_   ,10, CY         , { R(cpu->hl, cpu->hl + cpu->sp); });
    OP(0x3a, LDA, ADDR,_ ,13, _          , { cpu->a = cpu->ram[addr]; });
    OP(0x3b, DCX, SP,_   , 5, _          , { cpu->sp--; });
    OP(0x3c, INR, A,_    , 5, Z|S|P|AC   , { R(cpu->a, cpu->a + 1); });
    OP(0x3d, DCR, A,_    , 5, Z|S|P|AC   , { R(cpu->a, cpu->a - 1); });
    OP(0x3e, MVI, A,D8   , 7, _          , { cpu->a = d8; });
    OP(0x3f, CMC, _,_    , 4, _          , { cpu->cy = ~cpu->cy; });
    MOV_BLOCK(0x40, B, b);
    MOV_BLOCK(0x48, C, c);
    MOV_BLOCK(0x50, D, d);
    MOV_BLOCK(0x58, E, e);
    MOV_BLOCK(0x60, H, h);
    MOV_BLOCK(0x68, L, l);
    OP(0x70, MOV, (HL),B , 7, _          , { WRITE(cpu->hl, cpu->b); });
    OP(0x71, MOV, (HL),C , 7, _          , { WRITE(cpu->hl, cpu->c); });
    OP(0x72, MOV, (HL),D , 7, _          , { WRITE(cpu->hl, cpu->d); });
    OP(0x73, MOV, (HL),E , 7, _          , { WRITE(cpu->hl, cpu->e); });
    OP(0x74, MOV, (HL),H , 7, _          , { WRITE(cpu->hl, cpu->h); });
    OP(0x75, MOV, (HL),L , 7, _          , { WRITE(cpu->hl, cpu->l); });
    OP(0x77, MOV, (HL),A , 7, _          , { WRITE(cpu->hl, cpu->a); });
    MOV_BLOCK(0x78, A, a);
    ARITH_BLOCK(0x80, ADD, Z|S|P|CY |AC , +);
    ARITH_BLOCK(0x88, ADC, Z|S|P|CY |AC , + cpu->cy +);
    ARITH_BLOCK(0x90, SUB, Z|S|P|CY |AC , -);
    ARITH_BLOCK(0x98, SBB, Z|S|P|CY |AC , - cpu->cy -);
    ARITH_BLOCK(0xa0, ANA, Z|S|P|CYC|AC , &);
    ARITH_BLOCK(0xa8, XRA, Z|S|P|CYC|ACC, ^);
    ARITH_BLOCK(0xb0, ORA, Z|S|P|CYC|ACC, |);
    OP(0xb8, CMP, B,_    , 4, Z|S|P|CY|AC, { r = cpu->a - cpu->b; });
    OP(0xb9, CMP, C,_    , 4, Z|S|P|CY|AC, { r = cpu->a - cpu->c; });
    OP(0xba, CMP, D,_    , 4, Z|S|P|CY|AC, { r = cpu->a - cpu->d; });
    OP(0xbb, CMP, E,_    , 4, Z|S|P|CY|AC, { r = cpu->a - cpu->e; });
    OP(0xbc, CMP, H,_    , 4, Z|S|P|CY|AC, { r = cpu->a - cpu->h; });
    OP(0xbd, CMP, L,_    , 4, Z|S|P|CY|AC, { r = cpu->a - cpu->l; });
    OP(0xbe, CMP, (HL),_ , 7, Z|S|P|CY|AC, { r = cpu->a - cpu->ram[cpu->hl]; });
    OP(0xbf, CMP, A,_    , 4, Z|S|P|CY|AC, { r = cpu->a - cpu->a; });
    OP(0xc0, RNZ, _,_    , 5, _          , { if (!cpu->z) { cc += 6; goto ret; } });
    OP(0xc1, POP, BC,_   ,10, _          , { cpu->bc = TO16(cpu->ram[cpu->sp+1], cpu->ram[cpu->sp]); cpu->sp+= 2; });
    OP(0xc2, JNZ, ADDR,_ ,10, _          , { if (!cpu->z) cpu->pc = addr; });
    OP(0xc3, JMP, ADDR,_ ,10, _          , { cpu->pc = addr; });
    OP(0xc4, CNZ, ADDR,_ ,11, _          , { if (!cpu->z) { cc += 6; goto call; } });
    OP(0xc5, PUSH, BC,_  ,11, _          , { cpu->ram[--cpu->sp] = cpu->b; cpu->ram[--cpu->sp] = cpu->c; });
    OP(0xc6, ADI, D8,_   , 7, Z|S|P|CY|AC, { R(cpu->a, cpu->a + d8); });
    OP(0xc8, RZ, _,_     , 5, _          , { if (cpu->z) { cc += 6; goto ret; } });
    OP(0xc9, RET, _,_    ,10, _          , { ret: cpu->pc = TO16(cpu->ram[cpu->sp+1], cpu->ram[cpu->sp]); cpu->sp+= 2; });
    OP(0xca, JZ, ADDR,_  ,10, _          , { if (cpu->z) cpu->pc = addr; });
    OP(0xcc, CZ, ADDR,_  ,11, _          , { if (cpu->z) { cc += 6; goto call; } });
    OP(0xcd, CALL, ADDR,_,17, _          , { call: cpu->ram[--cpu->sp] = cpu->pc >> 8;
                                                   cpu->ram[--cpu->sp] = cpu->pc;
                                                   cpu->pc = addr;
                                                   cpu->is_call = true; });
    OP(0xce, ACI, D8,_   , 7, Z|S|P|CY|AC, { R(cpu->a, cpu->a + d8 + cpu->cy); });
    OP(0xd0, RNC, _,_    , 5, _          , { if (!cpu->cy) { cc += 6; goto ret; } });
    OP(0xd1, POP, DE,_   ,10, _          , { cpu->de = TO16(cpu->ram[cpu->sp+1], cpu->ram[cpu->sp]); cpu->sp+= 2; });
    OP(0xd2, JNC, ADDR,_ ,10, _          , { if (!cpu->cy) cpu->pc = addr; });
    OP(0xd3, OUT, D8,_   ,10, _          , { out(cpu, d8); });
    OP(0xd4, CNC, ADDR,_ ,11, _          , { if (!cpu->cy) { cc += 6; goto call; } });
    OP(0xd5, PUSH, DE,_  ,11, _          , { cpu->ram[--cpu->sp] = cpu->d; cpu->ram[--cpu->sp] = cpu->e; });
    OP(0xd6, SUI, D8,_   , 7, Z|S|P|CY|AC, { R(cpu->a, cpu->a - d8); });
    OP(0xd7, RST, 2,_    ,11, _          , { cpu_interrupt(cpu, 2); });
    OP(0xd8, RC, _,_     , 5, _          , { if (cpu->cy) { cc += 6; goto ret; } });
    OP(0xda, JC, ADDR,_  ,10, _          , { if (cpu->cy) cpu->pc = addr; });
    OP(0xdb, IN, D8,_    ,10, _          , { in(cpu, d8); });
    OP(0xdc, CC, ADDR,_  ,11, _          , { if (cpu->cy) { cc += 6; goto call; } });
    OP(0xde, SBI, D8,_   , 7, Z|S|P|CY|AC, { R(cpu->a, cpu->a - d8 - cpu->cy); });
    OP(0xe0, RPO, _,_    , 5, _          , { if (!cpu->p) { cc += 6; goto ret; } });
    OP(0xe1, POP, HL,_   ,10, _          , { cpu->hl = TO16(cpu->ram[cpu->sp+1], cpu->ram[cpu->sp]); cpu->sp+= 2; });
    OP(0xe2, JPO, ADDR,_ ,10, _          , { if (!cpu->p) cpu->pc = addr; });
    OP(0xe3, XTHL, _,_   ,18, _          , { SWAP(cpu->h, cpu->ram[cpu->sp+1]); SWAP(cpu->l, cpu->ram[cpu->sp]); });
    OP(0xe4, CPO, ADDR,_ ,11, _          , { if (!cpu->p) { cc+= 6; goto call; } });
    OP(0xe5, PUSH, HL,_  ,11, _          , { cpu->ram[--cpu->sp] = cpu->h; cpu->ram[--cpu->sp] = cpu->l; });
    OP(0xe6, ANI, D8,_   , 7, Z|S|P|CY|AC, { R(cpu->a, cpu->a & d8); });
    OP(0xe8, RPE, _,_    , 5, _          , { if (cpu->p) { cc+= 6; goto ret; } });
    OP(0xe9, PCHL, _,_   , 5, _          , { cpu->pc = cpu->hl; });
    OP(0xea, JPE, ADDR,_ ,10, _          , { if (cpu->p) cpu->pc = addr; });
    OP(0xeb, XCHG, _,_   , 4, _          , { SWAP(cpu->h, cpu->d); SWAP(cpu->l, cpu->e); });
    OP(0xec, CPE, ADDR,_ ,11, _          , { if (cpu->p) { cc+= 6; goto call; } });
    OP(0xee, XRI, D8,_   , 7, Z|S|P|CY|AC, { R(cpu->a, cpu->a ^ d8); });
    OP(0xf0, RP, _,_     , 5, _          , { if (!cpu->s) { cc += 6; goto ret; } });
    OP(0xf1, POP, PSW,_  ,10, _          , { cpu->flags = cpu->ram[cpu->sp++]; cpu->a = cpu->ram[cpu->sp++]; });
    OP(0xf2, JP, ADDR,_  ,10, _          , { if (!cpu->s) cpu->pc = addr; });
    OP(0xf3, DI, _,_     , 4, _          , { cpu->interrupts_enabled = false; });
    OP(0xf4, CP, ADDR,_  ,11, _          , { if (!cpu->s) { cc += 6; goto call; } });
    OP(0xf5, PUSH, PSW,_ ,11, _          , { cpu->ram[--cpu->sp] = cpu->a; cpu->ram[--cpu->sp] = cpu->flags; });
    OP(0xf6, ORI, D8,_   , 7, Z|S|P|CY|AC, { R(cpu->a, cpu->a | d8); });
    OP(0xf8, RM, _,_     , 5, _          , { if (cpu->s) { cc += 6; goto ret; } });
    OP(0xf9, SPHL, _,_   , 5, _          , { cpu->sp = cpu->hl; });
    OP(0xfa, JM, ADDR,_  ,10, _          , { if (cpu->s) cpu->pc = addr; });
    OP(0xfb, EI, _,_     , 4, _          , { cpu->interrupts_enabled = true; });
    OP(0xfc, CM, ADDR,_  ,11, _          , { if (cpu->s) { cc += 6; goto call; } });
    OP(0xfe, CPI, D8,_   , 7, Z|S|P|CY|AC, { r = cpu->a - d8; });

#ifdef CPUDIAG
    OP(0xed, CPUOK, _,_  , 1, _          , { printf("\nCPU OK\n"); exit(0); });
    OP(0xfd, CPUER, _,_  , 1, _          , { printf("\nCPU diag errored\n"); exit(1); });
#endif

  default:
    printf("unimplemented opcode: $%02x\n", op[0]);
    exit(1);
  }

  return cc;
}

void cpu_interrupt(CPU *const cpu, u8 rst) {
  if (cpu->interrupts_enabled) {
    cpu->ram[--cpu->sp] = cpu->pc >> 8;
    cpu->ram[--cpu->sp] = cpu->pc;
    cpu->pc = 8 * rst;
  }
}

void cpu_run_for(CPU *const cpu, size_t cycles)
{
  cpu->remaining_cycles += cycles;
  while (cpu->remaining_cycles > 0)
    cpu->remaining_cycles -= cpu_step(cpu);
}

void cpu_emulate_one_frame(CPU *cpu) {
  // Emulate for 1/60 second at 2MHz: 33333 cycles per frame
  // 1 frame = 256 scanlines
  // mid-vblank interrupt happens at  96 lines: cycle 12500
  // vblank     interrupt happens at 224 lines: cycle 29167
  // remaining cycles: 4166

  cpu_run_for(cpu, 12500);
  cpu_interrupt(cpu, 1); // mid-vblank
  cpu_run_for(cpu, 16667);
  cpu_interrupt(cpu, 2); // vblank
  cpu_run_for(cpu, 4166);
}
