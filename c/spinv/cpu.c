#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "cpu.h"

#ifndef BENCH
static void diff_state(const CPU *const old, const CPU *const new) {
  printf(" ");
  DIFF(a); DIFF(b); DIFF(c); DIFF(d); DIFF(e); DIFF(h); DIFF(l);
  DIFF(z); DIFF(s); DIFF(p); DIFF(cy); DIFF(ac); DIFF(sp);
}

static void print_mnemonic(const char *name, const char *arg1, const char *arg2) {
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
#endif

static bool parity(u8 x) {
  x ^= x >> 4;
  x ^= x >> 2;
  x ^= x >> 1;
  return x&1;
}

// TODO: eliminate redundancy of Register / Immediate / Memory accesses

int cpu_step(CPU *const cpu) {
  // Fetch and decode
  u8 d8;
  u16 r, d16, addr;

  u8 *op = &cpu->ram[cpu->pc];

  switch (op[0]) {
  case 0x08: case 0x10: case 0x20:
    // TODO: show immediate values for D8, D16?
    // Or better yet, the output could show values for all arguments
    OP(0x00, NOP, _,_    , _          , {});
    OP(0x01, LXI, BC,D16 , _          , { cpu->bc = d16; });
    OP(0x02, STAX,BC,_   , _          , { WRITE(cpu->bc, cpu->a); });
    OP(0x03, INX, BC,_   , _          , { cpu->bc++; });
    OP(0x04, INR, B,_    , Z|S|P|AC   , { R(cpu->b, cpu->b + 1) });
    OP(0x05, DCR, B,_    , Z|S|P|AC   , { R(cpu->b, cpu->b - 1) });
    OP(0x06, MVI, B,D8   , _          , { cpu->b = d8; });
    OP(0x09, DAD, BC,_   , CY         , { R(cpu->hl, cpu->hl + cpu->bc) });
    OP(0x0a, LDAX,BC,_   , _          , { cpu->a = cpu->ram[cpu->bc]; });
    OP(0x0b, DCX, BC,_   , _          , { cpu->bc--; });
    OP(0x0c, INR, C,_    , Z|S|P|AC   , { R(cpu->c, cpu->c + 1) });
    OP(0x0d, DCR, C,_    , Z|S|P|AC   , { R(cpu->c, cpu->c - 1) });
    OP(0x0e, MVI, C,D8   , _          , { cpu->c = d8; });
    OP(0x0f, RRC, _,_    , CYR        , { R(cpu->a, ((cpu->a&1) << 7) | (cpu->a >> 1)); });
    OP(0x11, LXI, DE,D16 , _          , { cpu->de = d16; });
    OP(0x12, STAX,DE,_   , _          , { WRITE(cpu->de, cpu->a); });
    OP(0x13, INX, DE,_   , _          , { cpu->de++; });
    OP(0x14, INR, D,_    , Z|S|P|AC   , { R(cpu->d, cpu->d + 1); });
    OP(0x15, DCR, D,_    , Z|S|P|AC   , { R(cpu->d, cpu->d - 1); });
    OP(0x16, MVI, D,D8   , _          , { cpu->d = d8; });
    OP(0x19, DAD, DE,_   , CY         , { r = cpu->hl; r += cpu->de; cpu->hl = r; });
    OP(0x1a, LDAX,DE,_   , _          , { cpu->a = cpu->ram[cpu->de]; });
    OP(0x1b, DCX, DE,_   , _          , { cpu->de--; });
    OP(0x1c, INR, E,_    , Z|S|P|AC   , { R(cpu->e, cpu->e + 1); });
    OP(0x1d, DCR, E,_    , Z|S|P|AC   , { R(cpu->e, cpu->e - 1); });
    OP(0x1e, MVI, E,D8   , _          , { cpu->e = d8; });
    OP(0x1f, RAR, _,_    , CYR        , { R(cpu->a, (cpu->cy << 7) | (cpu->a >> 1)); });
    OP(0x21, LXI, HL,D16 , _          , { cpu->hl = d16; });
    OP(0x22, SHLD,ADDR,_ , _          , { WRITE(addr, cpu->l); WRITE(addr+1, cpu->h); });
    OP(0x23, INX, HL,_   , _          , { cpu->hl++; });
    OP(0x24, INR, H,_    , _          , { R(cpu->h, cpu->h + 1); });
    OP(0x25, DCR, H,_    , _          , { R(cpu->h, cpu->h - 1); });
    OP(0x26, MVI, H,D8   , _          , { cpu->h = d8; });
    OP(0x27, DAA, _,_    , _          , { /* special */ });
    OP(0x29, DAD, HL,_   , CY         , { R(cpu->hl, cpu->hl+cpu->hl) });
    OP(0x2a, LHLD,ADDR,_ , _          , { cpu->l = cpu->ram[addr]; cpu->h = cpu->ram[addr+1]; });
    OP(0x2b, DCX, HL,_   , _          , { cpu->hl--; });
    OP(0x2c, INR, L,_    , _          , { R(cpu->l, cpu->l + 1); });
    OP(0x2d, DCR, L,_    , _          , { R(cpu->l, cpu->l - 1); });
    OP(0x2e, MVI, L,D8   , _          , { cpu->l = d8; });
    OP(0x2f, CMA, _,_    , _          , { cpu->a = ~cpu->a; });
    OP(0x31, LXI, SP,D16 , _          , { cpu->sp = d16; });
    OP(0x32, STA, ADDR,_ , _          , { WRITE(addr, cpu->a); });
    OP(0x34, INR, (HL),_ , Z|S|P|AC   , { WRITE(cpu->hl, r=cpu->ram[cpu->hl] + 1); });
    OP(0x35, DCR, (HL),_ , Z|S|P|AC   , { WRITE(cpu->hl, r=cpu->ram[cpu->hl] - 1); });
    OP(0x36, MVI, (HL),D8, _          , { WRITE(cpu->hl, d8); });
    OP(0x37, STC, _,_    , _          , { cpu->cy = 1; });
    OP(0x3a, LDA, ADDR,_ , _          , { cpu->a = cpu->ram[addr]; });
    OP(0x3c, INR, A,_    , Z|S|P|AC   , { R(cpu->a, cpu->a + 1); });
    OP(0x3d, DCR, A,_    , Z|S|P|AC   , { R(cpu->a, cpu->a - 1); });
    OP(0x3e, MVI, A,D8   , _          , { cpu->a = d8; });
    OP(0x3f, CMC, _,_    , _          , { cpu->cy = ~cpu->cy; });
    MOV_BLOCK(0x40, B, b);
    MOV_BLOCK(0x48, C, c);
    MOV_BLOCK(0x50, D, d);
    MOV_BLOCK(0x58, E, e);
    MOV_BLOCK(0x60, H, h);
    MOV_BLOCK(0x68, L, l);
    OP(0x70, MOV, (HL),B , _          , { WRITE(cpu->hl, cpu->b); });
    OP(0x71, MOV, (HL),A , _          , { WRITE(cpu->hl, cpu->c); });
    OP(0x72, MOV, (HL),A , _          , { WRITE(cpu->hl, cpu->d); });
    OP(0x73, MOV, (HL),A , _          , { WRITE(cpu->hl, cpu->e); });
    OP(0x74, MOV, (HL),A , _          , { WRITE(cpu->hl, cpu->h); });
    OP(0x75, MOV, (HL),A , _          , { WRITE(cpu->hl, cpu->l); });
    OP(0x77, MOV, (HL),A , _          , { WRITE(cpu->hl, cpu->a); });
    MOV_BLOCK(0x78, A, a);
    ARITH_BLOCK(0x80, ADD, +);
    ARITH_BLOCK(0x88, ADC, + cpu->cy +);
    ARITH_BLOCK(0x90, SUB, -);
    ARITH_BLOCK(0x98, SBB, - cpu->cy -);
    ARITH_BLOCK(0xa0, ANA, &);
    ARITH_BLOCK(0xa8, XRA, ^);
    ARITH_BLOCK(0xb0, ORA, |);
    OP(0xb8, CMP, B,_    , C|S|P|CY|AC, { r = cpu->a - cpu->b; });
    OP(0xb9, CMP, C,_    , C|S|P|CY|AC, { r = cpu->a - cpu->c; });
    OP(0xba, CMP, D,_    , C|S|P|CY|AC, { r = cpu->a - cpu->d; });
    OP(0xbb, CMP, E,_    , C|S|P|CY|AC, { r = cpu->a - cpu->e; });
    OP(0xbc, CMP, H,_    , C|S|P|CY|AC, { r = cpu->a - cpu->h; });
    OP(0xbd, CMP, L,_    , C|S|P|CY|AC, { r = cpu->a - cpu->l; });
    OP(0xbe, CMP, (HL),_ , C|S|P|CY|AC, { r = cpu->a - cpu->ram[cpu->hl]; });
    OP(0xbf, CMP, A,_    , C|S|P|CY|AC, { r = cpu->a - cpu->a; });
    OP(0xc0, RNZ, _,_    , _          , { if (!cpu->z) goto ret; });
    OP(0xc1, POP, BC,_   , _          , { cpu->bc = TO16(cpu->ram[cpu->sp+1], cpu->ram[cpu->sp]); cpu->sp+= 2; });
    OP(0xc2, JNZ, ADDR,_ , _          , { if (!cpu->z) cpu->pc = addr; });
    OP(0xc3, JMP, ADDR,_ , _          , { cpu->pc = addr; });
    OP(0xc4, CNZ, ADDR,_ , _          , { if (!cpu->z) goto call; });
    OP(0xc5, PUSH, BC,_  , _          , { cpu->ram[--cpu->sp] = cpu->b; cpu->ram[--cpu->sp] = cpu->c; });
    OP(0xc6, ADI, D8,_   , Z|S|P|CY|AC, { R(cpu->a, cpu->a + d8); });
    OP(0xc8, RZ, _,_     , _          , { if (cpu->z) goto ret; });
    OP(0xc9, RET, _,_    , _          , { ret: cpu->pc = TO16(cpu->ram[cpu->sp+1], cpu->ram[cpu->sp]); cpu->sp+= 2; });
    OP(0xca, JZ, ADDR,_  , _          , { if (cpu->z) cpu->pc = addr; });
    OP(0xcc, CZ, ADDR,_  , _          , { if (cpu->z) goto call; });
    OP(0xcd, CALL, ADDR,_, _          , { call: cpu->ram[--cpu->sp] = cpu->pc >> 8;
                                                cpu->ram[--cpu->sp] = cpu->pc;
                                                cpu->pc = addr; });
    OP(0xce, ACI, D8,_   , Z|S|P|CY|AC, { R(cpu->a, cpu->a + d8 + cpu->cy); });
    OP(0xd0, RNC, _,_    , _          , { if (!cpu->cy) goto ret; });
    OP(0xd1, POP, DE,_   , _          , { cpu->de = TO16(cpu->ram[cpu->sp+1], cpu->ram[cpu->sp]); cpu->sp+= 2; });
    OP(0xd2, JNC, ADDR,_ , _          , { if (!cpu->cy) cpu->pc = addr; });
    OP(0xd3, OUT, D8,_   , _          , { /* special */ });
    OP(0xd4, CNC, ADDR,_ , _          , { if (!cpu->cy) goto call; });
    OP(0xd5, PUSH, DE,_  , _          , { cpu->ram[--cpu->sp] = cpu->d; cpu->ram[--cpu->sp] = cpu->e; });
    OP(0xd6, SUI, D8,_   , Z|S|P|CY|AC, { R(cpu->a, cpu->a - d8); });
    OP(0xd7, RST, 2,_    , _          , { addr = 0x10; goto call; });
    OP(0xd8, RC, _,_     , _          , { if (cpu->cy) goto ret; });
    OP(0xda, JC, ADDR,_  , _          , { if (cpu->cy) cpu->pc = addr; });
    OP(0xdb, IN, D8,_    , _          , { /* special */ });
    OP(0xdc, CC, ADDR,_  , _          , { if (cpu->cy) goto call; });
    OP(0xde, SBI, D8,_   , Z|S|P|CY|AC, { R(cpu->a, cpu->a - d8 - cpu->cy); });
    OP(0xe0, RPO, _,_    , _          , { if (!cpu->p) goto ret; });
    OP(0xe1, POP, HL,_   , _          , { cpu->hl = TO16(cpu->ram[cpu->sp+1], cpu->ram[cpu->sp]); cpu->sp+= 2; });
    OP(0xe2, JPO, ADDR,_ , _          , { if (!cpu->p) cpu->pc = addr; });
    OP(0xe3, XTHL, _,_   , _          , { SWAP(cpu->h, cpu->ram[cpu->sp+1]); SWAP(cpu->l, cpu->ram[cpu->sp]); });
    OP(0xe4, CPO, ADDR,_ , _          , { if (!cpu->p) goto call; });
    OP(0xe5, PUSH, HL,_  , _          , { cpu->ram[--cpu->sp] = cpu->h; cpu->ram[--cpu->sp] = cpu->l; });
    OP(0xe6, ANI, D8,_   , Z|S|P|CY|AC, { R(cpu->a, cpu->a & d8); });
    OP(0xe8, RPE, _,_    , _          , { if (cpu->p) goto ret; });
    OP(0xea, JPE, ADDR,_ , _          , { if (cpu->p) cpu->pc = addr; });
    OP(0xeb, XCHG, _,_   , _          , { SWAP(cpu->h, cpu->d); SWAP(cpu->l, cpu->e); });
    OP(0xec, CPE, ADDR,_ , _          , { if (cpu->p) goto call; });
    OP(0xee, XRI, D8,_   , Z|S|P|CY|AC, { R(cpu->a, cpu->a ^ d8); });
    OP(0xf0, RP, _,_     , _          , { if (!cpu->s) goto ret; });
    OP(0xf1, POP, PSW,_  , _          , { cpu->flags = cpu->ram[cpu->sp++]; cpu->a = cpu->ram[cpu->sp++]; });
    OP(0xf2, JP, ADDR,_  , _          , { if (!cpu->s) cpu->pc = addr; });
    OP(0xf3, DI, _,_     , _          , { /* special */ });
    OP(0xf4, CP, ADDR,_  , _          , { if (!cpu->s) goto call; });
    OP(0xf5, PUSH, PSW,_ , _          , { cpu->ram[--cpu->sp] = cpu->a; cpu->ram[--cpu->sp] = cpu->flags; });
    OP(0xf6, ORI, D8,_   , Z|S|P|CY|AC, { R(cpu->a, cpu->a | d8); });
    OP(0xf8, RM, _,_     , _          , { if (cpu->s) goto ret; });
    OP(0xfa, JM, ADDR,_  , _          , { if (cpu->s) cpu->pc = addr; });
    OP(0xfb, EI, _,_     , _          , { /* special */ });
    OP(0xfc, CM, ADDR,_  , _          , { if (cpu->s) goto call; });
    OP(0xfe, CPI, D8,_   , Z|S|P|CY|AC, { r = cpu->a - d8; });

#ifdef CPUDIAG
    OP(0xfd, CPUER, _,_  , _          , { printf("\nCPU diag errored\n"); exit(1); });
#endif

  default:
    printf("unimplemented opcode: $%02x\n", op[0]);
    exit(1);
  }

  return 0;
}

void cpu_interrupt(CPU *const cpu) {
  cpu->ram[--cpu->sp] = cpu->pc >> 8;
  cpu->ram[--cpu->sp] = cpu->pc;
  cpu->pc = 0x10;
}
