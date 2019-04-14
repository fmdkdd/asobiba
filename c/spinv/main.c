#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

#define RAM_SIZE 0x10000         // 8080 is 16bit address

typedef struct CPU {
  u8 a;
  union {
    struct { u8  b, c, d, e, h, l; };
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
  u16 pc, sp;
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

#define DIFF(REG) {                                             \
    if (old->REG != new->REG) printf(#REG ": %02x ", new->REG); \
    else printf("      ");                                      \
  }

void diff_state(CPU *old, CPU *new) {
  printf("\t");
  DIFF(a); DIFF(b); DIFF(c); DIFF(d); DIFF(e); DIFF(h); DIFF(l);
  DIFF(z); DIFF(s); DIFF(p); DIFF(cy); DIFF(ac); DIFF(sp);
}

// TODO: other flags

#define OP(code, name, arg1, arg2, flags, expr)                         \
  case (code): {                                                        \
    CPU back = cpu;                                                     \
    u16 old_pc = cpu.pc;                                                \
    printf("%04x %02x ", cpu.pc++, op[0]);                              \
    OP_ARG(arg1);                                                       \
    OP_ARG(arg2);                                                       \
    if (cpu.pc - old_pc > 1) printf("%02x ", op[1]); else printf("   "); \
    if (cpu.pc - old_pc > 2) printf("%02x ", op[2]); else printf("   "); \
    printf(#name);                                                      \
    if ((arg1) != _) printf(" %s", #arg1);                              \
    if ((arg2) != _) printf(",%s", #arg2);                              \
    expr;                                                               \
    if ((flags) & Z)   { cpu.z = (r&0xff) == 0; }                       \
    if ((flags) & S)   { cpu.s = (r >> 7) & 1; }                        \
    if ((flags) & P)   { cpu.p = parity((r&0xff)); }                    \
    if ((flags) & CY)  { cpu.cy = r > 0xff; }                           \
    if ((flags) & CYR) { cpu.cy = r & 1; }                              \
    diff_state(&back, &cpu);                                            \
    printf("\n");                                                       \
  }                                                                     \
  break

#define OP_ARG(arg)                                                     \
  switch (arg) {                                                        \
  case _: case A:  case B: case C: case D: case E: case H:              \
  case L: case BC: case DE: case HL: case SP: case PSW: break;          \
  case D8  : d8   = cpu.ram[cpu.pc++]; break;                           \
  case D16 : d16  = TO16(op[2], op[1]); cpu.pc+=2; break;               \
  case ADDR: addr = TO16(op[2], op[1]); cpu.pc+=2; break;               \
  }

#define TO16(H,L) ((H) << 8 | (L))
#define SWAP(A,B) { u8 t = (A); (A) = (B); (B) = t; }
#define R(REG,V) { r = (V); REG = r; }

#define MOV_BLOCK(CODE, REG, reg)                                  \
  OP(CODE + 0, MOV,REG,B   , _, { cpu.reg = cpu.b; });             \
  OP(CODE + 1, MOV,REG,C   , _, { cpu.reg = cpu.c; });             \
  OP(CODE + 2, MOV,REG,D   , _, { cpu.reg = cpu.d; });             \
  OP(CODE + 3, MOV,REG,E   , _, { cpu.reg = cpu.e; });             \
  OP(CODE + 4, MOV,REG,H   , _, { cpu.reg = cpu.h; });             \
  OP(CODE + 5, MOV,REG,L   , _, { cpu.reg = cpu.l; });             \
  OP(CODE + 6, MOV,REG,(HL), _, { cpu.reg = cpu.ram[cpu.hl]; });   \
  OP(CODE + 7, MOV,REG,A   , _, { cpu.reg = cpu.a; });             \

bool parity(u8 x) {
  bool p = 0;
  for (u8 b=0; b < 8; ++b)
    p ^= (x >> b) & 1;
  return !p;
}

void die(const char *msg) {
  perror(msg);
  exit(1);
}

#define WRITE(ADDR, V)                                                  \
  switch (ADDR) {                                                       \
  case 0x0000 ... 0x1fff:                                               \
    printf("Invalid write to ROM @%02x\n", (ADDR));                     \
    break;                                                              \
  case 0x2400 ... 0x3fff:                                               \
    printf("Write to video RAM @%02x %02x\n", (ADDR), (V));             \
    cpu.ram[(ADDR)] = (V);                                              \
    break;                                                              \
  default:                                                              \
    cpu.ram[(ADDR)] = (V);                                              \
  }

// TODO: eliminate redundancy of Register / Immediate / Memory accesses

int main(int argc, char *argv[]) {

  if (argc != 2) {
    fprintf(stderr, "Usage: spinv ROM");
    exit(1);
  }

  char *path = argv[1];

#ifdef CPUDIAG
  int orig = 0x100;
#else
  int orig = 0;
#endif

  // Init CPU
  CPU cpu;
  memset(&cpu, 0, sizeof(CPU));

  // Map ROM
  FILE *rom = fopen(path, "rb");
  if (!rom) die("Cannot open file ");
  fread(cpu.ram + orig, sizeof(u8), 0x2000, rom);
  fclose(rom);

#ifdef CPUDIAG
  // Tweak the code so we directly fail when jumping to CPUER
  cpu.ram[0x0689] = 0xfd; // CPUER;
#endif

  cpu.pc = orig;
  u32 cycles = 0;

  // Fetch and decode
  while (true) {
    u8 d8;
    u16 r, d16, addr;

    u8 *op = &cpu.ram[cpu.pc];

    switch (op[0]) {
    case 0x08: case 0x10: case 0x20:
      // TODO: show immediate values for D8, D16?
      // Or better yet, the output could show values for all arguments
      OP(0x00, NOP, _,_    , _          , {});
      OP(0x01, LXI, BC,D16 , _          , { cpu.bc = d16; });
      OP(0x04, INR, B,_    , Z|S|P|AC   , { R(cpu.b, cpu.b + 1) });
      OP(0x05, DCR, B,_    , Z|S|P|AC   , { R(cpu.b, cpu.b - 1) });
      OP(0x06, MVI, B,D8   , _          , { cpu.b = d8; });
      OP(0x09, DAD, BC,_   , CY         , { R(cpu.hl, cpu.hl + cpu.bc) });
      OP(0x0c, INR, C,_    , Z|S|P|AC   , { R(cpu.c, cpu.c + 1) });
      OP(0x0d, DCR, C,_    , Z|S|P|AC   , { R(cpu.c, cpu.c - 1) });
      OP(0x0e, MVI, C,D8   , _          , { cpu.c = d8; });
      OP(0x0f, RRC, _,_    , CYR        , { R(cpu.a, ((cpu.a&1) << 7) | (cpu.a >> 1)); });
      OP(0x11, LXI, DE,D16 , _          , { cpu.de = d16; });
      OP(0x13, INX, DE,_   , _          , { cpu.de++; });
      OP(0x14, INR, D,_    , Z|S|P|AC   , { R(cpu.d, cpu.d + 1); });
      OP(0x15, DCR, D,_    , Z|S|P|AC   , { R(cpu.d, cpu.d - 1); });
      OP(0x16, MVI, D,D8   , _          , { cpu.d = d8; });
      OP(0x19, DAD, DE,_   , CY         , { r = cpu.hl; r += cpu.de; cpu.hl = r; });
      OP(0x1a, LDAX, DE,_  , _          , { cpu.a = cpu.ram[cpu.de]; });
      OP(0x1c, INR, E,_    , Z|S|P|AC   , { R(cpu.e, cpu.e + 1); });
      OP(0x1d, DCR, E,_    , Z|S|P|AC   , { R(cpu.e, cpu.e - 1); });
      OP(0x1e, MVI, E,D8   , _          , { cpu.e = d8; });
      OP(0x1f, RAR, _,_    , CYR        , { R(cpu.a, (cpu.cy << 7) | (cpu.a >> 1)); });
      OP(0x21, LXI, HL,D16 , _          , { cpu.hl = d16; });
      OP(0x23, INX, HL,_   , _          , { cpu.hl++; });
      OP(0x24, INR, H,_    , _          , { R(cpu.h, cpu.h + 1); });
      OP(0x25, DCR, H,_    , _          , { R(cpu.h, cpu.h - 1); });
      OP(0x26, MVI, H,D8   , _          , { cpu.h = d8; });
      OP(0x29, DAD, HL,_   , CY         , { R(cpu.hl, cpu.hl+cpu.hl) });
      OP(0x2c, INR, L,_    , _          , { R(cpu.l, cpu.l + 1); });
      OP(0x2d, DCR, L,_    , _          , { R(cpu.l, cpu.l - 1); });
      OP(0x2e, MVI, L,D8   , _          , { cpu.l = d8; });
      OP(0x31, LXI, SP,D16 , _          , { cpu.sp = d16; });
      OP(0x32, STA, ADDR,_ , _          , { WRITE(addr, cpu.a); });
      OP(0x35, DCR, (HL),_ , Z|S|P|AC   , { WRITE(cpu.hl, r=cpu.ram[cpu.hl]-1); });
      OP(0x36, MVI, (HL),D8, _          , { WRITE(cpu.hl, d8); });
      OP(0x3a, LDA, ADDR,_ , _          , { cpu.a = cpu.ram[addr]; });
      OP(0x3c, INR, A,_    , Z|S|P|AC   , { R(cpu.a, cpu.a + 1); });
      OP(0x3d, DCR, A,_    , Z|S|P|AC   , { R(cpu.a, cpu.a - 1); });
      OP(0x3e, MVI, A,D8   , _          , { cpu.a = d8; });
      MOV_BLOCK(0x40, B, b);
      MOV_BLOCK(0x48, C, c);
      MOV_BLOCK(0x50, D, d);
      MOV_BLOCK(0x58, E, e);
      MOV_BLOCK(0x60, H, h);
      MOV_BLOCK(0x68, L, l);
      OP(0x77, MOV, (HL),A , _          , { WRITE(cpu.hl, cpu.a); });
      MOV_BLOCK(0x78, A, a);
      OP(0x80, ADD, B,_    , Z|S|P|CY|AC, { R(cpu.a, cpu.a + cpu.b); });
      OP(0x81, ADD, C,_    , Z|S|P|CY|AC, { R(cpu.a, cpu.a + cpu.c); });
      OP(0x82, ADD, D,_    , Z|S|P|CY|AC, { R(cpu.a, cpu.a + cpu.d); });
      OP(0xa7, ANA, A,_    , Z|S|P|CY|AC, { R(cpu.a, cpu.a & cpu.a); });
      OP(0xaf, XRA, A,_    , Z|S|P|CY|AC, { R(cpu.a, cpu.a ^ cpu.a); });
      OP(0xb2, ORA, D,_    , Z|S|P|CY|AC, { R(cpu.a, cpu.a | cpu.d); });
      OP(0xc0, RNZ, _,_    , _          , { if (!cpu.z) goto ret; });
      OP(0xc1, POP, BC,_   , _          , { cpu.bc = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xc2, JNZ, ADDR,_ , _          , { if (!cpu.z) cpu.pc = addr; });
      OP(0xc3, JMP, ADDR,_ , _          , { cpu.pc = addr; });
      OP(0xc4, CNZ, ADDR,_ , _          , { if (!cpu.z) goto call; });
      OP(0xc5, PUSH, BC,_  , _          , { cpu.ram[--cpu.sp] = cpu.b; cpu.ram[--cpu.sp] = cpu.c; });
      OP(0xc6, ADI, D8,_   , Z|S|P|CY|AC, { R(cpu.a, cpu.a + d8); });
      OP(0xc8, RZ, _,_     , _          , { if (cpu.z) goto ret; });
      OP(0xc9, RET, _,_    , _          , { ret: cpu.pc = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xca, JZ, ADDR,_  , _          , { if (cpu.z) cpu.pc = addr; });
      OP(0xcc, CZ, ADDR,_  , _          , { if (cpu.z) goto call; });
      OP(0xcd, CALL, ADDR,_, _          , { call: cpu.ram[--cpu.sp] = cpu.pc >> 8;
                                              cpu.ram[--cpu.sp] = cpu.pc;
                                              cpu.pc = addr; });
      OP(0xce, ACI, D8,_   , Z|S|P|CY|AC, { R(cpu.a, cpu.a + d8 + cpu.cy); });
      OP(0xd0, RNC, _,_    , _          , { if (!cpu.cy) goto ret; });
      OP(0xd1, POP, DE,_   , _          , { cpu.de = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xd2, JNC, ADDR,_ , _          , { if (!cpu.cy) cpu.pc = addr; });
      OP(0xd3, OUT, D8,_   , _          , { /* special */ });
      OP(0xd4, CNC, ADDR,_ , _          , { if (!cpu.cy) goto call; });
      OP(0xd5, PUSH, DE,_  , _          , { cpu.ram[--cpu.sp] = cpu.d; cpu.ram[--cpu.sp] = cpu.e; });
      OP(0xd6, SUI, D8,_   , Z|S|P|CY|AC, { R(cpu.a, cpu.a - d8); });
      OP(0xd7, RST, 2,_    , _          , { addr = 0x10; goto call; });
      OP(0xd8, RC, _,_     , _          , { if (cpu.cy) goto ret; });
      OP(0xda, JC, ADDR,_  , _          , { if (cpu.cy) cpu.pc = addr; });
      OP(0xdb, IN, D8,_    , _          , { /* special */ });
      OP(0xdc, CC, ADDR,_  , _          , { if (cpu.cy) goto call; });
      OP(0xde, SBI, D8,_   , Z|S|P|CY|AC, { R(cpu.a, cpu.a - d8 - cpu.cy); });
      OP(0xe0, RPO, _,_    , _          , { if (!cpu.p) goto ret; });
      OP(0xe1, POP, HL,_   , _          , { cpu.hl = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xe2, JPO, ADDR,_ , _          , { if (!cpu.p) cpu.pc = addr; });
      OP(0xe3, XTHL, _,_   , _          , { SWAP(cpu.h, cpu.ram[cpu.sp+1]); SWAP(cpu.l, cpu.ram[cpu.sp]); });
      OP(0xe4, CPO, ADDR,_ , _          , { if (!cpu.p) goto call; });
      OP(0xe5, PUSH, HL,_  , _          , { cpu.ram[--cpu.sp] = cpu.h; cpu.ram[--cpu.sp] = cpu.l; });
      OP(0xe6, ANI, D8,_   , Z|S|P|CY|AC, { R(cpu.a, cpu.a & d8); });
      OP(0xe8, RPE, _,_    , _          , { if (cpu.p) goto ret; });
      OP(0xea, JPE, ADDR,_ , _          , { if (cpu.p) cpu.pc = addr; });
      OP(0xeb, XCHG, _,_   , _          , { SWAP(cpu.h, cpu.d); SWAP(cpu.l, cpu.e); });
      OP(0xec, CPE, ADDR,_ , _          , { if (cpu.p) goto call; });
      OP(0xee, XRI, D8,_   , Z|S|P|CY|AC, { R(cpu.a, cpu.a ^ d8); });
      OP(0xf0, RP, _,_     , _          , { if (!cpu.s) goto ret; });
      OP(0xf1, POP, PSW,_  , _          , { cpu.flags = cpu.ram[cpu.sp++]; cpu.a = cpu.ram[cpu.sp++]; });
      OP(0xf2, JP, ADDR,_  , _          , { if (!cpu.s) cpu.pc = addr; });
      OP(0xf3, DI, _,_     , _          , { /* special */ });
      OP(0xf4, CP, ADDR,_  , _          , { if (!cpu.s) goto call; });
      OP(0xf5, PUSH, PSW,_ , _          , { cpu.ram[--cpu.sp] = cpu.a; cpu.ram[--cpu.sp] = cpu.flags; });
      OP(0xf6, ORI, D8,_   , Z|S|P|CY|AC, { R(cpu.a, cpu.a | d8); });
      OP(0xf8, RM, _,_     , _          , { if (cpu.s) goto ret; });
      OP(0xfa, JM, ADDR,_  , _          , { if (cpu.s) cpu.pc = addr; });
      OP(0xfb, EI, _,_     , _          , { /* special */ });
      OP(0xfc, CM, ADDR,_  , _          , { if (cpu.s) goto call; });
      OP(0xfe, CPI, D8,_   , Z|S|P|CY|AC, { r = cpu.a - d8; });

#ifdef CPUDIAG
      OP(0xfd, CPUER, _,_  , _          , { printf("\nCPU diag errored\n"); exit(1); });
#endif

    default:
      printf("cycle %d: unimplemented opcode: $%02x\n", cycles, op[0]);
      exit(1);
    }

    cycles++;
  }

  return EXIT_SUCCESS;
}
