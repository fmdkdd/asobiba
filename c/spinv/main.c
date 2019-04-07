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

// TODO: other flags

#define OP(code, name, arg1, arg2, flags, expr)                         \
  case (code): {                                                        \
    u16 old_pc = cpu.pc;                                                \
    printf("%04x %02x ", cpu.pc++, op[0]);                              \
    OP_ARG(arg1);                                                       \
    OP_ARG(arg2);                                                       \
    if (cpu.pc - old_pc > 1) printf("%02x ", op[1]); else printf("   "); \
    if (cpu.pc - old_pc > 2) printf("%02x ", op[2]); else printf("   "); \
    printf(#name);                                                      \
    if ((arg1) != _) printf(" %s", #arg1);                              \
    if ((arg2) != _) printf(",%s", #arg2);                              \
    printf("\n");                                                       \
    expr;                                                               \
    if ((flags) & Z)   { cpu.z = r == 0; }                              \
    if ((flags) & S)   { cpu.s = (r >> 7) & 1; }                        \
    if ((flags) & P)   { cpu.p = parity(r); }                           \
    if ((flags) & CY)  { cpu.cy = r > 0xff; }                           \
    if ((flags) & CYR) { cpu.cy = r & 1; }                              \
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

// TODO: eliminate redundancy of Register / Immediate / Memory accesses

int main(int argc, char *argv[]) {

  if (argc != 3) {
    fprintf(stderr, "Usage: spinv ROM ORIG");
    exit(1);
  }

  char *path = argv[1];
  int orig = strtoul(argv[2], NULL, 0);

  // Init CPU
  CPU cpu;
  memset(&cpu, 0, sizeof(CPU));

  // Map ROM
  FILE *rom = fopen(path, "rb");
  if (!rom) die("Cannot open file ");
  fread(cpu.ram + orig, sizeof(u8), 0x2000, rom);
  fclose(rom);

  // @Temp: should be behind a flag
  // Tweak the code so we directly fail when jumping to CPUER
  cpu.ram[0x0689] = 0xfd; // CPUER;

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
      // (registers included) as well as before/after for relevant registers
      OP(0x00, NOP, _,_    , _          , {});
      OP(0x01, LXI, BC,D16 , _          , { cpu.bc = d16; });
      OP(0x05, DCR, B,_    , Z|S|P|AC   , { r = cpu.b; r--; cpu.b = r; });
      OP(0x06, MVI, B,D8   , _          , { cpu.b = d8; });
      OP(0x09, DAD, BC,_   , CY         , { r = cpu.hl; r += cpu.bc; cpu.hl = r; });
      OP(0x0d, DCR, C,_    , Z|S|P|AC   , { r = cpu.c; r--; cpu.c = r; });
      OP(0x0e, MVI, C,D8   , _          , { cpu.c = d8; });
      OP(0x0f, RRC, _,_    , CYR        , { r = cpu.a; cpu.a = ((r&1) << 7) | (r >> 1); });
      OP(0x11, LXI, DE,D16 , _          , { cpu.de = d16; });
      OP(0x13, INX, DE,_   , _          , { cpu.de++; });
      OP(0x19, DAD, DE,_   , CY         , { r = cpu.hl; r += cpu.de; cpu.hl = r; });
      OP(0x1a, LDAX, DE,_  , _          , { cpu.a = cpu.ram[cpu.de]; });
      OP(0x1f, RAR, _,_    , CYR        , { r = cpu.a; cpu.a = (cpu.cy << 7) | (r >> 1); });
      OP(0x21, LXI, HL,D16 , _          , { cpu.hl = d16; });
      OP(0x23, INX, HL,_   , _          , { cpu.hl++; });
      OP(0x24, INR, H,_    , _          , { cpu.h++; });
      OP(0x26, MVI, H,D8   , _          , { cpu.h = d8; });
      OP(0x29, DAD, HL,_   , CY         , { r = cpu.hl; r += cpu.hl; cpu.hl = r; });
      OP(0x31, LXI, SP,D16 , _          , { cpu.sp = d16; });
      OP(0x36, MVI, (HL),D8, _          , { cpu.ram[cpu.hl] = d8; });
      OP(0x44, MOV, B,H    , _          , { cpu.b = cpu.h; });
      OP(0x49, MOV, C,C    , _          , { cpu.c = cpu.c; });
      OP(0x5f, MOV, E,A    , _          , { cpu.e = cpu.a; });
      OP(0x6f, MOV, L,A    , _          , { cpu.l = cpu.a; });
      OP(0x77, MOV, (HL),A , _          , { cpu.ram[cpu.hl] = cpu.a; });
      OP(0x7c, MOV, A,H    , _          , { cpu.a = cpu.h; });
      OP(0x7d, MOV, A,L    , _          , { cpu.a = cpu.l; });
      OP(0xc2, JNZ, ADDR,_ , _          , { if (!cpu.z) { cpu.pc = addr; }; });
      OP(0xc1, POP, BC,_   , _          , { cpu.bc = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xc3, JMP, ADDR,_ , _          , { cpu.pc = addr; });
      OP(0xc5, PUSH, BC,_  , _          , { cpu.ram[--cpu.sp] = cpu.b; cpu.ram[--cpu.sp] = cpu.c; });
      OP(0xc6, ADI, D8,_   , Z|S|P|CY|AC, { r = cpu.a; r += d8; cpu.a = r; });
      OP(0xc9, RET, _,_    , _          , { cpu.pc = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xca, JZ, ADDR,_  , _          , { if (cpu.z) cpu.pc = addr; ; });
      OP(0xcd, CALL, ADDR,_, _          , { call: cpu.ram[--cpu.sp] = cpu.pc >> 8;
                                              cpu.ram[--cpu.sp] = cpu.pc;
                                              cpu.pc = addr; });
      OP(0xd1, POP, DE,_   , _          , { cpu.de = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xd2, JNC, ADDR,_ , _          , { if (!cpu.cy) cpu.pc = addr; });
      OP(0xd3, OUT, D8,_   , _          , { /* special */ });
      OP(0xd5, PUSH, DE,_  , _          , { cpu.ram[--cpu.sp] = cpu.d; cpu.ram[--cpu.sp] = cpu.e; });
      OP(0xe1, POP, HL,_   , _          , { cpu.hl = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xe3, XTHL, _,_   , _          , { SWAP(cpu.h, cpu.ram[cpu.sp+1]); SWAP(cpu.l, cpu.ram[cpu.sp]); });
      OP(0xe5, PUSH, HL,_  , _          , { cpu.ram[--cpu.sp] = cpu.h; cpu.ram[--cpu.sp] = cpu.l; });
      OP(0xe6, ANI, D8,_   , Z|S|P|CY|AC, { r = cpu.a & d8; });
      OP(0xea, JPE, ADDR,_ , _          , { if (cpu.p) cpu.pc = addr; });
      OP(0xeb, XCHG, _,_   , _          , { SWAP(cpu.h, cpu.d); SWAP(cpu.l, cpu.e); });
      OP(0xf1, POP, PSW,_  , _          , { cpu.flags = cpu.ram[cpu.sp++]; cpu.a = cpu.ram[cpu.sp++]; });
      OP(0xf2, JP, ADDR,_  , _          , { if (cpu.s) cpu.pc = addr; });
      OP(0xf3, DI, _,_     , _          , { /* special */ });
      OP(0xf5, PUSH, PSW,_ , _          , { cpu.ram[--cpu.sp] = cpu.a; cpu.ram[--cpu.sp] = cpu.flags; });
      OP(0xfa, JM, ADDR,_  , _          , { if (!cpu.s) cpu.pc = addr; });
      OP(0xfc, CM, ADDR,_  , _          , { if (!cpu.s) goto call; });
      OP(0xfd, CPUER, _,_  , _          , { printf("CPU diag errored\n"); exit(1); });
      OP(0xfe, CPI, D8,_   , Z|S|P|CY|AC, { r = cpu.a - d8; });


    default:
      printf("cycle %d: unimplemented opcode: $%02x\n", cycles, op[0]);
      exit(1);
    }

    cycles++;
  }


  return EXIT_SUCCESS;
}
