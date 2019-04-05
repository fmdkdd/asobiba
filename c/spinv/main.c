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
    struct { u8 z : 1, s : 1, p : 1, cy : 1, ac: 1; };
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
  CYB = 0x20,
} FLAGS;

typedef enum {
 _, A, B, C, D, E, H, L, BC, DE, HL, SP, D8, D16, ADDR,
} ARGS;

// TODO: result is first argument of opcode.  How to obtain it in general
// without too much verbosity?  Same problem for carry: needs to know the
// arguments to compute them in u16.  Maybe use generic variables?
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
    if ((flags) & Z)  { cpu.z = r == 0; }                               \
    if ((flags) & S)  { cpu.s = r & 0x80; }                             \
    if ((flags) & CY) { cpu.cy = r > 0xff; }                            \
    if ((flags) & CYB) { cpu.cy = r & 1; }                              \
  }                                                                     \
  break


//printf(#name " %s=$%02x,%s=$%02x",
//(#arg1), OP, (#arg2), );


#define OP_ARG(arg)                                                     \
  switch (arg) {                                                        \
  case _ ... SP: break;                                                 \
  case D8  : d8   = cpu.ram[cpu.pc++]; break;                           \
  case D16 : d16  = TO16(op[2], op[1]); cpu.pc+=2; break;               \
  case ADDR: addr = TO16(op[2], op[1]); cpu.pc+=2; break;               \
  }

#define TO16(H,L) ((H) << 8 | (L))

void die(const char *msg) {
  perror(msg);
  exit(1);
}

// TODO: eliminate redundancy of Register / Immediate / Memory accesses

int main() {
  CPU cpu;

  // Zero it out
  memset(&cpu, 0, sizeof(CPU));

  // Map ROM
  FILE *rom = fopen("rom/invaders", "rb");
  if (!rom) die("Cannot open file rom/invaders");
  fread(cpu.ram, sizeof(u8), 0x2000, rom);
  fclose(rom);

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
      OP(0x00, NOP, _,_      , _       , {});
      OP(0x01, LXI, BC,D16   , _       , { cpu.bc = d16; });
      OP(0x05, DCR, B,_      , Z|S|P|AC, { r = cpu.b; r--; cpu.b = r; });
      OP(0x06, MVI, B,D8     , _       , { cpu.b = d8; });
      OP(0x09, DAD, BC,_     , CY      , { r = cpu.hl; r += cpu.bc; cpu.hl = r; });
      OP(0x0d, DCR, C,_      , Z|S|P|AC, { r = cpu.c; r--; cpu.c = r; });
      OP(0x0e, MVI, C,D8     , _       , { cpu.c = d8; });
      OP(0x11, LXI, DE,D16   , _       , { cpu.de = d16; });
      OP(0x13, INX, DE,_     , _       , { cpu.de++; });
      OP(0x19, DAD, DE,_     , CY      , { r = cpu.hl; r += cpu.de; cpu.hl = r; });
      OP(0x1a, LDAX, DE,_    , _       , { cpu.a = cpu.ram[cpu.de]; });
      OP(0x1f, RAR, _,_      , CYB     , { r = cpu.a; cpu.a = (cpu.cy << 7) | (r >> 1); });
      OP(0x21, LXI, HL,D16   , _       , { cpu.hl = d16; });
      OP(0x23, INX, HL,_     , _       , { cpu.hl++; });
      OP(0x24, INR, H,_      , _       , { cpu.h++; });
      OP(0x26, MVI, H,D8     , _       , { cpu.h = d8; });
      OP(0x29, DAD, HL,_     , CY      , { r = cpu.hl; r += cpu.hl; cpu.hl = r; });
      OP(0x31, LXI, SP,D16   , _       , { cpu.sp = d16; });
      OP(0x36, MVI, (HL),D8  , _       , { cpu.ram[cpu.hl] = d8; });
      OP(0x44, MOV, B,H      , _       , { cpu.b = cpu.h; });
      OP(0x49, MOV, C,C      , _       , { cpu.c = cpu.c; });
      OP(0x6f, MOV, L,A      , _       , { cpu.l = cpu.a; });
      OP(0x77, MOV, (HL),A   , _       , { cpu.ram[cpu.hl] = cpu.a; });
      OP(0x7c, MOV, A,H      , _       , { cpu.a = cpu.h; });
      OP(0xc2, JNZ, ADDR,_   , _       , { if (!cpu.z) { cpu.pc = addr; }; });
      OP(0xc1, POP, BC,_     , _       , { cpu.bc = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xc3, JMP, ADDR,_   , _       , { cpu.pc = addr; });
      OP(0xc5, PUSH, B,_     , _       , { cpu.ram[--cpu.sp] = cpu.b;
                                           cpu.ram[--cpu.sp] = cpu.c; });
      OP(0xc9, RET, _,_      , _       , { cpu.pc = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xcd, CALL, ADDR,_  , _       , { call: cpu.ram[--cpu.sp] = cpu.pc >> 8;
                                           cpu.ram[--cpu.sp] = cpu.pc;
                                           cpu.pc = addr; });
      OP(0xd1, POP, DE,_      , _      , { cpu.de = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xd3, OUT, D8,_     , _       , { /* special */ });
      OP(0xd5, PUSH, D,_     , _       , { cpu.ram[--cpu.sp] = cpu.d;
                                           cpu.ram[--cpu.sp] = cpu.e; });
      OP(0xe1, POP, HL,_     , _       , { cpu.hl = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP(0xe5, PUSH, H,_     , _       , { cpu.ram[--cpu.sp] = cpu.h;
                                           cpu.ram[--cpu.sp] = cpu.l; });
      OP(0xeb, XCHG, _,_     , _       , { u8 t = cpu.h; cpu.h = cpu.d; cpu.d = t;
                                              t = cpu.l; cpu.l = cpu.e; cpu.e = t; });
      OP(0xf3, DI, _,_       , _       , { /* special */ });
      OP(0xfc, CM, ADDR,_    , _       , { if (!cpu.s) goto call; });
      OP(0xfe, CPI, D8,_     , Z|S|P|CY|AC, { r = cpu.a - d8; });


    default:
      printf("cycle %d: unimplemented opcode: $%02x\n", cycles, op[0]);
      exit(1);
    }

    cycles++;
  }


  return EXIT_SUCCESS;
}
