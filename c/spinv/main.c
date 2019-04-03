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
  Z  = 0x01,
  S  = 0x02,
  P  = 0x04,
  CY = 0x08,
  AC = 0x10,
} FLAGS;

typedef enum {
  _, A, B, C, D, E, BC, DE, HL, SP, D8, D16, ADDR,
} ARGS;

// TODO: result is first argument of opcode.  How to obtain it in general
// without too much verbosity?  Same problem for carry: needs to know the
// arguments to compute them in u16.  Maybe use generic variables?
// TODO: other flags

#define OP(code, name, size, flags, expr)                               \
  case (code): {                                                        \
    printf("%04x " name "\n", cpu.pc);                                  \
    expr;                                                               \
    if ((flags) & Z) { cpu.z = r == 0 ? 1 : 0; }                        \
    cpu.pc += (size);                                                   \
  }                                                                     \
  break

#define OP2(code, name, arg1, arg2, flags, expr)                        \
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
    if ((flags) & Z) { cpu.z = r == 0 ? 1 : 0; }                        \
  }                                                                     \
  break


//printf(#name " %s=$%02x,%s=$%02x",
//(#arg1), OP, (#arg2), );


#define OP_ARG(arg)                                             \
  switch (arg) {                                                \
  case _: case A: case B: case DE: case HL: case SP: break;     \
  case D8  : d8   = cpu.ram[cpu.pc++]; break;                   \
  case D16 : d16  = TO16(op[2], op[1]); cpu.pc+=2; break;       \
  case ADDR: addr = TO16(op[2], op[1]); cpu.pc+=2; break;       \
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

  // Fetch and decode
  while (true) {
    u8 r, d8;
    u16 d16, addr;

    u8 *op = &cpu.ram[cpu.pc];

    switch (op[0]) {
    case 0x08: case 0x10: case 0x20:
      // TODO: show immediate values for D8, D16?
      // Or better yet, the output could show values for all arguments
      // (registers included) as well as before/after for relevant registers
      OP2(0x00, NOP, _,_      , _       , {});
      OP2(0x05, DCR, B,_      , Z|S|P|AC, { r = --cpu.b; });
      OP2(0x06, MVI, B,D8     , _       , { cpu.b = d8; });
      OP2(0x11, LXI, DE,D16   , _       , { cpu.de = d16; });
      OP2(0x13, INX, DE,_     , _       , { cpu.de++; });
      OP2(0x1a, LDAX, DE,_    , _       , { cpu.a = cpu.ram[cpu.de]; });
      OP2(0x21, LXI, HL,D16   , _       , { cpu.hl = d16; });
      OP2(0x23, INX, HL,_     , _       , { cpu.hl++; });
      OP2(0x31, LXI, SP,D16   , _       , { cpu.sp = d16; });
      OP2(0x77, MOV, (HL),A   , _       , { cpu.ram[cpu.hl] = cpu.a; });
      OP2(0xc2, JNZ, ADDR,_   , _       , { if (!cpu.z) { cpu.pc = addr; }; });
      OP2(0xc3, JMP, ADDR,_   , _       , { cpu.pc = addr; });
      OP2(0xc9, RET, _,_      , _       , { cpu.pc = TO16(cpu.ram[cpu.sp+1], cpu.ram[cpu.sp]); cpu.sp+= 2; });
      OP2(0xcd, CALL, ADDR,_  , _       , { cpu.ram[--cpu.sp] = cpu.pc >> 8;
                                            cpu.ram[--cpu.sp] = cpu.pc;
                                            cpu.pc = addr; });


    default:
      printf("unimplemented opcode: $%02x\n", op[0]);
      exit(1);
    }
  }


  return EXIT_SUCCESS;
}
