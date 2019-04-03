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
  break;

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
    u8 r = 0;

    u8 *op = &cpu.ram[cpu.pc];

    switch (op[0]) {
    case 0x08: case 0x10: case 0x20:
      // TODO: show immediate values for D8, D16?
      // Or better yet, the output could show values for all arguments
      // (registers included) as well as before/after for relevant registers
      // TODO: opcode size should be determined by pattern (D8 = +1, D16 = +2, addr = +2)
      OP(0x00, "NOP"       , 1, 0       , {});
      OP(0x05, "DCR B"     , 1, Z|S|P|AC, { r = cpu.b--; });
      OP(0x06, "MVI B,D8"  , 2, 0       , { cpu.b = op[1]; });
      OP(0x11, "LXI DE,D16", 3, 0       , { cpu.de = TO16(op[2], op[1]); });
      OP(0x13, "INX DE"    , 1, 0       , { cpu.de++; });
      OP(0x1a, "LDAX DE"   , 1, 0       , { cpu.a = cpu.ram[cpu.de]; });
      OP(0x21, "LXI HL,D16", 3, 0       , { cpu.hl = TO16(op[2], op[1]); });
      OP(0x23, "INX HL"    , 1, 0       , { cpu.hl++; });
      OP(0x31, "LXI SP,D16", 3, 0       , { cpu.sp = TO16(op[2], op[1]); });
      OP(0x77, "MOV (HL),A", 1, 0       , { cpu.ram[cpu.hl] = cpu.a; });
      // TODO: can we extract the variables from the string?
      OP(0xc2, "JNZ addr"  , 3, 0       , { u16 addr = TO16(op[2], op[1]); if (!cpu.z) { cpu.pc = addr-3; }; });
      OP(0xc3, "JMP addr"  , 0, 0       , { u16 addr = TO16(op[2], op[1]); cpu.pc = addr; });
      // TODO: not sure about the order of args to TO16 here
      OP(0xc9, "RET"       , 0, 0       , { cpu.pc = TO16(cpu.ram[cpu.sp++], cpu.ram[cpu.sp++]); });
      OP(0xcd, "CALL addr" , 0, 0       , { u16 addr = TO16(op[2], op[1]);
                                            cpu.ram[--cpu.sp] = cpu.pc >> 8;
                                            cpu.ram[--cpu.sp] = cpu.pc;
                                            cpu.pc = addr; });


    default:
      printf("unimplemented opcode: $%02x\n", op[0]);
      exit(1);
    }
  }


  return EXIT_SUCCESS;
}
