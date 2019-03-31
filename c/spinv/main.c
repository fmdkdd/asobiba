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
  u16 pc, sp;
  u8 ram[RAM_SIZE];
} CPU;

// TODO: flags, but better yet reduce the redundancy with macros

int main() {

  CPU cpu;

  // Zero it out
  memset(&cpu, 0, sizeof(CPU));

  // Map ROM
  FILE *rom = fopen("rom/invaders", "rb");
  fread(cpu.ram, sizeof(u8), 0x2000, rom);
  fclose(rom);

  // Fetch and decode
  while (true) {
    u8 *op = &cpu.ram[cpu.pc];

    printf("%04x ", cpu.pc);

    switch (op[0]) {
      // NOP
    case 0x00: case 0x08: case 0x10: case 0x20: {
      printf("NOP\n");
      cpu.pc++;
      break;
    }

      // DCR B
    case 0x05: {
      cpu.b--;
      printf("DCR B\n");
      cpu.pc++;
      break;
    }

      // MVI B, D8
    case 0x06: {
      cpu.b = op[1];
      printf("MVI B, $%02x\n", cpu.b);
      cpu.pc += 2;
      break;
    }

      // LXI DE, D16
    case 0x11: {
      printf("LXI DE, $%02x%02x\n", op[2], op[1]);
      cpu.de = op[2] << 8 | op[1];
      cpu.pc += 3;
      break;
    }

      // INX DE
    case 0x13: {
      printf("INX DE\n");
      cpu.de++;
      cpu.pc++;
      break;
    }

    case 0x1a: {
      printf("LDAX DE\n");
      cpu.a = cpu.ram[cpu.de];
      cpu.pc++;
      break;
    }

      // LXI HL, D16
    case 0x21: {
      printf("LXI HL, $%02x%02x\n", op[2], op[1]);
      cpu.hl = op[2] << 8 | op[1];
      cpu.pc += 3;
      break;
    }

      // INX H
    case 0x23: {
      cpu.hl++;
      cpu.pc++;
      break;
    }

      // LXI SP, D16
    case 0x31: {
      cpu.sp = op[2] << 8 | op[1];
      printf("LXI SP, $%04x\n", cpu.sp);
      cpu.pc += 3;
      break;
    }

      // JMP
    case 0xc3: {
      u16 addr = op[2] << 8 | op[1];
      printf("JMP $%04x\n", addr);
      cpu.pc = addr;
      break;
    }

      // MOV (HL), A
    case 0x77: {
      printf("MOV (HL), A\n");
      cpu.ram[cpu.hl] = cpu.a;
      cpu.pc++;
      break;
    }

      // CALL addr
    case 0xcd: {
      u16 addr = op[2] << 8 | op[1];
      printf("CALL $%04x\n", addr);
      cpu.ram[--cpu.sp] = cpu.pc >> 8;
      cpu.ram[--cpu.sp] = cpu.pc;
      cpu.pc = addr;
      break;
    }

    /*   // RET */
    /* case 0xc9: { */
    /*   printf("RET\n"); */
    /*   cpu.pc = cpu.ram[cpu.sp++]; */
    /*   cpu.pc = cpu.ram[cpu.sp++]; */
    /*   break; */
    /* } */

    default:
      printf("unimplemented opcode: $%02x\n", op[0]);
      exit(1);
    }
  }


  return EXIT_SUCCESS;
}
