#define _POSIX_C_SOURCE 200809L

#include <stdlib.h>
#include <sys/mman.h>

#include "common.h"
#include "cpu.h"
#include "jit.h"

#define PAGE_SIZE 4096

// Compile block at ADDR and return a function to it.
static jit_function jit_compile(int addr, const CPU *cpu) {
  // Decode and translate until we jump

  const u8 *op = &cpu->ram[addr];

  u8 code[100];
  // fill with RET
  memset(code, 0xc3, sizeof(code));
  u8 *cp = code;

  // TODO: how to update PC/CPU state in compiled instructions?
  // TODO: count cycles

  bool done = false;
  while (!done) {
    switch (op[0]) {
    case 0x00: case 0x08: case 0x10: case 0x20:
      *cp++ = 0x90; // NOP
      break;

      // break on jumps
    case 0xc0: case 0xc1: case 0xc2: case 0xc3: case 0xc4: case 0xc5:
    case 0xc8: case 0xc9: case 0xca: case 0xcc: case 0xcd:
    case 0xd0: case 0xd2: case 0xd4: case 0xd7: case 0xd8:
    case 0xda: case 0xdc: case 0xe0: case 0xe2: case 0xe4:
    case 0xe8: case 0xea: case 0xec: case 0xf2: case 0xf4:
    case 0xf8: case 0xfa: case 0xfc:
      done = true;
      break;

    default:
      *cp++ = 0x90; // NOP
    }

    op++;
  }

  // malloc an aligned code page
  void *page;
  posix_memalign(&page, PAGE_SIZE, PAGE_SIZE);
  // fill with RET
  memset(page, 0xc3, PAGE_SIZE);
  // copy the code
  memcpy(page, code, sizeof(code));
  // turn the exec bit
  mprotect(page, PAGE_SIZE, PROT_READ | PROT_EXEC);
  // return a function pointer to it
  return (jit_function) page;
}

// Return the JITted block for ADDR, and compile it if necessary.
#if 0
static jit_function jit_fetch(const CPU *cpu) {
  int pc = cpu->pc;
  jit_function_cache *f = &jit_cache[pc % 100];

  if (!f->valid || f->address != pc) {
    printf("recompiling function at 0x%04x\n", pc);
    f->function = jit_compile(pc, cpu);
    f->address = pc;
    f->valid = true;
  }

  return f->function;
}
#endif

void jit_init(Jit *jit, CPU *cpu) {
  jit->cpu = cpu;
  jit->remaining_cycles = 0;
  memset(jit->exec_hits, 0, sizeof(jit->exec_hits));
  memset(jit->hot_routines_marked, 0, sizeof(jit->hot_routines_marked));
  memset(jit->hot_routines, 0, sizeof(jit->hot_routines));
  jit->hot_routines_size = 0;

  //memset(jit->jit_cache, 0, sizeof(jit->function_cache));
}

#define OP(code, name, arg1, arg2, cycles, flags, expr)                 \
  case (code): {                                                        \
  cc += (cycles);                                                       \
  u16 old_pc = pc;                                                      \
  printf("%04x %02x ", pc, op[0]);                                      \
  pc++;                                                                 \
  OP_ARG(arg1);                                                         \
  OP_ARG(arg2);                                                         \
  if (pc - old_pc > 1) printf("%02x ", op[1]); else printf("   ");      \
  if (pc - old_pc > 2) printf("%02x ", op[2]); else printf("   ");      \
  print_mnemonic(#name, #arg1, #arg2);                                  \
  expr;                                                                 \
  printf("\n");                                                         \
  }                                                                     \
  break

#define OP_ARG(arg)                                                     \
  switch (arg) {                                                        \
  case _: case A:  case B: case C: case D: case E: case H:              \
  case L: case BC: case DE: case HL: case SP: case PSW: break;          \
  case D8  : pc++; break;                                               \
  case D16 : pc+=2; break;                                              \
  case ADDR: pc+=2; break;                                              \
  }

#define MOV_BLOCK(code, REG, reg)                                       \
  OP(code + 0, MOV,REG,B   , 5, _, {});                                 \
  OP(code + 1, MOV,REG,C   , 5, _, {});                                 \
  OP(code + 2, MOV,REG,D   , 5, _, {});                                 \
  OP(code + 3, MOV,REG,E   , 5, _, {});                                 \
  OP(code + 4, MOV,REG,H   , 5, _, {});                                 \
  OP(code + 5, MOV,REG,L   , 5, _, {});                                 \
  OP(code + 6, MOV,REG,(HL), 7, _, {});                                 \
  OP(code + 7, MOV,REG,A   , 5, _, {});

#define ARITH_BLOCK(code, name, op)                                     \
  OP(code + 0, name,A,B   , 4, Z|S|P|CY|AC, {});                        \
  OP(code + 1, name,A,C   , 4, Z|S|P|CY|AC, {});                        \
  OP(code + 2, name,A,D   , 4, Z|S|P|CY|AC, {});                        \
  OP(code + 3, name,A,E   , 4, Z|S|P|CY|AC, {});                        \
  OP(code + 4, name,A,H   , 4, Z|S|P|CY|AC, {});                        \
  OP(code + 5, name,A,L   , 4, Z|S|P|CY|AC, {});                        \
  OP(code + 6, name,A,(HL), 4, Z|S|P|CY|AC, {});                        \
  OP(code + 7, name,A,A   , 4, Z|S|P|CY|AC, {});


// Disassemble the basic block starting at ADDR
static void jit_disassemble(int addr, const CPU *cpu) {
  // Decode instructions until a jump

  printf("disassembling 0x%04x\n", addr);

  u16 pc = addr;
  u8 cc = 0;
  bool done = false;
  while (!done) {
    const u8 *op = &cpu->ram[pc];

    // TODO: this is just a copy/paste from cpu.c, could be better

    switch (op[0]) {
    case 0x08: case 0x10: case 0x20:
      OP(0x00, NOP, _,_    , 4, _          , {});
      OP(0x01, LXI, BC,D16 ,10, _          , {});
      OP(0x02, STAX,BC,_   , 7, _          , {});
      OP(0x03, INX, BC,_   , 5, _          , {});
      OP(0x04, INR, B,_    , 5, Z|S|P|AC   , {});
      OP(0x05, DCR, B,_    , 5, Z|S|P|AC   , {});
      OP(0x06, MVI, B,D8   , 7, _          , {});
      OP(0x09, DAD, BC,_   ,10, CY         , {});
      OP(0x0a, LDAX,BC,_   , 7, _          , {});
      OP(0x0b, DCX, BC,_   , 5, _          , {});
      OP(0x0c, INR, C,_    , 5, Z|S|P|AC   , {});
      OP(0x0d, DCR, C,_    , 5, Z|S|P|AC   , {});
      OP(0x0e, MVI, C,D8   , 7, _          , {});
      OP(0x0f, RRC, _,_    , 4, CYR        , {});
      OP(0x11, LXI, DE,D16 ,10, _          , {});
      OP(0x12, STAX,DE,_   , 7, _          , {});
      OP(0x13, INX, DE,_   , 5, _          , {});
      OP(0x14, INR, D,_    , 5, Z|S|P|AC   , {});
      OP(0x15, DCR, D,_    , 5, Z|S|P|AC   , {});
      OP(0x16, MVI, D,D8   , 7, _          , {});
      OP(0x19, DAD, DE,_   ,10, CY         , {});
      OP(0x1a, LDAX,DE,_   , 7, _          , {});
      OP(0x1b, DCX, DE,_   , 5, _          , {});
      OP(0x1c, INR, E,_    , 5, Z|S|P|AC   , {});
      OP(0x1d, DCR, E,_    , 5, Z|S|P|AC   , {});
      OP(0x1e, MVI, E,D8   , 7, _          , {});
      OP(0x1f, RAR, _,_    , 4, CYR        , {});
      OP(0x21, LXI, HL,D16 ,10, _          , {});
      OP(0x22, SHLD,ADDR,_ ,16, _          , {});
      OP(0x23, INX, HL,_   , 5, _          , {});
      OP(0x24, INR, H,_    , 5, _          , {});
      OP(0x25, DCR, H,_    , 5, _          , {});
      OP(0x26, MVI, H,D8   , 7, _          , {});
      OP(0x27, DAA, _,_    , 4, _          , {});
      OP(0x29, DAD, HL,_   ,10, CY         , {});
      OP(0x2a, LHLD,ADDR,_ ,16, _          , {});
      OP(0x2b, DCX, HL,_   , 5, _          , {});
      OP(0x2c, INR, L,_    , 5, _          , {});
      OP(0x2d, DCR, L,_    , 5, _          , {});
      OP(0x2e, MVI, L,D8   , 7, _          , {});
      OP(0x2f, CMA, _,_    , 4, _          , {});
      OP(0x31, LXI, SP,D16 ,10, _          , {});
      OP(0x32, STA, ADDR,_ ,13, _          , {});
      OP(0x34, INR, (HL),_ ,10, Z|S|P|AC   , {});
      OP(0x35, DCR, (HL),_ ,10, Z|S|P|AC   , {});
      OP(0x36, MVI, (HL),D8,10, _          , {});
      OP(0x37, STC, _,_    , 4, _          , {});
      OP(0x3a, LDA, ADDR,_ ,13, _          , {});
      OP(0x3c, INR, A,_    , 5, Z|S|P|AC   , {});
      OP(0x3d, DCR, A,_    , 5, Z|S|P|AC   , {});
      OP(0x3e, MVI, A,D8   , 7, _          , {});
      OP(0x3f, CMC, _,_    , 4, _          , {});
      MOV_BLOCK(0x40, B, b);
      MOV_BLOCK(0x48, C, c);
      MOV_BLOCK(0x50, D, d);
      MOV_BLOCK(0x58, E, e);
      MOV_BLOCK(0x60, H, h);
      MOV_BLOCK(0x68, L, l);
      OP(0x70, MOV, (HL),B , 7, _          , {});
      OP(0x71, MOV, (HL),C , 7, _          , {});
      OP(0x72, MOV, (HL),D , 7, _          , {});
      OP(0x73, MOV, (HL),E , 7, _          , {});
      OP(0x74, MOV, (HL),H , 7, _          , {});
      OP(0x75, MOV, (HL),L , 7, _          , {});
      OP(0x77, MOV, (HL),A , 7, _          , {});
      MOV_BLOCK(0x78, A, a);
      ARITH_BLOCK(0x80, ADD, +);
      ARITH_BLOCK(0x88, ADC, + cpu->cy +);
      ARITH_BLOCK(0x90, SUB, -);
      ARITH_BLOCK(0x98, SBB, - cpu->cy -);
      ARITH_BLOCK(0xa0, ANA, &);
      ARITH_BLOCK(0xa8, XRA, ^);
      ARITH_BLOCK(0xb0, ORA, |);
      OP(0xb8, CMP, B,_    , 4, C|S|P|CY|AC, {});
      OP(0xb9, CMP, C,_    , 4, C|S|P|CY|AC, {});
      OP(0xba, CMP, D,_    , 4, C|S|P|CY|AC, {});
      OP(0xbb, CMP, E,_    , 4, C|S|P|CY|AC, {});
      OP(0xbc, CMP, H,_    , 4, C|S|P|CY|AC, {});
      OP(0xbd, CMP, L,_    , 4, C|S|P|CY|AC, {});
      OP(0xbe, CMP, (HL),_ , 7, C|S|P|CY|AC, {});
      OP(0xbf, CMP, A,_    , 4, C|S|P|CY|AC, {});
      OP(0xc0, RNZ, _,_    , 5, _          , {});
      OP(0xc1, POP, BC,_   ,10, _          , {});
      OP(0xc2, JNZ, ADDR,_ ,10, _          , {});
      OP(0xc3, JMP, ADDR,_ ,10, _          , { done = true; });
      OP(0xc4, CNZ, ADDR,_ ,11, _          , {});
      OP(0xc5, PUSH, BC,_  ,11, _          , {});
      OP(0xc6, ADI, D8,_   , 7, Z|S|P|CY|AC, {});
      OP(0xc8, RZ, _,_     , 5, _          , {});
      OP(0xc9, RET, _,_    ,10, _          , { done = true; });
      OP(0xca, JZ, ADDR,_  ,10, _          , {});
      OP(0xcc, CZ, ADDR,_  ,11, _          , {});
      OP(0xcd, CALL, ADDR,_,17, _          , {});
      OP(0xce, ACI, D8,_   , 7, Z|S|P|CY|AC, {});
      OP(0xd0, RNC, _,_    , 5, _          , {});
      OP(0xd1, POP, DE,_   ,10, _          , {});
      OP(0xd2, JNC, ADDR,_ ,10, _          , {});
      OP(0xd3, OUT, D8,_   ,10, _          , {});
      OP(0xd4, CNC, ADDR,_ ,11, _          , {});
      OP(0xd5, PUSH, DE,_  ,11, _          , {});
      OP(0xd6, SUI, D8,_   , 7, Z|S|P|CY|AC, {});
      OP(0xd7, RST, 2,_    ,11, _          , {});
      OP(0xd8, RC, _,_     , 5, _          , {});
      OP(0xda, JC, ADDR,_  ,10, _          , {});
      OP(0xdb, IN, D8,_    ,10, _          , {});
      OP(0xdc, CC, ADDR,_  ,11, _          , {});
      OP(0xde, SBI, D8,_   , 7, Z|S|P|CY|AC, {});
      OP(0xe0, RPO, _,_    , 5, _          , {});
      OP(0xe1, POP, HL,_   ,10, _          , {});
      OP(0xe2, JPO, ADDR,_ ,10, _          , {});
      OP(0xe3, XTHL, _,_   ,18, _          , {});
      OP(0xe4, CPO, ADDR,_ ,11, _          , {});
      OP(0xe5, PUSH, HL,_  ,11, _          , {});
      OP(0xe6, ANI, D8,_   , 7, Z|S|P|CY|AC, {});
      OP(0xe8, RPE, _,_    , 5, _          , {});
      OP(0xea, JPE, ADDR,_ ,10, _          , {});
      OP(0xeb, XCHG, _,_   , 4, _          , {});
      OP(0xec, CPE, ADDR,_ ,11, _          , {});
      OP(0xee, XRI, D8,_   , 7, Z|S|P|CY|AC, {});
      OP(0xf0, RP, _,_     , 5, _          , {});
      OP(0xf1, POP, PSW,_  ,10, _          , {});
      OP(0xf2, JP, ADDR,_  ,10, _          , {});
      OP(0xf3, DI, _,_     , 4, _          , {});
      OP(0xf4, CP, ADDR,_  ,11, _          , {});
      OP(0xf5, PUSH, PSW,_ ,11, _          , {});
      OP(0xf6, ORI, D8,_   , 7, Z|S|P|CY|AC, {});
      OP(0xf8, RM, _,_     , 5, _          , {});
      OP(0xfa, JM, ADDR,_  ,10, _          , {});
      OP(0xfb, EI, _,_     , 4, _          , {});
      OP(0xfc, CM, ADDR,_  ,11, _          , {});
      OP(0xfe, CPI, D8,_   , 7, Z|S|P|CY|AC, {});

    default:
      printf("unimplemented opcode: $%02x\n", op[0]);
      exit(1);
    }
  }

  printf("disassembly total cycles: %u\n", cc);
}

// Gather addresses of hot routines, dump the disassembled code at program exit

void jit_dump_hot_routines(Jit *jit) {
  for (size_t i=0; i < jit->hot_routines_size; ++i) {
    u16 addr = jit->hot_routines[i];
    printf("hot routine at 0x%0x: %u\n", addr, jit->exec_hits[addr]);
    jit_disassemble(addr, jit->cpu);
  }
}

#define HOT_ROUTINE_HIT_THRESHOLD 1000

static int unfolded_xra_aa(CPU *cpu) {
  cpu->pc++;
  cpu->a = 0;
  cpu->z = 1;
  cpu->s = 0;
  cpu->p = 1;
  cpu->cy = 0;
  cpu->ac = 0;
  return 4;
}

static jit_function jitted_xra_aa = NULL;

static void compile_xra_aa() {
  u8 xra_aa_bytes[] = { 0x55,
                        0x48, 0x89, 0xe5,
                        0x48, 0x89, 0x7d, 0xf8,
                        0x48, 0x8b, 0x45, 0xf8,
                        0x0f, 0xb7, 0x00,
                        0x8d, 0x50, 0x01,
                        0x48, 0x8b, 0x45, 0xf8,
                        0x66, 0x89, 0x10,
                        0x48, 0x8b, 0x45, 0xf8,
                        0xc6, 0x40, 0x04, 0x00,
                        0x48, 0x8b, 0x45, 0xf8,
                        0x0f, 0xb6, 0x50, 0x0c,
                        0x83, 0xca, 0x01,
                        0x88, 0x50, 0x0c,
                        0x48, 0x8b, 0x45, 0xf8,
                        0x0f, 0xb6, 0x50, 0x0c,
                        0x83, 0xe2, 0xfd,
                        0x88, 0x50, 0x0c,
                        0x48, 0x8b, 0x45, 0xf8,
                        0x0f, 0xb6, 0x50, 0x0c,
                        0x83, 0xca, 0x04,
                        0x88, 0x50, 0x0c,
                        0x48, 0x8b, 0x45, 0xf8,
                        0x0f, 0xb6, 0x50, 0x0c,
                        0x83, 0xe2, 0xf7,
                        0x88, 0x50, 0x0c,
                        0x48, 0x8b, 0x45, 0xf8,
                        0x0f, 0xb6, 0x50, 0x0c,
                        0x83, 0xe2, 0xef,
                        0x88, 0x50, 0x0c,
                        0xb8, 0x04, 0x00, 0x00, 0x00,
                        0x5d,
                        0xc3
  };

  {
    // malloc an aligned code page
    void *page;
    posix_memalign(&page, PAGE_SIZE, PAGE_SIZE);
    // fill with RET
    memset(page, 0xc3, PAGE_SIZE);
    // copy the code
    memcpy(page, xra_aa_bytes, sizeof(xra_aa_bytes));
    // turn the exec bit
    mprotect(page, PAGE_SIZE, PROT_READ | PROT_EXEC);
    jitted_xra_aa = page;
  }
}

// Look up in the JIT cache, and execute the result.
static int jit_step(Jit *jit) {
  u16 pc = jit->cpu->pc;
  if (jit->cpu->is_call) {
    if (++jit->exec_hits[pc] > HOT_ROUTINE_HIT_THRESHOLD
        && !jit->hot_routines_marked[pc]
        && jit->hot_routines_size < 100) {
      jit->hot_routines[jit->hot_routines_size++] = pc;
      jit->hot_routines_marked[pc] = true;
    }
  }
#if 0
  if (jit->cpu->ram[pc] == 0xaf) { // XRA A,A
    if (jitted_xra_aa == NULL) {
      compile_xra_aa();
    }
    return jitted_xra_aa(jit->cpu);
  }
  else
#endif
    return cpu_step(jit->cpu);
}

static void jit_run_for(Jit *jit, size_t cycles) {
  jit->remaining_cycles += cycles;
  while (jit->remaining_cycles > 0)
    jit->remaining_cycles -= jit_step(jit);
}

void jit_emulate_one_frame(Jit *jit)
{
  jit_run_for(jit, 12500);
  cpu_interrupt(jit->cpu, 1); // mid-vblank
  jit_run_for(jit, 16667);
  cpu_interrupt(jit->cpu, 2); // vblank
  jit_run_for(jit, 4166);
}
