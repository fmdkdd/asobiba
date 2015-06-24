/* From http://www.felixangell.com/virtual-machine-in-c/ */

#include <stdbool.h>
#include <stdio.h>

typedef enum {
  PSH,
  ADD,
  POP,
  SET,
  HLT,
  JMP
} InstructionSet;

typedef enum {
  A, B, C, D, E, F,
  IP, SP,
  NUM_OF_REGISTERS
} Registers;

#define sp (registers[SP])
#define ip (registers[IP])

const int program[] = {
  PSH, 5,
  PSH, 6,
  ADD,
  POP,
  HLT
};

int stack[256];
int registers[NUM_OF_REGISTERS];
bool running = true;

int fetch() {
  return program[ip];
}

void eval(int instr) {
  switch (instr) {
  case PSH: {
    int x = program[++ip];
    stack[++sp] = x;
    break;
  }
  case ADD: {
    int y = stack[sp--];
    int x = stack[sp--];
    stack[++sp] = x + y;
    break;
  }
  case POP: {
    int x = stack[sp--];
    printf("%d\n", x);
    break;
  }
  case SET: {
    int r = program[++ip];
    int x = program[++ip];
    registers[r] = x;
    break;
  }
  case HLT: {
    running = false;
    break;
  }
  }
}

int main() {
  ip = 0;
  sp = -1;

  while (running) {
    eval(fetch());
    ++ip;
  }
}
