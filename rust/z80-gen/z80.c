#include <stdint.h>
#include <stdio.h>

struct {
  union {
    struct {
#ifdef BIG_ENDIAN
      uint8_t a, f, b, c, d, e, h, l;
#else
      uint8_t f, a, c, b, e, d, l, h;
#endif
    };
    struct {
      uint16_t af, bc, de, hl;
    };
  };

  uint8_t i, r;
  uint16_t pc, sp, ix, iy;
  uint16_t af_, bc_, de_, hl_; // shadow registers

} reg;

// Return value from RAM at ADDR
uint8_t read(uint16_t addr) {
  return 0;
}

// Write VAL into RAM at ADDR
void write(uint16_t addr, uint8_t val) {
}

// Execute one instruction and return the number of cycles
uint32_t step() {
  uint8_t opcode = read(reg.pc++);
  uint32_t cycles = 0;
  uint32_t t;

  switch (opcode) {
  }
}

int main() {
  reg.a = 2;
  reg.f = 3;
  printf("%02x %02x %02x\n", reg.a, reg.f, reg.af);
}
