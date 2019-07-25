#include <stdio.h>

long switchv(long idx) {
  long result = 0;
  switch (idx) {
  case 0:
    result = 0xaaa;
    break;
  case 2:
  case 5:
    result = 0xbbb;
    break;
  case 3:
    result = 0xccc;
    break;
  default:
    result = 0xddd;
  }
  return result;
}

long switchv_asm(long idx) {
  asm (
       "and $7, %%rdi\n\t"
       "lea (%%rip), %%rax\n\t"
       "lea 8(%%rax,%%rdi,8), %%rax\n\t"
       "push %%rax\n\t"
       "ret\n"
       "case0:\n\t"
       "mov $0xaaa, %%rax\n\t"
       "ret\n"
       "case1:\n\t"
       "mov $0xddd, %%rax\n\t"
       "ret\n"
       "case2:\n\t"
       "mov $0xbbb, %%rax\n\t"
       "ret\n"
       "case3:\n\t"
       "mov $0xccc, %%rax\n\t"
       "ret\n"
       "case4:\n\t"
       "mov $0xddd, %%rax\n\t"
       "ret\n"
       "case5:\n\t"
       "mov $0xbbb, %%rax\n\t"
       "ret\n"
       "case6:\n\t"
       "mov $0xddd, %%rax\n\t"
       "ret\n"
       "case7:\n\t"
       "mov $0xddd, %%rax\n\t"
       "ret\n"
       : : : "rax");
}

int main() {
  for (long i = -1; i < 8; ++i) {
    printf("%3ld  0x%lx  0x%lx\n", i, switchv(i), switchv_asm(i));
  }
  return 0;
}
