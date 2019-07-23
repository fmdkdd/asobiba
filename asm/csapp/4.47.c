#include <stdio.h>

void bubble_a(long *data, long count) {
  long i, last;
  for (last = count-1; last > 0; --last) {
    for (i = 0; i < last; ++i) {
      if (data[i+1] < data[i]) {
        long t = data[i+1];
        data[i+1] = data[i];
        data[i] = t;
      }
    }
  }
}

void bubble_b(long *data, long count) {
  long *cur, last;
  for (last = count-1; last > 0; --last) {
    for (cur = data; cur < &data[last]; ++cur) {
      long *next = cur+1;
      if (*next < *cur) {
        long t = *next;
        *next = *cur;
        *cur = t;
      }
    }
  }
}

void bubble_c(long *data, long count) {
  // rdi = data
  // rsi = count
  asm (
    "dec %%rsi\n\t"
    "lea (%%rdi,%%rsi,8), %%rax\n\t" // rax = &data[count-1]

    // outer loop
    "jmp test_last\n"
    "loop_last:\n\t"
    "mov %%rdi, %%rbx\n\t"        // rbx = data
    "lea 8(%%rdi), %%rdx\n\t"     // rdx = data+1

    // inner loop
    "jmp test\n"
    "loop:\n\t"

    "mov (%%rbx), %%r8\n\t"
    "cmp %%r8, (%%rdx)\n\t"
    "jge skip\n\t"
    "xchg %%r8, (%%rdx)\n\t"
    "mov %%r8, (%%rbx)\n"

    "skip:\n\t"
    "lea 8(%%rbx), %%rbx\n\t" // rbx++
    "lea 8(%%rdx), %%rdx\n"   // rdx++
    "test:\n\t"
    "cmp %%rbx, %%rax\n\t"
    "jne loop\n\t"

    "lea -8(%%rax), %%rax\n\t"
    "dec %%rsi\n"
    "test_last:\n\t"
    "jne loop_last\n\t"
    :
    :
    : "rax", "rbx", "rdx", "r8", "r9", "cc"
  );
}

// 4.48
void bubble_d(long *data, long count) {
  // rdi = data
  // rsi = count
  asm (
    "dec %%rsi\n\t"
    "lea (%%rdi,%%rsi,8), %%rax\n\t" // rax = &data[count-1]

    // outer loop
    "jmp dtest_last\n"
    "dloop_last:\n\t"
    "mov %%rdi, %%rbx\n\t"        // rbx = data
    "lea 8(%%rdi), %%rdx\n\t"     // rdx = data+1

    // inner loop
    "jmp dtest\n"
    "dloop:\n\t"

    "mov (%%rbx), %%r8\n\t"
    "mov (%%rdx), %%r9\n\t"
    "cmp %%r8, %%r9\n\t"

    // Swap without branch with 3 cmov
    "cmovle %%r9, %%r10\n\t"
    "cmovle %%r8, %%r9\n\t"
    "cmovle %%r10, %%r8\n\t"

    "mov %%r8, (%%rbx)\n\t"
    "mov %%r9, (%%rdx)\n"

    "lea 8(%%rbx), %%rbx\n\t" // rbx++
    "lea 8(%%rdx), %%rdx\n"   // rdx++
    "dtest:\n\t"
    "cmp %%rbx, %%rax\n\t"
    "jne dloop\n\t"

    "lea -8(%%rax), %%rax\n\t"
    "dec %%rsi\n"
    "dtest_last:\n\t"
    "jne dloop_last\n\t"
    :
    :
    : "rax", "rbx", "rdx", "r8", "r9", "r10", "cc"
  );
}

// 4.49
void bubble_e(long *data, long count) {
  // rdi = data
  // rsi = count
  asm (
    "dec %%rsi\n\t"
    "lea (%%rdi,%%rsi,8), %%rax\n\t" // rax = &data[count-1]

    // outer loop
    "jmp etest_last\n"
    "eloop_last:\n\t"
    "mov %%rdi, %%rbx\n\t"        // rbx = data
    "lea 8(%%rdi), %%rdx\n\t"     // rdx = data+1

    // inner loop
    "jmp etest\n"
    "eloop:\n\t"

    "mov (%%rbx), %%r8\n\t"
    "mov (%%rdx), %%r9\n\t"

    // Swap without branch with 1 cmov
    //            (x: a    , y: b)
    // x = x ^ y  (x: a ^ b, y: b)
    // y = b                        y = a
    // y = x ^ y  (x: a ^ b, y: a)  y = x ^ y  (x: a ^ b, y: b)
    // x = x ^ y  (x: b    , y: a)  x = x ^ y  (x: a    , y: b)
    "xor %%r9, %%r8\n\t"
    "cmp %%r9, (%%rbx)\n\t"
    "cmovle (%%rbx), %%r9\n\t"
    "xor %%r8, %%r9\n\t"
    "xor %%r9, %%r8\n\t"

    "mov %%r8, (%%rbx)\n\t"
    "mov %%r9, (%%rdx)\n"

    "lea 8(%%rbx), %%rbx\n\t" // rbx++
    "lea 8(%%rdx), %%rdx\n"   // rdx++
    "etest:\n\t"
    "cmp %%rbx, %%rax\n\t"
    "jne eloop\n\t"

    "lea -8(%%rax), %%rax\n\t"
    "dec %%rsi\n"
    "etest_last:\n\t"
    "jne eloop_last\n\t"
    :
    :
    : "rax", "rbx", "rdx", "r8", "r9", "cc"
  );
}


int main() {
  {
    long data[] = {20,5,6,1,8,3,15};
    long count = 7;

    bubble_a(data, count);
    for (int i=0; i < count; ++i)
      printf("%ld ", data[i]);
    printf("\n");
  }

  {
    long data[] = {20,5,6,1,8,3,15};
    long count = 7;

    bubble_b(data, count);
    for (int i=0; i < count; ++i)
      printf("%ld ", data[i]);
    printf("\n");
  }

  {
    long data[] = {20,5,6,1,8,3,15};
    long count = 7;

    bubble_c(data, count);
    for (int i=0; i < count; ++i)
      printf("%ld ", data[i]);
    printf("\n");
  }

  {
    long data[] = {20,5,6,1,8,3,15};
    long count = 7;

    bubble_d(data, count);
    for (int i=0; i < count; ++i)
      printf("%ld ", data[i]);
    printf("\n");
  }

  {
    long data[] = {20,5,6,1,8,3,15};
    long count = 7;

    bubble_e(data, count);
    for (int i=0; i < count; ++i)
      printf("%ld ", data[i]);
    printf("\n");
  }

  return 0;
}
