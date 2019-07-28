#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <x86intrin.h>

void* b_memset(void *s, int c, size_t n) {
  unsigned char *schar = s;
  for (size_t i=0; i < n; ++i) {
    *schar++ = (unsigned char) c;
  }
  return s;
}

// Write 8 bytes each time
void* b_memset2(void *s, int c, size_t n) {
  unsigned long b = (unsigned char) c;
  // This assumes word = 64 bits
  unsigned long d = b | b<<8  | b<<16 | b<<24 | b<<32
                      | b<<40 | b<<48 | b<<56;

  unsigned long *p = s;
  unsigned char *pbyte;
  void *stop = s + n;

  // First align pbyte to unsigned long
 byte_step:
  pbyte = (void*)p;
 loop:
  if (pbyte == stop)
    goto done;
  if ((unsigned long)pbyte % sizeof(unsigned long) == 0)
    goto quad_step;
  *pbyte++ = b;
  goto loop;

  // Write a quad word each iteration
 quad_step:
  p = (void*)pbyte;
  while ((void*)p < (stop-7))
    *p++ = d;

  // Finish remaining bytes
  goto byte_step;

 done:
  return s;
}

// Using a single pointer
void* b_memset3(void *s, int c, size_t n) {
  unsigned char b = c;
  unsigned char *pbyte = s;
  void *stop = s + n;

 byte_step:
  if (pbyte == stop)
    goto done;
  if ((unsigned long)pbyte % sizeof(unsigned long) == 0)
    goto quad_step;
  *pbyte++ = b;
  goto byte_step;

 quad_step:
  while ((void*)pbyte < stop-7) {
    *(pbyte+1) = b;
    *(pbyte+2) = b;
    *(pbyte+3) = b;
    *(pbyte+4) = b;
    *(pbyte+5) = b;
    *(pbyte+6) = b;
    *(pbyte+7) = b;
    pbyte+=8;
  }

  goto byte_step;

 done:
  return s;
}

// Assume n is multiple of 64
void* b_memset4(void *s, int c, size_t n) {
  assert (n % 64 == 0);

  unsigned long b = (unsigned char) c;
  unsigned long d = b | b<<8  | b<<16 | b<<24 | b<<32
                      | b<<40 | b<<48 | b<<56;

  unsigned long *p0 = s;
  unsigned long *p1 = p0+1;
  unsigned long *p2 = p0+2;
  unsigned long *p3 = p0+3;
  void *stop = s + n;

  while ((void*)p3 < stop) {
    *p0 = d;
    *p1 = d;
    *p2 = d;
    *p3 = d;
    p0 += 4;
    p1 += 4;
    p2 += 4;
    p3 += 4;
  }

  while ((void*)p0 < stop)
    *p0++ = d;

  return s;
}

//                 100   1000  10000
// memset -Og      0.39  0.20  3.37
// b_memset -Og    4.16  3.59  6.01
// b_memset2 -Og   1.45  0.54  2.95
// b_memset3 -Og   3.10  2.76  5.13

//                 128   1024  8192  16384
// b_memset4 -Og   0.73  0.46  2.70  3.29

// Skylake numbers from https://uops.info/table.html
//
//         latency   throughput/measured
// add       1-10     1.00/1.00
// imul      3-8      1.00/1.00
// addss     4-11     0.50/0.52
// mulss     4-11     0.50/0.52

int main() {
  long n = 16384;
  int  u[n];

  unsigned long long start = __rdtsc();
  memset(u, 0, sizeof(u));
  unsigned long long cycles = __rdtsc() - start;

  int v[n];
  memset(v, 0, sizeof(v));
  assert(memcmp(v, u, sizeof(v)) == 0);

  printf("%llucy  CPE: %lf\n", cycles, (double)cycles / n);
}
