#include <stdio.h>
#include <x86intrin.h>

void psum1b(float a[], float p[], long n) {
  long i;
  p[0] = a[0];
  float pprev = p[0];
  for (i = 1; i < n; ++i) {
    pprev = p[i] = pprev + a[i];
  }
}

// 2x1 unrolling
void psum1c(float a[], float p[], long n) {
  long i;
  float pp0 = p[0] = a[0];
  float a0;
  for (i = 1; i < n-1; i+=2) {
    a0 = a[i];
    p[i] = pp0 + a0;
    a0 += a[i+1];
    pp0 = p[i+1] = pp0 + a0;
  }

  for (; i < n; ++i) {
    pp0 = p[i] = pp0 + a[i];
  }
}

// 4x1 unrolling
void psum1d(float a[], float p[], long n) {
  long i;
  float pp0 = p[0] = a[0];
  float a0;
  for (i = 1; i < n-3; i+=4) {
    asm("# LLVM-MCA-BEGIN psum1d");
    a0 = a[i];
    p[i] = pp0 + a0;
    a0 += a[i+1];
    p[i+1] = pp0 + a0;
    a0 += a[i+2];
    p[i+2] = pp0 + a0;
    pp0 = p[i+3] = pp0 + (a0 + a[i+3]);
    asm("# LLVM-MCA-END");
  }

  for (; i < n; ++i) {
    pp0 = p[i] = pp0 + a[i];
  }
}

// Can we get to 0.50?  Not sure if the stores don't get in the way.

//               100    1k    10k   100k
// psum1b -Og                       2.82
// psum1c -Og                       1.47
// psum1d -Og                       1.01

// Skylake numbers from https://uops.info/table.html
//
//         latency   throughput/measured
// add       1-10     1.00/1.00
// imul      3-8      1.00/1.00
// addss     4-11     0.50/0.52
// mulss     4-11     0.50/0.52

int main() {
  long n = 100000;
  float a[n];
  float p[n];
  float p2[n];

  for (int i=0; i < n; ++i) {
    a[i] = i+1;
    p[i] = 0;
    p2[i] = 0;
  }

  unsigned long long start = __rdtsc();
  psum1d(a, p, n);
  unsigned long long cycles = __rdtsc() - start;

  /* psum1b(a, p2, n); */
  /* for (int i=0; i < n; ++i) */
  /*   printf("%.10lf ", p[i]); */
  /* printf("\n"); */
  /* for (int i=0; i < n; ++i) */
  /*   printf("%.10lf ", p2[i]); */
  /* printf("\n"); */

  printf("%llucy  CPE: %lf\n", cycles, (double)cycles / n);
}
