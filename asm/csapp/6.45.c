#include <stdio.h>
#include <x86intrin.h>

void transpose(int *s, int *d, int n) {
  for (int i=0; i < n; ++i) {
    for (int j=0; j < n; ++j) {
      d[j*n + i] = s[i*n + j];
    }
  }
}

void transpose2(int *s, int *d, int n) {
  for (int i=0; i < n; ++i) {
    int *dd = d;
    for (int j=0; j < n; ++j) {
      *dd = *s;
      ++s;
      dd += n;
    }
    ++d;
  }
}

void transpose2b(int *s, int *d, int n) {
  for (int i=0; i < n; ++i) {
    int *ss = s;
    for (int j=0; j < n; ++j) {
      *d = *ss;
      ++d;
      ss += n;
    }
    ++s;
  }
}

void transpose3(int *s, int *d, int n) {
  int i,j;
  int *dd;
  long *dl, *sl;

  for (i=0; i < n; ++i) {
    dl = (long*)d;
    sl = (long*)s;
    for (j=0; j < n-1; j+=2) {
      *dl = *sl;
      ++sl;
      dl += n;
    }
    /* dd = (int*)dl; */
    /* s = (int*) sl; */
    /* for (; j < n; ++j) { */
    /*   *dd = *s; */
    /*   ++s; */
    /*   dd += n; */
    /* } */
    d+=2;
  }
}

//                  10    100   1k
// transpose   -Og  3.45  5.15  6.04
// transpose2  -Og  2.92  4.46  6.02
// transpose2b -Og  2.67  4.43  5.44
// transpose3  -Og  2.08  3.70  4.51

// Skylake numbers from https://uops.info/table.html
//
//         latency   throughput/measured
// add       1-10     1.00/1.00
// imul      3-8      1.00/1.00
// addss     4-11     0.50/0.52
// mulss     4-11     0.50/0.52
// divss    11-18     5.00/3.00

int main() {
  long n = 1000;
  long nn = n*n;
  int s[nn];
  int d[nn];

  for (int i=0; i < nn; ++i) {
    s[i] = i;
  }

  unsigned long long start = __rdtsc();
  transpose3(s, d, n);
  unsigned long long cycles = __rdtsc() - start;

  printf("%llucy  CPE: %lf\n", cycles, (double)cycles / nn);
}
