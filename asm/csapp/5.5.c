#include <stdio.h>
#include <x86intrin.h>

typedef int data_t;
#define OP *
#define IDENT 1

typedef struct {
  data_t *ptr;
  long len;
} vec_t;

void combine1(vec_t *v, data_t *dst) {
  long i;
  *dst = IDENT;
  for (i = 0; i < v->len; ++i) {
    *dst = *dst OP v->ptr[i];
  }
}

void combine2(vec_t *v, data_t *dst) {
  long i;
  long len = v->len;
  *dst = IDENT;
  for (i = 0; i < len; ++i) {
    *dst = *dst OP v->ptr[i];
  }
}

void combine4(vec_t *v, data_t *dst) {
  long i;
  long len = v->len;
  data_t *data = v->ptr;
  data_t acc = IDENT;
  for (i = 0; i < len; ++i) {
    acc = acc OP data[i];
  }
  *dst = acc;
}

// 2x1 loop unrolling
void combine5(vec_t *v, data_t *dst) {
  long i;
  long len = v->len;
  data_t *data = v->ptr;
  data_t acc = IDENT;

  for (i = 0; i < len-1; i+=2) {
    acc = (acc OP data[i]) OP data[i+1];
  }

  for (; i < len; ++i) {
    acc = acc OP data[i];
  }


  *dst = acc;
}

// 2x2 loop unrolling
void combine6(vec_t *v, data_t *dst) {
  long i;
  long len = v->len;
  data_t *data = v->ptr;
  data_t acc0 = IDENT;
  data_t acc1 = IDENT;

  for (i = 0; i < len-1; i+=2) {
    acc0 = acc0 OP data[i];
    acc1 = acc1 OP data[i+1];
  }

  for (; i < len; ++i) {
    acc0 = acc0 OP data[i];
  }

  *dst = acc0 OP acc1;
}

// 2x1a loop unrolling
void combine7(vec_t *v, data_t *dst) {
  long i;
  long len = v->len;
  data_t *data = v->ptr;
  data_t acc = IDENT;

  for (i = 0; i < len-1; i+=2) {
    acc = acc OP (data[i] OP data[i+1]);
  }

  for (; i < len; ++i) {
    acc = acc OP data[i];
  }

  *dst = acc;
}

// 6x1a loop unrolling
void combine8(vec_t *v, data_t *dst) {
  long i;
  long len = v->len;
  data_t *data = v->ptr;
  data_t acc = IDENT;

  for (i = 0; i < len-5; i+=6) {
    asm("# LLVM-MCA-BEGIN combine8");
    acc = acc OP
      (data[i] OP
       (data[i+1] OP
        (data[i+2] OP
         (data[i+3] OP
          (data[i+4] OP data[i+5])))));
    asm("# LLVM-MCA-END");
  }

  for (; i < len; ++i) {
    acc = acc OP data[i];
  }

  *dst = acc;
}

//                     int        float
//                   +     *     +     *     /
// combine1 -Og     3.52  5.27  6.33  6.33
// combine1 -O1     3.52  5.27  6.33  6.33
// combine2 -Og     3.68  4.91  6.33  6.33
// combine4 -Og     0.80  2.11  2.82  3.10  8.03
// combine4 -Og     0.80  2.11  2.82  3.10
// combine5 -Og     0.71  2.11  2.82  3.10
// combine6 -Og     0.62  1.06  1.41  1.71
// combine7 -Og     0.71  1.06  1.41  1.70
// combine8 -Og     0.49  0.71  0.50  0.79  2.40
//
// llvm-mca         0.52  1.03  0.71  0.71  3.06
// i7 Haswell       0.55  1.00  1.01  0.52

// Skylake numbers from https://uops.info/table.html
//
//         latency   throughput/measured
// add       1-10     1.00/1.00
// imul      3-8      1.00/1.00
// addss     4-11     0.50/0.52
// mulss     4-11     0.50/0.52
// divss    11-18     5.00/3.00

int main() {
  long n = 10000;
  vec_t v;
  v.ptr = malloc(sizeof(*v.ptr)*n);
  v.len = n;

  for (int i=0; i < n; ++i) {
    v.ptr[i] = i;
  }

  data_t res;

  unsigned long long start = __rdtsc();
  combine8(&v, &res);
  unsigned long long cycles = __rdtsc() - start;

  printf("%llucy  CPE: %lf\n", cycles, (double)cycles / n);
}
