#include <stdio.h>
#include <x86intrin.h>

typedef float data_t;

typedef struct {
  data_t *ptr;
  long len;
} vec_t;

void inner(vec_t *u, vec_t *v, data_t *dst) {
  long i;
  long len = u->len;
  data_t *up = u->ptr;
  data_t *vp = v->ptr;
  data_t sum = 0;
  for (i = 0; i < len; ++i) {
    sum = sum + up[i] * vp[i];
  }
  *dst = sum;
}

// 5.14: 6x1 loop unrolling
void inner2(vec_t *u, vec_t *v, data_t *dst) {
  long i;
  long len = u->len;
  data_t *up = u->ptr;
  data_t *vp = v->ptr;
  data_t sum = 0;
  for (i = 0; i < len-5; i+=6) {
    sum = sum + up[i] * vp[i];
    sum = sum + up[i+1] * vp[i+1];
    sum = sum + up[i+2] * vp[i+2];
    sum = sum + up[i+3] * vp[i+3];
    sum = sum + up[i+4] * vp[i+4];
    sum = sum + up[i+5] * vp[i+5];
  }
  for (; i < len; ++i) {
    sum = sum + up[i] * vp[i];
  }
  *dst = sum;
}

// 5.15: 6x6 loop unrolling
void inner3(vec_t *u, vec_t *v, data_t *dst) {
  long i;
  long len = u->len;
  data_t *up = u->ptr;
  data_t *vp = v->ptr;
  data_t sum0 = 0;
  data_t sum1 = 0;
  data_t sum2 = 0;
  data_t sum3 = 0;
  data_t sum4 = 0;
  data_t sum5 = 0;
  for (i = 0; i < len-5; i+=6) {
    sum0 = sum0 + up[i] * vp[i];
    sum1 = sum1 + up[i+1] * vp[i+1];
    sum2 = sum2 + up[i+2] * vp[i+2];
    sum3 = sum3 + up[i+3] * vp[i+3];
    sum4 = sum4 + up[i+4] * vp[i+4];
    sum5 = sum5 + up[i+5] * vp[i+5];
  }
  for (; i < len; ++i) {
    sum0 = sum0 + up[i] * vp[i];
  }
  *dst = sum0 + sum1 + sum2 + sum3 + sum4 + sum5;
}

// 5.16: 6x1a loop unrolling
void inner4(vec_t *u, vec_t *v, data_t *dst) {
  long i;
  long len = u->len;
  data_t *up = u->ptr;
  data_t *vp = v->ptr;
  data_t sum = 0;
  for (i = 0; i < len-5; i+=6) {
    sum = sum + (up[i] * vp[i]
                 + up[i+1] * vp[i+1]
                 + up[i+2] * vp[i+2]
                 + up[i+3] * vp[i+3]
                 + up[i+4] * vp[i+4]
                 + up[i+5] * vp[i+5]);
  }
  for (; i < len; ++i) {
    sum = sum + up[i] * vp[i];
  }
  *dst = sum;
}

//               int   float
// inner -Og     1.42  2.48
// inner2 -Og    0.85  2.82
// inner3 -Og    0.85  0.84
// inner4 -Og    1.00  0.88

// Skylake numbers from https://uops.info/table.html
//
//         latency   throughput/measured
// add       1-10     1.00/1.00
// imul      3-8      1.00/1.00
// addss     4-11     0.50/0.52
// mulss     4-11     0.50/0.52

int main() {
  long n = 10000;
  vec_t u, v;
  u.ptr = malloc(sizeof(*u.ptr)*n);
  u.len = n;
  v.ptr = malloc(sizeof(*v.ptr)*n);
  v.len = n;

  for (int i=0; i < n; ++i) {
    u.ptr[i] = i;
    v.ptr[i] = i;
  }

  data_t res;

  unsigned long long start = __rdtsc();
  inner4(&u, &v, &res);
  unsigned long long cycles = __rdtsc() - start;

  printf("%llucy  CPE: %lf\n", cycles, (double)cycles / n);
}
