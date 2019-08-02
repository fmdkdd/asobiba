#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <x86intrin.h>


double poly(double a[], double x, long degree) {
  long i;
  double result = a[0];
  double xpwr = x;
  for (i = 1; i <= degree; ++i) {
    result += a[i] * xpwr;
    xpwr = x * xpwr;
  }

  return result;
}

double horner(double a[], double x, long degree) {
  long i;
  double result = a[degree];
  for (i = degree-1; i >= 0; --i) {
    result = a[i] + x*result;
  }

  return result;
}

// 6x6 unrolling
double poly2(double a[], double x, long degree) {
  long i = 1;
  double result0 = a[0];
  double result1 = 0;
  double result2 = 0;
  double result3 = 0;
  double result4 = 0;
  double result5 = 0;
  double xpwr = x;
  double x6 = x * x * x * x * x * x;
  for (; i <= degree-5; i+=6) {
    result0 += a[i]   * xpwr;
    result1 += a[i+1] * xpwr;
    result2 += a[i+2] * xpwr;
    result3 += a[i+3] * xpwr;
    result4 += a[i+4] * xpwr;
    result5 += a[i+5] * xpwr;
    xpwr *= x6;
  }
  for (; i <= degree; ++i) {
    result0 += a[i] * xpwr;
    xpwr *= x;
  }
  return result0 + x * (result1 + x * (result2 + x * (result3 + x * (result4 + x * (result5)))));
}

//               100    1k    10k   100k
// poly   -Og    27.90  9.17  3.94  3.42
// horner -Og    29.64  8.04  5.89  5.66
// poly2  -Og    25.60  5.77  1.46  1.04

// Skylake numbers from https://uops.info/table.html
//
//         latency   throughput/measured
// add       1-10     1.00/1.00
// imul      3-8      1.00/1.00
// addss     4-11     0.50/0.52
// mulss     4-11     0.50/0.52

int main() {
  long n = 100000;
  double u[n];
  double x = 0.12;

  for (long i=0 ; i <= n; ++i) {
    u[i] = i;
  }

  unsigned long long start = __rdtsc();
  double r = poly2(u, x, n);
  unsigned long long cycles = __rdtsc() - start;

  //printf("%.24lf %.24lf\n", r, poly(u, x, n));
  assert(fabs(r - poly(u, x, n)) < 0.000001);

  printf("%llucy  CPE: %lf\n", cycles, (double)cycles / n);
}
