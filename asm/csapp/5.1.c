#include <stdio.h>
#include <x86intrin.h>

// min CPE: 6.37
void psum1(float a[], float p[], long n) {
  long i;
  p[0] = a[0];
  for (i = 1; i < n; ++i) {
    p[i] = p[i-1] + a[i];
  }
}

// min CPE: 4.58
void psum2(float a[], float p[], long n) {
  long i;
  p[0] = a[0];
  for (i = 1; i < n-1; i+=2) {
    float mid = p[i-1] + a[i];
    p[i] = mid;
    p[i+1] = mid + a[i+1];
  }
  if (i < n)
    p[i] = p[i-1] + a[i];
}

// min CPE: 2.82
void psum1b(float a[], float p[], long n) {
  long i;
  p[0] = a[0];
  float pprev = p[0];
  for (i = 1; i < n; ++i) {
    pprev = p[i] = pprev + a[i];
  }
}

int main() {
  long n = 10000;
  float a[n];
  float p[n];

  for (int i=0; i < n; ++i) {
    a[i] = i;
    p[i] = 0;
  }

  unsigned long long start = __rdtsc();
  psum2(a, p, n);
  unsigned long long cycles = __rdtsc() - start;
  printf("%llucy  CPE: %lf\n", cycles, (double)cycles / n);
}
