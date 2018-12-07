#include <stdio.h>
#include <stdlib.h>

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Runtime functions called from compiled Diamondback code
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

const int BOOL_TAG   = 1;
const int BOOL_TRUE  = 0xffffffff;
const int BOOL_FALSE = 0x7fffffff;

enum Error { Arith_Expect_Num = 1,
             Comp_Expect_Num,
             If_Cond_Expect_Bool,
             Overflow,
};

// Helper
void _print(int r, FILE *stream) {
  if (r & BOOL_TAG) {
    if (r == -1)              { fprintf(stream, "true");  }
    else if (r == 0x7FFFFFFF) { fprintf(stream, "false"); }
    else                      { fprintf(stderr, "Invalid value: 0x%x\n", r); }
  } else {
    // Number
    fprintf(stream, "%d", r >> 1);
  }
}

int print(int r) {
  printf(" ");
  _print(r, stdout);
  return r;
}

int is_bool(int n) {
  if (n & BOOL_TAG) {
    return BOOL_TRUE;
  } else {
    return BOOL_FALSE;
  }
}

int is_num(int n) {
  if ((n & BOOL_TAG) == 0) {
    return BOOL_TRUE;
  } else {
    return BOOL_FALSE;
  }
}

void num_check(int n) {
  if (is_num(n) == BOOL_FALSE) {
    fprintf(stderr, "Error: expected a number, got ");
    _print(n, stderr);
    exit(Arith_Expect_Num);
  }
}

void num_check2(int a, int b) {
  if (is_num(a) == BOOL_FALSE) {
    fprintf(stderr, "Error: expected a number, got ");
    _print(a, stderr);
    exit(Arith_Expect_Num);
  } else if (is_num(b) == BOOL_FALSE) {
    fprintf(stderr, "Error: expected a number, got ");
    _print(b, stderr);
    exit(Arith_Expect_Num);
  }
}

void bool_check(int n) {
  if (is_bool(n) == BOOL_FALSE) {
    fprintf(stderr, "Error: expected a boolean, got ");
    _print(n, stderr);
    exit(Comp_Expect_Num);
  }
}

void bool_check2(int a, int b) {
  if (is_bool(a) == BOOL_FALSE) {
    fprintf(stderr, "Error: expected a boolean, got ");
    _print(a, stderr);
    exit(Comp_Expect_Num);
  } else if (is_bool(b) == BOOL_FALSE) {
    fprintf(stderr, "Error: expected a boolean, got ");
    _print(b, stderr);
    exit(Comp_Expect_Num);
  }
}

void if_cond_check(int n) {
  if (is_bool(n) == BOOL_FALSE) {
    fprintf(stderr, "Error: 'if' expected a boolean, got ");
    _print(n, stderr);
    exit(If_Cond_Expect_Bool);
  }
}

void overflow() {
  fprintf(stderr, "Error: integer overflow");
  exit(Overflow);
}

int max(int a, int b) {
  return a > b ? a : b;
}

int add1(int a) {
  num_check(a);
  return a + 2;
}

int sub1(int a) {
  num_check(a);
  return a - 2;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Main
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

extern int entry_point() asm("entry_point");

int main() {
  int result = entry_point();
  print(result);
  return 0;
}
