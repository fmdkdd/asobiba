#include <stdio.h>
#include <stdlib.h>

extern int entry_point() asm("entry_point");

const int BOOL_TAG   = 1;
const int BOOL_TRUE  = 0xffffffff;
const int BOOL_FALSE = 0x7fffffff;

enum Error { Arith_Expect_Num = 1,
             Comp_Expect_Num,
             If_Cond_Expect_Bool,
};

void error(enum Error code) {
  switch (code) {
  case Arith_Expect_Num    : fprintf(stderr, " Error: arithmetic expected a number\n"); break;
  case Comp_Expect_Num     : fprintf(stderr, " Error: comparison expected a number\n"); break;
  case If_Cond_Expect_Bool : fprintf(stderr, " Error: if expected a boolean\n"); break;
  }

  exit(code);
}

int print(int r) {
  printf(" ");

  if (r & BOOL_TAG) {
    if (r == -1)              { printf("true");  }
    else if (r == 0x7FFFFFFF) { printf("false"); }
    else                      { fprintf(stderr, "Invalid value: 0x%x\n", r); }
  } else {
    // Number
    printf("%d", r >> 1);
  }

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
    error(Arith_Expect_Num);
  }
}

void bool_check(int n) {
  if (is_bool(n) == BOOL_FALSE) {
    error(Comp_Expect_Num);
  }
}

void if_cond_check(int n) {
  if (is_bool(n) == BOOL_FALSE) {
    error(If_Cond_Expect_Bool);
  }
}

int main() {
  int result = entry_point();
  print(result);
  return 0;
}
