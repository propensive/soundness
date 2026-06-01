// Sample C header (the kind cbindgen emits for a Rust crate's extern "C" API).

#ifndef LIBRARY_H
#define LIBRARY_H

#include <stdint.h>

typedef struct Point {
  int x;
  int y;
} Point;

typedef int Counter;

typedef enum { LEFT, RIGHT } Direction;

typedef union {
  int i;
  float f;
} Number;

int abs(int n);
int add(int a, int b);
double pow(double base, double exponent);
size_t strlen(const char* s);
const char* version(void);
int32_t identity(int32_t value);
Counter increment(Counter c);
Direction flip(Direction d);

#endif
