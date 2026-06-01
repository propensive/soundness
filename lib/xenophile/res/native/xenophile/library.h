// Sample C header (the kind cbindgen emits for a Rust crate's extern "C" API).

#ifndef LIBRARY_H
#define LIBRARY_H

#include <stdint.h>

typedef struct Point {
  int x;
  int y;
} Point;

int abs(int n);
double pow(double base, double exponent);
size_t strlen(const char* s);
const char* version(void);

#endif
