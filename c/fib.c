#include <stdlib.h>
#include <stdio.h>

const int REQUIRED_ARGS = 3; // executable name, width, depth

int fib(int n) {
  return n == 0 ? 0 : n == 1 ? 1 : fib(n-2) + fib(n-1);
}

int *mapFib(int width, int *xs) {
  int *ys = malloc(width);
  for (int i = 0; i < width; i++) {
    ys[i] = fib(xs[i]);
  }
  return ys;
}

int usage() {
  printf("Usage: ./fib WIDTH DEPTH");
  return 1;
}

int main(int argc, char *argv[]) {
  if (argc < REQUIRED_ARGS) return usage();
  int width = atoi(argv[1]);
  int depth = atoi(argv[2]);
  if (width == 0 || depth == 0) return usage();
  int xs[width];
  for (int i = 0; i < width; i++) {
    xs[i] = depth;
  }
  int *ys = mapFib(width, xs);
  free(ys);
}