#include <stdio.h>
#include <stdlib.h>

int fibonacci_rec(int n) {
  if (n == 1 || n == 2)
    return 1;
  else
    return fibonacci_rec(n-1) + fibonacci_rec(n-2);
}

int main(int argc, char **argv) {
  int n = atoi(argv[1]);
  printf("%d\n", fibonacci_rec(n));
  return 0;
}
