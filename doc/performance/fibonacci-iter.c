#include <stdio.h>
#include <stdlib.h>

int fibonacci_iter(int n) {
  int acc1 = 1;
  int acc2 = 1;
  int sum = 0;

  if (n == 1 || n == 2)
    return 1;
  else
    for(int i = n; n > 2; n--) {
      sum = acc1 + acc2;
      acc1 = acc2;
      acc2 = sum;
    }
  return acc2;
}

int main(int argc, char **argv) {
  int n = atoi(argv[1]);
  printf("%d\n", fibonacci_iter(n));
  return 0;
}
