#include <stdio.h>

#define N 100
#define SIEVE_SIZE 101

int main() {
  int sieve[SIEVE_SIZE];
  int i, j;

  for (i = 0; i <= N; i++)
    sieve[i] = 0;

  for (i = 2; i * i <= N; i++) {
    if (!sieve[i]) {
      for (j = i * i; j <= N; j += i)
        sieve[j] = 1;
    }
  }

  for (i = 2; i <= N; i++) {
    if (!sieve[i])
      printf("%d\n", i, 0, 0);
  }

  return 0;
}
