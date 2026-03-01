#include <stdio.h>

int main() {
  char buf[12];
  puts(itoa(0, buf));
  puts(itoa(42, buf));
  puts(itoa(-7, buf));
  puts("Hello, World!");
  printf("value=%d\n", 42, 0, 0);
  printf("%d + %d = %d\n", 1, 2, 3);
  return 0;
}
