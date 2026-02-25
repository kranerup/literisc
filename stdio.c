/* stdio.c - minimal stdio for liteRISC emulator
 *
 * Provides: putchar, puts, itoa, printf (handles %d only)
 *
 * Note: printf uses a fixed signature (no varargs support in this compiler):
 *   int printf(char *fmt, int a0, int a1, int a2)
 * Supports up to three %d format specifiers per call.
 */

volatile char *_outch = (volatile char*)0xffffffff;

int putchar(int c) {
  *_outch = (char)c;
  return c;
}

int puts(char *s) {
  while (*s) putchar(*s++);
  putchar('\n');
  return 0;
}

char *itoa(int n, char *buf) {
  int i = 0;
  int j, k;
  char tmp;
  int neg = 0;
  if (n == 0) { buf[0] = '0'; buf[1] = 0; return buf; }
  if (n < 0) { neg = 1; n = -n; }
  while (n > 0) {
    buf[i++] = '0' + n % 10;
    n /= 10;
  }
  if (neg) buf[i++] = '-';
  buf[i] = 0;
  j = 0; k = i - 1;
  while (j < k) {
    tmp = buf[j]; buf[j] = buf[k]; buf[k] = tmp;
    j++; k--;
  }
  return buf;
}

int printf(char *fmt, int a0, int a1, int a2) {
  int argc = 0;
  char ibuf[12];
  int count = 0;
  int c;
  int val;
  char *s;
  while (*fmt) {
    c = *fmt;
    fmt++;
    if (c == '%') {
      c = *fmt;
      fmt++;
      if (c == 'd') {
        if (argc == 0) val = a0;
        else if (argc == 1) val = a1;
        else val = a2;
        argc++;
        s = itoa(val, ibuf);
        while (*s) { putchar(*s); s++; count++; }
      } else {
        putchar('%'); count++;
        putchar(c); count++;
      }
    } else {
      putchar(c); count++;
    }
  }
  return count;
}

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
