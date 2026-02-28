/* stdio.c - minimal stdio for liteRISC emulator
 *
 * Provides: putchar, print_str, puts, itoa_r, itoa, printf
 *
 * Note: printf uses a fixed, non-standard signature (no varargs, no return value):
 *   void printf(char *fmt, int a0, int a1, int a2)
 * Supports up to three %d format specifiers per call.
 */

volatile char *_outch = (volatile char*)0xffffffff;

int putchar(int c) {
  *_outch = (char)c;
  return c;
}

void print_str(char *s) {
  while (*s) putchar(*s++);
}

int puts(char *s) {
  print_str(s);
  putchar('\n');
  return 0;
}

char *itoa_r(int n, char *p) {
  if (n > 9) p = itoa_r(n / 10, p);
  *p++ = '0' + n % 10;
  return p;
}

char *itoa(int n, char *buf) {
  char *p = buf;
  if (n == 0) { buf[0] = '0'; buf[1] = 0; return buf; }
  if (n < 0) { *p++ = '-'; n = -n; }
  p = itoa_r(n, p);
  *p = 0;
  return buf;
}

void printf(char *fmt, int a0, int a1, int a2) {
  int args[3];
  int argc = 0, c, spec;
  char ibuf[12];
  args[0] = a0; args[1] = a1; args[2] = a2;
  while (*fmt) {
    c = *fmt++;
    if (c == '%') {
      spec = *fmt++;
      if (spec == 'd') {
        print_str(itoa(args[argc++], ibuf));
      } else {
        putchar('%');
        putchar(spec);
      }
    } else {
      putchar(c);
    }
  }
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
