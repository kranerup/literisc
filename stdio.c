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
  int i = 0, j = 0, k;
  char tmp;
  if (n == 0) { buf[0] = '0'; buf[1] = 0; return buf; }
  if (n < 0) { buf[i++] = '-'; n = -n; j = 1; }
  while (n > 0) { buf[i++] = '0' + n % 10; n /= 10; }
  buf[i] = 0;
  k = i - 1;
  while (j < k) { tmp = buf[j]; buf[j] = buf[k]; buf[k] = tmp; j++; k--; }
  return buf;
}

int printf(char *fmt, int a0, int a1, int a2) {
  int args[3];
  int argc = 0, count = 0, c, spec;
  char ibuf[12];
  char *s;
  args[0] = a0; args[1] = a1; args[2] = a2;
  while (*fmt) {
    c = *fmt++;
    if (c == '%') {
      spec = *fmt++;
      if (spec == 'd') {
        s = itoa(args[argc], ibuf);
        argc++;
        while (*s) { putchar(*s++); count++; }
      } else {
        putchar('%'); count++;
        putchar(spec); count++;
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
