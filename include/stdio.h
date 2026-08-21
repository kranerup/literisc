/* stdio.h - liteRISC minimal stdio implementation
 *
 * Provides: putchar, print_str, puts, itoa_r, itoa, printf
 *
 * Non-standard printf signature (no varargs, void return):
 *   void printf(char *fmt, int a0, int a1, int a2)
 * Supports up to three %d format specifiers per call.
 *
 * Use with: cpp -E -P -I <dir-containing-this-file> source.c | lrcc
 */

#ifndef STDIO_H
#define STDIO_H

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

int div10(int n, int *rem) {
  uint32_t u = (uint32_t)n;
  uint32_t q = 0;
  uint32_t b = 1u << 27;           /* highest quotient bit for n <= INT_MAX */
  uint32_t b10 = 0x50000000u;      /* 10 * 2^27, invariant b10 == 10*b */
  while (b != 0) {
    if (b10 <= u) {
      u -= b10;
      q |= b;
    }
    b10 >>= 1;
    b >>= 1;
  }
  *rem = (int)u;                      /* what's left of u is n % 10 */
  return (int)q;
}

char *itoa_r(int n, char *p) {
  int r;
  int q = div10(n, &r);
  if (q != 0) p = itoa_r(q, p);
  *p++ = '0' + r;
  return p;
}

char *itoa(int n, char *buf) {
  char *p = buf;
  int i;
  if (n == 0) { buf[0] = '0'; buf[1] = 0; return buf; }
  if (n == -2147483648) {
    char *lit = "-2147483648";
    for (i = 0; lit[i]; i++) buf[i] = lit[i];
    buf[i] = 0;
    return buf;
  }
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
      if (*fmt == 0) { putchar('%'); break; }
      spec = *fmt++;
      if (spec == 'd' && argc < 3) {
        print_str(itoa(args[argc++], ibuf));
      } else if (spec == 's' && argc < 3) {
        print_str((char *)args[argc++]);
      } else if (spec == 'x' && argc < 3) {
          unsigned int u = (unsigned int)args[argc++];
          int i, started = 0;
          for (i = 28; i >= 0; i -= 4) {
              int d = (u >> i) & 0xf;
              if (d || started || i == 0) {
                  putchar(d < 10 ? '0' + d : 'a' + d - 10);
                  started = 1;
              }
          }
      } else {
        putchar('%');
        putchar(spec);
      }
    } else {
      putchar(c);
    }
  }
}

#endif /* STDIO_H */
