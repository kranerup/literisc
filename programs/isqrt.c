#include <stdio.h>

int my_isqrt(int n) {
    int x, x1;
    if (n <= 0) return 0;
    x = n;
    x1 = (x + 1) / 2;
    while (x1 < x) {
        x = x1;
        x1 = (x + n / x) / 2;
    }
    return x;
}

int main() {
    printf("%d\n", my_isqrt(0), 0, 0);
    printf("%d\n", my_isqrt(1), 0, 0);
    printf("%d\n", my_isqrt(4), 0, 0);
    printf("%d\n", my_isqrt(9), 0, 0);
    printf("%d\n", my_isqrt(16), 0, 0);
    printf("%d\n", my_isqrt(100), 0, 0);
    printf("%d\n", my_isqrt(99), 0, 0);
    printf("%d\n", my_isqrt(1000), 0, 0);
    return 0;
}
