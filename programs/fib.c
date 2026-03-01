#include <stdio.h>

int main() {
    int a, b, tmp, i;
    a = 0;
    b = 1;
    printf("%d\n", a, 0, 0);
    for (i = 1; i <= 15; i++) {
        printf("%d\n", b, 0, 0);
        tmp = a + b;
        a = b;
        b = tmp;
    }
    return 0;
}
