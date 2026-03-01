#include <stdio.h>

int my_pow(int base, int exp) {
    int result, i;
    result = 1;
    for (i = 0; i < exp; i++) {
        result = result * base;
    }
    return result;
}

int main() {
    printf("%d\n", my_pow(2, 0), 0, 0);
    printf("%d\n", my_pow(2, 10), 0, 0);
    printf("%d\n", my_pow(3, 5), 0, 0);
    printf("%d\n", my_pow(5, 3), 0, 0);
    printf("%d\n", my_pow(10, 4), 0, 0);
    return 0;
}
