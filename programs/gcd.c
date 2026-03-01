#include <stdio.h>

int my_gcd(int a, int b) {
    int tmp;
    while (b != 0) {
        tmp = b;
        b = a % b;
        a = tmp;
    }
    return a;
}

int main() {
    printf("gcd(48,18) = %d\n", my_gcd(48, 18), 0, 0);
    printf("gcd(100,75) = %d\n", my_gcd(100, 75), 0, 0);
    printf("gcd(17,13) = %d\n", my_gcd(17, 13), 0, 0);
    printf("gcd(0,5) = %d\n", my_gcd(0, 5), 0, 0);
    return 0;
}
