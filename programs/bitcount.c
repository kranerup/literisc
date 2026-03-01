#include <stdio.h>

int popcount(int n) {
    int count;
    count = 0;
    while (n != 0) {
        n = n & (n - 1);
        count = count + 1;
    }
    return count;
}

int main() {
    printf("%d %d\n", 0, popcount(0), 0);
    printf("%d %d\n", 1, popcount(1), 0);
    printf("%d %d\n", 7, popcount(7), 0);
    printf("%d %d\n", 255, popcount(255), 0);
    printf("%d %d\n", 256, popcount(256), 0);
    printf("%d %d\n", 1023, popcount(1023), 0);
    printf("%d %d\n", 65535, popcount(65535), 0);
    return 0;
}
