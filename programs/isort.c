#include <stdio.h>

int main() {
    int a[10];
    int i, j, key;

    a[0] = 64;
    a[1] = 25;
    a[2] = 12;
    a[3] = 22;
    a[4] = 11;
    a[5] = 90;
    a[6] = 3;
    a[7] = 47;
    a[8] = 55;
    a[9] = 8;

    for (i = 1; i < 10; i++) {
        key = a[i];
        j = i - 1;
        while (j >= 0 && a[j] > key) {
            a[j + 1] = a[j];
            j = j - 1;
        }
        a[j + 1] = key;
    }

    for (i = 0; i < 10; i++) {
        printf("%d\n", a[i], 0, 0);
    }

    return 0;
}
