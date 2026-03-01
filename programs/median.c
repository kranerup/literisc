#include <stdio.h>

void isort(int *a, int n) {
    int i, j, key;
    for (i = 1; i < n; i++) {
        key = a[i];
        j = i - 1;
        while (j >= 0 && a[j] > key) {
            a[j + 1] = a[j];
            j = j - 1;
        }
        a[j + 1] = key;
    }
}

int main() {
    int a[9];
    int n;

    a[0] = 42;
    a[1] = 17;
    a[2] = 93;
    a[3] = 5;
    a[4] = 61;
    a[5] = 28;
    a[6] = 74;
    a[7] = 36;
    a[8] = 50;

    n = 9;
    isort(a, n);
    printf("%d\n", a[n / 2], 0, 0);

    return 0;
}
