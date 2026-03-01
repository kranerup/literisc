#include <stdio.h>

int tmp[12];

void my_merge(int *a, int lo, int mid, int hi) {
    int i, j, k;
    i = lo;
    j = mid + 1;
    k = lo;
    while (i <= mid && j <= hi) {
        if (a[i] <= a[j]) {
            tmp[k] = a[i];
            i = i + 1;
        } else {
            tmp[k] = a[j];
            j = j + 1;
        }
        k = k + 1;
    }
    while (i <= mid) {
        tmp[k] = a[i];
        i = i + 1;
        k = k + 1;
    }
    while (j <= hi) {
        tmp[k] = a[j];
        j = j + 1;
        k = k + 1;
    }
    for (i = lo; i <= hi; i++) {
        a[i] = tmp[i];
    }
}

void my_mergesort(int *a, int lo, int hi) {
    int mid;
    if (lo >= hi) return;
    mid = (lo + hi) / 2;
    my_mergesort(a, lo, mid);
    my_mergesort(a, mid + 1, hi);
    my_merge(a, lo, mid, hi);
}

int main() {
    int a[12];
    int i;

    a[0]  = 38;
    a[1]  = 27;
    a[2]  = 43;
    a[3]  = 3;
    a[4]  = 9;
    a[5]  = 82;
    a[6]  = 10;
    a[7]  = 14;
    a[8]  = 55;
    a[9]  = 21;
    a[10] = 67;
    a[11] = 5;

    my_mergesort(a, 0, 11);

    for (i = 0; i < 12; i++) {
        printf("%d\n", a[i], 0, 0);
    }

    return 0;
}
