#include <stdio.h>

void quicksort(int *a, int lo, int hi) {
    int pivot, tmp, i, j;
    if (lo >= hi) return;
    pivot = a[lo];
    i = lo + 1;
    j = hi;
    while (i <= j) {
        if (a[i] <= pivot) {
            i++;
        } else {
            tmp = a[i];
            a[i] = a[j];
            a[j] = tmp;
            j--;
        }
    }
    /* place pivot */
    tmp = a[lo];
    a[lo] = a[j];
    a[j] = tmp;
    quicksort(a, lo, j - 1);
    quicksort(a, j + 1, hi);
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

    quicksort(a, 0, 11);

    for (i = 0; i < 12; i++) {
        printf("%d\n", a[i], 0, 0);
    }

    return 0;
}
