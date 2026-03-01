#include <stdio.h>

int bsearch(int *a, int n, int target) {
    int lo, hi, mid;
    lo = 0;
    hi = n - 1;
    while (lo <= hi) {
        mid = (lo + hi) / 2;
        if (a[mid] == target) return mid;
        if (a[mid] < target) lo = mid + 1;
        else hi = mid - 1;
    }
    return -1;
}

int main() {
    int a[16];
    int result;

    a[0]  = 2;
    a[1]  = 5;
    a[2]  = 8;
    a[3]  = 12;
    a[4]  = 16;
    a[5]  = 23;
    a[6]  = 38;
    a[7]  = 45;
    a[8]  = 56;
    a[9]  = 72;
    a[10] = 91;
    a[11] = 103;
    a[12] = 117;
    a[13] = 134;
    a[14] = 150;
    a[15] = 199;

    result = bsearch(a, 16, 23);
    if (result >= 0) printf("found %d\n", 23, 0, 0);
    else printf("not found\n", 0, 0, 0);

    result = bsearch(a, 16, 100);
    if (result >= 0) printf("found %d\n", 100, 0, 0);
    else printf("not found\n", 0, 0, 0);

    result = bsearch(a, 16, 2);
    if (result >= 0) printf("found %d\n", 2, 0, 0);
    else printf("not found\n", 0, 0, 0);

    result = bsearch(a, 16, 199);
    if (result >= 0) printf("found %d\n", 199, 0, 0);
    else printf("not found\n", 0, 0, 0);

    result = bsearch(a, 16, 1);
    if (result >= 0) printf("found %d\n", 1, 0, 0);
    else printf("not found\n", 0, 0, 0);

    result = bsearch(a, 16, 56);
    if (result >= 0) printf("found %d\n", 56, 0, 0);
    else printf("not found\n", 0, 0, 0);

    return 0;
}
