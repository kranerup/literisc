#include <stdio.h>

int my_strlen(char *s) {
    int n;
    n = 0;
    while (s[n] != 0) {
        n = n + 1;
    }
    return n;
}

int is_palindrome(char *s) {
    int lo, hi;
    lo = 0;
    hi = my_strlen(s) - 1;
    while (lo < hi) {
        if (s[lo] != s[hi]) return 0;
        lo = lo + 1;
        hi = hi - 1;
    }
    return 1;
}

void check(char *s) {
    if (is_palindrome(s)) {
        puts("yes");
    } else {
        puts("no");
    }
}

int main() {
    check("racecar");
    check("hello");
    check("level");
    check("abcba");
    check("abc");
    return 0;
}
