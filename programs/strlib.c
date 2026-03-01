#include <stdio.h>

int my_strlen(char *s) {
    int n;
    n = 0;
    while (*s) {
        n++;
        s++;
    }
    return n;
}

void my_strcpy(char *dst, char *src) {
    while (*src) {
        *dst = *src;
        dst++;
        src++;
    }
    *dst = 0;
}

int my_strcmp(char *a, char *b) {
    while (*a && *a == *b) {
        a++;
        b++;
    }
    return *a - *b;
}

int main() {
    char buf[32];
    int r;

    printf("strlen(hello) = %d\n", my_strlen("hello"), 0, 0);
    printf("strlen() = %d\n", my_strlen(""), 0, 0);
    printf("strlen(abcdefgh) = %d\n", my_strlen("abcdefgh"), 0, 0);

    my_strcpy(buf, "world");
    printf("strcpy: %d\n", my_strlen(buf), 0, 0);

    r = my_strcmp("abc", "abc");
    if (r == 0) printf("strcmp equal\n", 0, 0, 0);
    else printf("strcmp not equal\n", 0, 0, 0);

    r = my_strcmp("abc", "abd");
    if (r < 0) printf("strcmp less\n", 0, 0, 0);
    else printf("strcmp not less\n", 0, 0, 0);

    r = my_strcmp("abd", "abc");
    if (r > 0) printf("strcmp greater\n", 0, 0, 0);
    else printf("strcmp not greater\n", 0, 0, 0);

    return 0;
}
