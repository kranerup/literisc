#include <stdio.h>

int stk[16];
int stk_top;

void push(int v) {
    stk[stk_top] = v;
    stk_top = stk_top + 1;
}

int pop() {
    stk_top = stk_top - 1;
    return stk[stk_top];
}

int peek() {
    return stk[stk_top - 1];
}

int depth() {
    return stk_top;
}

int main() {
    int i, v;

    stk_top = 0;

    for (i = 1; i <= 8; i++) {
        push(i);
    }

    /* pop 4 items */
    for (i = 0; i < 4; i++) {
        v = pop();
        printf("%d\n", v, 0, 0);
    }

    /* peek top */
    v = peek();
    printf("%d\n", v, 0, 0);

    /* print depth */
    v = depth();
    printf("%d\n", v, 0, 0);

    return 0;
}
