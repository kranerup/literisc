#include <stdio.h>

int main() {
    int A[9];
    int B[9];
    int C[9];
    int i, j, k, s;

    /* A row-major: [[1,2,3],[4,5,6],[7,8,9]] */
    A[0] = 1; A[1] = 2; A[2] = 3;
    A[3] = 4; A[4] = 5; A[5] = 6;
    A[6] = 7; A[7] = 8; A[8] = 9;

    /* B row-major: [[9,8,7],[6,5,4],[3,2,1]] */
    B[0] = 9; B[1] = 8; B[2] = 7;
    B[3] = 6; B[4] = 5; B[5] = 4;
    B[6] = 3; B[7] = 2; B[8] = 1;

    /* C = A * B */
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            s = 0;
            for (k = 0; k < 3; k++) {
                s = s + A[i*3+k] * B[k*3+j];
            }
            C[i*3+j] = s;
        }
    }

    for (i = 0; i < 3; i++) {
        printf("%d %d %d\n", C[i*3+0], C[i*3+1], C[i*3+2]);
    }

    return 0;
}
