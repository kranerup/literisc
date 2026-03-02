#include <stdio.h>

int main() {
  int A[3][3];
  int B[3][3];
  int C[3][3];
  int i, j, k;

  A[0][0]=1; A[0][1]=2; A[0][2]=3;
  A[1][0]=4; A[1][1]=5; A[1][2]=6;
  A[2][0]=7; A[2][1]=8; A[2][2]=9;

  B[0][0]=9; B[0][1]=8; B[0][2]=7;
  B[1][0]=6; B[1][1]=5; B[1][2]=4;
  B[2][0]=3; B[2][1]=2; B[2][2]=1;

  for (i = 0; i < 3; i++)
    for (j = 0; j < 3; j++) {
      int s = 0;
      for (k = 0; k < 3; k++) {
        int aik = A[i][k];
        int bkj = B[k][j];
        s = s + aik * bkj;
      }
      C[i][j] = s;
    }

  for (i = 0; i < 3; i++)
    printf("%d %d %d\n", C[i][0], C[i][1], C[i][2]);

  return 0;
}
