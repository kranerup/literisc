// Standalone reproduction of the L3RoutingTCAM destIPAddrMaskN round-trip
// bug, isolated from the rest of wr_rd_field_test.c.
#include <stdio.h>
#include "flexswitch_fields.h"

int main() {
  uint8_t in[16]  = {11,22,33,44, 55,66,77,88, 99,100,101,102, 103,104,105,106};
  uint8_t out[16] = {0};

  wr_L3RoutingTCAM_destIPAddrMaskN(0, in);
  rd_L3RoutingTCAM_destIPAddrMaskN(0, out);

  int ok = 1;
  for (int k = 0; k < 16; k++) {
    printf("out[%d]=%d expected=%d", k, out[k], in[k]);
    if (out[k] != in[k]) {
      printf(" <-- MISMATCH");
      ok = 0;
    }
    printf("\n");
  }
  return ok ? 0 : 1;
}
