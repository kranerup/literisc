#define _GNU_SOURCE
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include "flexswitch_fields.h"

int main() {
  void *base = mmap((void*)65536, 4*1024*1024, PROT_READ|PROT_WRITE,
                     MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED, -1, 0);
  if (base == MAP_FAILED) { perror("mmap"); return 100; }

  uint64_t vals[] = { 0x123456789ABCULL, 0xFFFFFFFFFFFFULL, 0xABCDEF012345ULL, 0 };
  int ok = 1;
  for (int t = 0; t < 4; t++) {
    uint64_t v = vals[t];
    uint64_t got_da = 0, got_end = 0;

    wr_IngressL2ACLMatchDataEntries_daMac(0, v);
    rd_IngressL2ACLMatchDataEntries_daMac(0, &got_da);
    printf("daMac  in=%016llx got=%016llx %s\n",
           (unsigned long long)v, (unsigned long long)got_da,
           v == got_da ? "OK" : "<-- MISMATCH");
    if (v != got_da) ok = 0;

    wr_IngressVIDMACRangeSearchData_end(0, v);
    rd_IngressVIDMACRangeSearchData_end(0, &got_end);
    printf("end    in=%016llx got=%016llx %s\n",
           (unsigned long long)v, (unsigned long long)got_end,
           v == got_end ? "OK" : "<-- MISMATCH");
    if (v != got_end) ok = 0;
  }
  return ok ? 0 : 1;
}
