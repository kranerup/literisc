// Reproduce the L3RoutingTCAM row write/read sequence exactly as generated
// (proto, vrf, destIPAddr, protoMaskN, vrfMaskN, destIPAddrMaskN, valid) to
// check whether destIPAddr and destIPAddrMaskN corrupt each other via
// their shared device word, or whether each is wrong independently.
#define _GNU_SOURCE
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include "flexswitch_fields.h"

int main() {
  void *base = mmap((void*)65536, 4*1024*1024, PROT_READ|PROT_WRITE,
                     MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED, -1, 0);
  if (base == MAP_FAILED) { perror("mmap"); return 100; }

  uint32_t idx = 0;
  uint32_t proto = 2, vrf = 1, protoMaskN = 3, vrfMaskN = 0, valid = 1;
  uint8_t destIPAddr[16]     = {11,22,33,44, 55,66,77,88, 99,100,101,102, 103,104,105,106};
  uint8_t destIPAddrMaskN[16] = {211,212,213,214, 215,216,217,218, 219,220,221,222, 223,224,225,226};

  wr_L3RoutingTCAM_proto(idx, proto);
  wr_L3RoutingTCAM_vrf(idx, vrf);
  wr_L3RoutingTCAM_destIPAddr(idx, destIPAddr);
  wr_L3RoutingTCAM_protoMaskN(idx, protoMaskN);
  wr_L3RoutingTCAM_vrfMaskN(idx, vrfMaskN);
  wr_L3RoutingTCAM_destIPAddrMaskN(idx, destIPAddrMaskN);
  wr_L3RoutingTCAM_valid(idx, valid);

  uint32_t g_proto, g_vrf, g_protoMaskN, g_vrfMaskN, g_valid;
  uint8_t g_destIPAddr[16], g_destIPAddrMaskN[16];
  rd_L3RoutingTCAM_proto(idx, &g_proto);
  rd_L3RoutingTCAM_vrf(idx, &g_vrf);
  rd_L3RoutingTCAM_destIPAddr(idx, g_destIPAddr);
  rd_L3RoutingTCAM_protoMaskN(idx, &g_protoMaskN);
  rd_L3RoutingTCAM_vrfMaskN(idx, &g_vrfMaskN);
  rd_L3RoutingTCAM_destIPAddrMaskN(idx, g_destIPAddrMaskN);
  rd_L3RoutingTCAM_valid(idx, &g_valid);

  int ok = 1;
  #define CHK(name, got, exp) do { \
    printf("%-10s got=%u expected=%u%s\n", name, (unsigned)(got), (unsigned)(exp), \
           ((got)==(exp)) ? "" : " <-- MISMATCH"); \
    if ((got) != (exp)) ok = 0; \
  } while (0)

  CHK("proto", g_proto, proto);
  CHK("vrf", g_vrf, vrf);
  CHK("protoMaskN", g_protoMaskN, protoMaskN);
  CHK("vrfMaskN", g_vrfMaskN, vrfMaskN);
  CHK("valid", g_valid, valid);
  for (int k = 0; k < 16; k++) {
    char label[32];
    sprintf(label, "destIPAddr[%d]", k);
    CHK(label, g_destIPAddr[k], destIPAddr[k]);
  }
  for (int k = 0; k < 16; k++) {
    char label[32];
    sprintf(label, "maskN[%d]", k);
    CHK(label, g_destIPAddrMaskN[k], destIPAddrMaskN[k]);
  }

  return ok ? 0 : 1;
}
