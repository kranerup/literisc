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
  uint32_t address = 56818 + idx*16;
  uint8_t destIPAddr[16]     = {11,22,33,44, 55,66,77,88, 99,100,101,102, 103,104,105,106};

  wr_L3RoutingTCAM_proto(idx, 2);
  printf("after proto:        word4=%08x\n", readFromDevice(address+4,0));
  wr_L3RoutingTCAM_vrf(idx, 1);
  printf("after vrf:          word4=%08x\n", readFromDevice(address+4,0));
  wr_L3RoutingTCAM_destIPAddr(idx, destIPAddr);
  printf("after destIPAddr:   word4=%08x\n", readFromDevice(address+4,0));
  wr_L3RoutingTCAM_protoMaskN(idx, 3);
  printf("after protoMaskN:   word4=%08x\n", readFromDevice(address+4,0));
  wr_L3RoutingTCAM_vrfMaskN(idx, 0);
  printf("after vrfMaskN:     word4=%08x\n", readFromDevice(address+4,0));

  uint32_t g;
  rd_L3RoutingTCAM_vrfMaskN(idx, &g);
  printf("rd_vrfMaskN -> %u\n", g);
  return 0;
}
