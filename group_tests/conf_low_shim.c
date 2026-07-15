// flexswitch_fields.h's readFromDevice/writeToDevice dereference a raw
// pointer at the fixed address CONF_LOW (65536). Native (non-liteRISC)
// builds need real memory mapped there before main() runs, or the very
// first device access segfaults. Link this file in alongside a group test
// when compiling with a normal host compiler (clang/gcc) instead of lrcc.
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>

#define CONF_LOW_BASE ((void *)65536)
#define CONF_LOW_SIZE (64UL * 1024 * 1024)

__attribute__((constructor))
static void map_conf_low(void) {
  void *base = mmap(CONF_LOW_BASE, CONF_LOW_SIZE, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
  if (base == MAP_FAILED) {
    perror("mmap CONF_LOW");
    _exit(100);
  }
}
