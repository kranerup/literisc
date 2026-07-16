// Host-side backing memory for flexswitch_fields.h's device area when a
// group driver is compiled natively (run_groups_clang.sh).
//
// On liteRISC the field API's readFromDevice/writeToDevice hit the
// configuration memory at the fixed address CONF_LOW. On the host that
// address is not mappable (vm.mmap_min_addr), so run_groups_clang.sh
// instead preprocesses the test with
//   -DCONF_LOW='((unsigned long)conf_low_mem)'
// making all device accesses land in this zero-initialized array. Its
// size matches the --conf-mem-size (in 32-bit words) that run_groups.sh
// passes to lrcc; override with -DCONF_LOW_SHIM_WORDS=<n> if needed.

#ifndef CONF_LOW_SHIM_WORDS
#define CONF_LOW_SHIM_WORDS 10000000
#endif

unsigned int conf_low_mem[CONF_LOW_SHIM_WORDS];
