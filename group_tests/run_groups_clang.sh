#!/bin/bash
# Like run_groups.sh, but compiles+runs each isolated TEST_GROUP_N copy
# natively with clang instead of through lrcc/the liteRISC emulator. This
# is for checking whether a failure is a liteRISC-specific compiler bug or
# a bug in the (externally generated) flexswitch_fields.h itself: if a
# group fails here too, it fails on the reference toolchain, not just ours.
#
# Native builds skip liteRISC's own include/stdio.h (its custom putchar
# writes to a fixed volatile MMIO address, which doesn't exist as a real
# process) by NOT passing that include dir, so <stdio.h> resolves to the
# host libc instead. conf_low_shim.c mmaps real memory at flexswitch_fields.h's
# fixed CONF_LOW address so its readFromDevice/writeToDevice work unmodified.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
API_DIR="$(dirname "$SCRIPT_DIR")"
SHIM="$SCRIPT_DIR/conf_low_shim.c"
RESULTS_DIR="$SCRIPT_DIR/results_clang"
TIMEOUT_SECS="${TIMEOUT_SECS:-60}"

mkdir -p "$RESULTS_DIR"

pass=0
fail=0

for i in $(seq 0 19); do
  SRC="$SCRIPT_DIR/wr_rd_field_test_group${i}.c"
  PRE="$RESULTS_DIR/group${i}.pre.c"
  BIN="$RESULTS_DIR/group${i}.bin"
  LOG="$RESULTS_DIR/group${i}.log"

  if [[ ! -f "$SRC" ]]; then
    echo "group $i: SKIP (missing $SRC, run generate_groups.sh first)"
    continue
  fi

  printf 'group %2d ... ' "$i"

  if ! clang -E -P -I "$API_DIR" "$SRC" > "$PRE" 2> "$LOG"; then
    echo "PREPROCESS FAILED  see $LOG"
    fail=$((fail + 1))
    continue
  fi

  if ! clang "$PRE" "$SHIM" -o "$BIN" >> "$LOG" 2>&1; then
    echo "COMPILE FAILED  see $LOG"
    fail=$((fail + 1))
    continue
  fi

  start=$(date +%s)
  timeout "$TIMEOUT_SECS" "$BIN" > "$LOG" 2>&1
  status=$?
  end=$(date +%s)
  elapsed=$((end - start))

  if [[ $status -eq 124 ]]; then
    echo "TIMEOUT (${elapsed}s)  see $LOG"
    fail=$((fail + 1))
  elif [[ $status -ne 0 ]]; then
    echo "FAIL (exit $status, ${elapsed}s)  see $LOG"
    fail=$((fail + 1))
  else
    echo "PASS (${elapsed}s)"
    pass=$((pass + 1))
  fi
done

echo
echo "Results: $pass passed, $fail failed (logs/binaries in $RESULTS_DIR)"
