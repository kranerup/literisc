#!/bin/bash
# Compile and run each single-group driver of the struct-API test
# (wr_rd_test_group<N>.c, which #includes wr_rd_test.c with one
# TEST_GROUP_N enabled) natively on the host. wr_rd_test.c runs against
# a malloc'ed device area and links against the struct API in
# flexswitch.c (compiled once, without FLEXSW_DEBUG so the logs stay
# clean for checksum comparison).
#
# By default all 20 groups run. To run a subset, pass group numbers
# and/or ranges as arguments, e.g.:
#   ./run_groups_struct.sh 3 7 12-15
#
# Compare the resulting logs against the field-API runs with
# compare_checksums.sh.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
API_DIR="$(dirname "$SCRIPT_DIR")/api"
RESULTS_DIR="$SCRIPT_DIR/results_struct"
TIMEOUT_SECS="${TIMEOUT_SECS:-60}"
CC="${CC:-clang}"

mkdir -p "$RESULTS_DIR"

groups_to_run=""
if [[ $# -eq 0 ]]; then
  groups_to_run=$(seq 0 19)
else
  for arg in "$@"; do
    if [[ "$arg" =~ ^[0-9]+$ ]]; then
      groups_to_run+=" $arg"
    elif [[ "$arg" =~ ^([0-9]+)-([0-9]+)$ ]]; then
      groups_to_run+=" $(seq "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}")"
    else
      echo "bad group spec: '$arg' (use N or N-M)" >&2
      exit 1
    fi
  done
fi

# The struct API itself is compiled once and linked into every driver.
FLEXSW_O="$RESULTS_DIR/flexswitch.o"
if ! "$CC" -I "$API_DIR" -c "$API_DIR/flexswitch.c" -o "$FLEXSW_O" \
     > "$RESULTS_DIR/flexswitch.log" 2>&1; then
  echo "failed to compile flexswitch.c, see $RESULTS_DIR/flexswitch.log" >&2
  exit 1
fi

pass=0
fail=0

for i in $groups_to_run; do
  SRC="$SCRIPT_DIR/wr_rd_test_group${i}.c"
  BIN="$RESULTS_DIR/group${i}.bin"
  LOG="$RESULTS_DIR/group${i}.log"

  if [[ ! -f "$SRC" ]]; then
    echo "group $i: SKIP (missing $SRC, run generate_groups.sh first)"
    continue
  fi

  printf 'group %2d ... ' "$i"

  if ! "$CC" -I "$API_DIR" "$SRC" "$FLEXSW_O" -o "$BIN" > "$LOG" 2>&1; then
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
