#!/bin/bash
# Compile and run each isolated TEST_GROUP_N copy with the usual lrcc
# invocation, logging pass/fail per group so a failing register region
# can be bisected without compiling the full (all-groups) test file.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
API_DIR="$(dirname "$SCRIPT_DIR")"
ROOT_DIR="$(dirname "$API_DIR")"
LRCC="$ROOT_DIR/lrcc.lisp"
RESULTS_DIR="$SCRIPT_DIR/results"
TIMEOUT_SECS="${TIMEOUT_SECS:-300}"

mkdir -p "$RESULTS_DIR"

pass=0
fail=0

for i in $(seq 0 19); do
  SRC="$SCRIPT_DIR/wr_rd_field_test_group${i}.c"
  LOG="$RESULTS_DIR/group${i}.log"

  if [[ ! -f "$SRC" ]]; then
    echo "group $i: SKIP (missing $SRC, run generate_groups.sh first)"
    continue
  fi

  printf 'group %2d ... ' "$i"
  start=$(date +%s)
  timeout "$TIMEOUT_SECS" "$LRCC" "$SRC" -Os -I "$ROOT_DIR/include" -I "$API_DIR" -r --conf-mem-size 10000000 \
    > "$LOG" 2>&1
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
echo "Results: $pass passed, $fail failed (logs in $RESULTS_DIR)"
