#!/bin/bash
# Compile and run each single-group driver (which #includes
# wr_rd_field_test.c with one TEST_GROUP_N enabled) with the usual lrcc
# invocation, logging pass/fail per group so a failing register region
# can be bisected without compiling the full (all-groups) test file.
#
# By default all 20 groups run. To run a subset, pass group numbers
# and/or ranges as arguments, e.g.:
#   ./run_groups.sh 3 7 12-15
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo $SCRIPT_DIR
API_DIR="$(dirname "$SCRIPT_DIR")/api"
echo $API_DIR
ROOT_DIR="$(dirname "$API_DIR")"
echo $ROOT_DIR
LRCC="$ROOT_DIR/lrcc.lisp"
RESULTS_DIR="$SCRIPT_DIR/results"
TIMEOUT_SECS="${TIMEOUT_SECS:-300}"

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

pass=0
fail=0

for i in $groups_to_run; do
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
