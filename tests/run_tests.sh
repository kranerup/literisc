#!/usr/bin/env bash
#
# Run the c-testsuite tests (tests/*.c).
# These are simple C programs that return 0 on success, non-zero on failure.
# lrcc exit code reflects the program's return value (via logand with 255).
#
# Usage: cd tests && bash run_tests.sh
#        or: tests/run_tests.sh (from repo root)

SCRIPTDIR=$(cd "$(dirname "$0")" && pwd)
LRCC="$SCRIPTDIR/../lrcc.lisp"

pass=0
fail=0

for f in "$SCRIPTDIR"/*.c; do
    if "$LRCC" -r "$f" > /dev/null 2>&1; then
        echo "PASS  $(basename "$f")"
        pass=$((pass + 1))
    else
        echo "FAIL  $(basename "$f")"
        fail=$((fail + 1))
    fi
done

echo ""
echo "Results: $pass passed, $fail failed"
[ $fail -eq 0 ]
