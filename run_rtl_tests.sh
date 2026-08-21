#!/usr/bin/env bash
#
# run_rtl_tests.sh - Run C programs through both the Lisp emulator (lrcc -r)
# and the MyHDL RTL simulation (lrcc --rtl), and check they agree on both
# stdout and exit code. The emulator's output is treated as the reference;
# a mismatch means the RTL model produced different output/behavior for
# the same program.
#
# Usage:
#   ./run_rtl_tests.sh file1.c file2.c ...
#   ./run_rtl_tests.sh tests/*.c
#   ./run_rtl_tests.sh --cycles 50000 test_stdio.c
#
# Options:
#   --cycles <n>   RTL clock cycles to run per program (default: 20000,
#                   matching lrcc --rtl's own default)
#
# Exits 0 if all given programs agree between emulator and RTL, 1 otherwise.

SCRIPTDIR=$(cd "$(dirname "$0")" && pwd)
LRCC="$SCRIPTDIR/lrcc.lisp"
CYCLES=20000

files=()
while [ $# -gt 0 ]; do
    case "$1" in
        --cycles)
            CYCLES="$2"
            shift 2
            ;;
        *)
            files+=("$1")
            shift
            ;;
    esac
done

if [ ${#files[@]} -eq 0 ]; then
    echo "Usage: $0 [--cycles <n>] file1.c file2.c ..." >&2
    echo "       $0 tests/*.c" >&2
    exit 1
fi

pass=0
fail=0

for f in "${files[@]}"; do
    name=$(basename "$f")

    emu_out=$("$LRCC" -I "$SCRIPTDIR/include" -Os -r "$f" 2>/dev/null)
    emu_code=$?

    rtl_out=$("$LRCC" -I "$SCRIPTDIR/include" -Os --rtl --rtl-cycles "$CYCLES" "$f" 2>/dev/null)
    rtl_code=$?

    if [ "$emu_out" == "$rtl_out" ] && [ "$emu_code" -eq "$rtl_code" ]; then
        echo "PASS  $name"
        pass=$((pass + 1))
    else
        echo "FAIL  $name"
        echo "        emulator: exit=$emu_code output=$(printf '%q' "$emu_out")"
        echo "        rtl:      exit=$rtl_code output=$(printf '%q' "$rtl_out")"
        fail=$((fail + 1))
    fi
done

echo ""
echo "Results: $pass passed, $fail failed"
[ $fail -eq 0 ]
