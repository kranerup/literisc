#!/bin/bash
# Compare the device memory produced by the struct-API test (wr_rd_test.c,
# compiled natively as the golden reference) against the memory produced by
# the field-API test (wr_rd_field_test.c, compiled with lrcc and run on the
# liteRISC emulator). The two tests are generated in lockstep (same
# PRNG/seeds, same registers, same TEST_GROUP grouping, same
# first/last-entry traversal), so with the same groups enabled their device
# memories must be identical -- any difference means the field API (or the
# liteRISC toolchain) packed a field differently than the struct API did.
#
# Memory extraction:
#  - struct test (host): fwrites its malloc'ed device area to a binary file,
#    which is then converted to a sparse text form.
#  - field test (emulator): liteRISC has no filesystem, so it prints every
#    non-zero conf word as a line "M <addr> <hi16> <lo16>" (liteRISC printf
#    only supports %d with at most 3 arguments, and %d is signed -- printing
#    two 16-bit halves keeps every value positive). The script greps these
#    lines out of the emulator's stdout.
# Since both memories start zeroed, comparing the sparse non-zero words
# compares the entire memory state.
#
# Usage:
#   compare_memory.sh           # all test groups enabled
#   compare_memory.sh <N>       # only TEST_GROUP_<N> enabled in both tests
#
# Env overrides:
#   CC             host compiler for the struct test (default gcc)
#   API_DIR        dir with the generated sources (default ../ or ../api)
#   CONF_MEM_SIZE  bytes backing the emulator conf window (default 10000000)
#   SCAN_WORDS     conf words the field test scans for the dump (default:
#                  2 * last non-zero word of the struct reference + 4096,
#                  capped to CONF_MEM_SIZE/4)
#   TIMEOUT_SECS   timeout for the emulator run (default 1800)
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
API_DIR="${API_DIR:-$ROOT_DIR}"
# The liteRISC checkout only has the generated API via its 'api' symlink.
if [[ ! -f "$API_DIR/wr_rd_test.c" && -f "$API_DIR/api/wr_rd_test.c" ]]; then
  API_DIR="$API_DIR/api"
fi

GROUP="${1:-all}"
CC="${CC:-gcc}"
LRCC="$ROOT_DIR/lrcc.lisp"
CONF_MEM_SIZE="${CONF_MEM_SIZE:-10000000}"
TIMEOUT_SECS="${TIMEOUT_SECS:-1800}"
DUMP_WORDS=16777216   # words the host reference dumps (64 MiB, cheap)
NR_GROUPS=20

OUT="$SCRIPT_DIR/results_memcmp"
mkdir -p "$OUT"

for f in "$API_DIR/wr_rd_test.c" "$API_DIR/wr_rd_field_test.c" \
         "$API_DIR/flexswitch.c" "$LRCC" "$ROOT_DIR/include/stdio.h"; do
  if [[ ! -f "$f" ]]; then
    echo "missing $f (run genapi / check API_DIR)" >&2
    exit 2
  fi
done

# Copy a test source, inserting dump code right before the final summary
# printf (i.e. after all writes and reads have run).
add_dump() {  # src dst dump-c-code
  awk -v dump="$3" '
    /printf\("Correct values/ && !done { print dump; done=1 }
    { print }
  ' "$1" > "$2"
}

# Rewrite the TEST_GROUP define block so only one group stays enabled
# (same line-based approach as generate_groups.sh).
select_group() {  # file group
  local first
  first=$(grep -n '^#define TEST_GROUP_0$' "$1" | head -1 | cut -d: -f1)
  if [[ -z "$first" ]]; then
    echo "could not locate '#define TEST_GROUP_0' in $1" >&2
    exit 2
  fi
  awk -v first="$first" -v last="$((first + NR_GROUPS - 1))" -v group="$2" '
    NR >= first && NR <= last {
      n = NR - first
      if (n == group) { print "#define TEST_GROUP_" n }
      else            { print "//#define TEST_GROUP_" n }
      next
    }
    { print }
  ' "$1" > "$1.tmp" && mv "$1.tmp" "$1"
}

# Binary word dump -> sparse "M <addr> <hi16> <lo16>" text, one line per
# non-zero word (the same format the field test prints on the emulator).
to_sparse() {  # binfile
  od -An -tu4 -v "$1" | awk '
    { for (i = 1; i <= NF; i++) {
        v = $i + 0
        if (v != 0) printf "M %d %d %d\n", idx, int(v / 65536), v % 65536
        idx++
      } }'
}

STRUCT_SRC="$OUT/struct_test.c"
FIELD_SRC="$OUT/field_test.c"
STRUCT_MEM="$OUT/mem_struct.bin"

# ---------------- struct-API reference (native) ----------------
add_dump "$API_DIR/wr_rd_test.c" "$STRUCT_SRC" \
  "  { FILE* dfp = fopen(\"$STRUCT_MEM\",\"wb\"); fwrite((const void*)device_ptr, 4, ${DUMP_WORDS}UL, dfp); fclose(dfp); }"
if [[ "$GROUP" != "all" ]]; then
  echo "restricting both tests to TEST_GROUP_${GROUP}"
  select_group "$STRUCT_SRC" "$GROUP"
fi

echo "compiling struct-API test (native reference) ..."
$CC -O1 -w -I "$API_DIR" "$STRUCT_SRC" "$API_DIR/flexswitch.c" \
  -o "$OUT/struct_test" 2> "$OUT/struct_build.log" || {
  echo "struct test build FAILED, see $OUT/struct_build.log" >&2; exit 1; }

rm -f "$STRUCT_MEM"
echo "running struct-API test ..."
"$OUT/struct_test" > "$OUT/struct_run.log" 2>&1
struct_status=$?
tail -2 "$OUT/struct_run.log" | sed 's/^/  /'
[[ ! -s "$STRUCT_MEM" ]] && { echo "struct memory dump missing"; exit 1; }

to_sparse "$STRUCT_MEM" > "$OUT/mem_struct.txt"

# ---------------- field-API test (liteRISC emulator) ----------------
# Scan bound for the emulator-side dump: generous margin over the highest
# word the reference wrote, so stray field-API writes past the expected
# region still show up, without scanning the entire conf window.
if [[ -z "${SCAN_WORDS:-}" ]]; then
  max_word=$(tail -1 "$OUT/mem_struct.txt" | awk '{print $2 + 0}')
  SCAN_WORDS=$((max_word * 2 + 4096))
fi
cap=$((CONF_MEM_SIZE / 4))
[[ $SCAN_WORDS -gt $cap ]] && SCAN_WORDS=$cap
echo "field test will scan $SCAN_WORDS conf words for the dump"

add_dump "$API_DIR/wr_rd_field_test.c" "$FIELD_SRC" \
  "  { uint32_t a; uint32_t v; for (a = 0; a < $SCAN_WORDS; a++) { v = readFromDevice(a, 0); if (v != 0) { printf(\"M %d %d %d\\n\", (int)a, (int)(v >> 16), (int)(v & 0xFFFF)); } } }"
if [[ "$GROUP" != "all" ]]; then
  select_group "$FIELD_SRC" "$GROUP"
fi

echo "compiling + running field-API test on the liteRISC emulator ..."
start=$(date +%s)
timeout "$TIMEOUT_SECS" "$LRCC" "$FIELD_SRC" -Os -I "$ROOT_DIR/include" -I "$API_DIR" \
  -r --conf-mem-size "$CONF_MEM_SIZE" > "$OUT/field_run.log" 2> "$OUT/field_run.err"
field_status=$?
elapsed=$(( $(date +%s) - start ))
if [[ $field_status -eq 124 ]]; then
  echo "field test TIMEOUT after ${elapsed}s (see $OUT/field_run.log)" >&2
  exit 1
fi
grep -E "Correct values|test passed|test FAILED" "$OUT/field_run.log" | sed 's/^/  /'
echo "  (emulator run took ${elapsed}s)"

grep '^M ' "$OUT/field_run.log" > "$OUT/mem_field.txt"

# ---------------- compare ----------------
fail=0
[[ $struct_status -ne 0 ]] && { echo "struct test exited $struct_status (see $OUT/struct_run.log)"; fail=1; }
[[ $field_status -ne 0 ]] && { echo "field test exited $field_status (see $OUT/field_run.log)"; fail=1; }

echo "comparing device memory (struct: native, field: emulator) ..."
if diff -q "$OUT/mem_struct.txt" "$OUT/mem_field.txt" > /dev/null; then
  echo "MEMORY IDENTICAL ($(wc -l < "$OUT/mem_struct.txt") non-zero words)"
else
  echo "MEMORY DIFFERS (word: value as <hi16> <lo16>):"
  diff "$OUT/mem_struct.txt" "$OUT/mem_field.txt" | head -30
  echo "  full dumps: $OUT/mem_struct.txt $OUT/mem_field.txt"
  fail=1
fi

exit $fail
