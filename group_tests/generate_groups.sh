#!/bin/bash
# Generate 20 copies of wr_rd_field_test.c, each with exactly one
# TEST_GROUP_N define active (the rest commented out), so each group's
# register region can be compiled/run in isolation.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
API_DIR="$(dirname "$SCRIPT_DIR")"
SRC="$API_DIR/wr_rd_field_test.c"

# The 20 defines live on fixed lines in the source (TEST_GROUP_0..19).
FIRST_LINE=$(grep -n '^#define TEST_GROUP_0$' "$SRC" | head -1 | cut -d: -f1)
if [[ -z "$FIRST_LINE" ]]; then
  echo "Could not locate '#define TEST_GROUP_0' in $SRC" >&2
  exit 1
fi
LAST_LINE=$((FIRST_LINE + 19))

for i in $(seq 0 19); do
  OUT="$SCRIPT_DIR/wr_rd_field_test_group${i}.c"
  awk -v first="$FIRST_LINE" -v last="$LAST_LINE" -v group="$i" '
    NR >= first && NR <= last {
      n = NR - first
      if (n == group) {
        print "#define TEST_GROUP_" n
      } else {
        print "//#define TEST_GROUP_" n
      }
      next
    }
    { print }
  ' "$SRC" > "$OUT"
  echo "wrote $OUT (TEST_GROUP_${i} active)"
done
