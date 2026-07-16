#!/bin/bash
# Compare the write (CHK) and read (RDCHK) checksum lines between the
# field-API test logs (wr_rd_field_test, from run_groups.sh's results/
# by default, or run_groups_clang.sh's results_clang/ with --clang) and
# the struct-API test logs (wr_rd_test, from run_groups_struct.sh's
# results_struct/), group by group.
#
# All CHK lines are compared in order, and likewise all RDCHK lines, so
# this works both for the default one-checksum-at-the-end output and
# for per-register TRACE_WR_CHECKSUM/TRACE_RD_CHECKSUM output -- in the
# trace case the first differing line pinpoints the register where the
# two APIs diverge.
#
# Usage: ./compare_checksums.sh [--clang] [groups...]
#   groups: group numbers and/or ranges (e.g. 3 7 12-15); default all.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FIELD_RESULTS="$SCRIPT_DIR/results"
STRUCT_RESULTS="$SCRIPT_DIR/results_struct"

group_args=()
for arg in "$@"; do
  if [[ "$arg" == "--clang" ]]; then
    FIELD_RESULTS="$SCRIPT_DIR/results_clang"
  else
    group_args+=("$arg")
  fi
done

groups_to_run=""
if [[ ${#group_args[@]} -eq 0 ]]; then
  groups_to_run=$(seq 0 19)
else
  for arg in "${group_args[@]}"; do
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

match=0
mismatch=0
skipped=0

for i in $groups_to_run; do
  FLOG="$FIELD_RESULTS/group${i}.log"
  SLOG="$STRUCT_RESULTS/group${i}.log"

  if [[ ! -f "$FLOG" || ! -f "$SLOG" ]]; then
    [[ -f "$FLOG" ]] || echo "group $i: SKIP (missing $FLOG)"
    [[ -f "$SLOG" ]] || echo "group $i: SKIP (missing $SLOG)"
    skipped=$((skipped + 1))
    continue
  fi

  group_ok=1
  summary=""
  details=""

  for tag in CHK RDCHK; do
    fvals=$(grep "^${tag} " "$FLOG" || true)
    svals=$(grep "^${tag} " "$SLOG" || true)

    if [[ -z "$fvals" || -z "$svals" ]]; then
      group_ok=0
      [[ -n "$fvals" ]] || details+="  no ${tag} lines in $FLOG"$'\n'
      [[ -n "$svals" ]] || details+="  no ${tag} lines in $SLOG"$'\n'
      summary+="${tag} MISSING  "
      continue
    fi

    if [[ "$fvals" == "$svals" ]]; then
      summary+="${tag} OK  "
    else
      group_ok=0
      summary+="${tag} MISMATCH  "
      fcnt=$(wc -l <<< "$fvals")
      scnt=$(wc -l <<< "$svals")
      if [[ "$fcnt" != "$scnt" ]]; then
        details+="  ${tag}: field log has $fcnt lines, struct log has $scnt"
        details+=" (was one side built with TRACE_*_CHECKSUM and the other not?)"$'\n'
      fi
      # First differing line (with TRACE_* this pinpoints the register).
      firstdiff=$(diff <(echo "$fvals") <(echo "$svals") | grep -m1 '^[0-9]' || true)
      fline=$(diff <(echo "$fvals") <(echo "$svals") | grep -m1 '^<' | cut -c3- || true)
      sline=$(diff <(echo "$fvals") <(echo "$svals") | grep -m1 '^>' | cut -c3- || true)
      details+="  ${tag} first difference (${firstdiff}):"$'\n'
      details+="    field:  ${fline:-<none>}"$'\n'
      details+="    struct: ${sline:-<none>}"$'\n'
    fi
  done

  printf 'group %2d ... %s\n' "$i" "$summary"
  [[ -z "$details" ]] || printf '%s' "$details"

  if [[ $group_ok -eq 1 ]]; then
    match=$((match + 1))
  else
    mismatch=$((mismatch + 1))
  fi
done

echo
echo "Checksums: $match groups match, $mismatch mismatch, $skipped skipped"
echo "(field logs: $FIELD_RESULTS, struct logs: $STRUCT_RESULTS)"

[[ $mismatch -eq 0 ]]
