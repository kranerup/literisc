#!/usr/bin/env bash
#
# run-bench.sh — track compiler output size over time.
#
# Compiles a benchmark C file in each optimization mode, records the resulting
# binary size (in bytes) and assembly line count, saves a snapshot of the
# generated assembly, and appends a row to bench/history.tsv keyed by the
# current git commit. Run it after a compiler change you think improves code
# generation, then compare against earlier commits.
#
# Usage:
#   bench/run-bench.sh                 # benchmark the default source (bench.c)
#   bench/run-bench.sh foo.c           # benchmark a different source
#   bench/run-bench.sh --log           # show the recorded history and exit
#
# Config (override via environment):
#   SRC      source file            (default: bench.c)
#   INC      include dir for lrcc   (default: api)
#   MODES    space-separated modes  (default: "none Os O")

set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

HIST="bench/history.tsv"
ASMDIR="bench/asm"
mkdir -p "$ASMDIR"

# --- --log: pretty-print history and exit -----------------------------------
if [ "${1:-}" = "--log" ]; then
    if [ ! -f "$HIST" ]; then echo "no history yet ($HIST)"; exit 0; fi
    column -t -s$'\t' "$HIST"
    exit 0
fi

SRC="${SRC:-${1:-bench.c}}"
INC="${INC:-api}"
MODES="${MODES:-none Os O}"

[ -f "$SRC" ] || { echo "source not found: $SRC" >&2; exit 1; }

# git context — the row is keyed by commit; a dirty tree is flagged with '+'.
COMMIT="$(git rev-parse --short HEAD 2>/dev/null || echo nogit)"
git diff --quiet 2>/dev/null && git diff --cached --quiet 2>/dev/null || COMMIT="${COMMIT}+"
SUBJECT="$(git log -1 --pretty=%s 2>/dev/null || echo '-')"
DATE="$(date +%Y-%m-%dT%H:%M:%S)"

# Count bytes in an lrcc hexdump (offset: BB BB .. |ascii|): tally 2-hex tokens
# that precede the ascii column.
count_bytes() {
    awk '{for(i=2;i<=NF;i++){if($i ~ /\|/)break; if($i ~ /^[0-9A-Fa-f][0-9A-Fa-f]$/)c++}} END{print c+0}' "$1"
}

mode_flag() {
    case "$1" in
        none) echo "" ;;
        Os)   echo "-Os" ;;
        O)    echo "-O" ;;
        *)    echo "-$1" ;;   # allow arbitrary flag names
    esac
}

# Initialize history header once.
if [ ! -f "$HIST" ]; then
    printf 'date\tcommit\tmode\tbytes\tasm_lines\tsource\tsubject\n' > "$HIST"
fi

printf '\nbenchmarking %s @ %s  (%s)\n' "$SRC" "$COMMIT" "$SUBJECT"
printf '%-6s %10s %10s   %s\n' "mode" "bytes" "asm_lines" "snapshot"
printf -- '------ ---------- ----------   --------\n'

base="$(basename "$SRC" .c)"
TMPHEX="$(mktemp)"
trap 'rm -f "$TMPHEX"' EXIT

for mode in $MODES; do
    flag="$(mode_flag "$mode")"
    asmfile="$ASMDIR/${base}-${COMMIT}-${mode}.s"

    # Binary: compile to a hex file, then count bytes.
    if ./lrcc.lisp -I "$INC" $flag -o "$TMPHEX" "$SRC" >/dev/null 2>&1; then
        bytes="$(count_bytes "$TMPHEX")"
    else
        bytes="FAIL"
    fi

    # Assembly snapshot.
    if ./lrcc.lisp -I "$INC" $flag -S "$SRC" > "$asmfile" 2>/dev/null && [ -s "$asmfile" ]; then
        asm_lines="$(wc -l < "$asmfile" | tr -d ' ')"
    else
        rm -f "$asmfile"
        asm_lines="FAIL"
        asmfile="-"
    fi

    printf '%-6s %10s %10s   %s\n' "$mode" "$bytes" "$asm_lines" "$asmfile"
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
        "$DATE" "$COMMIT" "$mode" "$bytes" "$asm_lines" "$SRC" "$SUBJECT" >> "$HIST"
done

# --- delta vs the most recent earlier commit for the same modes -------------
prev="$(awk -F'\t' -v c="$COMMIT" 'NR>1 && $2!=c {p=$2} END{print p}' "$HIST")"
if [ -n "$prev" ]; then
    printf '\ndelta vs %s:\n' "$prev"
    for mode in $MODES; do
        old="$(awk -F'\t' -v c="$prev" -v m="$mode" '$2==c && $3==m {v=$4} END{print v}' "$HIST")"
        new="$(awk -F'\t' -v c="$COMMIT" -v m="$mode" '$2==c && $3==m {v=$4} END{print v}' "$HIST")"
        if [ -n "$old" ] && [ -n "$new" ] && [ "$old" != "FAIL" ] && [ "$new" != "FAIL" ]; then
            d=$((new - old))
            pct="$(awk -v d="$d" -v o="$old" 'BEGIN{printf "%+.1f%%", o? d*100.0/o : 0}')"
            printf '  %-6s %6s -> %-6s  %+d bytes (%s)\n' "$mode" "$old" "$new" "$d" "$pct"
        fi
    done
fi
echo
