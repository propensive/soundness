#!/bin/sh
# Ratchet on uses of the `.stdlib` escape hatch in core sources. Tests and
# benchmarks are excluded: benchmark rival-side code uses it intentionally, and
# test migration is deferred. The count may only go down; when a change removes
# uses, lower etc/stdlib-count-baseline to match.
set -e
cd "$(dirname "$0")/.."

baseline=$(cat etc/stdlib-count-baseline)

# Comments are stripped before counting, so only code uses count: a justification
# comment mentioning the bridge must not inflate the number it justifies. (The
# stripping is line-based; a `//` inside a string literal would truncate that
# line, which is harmless here since no counted use follows one.)
count=$(find lib/*/src -name '*.scala' \
  | grep -v '/src/test/' | grep -v '/src/bench/' \
  | xargs sed 's@//.*@@' | grep -o '\.stdlib\b' | wc -l | tr -d ' ')

if [ "$count" -gt "$baseline" ]
then
  echo "error: $count uses of .stdlib in core sources, above the baseline of $baseline" >&2
  echo "Prefer the native collections API; if a new use is unavoidable (foreign" >&2
  echo "boundary, capture-checking failure), raise etc/stdlib-count-baseline." >&2
  exit 1
elif [ "$count" -lt "$baseline" ]
then
  echo "stdlib ratchet: $count uses, below the baseline of $baseline"
  echo "Lower etc/stdlib-count-baseline to $count to lock in the improvement."
else
  echo "stdlib ratchet: $count uses (at baseline)"
fi
