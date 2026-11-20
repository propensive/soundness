#!/bin/bash
# usage: run.sh probe.scala...   prints PASS/FAIL per probe and the first diagnostics
root=$(cd "$(dirname "$0")/../.." && pwd)
for src in "$@"; do
  out=$(mktemp -d); log="$out/log"
  flags=$(sed -n 's_^//FLAGS: __p' "$src")
  "$root/rep/toolchain.sh" -color:never -experimental -Ycc-new $flags -d "$out" "$src" > "$log" 2>&1
  if grep -qE '^-- .*Error|^[0-9]+ errors? found|Exception' "$log"; then got=red; else got=green; fi
  want=pos; [[ "$src" == *.neg.scala ]] && want=neg
  verdict=FAIL
  [[ $want == pos && $got == green ]] && verdict=PASS
  if [[ $want == neg && $got == red ]]; then
    verdict=PASS
    while IFS= read -r rx; do
      [[ -z "$rx" ]] && continue
      grep -qE "$rx" "$log" || verdict="FAIL(missing: $rx)"
    done < <(sed -n 's_^//EXPECT: __p' "$src")
  fi
  printf '%-52s %s (%s)\n' "$src" "$verdict" "$got"
  if [[ "$verdict" != PASS || -n "$SHOW" ]]; then grep -v '^$' "$log" | head -${LINES_SHOWN:-40}; fi
  rm -rf "$out"
done
