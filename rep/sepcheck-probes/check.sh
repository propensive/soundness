#!/usr/bin/env bash
# Separation-checking probe suite (Phase 0 of the streaming sepcheck plan — see
# rep/DECISIONS.md). Self-contained: no Soundness on the classpath; compiled directly with
# the fork scalac (never Mill — incremental compilation manufactures false greens under CC).
#
# Each probe is a single file:
#   pN-<name>.pos.scala   must compile
#   pN-<name>.neg.scala   must fail, and every `//EXPECT: <regex>` line must match the output
# A probe may name a prerequisite compiled first into its classpath with `//LIB: <file>`
# (how the two-unit cascade probes model separate-module compilation), and extra flags
# with `//FLAGS: ...`.
#
#   rep/sepcheck-probes/check.sh                 # both fork rows (3.9 + 3.10)
#   rep/sepcheck-probes/check.sh <scalac...>     # a specific toolchain
set -uo pipefail
cd "$(dirname "$0")"

if [[ $# -gt 0 ]]; then rows=("$@"); else rows=(
  /Users/propensive/work/worktrees/scala/soundness-390/release/bin/scalac
  /Users/propensive/work/worktrees/scala/all-main/release/bin/scalac
); fi

strip() { sed -e $'s/\x1b\\[[0-9;]*m//g'; }

fail=0
for scalac in "${rows[@]}"; do
  echo "══ $("$scalac" -version 2>&1 | sed 's/ --.*//') ══"
  for src in p*.pos.scala p*.neg.scala; do
    [[ -e "$src" ]] || continue
    out=$(mktemp -d); log="$out/log"
    flags=$(sed -n 's_^//FLAGS: __p' "$src")
    lib=$(sed -n 's_^//LIB: __p' "$src")
    cp_arg=()
    if [[ -n "$lib" ]]; then
      libflags=$(sed -n 's_^//FLAGS: __p' "$lib")
      if ! "$scalac" -experimental -Ycc-new $libflags -d "$out" "$lib" 2>&1 | strip > "$log"
      then :; fi
      if [[ ! -e "$out" ]] || grep -qE '^-- .*Error' "$log"; then
        printf '%-48s %s\n' "$src" "SETUP-FAIL (lib $lib)"; head -8 "$log"; fail=1
        rm -rf "$out"; continue
      fi
      cp_arg=(-classpath "$out")
    fi
    if "$scalac" -experimental -Ycc-new $flags "${cp_arg[@]}" -d "$out" "$src" 2>&1 | strip > "$log" \
       && ! grep -qE '^-- .*Error|^[0-9]+ errors? found' "$log"
    then got=green; else got=red; fi
    want=pos; [[ "$src" == *.neg.scala ]] && want=neg
    verdict=FAIL
    if [[ "$want" == pos && "$got" == green ]]; then verdict=PASS; fi
    if [[ "$want" == neg && "$got" == red ]]; then
      verdict=PASS
      while IFS= read -r rx; do
        [[ -z "$rx" ]] && continue
        grep -qE "$rx" "$log" || verdict="FAIL(missing: $rx)"
      done < <(sed -n 's_^//EXPECT: __p' "$src")
    fi
    printf '%-48s %s\n' "$src" "$verdict"
    if [[ "$verdict" != PASS ]]; then
      fail=1
      grep -viE 'warning' "$log" | head -12
    fi
    rm -rf "$out"
  done
done
exit $fail
