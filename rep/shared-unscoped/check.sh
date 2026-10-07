#!/usr/bin/env bash
# Calibrated repro for the shared-but-level-exempt tactic (see SharedTactic.scala and
# rep/DECISIONS.md). Three single-file variants of one program:
#
#   ExclusiveUnscoped.scala   today's design           GREEN  (control)
#   SharedTactic.scala        Shared, not Unscoped     GREEN on 3.9.1-dev-p17 (the design wanted;
#                                                      RED would mean the level wall is back)
#   SharedUnscoped.scala      Shared AND Unscoped      RED: "inherits two unrelated classifier
#                                                      traits" — the lattice has no such point
#
#   rep/shared-unscoped/check.sh                   # the pinned toolchain (rep/toolchain.sh)
#   rep/shared-unscoped/check.sh <scalac-command>  # another compiler
#
# Prints one verdict per file. The case is OPEN only while SharedTactic is RED on the pinned
# toolchain; a fork `caps.SharedUnscoped` classifier would additionally let a rewritten
# SharedUnscoped.scala compile.
set -uo pipefail
cd "$(dirname "$0")"

if [[ $# -gt 0 ]]; then cmd=("$@"); else cmd=(../toolchain.sh); fi

out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
flags="-experimental -Ycc-new -language:experimental.captureChecking -language:experimental.separationChecking"

for src in ExclusiveUnscoped.scala SharedTactic.scala SharedUnscoped.scala; do
  mkdir -p "$out/${src%.scala}"
  if "${cmd[@]}" $flags -d "$out/${src%.scala}" "$src" > "$out/${src%.scala}.log" 2>&1
  then printf '%-26s GREEN\n' "$src"
  else
    printf '%-26s RED\n' "$src"
    grep -aE 'cannot flow|not included|Error|error' "$out/${src%.scala}.log" | head -6 | sed 's/^/    /'
  fi
done
