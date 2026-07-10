#!/usr/bin/env bash
# Calibrated repro for the handler-provided-tactic class (see rep/DECISIONS.md):
#
#   Found:    (contextual$N : Tactic[E])
#   Required: Tactic[E]^{any}
#   ... capability `contextual$N` ... in an enclosing function is not visible from any in type raises
#
# at calls to `T raises E` methods inside `capture[E]{...}`'s block.
#
#   rep/handler-raises/check.sh <scalac-command...> [cczip]
#
# e.g.  rep/handler-raises/check.sh /Users/propensive/work/worktrees/scala/soundness-384/release/bin/scalac
#       rep/handler-raises/check.sh stock:3.8.4
#
# Three passes mirroring the real module mix: Contingency.scala (CC on), Zip.scala (CC OFF, as
# zeppelin.core is not CC-enabled; pass `cczip` as the last arg to compile it with CC too),
# Use.scala (CC on). Prints GREEN or RED.
set -uo pipefail
cd "$(dirname "$0")"

cczip=0
if [[ "${!#}" == cczip ]]; then cczip=1; set -- "${@:1:$(($#-1))}"; fi

out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

if [[ "$1" == stock:* ]]; then
  v="${1#stock:}"
  cp=$(cs fetch --classpath "org.scala-lang:scala3-compiler_3:$v")
  scalac() { java -cp "$cp" dotty.tools.dotc.Main -classpath "$cp:$out" "$@"; }
else
  cmd=("$@")
  scalac() { "${cmd[@]}" -classpath "$out" "$@"; }
fi

CC="-experimental -Ycc-new -language:experimental.captureChecking"

if ! scalac $CC -d "$out" Contingency.scala > "$out/contingency.log" 2>&1; then
  echo "SETUP FAILURE (contingency pass):"; cat "$out/contingency.log"; exit 2
fi

zipflags="-experimental"
[[ $cczip == 1 ]] && zipflags="$CC"
if ! scalac $zipflags -d "$out" Zip.scala > "$out/zip.log" 2>&1; then
  echo "SETUP FAILURE (zip pass):"; cat "$out/zip.log"; exit 2
fi

if scalac $CC -d "$out" Use.scala > "$out/use.log" 2>&1
then echo "GREEN"
else echo "RED"; grep -E 'Found|Required|Error|error|visible' "$out/use.log" | head -12
fi
