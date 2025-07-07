#!/usr/bin/env bash
# Calibrated repro for the stacked context-function-result class (see rep/DECISIONS.md):
#
#   Reference `contextual$N` is not included in the allowed capture set {any}
#   of an enclosing function literal with expected type (Tactic[E]^) ?=> T
#   ... any is a root capability in the result type of method <m>
#
# A method whose result stacks two capability context functions (`X logs E raises F`,
# or literal `A^ ?=> B^ ?=> T`) cannot reference the outer layer's parameter from the
# inner layer's body: the intermediate anonfun's params are level-blocked from the
# method-result fresh, even though erasure flattens them into the method's own
# parameter list. SingleLayerControl.scala shows the single-layer case is accepted.
#
#   rep/stacked-raises/check.sh <scalac-command...>
#   rep/stacked-raises/check.sh stock:3.8.4
#
# Prints GREEN or RED (both files compile / stacked fails).
set -uo pipefail
cd "$(dirname "$0")"

out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

if [[ "$1" == stock:* ]]; then
  v="${1#stock:}"
  cp=$(cs fetch --classpath "org.scala-lang:scala3-compiler_3:$v")
  scalac() { java -cp "$cp" dotty.tools.dotc.Main -classpath "$cp" "$@"; }
else
  cmd=("$@")
  scalac() { "${cmd[@]}" "$@"; }
fi

CC="-experimental -Ycc-new -language:experimental.captureChecking"

if ! scalac $CC -d "$out" SingleLayerControl.scala > "$out/control.log" 2>&1; then
  echo "SETUP FAILURE (single-layer control should always compile):"
  cat "$out/control.log"; exit 2
fi

if scalac $CC -d "$out" StackedRaises.scala > "$out/stacked.log" 2>&1
then echo "GREEN"
else echo "RED"; grep -aE "not included|Reference|Error" "$out/stacked.log" | head -8
fi
