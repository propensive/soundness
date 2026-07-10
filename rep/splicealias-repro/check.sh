#!/usr/bin/env bash
# Calibrated repro for the `splicealias` compiler fix (quote type-hole binder info left as
# TypeAlias(Any) when only an anon class's DECLS mention the binder — see rep/DECISIONS.md).
#
#   rep/splicealias-repro/check.sh <scalac-command...>
#
# e.g.  rep/splicealias-repro/check.sh /Users/propensive/work/worktrees/scala/soundness-384/release/bin/scalac
#       rep/splicealias-repro/check.sh stock:3.8.4     (resolve stock scalac via coursier)
#
# Compiles Macro.scala (no CC), then Use.scala under CC against it. Prints GREEN or RED.
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
  scalac() { "${cmd[@]}"; }
fi

if ! scalac -d "$out" Macro.scala > "$out/macro.log" 2>&1; then
  echo "SETUP FAILURE (macro pass):"; cat "$out/macro.log"; exit 2
fi

if scalac -experimental -Ycc-new -classpath "$out" -d "$out" Use.scala > "$out/use.log" 2>&1
then echo "GREEN"
else echo "RED"; grep -E '^-- |Error|error' "$out/use.log" | head -12
fi
