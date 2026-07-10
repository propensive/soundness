#!/usr/bin/env bash
# Compile a reproduction and show its capture-checking error.
#
#   rep/compile.sh <class>
#
# Two kinds of reproduction:
#  • SELF-CONTAINED (source starts with `//> using`): no Soundness dependency at all — compiled with
#    `scala-cli`. These are the gold standard (e.g. the two case-2 classes).
#  • SOUNDNESS-BACKED (has cp.txt/opts.txt from `rep/capture-classpath.sh`): the box only arises from
#    the real Soundness type-graph, so we compile the minimal source with `dotc` DIRECTLY against the
#    module's captured classpath. (Direct dotc is essential — Mill's incremental compilation
#    manufactures false-greens for these boxes.)
set -euo pipefail
cd "$(dirname "$0")/.."
cls="$1"; dir="rep/$cls"
src=$(ls "$dir"/*.scala | head -1)

if head -1 "$src" | grep -q '//> using'; then
  echo "▶ $cls — self-contained (scala-cli, no Soundness)"
  scala-cli compile "$src" 2>&1 | grep -viE 'SN-[0-9]|Compiling|Compiled' || true
else
  echo "▶ $cls — Soundness-backed (dotc + captured classpath)"
  [[ -f "$dir/cp.txt" ]] || { echo "run: rep/capture-classpath.sh $cls <module>  first"; exit 2; }
  compiler_cp=$(cs fetch --classpath org.scala-lang:scala3-compiler_3:3.8.4 2>/dev/null)
  python3 - "$src" "$dir" <<'PY'
import sys
src,dir=sys.argv[1:3]
opts=open(f'{dir}/opts.txt').read().splitlines()
cp=open(f'{dir}/cp.txt').read().strip()
args=opts+['-classpath',cp,src]
open('/tmp/_repargs','w').write('\n'.join('"%s"'%a.replace('"','\\"') for a in args)+'\n')
PY
  java -cp "$compiler_cp" dotty.tools.dotc.Main @/tmp/_repargs 2>&1 | grep -viE 'SN-[0-9]|warning' || true
fi
