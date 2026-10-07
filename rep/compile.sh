#!/usr/bin/env bash
# Compile a reproduction with the pinned toolchain and show its capture-checking error.
#
#   rep/compile.sh <case>            # the build's compiler and -Z repairs (rep/toolchain.sh)
#   rep/compile.sh --stock <case>    # a self-contained case under the stock compiler its
#                                    # `//> using scala` header names, via scala-cli — what an
#                                    # upstream report shows
#
# Two kinds of reproduction:
#  • SELF-CONTAINED (source starts with `//> using`): no Soundness dependency at all. The header's
#    `//> using options` line supplies the flags. These are the gold standard (the case-2 classes,
#    proxy-tagged) and the ones to hand upstream.
#  • SOUNDNESS-BACKED (has cp.txt/opts.txt from `rep/capture-classpath.sh`): the error only arises
#    from the real Soundness type-graph, so the minimal source is compiled with `dotc` DIRECTLY
#    against the module's captured classpath and options. (Direct dotc is essential — Mill's
#    incremental compilation manufactures false-greens for these.)
# A case directory with its own `check.sh` (a calibrated multi-pass repro) is run through that.
set -euo pipefail
cd "$(dirname "$0")/.."

stock=0
if [[ "${1:-}" == --stock ]]; then stock=1; shift; fi
cls="$1"; dir="rep/$cls"

if [[ -x "$dir/check.sh" ]]; then
  echo "▶ $cls — calibrated (rep/$cls/check.sh)"
  exec "$dir/check.sh"
fi

src=$(ls "$dir"/*.scala | head -1)

if head -1 "$src" | grep -q '//> using'; then
  if [[ $stock == 1 ]]; then
    echo "▶ $cls — self-contained, stock compiler (scala-cli)"
    scala-cli compile "$src" 2>&1 | grep -viE 'SN-[0-9]|Compiling|Compiled' || true
  else
    echo "▶ $cls — self-contained ($(rep/toolchain.sh -version 2>&1 | sed 's/ --.*//'))"
    opts=$(sed -n 's_^//> using options __p' "$src")
    out=$(mktemp -d); trap 'rm -rf "$out"' EXIT
    rep/toolchain.sh $opts -d "$out" "$src" 2>&1 | grep -viE 'SN-[0-9]|warning' || true
  fi
else
  echo "▶ $cls — Soundness-backed (dotc + captured classpath)"
  [[ -f "$dir/cp.txt" ]] || { echo "run: rep/capture-classpath.sh $cls <module>  first"; exit 2; }
  out=$(mktemp -d); trap 'rm -rf "$out"' EXIT
  python3 - "$src" "$dir" "$out" <<'PY'
import sys
src,dir,out=sys.argv[1:4]
opts=open(f'{dir}/opts.txt').read().splitlines()
cp=open(f'{dir}/cp.txt').read().strip()
args=opts+['-classpath',cp,'-d',out,src]
open('/tmp/_repargs','w').write('\n'.join('"%s"'%a.replace('"','\\"') for a in args)+'\n')
PY
  rep/toolchain.sh @/tmp/_repargs 2>&1 | grep -viE 'SN-[0-9]|warning' || true
fi
