#!/usr/bin/env bash
# Probe one non-test target under CC with a single-shot compile of its sources against a
# freshly-captured CC classpath. Usage: rep/probe-core.sh <module.target> [scalac]
set -uo pipefail
cd "$(dirname "$0")/.."
target="$1"
scalac="${2:-/Users/propensive/work/worktrees/scala/soundness-384/release/bin/scalac}"
cls="_probe/${target/./_}"
dir="rep/$cls"
mkdir -p "$dir"

if [[ ! -f "$dir/cp.txt" ]]; then
  rep/capture-core-classpath.sh "$cls" "$target" >/dev/null 2>&1 || { echo "$target CAPTURE-FAILURE"; exit 2; }
fi

out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
python3 - "$dir" "$out" <<'PY'
import sys
dir, out = sys.argv[1:3]
opts = open(f'{dir}/opts.txt').read().splitlines()
cp = open(f'{dir}/cp.txt').read().strip()
srcs = [s for s in open(f'{dir}/src.txt').read().splitlines() if s]
args = opts + ['-classpath', cp, '-d', out] + srcs
open(f'{dir}/args.txt','w').write('\n'.join('"%s"'%a.replace('"','\\"') for a in args)+'\n')
PY

if "$scalac" "@$dir/args.txt" > "$dir/compile.log" 2>&1; then
  echo "$target GREEN"
else
  echo "$target RED"
  grep -aE "^-- |Error" "$dir/compile.log" | head -6 | sed 's/^/    /'
fi
