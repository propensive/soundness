#!/usr/bin/env bash
# Probe one test suite under CC with a single-shot compile of its test sources against a
# freshly-captured CC classpath (the only trustworthy compile signal — see rep/DECISIONS.md).
#
#   rep/probe-suite.sh <module> [scalac]
#
# Prints "<module> GREEN" or "<module> RED" (first errors to rep/_probe/<module>.log).
set -uo pipefail
cd "$(dirname "$0")/.."
mod="$1"
scalac="${2:-/Users/propensive/work/worktrees/scala/soundness-384/release/bin/scalac}"
dir="rep/_probe/$mod"
mkdir -p "$dir"

if [[ ! -f "$dir/cp.txt" ]]; then
  rep/capture-classpath.sh "_probe/$mod" "$mod" >/dev/null 2>&1 || { echo "$mod CAPTURE-FAILURE"; exit 2; }
fi

out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
python3 - "$mod" "$dir" "$out" <<'PY'
import sys, glob
mod, dir, out = sys.argv[1:4]
opts = open(f'{dir}/opts.txt').read().splitlines()
cp = open(f'{dir}/cp.txt').read().strip()
srcs = sorted(glob.glob(f'lib/{mod}/src/test/**/*.scala', recursive=True))
args = opts + ['-classpath', cp, '-d', out] + srcs
open(f'{dir}/args.txt','w').write('\n'.join('"%s"'%a.replace('"','\\"') for a in args)+'\n')
PY

if "$scalac" "@$dir/args.txt" > "$dir/compile.log" 2>&1; then
  echo "$mod GREEN"
else
  echo "$mod RED"
  grep -aE "^-- |Error" "$dir/compile.log" | head -6 | sed 's/^/    /'
fi
