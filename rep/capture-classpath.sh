#!/usr/bin/env bash
# One-time: capture the compile classpath + scalac options for a module's test target (every
# module is capture-checked, so these are the build's own), so the reproduction can then be
# compiled with `dotc` directly (no Mill). Writes cp.txt / opts.txt into the given rep class
# directory. The paths are absolute and machine-specific (gitignored); re-run after `mill clean`.
set -euo pipefail
cd "$(dirname "$0")/.."
cls="$1"; mod="$2"
./mill "$mod.test.compileClasspath" >/dev/null 2>&1
./mill show "$mod.test.compileClasspath" > "rep/$cls/_cp.json" 2>/dev/null
./mill show "$mod.test.scalacOptions"    > "rep/$cls/_opts.json" 2>/dev/null
python3 - "$cls" <<'PY'
import json,sys,re
cls=sys.argv[1]
cp=json.load(open(f'rep/{cls}/_cp.json'))
opts=json.load(open(f'rep/{cls}/_opts.json'))
paths=[re.sub(r'^[a-z]+:[0-9a-f]*:','',x if isinstance(x,str) else x.get('path','')) for x in cp]
open(f'rep/{cls}/cp.txt','w').write(':'.join(paths)+'\n')
open(f'rep/{cls}/opts.txt','w').write('\n'.join(opts)+'\n')
PY
rm -f "rep/$cls/_cp.json" "rep/$cls/_opts.json"
echo "captured rep/$cls/{cp,opts}.txt ($(wc -l < rep/$cls/cp.txt) cp lines)"
