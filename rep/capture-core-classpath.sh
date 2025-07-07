#!/usr/bin/env bash
# Like capture-classpath.sh, but for a non-test target (e.g. anthology.bundle): captures the
# CC-enabled compile classpath + scalac options into rep/<cls>/{cp,opts}.txt, plus the target's
# source directory into src.txt (from mill's allSourceFiles).
set -euo pipefail
cd "$(dirname "$0")/.."
cls="$1"; target="$2"
python3 rep/enable_cc_target.py "$target" >/dev/null
trap 'git checkout build.mill >/dev/null 2>&1 || true' EXIT INT TERM
./mill "$target.compileClasspath" >/dev/null 2>&1 || true
./mill show "$target.compileClasspath" > "rep/$cls/_cp.json" 2>/dev/null
./mill show "$target.scalacOptions"    > "rep/$cls/_opts.json" 2>/dev/null
./mill show "$target.allSourceFiles"   > "rep/$cls/_src.json" 2>/dev/null
python3 - "$cls" <<'PY'
import json,sys,re
cls=sys.argv[1]
cp=json.load(open(f'rep/{cls}/_cp.json'))
opts=json.load(open(f'rep/{cls}/_opts.json'))
src=json.load(open(f'rep/{cls}/_src.json'))
clean=lambda x: (lambda s: s[s.index('/'):] if '/' in s else s)(x if isinstance(x,str) else x.get('path',''))
open(f'rep/{cls}/cp.txt','w').write(':'.join(clean(x) for x in cp)+'\n')
open(f'rep/{cls}/opts.txt','w').write('\n'.join(opts)+'\n')
open(f'rep/{cls}/src.txt','w').write('\n'.join(clean(x) for x in src)+'\n')
PY
rm -f "rep/$cls/_cp.json" "rep/$cls/_opts.json" "rep/$cls/_src.json"
echo "captured rep/$cls/{cp,opts,src}.txt"
