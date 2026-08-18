#!/usr/bin/env python3
"""Ratchet the number of files importing `proscenium.compat`, per library.

`proscenium.compat` is a migration-shim file (see the header of
`lib/proscenium/src/core/proscenium.compat.scala` and roadmap goal core-2 in
`doc/roadmap/core.md`): it exists to be drained, and its importer count is the
interim gauge. Between 2026-08-01 and 2026-08-18 that count *rose* from 475 to
494 because nothing guarded it; this script is the guard.

`etc/compat-baseline.txt` holds one `<library> <count>` line per library. A
library exceeding its baseline fails the build: drain the new import, or (with
justification) raise the baseline in the same commit. A library below its
baseline is reported so the win can be locked in by lowering the number; run
with `--update` to rewrite the baseline to current reality (only sensible in a
commit that also contains the drain). Run it with `make check-compat`.
"""
import collections, pathlib, re, subprocess, sys

BASELINE = pathlib.Path('etc/compat-baseline.txt')

def current() -> dict[str, int]:
    out = subprocess.run(
        ['git', 'grep', '-l', 'import proscenium.compat', '--', 'lib'],
        capture_output=True, text=True)
    counts: dict[str, int] = collections.Counter()
    for path in out.stdout.split():
        counts[path.split('/')[1]] += 1
    return counts

def read_baseline() -> dict[str, int]:
    counts = {}
    for line in BASELINE.read_text().splitlines():
        line = line.strip()
        if line and not line.startswith('#'):
            library, count = line.split()
            counts[library] = int(count)
    return counts

def write_baseline(counts: dict[str, int]) -> None:
    lines = ['# Files importing proscenium.compat, per library: the drain ratchet.',
             '# May only decrease; regenerate with etc/check-compat-ratchet.py --update.']
    lines += [f'{library} {count}' for library, count in sorted(counts.items())]
    BASELINE.write_text('\n'.join(lines) + '\n')

def main() -> int:
    counts = current()

    if '--update' in sys.argv:
        write_baseline(counts)
        print(f'Baseline rewritten: {sum(counts.values())} importing files '
              f'across {len(counts)} libraries.')
        return 0

    baseline = read_baseline()
    regressions = {lib: (baseline.get(lib, 0), n) for lib, n in counts.items()
                   if n > baseline.get(lib, 0)}
    improvements = {lib: (n, counts.get(lib, 0)) for lib, n in baseline.items()
                    if counts.get(lib, 0) < n}

    print(f'{sum(counts.values())} files import proscenium.compat '
          f'(baseline {sum(baseline.values())}).')

    if improvements:
        wins = ', '.join(f'{lib} {old}→{new}'
                         for lib, (old, new) in sorted(improvements.items()))
        print(f'Below baseline (run --update in a drain commit to lock in): {wins}')

    if regressions:
        print('ERROR: new proscenium.compat import(s); drain them or justify '
              'raising the baseline in the same commit:')
        for lib, (allowed, actual) in sorted(regressions.items()):
            print(f'  lib/{lib}: {actual} importing files (baseline {allowed})')
        return 1

    return 0

if __name__ == '__main__':
    sys.exit(main())
