#!/usr/bin/env python3
"""Report the unsafety census the Consequent plugin writes during a build.

Every checked component is compiled with `-P:consequent:metrics=…`, so each module leaves a
table of `file`, `indicator`, `count` under its own task directory in `out/`. This collects
them, sums by indicator, and prints the totals with the modules that carry most of each.

The indicators are the ways code tells the compiler to trust the author rather than check it:
declared escapes (`unsafely`, the `caps.unsafe` family, anything named `unsafe…`), casts and
null assertions, partial reads, and the imperative constructs the streaming kernel is meant to
confine. None is a violation and nothing here fails: the number is the point, and the point is
whether it is falling.

This reports; it does not ratchet. The gates that do are `etc/check-while-count.py` (per-file,
for `while`/`readUnchecked`/`charAt`) and `etc/check-stdlib-count.sh`.

Usage:
    python3 etc/unsafety-report.py             # print the census
    python3 etc/unsafety-report.py --record    # append today's totals to the history
    python3 etc/unsafety-report.py --diff      # compare with the last recorded totals
Run by `make build` and `make unsafety`.
"""
import collections, datetime, glob, os, sys

HISTORY = 'etc/unsafety-history.tsv'

# The census tables mill leaves behind. `scalacOptions` is overridden in a trait, so mill nests
# the override's directory under `.super/`; both shapes are matched rather than assumed.
TABLES = ['out/**/scalacOptions.dest/unsafety.tsv', 'out/**/*.dest/unsafety.tsv']


def module(path):
  """The library a census table belongs to: the first path segment below `out/`."""
  parts = path.split(os.sep)
  return parts[1] if len(parts) > 1 else path


def census():
  """Every record from every table, as (module, source file, indicator, count)."""
  seen, records = set(), []

  for pattern in TABLES:
    for path in glob.glob(pattern, recursive=True):
      if path in seen: continue
      seen.add(path)

      for line in open(path):
        fields = line.rstrip('\n').split('\t')
        if len(fields) != 3: continue
        source, indicator, count = fields
        if not count.isdigit(): continue
        records.append((module(path), source, indicator, int(count)))

  return records


def totals(records):
  out = collections.Counter()
  for _, _, indicator, count in records: out[indicator] += count
  return out


def byModule(records, indicator):
  out = collections.Counter()
  for name, _, found, count in records:
    if found == indicator: out[name] += count
  return out


def history():
  if not os.path.exists(HISTORY): return []
  rows = []
  for line in open(HISTORY):
    if line.startswith('#') or not line.strip(): continue
    date, indicator, count = line.rstrip('\n').split('\t')
    rows.append((date, indicator, int(count)))
  return rows


def latest():
  rows = history()
  if not rows: return {}
  when = rows[-1][0]
  return {indicator: count for date, indicator, count in rows if date == when}, when


records = census()

if not records:
  print('no census tables under out/; run `./mill soundness.all.compile` first', file=sys.stderr)
  raise SystemExit(1)

counts = totals(records)
files = len(set(source for _, source, _, _ in records))

if '--record' in sys.argv:
  today = datetime.date.today().isoformat()
  fresh = not os.path.exists(HISTORY)

  with open(HISTORY, 'a') as file:
    if fresh: file.write('# date\tindicator\tcount\n')
    for indicator in sorted(counts): file.write(f'{today}\t{indicator}\t{counts[indicator]}\n')

  print(f'recorded {len(counts)} indicators for {today} in {HISTORY}')
  raise SystemExit(0)

if '--diff' in sys.argv:
  previous, when = latest() if history() else ({}, None)
  if not previous:
    print(f'no history in {HISTORY} to compare against', file=sys.stderr)
    raise SystemExit(1)

  print(f'against {when}:')
  for indicator in sorted(set(counts) | set(previous)):
    was, now = previous.get(indicator, 0), counts.get(indicator, 0)
    if was != now: print(f'  {now - was:+6d}  {indicator}  ({was} -> {now})')

  raise SystemExit(0)

print(f'unsafety census: {len(counts)} indicators over {files} files')
print()

for indicator, count in sorted(counts.items(), key=lambda row: (-row[1], row[0])):
  worst = byModule(records, indicator).most_common(3)
  where = '  '.join(f'{name} {found}' for name, found in worst)
  print(f'{count:7d}  {indicator:<24}  {where}')
