#!/usr/bin/env python3
"""Ratchet on `while` loops and raw indexed reads in core sources (roadmap core-4, #1666).

Counts, per file, the constructs the confined-indexing campaign is draining. A file may
never gain more of any of them than `etc/while-baseline.tsv` records; when a change removes
some, run with `--update` to lower the baseline and lock the improvement in.

Per-file rather than aggregate, so a file that regresses is caught even when another
improves by the same amount. Strings and comments are stripped before counting, so a
`t"while"` keyword table or a comment mentioning `readUnchecked` does not inflate the number
it is meant to describe.
"""

import re, glob, sys, os

SETS = ['lib/*/src/core']

COLUMNS = [
  ('while',         r'\bwhile\b'),
  ('readUnchecked', r'\breadUnchecked\b'),
  ('charAt',        r'\.charAt\('),
]

BASELINE = 'etc/while-baseline.tsv'

STRIP = [
  (re.compile(r'"""(?:.|\n)*?"""'), '""'),
  (re.compile(r'"(?:[^"\\\n]|\\.)*"'), '""'),
  (re.compile(r'/\*(?:.|\n)*?\*/'), ''),
  (re.compile(r'//[^\n]*'), ''),
]


def code(source):
  for pattern, replacement in STRIP: source = pattern.sub(replacement, source)
  return source


def sources():
  for pattern in SETS:
    for path in sorted(glob.glob(pattern+'/*.scala')): yield path


def counts(path):
  text = code(open(path).read())
  return {name: len(re.findall(pattern, text)) for name, pattern in COLUMNS}


def attestations(path):
  """Every `unsafeAttested` call site must name the construction that proves the bound."""
  lines = open(path).read().split('\n')
  for index, line in enumerate(lines):
    if '.unsafeAttested(' not in line: continue
    if '//' in line: continue
    if index > 0 and '//' in lines[index - 1]: continue
    yield f'{path}:{index + 1}: `unsafeAttested` without a comment naming the proof'


def baseline():
  if not os.path.exists(BASELINE): return {}
  rows = {}
  for line in open(BASELINE):
    if line.startswith('#') or not line.strip(): continue
    fields = line.rstrip('\n').split('\t')
    rows[fields[0]] = dict(zip((name for name, _ in COLUMNS), map(int, fields[1:])))
  return rows


def write(current):
  with open(BASELINE, 'w') as file:
    file.write('# '+'\t'.join(['path']+[name for name, _ in COLUMNS])+'\n')
    for path in sorted(current):
      row = current[path]
      if not any(row.values()): continue
      file.write('\t'.join([path]+[str(row[name]) for name, _ in COLUMNS])+'\n')


current = {path: counts(path) for path in sources()}
totals = {name: sum(row[name] for row in current.values()) for name, _ in COLUMNS}

if '--totals' in sys.argv:
  print('  '.join(f'{name}: {totals[name]}' for name, _ in COLUMNS))
  raise SystemExit(0)

if '--update' in sys.argv:
  write(current)
  print(f'wrote {BASELINE}: '+'  '.join(f'{name} {totals[name]}' for name, _ in COLUMNS))
  raise SystemExit(0)

recorded = baseline()
risen, fallen, unlisted, unproven = [], [], [], []

for path, row in current.items():
  if path not in recorded:
    if any(row.values()): unlisted.append(path)
    continue
  for name, _ in COLUMNS:
    was, now = recorded[path][name], row[name]
    if now > was: risen.append((path, name, was, now))
    elif now < was: fallen.append((path, name, was, now))
  unproven.extend(attestations(path))

for path in current: unproven.extend(attestations(path))

for message in sorted(set(unproven)): print(f'error: {message}', file=sys.stderr)

for path, name, was, now in risen:
  print(f'error: {path}: {name} rose from {was} to {now}', file=sys.stderr)

for path in unlisted:
  print(f'error: {path} is not in {BASELINE} but has loops or raw reads', file=sys.stderr)

if risen or unlisted:
  print('', file=sys.stderr)
  print('Drain the loop, or — if it is a shape `doc/standards/loops.md` sanctions —', file=sys.stderr)
  print(f'raise its row in {BASELINE} and say which shape in the PR description.', file=sys.stderr)

if unproven:
  print('', file=sys.stderr)
  print('Name the construction that puts the index in range, on the call site or the', file=sys.stderr)
  print('line above it: the comment is the proof. See `doc/standards/loops.md`.', file=sys.stderr)

if risen or unlisted or unproven: raise SystemExit(1)

if fallen:
  for path, name, was, now in fallen: print(f'while ratchet: {path}: {name} {was} -> {now}')
  print(f'Run `python3 {sys.argv[0]} --update` to lock these in.')
else:
  print('while ratchet: '+'  '.join(f'{name} {totals[name]}' for name, _ in COLUMNS)+' (at baseline)')
