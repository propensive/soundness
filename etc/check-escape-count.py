#!/usr/bin/env python3
"""Ratchet on the escape surface (roadmap safety-2, safety-7 and core-3).

Counts, per file, every place the capture checker is overruled by hand — the four
`caps.unsafe` hatches — and the array conversions that hide a capture behind a helper
(`Array.unsafeFrozen`, `Array.unsafeJvm`, and
`!!`, the erased-evidence operator over `unsafeErasedValue`). It also counts the casts that
move a capability past the checker without any hatch: a cast to `AnyRef`, the "neutral carrier"
that a capability is stored as and later recovered from, and a cast whose target type carries a
capture set (`x.asInstanceOf[Reader^]`), which reasserts one.
A file may never gain more of any of them than `etc/escape-baseline.tsv` records; when a change
removes some, run with `--update` to lower the baseline and lock the improvement in.

It also counts, per file, the `caps.unsafe` hatches that carry no reason tag. A reason tag is a
bracketed name from the closed vocabulary below, in a comment on the hatch's own line or on one
of the three lines above it: `// [pump-overlap] the pump consumes the intake …`. The tag says
which blocker the hatch is waiting on, so the residue can be read as a queue (`--tags`) rather
than a number. Untagged hatches are ratcheted like the rest: the count may only go down.

It also counts the `.stdlib` bridge — the view of a Soundness collection as the Scala
collection beneath it, which leaves every guarantee of the native API behind — in core and test
sources alike, but not in benchmarks, whose rival-side code uses it on purpose. Three shape
columns read that residue as a queue: the dysasymptotic gate bypasses (`.stdlib.head`,
`.stdlib(i)` and the size reads on a `List`, which should open the gate explicitly or use `prim`),
the round trips that convert and convert straight back (`.stdlib.toList`, `Array.from(xs.stdlib)`),
and `.stdlib.iterator`; `--stdlib` lists the residue by what follows the bridge. The direct
`import scala.collection` lines (roadmap core-3) are counted the same way.

Per-file rather than aggregate, so a file that regresses is caught even when another improves
by the same amount. Strings and comments are stripped before counting hatches, so a comment
that mentions `unsafeAssumePure` does not inflate the number it is meant to describe.

  python3 etc/check-escape-count.py            # check against the baseline
  python3 etc/check-escape-count.py --update   # lower (or seed) the baseline
  python3 etc/check-escape-count.py --totals   # the gauge, one line
  python3 etc/check-escape-count.py --tags     # the residue per reason tag
  python3 etc/check-escape-count.py --stdlib   # the `.stdlib` residue per shape
"""

import re, glob, sys, os

SOURCES = 'lib/*/src/**/*.scala'
BASELINE = 'etc/escape-baseline.tsv'

HATCHES = [
  ('assumePure',     r'\bunsafeAssumePure\b'),
  ('assumeSeparate', r'\bunsafeAssumeSeparate\b'),
  ('untracked',      r'\buntrackedCaptures\b'),
  ('erasedValue',    r'\bunsafeErasedValue\b'),
]

WRAPPERS = [
  ('unsafeFrozen', r'\bArray\.unsafeFrozen\b'),
  ('unsafeJvm',    r'\bArray\.unsafeJvm\b'),
  ('frozen',       r'\bArray\.frozen\b'),
  ('erasedEvidence', r'(?<![!\w])!!(?![!=\w])'),
]

# Casts are counted by their type argument, read with balanced brackets, since a capturing target
# type is often nested (`asInstanceOf[(Stream[Text] over Credit)^]`).
CASTS = ['anyRefCast', 'captureCast']

# The `.stdlib` bridge and its shapes. A continuation line writes the bridge as `. stdlib`, so
# the dot and the name may be separated by spaces. The shapes overlap neither each other nor
# anything above; a bridge that fits none of them counts in `stdlib` alone.
BRIDGE = re.compile(r'\.\s*stdlib\b')
GATE = re.compile(r'\.\s*stdlib\s*(?:\.\s*(?:head|last|headOption|length|size|isEmpty|nonEmpty)\b|\()')
ROUND = re.compile(r'\.\s*stdlib\s*\.\s*to(?:\(|List\b|Vector\b|Set\b|Seq\b|Map\b|Array\b)|\bfrom\([^()]*\.\s*stdlib\b')
ITERATOR = re.compile(r'\.\s*stdlib\s*\.\s*iterator\b')
SHAPE = re.compile(r'\.\s*stdlib\b\s*(\.\s*\w+|\()?')
IMPORT = re.compile(r'^\s*import\s+scala\.collection\b')

STDLIB = ['stdlib', 'stdlibGate', 'stdlibRound', 'stdlibIterator', 'collectionImport']

COLUMNS = [name for name, _ in HATCHES+WRAPPERS]+CASTS+STDLIB+['untagged']

# The closed vocabulary. Each tag names a blocker; `rep/DECISIONS.md` has the case behind it.
TAGS = {
  'abstract-storage':   'an abstract `Storage` type cannot carry `^` (probe P5)',
  'aliased-read':       'an array read while another exclusive receiver holds its owner',
  'aliased-graph':      'mutable nodes reachable from several references (a linked or shared graph)',
  'anon-fresh-field':   'a fresh-typed field in an anonymous template hides its capability',
  'borrowing-stateful': 'a stateful instance minted by `new` cannot borrow an enclosing `this`',
  'by-name-capture':    'a by-name parameter is not a nameable capture (fork leg P4)',
  'by-name-receiver':   'a by-name argument captures the receiver of the call it is passed to',
  'closure-capture':    'a closure- or local-def-captured exclusive reads back read-only',
  'construction-fresh': 'the fresh capability an instance constitutes, laundered at its factory',
  'cursor-snapshot':    'a parser\'s snapshot of its cursor\'s buffer',
  'erased-evidence':    'compile-time evidence with no runtime content, built with an erased value',
  'field-fresh-param':  'a field whose type mentions a fresh parameter capability (probe P18)',
  'field-purity':       'a capability-carrying value stored in an object-level field',
  'fresh-in-lambda':    'a fresh result minted inside a lambda or `try`',
  'java-boundary':      'an array crossing to or from a Java API that takes or returns raw arrays',
  'live-view':          'a pure-typed view (`Termcap`, …) over live, capturing state',
  'pump-overlap':       'a pump consumes its target, which is read again afterwards',
  'quote-wall':         'a capture crossing a quote/splice boundary',
  'registry-lifetime':  'a handle smuggled through an application-lifetime registry',
  'stdio-readonly':     'a writer held through a read-only standard-streams reference',
  'stdlib-iterator':    'state in a `scala.Iterator`, whose methods cannot be `update`',
  'synchronized':       'state shared across threads under a lock, atomic or volatile: the honest model',
  'test-harness':       'test-only scaffolding, not library behaviour',
  'transfer':           'a resource moved into a task, whose previous owner consumed it',
}

TAG = re.compile(r'\[([a-z][a-z-]+)\]')


def strip(line):
  line = re.sub(r'"(?:[^"\\\n]|\\.)*"', '""', line)
  return line.split('//', 1)[0]


def lines(path):
  text = open(path).read()
  text = re.sub(r'"""(?:.|\n)*?"""', lambda m: '""'+'\n'*m.group(0).count('\n'), text)
  text = re.sub(r'/\*(?:.|\n)*?\*/', lambda m: '\n'*m.group(0).count('\n'), text)
  return text.split('\n'), open(path).read().split('\n')


def tagged(raw, index):
  # Nearest first, so that a tag above one hatch is not credited to an adjacent one below it.
  for line in reversed(raw[max(0, index - 3):index + 1]):
    if '//' not in line: continue
    for tag in TAG.findall(line.split('//', 1)[1]):
      if tag in TAGS: return tag
  return None


def casts(line):
  found = []
  start = 0

  while (begin := line.find('.asInstanceOf[', start)) >= 0:
    end = begin+len('.asInstanceOf[')
    depth = 1

    while end < len(line) and depth:
      if line[end] == '[': depth += 1
      elif line[end] == ']': depth -= 1
      end += 1

    found.append(line[begin+len('.asInstanceOf['):end-1])
    start = end

  return found


def census(path):
  code, raw = lines(path)
  row = dict.fromkeys(COLUMNS, 0)
  tags = {}
  shapes = {}
  bench = '/src/bench/' in path

  for index, line in enumerate(code):
    line = strip(line)
    for name, pattern in WRAPPERS: row[name] += len(re.findall(pattern, line))

    if not bench:
      row['stdlib'] += len(BRIDGE.findall(line))
      row['stdlibGate'] += len(GATE.findall(line))
      row['stdlibRound'] += len(ROUND.findall(line))
      row['stdlibIterator'] += len(ITERATOR.findall(line))
      row['collectionImport'] += 1 if IMPORT.match(line) else 0

      for match in SHAPE.finditer(line):
        shape = '.stdlib'+re.sub(r'\s+', '', match.group(1) or '')
        shapes[shape] = shapes.get(shape, 0) + 1

    for target in casts(line):
      if target.strip() == 'AnyRef': row['anyRefCast'] += 1
      elif '^' in target: row['captureCast'] += 1
    for name, pattern in HATCHES:
      found = len(re.findall(pattern, line))
      if not found: continue
      row[name] += found
      tag = tagged(raw, index)
      if tag is None: row['untagged'] += found
      else: tags[tag] = tags.get(tag, 0) + found

  return row, tags, shapes


def baseline():
  if not os.path.exists(BASELINE): return {}
  rows = {}
  for line in open(BASELINE):
    if line.startswith('#') or not line.strip(): continue
    fields = line.rstrip('\n').split('\t')
    rows[fields[0]] = dict(zip(COLUMNS, map(int, fields[1:])))
  return rows


def write(current):
  with open(BASELINE, 'w') as file:
    file.write('# '+'\t'.join(['path']+COLUMNS)+'\n')
    for path in sorted(current):
      row = current[path]
      if not any(row.values()): continue
      file.write('\t'.join([path]+[str(row[name]) for name in COLUMNS])+'\n')


paths = sorted(glob.glob(SOURCES, recursive=True))
results = {path: census(path) for path in paths}
current = {path: row for path, (row, _, _) in results.items()}
totals = {name: sum(row[name] for row in current.values()) for name in COLUMNS}


def gauge():
  return '  '.join(f'{name} {totals[name]}' for name in COLUMNS)


if '--totals' in sys.argv:
  print(gauge())
  raise SystemExit(0)

if '--tags' in sys.argv:
  counts = {}
  for _, tags, _ in results.values():
    for tag, count in tags.items(): counts[tag] = counts.get(tag, 0) + count

  for tag, count in sorted(counts.items(), key=lambda item: -item[1]):
    print(f'{count:5}  [{tag}]  {TAGS[tag]}')

  print(f'{totals["untagged"]:5}  untagged')
  raise SystemExit(0)

if '--stdlib' in sys.argv:
  # Each shape with the share of it that is test code, since the test suites hold most of the
  # residue and the two are drained differently.
  counts, tests = {}, {}
  for path, (_, _, shapes) in results.items():
    for shape, count in shapes.items():
      counts[shape] = counts.get(shape, 0) + count
      if '/src/test/' in path: tests[shape] = tests.get(shape, 0) + count

  for shape, count in sorted(counts.items(), key=lambda item: -item[1]):
    print(f'{count:5}  {shape:28} {tests.get(shape, 0):5} in tests')

  print(f'{totals["stdlib"]:5}  {"total":28} {sum(tests.values()):5} in tests')
  raise SystemExit(0)

if '--update' in sys.argv:
  write(current)
  print(f'wrote {BASELINE}: {gauge()}')
  raise SystemExit(0)

recorded = baseline()
risen, fallen, unlisted = [], [], []

for path, row in current.items():
  if path not in recorded:
    if any(row.values()): unlisted.append(path)
    continue

  for name in COLUMNS:
    was, now = recorded[path].get(name, 0), row[name]
    if now > was: risen.append((path, name, was, now))
    elif now < was: fallen.append((path, name, was, now))

for path, name, was, now in risen:
  print(f'error: {path}: {name} rose from {was} to {now}', file=sys.stderr)

for path in unlisted:
  print(f'error: {path} is not in {BASELINE} but overrules the checker or bridges to the stdlib', file=sys.stderr)

if risen or unlisted:
  print('', file=sys.stderr)
  print('Remove the escape, or — if it is unavoidable — tag a hatch with the blocker it waits on', file=sys.stderr)
  print(f'(the vocabulary is in {sys.argv[0]}), raise its row in {BASELINE}, and say why in', file=sys.stderr)
  print('the PR description. A `.stdlib` bridge has no tag: prefer the native API, or open the', file=sys.stderr)
  print('dysasymptotic gate explicitly when the cost is the point.', file=sys.stderr)
  raise SystemExit(1)

if fallen:
  for path, name, was, now in fallen: print(f'escape ratchet: {path}: {name} {was} -> {now}')
  print(f'Run `python3 {sys.argv[0]} --update` to lock these in.')
else:
  print(f'escape ratchet: {gauge()} (at baseline)')
