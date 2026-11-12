#!/usr/bin/env python3
"""Ratchet on example data that is not drawn from Shakespeare (doc/standards/examples.md).

Counts, per file, three kinds of example vocabulary the standard retires: arbitrary or real
people (and other fiction and brands), company structure, and metasyntactic placeholders. A
file may never gain more of any of them than `etc/example-data-baseline.tsv` records; when a
change removes some, run with `--update` to lower the baseline and lock the improvement in.

Per-file rather than aggregate, so a file that regresses is caught even when another improves
by the same amount. The baseline is keyed by path, never by line. A word that is genuine
vocabulary in one place (a `Role` that is an API type, the IANA registry's "Prospero") is not
counted at all: the patterns leave out words that are mostly real, and `EXEMPT` names the
files that are generated or define the vocabulary itself, and `GENUINE` the files in which a
column's words are the real thing.

  --totals        print the gauge: the total of each column, and the number of files
  --list [glob]   print every counted line (optionally only for paths matching the glob), as
                  the queue to work through
  --update        rewrite the baseline to the current counts
"""

import re, glob, sys, os, fnmatch

SETS = [
  'lib/*/src/**/*.scala',
  'lib/*/src/**/*.java',
  'src/**/*.scala',
  'doc/modules/*.md',
  'doc/api/*.tel',
  'doc/errors/*.md',
  'doc/philosophy/*.md',
  'doc/standards/*.md',
  'doc/roadmap/*.md',
  'doc/migration/*.md',
  'doc/*.md',
  # Hand-written resources; every other `res` file is vendored or captured.
  'lib/xenophile/res/typescript/xenophile/*.ts',
  'lib/punctuation/res/bench/punctuation/*.md',
  'lib/stratiform/res/test/stratiform/corpus/**/*',
]

EXEMPT = [
  'doc/standards/examples.md',                  # names the retired vocabulary in order to retire it
  'lib/stratiform/res/test/stratiform/corpus/DIVERGENCES.md',  # records the renaming
  'lib/locomotion/src/bench/BenchProtos.java',  # protoc output
  'lib/polysyllabic/res/*',                     # War and Peace
]

# Files in which a column's words are the real thing, not an example: (path glob, column).
GENUINE = [
  ('lib/enigmatic/src/x509/enigmatic.Distinguished.scala', 'company'),   # X.509 `O=` attribute
  ('lib/scintillate/src/server/scintillate.Reactor.scala', 'company'),   # the reactor's boss thread
  ('lib/scintillate/src/bench/scintillate.RivalNetty.scala', 'company'), # Netty's boss group
  ('doc/api/scintillate.tel', 'company'),
  ('lib/degustation/src/tasty/degustation.Universe.scala', 'placeholder'), # `$bar` is Scala's mangling of `|`
  ('lib/digression/src/core/digression.StackTrace.scala', 'placeholder'),
  ('lib/facsimile/src/core/facsimile.PdfEncoding.scala', 'placeholder'),   # Adobe glyph name `bar`
  ('lib/archimedes/src/core/archimedes.Ergo.scala', 'placeholder'),       # the overbar accent
  ('lib/tasseomancy/src/core/tasseomancy.*.scala', 'placeholder'),        # a chart's bars
  ('doc/api/tasseomancy.tel', 'placeholder'),
  ('doc/modules/charts.md', 'placeholder'),
  ('lib/xylophone/src/test/xylophone.ProviderTests.scala', 'people'),   # the W3C `po.xml` sample
  ('lib/aviation/src/test/aviation_test.scala', 'people'),               # Christmas Eve, New Year's Eve
  ('lib/gossamer/src/test/gossamer_test.scala', 'people'),               # a list of dictionary words
  ('lib/gossamer/src/test/gossamer_test.scala', 'company'),
  ('lib/gossamer/src/test/gossamer_test.scala', 'placeholder'),
  ('lib/ultimatum/src/test/*', 'placeholder'),                           # progress bars
  ('lib/burdock/src/test/*', 'placeholder'),
  ('doc/api/tasseomancy.tel', 'people'),                                  # a box plot's whiskers
]

# Released migration notes are frozen (AGENTS.md); only the unreleased one is checked.
def released(path):
  if not path.startswith('doc/migration/'): return False
  notes = sorted(glob.glob('doc/migration/*.md'), key=lambda p: [int(n) for n in re.findall(r'\d+', p)])
  return path != notes[-1]

PEOPLE = (r'(?:\b|(?<=\\[nt]))(?:Alice|Bob|Carol|Dave|Mallory|Ada|Lovelace|Turing|Hopper|Curie|John|Jane|Doe|Smith|Jack|Jill'
          r'|Simon|Peter|Paul|George|Ringo|Mary|Amy|Bea|Zoe|Zoë|Janet|Paweł|Ann|Bill|Ken|Eve'
          r'|Gondor|Acme|Dune|Beatles|Rex|Whiskers|Milo|Townville|Cityburg'
          r'|alice|bob|carol|jack|jill|gondor|acme)\b'
          r'|\bJon\b(?! Pretty)(?!,)(?!\'s \d{4})|\bjon\b')

COMPANY = (r'\b(?:ceo|cto|cfo|cio|CEO|CTO|CFO|CIO|Company|Employee|Org|Organization|Organisation|Firm|Staffer'
           r'|Department|Boss|boss|salary)\b')

# `bar` is a real word and `Bar` a real API (gauges, charts), so `bar` counts only where it is
# plainly a token: quoted, backticked, or a path, selection or interpolation segment.
PLACEHOLDER = (r'(?<![A-Za-z])[Ff]oo(?![a-z])|[a-z]Foo(?![a-z])|\bFOO|foobar|FOOBAR'
               r'|(?<![A-Za-z])[Bb]az(?![a-z])|[a-z]Baz(?![a-z])|\bBAZ\b'
               r'|(?i:\b(?:qux|quux|quuz|bippy|lorem|ipsum)\b)'
               r'''|(?<=["'`/.$])bar(?![A-Za-z])|(?<![A-Za-z])bar(?=["`/]|'(?!s\b))''')

COLUMNS = [('people', re.compile(PEOPLE)), ('company', re.compile(COMPANY)), ('placeholder', re.compile(PLACEHOLDER))]

BASELINE = 'etc/example-data-baseline.tsv'


def sources():
  seen = set()
  for pattern in SETS:
    for path in sorted(glob.glob(pattern, recursive=True)):
      if path in seen or not os.path.isfile(path): continue
      seen.add(path)
      if any(fnmatch.fnmatch(path, exempt) for exempt in EXEMPT) or released(path): continue
      yield path


def lines(path):
  try: text = open(path, encoding='utf-8').read()
  except UnicodeDecodeError: return
  for number, line in enumerate(text.split('\n'), 1):
    if 'Jon Pretty' in line and 'Copyright' in line: continue
    yield number, line


def genuine(path, column):
  return any(fnmatch.fnmatch(path, glob) and column == name for glob, name in GENUINE)


def counts(path):
  row = {name: 0 for name, _ in COLUMNS}
  for _, line in lines(path):
    for name, pattern in COLUMNS:
      if not genuine(path, name): row[name] += len(pattern.findall(line))
  return row


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


if '--list' in sys.argv:
  index = sys.argv.index('--list')
  only = sys.argv[index + 1] if len(sys.argv) > index + 1 else '*'
  for path in sources():
    if not fnmatch.fnmatch(path, only): continue
    for number, line in lines(path):
      kinds = [name for name, pattern in COLUMNS if pattern.search(line) and not genuine(path, name)]
      if kinds: print(f'{path}:{number}: [{",".join(kinds)}] {line.strip()[:140]}')
  sys.exit(0)

current = {path: counts(path) for path in sources()}
totals = {name: sum(row[name] for row in current.values()) for name, _ in COLUMNS}
files = sum(1 for row in current.values() if any(row.values()))

if '--totals' in sys.argv:
  print('\t'.join(f'{name} {totals[name]}' for name, _ in COLUMNS)+f'\tfiles {files}')
  sys.exit(0)

if '--update' in sys.argv:
  write(current)
  print(f'{BASELINE}: '+', '.join(f'{name} {totals[name]}' for name, _ in COLUMNS)+f' in {files} files')
  sys.exit(0)

recorded = baseline()
failures = []
slack = 0

for path, row in sorted(current.items()):
  allowed = recorded.get(path, {})
  for name, _ in COLUMNS:
    limit = allowed.get(name, 0)
    if row[name] > limit:
      failures.append(f'{path}: {name} {row[name]} > {limit}')
    elif row[name] < limit: slack += limit - row[name]

if failures:
  print('Example data outside the Shakespearean vocabulary (doc/standards/examples.md):')
  for failure in failures: print('  '+failure)
  print('See them with `python3 etc/check-example-data.py --list <path>`.')
  sys.exit(1)

print('example data: '+', '.join(f'{name} {totals[name]}' for name, _ in COLUMNS)+f' in {files} files'
      + (f'; {slack} below the baseline, run with --update to lock in' if slack else ''))
