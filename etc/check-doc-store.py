#!/usr/bin/env python3
"""Check that the Orthant store of the documentation (.orthant) has kept up with the repository.

The store's `release` axis has a value for each release, and `main` for what is not yet
released. A release is a git tag, and `orthant sync` makes a value of it: everything stored at
`main` becomes that release's, and `main` starts again beneath it. If the sync is not run, the
release has no snapshot, and whatever is stored next is mixed into the text which was released.
So this script, which `make docs` runs before anything reads the documentation, fails if

  - a release tag which HEAD contains is not a value of the `release` axis.

With `--translations` it instead reads `orthant grid`, and fails if any language, at any
release, has a line which is not translated, a passage whose English has changed beneath it, or
a conflict: run it (`make translations`) before a change to the documentation is proposed.

Usage:
    python3 etc/check-doc-store.py                  # the releases; exit 1 if one has no snapshot
    python3 etc/check-doc-store.py --translations   # the translations; exit 1 if any is behind
"""
import fnmatch, os, re, subprocess, sys

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..')
CONFIG = os.path.join(ROOT, '.orthant', 'config.tel')
ORTHANT = os.environ.get('ORTHANT', 'orthant')

def git(*arguments):
    """The output of a git command, or None if it failed."""
    done = subprocess.run(['git', '-C', ROOT, *arguments], capture_output=True, text=True)
    return done.stdout.strip() if done.returncode == 0 else None

def release_axis():
    """The `release` axis of the store: its tag pattern, and the commits of its values."""
    pattern, commits, root, inside, value = '*', [], None, False, None
    for line in open(CONFIG, encoding='utf-8'):
        if not line.startswith(' '):
            inside = False
        words = line.split()
        if line.startswith('  name ') and words[1] == 'release':
            inside = True
        elif inside and line.startswith('  pattern '):
            pattern = words[1]
        elif inside and line.startswith('  value'):
            value = {}
        elif inside and line.startswith('    ') and value is not None and len(words) > 1:
            value[words[0]] = words[1]
            if words[0] == 'commit':
                commits.append(words[1])
                if 'parent' not in value:
                    root = words[1]
    return pattern, commits, root

def releases():
    pattern, commits, root = release_axis()
    tags = git('tag', '--list', pattern, '--merged', 'HEAD')
    if tags is None:
        return 0  # not a git repository, or one without its history: nothing to compare
    missing = []
    for tag in tags.split():
        commit = git('rev-parse', f'{tag}^{{commit}}')
        if commit is None or commit in commits:
            continue
        # A tag older than the first release the store holds is not followed.
        if root is not None and git('merge-base', '--is-ancestor', root, commit) is None:
            continue
        missing.append(tag)
    if not missing:
        return 0
    def order(tag): return [int(part) if part.isdigit() else part for part in re.split(r'[.-]', tag)]
    names = ', '.join(sorted(missing, key=order))
    print(f'check-doc-store: the documentation store has no snapshot of {names}.', file=sys.stderr)
    print('  Run `orthant sync` and commit .orthant/ before anything else is stored with '
          '`orthant tag`;', file=sys.stderr)
    print('  if documentation has been stored since the release, see "A release which was not '
          'synced" in .orthant/readme.md.', file=sys.stderr)
    return 1

def translations():
    done = subprocess.run([ORTHANT, 'grid', 'language', 'release'], cwd=ROOT, capture_output=True,
                          text=True)
    if done.returncode != 0:
        print(f'check-doc-store: `{ORTHANT} grid` failed: {done.stderr.strip()}', file=sys.stderr)
        return 1
    lines = [re.sub(r'\x1b\[[0-9;]*m', '', line) for line in done.stdout.splitlines()]
    header = next((index for index, line in enumerate(lines) if line.split()[:1] == ['en']), None)
    if header is None or header == 0:
        print('check-doc-store: could not read the grid', file=sys.stderr)
        return 1
    columns = lines[header - 1].split()
    kinds = {'?': 'to translate', '!': 'stale', 'x': 'in conflict'}
    behind = []
    for line in lines[header:]:
        if not line.strip():
            break
        # Each cell is a run of words; cells are parted by two spaces or more.
        cells = re.split(r'\s{2,}', line.strip())
        language, cells = cells[0], cells[1:]
        for release, cell in zip(columns, cells):
            work = re.findall(r'(\d+)([?!x])', cell)
            if work:
                behind.append((language, release, ', '.join(f'{n} {kinds[k]}' for n, k in work)))
    for language, release, work in behind:
        print(f'check-doc-store: {language} at {release}: {work}', file=sys.stderr)
    if behind:
        print('  `orthant todo <language>` lists the work where it is best done; AGENTS.md says '
              'how it is stored.', file=sys.stderr)
        return 1
    print(f'check-doc-store: every translation is complete at {len(columns)} releases')
    return 0

if __name__ == '__main__':
    sys.exit(translations() if '--translations' in sys.argv[1:] else releases())
