#!/usr/bin/env python3
"""Check the store of examples in doc/examples, and report how much of it is filled.

An example shows one topic (doc/examples/topics.tel: what a reader wants to do, a tutorial of
doc/soundness/modules) under one theme (doc/examples/themes.tel: a way of working which runs
through all of Soundness), and is the file doc/examples/<topic>/<theme>.md, or
doc/examples/<topic>/<theme>.<variant>.md where a pair has more than one. This script fails if

  - a topic has no tutorial of its name, or a tutorial has no topic;
  - a directory is not a topic, or a file's name does not begin with a theme;
  - an example has no title on its first line, has no Scala fence, or has a comment in its code
    (doc/examples/readme.md says what a mark or a sentence does instead).

Whether the code compiles is fuel's to say (`make fuelcheck`), not this script's.

Usage:
    python3 etc/check-examples.py             # check; exit 1 on any fault
    python3 etc/check-examples.py --matrix    # the topics against the themes, one mark each
    python3 etc/check-examples.py --totals    # the number of examples of each topic and theme
"""
import glob, os, re, sys

ROOT = os.path.join(os.path.dirname(__file__), '..', 'doc')
STORE = os.path.join(ROOT, 'examples')
TARGET = 1000

def entries(name, keyword):
    """The ids of a list's entries, in order, each with its fields."""
    found, current = [], None
    for line in open(os.path.join(STORE, name), encoding='utf-8'):
        head = re.match(rf'{keyword} (\S+)$', line)
        field = re.match(r'  (\w+)  (.*)$', line)
        if head:
            current = {'id': head.group(1)}
            found.append(current)
        elif field and current is not None:
            current[field.group(1)] = field.group(2)
    return found

def code(text):
    """The Scala of each fence of an example: frames' and alternatives' code, and what follows `##`."""
    for fence in re.findall(r'^```scala[^\n]*\n(.*?)^```$', text, re.S | re.M):
        yield fence

def main():
    topics = [topic['id'] for topic in entries('topics.tel', 'topic')]
    themes = [theme['id'] for theme in entries('themes.tel', 'theme')]
    faults, filled = [], {}

    tutorials = {os.path.basename(path)[:-3] for path in glob.glob(f'{ROOT}/soundness/modules/*.md')}
    tutorials.discard('overview')
    for topic in sorted(set(topics) - tutorials): faults.append(f'topics.tel: no tutorial is called {topic}')
    for topic in sorted(tutorials - set(topics)): faults.append(f'topics.tel: the tutorial {topic} is not a topic')

    for path in sorted(glob.glob(f'{STORE}/*/*')):
        relative = os.path.relpath(path, STORE)
        topic, name = relative.split(os.sep)
        theme = name.split('.')[0]

        if topic not in topics: faults.append(f'{relative}: {topic} is not a topic')
        elif not name.endswith('.md') or theme not in themes:
            faults.append(f'{relative}: {theme} is not a theme')
        else:
            filled.setdefault((topic, theme), []).append(relative)
            text = open(path, encoding='utf-8').read()
            if not text.startswith('# '): faults.append(f'{relative}: the first line is not a title')
            fences = list(code(text))
            if not fences: faults.append(f'{relative}: there is no Scala fence')
            for fence in fences:
                for line in re.sub(r'""".*?"""', '""', fence, flags=re.S).split('\n'):
                    if '//' in re.sub(r'"(\\.|[^"\\])*"', '""', line):
                        faults.append(f'{relative}: a comment in the code: {line.strip()}')

    if '--matrix' in sys.argv:
        width = max(map(len, topics))
        for index, theme in enumerate(themes): print(f'{" "*width}  {"│ "*index}┌ {theme}')
        for topic in topics:
            row = ' '.join('●' if (topic, theme) in filled else '·' for theme in themes)
            print(f'{topic:>{width}}  {row}')

    if '--totals' in sys.argv:
        for label, ids, part in (('topic', topics, 0), ('theme', themes, 1)):
            for one in ids:
                count = sum(len(paths) for pair, paths in filled.items() if pair[part] == one)
                print(f'{label}  {one:<24} {count}')

    count = sum(map(len, filled.values()))
    pairs = len(topics)*len(themes)
    print(f'{count} examples of {len(filled)} of {pairs} pairs ({len(topics)} topics, {len(themes)} themes); '
          f'the aim is {TARGET}', file=sys.stderr)

    for fault in faults: print(fault, file=sys.stderr)
    sys.exit(1 if faults else 0)

main()
