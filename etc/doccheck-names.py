#!/usr/bin/env python3
"""Check the names the tutorials' ```scala examples use against the source of `main`.

The examples under doc/modules/ are prose, so nothing compiles them against the tree they
live in. This script does the mechanical part: every `import family.member` in a fence must
name a member declared somewhere under lib/, every capitalised name must be declared in lib/
(or by the fences themselves), and a short list of retired vocabulary must not appear at all.
For a missing name it looks for the `// OldName → New.Name` comment the API-nesting drive
left beside each renamed type, and prints the replacement.

Usage:
    python3 etc/doccheck-names.py             # every tutorial
    python3 etc/doccheck-names.py json time   # named tutorials
Exit status is 1 if anything is reported.
"""
import glob, os, re, sys

FENCE_OPEN = re.compile(r'^```scala\s*$')
FENCE_CLOSE = re.compile(r'^```\s*$')
CHOICE_IMPORT = re.compile(r'^\s*import\s+([a-z]\w*)\.([a-zA-Z]\w*)\s*$')
CHOICE_IMPORTS = re.compile(r'^\s*import\s+([a-z]\w*)\.\{([^}]*)\}\s*$')
CAPITALISED = re.compile(r'(?<![\w."`$])([A-Z][A-Za-z0-9]{2,})\b')
DECLARATION = re.compile(r'\b(?:case class|class|object|enum|trait|type|case|given|val|def)\s+([A-Z][A-Za-z0-9]*)')
RENAME = re.compile(r'//\s*([A-Z]\w*)\s*→\s*([A-Za-z][\w.]*)')
STRING_OR_COMMENT = re.compile(r'"(?:[^"\\]|\\.)*"|//.*$')

# Vocabulary retired from the Soundness API; each pattern maps to what replaced it. Only names
# verified absent from lib/ belong here: `Iterator`, `Ordering`, `distinct` and `sorted` all survive.
RETIRED = [
    (re.compile(r'\bLazyList\b'), 'LazyList: use List, Chain or Progression (#1693)'),
    (re.compile(r'\.sortBy\b'), '.sortBy: use order (#1909)'),
    (re.compile(r'\bproscenium\.compat\b'), 'proscenium.compat: deleted (#1848)'),
    (re.compile(r'\bTypename\b'), 'Typename: renamed Designator (#1685)'),
]

# Names that are Scala, Java or prose, not Soundness API, and so need no declaration.
IGNORED = set('''
Int Long Short Byte Char Boolean Double Float Unit String Nothing Any AnyRef AnyVal Null Array
Seq Some None Option Either Left Right Try Success Failure Tuple Product Function Vector Range
Predef Math StringContext Class ClassTag Throwable Exception Error RuntimeException Iterable
BigInt BigDecimal Numeric Integral Fractional Serializable Comparable Runnable Thread Object
Nil List Set Map Text Data Optional Unset IArray Chain Progression Expr Type Quotes Tuple1 Tuple2
Benchmarks Suite Tests
'''.split())

def fences(path):
    lines = open(path, encoding='utf-8').read().split('\n')
    inside, start, buffer = False, 0, []
    for number, line in enumerate(lines, 1):
        if not inside:
            if FENCE_OPEN.match(line):
                inside, start, buffer = True, number + 1, []
        elif FENCE_CLOSE.match(line):
            yield start, buffer
            inside = False
        else:
            buffer.append(line)

def source_text():
    """All library source (not tests, benches or demos), concatenated, plus the rename comments."""
    chunks, renames = [], {}
    for path in glob.glob('lib/*/src/*/*.scala'):
        if any(part in path for part in ('/src/test', '/src/bench', '/src/demo', '/src/example')):
            continue
        text = open(path, encoding='utf-8', errors='replace').read()
        chunks.append(text)
        for old, new in RENAME.findall(text):
            renames.setdefault(old, new)
    return '\n'.join(chunks), renames

def main():
    docs = [d if d.endswith('.md') else os.path.join('doc', 'modules', d + '.md') for d in sys.argv[1:]] \
        or sorted(glob.glob('doc/modules/*.md'))
    source, renames = source_text()
    declared_in_source = set(re.findall(r'(?:class|object|enum|trait|type|val|def|given|case)\s+([A-Z][A-Za-z0-9]*)', source))
    exported = set(re.findall(r'\b([A-Z][A-Za-z0-9]*)\b', '\n'.join(
        open(p, encoding='utf-8', errors='replace').read() for p in glob.glob('lib/*/src/*/soundness_*.scala'))))
    members = set(re.findall(r'\b(?:given|val|def|object|lazy val|export)\s+(?:[\w.{}, ]*\bas\s+)?(\w+)', source))
    members |= set(re.findall(r'\bas\s+(\w+)', source))

    problems = 0
    for doc in docs:
        declared_in_doc = set()
        for _, lines in fences(doc):
            for line in lines:
                declared_in_doc.update(DECLARATION.findall(line))
                cases = re.match(r'\s*case\s+([A-Z]\w*(?:\s*,\s*[A-Z]\w*)*)\s*(?:$|//|\()', line)
                if cases:
                    declared_in_doc.update(n.strip() for n in cases.group(1).split(','))
        seen = set()
        prose = []
        inside = False
        for number, line in enumerate(open(doc, encoding='utf-8').read().split('\n'), 1):
            if FENCE_OPEN.match(line) or (inside and FENCE_CLOSE.match(line)):
                inside = not inside
            elif not inside and line.startswith('```'):
                inside = True
            elif not inside:
                for span in re.findall(r'`([^`]+)`', line):
                    prose.append((number, span))
        # Prose names are checked more loosely than fence names — prose legitimately mentions JDK
        # classes, PDF operators and hypothetical types — so only a compound name of the kind the
        # API-nesting drive retired (`FooError`, `FooEvent`, …) or one with a recorded rename counts.
        for number, span in prose:
            for name in CAPITALISED.findall(span):
                if name.isupper() or name in IGNORED or name in declared_in_doc or name in declared_in_source \
                        or name in exported or name in seen:
                    continue
                if name not in renames and not re.search(r'[a-z](Error|Errors|Event|Reader|Writer|Backend)$', name):
                    continue
                seen.add(name)
                hint = f' (source says: {name} → {renames[name]})' if name in renames else ''
                print(f'{doc}:{number}: unknown name in prose: {name}{hint}')
                problems += 1
        for start, lines in fences(doc):
            for offset, line in enumerate(lines):
                number = start + offset
                code = STRING_OR_COMMENT.sub('', line)
                for pattern, message in RETIRED:
                    if pattern.search(code):
                        print(f'{doc}:{number}: retired vocabulary: {message}')
                        problems += 1
                single = CHOICE_IMPORT.match(line)
                multi = CHOICE_IMPORTS.match(line)
                names = [single.group(2)] if single else \
                    [n.strip() for n in multi.group(2).split(',') if n.strip() and n.strip() != 'given'] if multi else []
                for member in names:
                    if member != '*' and member not in members:
                        print(f'{doc}:{number}: import names no known given or member: {member}')
                        problems += 1
                for name in CAPITALISED.findall(code):
                    if name.isupper() or name in IGNORED or name in declared_in_doc or name in declared_in_source or name in exported:
                        continue
                    if name in seen:
                        continue
                    seen.add(name)
                    hint = f' (source says: {name} → {renames[name]})' if name in renames \
                        else ' (declared neither in lib/ nor in this tutorial\'s examples)'
                    print(f'{doc}:{number}: unknown name: {name}{hint}')
                    problems += 1
    print(f'{problems} problem(s)', file=sys.stderr)
    sys.exit(1 if problems else 0)

if __name__ == '__main__':
    main()
