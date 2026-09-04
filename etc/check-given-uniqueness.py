#!/usr/bin/env python3
"""Check the importable (orphan) contextual values against the naming standard
in doc/standards/given-naming.md.

Soundness exposes selection givens through lowercase family packages
(`package watchers:` holding `given javaBaseWatcher`) that each component's
`soundness_<component>_<module>.scala` file mirrors as a `package watchers:`
block. The standard requires every family-given name to be unique across *all*
families, judged with the package removed, so that the name alone identifies the
given and re-exports can never collide. This script enforces that, and also
reports any family declared in a library that no export file mirrors, since an
unmirrored family is invisible to `import soundness.*` users. Run it with
`make check-givens`.
"""
import re, glob, collections, sys

GIVEN = re.compile(r'^(?P<indent>[ ]*)(?:transparent\s+)?(?:inline\s+)?given\s+(?P<name>[a-z]\w*)\b', re.M)
PACKAGE = re.compile(r'^(?P<indent>[ ]*)package\s+(?P<name>[a-z][\w.]*):\s*$', re.M)
ITEM = re.compile(r'^(?P<indent>[ ]*)(?:package|object|class|trait|enum|def|val|given|export|extension|case)\b', re.M)

# Families sanctioned to have no umbrella mirror (fixture objects, or deliberately internal).
UNMIRRORED_OK = {'internal', 'stagedInternal', 'bintelInternal', 'inlinables',
                 'context'}   # frontier's `context.explainMissingContext` is an `implicit def` in the umbrella

declared = collections.defaultdict(set)       # leaf -> {path}
families = collections.defaultdict(set)       # family -> {path}
family_leaves = collections.defaultdict(set)  # family -> {leaf}

for path in glob.glob('lib/*/src/*/*.scala'):
    if '/src/test' in path or '/src/bench' in path or '/src/demo' in path or '/src/example' in path:
        continue
    if '/soundness_' in path:   # the umbrella mirrors are checked separately below
        continue
    text = open(path, encoding='utf-8', errors='replace').read()
    lines = text.split('\n')
    # Walk the file tracking the innermost enclosing family package (Scala 3 braceless blocks).
    stack = []   # (indent, family-name)
    for line in lines:
        if not line.strip() or line.lstrip().startswith('//'):
            continue
        indent = len(line) - len(line.lstrip(' '))
        while stack and indent <= stack[-1][0]:
            stack.pop()
        pkg = PACKAGE.match(line)
        if pkg:
            name = pkg.group('name')
            # A top-level `package foo:` block (indent 0, name lowercase) is a family; the
            # file's own `package lib` header has no trailing colon and is not matched.
            stack.append((indent, name))
            if not any(seg in UNMIRRORED_OK for seg in name.split('.')):
                families['.'.join(n for _, n in stack)].add(path)
            continue
        item = ITEM.match(line)
        if item and item.group(0).strip().split()[0] in ('object', 'class', 'trait', 'enum'):
            stack.append((indent, None))   # non-family scope: givens inside are companion-scoped
            continue
        g = GIVEN.match(line)
        if g and stack and all(n is not None for _, n in stack):
            declared[g.group('name')].add(path)
            family_leaves['.'.join(n for _, n in stack)].add(g.group('name'))
    del stack

exported_families = set()
export_leaves = collections.defaultdict(set)
for path in glob.glob('lib/*/src/*/soundness_*.scala'):
    text = open(path, encoding='utf-8', errors='replace').read()
    # Only exports inside a `package <family>:` block are family givens; the top-level
    # `export lib.{...}` term lists may legitimately repeat names (extension methods).
    blocks = re.split(r'^(?=package\s+[a-z][\w.]*:\s*$)', text, flags=re.M)
    for block in blocks:
        m = PACKAGE.match(block)
        if not m:
            continue
        exported_families.add(m.group('name'))
        for match in re.finditer(r'export\s+[\w.]+\s*\.\s*\{([^}]*)\}', block, re.S):
            for leaf in match.group(1).split(','):
                leaf = leaf.strip().strip('`')
                if leaf and leaf[0].islower():
                    export_leaves[leaf].add(path)
        for match in re.finditer(r'export\s+[\w.]+\.([a-z]\w*)\s*$', block, re.M):
            export_leaves[match.group(1)].add(path)
        for match in GIVEN.finditer(block):   # hand-written delegating givens
            export_leaves[match.group('name')].add(path)

failed = False
dups = {leaf: paths for leaf, paths in declared.items() if len(paths) > 1}
print(f'{len(declared)} distinct family-given names declared in {len(families)} families')
if dups:
    failed = True
    print(f'ERROR: {len(dups)} family-given name(s) declared in more than one place:')
    for leaf in sorted(dups):
        for path in sorted(dups[leaf]):
            print(f'  {leaf}  <-  {path}')

exp_dups = {leaf: paths for leaf, paths in export_leaves.items() if len(paths) > 1}
if exp_dups:
    failed = True
    print(f'ERROR: {len(exp_dups)} leaf name(s) exported from more than one place:')
    for leaf in sorted(exp_dups):
        for path in sorted(exp_dups[leaf]):
            print(f'  {leaf}  <-  {path}')

# A nested library family (`dateFormats.months`) may be mirrored under a flat umbrella name
# (`monthFormats`); accept any family whose leaf givens are all exported somewhere.
unmirrored = []
for fam, paths in sorted(families.items()):
    if fam in exported_families or fam.split('.')[-1] in exported_families:
        continue
    fam_leaves = family_leaves.get(fam, set())
    if fam_leaves and fam_leaves <= set(export_leaves):
        continue
    unmirrored.append((fam, sorted(paths)))
if unmirrored:
    failed = True
    print(f'ERROR: {len(unmirrored)} family package(s) not mirrored by any soundness_*.scala export file:')
    for fam, paths in unmirrored:
        print(f'  {fam}  <-  {", ".join(paths)}')

if failed:
    sys.exit(1)
print('All family-given names are globally unique and every family is mirrored.')
