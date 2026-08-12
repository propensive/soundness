#!/usr/bin/env python3
"""Regenerate the name tables in doc/api-reduction-candidates.md from the export files.

Reads the exported type-level names out of every `soundness_*.scala`, keeps the multi-word
ones, and rewrites two sections in place: the prefix-family table and the singleton list.

Names listed in the hand-curated "Reviewed items which should not be moved" section are
subtracted from both, and that section is left untouched — so curation survives a rerun.
"""
import re, glob, collections, textwrap, sys

DOC = 'doc/api-reduction-candidates.md'
REVIEWED = '### Reviewed items which should not be moved'
FAMILIES = '## Remaining names by prefix family'
SINGLETONS = '### Singletons'
TAIL = '## Retained from the original inventory'

def strip_package_blocks(text):
    out, skipping = [], False
    for line in text.split('\n'):
        if re.match(r'^package \w+:', line): skipping = True; continue
        if skipping:
            if not line.strip() or line.startswith(' '): continue
            skipping = False
        out.append(line)
    return '\n'.join(out)

def exported():
    names = {}
    for path in sorted(glob.glob('lib/*/src/*/soundness_*.scala')):
        lib = path.split('/')[1]
        body = strip_package_blocks(open(path, encoding='utf-8', errors='replace').read())
        body = re.sub(r'//[^\n]*', '', body)
        for m in re.finditer(r'\bexport\b([^{}\n]|\n)*?\{([^}]*)\}', body):
            for n in m.group(2).split(','):
                n = n.strip().split('=>')[0].strip()
                if re.fullmatch(r'[A-Z]\w*', n): names.setdefault(n, set()).add(lib)
        for m in re.finditer(r'\bexport\s+[\w.]*?\.([A-Z]\w*)\s*$', body, re.M):
            names.setdefault(m.group(1), set()).add(lib)
    return names

doc = open(DOC, encoding='utf-8').read()
reviewed_section = doc[doc.index(REVIEWED):doc.index(SINGLETONS)]
reviewed = set(re.findall(r'`([A-Z]\w*)`', reviewed_section))

multi = {n: l for n, l in exported().items()
         if re.search(r'[a-z0-9][A-Z]', n) and n not in reviewed}

fam = collections.defaultdict(dict)
for n, libs in multi.items():
    fam[re.match(r'[A-Z][a-z0-9]*', n).group(0)][n] = libs
families = {k: v for k, v in fam.items() if len(v) > 1}
singles = sorted((n for k, v in fam.items() if len(v) == 1 for n in v), key=str.lower)

rows = []
for prefix in sorted(families, key=str.lower):
    members = families[prefix]
    libs = sorted({l for s in members.values() for l in s})
    rows.append(f'| `{prefix}*` | {", ".join(libs)} | {len(members)} | '
                + ', '.join(f'`{n}`' for n in sorted(members, key=str.lower)) + ' |')

head = doc[:doc.index(FAMILIES)]
nfam, ninfam, nsing = len(families), sum(len(v) for v in families.values()), len(singles)

head = re.sub(r'- exported multi-word type-level names[\s\S]*?(?=\n- )',
              f'- exported multi-word type-level names still under review: **{ninfam + nsing}**;\n'
              f'  {ninfam} sit in a prefix family of two or more, across {nfam} families, and\n'
              f'  {nsing} are singletons. Names under "Reviewed items which should not be moved"\n'
              f'  are excluded from both counts', head)

body = (f'{FAMILIES}\n\n| prefix | libraries | n | names |\n|---|---|---|---|\n'
        + '\n'.join(rows) + '\n\n' + reviewed_section
        + f'{SINGLETONS} ({nsing})\n\n'
        + '\n'.join(textwrap.wrap(', '.join(f'`{n}`' for n in singles), width=98))
        + '\n\n' + doc[doc.index(TAIL):])

# The curated section must survive byte-for-byte; a silent migration of rows into it
# would quietly retire names from review.
result = head + body
check = result[result.index(REVIEWED):result.index(SINGLETONS)]
if check != reviewed_section:
    raise SystemExit('refusing to write: the reviewed section would change')

open(DOC, 'w', encoding='utf-8').write(result)
print(f'{ninfam} names in {nfam} families, {nsing} singletons, '
      f'{len(reviewed)} reviewed-and-excluded')
