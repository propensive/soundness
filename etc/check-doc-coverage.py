#!/usr/bin/env python3
"""Map every library under lib/ to the tutorials in doc/modules/ that cover it, and fail if
any library is covered by none (roadmap item doc-4).

A tutorial never names a library — it says "Soundness" — so coverage is judged by the names
a library exports into the umbrella (its `soundness_<component>.scala` files): a tutorial
mentioning a type that only that library exports covers it. Names exported by more than one
library (`Error`, `Text`, …) are no evidence and are ignored. Libraries whose exported names
are too few or too short to be found that way are mapped by hand in COVERED_BY, and libraries
that are plumbing rather than user-facing are listed in INTERNAL and need no tutorial.

Usage:
    python3 etc/check-doc-coverage.py            # report uncovered libraries; exit 1 if any
    python3 etc/check-doc-coverage.py --table    # print the full library → tutorials table
Run by `make build`.
"""
import collections, glob, os, re, sys

EXPORT = re.compile(r'\bexport\s+([\w.]+)\s*\.\s*(?:\{([^}]*)\}|(\w+))', re.S)
STOP = set('''Error Errors Text Data List Set Map Optional Unset Nil Type Value Name Key Result Reason
Event Config Info Id Path Kind Mode Style Format Status Flag Option Options Entry Field Node Element
Focus Client Server Session Request Response Message Header Body Content Source Target Target
'''.split())

# Libraries that are infrastructure for other libraries, not something a reader uses directly.
INTERNAL = {
    'rudiments',      # foundation utilities used by every tutorial; nothing to teach on their own
    'anticipation', 'prepositional', 'proscenium', 'murmuration', 'corpuscular', 'tessellate',
    'beneficence', 'umbrageous', 'prescience', 'frontier', 'symbolism', 'denominative',
    'concordance', 'delicious', 'prophesy', 'stenography', 'wisteria', 'polaris',
}

# Libraries whose coverage the export-name heuristic cannot see, mapped by hand.
COVERED_BY = {
    'exegesis': ['lsp.md'],
    'espionage': ['acp.md'],
    'degustation': ['library-archives.md'],
    'reliquary': ['library-archives.md'],
    'virility': ['cli.md'],
    'ziggurat': ['packaging.md'],
    'digression': ['stack-traces.md'],
    'sibylline': ['llm.md'],
    'murmuration': ['collections.md'],
    'praxinoscope': ['patterns.md'],     # selected as `regexBackends.re2`, a kaleidoscope name
    'cardinality': ['numbers.md'],       # bounded numbers are written with the `~` type operator
    'chiaroscuro': ['testing.md'],       # the contrast rendering behind a failed assertion
    'diuretic': ['foreign-interop.md'],  # adapters for java.io.File, java.net.URL and the time types
    'perihelion': ['http-server.md'],    # websockets, reached through the HTTP server and client
}

def exported_names():
    """library → the capitalised names it exports into the umbrella."""
    names = collections.defaultdict(set)
    for path in glob.glob('lib/*/src/*/soundness_*.scala'):
        library = path.split(os.sep)[1]
        text = open(path, encoding='utf-8', errors='replace').read()
        for _, group, single in EXPORT.findall(text):
            for item in (group.split(',') if group else [single]):
                item = item.strip()
                if ' as ' in item:
                    item = item.split(' as ')[-1].strip()
                if re.fullmatch(r'[A-Z][A-Za-z0-9]{2,}', item) and item not in STOP:
                    names[library].add(item)
    return names

def main():
    table = '--table' in sys.argv
    libraries = sorted(os.path.basename(p) for p in glob.glob('lib/*'))
    names = exported_names()
    owners = collections.defaultdict(set)
    for library, exported in names.items():
        for name in exported:
            owners[name].add(library)
    unique = {library: {n for n in exported if len(owners[n]) == 1} for library, exported in names.items()}

    docs = {}
    for path in sorted(glob.glob('doc/modules/*.md')):
        docs[os.path.basename(path)] = set(re.findall(r'\b[A-Z][A-Za-z0-9]{2,}\b', open(path, encoding='utf-8').read()))

    coverage = {}
    for library in libraries:
        covering = {doc for doc, words in docs.items() if words & unique.get(library, set())}
        covering |= {doc for doc in COVERED_BY.get(library, []) if doc in docs}
        coverage[library] = sorted(covering)

    if table:
        for library in libraries:
            tag = ' (internal)' if library in INTERNAL else ''
            print(f'{library}{tag}: {" ".join(coverage[library]) or "-"}')
        return

    uncovered = [l for l in libraries if not coverage[l] and l not in INTERNAL]
    for library in uncovered:
        print(f'uncovered: lib/{library} — no tutorial in doc/modules mentions a name only it exports')
    missing = {doc for docs_ in COVERED_BY.values() for doc in docs_ if doc not in docs}
    for doc in sorted(missing):
        print(f'COVERED_BY names a tutorial that does not exist yet: {doc}')
    print(f'{len(libraries) - len(uncovered)}/{len(libraries)} libraries covered', file=sys.stderr)
    sys.exit(1 if uncovered or missing else 0)

if __name__ == '__main__':
    main()
