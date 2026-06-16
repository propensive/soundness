#!/usr/bin/env python3
"""Verify that every importable `given` re-exported into the `soundness` package
has a globally-unique leaf name.

Soundness exposes selection/default `given`s through lowercase family packages
(e.g. `soundness.watchers.nativeWatcher`) that are re-exported from each
component's `soundness_<component>_<module>.scala` files. The naming convention
requires each exported leaf name to be unique across *all* families, so that the
name alone identifies the given and re-exports can never collide. This script
enforces that invariant; run it with `make check-givens`.
"""
import re, glob, collections, sys

leaves = collections.defaultdict(set)
for path in glob.glob('lib/*/src/*/soundness_*.scala'):
    text = open(path, encoding='utf-8', errors='replace').read()
    # `export a.b.c.{leaf1, leaf2, ...}` — collect lowercase-initial leaves
    for match in re.finditer(r'export\s+[\w.]+\.\{([^}]*)\}', text, re.S):
        for leaf in match.group(1).split(','):
            leaf = leaf.strip().strip('`')
            if leaf and leaf[0].islower():
                leaves[leaf].add(path)
    # `export a.b.c.leaf` — single lowercase-initial leaf
    for match in re.finditer(r'export\s+[\w.]+\.([a-z]\w*)\s*$', text, re.M):
        leaves[match.group(1)].add(path)

duplicates = {leaf: paths for leaf, paths in leaves.items() if len(paths) > 1}

print(f'{len(leaves)} distinct exported family-given leaves checked')
if duplicates:
    print(f'ERROR: {len(duplicates)} leaf name(s) exported from more than one place:')
    for leaf in sorted(duplicates):
        for path in sorted(duplicates[leaf]):
            print(f'  {leaf}  <-  {path}')
    sys.exit(1)
print('All exported given leaf names are globally unique.')
