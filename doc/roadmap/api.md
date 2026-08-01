# API Coherence

Predictability is a safety property. A developer — or an agent — who has learned one corner of
Soundness should be able to guess the next corner correctly: similar operations share one API,
variation is expressed in parameters rather than in names, and names are honest. If two
declarations share a name, they are the same thing; if they differ in name, they differ in
meaning. An exported surface that violates these rules taxes every user on every lookup, and it
taxes agents hardest, because agents generalise aggressively from patterns.

The surface has already been measured: `doc/api-reduction-candidates.md` inventories every
identifier the `soundness` package exports, and classifies the reduction candidates — multi-word
names that should nest inside the types that prefix them (C1), non-established abbreviations
(C2), typeclass-backing entities that need not be exported at all (C3), and the homonym and
synonym tables (C4), where one name means two things or two names mean one. That inventory is
scaffolding, not documentation: this track applies it category by category, and ends by
deleting it. The file's absence is the completion signal.

## api-1: multi-word names nest

Horizon: near

The C1 candidates — multi-word names whose prefix already names a same-module type — move
inside that type: `Foo.Bar`, not `FooBar`. Each move ships with migration instructions once
`dist-2` establishes the convention.

Done when: the C1 section of the inventory is empty.

## api-2: homonyms and synonyms resolve

Horizon: near

Every C4-homonym pair either unifies into one declaration or one side renames; every C4-synonym
family collapses onto a single term for the role. Same name, same thing; different name,
different thing.

Done when: the C4-homonym and C4-synonym sections of the inventory are empty, and a
same-name-same-thing check runs in the ordinary build (extending `make check-givens`).

## api-3: the backing entities demote

Horizon: mid

The C3 candidates — typeclass-backing entities and nested derivation objects that exist to be
summoned, not named — are demoted, inlined or de-exported, and the C2 abbreviations are spelled
out or established.

Done when: the C2, C3 and C3b sections of the inventory are empty.

## api-4: naming conformance is checked

Horizon: mid

The naming standards in `doc/standards/naming.md` and `doc/standards/given-naming.md` gain a
conformance sweep, so that drift back into incoherence is caught mechanically rather than in
review.

Done when: a checked-in sweep script reports zero deviations from the naming standards, and
runs in the ordinary build.

## api-5: the inventory is deleted

Horizon: long
Needs: api-1, api-2, api-3

When every category is drained, the scaffolding comes down.

Done when:

    test -f doc/api-reduction-candidates.md || echo absent    # absent

## api-6: surface changes flow through the migration convention

Horizon: long
Needs: dist-2

From here on, coherence is maintained rather than restored: any change to the exported surface
ships with agent-executable migration instructions, per the convention `dist-2` defines. There
is no fear of improving an API, because migration is cheap.

Done when: the `dist-2` CI check has been enforced on every surface-changing pull request for
ten consecutive releases.
