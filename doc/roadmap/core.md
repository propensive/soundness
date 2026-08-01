# The Standard-Library Drain

Soundness promises that code cannot fail surprisingly at runtime. The Java and Scala standard
libraries make no such promise: their collections throw on empty heads and missing keys, their
I/O throws unchecked, their types equivocate about null. Every place a Soundness module reaches
past its own abstractions into the standard library is a place where that partiality leaks back
in — and a single leak undermines the claim everywhere, because a user cannot tell a sealed
surface from a porous one.

The answer is already in place: proscenium is the opaque boundary. Its collections wrap the
standard library's data structures behind total APIs, its `Array` makes mutation
separation-checked, and the `-Yimports:java.lang,proscenium` prelude makes the curated surface
ambient while leaving everything else deliberately out of reach. What remains is the drain: the
migration shims in `proscenium.compat` exist precisely so that call sites could compile
unchanged on day one, and every one of them is an independently deletable forwarder. The drain
loop is deprecate → fix call sites → delete. This track is complete when the standard library
is invisible — in signatures, in stack traces, in rendered values — everywhere but inside
proscenium itself.

## core-1: sorted and ordered collections have Soundness equivalents

Horizon: near
Baseline: 16 files (measured 2026-08-01)

Files using `TreeMap`, `TreeSet`, `TrieMap`, `SortedMap` or `SortedSet` have no opaque type to
drain to; the equivalents must exist before the drain can take them.

Done when:

    git grep -lE 'TreeMap|TreeSet|TrieMap|SortedMap|SortedSet' -- lib | grep -v '^lib/proscenium/' | wc -l    # 0

## core-2: `proscenium.compat` is empty

Horizon: near → mid
Needs: core-1
Baseline: 475 importing files; the compat file is 551 lines (measured 2026-08-01)

The compat file's own header states the contract: each shim is an independently deletable
inline forwarder, and the file's emptiness is the completion signal. The importer count is the
interim gauge.

Done when:

    git grep -l 'import proscenium.compat' -- lib | wc -l    # 0

and `lib/proscenium/src/core/proscenium.compat.scala` contains no members.

## core-3: no direct `scala.collection` imports

Horizon: near → mid
Needs: core-1
Baseline: 219 files, of which 170 import `scala.collection.mutable` (measured 2026-08-01)

Importing `scala.collection` bypasses the prelude's curation entirely. The mutable imports are
the larger share, and their replacement is not immutability but *safe* mutability:
separation-checked structures in the style of `proscenium.Array`.

Done when:

    git grep -l 'import scala.collection' -- lib | grep -v '^lib/proscenium/' | wc -l    # 0

## core-4: indexed access is total by construction

Horizon: mid
Baseline: 2244 `while … do` loops across 330 Scala files (measured 2026-08-01)

The `var i = 0; while i < length` pattern is maximally efficient and maximally unsafe: the
index is just an `Int`, unconstrained by the collection it indexes. The design in
[#1666](https://github.com/propensive/soundness/issues/1666) makes indexing total by
construction: an index typed `Ordinal in collection.type` can only exist in range, so the
`inline` `apply` taking it is total at zero cost, while an unqualified `Ordinal` reaches only
the fallback `apply` returning `Optional`. Iteration then flows through inline combinators
that supply dependently-typed ordinals, compiling to the same bytecode as the loops they
replace.

Done when: no collection in `lib/` exposes a partial indexed `apply`, and the indexing
`while`-loop pattern is drained. Interim gauge:

    git grep -E 'while .* do( |$)' -- 'lib/**/*.scala' | wc -l    # 2244 and falling

## core-5: nothing Java-shaped at debug time

Horizon: mid → long

A stack trace, a rendered value or a reported type name never exposes a Java encoding:
digression renders traces in Soundness terms, and displayed values are the opaque types, not
their underlying representations. This is what makes debugging feel native rather than hosted.

Done when: a test suite asserts the rendering of representative stack traces, values and type
names contains no Java encodings, and runs in the ordinary suite.

## core-6: the totality audit

Horizon: long

The claim "no partial APIs" becomes checked rather than asserted: an automated audit — in the
style of larceny, or at the bytecode level — verifies that no exported operation throws an
undeclared exception or returns null.

Done when: the audit runs in CI and reports zero violations.
