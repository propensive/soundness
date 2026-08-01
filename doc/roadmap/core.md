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

## core-4: raw arrays confined to a declared boundary

Horizon: mid
Baseline: 235 files touch `scala.Array` or `scala.IArray` (measured 2026-08-01)

Raw arrays are legitimate at JDK and erasure boundaries and nowhere else. The boundary must be
declared, not assumed: a checked-in allowlist of files permitted to touch raw arrays, so that
every use is either inherent or visible debt. The allowlist then shrinks to the inherent set.

Done when: an allowlist file exists, is enforced by a check in the ordinary build, and

    git grep -lE 'scala\.(Array|IArray)' -- lib | grep -vFf etc/array-boundary.txt | wc -l    # 0

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
