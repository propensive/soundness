# Loops and Indexing

Soundness iterates by proof, not by counter. An index is an `Ordinal` confined to the
collection it indexes — `Ordinal in xs.type` — and only a combinator that has already
established the bound can mint one. Reading with a confined index goes through a total
`apply` that yields an element; reading with an unproven one yields an `Optional`. The
pattern being drained is `var i = 0; while i < xs.length`, where the index is a bare `Int`
unconstrained by anything (roadmap core-4, [#1666]).

`etc/check-while-count.py`, run by `make build`, holds the line: no file may gain a `while`,
a `readUnchecked` or a `.charAt(` beyond what `etc/while-baseline.tsv` records.

## What replaces what

| shape | replacement |
| --- | --- |
| counter never used in the body | `repeat(n): …` |
| `xs(i)` over a countable receiver | `xs.extent.each: i => … xs(i) …` |
| the same, over part of it | `xs.extent.capped(n).each`, or `start thru end` from confined endpoints |
| the same, anchored on the collection | `xs.iterate: i => …`, and `xs.iterate(range)` |
| descending | `xs.retrace: i => …` |
| guarded scan whose caller resumes at `i` | `xs.prefix(p)` and `xs.prefix(after)(p)`; the stopping index alone is `xs.spot(p)` |
| trailing trim | `xs.pare(floor)(p)` |
| `iterator.hasNext` | `each` or `fuse` on the source |
| byte-at-a-time cursor | `xs.survey: cursor => …` |
| filling a fresh array | `Array.scribe(n): scribe => …` |
| `y*stride + x` | `xs.lattice(width, stride, offset): lattice => …` |
| fixed count of side effects | `repeat(n): …` |

## Which receivers can be drained

Only a receiver with a `Countable` instance has an `extent`, and only one with an
`Applicable` instance can be read with an ordinal. In practice that means the frozen
`Array[element]^{}` (and `Data`), `Sequence`, `Text`, `IndexedSeq`, `Map`, `Set`, and — behind
a dysasymptotic acknowledgement — `List` and `Chain`.

A loop over anything else has no confined form and is left alone. The receivers that come up
and cannot be drained are the mutable `scala.Array`, `scala.IArray`, a varargs `Seq`, and an
`IndexedSeq` whose elements carry capabilities, since the `Countable` instance is capture-free
and a capability cannot flow into it. A mutable buffer being filled is not an exception to
find a way around: write it through `Array.scribe`, whose handle brands its own indices.

## When a `while` may stay

A `while` may remain only when no expression in its condition is compared against the size of
a collection that the body indexes. Three shapes qualify.

1. **Termination by state, not by bound.** `while !settled do`, `while cursor.more do`,
   `while capacity < required do capacity *= 2`.
2. **Index arithmetic derived from the data.** Knuth division, compressor back-references,
   PNG predictors, a heap sift, a UTF-8 walk advancing by `charCount`. One comment above the
   loop names the invariant that keeps every index in range, and reads use `readUnchecked`
   with that comment as their proof.
3. **The vocabulary's own internals**, in denominative, concordance, zephyrine and proscenium,
   which implement the combinators the rest of the library uses.

## Proof comments

`attested` and `readUnchecked` are the two places a bound cannot follow the program. Each call
site carries a comment, on the line or the one above it, naming the construction that proves
the bound — "`n = min(a.length, b.length)`, checked at :117". The comment is the proof, and
the ratchet rejects an `attested` without one. Prefer the block form, `xs.attested(i): i => …`,
so the attestation's extent is visible.

## Traps

A counter that outlives its loop cannot simply be deleted. It may be shared with a later loop
that resumes from where the first stopped, in which case removing the declaration silently
changes how many times the second runs; or it may be reset before reuse, in which case the
declaration moves. Neither fails to compile.

`each` is a popular name. A same-named extension on an unrelated type, reached through the
companion of a type the file imports, outranks the interval `each` and strips the brand
without any error — the read simply becomes `Optional` again. Such a name never reaches the
umbrella, so the compile error that normally catches a clash cannot fire; the fix is to rename
the other method, as `hypotenuse.Bcd#each` became `eachNibble`.

A name crossing into `package soundness` can fail in three ways, and only the first is the
compile error that the naming philosophy describes as catching clashes. Two modules exporting
one name silently drop one. An unbranded alternative reachable through implicit scope outranks
a branded one and strips the brand with no error at all. And an `export` of an *overloaded*
extension forwards only one overload, so the other vanishes — which is why `thru`, `till` and
`each` are hand-written in their umbrella files rather than exported.

Extension lookup on a branded ordinal is fragile where the brand is a lender's handle: `.n0`
does not resolve on an `Ordinal in scribe.type` until the receiver is ascribed back to
`Ordinal`, as `(i: Ordinal).n0`. Reach for the ascription when an ordinal is reported as
`Required: Int` at its own position.

`extent` and the interval combinators are package-level in `denominative`, so a file that
reaches them for the first time needs `import denominative.*`; only the operations in the
opaque types' own companions arrive through implicit scope.

[#1666]: https://github.com/propensive/soundness/issues/1666
