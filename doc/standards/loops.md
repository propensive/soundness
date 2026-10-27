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
| building a collection from `xs(i)` | `xs.tabulate: i => … xs(i) …` (and `range.tabulate` over any branded sub-interval) |
| `if i < xs.length then … xs(i) …` | `xs.pick(i): i => … xs(i) …`, or `xs.at(i)` when the element is all the body wants |
| `xs(i)` and `ys(i)` over two same-length receivers | `xs.coextent(ys).let: shared => … ys(shared(i)) …`, or `shared.each: (i, j) => …` |
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
and cannot be drained are the mutable `scala.Array`, a varargs `Seq`, and an
`IndexedSeq` whose elements carry capabilities, since the `Countable` instance is capture-free
and a capability cannot flow into it. A mutable buffer being filled is not an exception to
find a way around: write it through `Array.scribe`, whose handle brands its own indices.

## Proven lengths

A brand can record a *length* as well as a bound. `Sized[n]` (proscenium, beside `Populated`)
is a phantom refinement, `Any { type Length = n }`, minted three ways:

- **by construction**: `Array.fill(512)(…)`, `Array.tabulate(8)(…)` and `Data.fill(16)(…)` with
  a literal length return `… & Sized[n]`, as `List(a, b)` returns `& Populated`; a `val` or
  an expression length does not brand (only a compile-time constant is a proof the macro can
  read). The brand is a refinement of the value's type: the collection instances for the
  frozen array (`Traversable`, `Encodable`, `Streamable`, `Countable`, `Applicable`,
  `Terminable`, and downstream `Postable`, `Servable`, `Transmissible`, `Readable`,
  `Encodable in Protobuf`) have a `Sized` instance — spelt with the brand in the `Self`, since a type
  variable bounded by a capturing type acquires `any.rd` — but a *medium* typeclass (`Addressable`, behind `stream`, `Cursor(…)` and
  `Stream(…)`) names the medium a stream is over, where a brand is meaningless, so a `Data`
  meant as a medium names its type: `val payload: Data = Data.fill(100)(_.toByte)`. The rule
  for a new `Data is X` given: if `Self` appears only in argument position, add a `sized…` twin
  (`[length <: Int] => (Data & Sized[length]) is X`) beside it, so a proven block is accepted
  wherever the plain one is; if `Self` is a result or a stream element (a medium), do not;
- **by declaration**: a producer that fixes a layout — a 512-byte tar header, an 8-byte chunk
  header — declares `Sized[n]` on its result and discharges it once at the boundary;
- **by check**: `xs.sized[512]` yields `Optional[xs.type & Sized[512]]`, the counterpart of
  `occupied`.

On a `Sized[n]` receiver, `xs(156.z)` — or `xs(Quat)`, since the ordinal literals inline to
their integers — is read by a macro that decides `156 < n` as it expands: the read is total,
and a literal beyond the length does not compile. The ordinals keep their plain type (a
refined literal type would infer into every `val` that binds it); a variable ordinal on a
`Sized` receiver yields `Optional`, as `at` does, and `pick`/`extent` remain the proofs for
computed indices. Lengths combine at the type level:
`Array.concat` of two proven arrays is `Sized[n + m]`, reduced when both are literals and
carried symbolically otherwise. Prefer the brand to a `readUnchecked` with a comment whenever
the layout is fixed: the byte-probe tests (`header(156.z)`) are exactly this case.

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

Extension members resolve on a branded ordinal: `i.n0`, `i.next`, `i.within(ys)` and the
rest work on an `Ordinal in xs.type` without an upcast, and `i + 1` and `i - j` go through the
brand-generic givens in `Ordinal`'s companion (the result is a plain `Ordinal`, since
arithmetic leaves the proven range). The upcast `(i: Ordinal).n0` is no longer needed anywhere.
What makes this work is inside `denominative.internal`: every inline extension there widens
its receiver with an erased cast (`ordinal.asInstanceOf[Int]`), because the inliner cannot
retype a bare or ascribed receiver against the brand once the call crosses a compilation
unit. Keep that discipline when adding one.

One brand cannot serve two receivers: `Ordinal in xs.type & Ordinal in ys.type` is not a
subtype of either (same-named alias refinements do not intersect into one), so a parallel
read goes through `coextent`, whose token transfers the proof instead.

`extent` and the interval combinators are package-level in `denominative`, so a file that
reaches them for the first time needs `import denominative.*`; only the operations in the
opaque types' own companions arrive through implicit scope.

[#1666]: https://github.com/propensive/soundness/issues/1666
