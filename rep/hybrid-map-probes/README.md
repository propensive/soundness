# `rep/hybrid-map-probes/` — a hybrid `Map`: what the checker allows

Spike for the question "can `soundness.Map` be a flat hash table while it is built and read,
and grow its persistent tree lazily on the first `define`, as one type?" (2026-11-20, Proscala
3.10.0-pro-1.1.4). Self-contained probes, compiled with the pinned compiler and its `-Z` flags:

```bash
rep/hybrid-map-probes/run.sh rep/hybrid-map-probes/q*.scala
```

`*.pos.scala` must compile; `*.neg.scala` must fail, matching each `//EXPECT:` regex. A probe
named `…-ACCEPTED.pos` compiles where one would wish it did not: it records a gap.

| # | Probe | Establishes |
|---|---|---|
| Q1a | `q1a-flat-is-scimap-and-mutable.neg` | The standard library's immutable `Map` is a pure type, so a flat table that extends it cannot also extend `caps.Mutable`. Being a stdlib `Map` (which keeps the opaque alias's right-hand side, `.stdlib`, equality and `hashCode`) and being mutable-classified are mutually exclusive. |
| Q1b, Q1c | `q1b-flat-pure-scimap.pos`, `q1c-builder-escape.neg` | The pure encoding works: a flat table that is a stdlib `Map`, its tree cached lazily behind `updated`/`removed`, filled through a `caps.Mutable` builder lent to a block. The builder cannot escape the block. |
| Q2 | `q2a…-ACCEPTED.pos`, `q2c…-ACCEPTED.pos`, `q2b….neg` | With the alias left over the stdlib `Map`, `Map[k, v]^` can be written (outside the defining scope) and `consume` is tracked, but a writer taking `Map^` accepts a frozen map and a bare one. No write protection. |
| Q3 | `q3-alias-over-intersection.neg` | The classification cannot be borrowed by intersecting the right-hand side with `caps.Mutable`. |
| Q4a–c, f | `q4a…pos`, `q4b…neg`, `q4c…neg`, `q4f…neg` | An alias over a Soundness-owned `caps.Mutable` carrier gives the full discipline: a write through a frozen or bare map is rejected, and so is use after `freeze`. |
| Q4d, e, e4, i | `q4d…pos`, `q4e…neg`, `q4e4…neg`, `q4i…neg` | Under that encoding a **bare** `Map[k, v]` is a read-only view of something possibly still being written, not a pure value — as a parameter, as a declared local, and as a type argument. It cannot be captured by a pure function or stored in a pure value. |
| Q4e2, e3 | `q4e2…pos`, `q4e3…neg` | A class with a bare `Map` field is pure when it holds a frozen map, and capturing when it holds a writable one. |
| Q4g, h | `q4g…pos`, `q4h…pos` | A map can hold capturing values, and a frozen map survives a generic combinator and a type variable in the small. |
| Q5 | `q5a…pos`, `q5b…pos`, `q5c…neg` | A second name for the frozen form — `type Frozen[k, +v] = Map[k, v]^{}`, transparent or opaque — is pure and rejects a still-writable map: the `Array`/`Data` arrangement. |

## In the real build

Reclassifying `proscenium.Map` (alias over a `caps.Mutable` carrier wrapping the stdlib map)
fails in its own 128-line file with 9 errors: a bare `Map[key, value]` reads as `^{fresh.rd}`
in result position too, so the `Factory`, `ToExpr` and `FromExpr` overrides and `Map.from` no
longer conform. Nothing downstream is reachable. `lib/*/src/core*` mentions the bare type 532
times in 145 files.

Declaring the alias frozen — `opaque type Map[key, +value] = MapCarrier[key, value]^{}` — clears
it: from a clean build (`./mill clean`, since an incremental compile gives false greens under
capture checking), proscenium and 52 modules above it compile with no other source change,
among them denominative, murmuration, rudiments, jacinta, anamnesis and revolution. So a
mutable-classified carrier can sit under `Map` at no cost to its users, *provided `Map` names
the frozen form*; the writable form then needs a name of its own. Test and benchmark modules
were not compiled. `proscenium-map-reclassified.patch` is that change.

## Consequences

- "`Map[k, v]^` writable, `Map[k, v]^{}` frozen" on the one name is out: either the name gives
  no protection (Q2) or every bare mention changes meaning (Q4, and the nine errors above).
- Two encodings survive, and both need a second name for the writable form:
  - **pure flat table, lent builder** (Q1b): the flat table is a stdlib `Map`, so the alias,
    `.stdlib` and equality are untouched and no map carries a wrapper; the writable handle is a
    small `caps.Mutable` builder over the same table, and freezing hands the table over;
  - **mutable carrier, frozen alias** (Q4, Q5): one object is both forms, but the alias no
    longer sits over the stdlib `Map`, so every tree-backed map needs a wrapper or a
    Soundness-owned tree.
- The standard library's `Map.from` passes only its own implementations through; anything else
  is copied into a tree. A flat table must enter through proscenium's own cast, never `from`
  (the first attempt at `jacinta-end-to-end.patch` was 20% *slower* for exactly this reason).

## Measurements

These are the spike's measurements, of three throwaway flat-table prototypes: over a
`java.util.HashMap`, over a naive open-addressing table, and over one with stored hashes. The
prototypes are gone; `proscenium.Hashtable` and `Map.build` are the implementation that came of
them, and `lib/murmuration/src/bench/murmuration.MapBench.scala` now measures that:

```bash
./mill -j 4 murmuration.bench.assembly
./etc/ci/fume-run.sh --bench -c out/murmuration/bench/assembly.dest/out.jar 'Map *'
```

Three runs agreed to within a few percent. Times are for 1,024 lookups, or one build; the
call sites are monomorphic, which flatters every contender alike.

| | 1 | 4 | 16 | 256 | 4,096 | 100,000 |
|---|---|---|---|---|---|---|
| **lookup, text keys, found** | | | | | | |
| today's `at` (two probes) | 6.75 µs | 14.99 µs | 33.28 µs | 35.57 µs | 47.96 µs | 56.14 µs |
| stdlib map, one probe | 3.37 µs | 7.54 µs | 8.91 µs | 10.70 µs | 18.55 µs | 27.37 µs |
| flat over `java.util.HashMap` | 3.43 µs | 3.42 µs | 3.47 µs | 3.60 µs | 5.65 µs | 7.20 µs |
| flat, own table with stored hashes | 3.17 µs | 3.13 µs | 3.19 µs | 3.23 µs | 4.99 µs | 5.68 µs |
| **lookup, integer keys, found** | | | | | | |
| today's `at` (two probes) | 2.50 µs | 9.01 µs | 8.58 µs | 16.87 µs | 36.03 µs | 46.58 µs |
| stdlib map, one probe | 0.61 µs | 2.35 µs | 4.37 µs | 7.35 µs | 12.25 µs | 20.48 µs |
| flat over `java.util.HashMap` | 2.72 µs | 3.05 µs | 3.72 µs | 3.46 µs | 4.31 µs | 6.20 µs |
| flat, own table with stored hashes | 1.45 µs | 1.71 µs | 1.79 µs | 1.90 µs | 1.90 µs | 2.56 µs |
| **build, text keys** | | | | | | |
| `define` loop | 0.028 µs | 0.076 µs | 0.461 µs | 8.98 µs | 232 µs | 14.47 ms |
| stdlib builder | 0.027 µs | 0.106 µs | 0.432 µs | 10.21 µs | 177 µs | 9.20 ms |
| flat over `java.util.HashMap` | 0.034 µs | 0.044 µs | 0.109 µs | 1.73 µs | 25.5 µs | 1.16 ms |
| flat, own table with stored hashes | 0.049 µs | 0.050 µs | 0.116 µs | 1.55 µs | 23.3 µs | 1.30 ms |
| **one `define`, text keys** | | | | | | |
| stdlib map | 0.027 µs | 0.201 µs | 0.046 µs | 0.058 µs | 0.083 µs | 0.085 µs |
| flat map, the first (builds the tree) | 0.102 µs | 0.117 µs | 0.390 µs | 10.55 µs | 189 µs | 10.05 ms |
| flat map, every later one | 0.039 µs | 0.041 µs | 0.042 µs | 0.057 µs | 0.076 µs | 0.091 µs |

The naive open table (universal `==`/`##`, no stored hashes) was half the speed of
`java.util.HashMap` on text keys; storing each slot's hash and comparing with `equals` closed
the gap, which is why the third prototype exists.

End to end, `jacinta-end-to-end.patch` points `decodeObject`'s per-record map at the flat
table: "Decode via the Json AST" (100 records of five fields) went from 52.3 µs to 27.3 µs and
from 0.172 MB to 0.138 MB an operation, with "Decode directly with Parsable", which does not
touch the map, unchanged at 10.7 µs. The decoded records are equal to the direct path's.
