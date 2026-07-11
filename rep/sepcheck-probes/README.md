# `rep/sepcheck-probes/` — separation-checking characterization

Phase 0 of the plan to apply Scala's experimental **separation checking**
(`import language.experimental.separationChecking`, implies capture checking) to the
mutable streaming kernel (zephyrine `Stream`/`Intake`/`Duct`/`Conduit`/`Cursor`) and the
wider streaming stack. The kernel's zero-copy windows (`Stream.window(using Unsafe)`,
`Intake.buffer`, `Cursor.unsafeBuffer`) rely on a single-owner discipline that today is
enforced only by `Unsafe` and doc comments; these probes establish exactly which of those
invariants the checker can carry, on **both** fork toolchain rows.

Self-contained (no Soundness on the classpath), compiled directly with the fork scalac —
never Mill (incremental compilation manufactures false greens under CC):

```bash
rep/sepcheck-probes/check.sh          # both rows: 3.9.0-RC1-propensive + 3.10.0-propensive
rep/sepcheck-probes/check.sh <scalac> # one toolchain
```

`pN-*.pos.scala` must compile; `pN-*.neg.scala` must fail with every `//EXPECT:` regex
matched. `//LIB: <file>` compiles a prerequisite unit first into the probe's classpath
(models separate-module compilation for the cascade probes); `//FLAGS:` adds options.

## Findings (2026-07-11, both rows green unless noted)

| # | Probe | Establishes |
|---|---|---|
| P1 | `p1-mutable-update.*`, `p1-borrow*` | `Mutable` + `update` model works; bare ref = read-only (update call rejected); the **scoped CPS window-borrow** (`reading[T](lambda: (...) ->{caps.any, this.rd} T)`) both **rejects refill-during-borrow** and **rejects storage escape** — the encoding of "window valid only during the borrow". |
| P2 | `p2-anon-factory.pos` | The zephyrine factory shape (anonymous Mutable subclass, private vars, closures over update methods) is green — **including on the 3.9 row**, despite it lacking SepCheck fix `562b513a4e` (2026-07-01, 3.10-only). No cherry-pick needed so far. |
| P3 | `p3-*` | **The cascade is real**: a consumer compiled with ONLY captureChecking (the repo default) already gets the read-only discipline on a bare ref to a sepcheck-defined stateful type — re-parenting kernel types reaches all consumers regardless of per-module gating. The fix is mechanical (`^` on the param). |
| P4 | `p4-*` | **A `Mutable` class may not capture non-Unscoped capabilities** (Unscoped is a classifier) — a Cursor holding an arbitrary `load` thunk cannot extend `Mutable`; it must extend `ExclusiveCapability, Stateful`, which retains the full exclusive/read-only discipline. Pure opaques (`Credit <: Long`), `tracked val`, and `cap^` params coexist fine. |
| P5 | `p5-*` | Concrete arrays support the full repertoire: exclusive `Array[Byte]^` field **reallocated and swapped in an update method** (the `ensureCapacity` shape), `freeze` to `^{}`, mutate-after-freeze rejected. **But Array-as-Mutable does NOT compose through an abstract type member** (`Addressable.Storage`): bare abstract `Storage` params mean *pure*, fresh results can't flow into the abstract view, exclusive abstract fields can't be reassigned. Addressable's primitives must keep untracked storage (or go per-medium concrete); stage-level exclusivity carries the safety. |
| P6 | `p6-*` | Under the sepcheck import, mutable fields are only allowed in Stateful classes; `@untrackedCaptures` (from `caps.unsafe`) is the working escape hatch for a file that opts in before its classes are re-parented (the Phase 1 Conduit strategy). |
| P7 | `p7-*` | `consume` params on constructors and factories work (Cursor-adopts-stream); use-after-consume rejected. **GOTCHA**: in a class body, normal members must be declared BEFORE `consume` methods — a consume method's `Self^` result hides `this` for all later members (template treated as a statement sequence). Upstream-reportable. |
| P8 | `p8-*` | The safe mutable front-end shape works: `consume`-typed extension combinators chain (`Counter(10).mapped(_ * 2).fold(0)(_ + _)`); pulling from an already-piped upstream is rejected; returning an outer exclusive capability from a fresh-result method is rejected (the confinement LazyList could never have under plain CC). |

## Upstream-reportable candidates

- P7 member-order sensitivity (consume method hides `this` from later template members).
- P5 abstract-type-member opacity to the built-in Array-`Mutable` treatment (may be by
  design; worth asking).
