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
rep/sepcheck-probes/check.sh                                       # the pinned toolchain
SOUNDNESS_SCALA_RELEASE=3.10.1-dev-p17 rep/sepcheck-probes/check.sh # the other stream
rep/sepcheck-probes/check.sh <scalac>                              # any other compiler
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
| P10 | `p10-inline-update.pos` | `inline update def` mutating private state (the Cursor hot-path shape) — **requires fork fix #11 `inlineupdate`** (AccessProxies propagates the Mutable flag to accessors for update methods and mutable-field setters; stock compilers fail with "access is in method `Cur$$inline$pos_=`, which is not an update method"). Green on both rebuilt rows. |
| P15 | `p15-shared-tactic.pos`, `p15-exclusive-tactic.neg` | **A tactic classified `SharedCapability` may be aliased** — captured by a summoned codec AND passed as a using-argument to the same call (the `Json#as` shape) — and stays tracked (`^{tactic}` is still demanded); the same program with `ExclusiveCapability` (contingency's `Emit` today) is a separation failure. The root cause of every `unsafeAssumeSeparate(json.as[…])` (2026-10-07, 3.9.1-dev-p17). |
| P16 | `p16-shared-unscoped.neg` | **Shared and Unscoped do not combine**: `Unscoped extends ExclusiveCapability`, and a class extending both is rejected as inheriting "two unrelated classifier traits" (3.9.1-dev-p17 diagnoses it; the least classifier used to collapse silently to `Nothing`). So a shared tactic cannot also be the level-exempt ambient strategy — but `rep/shared-unscoped` shows the pinned release no longer needs Unscoped for the minimal `raises` shape. |
| P17 | `p17-shared-ambient-byname.neg`, `p17-unscoped-ambient-byname.pos`, `p17-sharedunscoped-ambient-byname.pos` | **A shared ambient strategy minted inside a by-name codec thunk is level-checked**: the `throwUnsafely` + derived-`Optional`-field shape. Unscoped passes; plain Shared fails on 3.9 and 3.10; `caps.SharedUnscoped` (proscala `sharedunscoped`, from 3.9.1-dev-p18) passes. The reason `safety-8` needed a fork patch. |
| P18 | `p18-registry-slot.neg` | **A field whose type mentions a fresh parameter capability has no expressible type**: a stateful registry slot `Slot[(workspace: Workspace^) ?=> Unit] \| Null` turns the parameter's `^` into a per-instance `registry.any`, so no handler can be assigned and no workspace passed to one read back. Why exegesis's and espionage's `Registry` slots are an erased `AnyRef \| Null` rim (`[field-fresh-param]`, 2026-10-08, 3.9.1-dev-p19). |
| P19 | `p19-exclusive-monitor.neg`, `p19-shared-monitor.pos`, `p19-shared-monitor-escape.neg` | **The harness overlap is the Tactic overlap for the supervisor**: a task handle captures the `Monitor` it was spawned under and `await` takes the same monitor as a using-argument; exclusive (parasite today) it is a separation failure, shared it passes, and the handle still cannot leave `supervise`. Behind every `unsafeAssumeSeparate(task.await())` (zephyrine, 50) and `(Shell.tmux()(Tmux.completions(…)))` (exoskeleton, 57) (2026-10-08, 3.9.1-dev-p19). |
| P21 | `p21-durable.pos`, `p21-durable-tactic.neg` | **A task body may capture only durable capabilities**: `Durable extends SharedCapability` and its meet with `SharedUnscoped`, `DurableUnscoped` (accepted from proscala p20, `classifiermeet`), admit monitors, connections, loggers and sinks and reject a `Tactic`; the ambient sink and `ThrowTactic` stay level-exempt. Why parasite's task bodies are `->{any.only[Durable]}` (2026-10-09, 3.9.1-dev-p20). |
| P9 | `p9-field-move.neg` | **A tracked capability cannot be moved out of a field**: `val slab = current; current = fresh` fails — the field read widens to a fresh `any` hiding the enclosing instance, rejecting the re-mint and all later `this`-rooted access (no take/replace primitive). Also proven en route: `consume` on an untracked abstract-Storage param is VACUOUS (no use-after rejection). Consequence: Conduit publish and Cursor buffer adoption keep one audited Unsafe swap point each; enforceable `consume` applies to values threaded through call chains, and there is no meaningful file-local Phase 1 — enforcement starts at the re-parenting phases. |

## Upstream-reportable candidates

- P9 field-move: no way to move a tracked capability out of a field (`val slab = current;
  current = fresh` — the read hides the enclosing instance). Blocks checked hand-off/
  adoption patterns; see `p9-field-move.neg.scala`.
- P7 member-order sensitivity (consume method hides `this` from later template members).
- P5 abstract-type-member opacity to the built-in Array-`Mutable` treatment (may be by
  design; worth asking).
