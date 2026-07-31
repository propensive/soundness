# Handover: Buffer migration & honest separation checking (Soundness)

## The big picture

Branch **`collections/iarray-flip`** in worktree
`/Users/propensive/work/worktrees/soundness/numenor/soundness` is a long-running
migration replacing Scala's stdlib collections with opaque, total-API aliases
(`List`, `Set`, `Map`, `Series`, `Progression`, `IArray` in `proscenium`; prelude
`-Yimports:java.lang,proscenium`). It is **PR #1658**, being finished entirely
in-branch before merge.

The current arc is **`Array` → separation-checked `vacuous.Buffer`**. The end goal
(user-stated): use `Buffer` everywhere `Array` is currently used, then **rename
`Buffer` → `Array` and drop native `scala.Array` from prelude scope** — exactly how
`List`/`IArray` were flipped. The prelude already mediates the name: `type Array` /
`val Array` aliases live at `lib/proscenium/src/core/proscenium.prelude.scala:59-60`;
the final rename repoints those two lines at `Buffer`, not every call site.

Toolchain: **proscala fork 3.9.0-RC4-p8** (pinned in `build.mill`, two occurrences).
All 298 build components are separation-checked (`settings.sep` in build.mill; only
the `tests/wasm` scenario harness stays `settings.cc`).

## `vacuous.Buffer` — the vehicle

`lib/vacuous/src/core/vacuous.Buffer.scala`. `opaque type Buffer[e] = Array[e]`
(zero allocation; the mutalias compiler patch — shipped in p8 — classifies the
opaque as mutable so exclusivity is enforced at call sites). API:
- `Buffer[e: ClassTag](size): Buffer[e]^`; `Buffer.freeze(consume b): IArray[e]`
  (zero-copy); `Buffer.grow(consume b, size): Buffer[e]^` (linear growth — note its
  body uses **raw arrays** because in-scope the opaque is transparent so extensions
  don't apply).
- Any ref: `.length`, `.at(i): Optional[e]` (total).
- Exclusive `^` ref: `b(i)` unguarded read + `b(i)=v` write (both **partial/unchecked**
  — bounds are NOT checked, see task #14), `.fill(v)`, `.copyFrom(iarray,...)`,
  `.copyFromBuffer(otherBuf,...)` (rejects self-overlap), `.raw: Array[e]^` (JDK interop:
  `random.nextBytes(b.raw)`, `System.arraycopy(src,_,b.raw,_,_)`).
- Verified element types: `String|Null`, `IArray[_]`, `Data` all fine; only **mutable**
  nested elements (`Array[Byte]` inside a Buffer) fail — retype as `Data`/`IArray`.

## The "honest Mutable" recipe (retiring @untrackedCaptures on codec state)

Exemplar commits: `git show` "Make WebpBitReader honestly Mutable" and the pneumatic
DEFLATE flatten "Flatten pneumatic's DEFLATE engines into honestly Mutable state
machines". Pattern:
1. `class X extends caps.Mutable` (add `import scala.caps`).
2. Delete `@scala.caps.unsafe.untrackedCaptures` on its vars; they become tracked.
3. Every method that mutates state → `update def`.
4. `raises X` / `logs X` sugar on an `update def` → explicit `(using Tactic[X])` clause
   (the desugared context-function result type hides `this`).
5. Consumers hold the instance exclusively: param/val `X` → `X^`; a class *storing* one
   may itself need to become Mutable (recurse).
6. **Mutual-backreference graphs** (A↔B each mutating the other) are a contradiction
   under sep — FLATTEN them into one Mutable class (inline the sub-objects as field/method
   blocks, prefix-rename collisions like `codesMode`/`blocksMode`), or ownership-tree
   (parent owns `child: Child^`, child never points back, needed state passed as
   update-method args), or borrow (sibling passed as exclusive param for the call).

### Checker mechanisms proven to work
- A **field** array reads as `.rd` even inside `update` methods → type fields `Array[T]^`.
- `Array[T]^{this}`-typed **method parameters** accept the object's own fields, are
  writable through, duplicable, chainable — use to pass scratch between private update methods.
- One-cell out-param arrays (JZlib `hn`/`bb`/…) → plain `Int` fields.
- The output buffer is THREADED as an `Array[Byte]^` parameter through the drive loop,
  not stored as a field.
- Fixed/static Huffman-style trees: per-instance mutable clones so all flow through the
  same exclusive-param shape; static constant tables use the `IArray + asInstanceOf` idiom.
- A local `val s = State()` captured by nested defs is HIDDEN by the statement rule (same
  failure arrays hit) → make `state: State^` a **parameter** of the driver, construct it
  inline at the call site. (honeycomb parser learned this.)
- private members reached from update-**inline** helpers must be public (the accessor bridge
  is read-only); nested update-inline expansion loses exclusivity → out-of-line such helpers.
- `this`-derived arguments to methods on exclusive `this` are separation failures → move the
  helper to the companion or restructure.

## PRECISE CURRENT STATE (verify before trusting)

**Main branch `collections/iarray-flip`:** HEAD = `5ebd627fb` (honeycomb parser
restructure), working tree CLEAN, **1 commit UNPUSHED**. Last attested commit is
`0a90dccb1` (before honeycomb) — so honeycomb is untested by the suite/attest and
**needs a full `make test` + `make attest` + `make push` cycle**. Counts on main:
untrackedCaptures **457**, `new Array` (lib non-test/bench) **465**.

**IN-FLIGHT isolated worktree (the interrupted Lzma/Brotli agent):**
- Path: `/Users/propensive/work/soundness/.claude/worktrees/agent-a4c8f4290dba14032`
  Branch: `worktree-agent-a4c8f4290dba14032` (based on `0a90dccb1`).
- **3 COMMITTED** commits (LZMA/XZ side — believed good but NOT yet suite-tested):
  `1e48be33f` Flatten LZMA2 decompressor, `3fdc576ff` Flatten LZMA2 compressor,
  `99c13b6db` Make XZ integrity checkers honestly Mutable.
- **DIRTY / UNCOMMITTED**: `pneumatic.Brotli.scala` + `pneumatic.BrotliDecoder.scala`
  — the agent only did the mechanical annotation-strip (annotations → 0), NOT the Mutable
  flatten. **VERIFIED: this WIP does NOT compile — 64 errors** (`/tmp/brotli-wip-check.log`,
  first at `Brotli.scala:104`, cluster in `BrotliDecoder.scala:195-246`). RECOMMENDATION:
  `git checkout -- lib/pneumatic/src/core/pneumatic.Brotli*.scala` to discard the half-edit,
  then redo Brotli from scratch with the flatten recipe (BrotliDecoder has the same
  backreference disease as DEFLATE — flatten its sub-state into one Mutable class, thread the
  output/window buffers as `Array[Byte]^` params). The LZMA2/XZ 3 commits are the real value
  and are independent of Brotli.
- pneumatic annotations: main **139** → isolated working tree **23** (so this agent's work
  retires ~116; Brotli accounts for the last chunk).

## IMMEDIATE NEXT STEPS
1. **Push honeycomb first** (it's a clean, tested-per-module win sitting unpushed): from the
   main worktree run `make test` → `make attest` → `make push`. (Honeycomb removed 21
   writable/confined casts; 321 tests passed per-module.)
2. **Finish/verify the Lzma/Brotli work.** Check `/tmp/brotli-wip-check.log`. If Brotli WIP
   compiles: commit it, then in the isolated worktree run `./mill pneumatic.test.run`
   (expect 81 pass + the KNOWN pre-existing gzip-duct StackOverflow FATAL — verify it exists
   at base first), plus integration `./mill zeppelin.test.run` and `./mill bitumen.test.run`.
   If it doesn't compile: `git checkout -- lib/pneumatic/src/core/pneumatic.Brotli*.scala`
   and redo Brotli with the recipe (it shares the same backreference disease as DEFLATE).
3. **Merge** the isolated branch into `collections/iarray-flip` (`git merge --no-ff`), then
   full `make test` + `make attest` + `make push`.
4. Continue remaining honest-typing frontier (see roadmap).

## REMAINING ROADMAP (toward Buffer-everywhere + rename)
- **gastronomy** `Digestion` typeclass redesign — mutation flows through a SAM typeclass
  whose derivation givens break under capture tracking; a real design task, not a sweep.
- Small documented residues left annotated at checker-opaque boundaries: hallucination
  (Vp8 segmentProbs/tokenProbs, immutable-after-ctor table holders), ypsiloid/stratiform
  AnyRef-pattern fields + macro-staged capture-erased rims, pneumatic setInput provenance cast.
- Interop-boundary `Array` (JDK `readAllBytes`/`arraycopy`, erased `Array[AnyRef]`) stays
  `scala.Array` by QUALIFIED name forever — same policy as `scala.List` in macro files.
- **THEN the flip**: repoint `proscenium.prelude.scala:59-60` at Buffer; rename Buffer's API
  Array-native (`Array(size)`, `Array.freeze`…); drain residual bare-`Array`-meaning-JVM sites.
- **Task #14 (PARKED by user, but the ultimate goal is full safety):** bounds safety. ~424
  `while i<x.length` loops + ~123 `.stdlib(i)` + ~105 unchecked Buffer applies are convention-
  checked only. Route: (1) total `sweep`/`mutate` combinators (library-only, kills most),
  (2) `Ordinal` `at()` where not primitive-hot, (3) branded/refinement index types (a proscala
  feature — the principled endgame). Do NOT start without user's go-ahead; it's parked.

## BUILD / TEST / ATTEST WORKFLOW
- Per-module: `./mill <mod>.core.compile` / `.test.run` (`./mill resolve <mod>._` to enumerate
  targets). The literal string `Tactic[error]` in warnings is HARMLESS — filter it out.
- Full: `./mill soundness.all` (JVM+WASI; the `finalMainClass No main class` error at the end
  is BENIGN success), `./mill soundness.js.compile`, `./mill soundness.native.compile`
  (crosses cover src/core-native & src/native dirs `soundness.all` misses).
- `make test` = suite (~9,636 tests; green = "9632 passed, 0 failed, +aspire"). `make attest`
  = clean-build + suite + wasm-e2e + crosses in a throwaway worktree, signs a note. `make push`
  = commits + attestation note together. ALWAYS attest+push after a batch.
- **DISK HAZARD (bitten repeatedly):** `anthology.LinkError "linker terminated abnormally"`
  and wasm-e2e TCP/timeout failures during attest are almost always **`No space left on
  device`** (the swallowed cause; instrument `anthology.Linkage`'s catch to see it). Keep
  **≥7GB free**: `rm -rf` old `/var/folders/.../T/<uuid>` dirs, `~/work/proscala-wt/*/{.build,release}`,
  and `/tmp/*out` scratch before attesting. `df -h /` first.
- **Mill cache hazard:** bouncing commits/branches through one `out/` poisons zinc — a full
  `./mill clean` is the only reliable reset after cross-commit checkouts.
- Scratch dotc (for spikes): jars in `~/.cache/soundness/proscala/3.9.0-RC4-p8/lib` + coursier
  compiler-interface/util-interface; ALWAYS `mkdir -p` the `-d` output dir first (a missing dir
  makes scalac silently compile nothing → false "clean").

## PROSCALA SIDE (the compiler fork)
Repo `/Users/propensive/work/proscala` (branch model in its AGENTS.md; LLM_POLICY.md requires
stating LLM use + Co-Authored-By). Two features shipped this arc, both in p8 releases:
`spliceopaque` cc fix and **`iarraypure-mutalias`** (classifies opaque aliases over mutable
types — what makes opaque `Buffer` enforce exclusivity). Release PRs go trunk→release per
stream (3.8/3.9/3.10); 3.9 needs a `prep/release-3.9` resolution-merge branch. No proscala
work is pending right now.

## MEMORY (durable, load each session)
Read `/Users/propensive/.claude/projects/-Users-propensive-work-soundness/memory/iarray-flip.md`
(the master record for this whole arc — Buffer design, tranches 1-3, pneumatic flatten, all
hazards) and `mutable-codec-conversion.md`. Index at MEMORY.md.

## FLIP LANDED (2026-07-30)

The endgame is complete on `collections/iarray-flip` (attested at `c832d6a153`):
`proscenium.Array` IS the separation-checked opaque (ex-`vacuous.Buffer`); the prelude's
scala.Array aliases are deleted; the name Buffer is retired. `scala.Array` survives
qualified-only at JDK/erasure borders; `Array.scratch[T](n)` is the blessed interior
allocation; vacuous supplies the total `at` over proscenium's `readUnchecked`.
Commit trail: `a8c657f373` (relocation), `bda571d774` (spelling qualification
checkpoint), `c832d6a153` (flip). Hazards encountered are recorded in the iarray-flip
memory (export-forwarder capture degradation; grep binary-detection misses; multiline
and factory-position spellings; foreign Buffer/Array names).

Remaining (unchanged): task #14 bounds safety (PARKED); 5 hallucination + 3 pneumatic
documented annotations; PR #1658 merge strategy.
