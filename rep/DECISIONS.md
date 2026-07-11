# Capture-checking conversion — decision log

Judgement calls made during the CC completion work, for Jon's review. Policy baseline
(Jon, 2026-07-04): laundering (`unsafeAssumePure`/casts) is undesirable but acceptable to work
around compiler errors when provably safe and commented; a definite compiler bug may be worked
around by cast/launder with a comment; the patched compiler may be used without further approval.

## ⚑ Open design decisions for Jon (updated 2026-07-06)

1. **Given-captures-context — DECIDED (Jon, 2026-07-06)**: such instances "are most definitely
   capabilities and should be treated as such. This is generally true of any given that takes a
   capability as a parameter, not just Tactics." → capturing instance types, not laundering,
   for the service-class givens (cordillera AsyncTactic, caduceus `couriers.resend`,
   telekinesis.core, cataclysm.core, scintillate.server servers, ethereal DaemonService,
   jacinta.records). OPEN SUB-QUESTION flagged back to Jon: whether the already-shipped codec
   THUNK seals (jacinta/caesura/stratiform — approved in earlier rounds with documented
   invariants) should also be redone as capturing instance types; that cascade runs through
   wisteria derivation and is much larger, so they stand until he says otherwise.
2. **jacinta codec-thunk seal invariants** (shipped, needs blessing): `shape()` must never
   invoke its tactic; inner-codec thunks laundered pure on the argument that instance lifetime
   = given-resolution lifetime.
3. **parasite daemon error propagation** (`ad3511b34`, pre-existing flag): deleted
   handler-nesting tests + Task monad dropped `Tactic[AsyncError]`; bless or redesign.
4. **coaxial `port.listen`** result leaking `any` into `val server` (also surfaces inside
   ethereal.core at `domainSocket.listen`): genuine escape; scoped-API redesign candidate.
5. **zeppelin.core `Zip.Entry`** lazily-captured `storedBytes` thunk: genuine capture
   (LazyList-confinement limit); design case.
6. **tarantula `Navigator`** (browser session as capability), **probably.cli**
   `given testable: Testable = this` — RESOLVED in passing: probes now green (already-enabled
   punctuation.core/probably.cli reconfirmed 2026-07-06).
7. **contingency `dare`** inner `value => success` arrow: the lambda captures the ?=>-bound
   OptionalTactic; needs either a capturing arrow in the signature or the single-layer
   treatment (cannot express dependent capture without curried-dependent CFTs).
8. **Foci `tracks` composition**: the honest type of a `raises…tracks…` function VALUE needs
   curried dependent context functions ("Implementation restriction: not yet supported");
   options = compiler feature work, or a `Validate`/`protect` redesign avoiding function-value
   indirection in tests, or defer the 5 suites.

## Toolchain (CURRENT: the propensive/scala fork — supersedes the 3.8.4-cc1 arrangement below)

Since 2026-07-05 the build uses `/Users/propensive/work/scala` (fork of scala/scala3) as the
compiler source. Branches are `feature/<ver>/<feat>`; the case-2 fix is
`feature/<ver>/unboxedpure`; `feature/<ver>/make` adds a Makefile that builds a full distribution
into `release/` (12 module jars incl. sbt-bridge, staging, the unified scala-library, the Scala.js
stdlib, plus `bin/scalac`). Custom compilers = merge same-version feature branches. The active
build worktree is `/Users/propensive/work/worktrees/scala/soundness-384` on
`feature/3.8.4/make-unboxedpure` (soundness needs the 3.8.4 row — 3.9.0-RC1 can't parse its
experimental syntax). Version baked: `3.8.4-propensive`.

Mill consumes the jars DIRECTLY (no publishing): `build.mill`'s `object toolchain` lays out a
Maven-format repository view (versioned copies + minimal POMs) from `release/lib` into its own
task dest; `trait Toolchain` adds it via `repositories` (covering scala-library force-versions,
scala3-compiler_3/staging_3 module deps, the sbt bridge, and scala3-library_sjs1_3 uniformly) and
overrides `scalaCompilerClasspath`/`scalaCompilerBridge` with content-hashed paths so a rebuilt
compiler is picked up automatically. NB Zinc finds the stdlib in the compiler classpath BY
FILENAME, so that classpath uses the repo's versioned copies. `SOUNDNESS_SCALA_HOME` switches
distributions. After a fork rebuild: `./mill clean` (and re-attest — the compiler lives outside
the CI input digest).

New compiler bugfixes: create `feature/3.8.4/<fix>` in the fork, merge into the build worktree's
branch, `make`, `./mill clean`. First candidate: the inliner-proxy bug (see the anon-instance
class above).

## Toolchain (historical)

- **Patched compiler `3.8.4-cc1`** (in use): the two-change cc/Setup.scala fix for the case-2
  pure-value box (dotty #16978 class), backported from `~/pub/scala3` branch `cc-pure-type-box-fix`
  (`1a871f44bc`, zero regressions on the captures corpus) onto the 3.8.4 tag. Worktree
  `~/pub/scala3-384-cc`, branch `cc-pure-type-box-fix-3.8.4`. Published to `~/.ivy2/local` via
  `sbt publishLocal` (doc generation disabled — the 3.8.4 doc task crashes). Anyone building
  Soundness needs this local publish until the fix is upstreamed; consider `etc/` scripting it.
- **Upstream PR**: the fix + two pos regression tests are PR-ready on `cc-pure-type-box-fix`;
  filing it (against scala/scala3 main, linking #16978) awaits Jon pressing the button.

## capturing-raises SOLVED at source: `caps.Unscoped` ambient tactics (2026-07-06)

Root understanding (from the CC docs Jon pointed at + compiler internals): the `raises`
existential in a non-inline method result accepts only capabilities whose LEVEL is visible from
it. `strategies.throwUnsafely` is a polymorphic given def, so each summon minted a FRESH tactic
at the use site's own level — never visible to the existential ("`any` cannot flow into {any²}").
That is why outer-scope handler tactics (`handle`/`recover`, boundary-based) always worked, why
`inline` methods worked (no method-result existential), and why the whole class only bit ambient
strategies inside deferred blocks.

Approaches tried and REJECTED:
- `Tactic extends caps.Control` (CanThrow-analogy, severing `Tactic <: Emit` with a derivation
  given): contingency compiled (with pure `contramap`/`combine` restrictions) but did NOT fix the
  repro — classification doesn't change level visibility.
- A single GLOBAL `throwTactic` val (holder object + the new `uses` clause on
  `object strategies`): FIXED the repro, but every USE site's enclosing object then needs its own
  `uses contingency.globalTactic.throwTactic` clause — an unacceptable cascade (kaleidoscope.Regex
  showed it immediately; every test Suite object would follow).

ADOPTED: classify the capture-free ambient tactics as **`caps.Unscoped`** ("capabilities that may
escape their environment" — the classifier guarantees they capture nothing scoped, and the level
check `acceptsLevelOf` exempts them). Changes (contingency only):
- `ThrowTactic extends Tactic[error], caps.Unscoped`; its `CanThrow` evidence moves to the
  `strategies` givens (even an ERASED constructor param is a retained capture!), and
  `record`/`abort` import `canThrowAny` locally (the uncheckedErrors idiom).
- New `UncheckedTactic`/`FatalTactic` classes, likewise Unscoped, replacing the anonymous given
  instances (the classifier must be on the declared type for the summon's fresh to adopt it).
- Boundary/handler tactics are untouched: they genuinely capture their `Label` and remain scoped
  — a boundary tactic escaping its boundary is still an error, which is exactly the guarantee
  soundness wants from CC.
Verified: rep/capturing-raises (zeppelin, the canonical red) is GREEN; the zeppelin cone compiles.

All-suite triage on the Unscoped design (full-clean gate, 37 enabled): **5 new greens —
apoplexy, ethereal, profanity, scintillate, archimedes** (exactly the pure ambient-tactic
suites; total 86/127). The 32 residual reds are the OTHER known classes, now with sharper
diagnostics:
- NEW crisp shape for the handler-provided cluster (zeppelin, telekinesis, ziggurat, burdock…):
  `Found: (contextual$N: Tactic[E]) Required: Tactic[E]^{any}` — a tactic BOUND AS A PARAMETER of
  an inner lambda (e.g. `capture[E]{…}`'s block) cannot flow OUTWARD into the enclosing method's
  local `any` in the callee's expected type. This is a level violation in the opposite direction
  from the ambient case — scoped handler tactics working as designed OR a follow-up encoding
  question for `capture`/the harness; next round's target.
- exoskeleton/obligatory/surveillance: safely/OptionalTactic boundary `Label` fresh-vars.
- caesura/jacinta/stratiform/xylophone/ypsiloid: the Foci `tracks` helper class (unchanged).
- austronesian/aviation: `unsafely` nested context-fn; quantitative/cataclysm/honeycomb: macros;
  plutocrat/serpentine/galilei/coaxial/parasite/baroque/breviloquence/enigmatic/panopticon/
  superlunary/wisteria/cordillera/perihelion/contingency: individual deep shapes as previously
  recorded.

## Post-Unscoped core probe (2026-07-06)

The five raises-blocked cores do NOT clear under the Unscoped design: anthology.bundle and
telekinesis.core stay red (handler-provided existential and given-captures-context respectively —
both already-catalogued residual classes); ethereal.core, scintillate.server and
embarcadero.containerd sit downstream. All five stay deferred pending those two classes.

## Compiler fix #3: `feature/3.8.4/aliascap` (2026-07-06) — handler-provided cluster SOLVED

The "handler-provided tactic" class (`Found: (contextual$N: Tactic[E]) / Required:
Tactic[E]^{any}` at raises-method calls inside `capture[E]{…}` blocks) is NOT about handler
tactics at all — the block shape was incidental. Root cause, isolated with a new SELF-CONTAINED
three-pass repro (`rep/handler-raises/`, check.sh compiles Contingency.scala CC-on, Zip.scala
CC-OFF, Use.scala CC-on): **mixed-compilation interop**. A method in a NON-capture-checked
module whose signature mentions the CC-compiled `raises` alias keeps the raw alias application
in its fluidified info (`Setup.fluidify` doesn't follow aliases); rechecking a call dealiases
LATE and exposes the cap from the ALIAS SYMBOL's own transformed info — where
`transformExplicitType`'s final `globalCapToLocal(tp2, Origin.InDecl(sym))` had pre-localized
`caps.any` to a LocalCap owned by `type raises` itself (`-Ycc-verbose`: "<any created in type
raises in the type of type raises>"). A package-level capability that no use-site tactic can
flow into. CC-compiled callees never consult that info (their Setup dealiases the alias and
localizes per use site) — which is why all-CC and single-pass variants were always green, and
why the class only ever bit test suites/cores sitting above a non-CC module (zeppelin.core,
ziggurat.packager, telekinesis.core…). Upstream: RED on stock 3.8.4 AND on `all/3.10.0` — an
unfixed upstream bug; the self-contained repro is upstream-report-ready.

Negative results first (both proven on the calibrated repro before the compiler fix):
- `inline def capture` + `inline block` does NOT fix it (the plan's recommended source fix):
  the inlined boundary tactic becomes a caller-local val and STILL cannot flow into the
  alias-level `any`.
- Everything the earlier triage recorded (explicit `raises` ascription, `unsafely{}`) fails for
  the same reason — nothing at any use site can reach the alias's own level.

FIX (fork branch `feature/3.8.4/aliascap`, one guard in cc/Setup.scala:540): skip the final
`globalCapToLocal` when `sym.isAliasType`, so a type alias's own info keeps `caps.any` global
and recheck freshens it per application (`Origin.Formal`) — exactly what CC-compiled callees
get via setup-time alias expansion. Verified: repro GREEN (mixed) + GREEN (all-CC unchanged);
REAL zeppelin_test.scala and ziggurat_test.scala compile single-shot under CC against their
captured classpaths. Corpus gate: before/after pass-fail + neg-output-hash diff over
tests/{pos,neg}-custom-args/captures, 616 entries — ZERO pass/fail changes; 3 neg tests
(boundschecks2/3, i19330) change only a diagnostic NOTE ("a root capability in the type of
type T" → "the root capability caps.any"), errors unchanged. Committed `5c0d44f33e` on
`feature/3.8.4/aliascap`; build worktree branch renamed to
`feature/3.8.4/make-unboxedpure-splicealias-aliascap` (merge `089fe22c4f`); both pushed.
The superseded remote name `feature/3.8.4/make-unboxedpure-splicealias` was left in place
(deleting remote branches needs Jon). Upstream-report candidate: rep/handler-raises is
self-contained and red on all/3.10.0.

### All-suite probe under aliascap (2026-07-06, single-shot dotc per suite via rep/probe-suite.sh)

Full-clean gate on the pre-existing enabled set first: 16497/16497 SUCCESS. Then all 38
deferred suites probed against freshly-captured CC classpaths. **12 new greens — burdock,
cordillera, cosmopolite, embarcadero, exoskeleton, gigantism, legerdemain, orthodoxy,
surveillance, telekinesis, zeppelin, ziggurat** (the whole handler-provided cluster plus most
of the safely/OptionalTactic class and five never-diagnosed suites). 26 still red, first-error
classes (logs in rep/_probe/<mod>/compile.log):
- **Foci/tracks cluster unchanged** (caesura, jacinta, stratiform, xylophone, ypsiloid): the
  helper-param lambda `_.as[R]`'s inner arrow captures the Foci binder (`?->{x$0}`) and won't
  conform to the pure declared chain. Tried and REJECTED at source: (a) `inline` helpers — the
  transparent-inline `validate` DSL types as `Any` inside an inline body ("value protect is not
  a member of Any"); (b) dependent CFT declaration `(foci: Foci[CellRef]^) ?=> Tactic^ ?->{foci}
  result` — "Implementation restriction: curried dependent context function types not yet
  supported"; (c) single-layer `(Foci[CellRef]^, Tactic[DsvError]^) ?=> result` — still
  "capability any cannot flow into {}" (invariant Foci type argument). Needs a
  validate/protect-side design, own round.
- **Singles as previously catalogued** (first errors unchanged in kind): austronesian (Unsafe
  nested ctx fn), aviation (protointernal Instant over Monotonic), baroque (?1.Result),
  breviloquence (Cbor Ast union), coaxial (SocketService^{any} escape), distillate (Extractable
  refinement), enigmatic (BlockCipherPadding^{any}), exegesis (TestServer needs to extend
  Capability), galilei (Path refinement), honeycomb (Text^'s1), panopticon (Optic refinement),
  plutocrat (path-dependent Money), serpentine (Text^'s1), wisteria (tactic-capture on
  Readable), quantitative + cataclysm (macro-under-cc).
- **Sharper now**: contingency.test (1 error: `dare`'s inner `value => success` arrow — the
  lambda captures the ?=>-bound OptionalTactic, level-blocked; contingency API question);
  parasite.test (1 error: AtomicReference boxed union at `captured.get().nn`); superlunary
  (dispatch fresh into Rig result + staging Conversion override); obligatory (parasite.Worker
  ?-> vs ?=>); perihelion (Message handler ?->'s1 vs ?=>).

Core probe (rep/probe-core.sh over the 23 deferred non-plugin components): **beneficence.core
GREEN** (flipped on); anthology.bundle/ethereal.core = method-result-existential (NOT fixed by
aliascap — the fresh lives in the enclosing method's own result type, a different level wall);
apoplexy.core/caduceus.resend/synesthesia.core/exegesis.core = the jacinta thunk cluster
(below); the rest unchanged in kind.

## jacinta codec-thunk seal (2026-07-06) — jacinta.schema GREEN, cluster reclassified

The jacinta-derivation cluster's mechanism, pinned by the post-aliascap probes: given
resolution synthesizes by-name thunks (`() => Json.boolean(x$1)`) for jacinta's PURE `->`
parameters — `{Encodable,Decodable}.apply(shape0: -> Morphology)` and the six
`-> (inner is Json.{De,En}codable)` collection/optional givens — and the consumer given's
`Tactic` param makes those thunks capture-carrying, violating the empty capture set.
FIX (jacinta.Json.scala): the eight slots became impure `=>` and are IMMEDIATELY laundered to
pure `() -> …` thunk vals via `caps.unsafe.unsafeAssumePure`, with safety comments (policy:
launder-with-comment). Safety argument: `shape()` only reads static metadata and never invokes
a tactic (documented invariant, ⚑ for Jon); the inner-codec thunks' captured tactic has the
same lifetime as the produced instance (both bound by one given resolution), preserving the
pre-CC status quo. The HONEST alternative — capturing instance types
(`(value is Json.Decodable)^{tactic}`) — is the given-captures-context design question; the
launder keeps codec instances pure until that is decided.
Result: **jacinta.schema GREEN** (flipped). apoplexy.core/caduceus.resend/synesthesia.core/
exegesis.core advance past the thunk errors and RECLASSIFY:
- `$_lazy_implicit_$N^{x$1, x$2}` vs pure (apoplexy:164, exegesis:72): the compiler-synthesized
  LAZY IMPLICIT wrapper class for recursive giv derivations captures the tactic params. New
  named class: **lazy-implicit-capture**. Possibly compiler-side; not yet reduced.
- `new couriers.resend(using x$1,…)` (caduceus:60): a given CLASS retains its Tactic given
  param as a field → instance type captures. This IS given-captures-context (telekinesis.core,
  cataclysm.core family) — genuine design decision for Jon.
- macro Expr shapes (synesthesia.internal:106 `Expr.summon[Tactic[JsonError]]` bound mismatch,
  apoplexy.internal:517 `asExprOf`): macro-under-cc family.

## Compiler fix #4: `feature/3.8.4/ctxresult` (2026-07-06) — stacked context-function results

anthology.bundle's residual (and embarcadero.containerd's, and parts of ethereal.core's) turned
out to be a SECOND fundamental compiler gap, isolated self-contained in `rep/stacked-raises/`
(check.sh; SingleLayerControl.scala proves the single-layer case is fine): a method whose result
STACKS capability context functions — `X logs E raises F`, or literal `A^ ?=> B^ ?=> T` — cannot
reference the outer layer's parameter from the inner layer's body:

    Reference `contextual$N` is not included in the allowed capture set {any}
    of an enclosing function literal ... any is a root capability in the result type of method f

Red on stock 3.8.4 AND upstream main; alias-independence proven (literal arrows fail
identically). Erasure FLATTENS these closure chains' parameters into the method's own parameter
list (`ContextFunctionResults`/ContextResultCount), so the level wall contradicts the
post-erasure reality. FIX (`feature/3.8.4/ctxresult`, `ae0b9842bf`, 36 lines):
`CheckCaptures.recheckDefDef` registers the chain closures (up to `contextResultCount(sym)`)
in a new `CCState.contextResultClosures` map; `acceptsLevelOf` maps a reference whose level
owner is such a closure up to the method. Corpus: ONE pass/fail change — neg test
`erased-methods2.scala` (header: "was a neg test before") now compiles; it encodes exactly this
limitation (stacked-CanThrow/safer-exceptions pattern), moved to pos in the commit. Build branch
now `feature/3.8.4/make-unboxedpure-splicealias-aliascap-ctxresult`; both pushed.

Post-fix probe of all residual reds: **new greens = embarcadero.containerd, obligatory.grpc,
stratiform.binary** (stacked-CFT class) plus census-gap discoveries below. anthology.bundle
ADVANCES to the serpentine `toward` macro class (`Found: Path{…}^'s2 / Required: (file : …)^{file}`
— inline-proxy singleton, macro-under-cc); ethereal.core advances to DaemonService
instance-capture (given-captures-context family) + the coaxial SocketService escape.

### Census correction (2026-07-06): multiline-extends targets were invisible

The earlier build.mill census regex missed submodules whose `extends` sits on a continuation
line. Newly probed: **ziggurat.packager GREEN, embarcadero.oci GREEN** (flipped);
punctuation.core/probably.cli were in fact ALREADY enabled (commit 248475a17 — the earlier
"red, reverted" note was stale; probes reconfirm green). Still red, new shapes catalogued:
burdock.core (E223 + fallible union-tactic chain), cordillera.core
(`given Tactic[Http2Error] = AsyncTactic()` — given-captures-context), superlunary.core
(compiler CRASH: MatchError on a Quote tree during recheck — macro-under-cc, crash variant).

## Phase 4 + unsafely fusion (2026-07-06): quantitative & cataclysm tests GREEN

- **quantitative.test**: the `checkable` MatchError was `UnitsMap.apply`'s `recur` falling to
  the empty map on `@caps.internal.inferred`-wrapped measures (`readUnitPower` had the strip;
  `recur` didn't). Strip added; the "no descriptive name" errors were downstream of the same
  empty map. GREEN (core stays non-CC — the transparent-macro pickling constraint on ENABLING
  quantitative.core is unchanged).
- **cataclysm.test**: `attributeFor`'s `ConstantType(StringConstant(…))` match got the same
  strip (cataclysm.protointernal.scala; gotchas: needs `import scala.compiletime.asMatchable`,
  and a case binder named `underlying` collides with anticipation's extension). GREEN.
- **`unsafely` single-layer fusion** (contingency_core.scala): the block type
  `Unsafe ?=> (ThrowTactic, CanThrow) ?=> success` was the stacked-CFT problem in VALUE-PARAM
  position (fix #4 only covers method results); fused to
  `(Unsafe, ThrowTactic[error, success]^, CanThrow[Exception]) ?=> success`, the documented
  `safely` idiom. Clears austronesian's `unsafely(…decode…)` sites and aviation's ts""-macro
  splices. GOTCHA repeated: after changing a signature spliced INTO quotes (`'{unsafely(…)}`
  in aviation.internal), dependents' pickled quotes are STALE under mill incremental — clean
  the quoting module (aviation.core) before judging errors.
- Residuals: austronesian now 1 error (enum-union `& Color^'s1` intersection — wisteria
  delegate typing); aviation back to its path-dependent `Instant over Monotonic` single.

## Typeclass de-Pure convergence (2026-07-06, in progress — SCOPE DECISION PENDING)

`Typeclass` no longer extends `Pure` (Jon's ruling). Wave-by-wave convergence so far:
- Wave 1-2: 35 `contramap`/`map` combinators → `(self2 is TC)^{this, lambda}` (precise; plain
  `^` was too coarse — it poisoned derived givens off GLOBAL receivers, e.g. fulminate's
  `text.contramap(_.tt)`; and `^{this}` fails where `this` is untracked-empty, e.g. Loggable —
  left bare there).
- Typeclass-as-SharedCapability EXPERIMENT (for implied captures / "no ^ anywhere"): REJECTED —
  `CSImpliedByCapability` is unconditionally `{any}`, so every companion holding given VALS
  (symbolism.Addable etc.) is forced to become a capability. Typeclass is now a PLAIN trait.
- Waves 3-4: latent tactic-capturing SAM givens, sealed pure with `unsafeAssumePure`
  (whole-instance form): distillate.Decodable primitives ×9 + Extractable + Requirable,
  capricious.Randomizable ×3, spectacular.Inspectable ×10.
- Wave 5 (current frontier): ambience.Property, geodesy, urticose ×8, obligatory framers ×3,
  turbulence Writable/Readable/Streamable/Aggregable ×9 — same shape.
- MEASURED TOTAL: ~145 tactic/emit-parameterized givens in 77 files tree-wide.
SCOPE DECISION (Jon, 2026-07-06): **(B) — honest capturing types**, falling back to (A) per
family only where structurally blocked. THE (B) PLAYBOOK, converged over
Framable/Writable/Readable/Streamable/Property/decodables/guillotine:
1. Capturing given result: `given name: (tactic: Tactic[E]) => ((X is TC)^{tactic}) = …` —
   DOUBLE parens required (the given-signature grammar rejects postfix `^{…}` after `=>`);
   all params in a clause must be named if any is.
2. Consumer evidence: context bounds cannot express capture — convert to explicit
   `(using tc: (X is TC)^)`. Strict-result methods (decode/writeTo/stdin) need nothing more;
   the capture matters only during the call. Hub conversions (distillate `.decode`,
   turbulence `writeTo`) absorb most downstream breakage.
3. Trait-level private helpers called from instance SAMs make every instance capture the
   TRAIT (`Tel2.this`) — hoist them to file level (stratiform `primitiveFault`).
4. Local `given Tactic = <launder>` vals are now THEMSELVES tracked (Tactic derives
   Capability → implied `^` on the val's type): replace with per-call `unsafely(…)`, whose
   tactic is minted inside so the closure stays pure (ambience javaClassVersion, serpentine
   trustedInstantiable).
5. Factory `apply`s: capture-polymorphic param + dependent result
   (`def apply(decoder: (X is D in F)^): ((X is TC)^{decoder})` — stratiform Tel.Decodable).
6. FALLBACK (A) seals remain ONLY for by-name element-codec params (`decodable: => inner is …`)
   which cannot be named in capture sets: jacinta collections, locomotion Protobuf collections,
   spectacular Inspectable collections, capricious Randomizable, distillate primitives (could be
   converted to named-tactic honest form later), caesura/stratiform/telekinesis codec factories.
7. Lazy-Stream results (`Streamable.stream`) cannot carry captures (Stream is pure — the
   LazyList confinement limit): the INSTANCE capture is tracked honestly; the stream's
   retention of it remains unexpressed, as before.

## Compiler fix #5 (2026-07-07): cast-argument boxing — the "inliner-proxy/aka-Tagged" class

Jon chose the fork fix over the interim revert. Root cause isolated with a NEW self-contained
repro (`rep/proxy-tagged/` — opaque covariant `Tagged` + companion `apply` + `aka`-style
extension + a capturing given, all inline): Setup boxes the type arguments of an UNDEALIASABLE
application (an opaque spelled outside its scope) via `normalizeCaptures`, but the special case
for `asInstanceOf`/type-test type arguments (Setup TypeApply traversal) skipped that boxing —
so the cast inside an opaque companion's `apply` produces an UNBOXED spelling of a type whose
canonical declaration-side spelling has boxed arguments, and the two spellings cannot compare
("is boxed but … is not"; with a singleton argument it surfaced as the
`Tagged[(any$proxy : …)^{any$proxy}]` mismatch). Diagnosis method: `Thread.dumpStack()` trace on
self-capturing-singleton creation → `boxDeeply` ← `normalizeCaptures` ← `transformResultType`,
then repro bisection (no-unwrap variant compiles; widened variant shows the raw box-status
message). FIX: box the arguments of undealiasable applications in cast type arguments exactly
as `normalizeCaptures` does elsewhere (one guarded case in Setup's TypeApply traversal).
Verified: repro GREEN; ulysses.core regression cleared; corpus fix4→fix5 diff EMPTY (modulo the
erased-methods2 relocation from fix #4). To be committed as `feature/3.8.4/castbox` once the
tree is green.

### ⚠ CORRECTION + resolution of the Setup wedge (2026-07-07 midnight)

**Everything tested between ~22:40 and ~23:20 ran a STALE 19:12 compiler jar**: the fork's
`make` does NOT propagate compile failures (exit 0 with errors in the log), and the LazyRef
guard had a visibility bug (`private val` in trait SetupTypeMap is invisible to
`toCapturing`'s `innerApply` — must be `protected`), so three successive "rebuilds" silently
failed. The "guard-only fixes gossamer" observation was actually the staleread build.
ALWAYS verify `release/lib/scala3-compiler.jar` mtime after `make`.

With the visibility fixed and a VERIFIED-fresh build, the true resolution:
- staleread REVERTED (still correct to drop it);
- ★ fix #7 = the SetupTypeMap **LazyRef cycle guard** (protected set, re-entrant LazyRefs
  returned unmapped) — cures the gossamer/suspension crash;
- ★ fix #8 = the SetupTypeMap **inert-type cache** (types mapping to themselves are never
  re-walked) — cures the exponential re-expansion of nested alias unions
  (honeycomb `Flow`/`Phrasing`; minimal repro: one `def f(x: Html of Flow)` never
  finished; STOCK RC1 hangs identically, 3.10.0 stream terminates — upstream-report both);
- fix #6 permitlazy, fix #9(sic, order) nullreceiver as before.
Full battery GREEN: gossamer, formrepro, legerdemain, punctuation, graffiti, proxy-tagged,
mandible-repro. Final clean+incremental gates running.

### 3.9.0-RC1: the honeycomb-consumer Setup wedge (late evening)

- `staleread` (fix #7) REVERTED: its semantics (serving newer-run denotations to stale-run
  readers) produced cyclic type views — first an infinite `current` loop, then an unbounded
  Setup alias recursion. Two failure modes were enough; wrong approach.
- ★ What ACTUALLY fixes the gossamer suspension crash: the **SetupTypeMap LazyRef cycle
  guard** (re-entered LazyRefs return unmapped; `mapConserveSuper` forced them eagerly).
  Deterministic repro flips RED→GREEN with the guard alone. Fix #7 is now the guard.
- ★ SEPARATE upstream RC1 defect: legerdemain/punctuation/graffiti (all heavy honeycomb
  consumers) WEDGE in CC Setup — `innerApply → mapOver(OrType) → apply` recursion over
  honeycomb's giant union types, interleaved with `mapFollowingAliases` and the
  `containsGlobalFreshDirectly` depFun conversion. **Stock 3.9.0-RC1 hangs identically**
  (coursier scalac, 5-min timeout) — NOT caused by our fork fixes. Testing the 3.10.0
  stream (all-main build) to decide backport-vs-new-fix.
- Jon: the 3.10.0 branch stream exists but needs more porting work before it could be a
  target row.

### 3.9.0-RC1 true-migration tail (session 2, evening)

⚠ The first "green" 3.9.0 gates were PARTIALLY 3.8.4-CONTAMINATED: running with
`SOUNDNESS_SCALA_HOME` but the old `3.8.4-propensive` version string let coursier serve
cached 3.8.4 artifacts for parts of the classpath (the exact hazard the build.mill comment
documents). Only gates with the BAKED `3.9.0-RC1-propensive` string are trustworthy. The
real RC1 tail, discovered iteratively:
- larceny plugin: `.nn` on `String.substring` (compiler API explicit-nulls).
- Self-aliased typeclass traits need explicit self types once combinators return capturing
  results (Loggable, Aggregable, Digestible, Rasterizable, Audible; swept all
  `trait X extends …Typeclass…: alias =>` occurrences).
- RC1 tracks by-name captures in SAM closures: gastronomy Digestible collection givens
  re-sealed (codec-thunk pattern); Digestible.contramap kept the PURE `->` form with a
  documented deviation from the impure-lambda convention (capturing form contaminates all
  SAM conversions in the file).
- turbulence Aggregable.bytesText: honest `^{decoder}` result.
- ★ FORK FIX #8 `nullreceiver`: backend crash «Cannot create ClassBType for special class
  symbol Null» (honeycomb) — genCallMethod's `useSpecificReceiver` guard uses
  `defn.isBottomClass`, which under `Mode.SafeNulls` (-Yexplicit-nulls) no longer counts
  `Null` as bottom, letting a Null-typed receiver reach descriptor emission. Fixed with the
  after-erasure bottom check / widening to Object. NB `make` branches TRACK `release/` and
  `.build/` — never `checkout -B` a bare-base branch inside the build worktree (artifact
  conflicts produce half-applied states); cherry-pick the compiler file out instead.
- ★ FORK FIX #7 `staleread` (earlier today): `current` returns the fresher denotation when
  a stale-run context (LazyRef/TypeMap closure surviving a macro-suspension re-run) reads
  an already-migrated denotation; the first attempt (guard in bringForward) LOOPED —
  the guard must live in `current` before the `toNewRun` dispatch.

### 3.9.0-RC1 migration (2026-07-07, session 2 continued)

Jon's direction: adopt the 3.9.0-RC1 fork row. State:
- Ported aliascap (one Setup.scala conflict: 3.9.0 adds an `initialVariance < 0` guard before
  the alias branch — merged as `else if sym.isAliasType`), ctxresult and castbox (clean
  cherry-picks) onto `release-3.9.0` (== tag 3.9.0-RC1); built
  `feature/3.9.0/make-unboxedpure-splicealias-aliascap-ctxresult-castbox[-permitlazy]`
  (worktree /tmp/fork-390-port, `VERSION 3.9.0-RC1-propensive`).
- ★ The June "gossamer denotation crash" (SortedSetFactoryDefaults invalid in run 2) does
  NOT reproduce under the five-fix compiler — gossamer.core compiles under CC on 3.9.0.
- ★ NEW FORK FIX #6 `permitlazy`: 3.9.0-RC1's sealed-Java-classes support (#25788) resolves
  `PermittedSubclasses` children EAGERLY during the enclosing completion → completion cycle
  on JDK 25's `java.lang.classfile` (upstream #25451; «Cyclic reference involving class
  BufferedMethodBuilder»). Fixed by moving `getClassSymbol` inside the deferred ChildAnnot
  tree. Repro: /tmp/mandible-repro (RED without, GREEN with). NB mandible itself no longer
  uses `java.lang.classfile` (own parser since the June note) — the fix future-proofs
  staging/runtime compilation on JDK 25.
- NB upstream `release-3.9.0` has moved past RC1 (prep commits); we stay on the RC1 tag per
  Jon's instruction. Rebase the row when 3.9.0-final lands.

### ⚠ CURRENT STATE (2026-07-07, session 2): CLEAN GATE GREEN 16508/16508 — commit series next

(Verified by a full `./mill clean` + `test.assembly` sweep, 425s, plus an incremental sweep
after a style-only cleanup pass over the new jacinta/JsonSchema code. NB the house-lint
SN-811 «space inside ( … )» warnings fire across the whole conversion diff's `=> (tactic: …)`
parameter blocks — a mechanical sweep candidate for Jon to rule on before or after merge.)

Fork fix #5 COMMITTED & PUSHED: `feature/3.8.4/castbox` (0159385d0d, off the 3.8.4 tag),
merged as `feature/3.8.4/make-unboxedpure-splicealias-aliascap-ctxresult-castbox`; the merge
reproduces the working Setup.scala byte-identically, so the already-built `release/` needs no
rebuild. This session's conversions (all incremental-green; clean gate = final word):
- locomotion.Protobuf: sealed the 7 primitives the earlier re-seal missed (int, u32/u64/
  s32/s64/b32/b64) + laundered the conjunction/disjunction decode lambdas (they close over
  the `provide`-summoned tactic) — cleared locomotion.test AND embarcadero.containerd.
- octogenarian_core `environmentDefaultGitCommand`: Instantiable evidence → `(…)^`
  (NB the `^` needs wrapping parens before a continuation `=>` line or the parser stops).
- ambience: Xdg methods un-sugar bounds to `(using instantiable: (…)^)`; Variable
  Instantiable-family givens take capturing evidence but their RESULTS are re-sealed bare
  (unsafeAssumePure), because `Environment.apply/selectDynamic`'s reader params must stay
  pure — they are summoned INSIDE STAGED QUOTES (ethereal daemon cli blocks).
- anticipation Paths.Resolver trusted/fromText: same seal, same quote reason
  (workingDirectory/temporaryDirectory inline resolvers expand inside quotes).
- guillotine Computable.instantiable: honest capturing (evidence + `^{evidence}` result).
- turbulence `load` extension: bound → `(using loadable: (result is Loadable by Text)^)`
  (cleared savagery.test).
- jacinta: fetchingRegistry given → named params + `JsonPointer.Registry^{client}` result
  (instance retains HttpClient); JsonPointer.apply registry param → `(…)^`; JsonSchema
  decodable re-sealed whole-instance with LOCAL laundered primitive codecs + a laundering
  `field` helper; discriminate reads `.root.string` directly (the jacinta.time dodge).
- ★ QUOTE RULE (hard-won): capturing by-name codec params (`=> ((…)^)`) and `^` on
  Json's at-Focus `as`/decodableAtFocus produce «Illegal capture reference: (?1 : Any)»
  when the summon chain is inline-expanded inside staged quotes (superlunary Stageable.json
  deserialize/extract under Rig.dispatch → ethereal/exoskeleton/profanity tests). Those
  were tried and REVERTED to bare; jacinta collection by-names must stay pure thunks.
  Trailing suspicion: the last ethereal.test ?1 (with everything reverted) is stale-zinc;
  clean gate decides. If it persists there, the ?1 minter is extract's
  `provide[entity is Decodable in Json]` expansion — debug with
  `scalac @/tmp/eth-args.txt -Ydebug-error` (args file has the captured classpath).
Remaining after gate: commit series + attest.

CLEAN-GATE VERDICT (11:09): the ?1 cluster was REAL, not zinc-stale — and several
incremental greens (punctuation, apoplexy, embarcadero) were stale. Root cause of the whole
tail: capturing PRIMITIVE codec givens (`^{tactic}`) meeting (i) wisteria's bare field
summons — including inside the `'{wisteria.internal.field[…]}` macro splice, where the
capture prints as «Illegal capture reference (?1 : Any)» — and (ii) superlunary's
quote-expanded Stageable bodies. Resolution per the sanctioned fallback clause: SEAL the
derivation-facing primitives — jacinta's 10 `Json.Decodable` primitives + `bytes` + the
`Json`-from-`Text` parser givens, xylophone's 7 `Decodable in Xml` scalars, gesticulate
`MediaType is Decodable in Text`, obligatory `Sse is Decodable in Text`. The capturing
by-name experiments in jacinta (`optional`/`array`/`map`) and the at-Focus `as`/
`decodableAtFocus` conversions were REVERTED to bare (quote rule). JsonSchema.decodable =
whole-instance seal + laundering local `field` helper. Also converted: guillotine
`Executable.apply()` Computable evidence `(…)^` (octogenarian `sh"which git"()`), and
octogenarian `GitRepo.addWorktree`/`Worktree` bare `Decodable in Text` evidence → `(…)^`.
Final tail: jacinta's `decodable` summonFrom TEXTUAL branch also sealed — it builds
`Json.Decodable(Morphology.Str)(provide[Tactic[JsonError]](…))`, whose lambda closes over
the provide-summoned tactic; under wisteria's bare field summon this rejected (embarcadero
Manifest's MediaType field). NB mill zinc staleness repeatedly produced false greens AND
false reds this session — `./mill clean <module>` on a suspect module, or the clean gate,
before believing any surprising result.

### superseded (2026-07-07 morning): 11/16508 targets failing, 42 errors — ALL the same shape

Full error texts persisted in `rep/_probe/remaining-errors.txt` (from the last full gate).
Every remaining error is the established consumer conversion: a summon of a now-capturing
codec instance against a BARE evidence/bound. Fix mechanically with the playbook (context
bound → `(using x: (…)^)`, or given result → `((…)^{tactic})` / `^` fresh where the SAM also
captures a raises-tactic; un-sugar `raises` to a named `using` when its tactic must appear in
a dependent result — see telekinesis.Receivable.apply). Remaining files:
- galilei `Handle.read`-style direct methods (bitumen tests' `.open(_.read[Data])`) — the
  Readable evidence on galilei's open/read methods.
- exoskeleton.completions ×8 (Xdg.dataDirs bound, workingDirectory.resolve, Pathname).
- jacinta.schema ×4 + jacinta_schema ×2 (more `field`-style bounds).
- octogenarian ×10 (its core Decodable-in-Text consumers), locomotion tests ×5 (Protobuf
  aggregable/read path), obligatory.GrpcFraming ×1, punctuation test ×1 (`as[List[…]]` chain),
  savagery/telekinesis/zeppelin/ziggurat tests (leaf summons in test code).
After green: (1) commit fork fix #5 as `feature/3.8.4/castbox` (implemented+corpus-clean in
the soundness-384 worktree, uncommitted), merge into the build branch, push; (2) the big
soundness commit series (Typeclass de-Pure; service capabilities; honest conversions; seals;
this log); (3) attest.

### Post-fix-5 convergence (2026-07-07, running log)

With fix #5 in, the wisteria `field`/`fieldInstance` capture-polymorphic edit was REVERTED
(it PRESERVED `^{any}` on every tactic-built summon, feeding fragile aka-Tagged/level shapes;
with the box asymmetry gone it was net-negative). chiaroscuro/spectacular tests and
embarcadero.oci/phoenicia cleared immediately. Since then, converted honestly: caduceus
Sendable.htmlDoc, archimedes+savagery Loadable (summon `(…)^` for inner Loadable),
graffiti streamable, stratiform.time twins, telekinesis Auth/Http.streamable/
Receivable/Servable/headers.selectDynamic evidence ×2, guillotine exec+await evidence,
eucalyptus test fixture; jacinta.time uses direct `json.root.long` (a `.as[Long]` resolution
mystery not worth chasing). GOTCHA: uncommitted build.mill flips were wiped a SECOND time by
probe classpath-capture traps (telekinesis.core, caduceus.resend re-flipped again) — COMMIT
build.mill flips promptly, or don't run captures with uncommitted flips.
REMAINING at last gate (fix mechanically): exoskeleton.completions (Xdg.dataDirs context
bound `[path: Instantiable across Paths from Text]` + workingDirectory.resolve + Pathname —
convert bounds to `(using (…)^)`); octogenarian tests (its core's Decodable-in-Text consumers,
same conversion); caesura tests + locomotion tests (derivation-facing primitive codec givens:
per Jon's fallback clause, RE-SEAL caesura.Dsv primitives+Spannable-path and
locomotion.Protobuf primitives — i.e. revert their `^{tactic}` results to bare + whole-instance
`unsafeAssumePure`, keeping the seals' comments; the honest form returns with the future
wisteria capture-polymorphism round); graffiti_test:254.

### ⚠ superseded in-flight handover (2026-07-06 late; fix #5 landed the next morning)

Gate: 15/16508 targets failing. The (B) conversion is ~90% complete but the final frontier is
the **wisteria-derivation chain**: derived products summon the now-capturing leaf codec givens
against BARE expected types. Attempted fix (wisteria.internal `field`/`fieldInstance` made
capture-polymorphic — currently IN the working tree): it clears the bare-refinement rejections
but wakes the **inliner-proxy/aka-Tagged class** — `Tagged[(any$proxy7 : TC{…}^{any})]` vs
`Tagged[(any$proxy7 : …)^{any$proxy7}]` in ulysses.core (REGRESSION on an enabled core),
chiaroscuro.test, spectacular.test — the `aka` context params over capturing singleton proxies
need the self-capture the inliner doesn't add. This is the surveillance-$proxy compiler class;
likely fork fix #5 territory (singleton proxy types of capturing instances should self-capture)
or a denominative/aka-machinery change.

NEXT-SESSION OPTIONS (decide, then finish):
(i) Fork fix #5 for the proxy self-capture, keeping everything honest; or
(ii) interim: REVERT the wisteria field/fieldInstance edits AND revert the leaf codec-given
    `^{tactic}` conversions in the families wisteria summons for currently-ENABLED test suites
    (caesura.Dsv primitives + Spannable path, serpentine Path.decodable, locomotion Protobuf
    primitives, octogenarian decoders, guillotine Computable, eucalyptus test fixture) back to
    SEALS, restoring gate-green with the rest of (B) intact; then do (i) at leisure.
ALSO OUTSTANDING: jacinta.Json:1194 discriminate rewrite has a type error (`.root.string`
already yields Text — drop the `.tt`); caesura_test:184 unknown; ulysses only red under the
wisteria edit. NOTHING from today's post-attest work is committed — the last attested commit
is `ebed8a998` (101/127 suites green). The working tree carries: service capabilities round
(committed already: caduceus/telekinesis flips are in build.mill? NO — the caduceus.resend +
telekinesis.core build.mill flips are ALSO uncommitted), Typeclass de-Pure, 35 combinator
annotations, all (B) conversions and seals, the wisteria edit, and this log.

STATE at the earlier sub-checkpoint (gate then: 11 of 16508 failing, all classified): the (B) conversion
covers anticipation, turbulence, distillate, ambience, urticose(+url), serpentine, galilei,
guillotine, hellenism(+jvm), nomenclature, geodesy, gesticulate, revolution, xylophone,
octogenarian, jacinta, breviloquence, caesura, stratiform, locomotion, chiaroscuro,
austronesian, telekinesis, caduceus, coaxial, obligatory + the earlier service capabilities.
REMAINING FRONTIER (next round): **wisteria-minted bare expected types** — the derivation
macros summon field codecs against pure refinements (`(Decodable in Dsv){type Self = Text}`),
rejecting capturing instances; fix in wisteria's inline machinery (`summonInline[(tc)^`-style)
— this is also the previously-catalogued wisteria-anchor-given class. Plus a tail of
unconverted consumers surfaced by it: octogenarian test path summons, guillotine
`Computable.dataStream` (throwUnsafely-instantiated), eucalyptus test Writable fixture,
jacinta/breviloquence `discriminate` (safely-scoped summon vs invariant Self bound).

## Given-captures round 1 (2026-07-06, post-ruling): caduceus.resend + telekinesis.core GREEN

Applying Jon's capability ruling. THE KEY STRUCTURAL SPLIT discovered immediately:
- **Service traits** (not `Typeclass`-derived) take the capability treatment cleanly:
  `Courier` and `HttpClient` now extend `caps.ExclusiveCapability` (instances retain
  tactics/backends/clients); `Orchestrate` likewise (retains the caller's render function).
  caduceus.resend GREEN (flipped); telekinesis.core GREEN (flipped) — the biggest dependency
  gate in the residual set.
- **`Typeclass`-family instances CANNOT be capabilities today**: `prepositional.Typeclass
  extends Findable, Pure` — making `Postable` a capability produced "Postable is a pure type,
  it makes no sense to add a capture set" plus "object Postable needs to extend Capability"
  (pure instance vals stored in the companion get implied `any`). RESOLVED (Jon, 2026-07-06):
  "`Typeclass` should not extend `Pure` because in general, it's not `Pure`" — the marker is
  dropped; instances' captures are tracked, not asserted away. The old comment warned that
  `contramap`-style combinators relied on the purity assertion; whatever the full gate
  surfaces from that is this round's worklist. The launder seals (jacinta/caesura/stratiform/
  Postable) remain in place for now — with Pure gone they can migrate to honestly-capturing
  given result types incrementally (each needs its consumers' context bounds converted to
  explicit capturing evidence, the coaxial `transmit` lesson).
- **coaxial `Serviceable`** given was ALREADY declared capturing (`(DomainSocket is
  Serviceable)^`); the blocker was the CONSUMER side — `transmit`/`exchange`'s context bound
  demanded a pure instance. Context bounds cannot express capturing evidence: replaced with an
  explicit `(using serviceable: (endpoint is Serviceable)^)` param (the locomotion lesson).
- **`getOrElse` under CC** (telekinesis.internal macro): the `B >: A` bound infers against the
  capture-decorated summon result and fails (E057); a plain `match` avoids the inference.
  General macro-tolerance lesson alongside the annotation-strips.
- cordillera.core: the AsyncTactic given-alias error has evolved away; its remaining first
  error is parasite's pure `?->{}` daemon-block arrow — gated on ⚑3 (daemon semantics), NOT
  on given-captures. Same for obligatory.test.

## Foci/tracks cluster round (2026-07-06): caesura GREEN; cluster dissolved into other classes

The cluster's real blocker: a `raises … tracks …` FUNCTION VALUE cannot be typed under CC (its
honest type is a curried DEPENDENT context function — "Implementation restriction: … not yet
supported"), so test helpers taking `decode: X => result raises E tracks F` params can never
accept the `_.as[R]` lambdas. WORKING PATTERN (caesura template):
1. Helpers become `inline def` with `inline decode`, so the lambda beta-reduces into
   `protect`'s inline position and no function value with the impossible type ever exists.
2. The transparent `validate` sugar types as `Any` inside an inline body ("value protect is
   not a member of Any" — transparent macros are not expanded while typing an enclosing inline
   def), so the helper constructs `Validate[Accrual, [r] =>> r raises E, Focus](initial,
   handler)` DIRECTLY — `validateWithin`'s tree extraction accepts the explicit construction,
   and the aka-named `prior`/`accrual` context accessors still resolve.
3. The derived decoder factory needs the jacinta-style seal: caesura's `DsvProductDecoder`
   ctor keeps its pure `Dsv -> derivation` lambda type and the CONSTRUCTION SITE launders the
   (tactic-capturing) derived lambda via `unsafeAssumePure` (laundering an impure CTOR PARAM
   inside the class does NOT work — the class still retains the param: "Reference …this.lambda
   is not included in {} of the self type"). Same seal applied to stratiform's
   `Tel.{Encodable,Decodable}.apply` shape thunks (stratiform.Tel2's `-> (inner is …)` given
   slots are the same latent trap as jacinta's and can be converted when needed).
Local case classes must stay OUTSIDE the inline helpers (method-local types in inline defs).
Results: **caesura GREEN (flipped)**; jacinta 34→16 errors and ypsiloid 10 — both now blocked
on a NEW named class: **wisteria-anchor-given capture** (`Reference contextual$N is not
included in the allowed capture set 's1 of the enclosing given instance wisteria$1` — the
anchor givens minted by wisteria's derivation reuse capture the consumer's tactic); stratiform
3 errors (enum-union intersection — austronesian's class — and the panopticon lens single);
xylophone 4 (x"" interpolator Text box + others). The `tracks` design question (⚑8) remains
for library-USER ergonomics, but tests no longer block on it.

## Compiler fix #2: `feature/3.8.4/splicealias` (2026-07-05) — anon-instance cluster SOLVED

Root cause of the "anon-instance fresh-var" class (mis-labelled earlier — the capture var was
incidental): when a pickled quote with type holes is unpickled, `PickledQuotes.spliceTypes`
substitutes hole-binder occurrences via a TreeTypeMap, but a symbol whose ClassInfo is unchanged
by the map — an anonymous class whose DECLARATIONS (not parents) mention a binder — is never
copied, so its members keep referring to the binder, whose pickled placeholder info is
`TypeAlias(Any)`. Plain compilation never re-reads those members; capture checking's recheck does
→ `type Self` dealias to `Any` → refinement-member conformance failure + "object creation
impossible". FIX (fork branch `feature/3.8.4/splicealias`, 13 lines): set each binder's own info
to its spliced type (stripping CC's `@caps.internal.inferred` wrappers, which would otherwise
fail the member's declared bounds in override checking) so survivors dealias correctly.
Verified: adversaria, vexillology, vicarious green under CC. TWO follow-ups from the full gate:
- The unscoped fix REGRESSED non-CC jacinta.test (`record.sub.date`): the polyvinyl record macro
  OBSERVES the binder alias through TypeRepr inspection and depends on the `Any` placeholder.
  The fix is therefore gated on `Feature.ccEnabledSomewhere` — non-CC compilation is preserved
  bit-for-bit; only capture-checked runs get the corrected dealiasing. (A future deeper fix would
  address the surviving references themselves — the ClassInfo-decls gap in ReplaceSplicedTyped.)
- wisteria.test stays DEFERRED: with the Specific box gone, its remaining error is pure
  tactic-capture (`Readable[Wrapped]^{given_Tactic_VariantError, wisteria.Tests}` vs the pure
  summon type; even an explicit `Readable[Wrapped]^` ascription is rejected — the required
  capture set prints as `{}`). Same family as the jacinta-derivation cluster; batch with it.
NOT this bug: surveillance.core (`$proxy` = Inliner term proxies, separate), sedentary.core
(`fqcn""` interpolation box). Build branch is now `feature/3.8.4/make-unboxedpure-splicealias`.
NB the wisteria false-green also re-proved: a module-scoped clean compile batch is NOT a valid
signal even for a single suite — only the full gate, then attest.

### Does Soundness compile BETTER on 3.9.0 / 3.10.0? — NO (2026-07-06)

Experiment: throwaway worktree (`/tmp/soundness-ccprobe`, detached at `212bde060`),
`SOUNDNESS_SCALA_HOME` pointed at the `all/3.9.0` and `all/3.10.0` release builds (the repo view
relabels the jars as `3.8.4-propensive`; provenance verified via classpath paths + TASTy minor
version), FULL `./mill clean` before every sweep, gate = `mill --keep-going test.assembly`
(16,497 targets). Baseline: on the 3.8.4 fork row this HEAD is entirely green (attested).

**VERDICT: neither newer row can build Soundness today; 3.8.4+fork remains the only viable
toolchain. The blocker is one shared upstream regression, not capture-checking quality.**

**★ THE shared hard blocker (upstream regression, absent in 3.8.4, present in 3.9.0-RC1 AND
current main):** compiling `gossamer.core` under CC crashes with
`AssertionError: denotation trait SortedSetFactoryDefaults invalid in run 2`
(`SingleDenotation.updateValidity` ← `bringForward`), while rechecking
`new mutable.BitSet(...)` in `gossamer.proximities`. Reproduces with a SINGLE direct dotc
invocation (not a Zinc artifact): the run advances to "run 2" via the compiler's own
macro-suspension, and CC's recheck then brings a stdlib TASTy denotation forward across runs
into the validity assertion. gossamer sits under everything (wisteria → spectacular → gossamer →
world), so both rows prune there. Candidate fork fix #3 / upstream report; repro recipe = compile
gossamer.core sources directly against a compiled classpath with the module's flags (sans
`-Wconf`, whose grammar 3.10 also rejects).

Per-row detail:
- **3.9.0 (`all/3.9.0`):** 49 modules green (incl. contingency.core with the full Unscoped/raises
  machinery, wisteria.core, adversaria, kaleidoscope, panopticon, polyvinyl cores) before the
  gossamer prune. Also red: larceny.plugin (3.8.4→3.9 compiler-API drift, `Constant.apply`).
  CC engine unchanged w.r.t. our fixes: case-2 repros RED on stock 3.9.0-RC1, GREEN on `all/3.9.0`
  — both fork fixes still required. Zero CC gains. Strictly worse than 3.8.4+fork.
- **3.10.0 (`all/main`):** the more interesting row. splicealias fixed natively (#26307); after
  experiment-local migration patches (below) 53 modules green incl. contingency.core AND
  wisteria.core; the graph then still dies at gossamer (same crash) plus a SECOND upstream
  compiler crash: adversaria.core hits `AssertionError` in `Inlines.inlineCallTrace`
  (`assert(ctx.source == pos.source)`, Inlines.scala:344) via `PickleQuotes.pickleAsTasty` —
  quote pickling of inline call traces with cross-file positions.
  Migration cost measured (experiment-local patches in the /tmp worktree, NOT on the branch):
  * 56 anonymous `using erased T` params + 12 anonymous `(erased T)` context-fn params must be
    named ("Erased method parameter must be named") — mechanical, 3.8.4-compatible.
  * proscenium must export `` Predef.`->` `` (3.10 demoted `ArrowAssoc` to a non-implicit and
    moved `->` to an inline extension).
  * NEW CC "Implementation restriction: polymorphic function types cannot wrap impure function
    types": 13 sites in wisteria Product/SumDerivation + vicarious. Fix that worked: make the
    FIRST value arrow after the poly binder pure (`typeclass[field] -> (…) ?=> result`) — inner
    context-fn impurity stays; all sites then compile, including the `aka`-Tagged DSL.
    (Beware the split-layout sites where the poly arrow and value arrow are on separate lines.)
  * larceny: `source.substring` is `String | Null` and 3.10's typed `Constant.apply` overloads
    reject it (3.8.4 took `Any`) — `.nn`; plus the same `-Wconf` grammar break at build level.
  * Deprecations (warnings only): `strictEqualityPatternMatching` now standard; `into` needs
    `-preview`.
- "More capture checking success"? NO new greens on either row: everything that compiled was
  already green on 3.8.4+fork. The residual CC classes (handler-tactic existential, Foci, etc.)
  remain UNTESTED on 3.10 — their modules sit above gossamer. Retest once the denotation crash
  has a fix; 3.10's CC engine is otherwise plausible (it processed the whole reachable graph
  without CC-specific failures beyond the poly-fn restriction).

### splicealias across compiler versions + fork branch conventions (2026-07-06)

Per-version necessity, established with a NEW self-contained calibrated repro
(`rep/splicealias-repro/` — macro splices a type hole into an anon class whose *decls* mention
the binder; `check.sh <scalac>` compiles the macro pass then the CC use pass; first synthetic
shape calibrated immediately, reproducing the exact adversaria signature
`Found: Object with repro.TC {...}^'s1, Required: repro.TC{type Self = String}`):

| row | stock | verdict |
|-----|-------|---------|
| 3.8.4 | RED | needed — `feature/3.8.4/splicealias` (existing) |
| 3.9.0-RC1 | RED (`PickledQuotes.scala` byte-identical to 3.8.4's) | needed — ported as `feature/3.9.0-RC1/splicealias` (clean cherry-pick of `ef98f60a79`) |
| main | — | NOT needed: upstream fixed the root cause 2026-06-25 — issue #25245, PR #26307 (`6d557cf587`): the ClassInfo case clones the decls scope whenever a decl references a spliced type, so `mapSymbols` copies the class and remaps member infos. This is exactly the "deeper fix" anticipated above, and it landed after the 3.9.0-RC1 cut. Consequence: no upstream PR for splicealias is warranted; the 3.8.4/3.9.0 branches are backports of a problem upstream has since fixed differently. |

Branch conventions settled with Jon: independent fixes (`unboxedpure`, `splicealias`) live as
single-feature branches directly off the version base and are combined ON-DEMAND by merges
(`feature/<ver>/make-…` hyphenated names for selective combos); `make`/`wasm`/`witcall` remain a
necessarily-stacked linear chain. NEW: per-version everything branches in a separate `all/`
namespace — `all/3.8.4`, `all/3.9.0-RC1`, `all/main` = the `make-wasm-witcall` stack tip + a
merge per independent feature (splicealias omitted on main per the verdict above). All merges
were clean; all three distributions build (`make`) and pass the repro matrix:
stock 3.8.4 RED / `all/3.8.4` GREEN; stock 3.9.0-RC1 RED / `all/3.9.0-RC1` GREEN; `all/main`
GREEN without splicealias. Branches pushed to propensive/scala.
NOTE (not acted on): the soundness build worktree stays on
`feature/3.8.4/make-unboxedpure-splicealias` — switching to `all/3.8.4` would swap the Scala.js
stdlib to the scala-wasm 1.22 runtime and add the wasm backend, requiring its own full gate +
re-attest. Also flagged: the 3.10.0 row's `make` Makefile bakes a stale `VERSION :=
3.9.0-RC1-propensive` (the `git.hash` in compiler.properties confirms the right tree; only the
string is wrong), and 3.8.4's unboxedpure commit still carries the stale `project/Build.scala`
pin `dottyVersion = 3.8.4-cc1` (inert under the Makefile build).

RENAMED (2026-07-06, same day): version rows `3.9.0-RC1` → `3.9.0` and `main` → `3.10.0`
(upstream main's `developedVersion` is 3.10.0), locally and on propensive/scala (old names
deleted from origin; worktree HEADs followed automatically). References above predate the
rename. 3.9.0-final prep is tracked via local branch `release-3.9.0` → `upstream/release-3.9.0`;
at rename time its HEAD == the 3.9.0-RC1 tag (zero prep commits), so the 3.9.0 row needed no
rebase — when upstream's branch moves, rebase the row's feature branches onto it and
reconstruct `all/3.9.0`.

## Audited branch changes — kept

- **17 `unsafeAssumePure` sites** (parasite ×13, ambience.Property:115, gossamer.Builder:53,
  ultimatum.Panes:59): ALL carry safety-argument comments already; compliant with the 2026-07-04
  policy. Worth re-probing after new CC work whether any become removable (esp. parasite's
  worker-registry pair, the known reach-capability hard case).
- **galilei Handle** (`86406d2f2`): dropped `is Streamable`/`is Writable` givens for direct
  `write`/`stream`/`read` methods. KEPT — the commit documents the genuine CC constraint (given
  resolution widens a scoped capability's capture to `{any}`), and the direct methods preserve
  function; consumers migrated. This is coherent with CC's goal: a scoped `Handle` must not escape
  through a widened given.
- **Calendar.jdn restructure** (`66f80a818`): under an *accruing* tactic the invalid-date path now
  returns `computeJdn(invalid)` instead of the old per-calendar sentinel. KEPT — the old sentinels
  were arbitrary and mutually inconsistent (`Date(2000,1,1)` here, `julianDay(0)` there); the new
  behaviour is uniform, deterministic, and arguably more sensible (Gregorian March 32 → April 1).
- **Pojo codec `provide` seal** (`c09ccbe35`): mirrors the established jacinta/stratiform/ypsiloid
  pattern. KEPT pending any test evidence to the contrary.

## Audited branch changes — flagged for Jon

- **parasite daemon error propagation** (`ad3511b34`, whose message wrongly claims "no source
  changes"): daemon-raised errors now escalate to `Fault` instead of an enclosing `handle` block;
  the tests "An error uncovered by an inner handler is handled by the outer handler" and "A case
  may re-emit a different error to the enclosing handler" were DELETED, and "A handler handles a
  daemon's emitted error" now asserts the inverse. This matches the known open design question
  (boundary-based Emit capture is CC-rejected), but it is a user-visible semantic change that
  should be either blessed or redesigned — not silently shipped.
  Also `given monad: (Monitor^, Probate^) => Monad[Task]` dropped its `Tactic[AsyncError]`
  requirement (whole instance laundered pure). Commented, but the dropped error-tracking on
  `bind`/`point`/`map` deserves explicit sign-off.

## Deferred-suite triage under `3.8.4-cc1` (2026-07-04, full-clean `mill -k test.assembly`)

Enabled 35 case-2-class suites; **6 green** (kept enabled): abacist (was "multi-blocker"!),
anticipation, chiaroscuro, mosquito, probably, spectacular. 29 red (toggles reverted), by class:

- **Anon-instance fresh-var** (adversaria, vexillology, vicarious, wisteria²⁄₂, aviation¹):
  `Found: Object with Trait {...}^'s1 / Required: Trait{pure refinement}` at an inline-given
  summon of a macro-built instance. NOT fixed by: the compiler patch, `extends caps.Pure` on the
  trait (contextual.Interpolation already has it and still boxes), macro-side ascription,
  macro-side `asInstanceOf`, or `transparent inline given` (all tried on adversaria — the Found
  type never changes, so the var attaches at expansion, not to the RHS). Does NOT reduce
  synthetically (a minimal trait+inline-given+anon-instance compiles clean).
  **ROOT CAUSE (direct dotc + -explain, 2026-07-04): not boxing at all.** The `^'s1` var is empty
  and irrelevant; the failing subtype step is
  `hasMatchingMember(anon.Self, [= Example1.type]) → evidence$1$11 <: Example1.type → Any <:
  Example1.type = false`: under CC's re-check of the (pickled-quote) macro expansion, the inline
  type-parameter proxy `evidence$1$11 := Example1.type` LOSES its alias and widens to `Any`, so
  the anon class's `type Self = entity` member no longer matches, with a follow-on "object
  creation impossible (2 unimplemented members)". Inliner/pickled-quotes proxy handling under CC
  (the proxy-type problem the #16978 thread flags), not a capture-set guard — a substantially
  bigger compiler fix than the two deployed guards. Deferred to its own workstream.
  Repro: `java -cp <3.8.4-cc1> dotty.tools.dotc.Main -classpath <adversaria.test cp> <its opts>
  -Ycc-new -language:experimental.captureChecking -explain lib/adversaria/src/test/adversaria_test.scala`.
- **Foci codec-helper** (caesura, jacinta, stratiform, xylophone, ypsiloid): `validateX(x)(_.as[R])`
  — the lambda's inferred `Foci[Focus^'s2]^'s3 ?->'s4 Tactic^'s5 ?->{x$0} R^'s6` won't unify with
  the declared `raises … tracks …` context-function type. The long-standing uncracked shape;
  unchanged by the compiler patch.
- **capturing-raises / ThrowTactic^{any} → raises ^{any²}** (telekinesis, scintillate, ethereal,
  profanity, apoplexy + the known 7): now clearly the dominant remaining class. Phase R
  (contingency encoding) or compiler work.
- **safely/OptionalTactic Label fresh-vars** (exoskeleton, obligatory): related to the above.
- **Singles**: serpentine (`toward` macro Text^'s1), galilei (Path Topic refinement), distillate
  (`As[Int]` extractor `Text^{}` — empty box INSIDE an inferred unapply type, not an explicit
  tpt, so the patch's strip doesn't reach it), panopticon (Optic `->{fresh}` varargs), coaxial
  (**real** escape finding: `port.listen` result leaks `any` into `val server` — possibly a
  genuine API smell worth a scoped redesign), parasite (AtomicReference boxed union), austronesian
  (`unsafely` nested context-fn), enigmatic (BlockCipherPadding given `^{any}`), baroque
  (Multiplicable `?1.Result`), breviloquence (Cbor Ast union), honeycomb (h"" Text^'s1),
  superlunary (dispatch fresh), aviation (Interpolation box + Tzdb raises tail).

Kept (unverified-but-harmless prep): wisteria_test's non-capturing `uncheckedErrors` tactic +
`provide[Tactic[VariantError]]` seal in the Readable fixture — the documented fix for the
derivation-capture half of wisteria's errors; the suite stays deferred on the Specific box.

## Attest-caught runtime regression: chiaroscuro labels under CC (fixed)

Enabling chiaroscuro.test compiled clean but FAILED 3 tests at runtime under attest — compile-green
≠ behaviour-preserved, again. Root cause (found by instrumenting `rudiments.internal.name`): under
capture checking, inferred types reaching an inline call site arrive as
`Double @scala.caps.internal.inferred`; `typeName`'s `TypeRepr.show` rendered the annotation, and
chiaroscuro's `rewrite` regex then extracted the LAST dotted segment — literally "inferred" — as
the display label (`Primitive(inferred, …)` instead of `Primitive(Double, …)`). The decomposition
VALUES and implicit branch selection were correct throughout; only the rendered name broke.
FIX (macro-tolerance, honest): `rudiments.internal.name` now strips any `scala.caps.*` annotation
(recursing into type arguments) before `.show`. All 12 chiaroscuro tests pass under CC.
LESSON for the taxonomy: `@scala.caps.internal.inferred` wrappers reach macros via inline type
parameters in EVERY CC-enabled module — any macro that pattern-matches or renders `TypeRepr`s
may need the same stripping (candidates: quantitative `checkable` MatchError, cataclysm
`Attribution` "must be a string literal" — the macro-under-cc class).

## Frontier-core enablement round (2026-07-04)

Attempted the six parasite-independent cores. **Enabled: imperial.core.**
- quantitative.core compiled under CC (fixes: `->` conversion params + `trait protointernal
  extends caps.Pure`) but was REVERTED to non-CC: the full gate showed the NON-CC
  `quantitative.test` breaking against the CC-compiled core — `0.5*Second/Metre` left an
  unreduced `?1.Result` (CanEqual failure). **New constraint discovered: a module whose
  TRANSPARENT macro givens yield refined types (divTypeclass etc.) pickles quotes under CC whose
  refinements no longer reduce in non-CC dependents.** Such macro-defining cores must be enabled
  only together with (or after) their dependents, or after a compiler-side fix. The
  `protointernal` purity marker is kept (compiles without CC; forward prep); the `->` params were
  reverted (`->` is CC-only syntax). The predicted `checkable` macro crash did NOT occur at core
  level (test-level concern).
- imperial.core: `case class BaseLayout extends caps.Pure` (pure path data; also keeps nested
  layouts' `Topic` tuple members like `Home.type` pure).
Deferred with findings:
- honeycomb.core: deep macro-under-cc (22 errors, several shapes: wildcard-summon `?N.Form^'s`
  refinements, quote patterns expecting `Expr[value^{}]^{}`, `$proxy` intersections). Direct
  Expr binding didn't reduce it. Needs its own session.
- aviation.core: 43 errors — the known Tzdb non-inlinable `raises` tail + `As[Base24]`
  extractor (distillate class).
- telekinesis.core (9) + cataclysm.core (3): a NEW named class — **given-captures-context-param**:
  `given x: Tactic[E] => X = …` instances capture their tactic (`Found: x{val x$1…}^{x$1}`,
  "cannot flow into {}"), plus cataclysm's `String.format` Java-varargs box (`Int` vs
  `(Object|Null)^'s1`). The given class is an API-design question (such givens ARE capabilities —
  declare them capturing, or restructure) tied to the Emit/Tactic capability model; batch with
  Phase R.

## Straggler-core sweep (2026-07-04, round 4)

**METHOD LESSON: a module-list clean compile (`mill clean && mill --keep-going <27 targets>`) is a
FALSE-GREEN signal** — 27/28 passed in isolation, but the full `test.assembly` gate re-ran them
with 14 real failures. Only the full-tree gate (then attest) counts.

Green (kept): **burdock.boot, stratiform.time** (+ imperial.core from round 3).
Red (reverted, first-error classes):
- probably.cli: `given testable: Testable = this` — `Suite^{any}` into pure `Testable` (the
  suite is genuinely capability-ish; needs a capturing `Testable^` declaration or design call).
- punctuation.core: synthesized union-literal tree (`H6` heading level `Int` vs `1|2|…|6`).
- surveillance.core: path-dependent `evidence$N$proxy1.Self` mismatch (inline proxy class again).
- jacinta.records: `class JsonBlueprint` is judged a capability (its `Intensional` field types
  carry captures) → object-capability wall.
- anthology.bundle: capturing-raises (`Tactic[ZipError]^ ?=>` existential).
- scintillate.server (gates the whole JVM stack), jacinta.schema, obligatory.grpc, profanity.core,
  archimedes.core, savagery.core, stratiform.binary/records, tarantula.core: not yet classified
  individually (mixed raises/capability shapes) — next round's worklist.
Skipped (downstream of reds, retried in round 5): apoplexy.core, caduceus.resend,
embarcadero.containerd, ethereal.core, exegesis.core, orthodoxy.core, obligatory.json,
perihelion.core, punctuation.ansi, sedentary.core, synesthesia.core.
zeppelin.core: deferred — `Zip.Entry` genuinely captures its lazy `storedBytes` thunk (the
LazyList-confinement limit); a real design case, not a spurious box.

## Straggler convergence rounds 5-8 (2026-07-04) — 10 modules enabled

**Enabled + gate-green: burdock.boot, stratiform.time, stratiform.records, obligatory.json,
perihelion.core, punctuation.ansi, archimedes.core, savagery.core, caduceus.resend,
orthodoxy.core** (full-clean `test.assembly` 16177/16177 SUCCESS).
`->` pure-function fixes: archimedes `QuantityEncodable.lambda`, savagery `Transformable(get, put)`,
stratiform `TelBlueprint.intensional`/`record`, and `polyvinyl.Specification.build`'s abstract
`transform` param (CC overrider stratiform matches with `->`; non-CC jacinta.records overrides
with `=>`, which non-CC compilation accepts).
Deferred with diagnosis:
- **jacinta-derivation cluster** (apoplexy.core, synesthesia.core, exegesis.core, jacinta.schema
  + cataclysm²): `Json.DecodableDerivation.derived`'s Morphology/field-decoder by-name thunks
  capture the consumer given's `Tactic[JsonError]` param (primitive field decoders are
  tactic-taking givens), and `Json.Decodable.apply(shape0: -> Morphology)` is (rightly) pure.
  ONE fix in jacinta's derivation/factory design would clear the whole cluster.
- **capturing-raises additions**: ethereal.core, embarcadero.containerd, anthology.bundle,
  scintillate.server (Loggable ?=> variants) — Phase R.
- tarantula.core: `Navigator.this` captured by nested `Server` (browser session is genuinely
  capability-like — design). sedentary.core: `fqcn""` Interpolation box (anon-instance class).
  jacinta.records: object-capability wall (Intensional fields). probably.cli: `Suite^{any}` into
  pure `Testable`. punctuation.core: synthesized union-literal tree. surveillance.core:
  `evidence$N$proxy1.Self` inline-proxy mismatch. zeppelin.core: genuine lazy-thunk capture.
- GOTCHA repeated twice now: `->` is CC-only syntax — reverting a module's toggle must also
  revert its `->` edits (quantitative, tarantula).

## larceny + vacuous suites: confirmed runtime-incompatible with CC (2026-07-04)

Both compile clean under CC but vacuous FAILS 3 tests at runtime ("Abstract type not proven
distinct…"): they are larceny `demilitarize` compile-error assertions, and with CC flags on the
test module the probed compilations legitimately produce different diagnostics. Not forced;
both stay deferred (as the old characterization predicted). Revisit only if the larceny plugin
grows CC awareness.

## Fixes applied in this pass

- **jacinta `divisible2`** (`jacinta.JsonPointer.scala:118`) and **ypsiloid `divisible2`**
  (`ypsiloid.YamlPath.scala:125`): the `Divisible:` SAM-factory form minted `Ordinal^'s1` under CC
  (jacinta was the attest-blocking red; ypsiloid only surfaced on a forced recompile — the CC
  multi-pass false-green effect). Replaced with explicit `new Divisible { … }` anonymous classes —
  compile under CC even on STOCK 3.8.4 and keep Scala.js linking intact (no `= lam` JS debt).
  These were the only two `by Ordinal` factory sites; the `by Text` twins don't box. NB the
  patched compiler does NOT fix this shape (fresh var on a pure opaque, not an empty set/tuple).
- **xenophile.wasm `enc`/`dec`** (`xenophile.Wasm.scala:52,61`): codec-builder lambda params
  changed `=>` → `->` (pure function). All 22 call sites pass pure accessors; a capturing lambda
  would make the codec instance itself a capability, which `Encodable`'s pure self type forbids.
  Surfaced only under `3.8.4-cc1` in the full-clean gate.
- **Scala.js under `3.8.4-cc1`**: `org.scala-js:scalajs-scalalib_2.13:3.8.4-cc1` does not exist
  upstream; republished the identical 3.8.4 jar under that version in `~/.ivy2/local` (hand-written
  ivy.xml). The payload is compiler-independent (sjsir of the 2.13 stdlib).
- **`.js` cross-modules × CC — pre-existing, deferred**: the `.js` modules inherit the JVM
  scalacOptions, so CC-enabled modules' js twins are capture-checked too — and they have NEVER been
  built on this branch (#1453 verified them on main, which has no CC; attest gates JVM only).
  Verified pre-existing on STOCK 3.8.4 (not caused by the compiler patch): contingency.core.js
  fails at `contingency.Fatal.scala:40` (`contramap`'s SAM instance captures `lambda` against
  Fatal's empty self capture set); contextual.core.js and panopticon.core.js also red. JVM twins
  of the same sources compile — the divergence is unexplained (plugin/tree differences?).
  Treat `jsAll` × CC as its own later workstream.


## Slicing: verification determinism and the platform-split transition (2026-07-08)

`commit.gpgsign=true` meant every re-slice re-signed commits, changing all SHAs despite
fixed commit dates — verification resume markers never matched. The verification series is
now sliced unsigned (deterministic SHAs); the final reviewed series is re-signed, with
equivalence proven by per-position tree-hash equality (trees exclude signatures).

The platform-split commit (serpentine+galilei) is a two-sided API transition: it moves the
OS platform types AND changes `Radical#length`/`decode` to `raises PathError`, so direct
path-API consumers (hellenism, urticose, zeppelin, octogenarian, bitumen, exoskeleton,
ziggurat, ethereal) fail in both directions across it (verified by hybrid probes: old-vs-new
at prefixes 32/64/102, new-vs-old in a final-tree-with-old-split build). They convert
atomically in the split commit. One-sided consumers (mandible, punctuation — plus profanity,
whose test deps on exoskeleton.rig) convert in their own commits ordered after it.
nomenclature's test is gated from its conversion until the split lands (its tests use
hellenism's capturing `cp""` interpolator).

The same analysis applies to zephyrine: its conversion removes `PositionTracking` and gives
`Cursor` a second (capture) type parameter, and Scala has no default type arguments, so all
Cursor-naming consumers (jacinta, honeycomb, xylophone, ypsiloid, cataclysm, stratiform,
telekinesis, scintillate, gesticulate, perihelion, obligatory) convert atomically with it.
Three unit-level cycles were broken with hybrid-build evidence (each 16508/16508 green):
harlequin converts after the fold (its md module needs new honeycomb; converted obligatory
tolerates old hyperbole), legerdemain converts after the fold (converted telekinesis
tolerates old legerdemain), and harlequin tolerates old punctuation/hellenism.

Every prefix of the final 107-commit series was verified by a full `test.assembly` build
(clean-retry on zinc cross-batch artifacts), split across two worktrees.

## Separation checking for the streaming kernel: Phase 0 characterization (2026-07-11)

Plan: apply `language.experimental.separationChecking` to the mutable streaming fast paths
(zephyrine windows/Conduit hand-off/Cursor, then turbulence and the HTTP stack), replacing
the convention-only single-owner discipline currently gated by `Unsafe`. Full plan in
`~/.claude/plans/now-that-we-have-expressive-summit.md`; probe suite + findings table in
`rep/sepcheck-probes/README.md`. All 20 probes green on BOTH rows (3.9.0-RC1-propensive and
3.10.0-propensive, single-shot fork scalac).

Key decisions established by the probes:

1. **Scoped CPS borrows are the window mechanism.** `reading[T](lambda: (storage, start,
   limit) ->{caps.any, this.rd} T)` rejects both refill-during-borrow and storage escape.
   First-class windows stay temporal (unexpressible) — raw accessors that remain keep
   `Unsafe`.
2. **Kernel stage types re-parent to `ExclusiveCapability, Stateful` — NOT `Mutable`.**
   Mutable implies the Unscoped classifier, and an Unscoped capability cannot capture
   non-Unscoped things (Cursor's `load` thunk, any stage wrapping an upstream). The
   exclusive/read-only discipline is identical without Unscoped.
3. **`Addressable.Storage` stays untracked.** The built-in Array-as-Mutable treatment does
   not compose through an abstract type member (bare abstract param = pure; fresh results
   don't flow; exclusive abstract fields can't be reassigned). Safety attaches to the
   STAGE capability; generic storage primitives keep their current typing. Concrete-array
   code (compaction, ensureCapacity-style swap, freeze) is fully supported.
4. **The read-only cascade is CC-level**: consumers compiled without the sepcheck import
   still get bare-ref = read-only against re-parented kernel types. Re-parenting is
   therefore a repo-visible API change (mechanical `^` sweep), gated behind the plan's
   decision gate; `consume`/hidden-set/freeze diagnostics remain per-unit.
5. The 3.9 row needs no SepCheck cherry-pick so far (`562b513a4e` absent but the anonymous
   factory shape is green). GOTCHA for kernel authors: declare normal members before
   `consume` methods in a template (later members are "hidden by" the consume result —
   upstream-reportable, like the abstract-Storage opacity).

## Sepcheck Phase 1/2 outcome: enforcement needs re-parenting; single-copy loader shipped (2026-07-11)

Two further probes killed Phase 1 as scoped (file-local `consume` in Conduit):

- `consume` on an UNTRACKED abstract-Storage param is VACUOUS — use-after-consume compiles
  clean. Combined with the P5 finding (Storage can't be tracked through the abstract
  member), a Conduit-internal consume helper would be safety theater; not shipped.
- A tracked carrier (concrete `Slab extends Mutable` field) fails differently
  (`p9-field-move.neg`, both rows): a tracked capability cannot be MOVED out of a field —
  the field read widens to a fresh `any` hiding the enclosing instance, rejecting the
  re-mint and everything after. No take/replace primitive exists. And the update-override
  rule chains commit/flush→publish→update all the way into Intake's public methods, so
  enforcement anywhere in Conduit requires the Stateful re-parenting of Intake itself.

CONCLUSION: separation checking's enforceable value for the kernel starts at Phase 3/4
re-parenting (exclusive/read-only stage discipline, consume factories/combinators, borrows,
freeze) and the hand-off/field-swap points keep one audited Unsafe site each even then.
Phases 1–2 as originally scoped deliver no checking; dropped in favour of presenting the
decision gate with this evidence.

Independently shipped (no sepcheck dependency): the stream→cursor single-copy refill.
`Cursor.Filler` (direct fill into the cursor buffer) replaces the materialize-then-copyChunk
loader in the stream-backed factory; the window is read, transferred and skipped within one
fill — the borrow discipline applied manually at the one place a cursor touches a window.
zephyrine suite 233/233; new benchmark `Cursor[Data].next over Stream, 100 × 100-byte
blocks`: 12.124 µs → 9.397 µs per 10 KB (82.5k → 106.4k op/s, ~29% more throughput).

## Sepcheck Phase 3: Cursor is now a stateful capability; fork fix #11; the parser wall (2026-07-11)

**Fork fix #11 `inlineupdate`** (both rows, pushed): inline accessors now inherit the
update classification (`AccessProxies.newAccessorSymbol` sets the Mutable flag, gated on
separationChecking) — without it every `inline update def` mutating a private var fails
inside its synthetic accessor. Probe P10 green on both rebuilt rows; build branches
rebuilt and pushed (3.9 `...-staleread` @016f16e656, `all/3.10.0` @86cd606d28).

**Cursor converted** (zephyrine suite 233/233): `extends caps.ExclusiveCapability,
caps.Stateful` (NOT Mutable, per P4); ~27 members classified `update` (no pure peek
exists: anything that can lazily refill mutates); `marks`/`offsets` fields `Array[Long]^`;
unit is separation-checked. Bare `Cursor` = read-only is enforced in CC-only consumers
(verified by deliberate-misuse compile).

**Transparent-factory gotcha**: the expansion's type pins the tracked-val refinement to
expansion-local `$proxy` singletons; avoidance then collapses the fresh capture to `^{}`
and every update call is rejected as read-only. Fix: all four factories bind and ascribe
`val cursor: Cursor[data, …]^` internally (load-bearing comment in file). COST: the
refinement is stripped — `Operand`/`Target` go abstract on factory-built cursors; the
per-medium extensions (peek/expect/buffer) are unaffected; direct `clone`/`seek`-operand
users cast (8 sites, zephyrine tests only). Upstream-reportable (with the P7/P9/P5 items).
The iterator factory's loader is additionally SEALED (`unsafeAssumePure`) and its
iterator param untracked.

**THE PARSER WALL (decision needed)**: jacinta's pooled `Parser` (ThreadLocal reuse,
`private var cursor` field) cannot hold an exclusive cursor in a CC-only unit — a fresh
capability cannot flow into an exclusive var field (`any` vs `any²`; the write-side dual
of P9), casts don't launder it, and `@untrackedCaptures` doesn't relax it under plain CC.
Options: (a) separation-check jacinta.Json.scala and classify Parser as
Stateful+ExclusiveCapability with update methods (~30 methods; the principled path — the
ThreadLocal pool then also needs a capability-aware shape); (b) fork accommodation
allowing fresh-into-`@untrackedCaptures`-field assignment under CC; (c) revert Cursor
exclusivity. jacinta reverted to pristine on this branch pending the decision; the other
8 parser modules are expected to hit the same pattern wherever they field-hold cursors.

## Sepcheck jacinta conversion: 95% done, blocked on cap-param field admission (2026-07-11)

Option 1 executed: `Parser` extracted from jacinta.Json.scala into its own separation-
checked unit (jacinta.Json.Parser.scala — keeps the derivation-bearing Json.scala away
from the sepcheck import), re-parented `ExclusiveCapability, Stateful`, ~40 methods
classified `update` by compiler-driven fixpoint sweep, `chars`/keyCache arrays exclusive,
`StringScanContinue` frozen to `IArray`, pool hand-out cast to `Parser^` (asserts the
per-thread single-owner invariant the ThreadLocal pool provides by construction).

Lessons that took bisection:
- A method result must be `Cursor[Data, {}]^` — the `^` matters (bare = implicit read-only
  `.rd` result) and the WILDCARD cap arg `Cursor[Data, ?]` misbehaves; use the concrete
  `{}` arg.
- Method-result freshes ARE admissible into exclusive fields (probe-proven, incl. cross-
  module and under the full flag set) — EXCEPT for classes with a `cap^` capture-set
  parameter: `p11-capparam-field.neg` (19 lines, self-contained) shows plain factory,
  ascribed local, consume adapter, and direct `new` ALL fail with "fresh … is not visible
  from any in variable …", and `@untrackedCaptures` does not relax it. Cursor has a cap
  param, so Parser's three cursor-reset assignments are blocked. FORK FIX #12 candidate:
  the admission logic (maxSubsumes/levelOK — ctxresult-adjacent) treats the CapSet type
  argument's presence as level-relevant. Until then jacinta.core does not compile; the
  conversion is committed as WIP.

## Fork fix #12 VERDICT: not a bug — Unscoped is the design; Cursor re-parented to Mutable (2026-07-11)

Jon's constraint (only fix genuine compiler bugs) applied to P11. Root cause found in
`Capability.acceptsLevelOf`: level checking is waived for Unscoped-classified capabilities
(`|| classifier.derivesFrom(defn.Caps_Unscoped)`). Scoped exclusive capabilities not
flowing into longer-lived fields is the INTENDED discipline — it protects a real hazard
(a pooled parser outlives the method frame; a cursor whose loader captured a `withFile`
handle must not be smuggled out via a field). NO COMPILER CHANGE.

Sound design instead: **Cursor extends `caps.Mutable`** (Unscoped), with the loader FIELD
sealed pure at the factory boundary (`unsafeAssumePure`, the one audited point) so the
class captures nothing non-Unscoped. Confinement moves to the API: the loader's
capabilities live in the `cap` TYPE ARGUMENT, and probe-verified, a cursor over a
file-reading loader CANNOT escape `withFile` (type-variable instantiation rejects the
leaked capability). zephyrine 233/233 green; jacinta's field assignments now admitted.

jacinta status: compiles down to 10 errors, all mechanical, two patterns:
1. **Cursor-op hiding**: any binding of the exclusive cursor field (explicit val, or the
   inliner's `Cursor_this` receiver proxy) is typed as a widened fresh whose hidden set
   covers the Parser itself, so parser state may not be touched afterwards in the same
   scope. DISCIPLINE (applied to reconcileLineation/moreSlow/bom): pre-read parser state
   into locals, bind the cursor ONCE inside a `locally:` block, do all cursor work through
   that binding, write parser state after the block. Remaining sites: holding/tail region
   (~714-740), parseWord (~826). NOTE: a toy WITHOUT Cursor's `cap^` type parameter does
   not exhibit the hiding (p14 shape is green) — the widened-fresh typing of
   cap-parameterized field reads is upstream-discussion-worthy, but the statement rule
   itself is by-design.
2. **`raises` result hides this** (line ~758 `tail`): update methods with `raises` sugar
   get context-function results that capture `this` — the established convention from the
   3.10 work applies: explicit `(using Tactic[...])` parameter instead of `raises`.
Also fixed en route: ThreadLocal pool is an erased `AnyRef` boundary with one rim cast
(`borrow()`), matching the Conduit-queue precedent; `TenPow`/`StringScanContinue` frozen
to `IArray`; Bcd.finish results freeze-asserted (opaque over post-finish-immutable array);
buffer-pool getters de-inlined (per-expansion reach caps don't unify).

## jacinta GREEN (298/298); the block-scoping discipline codified (2026-07-11)

jacinta.core compiles clean (from-scratch module build) and its full suite passes with the
Parser as a separation-checked stateful capability. gesticulate + telekinesis converted en
route (operand casts; `Cursor[Data, {}]^` params — concrete cap arg, never `?`, which
collapses inline receiver proxies to read-only; `Http.parse` returns `Request^` since the
body thunk legitimately retains the local cursor — local fresh may hide in a fresh result).

The reusable discipline for stateful holders mixing own state with a held capability:
1. NEVER bind the exclusive field (or let an inline method's receiver proxy bind it) in a
   scope that later touches other own-state: pre-read own state into locals, bind the
   capability ONCE inside a `locally:` block, do all capability work through that binding,
   write own state after the block.
2. Methods that READ the capability field must be non-inline (a private field read from
   inline code synthesizes an accessor whose exclusive result type is a template-level
   hider barring other member declarations — P7's member-order rule, unfixable by
   reordering since the accessor placement isn't source-controlled).
3. `raises` sugar on update methods → explicit `(using Tactic[...])` (context-function
   results hide `this`); same for methods returning fresh values built inside `yet`
   by-name operands or raising closures (hoist to a binding, or convert the method).
4. Scratch arrays: exclusive (`Array[T]^`) with element access through DIRECT field paths
   only (the Cursor.recordMark pattern); a local binding of one hides the owner.
5. Post-parse-frozen data: type it IArray and freeze at the single build site
   (`.immutable(using Unsafe)`) instead of laundering at use sites.
6. Cross-thread/pool rims stay as documented casts (ThreadLocal pool borrow()).

OBSERVATION (worth confirming upstream): once one unit of a module enables
separationChecking, CC-only units compiled in the same run exhibit sepcheck-flavoured
typing (fresh/rd decorations on Array/IArray construction, Stateful classification
demands) — the module effectively converts as a whole. Plan the per-module sweeps
accordingly (the module is the gating unit, not the file).

## Query per-module test failures: pre-existing, both compilers (2026-07-11)

The 5 telekinesis Query tests (`Query.make(...).show` expecting url-encoding) fail in
per-module `mill telekinesis.test.run` on a CLEAN build of MAIN with BOTH the
pre-inlineupdate and current compilers (verified via a scratch worktree at c494765a6 and
a temporary rebuild of the pre-fix compiler): `.show` resolves legerdemain's companion
`Query is Showable` (debug format). The attested umbrella `soundness.Tests` bundle
compiles all test sources together, where the implicit scope evidently differs. NOT a
regression from this branch or from fork fix #11 (which is thereby exonerated of
CC-only-unit effects). Left as-is; flagged for a separate look at the per-module/umbrella
implicit-scope divergence.

## Phase 4 in flight: kernel re-parented and Conduit split (2026-07-11, branch sepcheck-kernel)

zephyrine.core GREEN with the full Phase-4 kernel: Producer/Stream/Intake/Duct extend
`ExclusiveCapability, Stateful` (capture-friendly — NOT Mutable: implementations capture
iterators/queues/sockets) with honest update classification; Conduit is now a FACTORY
returning `((Intake[medium] over Credit)^, (Stream[medium] over Credit)^)` over a private
`SharedCapability` core (queue/atomics; volatiles untracked — JMM-managed);
through/flowTo/accepting are consume-typed (Phase 5's pipeline semantics), with
construction hoisted into `throughDuct`/`acceptingDuct` helpers whose consume PARAMETERS
carry explicit sets — a local binding of the fresh duct would hide it from the anonymous
class that wraps it (the statement rule); Ductile's `duct` takes `consume stage: Self^`.

New workarounds proven this leg:
- Overriding/implementing an update method requires RESTATING `update`.
- Fresh stateful instances capturing evidence type as `^{evidence, caps.any}` results
  (naming the evidence; hiding only their own state) — Sink.buffered, Source.stream.
- Varargs of capabilities: follow the compiler's hint — capture-polymorphic
  `[cap^](sources: (Stream[...])^{cap}*)` fixed Confluence's reach-leak; while-loops
  instead of for-comprehensions (the desugared foreach closure captures the reach).
- Collection/fiber rims carry `AnyRef` (the Conduit-queue precedent): Manifold's
  subscribers and Confluence's per-fiber hand-offs.

REMAINING for the next leg: ~10 errors in turbulence_core.scala (legacy LazyList-view
sections: freeze-casts + flow sites), then zephyrine tests (Conduit pair destructure),
coaxial/galilei/hieroglyph/scintillate/telekinesis sweeps (same recipes), Phase 5 surface
(borrow-based foreach/fold + freeze-based memoize on Stream^; combinators exist via the
consume-typed through), full gates. LazyList-bridge REMOVAL deliberately deferred: needs
Jon's API review (public break).

## Phase 4/5 sweep: repo compiles except telekinesis (2026-07-11, sepcheck-kernel)

Full `soundness.all` is green EXCEPT telekinesis.core (three small error classes left):
1. `Request is Showable` given: lambda param needs the Request refinement ascription
   (the class type now carries the body-thunk refinement).
2. `() => Stream(cursor.remainder.iterator)` at Http.scala:386: fresh-in-thunk-result
   admission ("any not visible from any² in method parse") — p11-family; likely needs a
   named helper returning the thunk, or a rim.
3. `Http.emptyBody()` result is bare (read-only) — needs `(Stream[Data] over Credit)^`.
Then: apoplexy/scintillate knock-ons from the thunk-type sweep (`=> (Stream[Data] over
Credit)^` swept across galilei/telekinesis/apoplexy), test suites, Phase 5 tests,
LazyList-bridge removal (Jon approved; NOT yet started — Streamable in 44 files), gates.

This leg's recipes: Producer factory results exclusive; serializer helpers take
`Writer^`/`ProtobufPrinter^` (capability wrapper classes over their producers);
consume-to-consume argument forwarding is NOT admitted (neutral-carrier hop);
`lazyList` seals INSIDE its definition (LazyList is pure — cannot carry the consumed
stream's capture; `^` on LazyList is a case-2 error); enum payloads holding stream
thunks type them `() => (Stream[...])^`.

## Spring: fresh scoped results must come from methods, not lambdas (2026-07-11)

Root cause of the telekinesis tail (P12, `p12-thunk-fresh-*.scala`): `Stream` is a
SCOPED exclusive capability (ExclusiveCapability + Stateful, deliberately not
Unscoped), and a lambda may not mint a fresh scoped capability as its result — the
closure's per-call fresh is level-bound to the closure and "not visible from" the
function type's existential result capture at the binder. So the thunk type
`() => (Stream[Data] over Credit)^` is UNCONSTRUCTIBLE from any lambda that creates
a stream (worked for Cursor earlier only because Cursor extends Mutable = Unscoped,
which waives the level check). This is the level discipline working as designed —
the same rule that rejects a `Stream^` escaping its scope — NOT a compiler bug.

The legitimate fix: a named SAM trait whose METHOD returns the fresh stream — method
results are re-leveled at each call site. `zephyrine.Spring[medium]`
(`def apply(): (Stream[medium] over Credit)^`) replaced the thunk type everywhere
(telekinesis Request.body / Body.Flowing / Http.Backend / HttpClient, galilei
Handle.source, apoplexy, obligatory); `Postable.Streamer[content]` is the 1-arg
analogue (`def stream(content): (Stream[Data] over Credit)^`) for Postable.apply's
payload parameter. SAM conversion keeps every `() => Stream(...)` construction site
compiling unchanged; `request.body()` call syntax unchanged via `apply`. In non-CC
units (telekinesis.jvm/.wasi, like honeycomb.core) write bare `Spring[Data]`.

Also fixed alongside: `Postable.stream`'s result was bare (= read-only) — now `^`;
the `Showable` given hoists `val stream = request.body()` before `.lazyList`;
`Http.emptyBody(): (Stream[Data] over Credit)^`. Ascribe vals holding SAM lambdas
(`val body: Spring[Data] = () => ...`) or the val infers the raw function type
and fails at use (obligatory.GrpcChannel).

## LazyList-bridge removal: stage map (2026-07-11, in progress)

DONE (committed 72ecfb4aaa, 34aac9e368): all whole-body `.lazyList.read[...]`
reads → `memoize` (Http Showable/query, synesthesia dispatch, cordillera h2
payload, wasi payload, telekinesis/scintillate/apoplexy tests); Postable.preview
= one bounded refill + materialize (no full drain); apoplexy test Recorder takes
`Spring[Data]^`; Spring/memoize/foreachWindow added to soundness umbrella exports
(consume-extension export forwarders work — flowTo precedent; only dependent-
typed ones need hand-written forwarders).

REMAINING (2026-07-11 end of leg; items 3,4,5 of the original list DONE —
inputStream adapter committed 71aa56e032, websocket Reader + Multipart interim
committed 1157194e4b):
1. The Http wire-form cluster (the finale; single refactor): `Request.serialize`
   (Http.scala ~275/289) and the Response wire form (~674) build LazyList[Data]
   from `body().lazyList`; `Body.stream` (~485) is the LazyList accessor. The
   Content-Length-vs-chunked probe pattern-matches the LazyList (forces <=2
   cells); a kernel-native version pulls one block: stream ended -> fixed
   length, else chunked with pulled block + remainder. But serialize RETURNING
   LazyList inherently needs the seal - the honest fix changes serialize to
   return (Stream[Data] over Credit)^ (or write to an Intake), which changes
   coaxial Transmissible + scintillate/servlet/cordillera Body.stream callers,
   and removes the Body.Streaming(LazyList) arm. Possibly worth Jon input on
   the Transmissible shape.
2. Multipart.parse cursor conversion (drops the interim memoize in
   scintillate.Acceptable) - part of the Streamable tail.
3. Source2.streamableData/Text + Sink2.writableData/Text transitional givens:
   delete once modules summon native instances (the 44-file Streamable tail).
4. Delete both `lazyList` defs in turbulence_core + bridge tests
   (turbulence_test:538 + "Streamable instance is a Source through the bridge").
Then dual gates: make attest (3.9) + 3.10 clean gate.

## Jon's design decisions for the wire-form finale (2026-07-11)

Verbatim intent: the websocket Channel SHOULD be Conduit-backed, and
Transmissible.serialize SHOULD return a `Stream^`. "Coaxial's design should be
adapted to take full advantage of our new mutable streams and separation
checking." Execution order (bottom-up, compiling commits): coaxial
Transmissible/transmit layer -> Http wire form (serialize/Response/Body) ->
perihelion Channel -> scintillate/servlet/cordillera knock-ons -> delete
lazyList defs + Source2/Sink2 + bridge tests -> dual gates.

## Coaxial kernel-stream redesign executed (2026-07-11, commits c7c25a1903..4f22574f93)

Jon's decisions delivered: Transmissible.serialize returns `(Stream[Data] over
Credit)^` (one call = one message's wire form; framed transports memoize to one
unit — UDP datagram, WS frame — byte transports foreachWindow zero-copy);
Duplex.send takes a kernel stream; Http wire form kernel-native (Request
probe-one-block for Content-Length vs chunked, empty chunks RETRIED never
framed; Response framed via chunk iterators; Body.Streaming arm DELETED —
Flowing(Spring) is the only streaming arm; Body.stream mints a kernel stream);
websocket Channel is Conduit-backed (bounded => real backpressure; enqueue is
synchronized multi-producer and FLUSHES per frame — the conduit otherwise
buffers a block and interactive protocols deadlock; found via jcmd thread-dump);
Reader.messages defers cursor construction (stream-backed Cursor refills
EAGERLY at <init> — blocks on live sockets before the 101 goes out; the
documented cursor-eager-refill gotcha). Stream.apply(iterator) now takes
`Iterator[medium]^` returning `^{iterator, caps.any}` (iterator-backed streams
retain their iterator's captures). Response keeps a PURE `body: Body` field —
a Body^ field cascades ^ through copy/updateDynamic/read(this); the one
capturing constructor (Response.parse's cursor spring) is sealed with the
single-owner justification. BRIDGES DELETED: both lazyList views + Source2/
Sink2; native Source instances added for hellenism Resource + classpath Path,
Http.Response (raises HttpError on non-2xx), zeppelin Entry/Zipfile,
hallucination Raster; generic Streamable read-sites route via stream[Data] +
the native LazyList Source. Receivable/successBody still LazyList-shaped via
memoize — the remaining Streamable-tail (with Multipart cursor parsing and
Duplex.stream receive side) is follow-up work, not bridge-blocking.

## DUAL GATES GREEN (2026-07-11, @635fdc5546)

attest (3.9 row): 7905 passed / 0 failed; attestation note pushed
(refs/notes/ci-attestation @b2811a90f3). 3.10 row: clean-gate compile in
cc-review (mill clean first — zinc false-greens bit three times today), 10457
tasks green, only the benign aggregate finalMainClass non-task. Attest caught
what per-module AND batched clean sweeps missed (monotonous read-only params,
xylophone/graffiti Document reads, cacophony/gesticulate missing native Source,
embarcadero duplex fixture) — per-module clean compile loops are the only
locally trustworthy approximation, and even they missed embarcadero once.
escritoire.test is pre-existing broken on main and outside the attest suite.
PR next: title/body awaiting Jon.

## Pure errors: Hazard = Exception & caps.Pure (2026-07-11, branch pure-errors)

Jon's directive: values passed to record/raise MUST be pure, so errors can never
smuggle capabilities out of scopes through `throw` — the one channel capture
checking cannot see. P13 probes (both rows green) established that `caps.Pure`
enforcement is REAL: a Pure subclass with a capability field is rejected with
E223 ("not included in the allowed capture set {} of the self type").

Design: `fulminate.Error extends caps.Pure` (every Soundness error is pure by
inheritance), and `fulminate.Hazard = Exception & caps.Pure` names the raisable
domain — the bound on Tactic/Emit/raise/abort/lest/Attempt/raises and every
tactic class ("Fault" was taken by parasite's uncaught-task pair). JDK
exceptions must be wrapped (`Error(throwable)`) to be raised. Macro internals:
caseDefs/mapping (handler ANALYSIS on PartialFunction[Exception, _]) keep
Exception bounds; tactic-CONSTRUCTING sites use Hazard. Recovery.Escape (the
recover-value briefcase) is pure via an AnyRef rim — its payload never leaves a
scope inside the exception; record unwraps it and escapes through the
capture-checked boundary.break. throwUnsafely: ThrowTactic[Hazard, success].
Sweep fallout was small: 13 bounds outside contingency (parasite/zephyrine/
obligatory), 7 Tactic[Exception]→Tactic[Hazard] sites, one raw test exception
(zephyrine Mismatch → Error). Suites green incl. contingency 91/parasite 148.

## Sepcheck rollout (2026-07-11, branch sepcheck-rollout)

`settings.sep` added to build.mill (cc + separationChecking, module-level).
MODULE-WIDE SEP ON ZEPHYRINE IS BLOCKED BY P5: flagging the module pulls
Addressable.scala (deliberately the unchecked interior) into sepcheck. New P5
data from the attempt: ANNOTATED abstract members DO work — `def allocate(size:
Int): Storage^` and `storage: Storage^`/`Storage^{caps.any.rd}` params override
concrete Array signatures cleanly (the probe only tried bare members). What
remains blocked is the FIELD layer: honest fresh `allocate` means kernel classes
need `Storage^` fields, and abstract exclusive fields cannot be reassigned
(Conduit.publish's `current = allocate(block)`). Fix requires per-medium Storage
concretization or an upstream/fork primitive. Until then zephyrine/turbulence
keep per-file separationChecking imports; the rollout targets CONSUMER modules
(which hold capabilities but never implement storage). Also: IArray fresh-ness
under sepcheck is an opacity artifact (`caps.freeze` accepts `Mutable | Array[?]`
and cannot see through the opaque type) — upstream stdlib-annotation gap.
