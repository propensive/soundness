# Handover — capture-checking (CC) reproductions

> **2026-07-04 update:** the case-2 compiler fix is DEPLOYED — the build now pins the
> locally-published `3.8.4-cc1` (see `build.mill:48` and `rep/DECISIONS.md`). §4's experiment is
> resolved: both self-contained repros are green under the patch, red on stock. The suite triage
> after deployment: 6 more suites enabled (abacist, anticipation, chiaroscuro, mosquito, probably,
> spectacular → 78 of 127 total); the remaining ~29 reds re-classify into the taxonomy captured in
> `rep/DECISIONS.md` (anon-instance fresh-var, Foci codec-helper, capturing-raises, singles) —
> capturing-raises is now the dominant class. Counts below are pre-deployment history.

This directory isolates the seven distinct capture-checking failure classes that block enabling CC on
the deferred test suites (`capture-checking-capabilities` branch). It exists so each failure can be
looked at — and eventually fixed — on its own, without running the whole Soundness build.

Read `README.md` for the mechanics; this file is the "what it means and what to do" summary.

---

## 0. The one rule: never trust Mill for CC

Mill's **incremental** compilation manufactures false-greens — it under-runs CC's multi-pass capture
estimation, so a suite that fails under `make attest` compiles clean under `mill …test.compile`. Every
reproduction here is therefore checked with a **single-shot** compile:

- **self-contained** repros → `scala-cli compile rep/<class>`
- **soundness-backed** repros → `rep/compile.sh <class>` (drives `dotc` directly against the real
  classpath; run `rep/capture-classpath.sh <class> <module>` once first)

`rep/compile.sh <class>` auto-detects which kind it is. A CC error printed = the reproduction. If you
ever see green where you expected red, you are almost certainly back on a Mill/incremental path.

---

## 1. How to run each

| class | run | reds with |
|---|---|---|
| case2-directmint | `scala-cli compile rep/case2-directmint` | `Text is a pure type, it makes no sense to add a capture set` |
| case2-freshvar | `scala-cli compile rep/case2-freshvar` | override `text`: `(Pounds,Stones)^'s1` incompatible |
| capturing-raises | `rep/capture-classpath.sh capturing-raises zeppelin && rep/compile.sh capturing-raises` | `any` cannot flow into `{any²}` … in type `raises` |
| capturing-derivation | `rep/capture-classpath.sh capturing-derivation austronesian && rep/compile.sh capturing-derivation` | derived `Decodable{… Matchable^'s24}` mismatch |
| capability-escape | `rep/capture-classpath.sh capability-escape exegesis && rep/compile.sh capability-escape` | `needs to extend Capability` |
| path-dependent-self | `rep/capture-classpath.sh path-dependent-self plutocrat && rep/compile.sh path-dependent-self` | `Money in Eur.Self` vs `internal$_this.Money` |
| macro-under-cc | `rep/capture-classpath.sh macro-under-cc quantitative && rep/compile.sh macro-under-cc` | `scala.MatchError: None` in `checkable` macro |

The first two are 22–26-line standalone files with **no Soundness dependency** — the ones to hand to
a dotty maintainer. The other five need the real Soundness type-graph (I tried to reduce them; they
compile clean without the real machinery), so they compile the minimal real source against the
module's captured classpath.

---

## 2. What each class is, and what needs fixing

### case-2 (direct-mint + fresh-var) — **68% of all failures. Fix this first.**
A value of a **pure** type wrongly acquires a capture set:
- *direct-mint*: a pure value (`Text`) gets `^{}` — "makes no sense to add a capture set to it".
- *fresh-var*: a Tuple of pure elements gets a fresh existential `^'s1`.

Both surface when an **inline given** of a `Self`-typeclass is instantiated at a pure type and the
result flows through an **inline extension**. This is almost certainly a **compiler bug in CC's
capture estimation**, not something fixable in Soundness source: the value is pure, nothing captures a
capability, yet the box is added. The self-contained repros pin it to exactly that shape.

**What we need:** a dotty fix so that instantiating an inline/typeclass method at a pure type does not
box the pure result. This single fix would unblock ~34 of the 50 suites. It is the highest-leverage
item by a wide margin — everything else is a long tail.

**How to verify a fix:** `scala-cli compile rep/case2-directmint` and `…/case2-freshvar` should go
green (point scala-cli/dotc at the candidate compiler — see §4).

### capturing-raises — 7 suites. Needs a Soundness (contingency) redesign OR a compiler fix.
A library method `… raises E` = `Tactic[E]^ ?=> …`. Satisfying it via the ambient `throwUnsafely`
mints a `Tactic^{any}` whose capability lives in the enclosing function and cannot flow into the
method's fresh `raises` existential `^{any²}`.

**What we need:** decide between two routes (documented in the repro header):
- a **contingency-core redesign** of the `raises` encoding so a sync error-raise threads the tactic
  without a fresh capture — *tested constraint:* simply removing the `^` from
  `raises = Tactic[error]^ ?=> success` breaks turbulence's async `await`/`safely` (the `^` is
  load-bearing for the Emit/async side), so the two encodings must be split; **or**
- a compiler fix to how the `raises` context-function's fresh existential unifies with a provided
  tactic. Call-site workarounds do **not** work (explicit `raises`, `inline`, `unsafely{}` all fail).

### capturing-derivation — 1 suite (austronesian). Has a Soundness-side fix.
A Wisteria-derived `Decodable` legitimately captures a `Tactic[VariantError]`; the derivation's
`derivedOne` `asMatchable.match` re-boxes the type-argument.
**What we need:** self-`provide[Tactic[VariantError]]` at the two genuinely-capturing sites
(austronesian `DecodableDerivation.disjunction`, the wisteria test fixture) — the jacinta/stratiform
pattern — so the derived instance is self-contained. This is a local source fix, no compiler change.

### capability-escape — 1 suite (exegesis). Has a Soundness-side fix (a design choice).
`lazy val dispatch = JsonRpc.serve[Lsp](this)` stores a `Tactic`-capturing function in a **field**, so
the object must "extend Capability". This capture is *real*, not spurious.
**What we need:** don't retain the capability in a field — either `provide` the tactic inside the
initializer, or make `dispatch` a `def` that takes the tactic. Local source fix.

### path-dependent-self — 1 suite (plutocrat). Likely a compiler fix.
An opaque `Money` (in `plutocrat.internal`, behind a `Currency` typeclass with `Self <: Label`) seen
through `Eur.Self` is not judged identical to the same opaque via the abstract `internal.Money`
self-path. **What we need:** dotty should treat the opaque as the same type across those paths under
CC. (Not yet reduced to a self-contained repro; see §3.)

### macro-under-cc — 2 suites (quantitative, cataclysm). Mixed.
CC annotations on operand trees break assumptions in a macro: quantitative's `checkable` throws
`MatchError: None`; cataclysm's `Attribution` literal-extraction reports "must be a string literal".
**What we need:** make the macros tolerate CC-annotated trees (Soundness-side, per macro), or a
compiler fix if CC is attaching nodes the macro API shouldn't expose. Triage per macro.

---

## 3. Suggested order of work

1. **case-2 compiler fix** (unblocks 68%). Use the two self-contained repros as the regression tests.
   Prove a candidate compiler fixes them before anything else.
2. **capturing-derivation + capability-escape** — local Soundness `provide` fixes; cheap wins (2 suites).
3. **capturing-raises** — the contingency `raises` split; medium, needs a full `make attest` to
   validate (contingency + turbulence are foundational).
4. **path-dependent-self, macro-under-cc** — narrower; do after the big rocks. First finish reducing
   path-dependent-self to a self-contained repro (it looked the most tractable of the remaining five).

The 4 suites that compile clean today (anticipation, probably real; larceny, vacuous fail at runtime)
are separate and unaffected by the above.

---

## 4. Testing whether a newer compiler fixes case-2

Published `3.9.0-RC1` **cannot parse** Soundness (missing `subCases`/`multiSpreads`/
`relaxedLambdaSyntax`). The only newer compiler that can is upstream `scala/scala3` `main`
(`~/pub/scala3`). Because the two case-2 repros are now **self-contained**, testing them against a new
compiler no longer needs the Soundness build at all:

```bash
# build/publish ~/pub/scala3 locally, then:
scala-cli compile rep/case2-directmint --scala <locally-published-version>
```

or point the `cs fetch` in `rep/compile.sh` at the locally-published `scala3-compiler`. If both case-2
repros go green there, the 68% is a compiler bug already fixed upstream and the fix is "bump the
compiler" — by far the best outcome. This is the recommended next experiment.

---

## 5. Housekeeping

- `rep/` is currently **untracked** (nothing committed). Commit it if you want it to travel with the branch.
- `cp.txt`/`opts.txt` (soundness-backed classpaths) are machine-specific absolute paths — gitignored;
  regenerate with `capture-classpath.sh`.
- `enable_cc.py` is the helper used by `capture-classpath.sh` to toggle `settings.cc(...)` on a
  module's `test` target in `build.mill` (auto-reverted).
