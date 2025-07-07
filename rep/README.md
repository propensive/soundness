# `rep/` — capture-checking (CC) failure reproductions

Isolated, **verified** reproductions of the distinct capture-checking failure classes found by
characterising all 50 deferred test suites on `capture-checking-capabilities`. One subdirectory per
class: a **minimal** source (real Soundness types, in the module's own package) with a header comment
explaining the pattern and what we want to happen.

## Two kinds of reproduction

**Never use Mill to check these.** Mill's incremental compilation manufactures false-greens (it
under-runs CC's multi-pass capture estimation). A single-shot compile is authoritative.

**① Self-contained (no Soundness at all).** The two case-2 classes — 68% of all the failures — are
reduced to standalone files with a `//> using` header and *zero* dependencies. Just:

```bash
scala-cli compile rep/case2-directmint     # or:  rep/compile.sh case2-directmint
```

Reducing them needed the right ingredients (each verified load-bearing by bisection): a pure OPAQUE
type over a real type (a plain `class T extends caps.Pure` is not enough), the `is`/`Self` typeclass
sugar (`-language:experimental.modularity`), an INLINE given, and an INLINE extension that summons it.
With those, `case2-directmint` is 26 lines and `case2-freshvar` is 22.

**② Soundness-backed (dotc + real classpath).** The other five need machinery that does not reduce to
synthetic form — I tried and they compile clean without it (the real `Emit`/`Tactic` `raises` encoding,
Wisteria derivation, the opaque `Money` in `plutocrat.internal` behind a `Currency` typeclass, or a
macro that actually crashes). For these the minimal source lives in the module's own package with
`import soundness.*`, and is compiled with `dotc` directly against the module's real classpath:

```bash
rep/capture-classpath.sh <class> <module>   # once: Mill builds cores + writes cp.txt/opts.txt
rep/compile.sh <class>                       # dotc directly — reliable, no Mill false-greens
```

`capture-classpath.sh` is the only Mill step (build cores + read back classpath/options).
`compile.sh` auto-detects the mode. (`cp.txt`/`opts.txt` hold absolute machine paths and are
gitignored — regenerate with `capture-classpath.sh`.)

## The classes

| Class | dir | mode | module | error |
|---|---|---|---|---|
| case-2 direct-mint | `case2-directmint/` | **self-contained** | (chiaroscuro) | `Text is a pure type, it makes no sense to add a capture set` |
| case-2 fresh-var | `case2-freshvar/` | **self-contained** | (abacist) | override `text`: `(Pounds,Stones)^'s1` incompatible |
| capturing-raises | `capturing-raises/` | soundness-backed | zeppelin | `any` cannot flow into `{any²}` … in type `raises` |
| capturing-derivation | `capturing-derivation/` | soundness-backed | austronesian | derived `Decodable{… Matchable^'s24}` mismatch |
| capability-escape | `capability-escape/` | soundness-backed | exegesis | `needs to extend Capability` (`given_Tactic_JsonError`) |
| path-dependent `Self` | `path-dependent-self/` | soundness-backed | plutocrat | `Money in Eur.Self` vs `internal$_this.Money` |
| macro breakage under CC | `macro-under-cc/` | soundness-backed | quantitative | `scala.MatchError: None` in `checkable` macro |

### Distribution across the 50 deferred suites
- **case-2 (compiler boxes a pure value): 34 (68%)** — 23 fresh-var + 11 direct-mint. Dominant blocker.
- capturing-raises: 7 · capturing-derivation: 1 · capability-escape: 1 · path-dependent-self: 1 ·
  macro-under-cc: 2 · compile-clean (attest-unverified): 4 (anticipation, probably; larceny/vacuous
  fail at runtime).

## What we want (summary)

- **case-2 (68%)**: a pure value (`Text`, a Tuple of pure elements) must not acquire a capture set.
  Almost certainly a compiler issue in CC's capture estimation — not addressable in Soundness source
  (see `case2-*/`).
- **capturing-raises / -derivation / -escape**: the instance *legitimately* captures a `Tactic`; CC
  must let that flow. capturing-raises is blocked in `contingency`'s `raises = Tactic[error]^ ?=>
  success` encoding (tested: not fixable at the call site — see `capturing-raises/`). capturing-
  derivation & -escape have a source fix (self-`provide` the tactic).
- **path-dependent-self / macro-under-cc**: narrower compiler interactions (opaque-`Self` identity
  across paths; macro tree-shape assumptions broken by CC nodes).

## Verifying a newer compiler

Published `3.9.0-RC1` cannot parse Soundness (lacks `subCases`/`multiSpreads`/`relaxedLambdaSyntax`).
The only newer compiler that can is upstream `scala/scala3` `main` (`~/pub/scala3`). To test whether it
fixes case-2, build that dotc and point `compile.sh`'s `cs fetch` at the locally-published compiler
instead of `3.8.4` — the classpath capture and everything else stays the same. This is the natural next
experiment now that the reproductions compile with `dotc` directly.
