# `rep/` status

The verdict for every case under the pinned toolchain, re-run whenever the release changes
(`rep/toolchain.sh -version`). "Fixed" means the case compiles as its header wants; a case
fixed by a flag-gated fork repair is RED again without the flag (`REP_STOCK=1`), which is the
"needs `-Z`" column. "Upstream" is the state of the report: the self-contained and calibrated
cases are the ones that can be filed as they stand. Diagnoses and decisions are in
`DECISIONS.md`; the roadmap track is `doc/roadmap/safety.md`.

Measured 2026-10-07 on `3.9.1-dev-p17`.

## Self-contained and calibrated

| Case | Class | Verdict | Needs `-Z` | Fixed by | Upstream |
|---|---|---|---|---|---|
| `case2-directmint` | a pure value (`Text`) gets a capture set through an inline given + inline extension | GREEN | `-Zunboxed-pure-types` | fork `unboxedpure` | scala/scala3 #16978 class; PR-ready on `cc-pure-type-box-fix`, unfiled |
| `case2-freshvar` | a tuple of pure elements gets a fresh existential | GREEN | `-Zunboxed-pure-types` | fork `unboxedpure` | as above |
| `proxy-tagged` | cast type arguments of an undealiasable opaque application not boxed (`aka`-Tagged) | GREEN | no | fork `castbox` | unreported |
| `stacked-raises` | stacked context-function results (`X logs E raises F`) level-block the outer parameter | GREEN | no | fork `ctxresult` | red on upstream main (2026-07); unreported |
| `handler-raises` | a non-capture-checked module between capture-checked ones breaks the `raises` alias (mixed compilation) | GREEN | `-Zalias-captures` | fork `aliascap` | red on upstream main (2026-07); unreported |
| `splicealias-repro` | quote type holes in an anonymous class's members lose their binder | GREEN | no | fork `splicealias` | fixed upstream in 3.10 (#26307) |
| `shared-unscoped` | a shared (non-exclusive) tactic as the ambient `raises` strategy | `SharedTactic` GREEN · `ExclusiveUnscoped` GREEN · `SharedUnscoped` RED (correct) | no | nothing needed on this release for the minimal shape | n/a — design calibration for `safety-8` |
| `sepcheck-probes` P1–P16 | separation-checking characterisation | 33/33 as expected | no | — | P7 (member order), P9 (no field move), P5 (abstract storage) still reportable |

## Soundness-backed

Need `rep/capture-classpath.sh <case> <module>` once per machine (the paths are not portable).
The captured `opts.txt` carries the build's own options, `-Z` flags included, so `REP_STOCK` does
not apply here. Four of these had rotted against API renames since July and were refreshed on
2026-10-07 (`filesystemOptions.*`, `Pojo#as`, `LspInbound`/`Lsp.Client`, `scala.language`).

| Case | Module | Class | Verdict | Fixed by | Notes |
|---|---|---|---|---|---|
| `capturing-raises` | zeppelin | ambient `throwUnsafely` tactic cannot flow into a `raises` existential | GREEN | source: ambient tactics are `caps.Unscoped` (2026-07-06) | the `safety-8` gate: must stay GREEN when `Emit` becomes shared and the strategies drop `Unscoped` |
| `capturing-derivation` | austronesian | a derived codec captures a tactic | GREEN | source: capture-polymorphic `conjunction`/`disjunction` (Phase 7) | |
| `capability-escape` | exegesis | the generated RPC dispatcher kept in an object field retains the codecs' tactic | RED, by design | source: the dispatcher is a local of the serving method (`Lsp.listen`, `LspSessional`) | also shows the P15 overlap inside `JsonRpc.serve`'s codec summons — the second gate for `safety-8` |
| `path-dependent-self` | plutocrat | opaque `Money` seen through `Eur.Self` vs `internal.Money` | GREEN | never explicitly closed; compiles on the pinned release | |
| `macro-under-cc` | quantitative | macros break on the `@caps.internal.inferred` annotations capture checking adds | GREEN | source: annotation strips in the macros (2026-07-06) | the general quote-wall class stays open |
| `case2-real` | chiaroscuro | case-2 in situ | GREEN | fork `unboxedpure` | |
| `case2-spectacular` | spectacular | case-2 in situ | GREEN | fork `unboxedpure` | |

## Open classes without a case yet

These are the blockers the residue in `lib/` cites by description; each needs a case here
before its fork leg can start (`doc/roadmap/safety.md`).

| Class | Tag | Where it bites | Roadmap |
|---|---|---|---|
| exclusive-tactic overlap | `[tactic-overlap]` | every `unsafeAssumeSeparate(json.as[…])`; contingency's own three seals | `safety-8` (case: `sepcheck-probes/p15`) |
| by-name parameter unnameable in a capture set | `[by-name-capture]` | jacinta `optional`/`array`/`map`, spectacular `Inspectable`, gastronomy `Digestible`, turbulence `Streamable` | `safety-10` (case to write: `byname-capture`) |
| quote wall: `^` types do not survive `'{ }` (`?1` illegal capture) | `[quote-wall]` | wisteria `Derivation.scala:44`, legerdemain Query decoder (two telekinesis tests disabled), xenophile `WasmInvoke` | open; `safety-11` checks the 3.10 row |
| curried dependent context functions unsupported | `[curried-cft]` | contingency `dare` (⚑7), Foci/`tracks` ergonomics (⚑8) | open |
| ExpandSAMs before capture checking under Scala.js | `[js-expandsams]` | turbulence `Streamable` tactic-as-`AnyRef` | #1520; open |
| frozen `Data` loses `^{}` through generic combinators | `[iarray-opacity]` | the irreducible `.stdlib` residue (stratiform, mandible, vivisection) | open; fix belongs in the typeclasses |
