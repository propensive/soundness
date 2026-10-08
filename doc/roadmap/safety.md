# Capabilities and Effects

Honest signatures are only as honest as their enforcement. Capture checking makes a signature's
promises about effects verifiable; separation checking makes mutation safe by construction; the
`raises` mechanism makes failure visible in types. All three are already the default — 370 of
the build's 464 modules compile with separation checking — but defaults are not the same as
guarantees. Every `caps.unsafe` call is a place where the checker was overruled by hand, and
each one is a standing IOU against the safety claim.

The escape hatches are therefore this track's central measure. Some are genuine debt with a
known retirement recipe; others are blocked on defects or gaps in the capture checker itself —
and those are ours to fix, because Soundness is built with the Proscala compiler by design. The
compiler is modifiable whenever capability or effect checking demands it; the `rep/` directory
holds a minimal reproduction of each blocker class, and `rep/STATUS.md` says which are fixed,
which are open, and which have been reported upstream. The one constraint is compatibility
outward: whatever Proscala does, the artifacts Soundness publishes must remain readable by the
mainline Scala compiler. The track ends when the grep for `caps.unsafe` returns nothing, and
that readability guarantee is enforced rather than assumed.

The residue is not one problem but seven, and they are not equally hard. Measured on
2026-10-07 over `lib/` — 1485 visible hatches (505 `untrackedCaptures`, 511 `unsafeAssumePure`,
419 `unsafeAssumeSeparate`, 50 `unsafeErasedValue`), with roughly as many again hidden behind
the named wrappers (`Array.unsafeFrozen` 283, `Array.unsafeJvm` 205, `Array.frozen` 151,
`unsafeMutable`/`unsafeImmutable` ~40, `!!` 30) — the clusters by root cause are:

| Cluster | Count | Retired by |
|---|---|---|
| `@untrackedCaptures` on `var`/`val` fields (437 vars; ~270 of them primitive, `Optional` or immutable-collection typed) | 505 | `safety-1`'s recipe, module by module |
| test-harness `unsafeAssumeSeparate` (tmux drivers, `await`, `recur`) | 236 | one loan-shaped helper per harness (`safety-9`) |
| derivation anchors `unsafeAssumePure(Json.…Derivation.derived)` in the JSON-RPC bindings | 103 | the field-purity rule, once tactics are shared (`safety-8`) |
| sealed codec givens whose SAM closes over a tactic | ~125 | the honest `^{tactic}` given form, unblocked by `safety-8` |
| exclusive-tactic overlap `unsafeAssumeSeparate(json.as[…])` and the three seals in contingency itself | ~60 | `safety-8` |
| self/callback/handle launders (`unsafeAssumePure(this)`, RPC proxies, stored callbacks) | ~60 | per-site design |
| by-name element-codec thunks laundered to `() -> …` | ~46 | nameable by-name captures (`safety-10`) |
| `unsafeErasedValue` for erased evidence | 50 | mechanical, if `compiletime.erasedValue` suffices |
| staging, quotes and macros (the quote wall) | ~27 | compiler-side |

## safety-1: retire `untrackedCaptures`

Horizon: near
Baseline: 484 occurrences (measured 2026-10-08 after the first class conversion; 507 on 2026-10-07; 468 on 2026-09-25; 288 on 2026-08-01)

The retirement recipe is documented and mechanical: the annotated class becomes `caps.Mutable`,
mutating methods become `update def`, consumers hold `X^`, and mutual back-references are
flattened. 437 of the annotations are on `var`s, and about 270 of those are primitives,
`Optional`s or immutable collections in classes that are not yet `Mutable` — pure data behind a
missing classifier. The first modules are the ones already partly migrated (xylophone's `Xml`
parser, sibylline's `Llm`, stratiform's `Tel`, zephyrine's core), then the others by size. The
88 `AnyRef | Null` fields are different: each is a capability handle smuggled past the checker,
and each is a design case, not a sweep. The first sweep (stripping every primitive-typed
annotation and letting the compiler object) found only 2 of 140 removable: the rest guard
`var`s in classes that are not `Stateful`, or locals captured by closures, and making a class
`Stateful` brings the read-only and `update def` discipline to it and its callers — so the
recipe is one class per change. The first, `profanity.Board` with its six surfaces (and
`Terminal`, whose size the surfaces read), removed 22 annotations and three separation seals
for one tagged purity seal (`Stdio.print` is read-only by interface); `rep/DECISIONS.md`
records the recipe as it actually went, including the two rules that shaped it: a read-only
method may not use a captured impure thunk, and a factory must return a fresh `X^` or every
holder becomes a read-only alias.

Done when:

    git grep -o untrackedCaptures -- lib | wc -l    # 0

## safety-2: every remaining hatch names its reason

Horizon: near
Baseline: 419 `unsafeAssumeSeparate`, 511 `unsafeAssumePure`, 50 `unsafeErasedValue` (measured 2026-10-07; 416, 502 and 50 on 2026-09-25; 323, 273 and 45 on 2026-08-01); 37 of them cite `rep/` at all, 2 by case

Each occurrence is either fixable now or blocked on something named. The triage makes the
distinction explicit with a closed vocabulary of reason tags, defined in the capabilities
standard (`safety-12`) and each mapped to a `rep/` case or a section of the standard —
`[tactic-overlap]`, `[field-purity]`, `[by-name-capture]`, `[quote-wall]`, `[curried-cft]`,
`[js-expandsams]`, `[iarray-opacity]`, `[fresh-in-lambda]`, `[registry-lifetime]`,
`[test-harness]` and so on. A hatch carries its tag in an adjacent comment; the census
(`safety-7`) counts the residue per tag, which is what decides the fork queue, and fails on an
untagged site once the untagged count has been ratcheted to zero.

Done when: every remaining `caps.unsafe` occurrence in `lib/` carries a reason tag, verified by
the census script reporting zero untagged occurrences.

## safety-3: `rep/` runs on the pinned toolchain

Horizon: near

The queue is only a queue if its entries can be run. Until 2026-10-07 `rep/compile.sh` fetched
a stock 3.8.4 compiler and the probe scripts named locally-built worktrees, so no case had been
re-run under the fork release the build actually uses — and the fork's opt-in repairs (`-Z…`)
were never passed, so a fixed case could still show red. `rep/toolchain.sh` now runs `dotc`
from the cached release with the `-Z` flags read from `build.mill`; every case and the probe
suite re-run under it, and `rep/STATUS.md` records the verdicts. The `capturing-raises` class
this item used to name was fixed at source on 2026-07-06 (ambient tactics as `caps.Unscoped`)
and compiles; the dominant classes now are the exclusive-tactic overlap (`safety-8`) and the
by-name capture gap (`safety-10`).

Done when: `rep/STATUS.md` records a verdict for every case under the pinned release, and the
probe suite and every calibrated case run green-as-expected from a clean checkout.

## safety-4: zero escape hatches

Horizon: mid → long
Needs: safety-8, safety-10
Baseline: 1485 occurrences in total (measured 2026-10-07; 1436 on 2026-09-25; 929 on 2026-08-01)

With the blockers fixed, the residue burns down to nothing. The grep is the signal.

Done when:

    git grep -oE 'unsafeAssumeSeparate|untrackedCaptures|unsafeAssumePure|unsafeErasedValue' -- lib | wc -l    # 0

## safety-5: the fork is an asset, not a trap

Horizon: long

Building with Proscala is a design decision, not debt: the freedom to fix the capture checker
is what makes zero escape hatches reachable at all. What keeps that freedom safe is downstream
readability — every published artifact remains consumable from a project built with the
mainline Scala compiler — and a rebase onto each upstream release whose cost is known rather
than feared.

Soundness's published artifacts reference two fork-only library symbols — `scala.Spreadable`
(proscenium's `multiSpreads` givens) and, since `safety-8`, `scala.caps.SharedUnscoped` on the
ambient tactics — both shipped in the fork's supplementary `proscala-library` jar. A mainline
consumer therefore needs that jar on its classpath; the readability test below must say so or
make it unnecessary.

Done when: a CI test consumes published Soundness artifacts from a mainline-Scala project, and
`rep/` documents the rebase procedure and the measured cost of the two most recent rebases.

## safety-6: separation checking without exemption

Horizon: long
Baseline: 81 of 464 modules compile without separation checking — 40 components, 26 test
suites, 11 of the 23 benchmark modules and 4 internal modules — and 2 benches opt for
`settings.cc` alone (measured 2026-10-07 after `Benchmarks` defaulted to `settings.sep`; 93 that
morning; 87 of 424 on 2026-09-25)

Separation checking is opted *into*: `Component`, `Tests` and `Benchmarks` default to plain
`settings.scalaOptions`, so a module that never mentions `settings.sep` is not checked at all.
Of the 93, only nine say why (praxinoscope's Pike VM, probably's event bridge, the wasm guest,
polyvinyl's record selection, enigmatic's padding given, four benchmarks that compile against
rival libraries which cannot take explicit nulls); the rest — every other benchmark, 22 test
suites, the staged and compiler-tooling components, the WASI backends, the JVM backends of
galilei and telekinesis — are unchecked by omission. The end-state has no such module. `Benchmarks` now
defaults to `settings.sep` like a library (nine benches opt down, each naming the one shape —
a suite object holding exclusive payload arrays as fields — that blocks them); every other
unreasoned module is to be flipped in a single-shot probe (`rep/probe-suite.sh`,
`rep/probe-core.sh`), and each one that stays red carries a comment in `build.mill` naming its
reason tag and a `rep/` case for the class.

Done when: no component in `build.mill` compiles with anything weaker than `settings.sep`.

## safety-7: measure the whole escape surface, not one grep at a time

Horizon: near

The escape hatches this track burns down are counted by hand, from baselines that go stale
between measurements: `safety-4`'s 929 was measured in August 2026 and had drifted past 1400
before anyone recompiled the grep. Two ratchets cover a slice of the surface properly, per
file and enforced — `etc/check-while-count.py` for `while`, `readUnchecked` and `charAt`, and
`etc/check-stdlib-count.sh` for the `.stdlib` bridge — and the rest is unmeasured.

The flair plugin (pinned in `etc/tools`, configured by `.pyrocosm/flair/config.tel`, enabled
on every checked component by the `-P:flair:` options in `flairToolchain`) provides the
census: every rule in the configuration counted over the sources — the declared escapes
(`unsafely`, the `caps.unsafe` family, every `unsafe`-prefixed name, every method gated by the
token), the compiler-trust bypasses (`asInstanceOf`, `.nn`, `@unchecked`, `???`, catch-all
clauses), the partial reads, and the imperative constructs the streaming kernel is meant to
confine. `make unsafety` runs `flair metrics --dry-run` and prints it; `flair metrics` records
it in git notes, commit by commit, so the trend lives in git rather than in a roadmap
paragraph. One rule already gates: `S1.1`, a method taking `(using Unsafe)` must be named
`unsafe…`, fails the build; its converse `S1.2` is advisory, with the ten definitions that
break it listed in `doc/standards/naming.md`.

The gate is the follow-up: one per-file baseline covering every indicator — the two ratchets'
constructs, the four `caps.unsafe` hatches, and the named wrappers that hide them
(`Array.unsafeFrozen`, `Array.frozen`, `Array.unsafeJvm`, `unsafeMutable`, `unsafeImmutable`,
`!!`) — with the same `--update`/`--totals` semantics as `check-while-count.py`, counting the
residue per reason tag (`safety-2`), run by `make build`. It subsumes the two existing ratchets
rather than sitting beside them.

Done when: the census is recorded on every release, `safety-1`, `safety-2` and `safety-4` read
their baselines from it rather than from a hand-run grep, and the per-file gate has replaced
`etc/check-while-count.py` and `etc/check-stdlib-count.sh`.

## safety-8: tactics are shared capabilities

Horizon: near
Needs: safety-3
Baseline: ~60 exclusive-tactic overlap seals in `lib/*/src/core`, 3 of them in contingency; 103 derivation anchors and ~125 sealed codec givens downstream (measured 2026-10-07)

`contingency.Emit` — and so `Tactic`, the capability behind `raises` — is a
`caps.ExclusiveCapability`, chosen in July as "the conservative classification". It is the
single largest root cause in the residue. A codec summoned for `Json#as` captures the ambient
tactic, and `as` takes the same tactic as a using-parameter: two uses of one *exclusive*
capability in one call, which separation checking rejects (`rep/sepcheck-probes/p15`). Every
`unsafeAssumeSeparate(json.as[…])` is that error, and the derivation anchors in the JSON-RPC
bindings abandoned the honest tactic-parameterised form for the same reason inside the
derivation.

A tactic is shared by nature — many codecs legitimately alias one — and raising an error is a
non-consuming effect. `caps.SharedCapability` keeps every `raises`/`emits` capture tracked and
only exempts aliases of one tactic from the overlap check; a tactic that mutates (accrual,
`Foci`) must then be internally sequential, the position already taken for `Monitor` and the
parasite queues. The ambient strategies drop `caps.Unscoped` (unrelated classifiers cannot
combine, `p16`); on the pinned release the minimal `raises` shape no longer needs it
(`rep/shared-unscoped`, GREEN). The flip is probe-gated: contingency first, with the
Soundness-backed `capturing-raises` case as the gate; if the full shape hits a level wall, the
fork adds a classifier that is shared and level-exempt before the flip proceeds. Then jacinta
(`as` drops its unused tactic parameter), the consumer sweep (sibylline, tarantula, orthodoxy,
breviloquence, ethereal, the `abort`-thunk seals), and the anchors.

Status: `Emit` is shared and the strategies are plain `Tactic`s; the overlap seals in nine
modules are gone (`unsafeAssumeSeparate` 419 → 350). The level wall survived in one shape —
`strategies.throwUnsafely` summoned inside a derivation's field thunk (`rep/sepcheck-probes/p17`,
the same on 3.10) — user-facing, so the fork grew `caps.SharedUnscoped` (proscala
`sharedunscoped`) for the ambient strategies, and `rootclassify` for the `normalizeLocalCaps`
crash the shared tactics exposed in every `Sessional` loan; both shipped in 3.9.1-dev-p18. A
shared *global* strategy value was tried instead and rejected: a package may not export a
capability, an `object strategies` forces `uses soundness.strategies` onto every user object,
and a capability-class instance cannot be typed pure (`rep/DECISIONS.md`). The derivation
anchors (now `[field-purity]`) remain.

Done when: `Emit extends caps.SharedCapability`; `git grep -c 'unsafeAssumeSeparate' -- lib/contingency` is 0; no `[tactic-overlap]` tag remains.

## safety-9: test harnesses do not seal

Horizon: near-mid
Baseline: 236 `unsafeAssumeSeparate` in `lib/*/src/test` (measured 2026-10-07): exoskeleton 63, zephyrine 50, facsimile 38, galilei 29, turbulence 19

More than half of all `unsafeAssumeSeparate` sites are in test suites, almost none commented,
in a handful of shapes: the tmux completion drivers (`Bash.tmux()(Tmux.completions(…))`),
`async(…).await()`, `recur()`, and `gather.data`. Tests are capture-checked like libraries, so
this is real residue, and it is the most mechanical batch: one loan-shaped helper per harness
(the coaxial seam — a start/stop trait and a `transparent inline` loan — proven in syndesis), a
`Task#await` that does not alias the `Monitor` with the handle, after a probe per shape
characterises what the checker objects to.

Done when: `git grep -c unsafeAssumeSeparate -- 'lib/*/src/test'` is 0.

## safety-10: by-name parameters can be named in capture sets

Horizon: mid
Needs: safety-3
Baseline: ~46 pure-thunk seals (jacinta, spectacular, gastronomy, turbulence; measured 2026-10-07)

A by-name parameter (`inner: => (X is Codec)^`) is not a stable reference, so a given whose
instance retains it cannot declare `^{inner}` and is sealed to a pure `() -> …` thunk instead —
the Phase-6 form, truthfully commented but a seal all the same. The shape is load-bearing:
given resolution synthesises the thunk, and recursive derivation depends on its deferral, so no
source-side form preserves both. This is a compiler gap ("upstream candidate #5" in
`rep/DECISIONS.md`): a by-name parameter should be nameable as the capture set of its
synthesised thunk. A self-contained `rep/byname-capture` case first; the fork change second;
an upstream report with the case.

Done when: `rep/byname-capture` compiles under the pinned release and no `[by-name-capture]` tag remains.

## safety-11: the 3.10 stream is evaluated for what it fixes

Horizon: near-mid
Needs: safety-3

The fork keeps a 3.10 row alongside 3.9, and upstream's capture checker has moved since the 3.9
branch point — 65 commits to `cc/` and `scala/caps` by August 2026: `consume` on overrides and
case classes and in method application, classifier `only`/`except` and classifiers in result
capabilities, LocalCap leak detection, boxed-status healing. Some of that may dissolve clusters
here without a fork change (the harness shapes, a finer tactic classification, the `?1` quote
leak in legerdemain's Query decoder); some of it touches exactly what Soundness relies on (the
`Unscoped` tightening; reach capabilities dropped). The evaluation is measured, not guessed: a
clean gate of the tree on the 3.10 release, the whole `rep/` corpus and probe suite on both
rows recorded as a second column of `rep/STATUS.md`, and one un-sealed sentinel per cluster
compiled on 3.10 only. The decision — switch the attested row, or backport the specific
commits onto 3.9 — follows from that table.

Done when: `rep/STATUS.md` carries a 3.10 column for every case and probe, and the roadmap
records the row decision.

## safety-12: a capabilities standard

Horizon: near

The recipes that make capture and separation checking workable — when a type is Exclusive,
Shared, Unscoped, Mutable or Pure and the lattice traps between them; the honest-given playbook
(`(tactic: Tactic[E]) => ((X is TC)^{tactic})`, explicit capturing evidence instead of context
bounds); where a fresh result may and may not be minted; the `consume` and loan seams; what a
test may and may not do — exist only as entries in `rep/DECISIONS.md`, a 2400-line log. They
belong in `doc/standards/capabilities.md`, with the tag vocabulary `safety-2` relies on and the
one sanctioned hatch form for each tag, and a pointer from `doc/philosophy/capture-checking.md`.

Done when: `doc/standards/capabilities.md` exists, defines every tag the census recognises, and
`rep/DECISIONS.md`'s recipe sections link to it rather than restating.
