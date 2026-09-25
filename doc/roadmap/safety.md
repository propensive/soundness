# Capabilities and Effects

Honest signatures are only as honest as their enforcement. Capture checking makes a signature's
promises about effects verifiable; separation checking makes mutation safe by construction; the
`raises` mechanism makes failure visible in types. All three are already the default — nearly
every build component compiles with separation checking enabled — but defaults are not the same
as guarantees. Every `caps.unsafe` call is a place where the checker was overruled by hand, and
each one is a standing IOU against the safety claim.

The escape hatches are therefore this track's central measure. Some are genuine debt with a
known retirement recipe; others are blocked on defects in the capture checker itself — and
those defects are ours to fix, because Soundness is built with the Proscala compiler by
design. The compiler is modifiable whenever capability or effect checking demands it; the
`rep/` directory maintains minimal reproductions of each blocker class as the queue of fixes.
The one constraint is compatibility outward: whatever Proscala does, the artifacts Soundness
publishes must remain readable by the mainline Scala compiler. The track ends when the grep
for `caps.unsafe` returns nothing, and that readability guarantee is enforced rather than
assumed.

## safety-1: retire `untrackedCaptures`

Horizon: near
Baseline: 468 occurrences (measured 2026-09-25; 288 on 2026-08-01)

The retirement recipe is documented and mechanical: the annotated class becomes `caps.Mutable`,
mutating methods become `update def`, consumers hold `X^`, and mutual back-references are
flattened.

Done when:

    git grep -o untrackedCaptures -- lib | wc -l    # 0

## safety-2: triage the remaining unsafes

Horizon: near
Baseline: 416 `unsafeAssumeSeparate`, 502 `unsafeAssumePure`, 50 `unsafeErasedValue` (measured 2026-09-25; 323, 273 and 45 on 2026-08-01)

Each occurrence is either fixable now or blocked on a known compiler defect. The triage makes
the distinction explicit: every surviving occurrence carries a comment naming its `rep/`
blocker case, so the residue is exactly the blocked set and nothing hides in it. No script
checks the annotations yet; the flair census (`safety-7`) counts the occurrences but does not
read the comments.

Done when: every remaining `caps.unsafe` occurrence in `lib/` names a `rep/` case in an
adjacent comment, verified by a checked-in script reporting zero unannotated occurrences.

## safety-3: resolve the `capturing-raises` blocker

Horizon: mid

The dominant remaining class of capture-checking failure, reproduced minimally in `rep/`, is
fixed in the proscala fork, unblocking the annotated portion of the unsafe residue.
`rep/DECISIONS.md` records the fix at source — the ambient tactics classified as
`caps.Unscoped` (2026-07-06) — and the canonical reproduction green, but the recipe that
compiles `rep/` (`rep/compile.sh`) still fetches a `3.8.4` compiler rather than the pinned
proscala toolchain, so the criterion has not been re-run under the current one.

Done when: the `rep/` reproduction for `capturing-raises` compiles cleanly under the current
toolchain, and its `rep/DECISIONS.md` entry records the fix.

## safety-4: zero escape hatches

Horizon: mid → long
Needs: safety-3
Baseline: 1436 occurrences in total (measured 2026-09-25; 929 on 2026-08-01)

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

Done when: a CI test consumes published Soundness artifacts from a mainline-Scala project, and
`rep/` documents the rebase procedure and the measured cost of the two most recent rebases.

## safety-6: separation checking without exemption

Horizon: long
Baseline: 87 of 424 modules compile without separation checking — 40 components, 27 test
suites and all 20 benchmark modules — and 2 opt for `settings.cc` alone (measured 2026-09-25;
"3 components" on 2026-08-01)

The August figure counted the modules that *override* their options downward — the wasm guest
and one benchmark, both justified in comments. It missed that separation checking is opted
*into*: `Component`, `Tests` and `Benchmarks` default to plain `settings.scalaOptions`, so a
module that never mentions `settings.sep` is not checked at all, and eighty-odd never do
(among them the anthology formats, reliquary, synesthesia, telekinesis's JVM backend and every
benchmark). The end-state has no such module: every one compiles with separation checking.

Done when: no component in `build.mill` compiles with anything weaker than `settings.sep`.

## safety-7: measure the whole escape surface, not one grep at a time

Horizon: near

The escape hatches this track burns down are counted by hand, from baselines that go stale
between measurements: `safety-4`'s 929 was measured in August 2026 and had drifted past 1400
before anyone recompiled the grep. Two ratchets cover a slice of the surface properly, per
file and enforced — `etc/check-while-count.py` for `while`, `readUnchecked` and `charAt`, and
`etc/check-stdlib-count.sh` for the `.stdlib` bridge — and the rest is unmeasured.

The flair plugin (pinned in `etc/tools`, configured by `.pyrocosm/flair/config.tel`, enabled
on every checked component by the `-P:flair:` options in `flairToolchain`) now provides the
census: every rule in the configuration counted over the sources — the declared escapes
(`unsafely`, the `caps.unsafe` family, every `unsafe`-prefixed name, every method gated by the
token), the compiler-trust bypasses (`asInstanceOf`, `.nn`, `@unchecked`, `???`, catch-all
clauses), the partial reads, and the imperative constructs the streaming kernel is meant to
confine. `make unsafety` runs `flair metrics --dry-run` and prints it; `flair metrics` records
it in git notes, commit by commit, so the trend lives in git rather than in a roadmap
paragraph. (`etc/unsafety-history.tsv` is the earlier, hand-appended form of the same record,
unwritten since #1995.) One rule already gates: `S1.1`, a method taking `(using Unsafe)` must
be named `unsafe…`, fails the build; its converse `S1.2` is advisory, with the ten definitions
that break it listed in `doc/standards/naming.md`.

It reports and does not gate: the counts must settle across a few clean builds before a
number is worth failing a build over. The gate is the follow-up, and it should subsume the two
existing ratchets rather than sit beside them — one per-file baseline covering every
indicator, not a script per construct.

Done when: the census is recorded on every release, `safety-1`, `safety-2` and `safety-4` read
their baselines from it rather than from a hand-run grep, and the per-file gate has replaced
`etc/check-while-count.py` and `etc/check-stdlib-count.sh`.
