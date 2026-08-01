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
Baseline: 288 occurrences (measured 2026-08-01)

The retirement recipe is documented and mechanical: the annotated class becomes `caps.Mutable`,
mutating methods become `update def`, consumers hold `X^`, and mutual back-references are
flattened.

Done when:

    git grep -o untrackedCaptures -- lib | wc -l    # 0

## safety-2: triage the remaining unsafes

Horizon: near
Baseline: 323 `unsafeAssumeSeparate`, 273 `unsafeAssumePure`, 45 `unsafeErasedValue` (measured 2026-08-01)

Each occurrence is either fixable now or blocked on a known compiler defect. The triage makes
the distinction explicit: every surviving occurrence carries a comment naming its `rep/`
blocker case, so the residue is exactly the blocked set and nothing hides in it.

Done when: every remaining `caps.unsafe` occurrence in `lib/` names a `rep/` case in an
adjacent comment, verified by a checked-in script reporting zero unannotated occurrences.

## safety-3: resolve the `capturing-raises` blocker

Horizon: mid

The dominant remaining class of capture-checking failure, reproduced minimally in `rep/`, is
fixed in the proscala fork, unblocking the annotated portion of the unsafe residue.

Done when: the `rep/` reproduction for `capturing-raises` compiles cleanly under the current
toolchain, and its `rep/DECISIONS.md` entry records the fix.

## safety-4: zero escape hatches

Horizon: mid → long
Needs: safety-3
Baseline: 929 occurrences in total (measured 2026-08-01)

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
Baseline: 3 components compile without full separation checking (measured 2026-08-01)

The exemptions — the wasm application build and one test suite — are individually justified
today, but the end-state has none: every component compiles with separation checking.

Done when: no component in `build.mill` overrides its settings to anything weaker than
`settings.sep`.
