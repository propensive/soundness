# Agent instructions for Soundness

Read `.claude/CLAUDE.md` first: it covers the build, code style, the commit and PR workflow,
and the local-CI attestation scheme. This file adds the rules for developing several PRs at
once, and for recording every material change in the migration notes.

## Stack concurrent PRs; never develop them side by side

When more than one PR is in flight, each new PR branch must be **based on the branch of the
previous open PR**, not on `main`, forming a single linear stack:

```
main ── A ── B ── C
```

Open the PR for `B` against `A`'s branch, `C` against `B`'s, and so on. GitHub retargets each
PR to `main` automatically when the branch beneath it merges and is deleted.

### Why: attestations are keyed by content

The `Build` check verifies a signed attestation that `make attest` attaches to the **filtered
git tree** of the commit (the tree with `.dockerignore`-excluded paths removed). A commit is
attested if some attested build produced exactly that tree.

- In a **stack**, attesting `B` builds `main + A + B`. When `A` is squash-merged from an
  up-to-date branch, `main`'s tree becomes `A`'s tree, so rebasing `B` onto the new `main` yields
  the *same* tree `B` already had. `B` merges on its existing attestation. Each PR in the stack
  is attested exactly once, and they merge bottom-up with no rebuilds.
- Developed **side by side** from `main`, `A` and `B` are each attested against `main` alone.
  Once `A` merges, `B` must be rebased, which produces a tree nobody has built, so `B` needs a
  full `make attest` again before it can merge. With `n` concurrent PRs that is up to `n - 1`
  extra attestations, each a from-scratch build and test run.

The same reasoning is why a branch must be rebased onto the current `origin/main` immediately
before merging: a squash of a stale branch produces a tree that was never built and has no
attestation.

### Rules for working in a stack

1. Start every new branch from the top of the stack (the newest open PR branch), after
   confirming that branch is itself based on the current `origin/main`.
2. Merge from the bottom up. Do not merge `B` before `A`.
3. Amending a lower PR invalidates everything above it: rebase each higher branch onto the
   amended one, in order, and re-attest each. Prefer finishing and merging lower PRs over
   revising them.
4. After a lower PR merges, rebase the next branch onto `origin/main`. Its filtered tree should
   be unchanged; `make attest` will report an existing attestation and exit without building.
   If it starts a build instead, something in between changed the content, and the rebuild is
   genuinely required.
5. Keep each PR's own diff reviewable: the PR base is the branch beneath it, so the diff shows
   only that PR's changes.

## Record every material change in `doc/migration/pending.md`

Downstream code is upgraded across Soundness releases by an LLM agent that reads one file per
release, `doc/migration/<version>.md`, and applies what it describes. `doc/migration.md`
explains the scheme. Since the 0.64.0 release, every PR contributes to the next file.

### When an entry is required

A PR must add an entry for every change a consumer of a Soundness library could observe:

- renames of packages, modules, types, members, givens, extension methods or annotations;
- moves between packages or modules, including changes to which module or import provides
  something, and changes to artifact or module names;
- signature changes: parameters added, removed, reordered or retyped; result types; type
  parameters; changed `inline`/`transparent`/`erased`/`using` status; new required givens;
- removals and deprecations, with their replacement if one exists;
- changed defaults, changed semantics, changed error types, changed ordering, changed
  formatting of output, changed thread-safety or laziness, and any other behaviour change that
  could alter what existing downstream code does;
- changes to compiler flags, Scala version, or build requirements that consumers must match.

No entry is needed for changes with no observable effect on consumers: formatting, comments,
performance work that preserves behaviour, tests, benchmarks, internal refactoring, and CI or
documentation changes. Purely additive API is optional; record it only when it supersedes an
existing way of doing something.

### How to write an entry

The reader is an intelligent LLM capable of complex refactoring, not a person. Optimise for
precision and density, not readability:

- State **what** changed, exactly. Give fully-qualified old and new names, complete old and
  new signatures, and exact old and new semantics. Do not describe motivation.
- Leave **how** to adapt to the reader, unless the adaptation is not deducible from the change
  itself (for example, a semantic change that requires call sites to be audited for a
  particular pattern). Then state the condition under which code must change.
- One entry per change; never merge distinct changes into one entry. Group entries under a
  `## <module>` heading per library, appending within the group. Reference the PR number.
- Be exhaustive within the change: if a rename applies to a family of members, list every
  member, not "and similar".
- If a change is reverted before release, delete its entry rather than adding a counter-entry.

A representative entry:

```
## gossamer

- `gossamer.Text#sub(from: Text, to: Text): Text` renamed to `replace`; `sub` removed.
  Behaviour unchanged. (#1900)
- `gossamer.Text#cut(separator: Text)` now returns `proscenium.List[Text]` instead of
  `scala.collection.immutable.List[Text]`; it also no longer drops a trailing empty
  element, so `t"a,b,".cut(t",")` yields three elements where it previously yielded two.
  (#1904)
```

### Rules

1. Write only to `doc/migration/pending.md`. Never edit a released `doc/migration/<version>.md`
   except to correct an error in it.
2. Add the entry in the same PR as the change, in the same commit as the change or a later one.
   `doc/migration/**` is outside the CI input set, so adding or amending an entry does not
   invalidate the PR's attestation.
3. On release, the release PR renames `doc/migration/pending.md` to
   `doc/migration/<version>.md` (`git mv`) and creates a fresh `pending.md` containing only its
   header. The release process is not complete until this has merged.
4. Reviewing a PR includes checking that `pending.md` covers every observable change the diff
   makes.
