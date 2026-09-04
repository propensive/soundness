# Agent instructions for Soundness

Read `.claude/CLAUDE.md` first: it covers the build, code style, the commit and PR workflow,
and the local-CI attestation scheme. This file adds the rules for developing several PRs at
once.

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
