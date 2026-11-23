# Agent instructions for Soundness

Read `.claude/CLAUDE.md` first: it covers the build, code style, the commit and PR workflow,
and the local-CI attestation scheme. This file adds the rules for developing several PRs at
once, for recording every material change in the migration notes, and for the pins in
`etc/refs` and `etc/tools`.

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

## The documentation is an Orthant store

The documentation — the tutorials and API overlays of `doc/soundness/`, their translations,
and the philosophy, standards, roadmap and errors sections — is held in the
[Orthant](https://github.com/propensive/orthant) store in `.orthant/`. The store has a
`language` axis (`en`, with `fr` and the other translations beneath it) and a `release` axis
that follows the release tags (from 0.76.0), whose last value, `main`, holds what is not yet
released.

`doc/` is the store's **live tree**: the English at `main`, as ordinary files in git, which are
edited where they are. Orthant never writes to them. Every other point — a translation, or the
English of a release — is in the store alone, and is edited through `orthant edit`, or in the
store's working tree, `.orthant/work/tree/`, which git ignores.

- To change the English for the next release, edit the file in `doc/`, then run `orthant tag`.
  It stores `doc/` in the store at `main`, and says which translations it has left with lines
  to translate or passages to check. Commit `doc/` and `.orthant/` together: a PR that changes
  the documentation changes both.
- `orthant status` shows what in `doc/` is not yet stored, and what storing it would leave to
  do; `orthant tag --dry-run` names the points whose content would change, and stores nothing.
- The scripts in `etc/` and the Makefile targets that read the documentation read `out/doc`, the
  English at `main` that `make docs` exports from the store (`build`, `doccheck` and `plumbing`
  run it first). `make docs` fails while `doc/` differs from the store, so an edit that was
  not stored with `orthant tag` is caught there.
- `orthant todo fr main` shows what has yet to be translated, and the translations of text that
  has since changed.
- Do not run `orthant tag <name>`: a release here is a git tag, which `orthant sync` takes up.

An edit stored at a point is seen at every point below it (later releases, and the
translations and dialects of its language) which has not changed the same lines. Each kind of
change is a choice of where to store the edit and how far it reaches; `.orthant/readme.md` has
a worked recipe for each:

| The change                                         | How                                                          |
|----------------------------------------------------|--------------------------------------------------------------|
| New or changed text for the next release           | edit it in `doc/`, then `orthant tag`                        |
| A removed or renamed document                      | delete or move it in `doc/`, then `orthant tag --remove`     |
| A correction to text that was already released     | `orthant edit <path> en 0.76.0`, then `orthant export doc en main` |
| A correction to one release, and no later one      | `orthant checkout en 0.76.0`, `orthant checkin --only release` |
| Translating, or revising a stale translation       | `orthant edit <path> fr`, or `orthant todo fr --checkout` and `orthant checkin` |
| An English edit which leaves the translation right | `orthant tag`, then `orthant confirm <path>` in the `fr` checkout |
| A new language                                     | `orthant axis value language de en`, `orthant todo de --checkout` |
| A dialect                                          | `orthant axis value language en/US en`, `orthant checkout en/US main` |
| After a pull or rebase that changed `.orthant/`    | `orthant update`, if a point is checked out                  |

A correction stored at a release reaches `main` in the store, but not the files in `doc/`:
`orthant tag` first, so that `doc/` has nothing unstored, make the correction, and then
`orthant export doc en main` writes the corrected English over `doc/`. Without that, the next
`orthant tag` would store the uncorrected text again.

## Record every material change in the next release's migration notes

Downstream code is upgraded across Soundness releases by an LLM agent that reads one file per
release, `etc/migration/<version>.md`, and applies what it describes. `etc/migration.md`
explains the scheme. Since the 0.64.0 release, every PR contributes to the next file: the
unreleased `etc/migration/<version>.md`, named from the start after the release it will be part
of, which is the one file in `etc/migration/` whose version has no tag.

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
documentation changes. Purely additive API — a new method, type, given, package or module
alongside the existing ones — is **not** recorded: downstream code that does not use it has
nothing to change. When an addition supersedes an existing way of doing something, the entry
belongs to the deprecation or removal of the old way, and names the addition as its replacement.

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

1. Write only to the unreleased `etc/migration/<version>.md`. The notes are ordinary files in
   git, outside the documentation and its Orthant store: they are written for an agent to
   apply, and are not translated. Never edit a released one
   except to correct an error in it. If every file there has been released, create the next,
   for the next minor version (`0.71.0.md` after 0.70.0), headed `# Changes since <previous>`.
2. Add the entry in the same PR as the change, in the same commit as the change or a later one.
   `.orthant/` is outside the CI input set, so adding or amending an entry does not
   invalidate the PR's attestation.
3. A release needs no preparatory PR: tagging `X.Y.Z` releases `etc/migration/X.Y.Z.md` as it
   stands. The release refuses to run if `X.Y.Z.md` is missing or another version's notes are
   unreleased, so to release under a different version than the file names (1.0.0 rather than
   0.71.0, say), rename it with `git mv` in any PR and merge that before tagging.
4. Reviewing a PR includes checking that the unreleased notes cover every observable change the
   diff makes.

## Dependencies in `etc/refs`, tools in `etc/tools`

Soundness sits at the bottom of the stack and depends on nothing propensive: `etc/refs`, where
a repository pins the libraries its jars are compiled against, is empty here. What Soundness
*runs* is pinned in `etc/tools`: the flare compiler plugin, `dev.propensive:flare-plugin`, which
every library compiles with (`-P:flare:` options in `flareToolchain`), and fume, which runs the
tests. The `xek` *builder* is pinned separately, by version and SHA-256, in `etc/xek.tsv`.
Downstream — Pyrocosm, then fume, flame and flare — pins Soundness in its `etc/refs`.

A tool is always a **release**, never a snapshot. A tool appears in no POM, so it is not part
of the transitive closure and does not gate a release; that is what keeps the release graph
free of cycles (flare depends on Pyrocosm, which depends on Soundness — were the plugin a
dependency of Soundness, none of the three could be released first).

### Rules

1. Never satisfy the plugin pin with a `publishLocal` or a snapshot from a flare checkout: the
   build then compiles against bytes CI cannot see, and `deps.py` rejects a snapshot in
   `etc/tools` anyway. A plugin change Soundness needs is *released* from flare first, and the
   pin bumped to that release. The build resolves the plugin (and the Proscala compiler) straight
   from their GitHub releases through crux (propensive/crux), so nothing is installed locally;
   `make tools` installs the commands. A stale `~/.ivy2/local/dev.propensive` from the earlier
   `sync-deps` flow shadows the releases — remove it.
2. The release runs `deps.py check`, which confirms every pin in `etc/refs` is a published
   release, transitively, and every tool in `etc/tools` a published release.
3. `make snapshot` publishes *this* commit's jars (all six hundred, at `<next minor>-<hex>`)
   for Pyrocosm to pin in its `etc/refs` when a Pyrocosm PR needs an unreleased Soundness. It
   needs a clean checkout of a commit already on GitHub, and a full build: the same build
   `make attest` does, so prefer running it right after attesting. It is not a release and
   needs no migration notes; `make snapshot-prune` deletes old ones.
4. `etc/github-ref` pins the commit of propensive/.github whose scripts run through
   `etc/shared`; bump it deliberately, in a one-line commit. `etc/github-ref` is outside the
   attestation input set, so the bump alone needs no re-attestation.

The whole flow, and the scripts, are documented in the README of
[propensive/.github](https://github.com/propensive/.github).

## Releasing

A release is cut by tagging, and by nothing else:

```sh
git tag -s X.Y.Z && git push --tags
```

There is no version to bump: `publishVersion` is driven from the tag through
`SOUNDNESS_RELEASE_VERSION`, and nothing must merge first: the migration notes are already
named `etc/migration/<version>.md` (above), so any green commit on `main` can be tagged with the
version its notes name. The tag fires
`.github/workflows/release.yml`, which runs the shared `release.sh` in propensive/.github.
Never publish by hand, and never create a release or upload an asset with `gh`.

After the release is published, run `orthant sync` and commit the change to
`.orthant/config.tel`: it adds the tag as a value of the `release` axis, makes everything
checked in at `main` that release's, and starts `main` again beneath it.

It gates before anything is published: the tag must be signed and verified by GitHub; CI must
*already* be green on that exact commit — which is what verifies the attestation note, so the
trust boundary is unchanged, the runner only checks a signature it could not have produced;
the migration notes must be finalised; `groupCheck.validate` and `verify-attest.sh` must pass;
and `deps.py check` must find every pin a published release. Only then are the six hundred jars
staged, uploaded into a draft in batches, checked digest by digest, and the draft published. If
a later step fails, the release **and** the tag are deleted from origin, so the retry is
`git tag -d X.Y.Z && git tag -s X.Y.Z && git push --tags`.

What this repository needs beyond the common path — the two-step JVM-then-Scala.js compile, the
`publishVersion` probes, the migration directory, the two gate commands — is declared in
`etc/release`, which `.dockerignore` keeps out of the CI input set, as it does `etc/github-ref`:
it configures the release, not the build. The release notes are generated: the **Changes**
section is built from the body of every pull request merged since the previous tag, which is
what `pull_request_template.md` asks each PR for, and a **Migration** section links
`etc/migration/<version>.md`. A hand-written overview can be added as `doc/notes/<version>.md`,
which is optional and ungated.
