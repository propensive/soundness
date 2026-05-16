# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- Build all modules: `mill soundness.all`
- Run all tests: `make test`
- Watch mode for development: `mill -w soundness.all`
- Run single test: `mill module.test.run` (e.g., `mill abacist.test.run`)
- Run the full CI test suite in Docker and sign the result: `make attest` (see "CI workflow" below)

## Code Style

- Scala 3.8.3 with advanced language features (experimental modularity, null safety)
- CamelCase for classes, camelCase for methods, lowercase for packages
- Avoid abbreviations in identifiers, except well-established conventions
- Wildcard imports (e.g., `import fulminate.*`)
- Errors as subclasses of `fulminate.Error` with descriptive names
- Use `Optional` from `vacuous` module instead of `Option` (with `Unset` instead of `None`)
- Heavy use of extension methods (often marked as `inline` or `transparent inline`)
- Tests extend `Suite(name)` class and implement `run()` method
- Test names should be descriptive, but under 70 characters
- Test assertions use fluent pattern (`. assert(_ == expectedValue)`)
- Strong emphasis on compile-time type safety and immutability

## Workflow

### Before the first commit of any task

- Never commit while on `main` or in detached-HEAD state. Stop and create a new branch first.
- Whenever you create a new branch, make sure `main` is up-to-date with `origin/main`.

### Commits

- Commit incrementally as you edit. The bar is _incremental compilation passes_ for the code — full clean compilation is not required per commit, and tests don't need to compile or pass. If a change leaves the code uncompilable, keep editing until incremental compilation succeeds, then commit.
- Before each commit, always pause and propose the commit message and pause so the user can review the staged changes (and may decide to amend them).
- Push commits as soon as they're made.

### Commit messages

- Subject: imperative one-line summary (≤ 70 chars).
- Body (optional): explanation of _why_, not _what_. Markdown is fine.

### Pull requests

- Check that the tests compile and pass before opening a PR, by running `make attest` (which builds and runs the suite in Docker and signs the result).
- Let the user approve the text before the PR is opened.
- Open as **draft** until ready for review. Set the PR to be auto-mergeable.
- Immediately before opening the PR, make sure the branch is based upon the current `origin/main` before opening the PR, and rebase if necessary.
- After pushing the branch, push attestation notes with `git push origin refs/notes/ci-attestation` (or use `make push` instead of plain `git push`).
- Title is a clear one-line description of the work.
- Body follows `.github/pull_request_template.md`: a single summary paragraph, a blank line, then Markdown release notes for users (with code examples if useful).
- Whenever a new commit is added to a PR, re-read the PR description and update it if it no longer accurately describes the full set of commits.
- PRs from external contributors do not have valid attestations. To merge them, pull the branch locally, run `make attest`, and merge locally with `make push`.

### CI workflow

This repository runs CI locally and verifies signed attestations on GitHub Actions. The slow build/test work happens in Docker on the developer's machine; the GitHub `Build` check only verifies the attestation note.

- The CI input set is the Docker build context of `img/test`, controlled by `.dockerignore`. Anything inside it changes the input digest.
- `make attest` builds and runs the test suite in `img/test`. On success, it computes the input digest, signs it with SSH (`ssh-keygen -Y sign`), and attaches a JSON envelope (in-toto Statement v1 + signature) as a git note in `refs/notes/ci-attestation` on HEAD.
- If only files outside the input set changed (docs, `.github/`, `.claude/`, etc.), `make attest` re-uses the existing attestation rather than rebuilding.
- `make verify-attest` is the local dry-run of what GitHub Actions does. It does not invoke Docker.
- `make push` pushes commits and the attestation notes ref together. Plain `git push` works too but you must also `git push origin refs/notes/ci-attestation`, otherwise the `Build` check will fail with "no attestation note".
- Required tooling locally: Docker (Desktop / Colima / dockerd), Python 3, `ssh-keygen`. The signing key defaults to `~/.ssh/id_ed25519`; override with `SOUNDNESS_CI_KEY=…`.
- The signer's email (from `git config user.email`) must appear in `.ci/allowed_signers` with their SSH public key. Adding a co-signer = a PR that adds a line.
