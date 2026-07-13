# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- Build all modules: `mill soundness.all`
- Run all tests: `make test`
- Watch mode for development: `mill -w soundness.all`
- Run single test: `mill module.test.run` (e.g., `mill abacist.test.run`)
- Run the full CI test suite from a clean build and sign the result: `make attest` (see "CI workflow" below)

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

- A PR may **only** be opened after `make attest` has completed successfully and the corresponding signed attestation note has been pushed to `refs/notes/ci-attestation` (use `make push`, or `git push origin refs/notes/ci-attestation` after a plain `git push`). The GitHub `Build` check verifies that note; opening a PR without one will leave the check red.
- Let the user approve the title and body text before the PR is opened.
- Open as **ready for review** (not draft). Enable auto-merge so the PR merges as soon as the `Build` check passes.
- Immediately before opening the PR, make sure the branch is based upon the current `origin/main`, and rebase if necessary.
- Title is a clear one-line description of the work.
- Body follows `.github/pull_request_template.md`: a single summary paragraph, a blank line, then Markdown release notes for users (with code examples if useful).
- Whenever a new commit is added to a PR, re-read the PR description and update it if it no longer accurately describes the full set of commits. Each new commit also requires a fresh `make attest && make push` before the `Build` check can pass.
- PRs from external contributors do not have valid attestations. To merge them, pull the branch locally, run `make attest`, and merge locally with `make push`.

### CI workflow

This repository runs CI locally and verifies signed attestations on GitHub Actions. The slow build/test work happens on the developer's machine; the GitHub `Build` check only verifies the attestation note.

- The Scala compiler is the `propensive/proscala` fork, downloaded from its GitHub releases — no local compiler build is required. `build.mill`'s `object toolchain` fetches the `SOUNDNESS_SCALA_RELEASE` tag (default `3.9.0-RC1-p1`) into `~/.cache/soundness/proscala/<tag>/lib` (checksum-verified, outside `out/`), then serves it as a Maven-layout repository view. The first build needs network access; the cache is reused offline thereafter, including by the throwaway-worktree attest build. Set `SOUNDNESS_SCALA_HOME` to a locally-built `release` directory to use that instead (fork developers). The toolchain lives outside the CI input digest, so switching it does not invalidate attestations, but a `./mill clean` is needed after switching.
- The CI input set is controlled by `.dockerignore` (reused as the input-set definition): everything *not* excluded there is part of the input digest. Anything inside it changes the input digest.
- `make attest` does a full, from-scratch build and runs the test suite in a throwaway git worktree checked out at HEAD (clean build cache every time; the committed tree, not the working tree). On success, it computes the input digest, signs it with SSH (`ssh-keygen -Y sign`), and attaches a JSON envelope (in-toto Statement v1 + signature) as a git note in `refs/notes/ci-attestation` on HEAD. Set `SOUNDNESS_CI_SKIP_BUILD=1` to skip the build when you know the inputs are unchanged.
- The attest build runs mill with `--no-daemon` (each invocation is its own short-lived JVM, so concurrent attests/`mill -w` on the same machine never collide, and no `mill shutdown` is needed) and `-j 6` (caps concurrent compilers to bound peak heap; as fast as `-j 12` on a clean build but ~1.5 GB lower). Memory is sized for a 24 GB box: mill heap `-Xmx8g` (`.mill-jvm-opts`), test heap `-Xmx4g` (`run-tests.sh`; measured peak ~2.5 GB). Override with `SOUNDNESS_CI_JOBS` / `SOUNDNESS_CI_TEST_HEAP`.
- If only files outside the input set changed (docs, `.github/`, `.claude/`, etc.), `make attest` re-uses the existing attestation rather than rebuilding.
- `make verify-attest` is the local dry-run of what GitHub Actions does.
- `make push` pushes commits and the attestation notes ref together. Plain `git push` works too but you must also `git push origin refs/notes/ci-attestation`, otherwise the `Build` check will fail with "no attestation note".
- Required tooling locally: a JDK, `curl` and network access for the first toolchain download (see above), plus the userland the suite exercises (shells like zsh/fish/tmux, and PowerShell/Zig/Rust for the modules that shell out to them), Python 3, and `ssh-keygen`. The signing key defaults to `~/.ssh/id_ed25519`; override with `SOUNDNESS_CI_KEY=…`.
- The signer's email (from `git config user.email`) must appear in `.ci/allowed_signers` with their SSH public key. Adding a co-signer = a PR that adds a line.
