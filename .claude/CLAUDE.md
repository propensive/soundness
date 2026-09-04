# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- Build all modules: `mill soundness.all`
- Run all tests: `make test`
- Watch mode for development: `mill -w soundness.all`
- Run single test: `mill module.test.run` (e.g., `mill abacist.test.run`)
- Compile every benchmark module: `mill benches.compile` (see "Benchmarks" below)
- Run the full CI test suite from a clean build and sign the result: `make attest` (see "CI workflow" below)

## Benchmarks

- Each library's benchmarks live in `lib/<name>/src/bench` and are declared as `object bench
  extends Benchmarks(…)` in `build.mill`; the cross-format corpus is the top-level `bench` module
  over `src/bench`, run with `make bench`.
- **Benchmarks must keep compiling.** They are *not* in `soundness.all`, the `test` aggregate, or
  `make attest`, so a library change that breaks one is otherwise invisible — that is exactly how
  all 11 of them came to rot behind the opaque-collections migration. The `benches` aggregate
  module exists to make them visible: `make build` runs `mill benches.compile`, so run `make
  build` (not just `mill soundness.all`) after any change to a collection API, a core type's
  surface, or a `Benchmarks(…)` module's dependencies.
- A new benchmark module needs no registration: `benches` collects every `Benchmarks` child of
  every library automatically. A `bench` object with no `src/bench` directory compiles vacuously
  and proves nothing — `zeppelin.bench` is currently in that state.
- Benchmarks compare Soundness against third-party rivals, so a bench file mixes two worlds. Keep
  the rival side written as that library's own users would write it: mirror records declared with
  the stdlib `List`/`Nil` (derivation macros in circe, Jsoniter and borer cannot see through
  opaque `proscenium` types), rival monads using their own `flatMap` rather than Soundness's
  `bind`, and conversions crossing back over the greppable `.stdlib` bridge at the boundary.

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

### Given placement (issue #1632)

- A `given` goes in the **companion of its subject type** (or, failing that, the typeclass's
  companion) whenever module layering permits, so it resolves through implicit scope with no
  import. Companion-to-companion placement is equivalent for resolution; pick the companion
  that avoids the unwanted dependency edge.
- A given that **selects an implementation** (a policy, backend, format or style) goes in a
  carefully-named package (`strategies`, `logging`, `alphabets`, `optics`, `teletypeables`,
  `wasiApis`, …) and is imported decisively by name. Mirror the package as a `package <name>:`
  block in the component's `soundness_*.scala` export file; blocks of the same name from
  different libraries merge in the umbrella, so member names must be globally distinct.
- A given that is single-canonical but structurally un-anchorable (subject owned elsewhere,
  union/alias subject, or a platform component whose subject's companion is platform-neutral)
  stays at package top level with a **unique, library-qualified name** (`tarPathOpenable`,
  `terminalStdio`) and a comment explaining why; never a bare generic name, and never
  anonymous — an unnameable given cannot be selectively imported or disambiguated.
- Traps: `import p.*` does NOT import givens (use by-name imports or `given` selectors — a
  sweep that replaces a by-name given import with a wildcard silently drops the given);
  synthesized export forwarders lose capture-annotated refinements (hand-write delegating
  givens, as `soundness_scintillate_server.scala` does); and a lexically-scoped given
  (imported or same-package toplevel) outranks any companion given, which is what makes the
  choice-package pattern override defaults.

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
- `make attest` does a full, from-scratch build and runs the test suite in a throwaway git worktree checked out at HEAD (clean build cache every time; the committed tree, not the working tree). On success, it computes the input digest, signs it with SSH (`ssh-keygen -Y sign`), and attaches a JSON envelope (in-toto Statement v1 + signature) as a git note in `refs/notes/ci-attestation`. Set `SOUNDNESS_CI_SKIP_BUILD=1` to skip the build when you know the inputs are unchanged.
- The note is keyed by HEAD's **filtered tree** (`etc/ci/compute-filtered-tree.sh`: the commit's tree with every `.dockerignore`-excluded path removed), not by the commit SHA. An attestation therefore depends only on the relevant content and survives squash, rebase and amend when the input set is unchanged; `verify-attest.sh` recomputes the filtered tree and looks the note up by it, falling back to a commit-keyed note for attestations made under the old scheme. The catch: squash-merging a branch that is *behind* `main` produces a tree nobody built, which has no attestation and fails the `Build` check on `main` — rebase onto `origin/main` before merging.
- The attest build runs mill with `--no-daemon` (each invocation is its own short-lived JVM, so concurrent attests/`mill -w` on the same machine never collide, and no `mill shutdown` is needed) and `-j 6` (caps concurrent compilers to bound peak heap; as fast as `-j 12` on a clean build but ~1.5 GB lower). Memory is sized for a 24 GB box: mill heap `-Xmx8g` (`.mill-jvm-opts`), test heap `-Xmx4g` (`run-tests.sh`; measured peak ~2.5 GB). Override with `SOUNDNESS_CI_JOBS` / `SOUNDNESS_CI_TEST_HEAP`.
- If only files outside the input set changed (docs, `.github/`, `.claude/`, etc.), the filtered tree is unchanged, so `make attest` finds the existing attestation and does not rebuild.
- `make verify-attest` is the local dry-run of what GitHub Actions does.
- `make push` pushes commits and the attestation notes ref together. Plain `git push` works too but you must also `git push origin refs/notes/ci-attestation`, otherwise the `Build` check will fail with "no attestation note".
- Required tooling locally: a JDK, `curl` and network access for the first toolchain download (see above), plus the userland the suite exercises (shells like zsh/fish/tmux, and PowerShell/Zig/Rust for the modules that shell out to them), Python 3, and `ssh-keygen`. Optionally, `chromedriver` or `geckodriver`: tarantula's live browser tests run when one is on the path and are skipped with a printed notice when neither is, so a browser is not required to attest. The signing key defaults to `~/.ssh/id_ed25519`; override with `SOUNDNESS_CI_KEY=…`.
- The `make wasm-e2e` stage (also run by `make attest`) links the `.wasi` backends into two Wasm components (`tests/wasm/`; `mill wasm.component` exports `wasi:cli/run`, `mill wasm.httpComponent` exports `wasi:http/incoming-handler`), runs their scenarios under wasmtime — the only stage that exercises the WIT ABI at runtime — then packages both as Wasm OCI Artifacts (`mill wasm.image`, `wasm.httpImage`), checks each archive against the artifact layout, and re-runs the component extracted from the archive (the HTTP one under `wasmtime serve`). Regenerating `tests/wasm/*/bindings.scala` after a world changes is a manual step (`wit-bindgen scala tests/wasm/wit --world <name>`); the output is checked in. **Known generator bug:** for an exported function taking an imported resource, `wit-bindgen scala` types the parameter as its own generated facade while the backend casts the incoming handle to the canonical `scala.scalajs.wasi.…` one, so the guest links cleanly and then traps with `wasm trap: cast failure` on the first call. Substitute the `scala.scalajs.wasi.…` types by hand, as `tests/wasm/http/bindings.scala` does and documents. It additionally requires `wasmtime` (tested with 46.x; needs the `function-references`, `gc` and `exceptions` proposals), `wasm-tools`, and the **scala-wasm fork** of `wit-bindgen` (`cargo install --git https://github.com/scala-wasm/wit-bindgen --tag scala-wasm-wasm.4 wit-bindgen-cli`; the crates.io release lacks the `scala` generator). All scenarios are offline and deterministic; set `SOUNDNESS_CI_ONLINE=1` to also run the outgoing-HTTP scenario.
- The signer's email (from `git config user.email`) must appear in `.ci/allowed_signers` with their SSH public key. Adding a co-signer = a PR that adds a line.
