# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- Build all modules: `mill soundness.all`
- Run all tests: `make test`
- Watch mode for development: `mill -w soundness.all`
- Run single test: `mill module.test.run` (e.g., `mill abacist.test.run`)

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

- Check that the tests compile and pass before opening a PR
- Let the user approve the text before the PR is opened.
- Open as **draft** until ready for review. Set the PR to be auto-mergeable.
- Immediately before opening the PR, make sure the branch is based upon the current `origin/main` before opening the PR, and rebase if necessary
- Title is a clear one-line description of the work.
- Body follows `.github/pull_request_template.md`: a single summary paragraph, a blank line, then Markdown release notes for users (with code examples if useful).
- Whenever a new commit is added to a PR, re-read the PR description and update it if it no longer accurately describes the full set of commits.
