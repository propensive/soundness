# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands
- Build all modules: `mill soundness.all`
- Watch mode for development: `mill -w soundness.all`
- Publish locally: `mill -k __.publishLocal && mill soundness.__.publishLocal`
- Run single test: `mill module.test.run` (e.g., `mill abacist.test.run`)

## Code Style
- Scala 3.6.1 with advanced language features (experimental modularity, nulls, generics)
- CamelCase for classes, camelCase for methods, lowercase for packages
- Wildcard imports (e.g., `import fulminate.*`)
- Errors as subclasses of `fulminate.Error` with descriptive names
- Use `Optional` from `vacuous` module instead of `Option` (with `Unset` instead of `None`)
- Heavy use of extension methods (often marked as `inline` or `transparent inline`)
- Tests extend `Suite(name)` class and implement `run()` method
- Test assertions use fluent pattern (`.assert(_ == expectedValue)`)
- Strong emphasis on compile-time type safety and immutability

## Workflow

### Before the first commit of any task
- Never commit while on `main` or in detached-HEAD state. Stop and create a new branch first.
- The new branch must be based on the branch of the open PR at the top of the stack (not `main`). Use `gh pr list` / `gh pr view` to identify the top-of-stack PR and its head branch, then `git checkout -b <new-branch> <top-of-stack-branch>`.
- If there are no open PRs in the stack, branch from `main`.

### Commits
- Commit incrementally as you edit. The bar is *incremental compilation passes* for the changed code — full clean compilation is not required per commit. If a change leaves the code uncompilable, keep editing until incremental compilation succeeds, then commit.
- Before each commit, propose the commit message and pause so the user can review the staged changes (and may decide to amend them).
- Push commits as soon as they're made.

### Commit messages
- Subject: imperative one-line summary (≤ 70 chars).
- Body (optional): explanation of *why*, not *what*. Markdown is fine.

### Pull requests
- Open as **draft** until ready for review.
- Title is a clear one-line description of the work.
- Body follows `.github/pull_request_template.md`: a single summary paragraph, a blank line, then Markdown release notes for users (with code examples if useful).
- Whenever a new commit is added to a PR, re-read the PR description and update it if it no longer accurately describes the full set of commits.

### Stacked PRs
- A new PR is always stacked on top of the highest open PR in the current stack, so the stack merges in order from the bottom up. Branch from the top-of-stack branch, not from `main`.
- Use `git-branchless` (`git smartlog`, `git restack`, `git move`) to keep the stack consistent as commits are added to or rebased onto branches lower in the stack.