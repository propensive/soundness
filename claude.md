# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands
- Build all modules: `mill soundness.all`
- Watch mode for development: `mill -w soundness.all`
- Publish locally: `mill -k __.publishLocal && mill soundness.__.publishLocal`
- Run single test: `mill module.test.run` (e.g., `mill abacist.test.run`)

## CI (local-attested)
CI runs locally inside Docker; GitHub Actions only verifies a GPG signature.
- Run CI + sign: `etc/ci-local` (builds `etc/ci-docker`, runs the build/tests,
  then signs the canonical manifest from `etc/ci-manifest` with GPG and stores
  the detached signature as a git note on `refs/notes/ci-verified`).
- Push: `git push origin HEAD refs/notes/ci-verified` (the notes ref must be
  pushed for the remote workflow to find the signature).
- Verify locally: `etc/ci-verify`.
- Trusted signer keys: GitHub usernames listed in `etc/ci-maintainers`; the
  workflow fetches `https://github.com/<user>.gpg` for each at verify time.

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