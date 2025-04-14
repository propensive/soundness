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