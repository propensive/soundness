# Soundness Module Layout Standards

_(Stub — scope for review; full content to follow.)_

This standard will define how a Soundness module is laid out on disk and how its
namespace is structured. It will cover the directory convention (`src/core` for the
library, `src/test` for tests, `src/bench` for benchmarks, with `src/<variant>` for
optional integrations), the file-naming rule that each top-level type lives in a
file named for it (`<module>.<Type>.scala`), and where package-level givens and
extensions belong (the `<module>_core.scala` file, with companion-first ordering).
It will also describe the `soundness_<module>_core.scala` re-export that lifts a
module's public surface into the umbrella `soundness` package, and the rules for
what is exported versus kept internal. Some of this is already touched on in
`syntax.md` §2; this standard will gather the structural conventions in one place.
