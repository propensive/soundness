# Soundness Module Layout Standards

_(Stub — scope for review; full content to follow.)_

This standard will define how a Soundness module is laid out on disk and how its
namespace is structured. It will cover these conventions:

```tabular
column  Place
column  Holds
row  `src/core`  the library
row  `src/test`  tests
row  `src/bench`  benchmarks
row  `src/<variant>`  optional integrations
row  `<module>.<Type>.scala`  one top-level type, named for it
row  `<module>_core.scala`  package-level givens and extensions, companion-first
```

It will also describe the `soundness_<module>_core.scala` re-export that lifts a
module's public surface into the umbrella `soundness` package, and the rules for
what is exported versus kept internal. Some of this is already touched on in
Consequent Style's Locatability rules (`L1`–`L5`, see [`syntax.md`](syntax.md));
this standard will gather the structural conventions in one place.
