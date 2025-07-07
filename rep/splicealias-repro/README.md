# splicealias — calibrated self-contained repro

Repro for compiler fix #2 (`feature/<ver>/splicealias`): when a pickled quote's type holes are
spliced at inline expansion, `PickledQuotes.spliceTypes` substitutes binder occurrences via a
TreeTypeMap, but an anonymous class whose *declarations* (not parents) mention the binder has an
unchanged ClassInfo, is not copied, and its members keep referring to the binder — whose pickled
placeholder info is `TypeAlias(Any)`. Only capture checking's recheck re-reads those members, so
the alias dealiases to `Any` and refinement-member conformance fails. Full diagnosis in
rep/DECISIONS.md.

Two-pass compile (macro without CC, use-site with `-experimental -Ycc-new` and the
`captureChecking` language import):

    rep/splicealias-repro/check.sh stock:3.8.4                                   # → RED
    rep/splicealias-repro/check.sh <release>/bin/scalac                          # fork → GREEN

## Calibration + verdict matrix (2026-07-06)

| compiler                                            | result | note |
|-----------------------------------------------------|--------|------|
| stock 3.8.4 (coursier)                              | RED    | `Found: Object with repro.TC {...}^'s1, Required: repro.TC{type Self = String}` — the adversaria/vexillology signature |
| fork `feature/3.8.4/make-unboxedpure-splicealias`   | GREEN  | soundness-384 build |
| stock 3.9.0-RC1 (coursier)                          | RED    | same signature — `PickledQuotes.scala` byte-identical to 3.8.4's |
| fork `all/3.9.0-RC1` (with cherry-picked fix)       | GREEN  | |
| fork `all/main` (upstream fix #25245, no splicealias)| GREEN | upstream PR #26307 (`6d557cf587`, 2026-06-25) fixes the root cause by cloning the decls scope so `mapSymbols` copies the class; landed after the 3.9.0-RC1 cut |
