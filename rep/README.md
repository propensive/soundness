# `rep/` — capture-checking reproductions: the compiler-fix queue

Isolated, **verified** reproductions of each class of capture-checking or separation-checking
failure that Soundness has hit. Soundness is built with the Proscala fork by design, so a class
that cannot be fixed at source is fixed in the compiler — and its reproduction here is the
queue entry, the regression test, and (for the self-contained ones) the upstream report.
`STATUS.md` is the current state of every case; `DECISIONS.md` is the running log of how each
was diagnosed and what was decided.

## The one rule: never trust Mill for capture checking

Mill's incremental compilation manufactures false greens (and false reds) under capture
checking: it under-runs the checker's multi-pass capture estimation, so a module that fails a
clean build can compile incrementally. Every reproduction here is therefore checked with a
**single-shot** compile of the pinned toolchain:

```bash
rep/toolchain.sh -version                 # the compiler build.mill compiles with
rep/compile.sh <case>                     # compile one case and show its error
rep/sepcheck-probes/check.sh              # the probe suite (every .pos green, every .neg red)
```

`rep/toolchain.sh` runs `dotc` from the release `build.mill` pins (`settings.scalaRelease`, cached
under `~/.cache/soundness/proscala/<tag>/lib` by any build), with the fork's opt-in repairs — the
`-Z` flags of `settings.scalaOptions`, read from `build.mill` so the two cannot drift. A fix that
is opt-in shows RED without its flag, so the flags are never left to the caller:

```bash
REP_STOCK=1 rep/compile.sh <case>                            # the same compiler, no -Z repairs
SOUNDNESS_SCALA_RELEASE=3.10.1-dev-p17 rep/compile.sh <case> # the other stream
rep/compile.sh --stock <case>                                # a self-contained case under the
                                                             # stock compiler its header names
```

## Three kinds of case

**Self-contained** (the source starts with `//> using`): no Soundness dependency; the header's
`//> using options` line carries the flags. These are the gold standard and the ones to hand
upstream: `case2-directmint`, `case2-freshvar`, `proxy-tagged`.

**Calibrated** (the directory has a `check.sh`): several single files compiled in a fixed order
to model separate compilation or to pair a failing shape with a passing control — `handler-raises`
(three passes, one of them not capture-checked), `stacked-raises`, `splicealias-repro`,
`shared-unscoped`. Each prints GREEN/RED per file with the header's expectation.

**Soundness-backed** (`cp.txt`/`opts.txt` present): the error only arises from the real
Soundness type graph — the `raises` encoding, Wisteria derivation, an opaque behind a
typeclass, a macro — so the minimal source lives in the module's own package and is compiled
with `dotc` directly against the module's captured classpath:

```bash
rep/capture-classpath.sh <case> <module>   # once per machine: Mill builds the module's test
                                           # classpath and writes cp.txt/opts.txt (gitignored)
rep/compile.sh <case>
```

`rep/probe-suite.sh <module>` and `rep/probe-core.sh <module.target>` do the same for a whole
test suite or component — a single-shot compile of its sources against a freshly-captured
classpath — which is how a module is judged before `build.mill` flips it.

## The probe suite

`sepcheck-probes/` characterises what the separation checker can and cannot express
(`Mutable`/`update`, borrows, `consume`, fresh results, untracked fields, pure typeclasses,
and — P15/P16 — shared versus exclusive tactics and the classifier lattice). Its `README.md`
records the finding each probe establishes. A `.pos` probe must compile; a `.neg` probe must
fail with every `//EXPECT:` regex matched.

## Adding a case

One directory per class. The source's header comment says what the pattern is, which error it
produces, where in Soundness it bites, and **what we want** to happen. Reduce to self-contained
form if the class survives reduction (bisect the ingredients; the case-2 headers show the
method); otherwise capture a classpath. Record the outcome in `STATUS.md` and the diagnosis in
`DECISIONS.md`. A hatch left in `lib/` for this class cites the case by directory name in its
comment, which is how the escape census groups the residue by blocker.
