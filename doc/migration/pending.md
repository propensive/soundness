# Changes since 0.69.0

This file is read by an LLM agent to upgrade code that consumes Soundness libraries from
0.69.0 to the next release. Each entry states precisely what changed; see `AGENTS.md` for the
format. Entries are grouped by module, most-recently-added last within a module.


## probably

- `probably.Test#aspire(predicate: test => Boolean): Unit` and `probably.Test#aspire(): Unit`
  (extensions on `Test[test]^`) removed. Replacement: the new top-level
  `probably.aspirationally[report, result](using runner: Runner[report])(block: Runner[report] ?=> result): result`
  (also exported as `soundness.aspirationally`); every assertion (`assert`, `check`, `matches`,
  spreads) evaluated within `block` records `Verdict.AspirePass`/`Verdict.AspireFail` in place
  of its usual verdict. `test(name)(body).aspire(p)` becomes
  `aspirationally(test(name)(body).assert(p))`, or the test (or an enclosing `suite(…)`)
  indented under `aspirationally:`. `assert` requires a pure body in a capture-checked unit,
  which `aspire` did not; an aspiration whose body captures a capability in such a unit uses
  `check` instead. (#PR)
- `probably.Spread#aspire[report](predicate: (value, result) => Boolean)(using Runner[report], Inclusion[report, Verdict], Inclusion[report, Verdict.Detail]): Unit`
  and `probably.Spread2#aspire[report](predicate: (left, right, result) => Boolean)(using …): Unit`
  removed; call `assert` within `aspirationally` instead. (#PR)
- `probably.Runner` changed from a class to a trait, `trait Runner[report] extends Findable`,
  with the new member `def aspirational: Boolean` (default `false`). Construct one with
  `probably.Runner[report](selection: Selection = Selection.all, workers0: Optional[Int] = Unset)(using Reporter[report]): Runner[report]`,
  which returns a `probably.Runner.Root[report]`; `new Runner(…)` no longer compiles, and
  `Runner(…)` without `new` is unchanged. `probably.Runner.Aspirational[report](base: Runner[report])`
  is the view `aspirationally` provides: it delegates to `base` and has `aspirational = true`.
  (#PR)
- An assertion with `Runner#aspirational` set is now queued for a worker (under
  `--workers=<n>`) on the same terms as any other `assert` — a pure body in a capture-checked
  unit — where an `aspire` always ran inline. (#PR)
