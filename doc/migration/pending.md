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
  `check` instead. (#2101)
- `probably.Spread#aspire[report](predicate: (value, result) => Boolean)(using Runner[report], Inclusion[report, Verdict], Inclusion[report, Verdict.Detail]): Unit`
  and `probably.Spread2#aspire[report](predicate: (left, right, result) => Boolean)(using …): Unit`
  removed; call `assert` within `aspirationally` instead. (#2101)
- `probably.Runner` changed from a class to a trait, `trait Runner[report] extends Findable`,
  with the new member `def aspirational: Boolean` (default `false`). Construct one with
  `probably.Runner[report](selection: Selection = Selection.all, workers0: Optional[Int] = Unset)(using Reporter[report]): Runner[report]`,
  which returns a `probably.Runner.Root[report]`; `new Runner(…)` no longer compiles, and
  `Runner(…)` without `new` is unchanged. `probably.Runner.Aspirational[report](base: Runner[report])`
  is the view `aspirationally` provides: it delegates to `base` and has `aspirational = true`.
  (#2101)
- An assertion with `Runner#aspirational` set is now queued for a worker (under
  `--workers=<n>`) on the same terms as any other `assert` — a pure body in a capture-checked
  unit — where an `aspire` always ran inline. (#2101)

## breviloquence

- `breviloquence.DynamicCborEnabler` (and its `soundness` export) removed. Its replacement is
  `breviloquence.Cbor is rudiments.Dynamical`. `breviloquence.dynamicAccess.dynamicCbor` is
  retained, retyped from `DynamicCborEnabler` to `Cbor is Dynamical`. The erased parameter
  `(using erased dynamicCborEnabler: DynamicCborEnabler)` of `Cbor#selectDynamic`,
  `Cbor#applyDynamic` and both `Cbor#updateDynamic` overloads, and the erased context parameter
  of the `Cbor.lens` given, are now `(? >: Cbor) is Dynamical`. `dynamicCbor` satisfies it, and
  so does a `rudiments.dynamically[Cbor]` or `dynamically` block. (#TBD)

## caesura

- `caesura.DynamicDsvEnabler` (and its `soundness` export) removed. Its replacement is
  `caesura.Dsv is rudiments.Dynamical`. `caesura.dynamicAccess.dynamicDsv` is retained, retyped
  from `DynamicDsvEnabler` to `Dsv is Dynamical`. The erased parameter
  `(using erased dynamicDsvEnabler: DynamicDsvEnabler)` of `Dsv#selectDynamic`, and the erased
  context parameter of the `caesura.dsvCellLens` given, are now `(? >: Dsv) is Dynamical`.
  `dynamicDsv` satisfies it, and so does a `rudiments.dynamically[Dsv]` or `dynamically`
  block. (#TBD)

## jacinta

- `jacinta.DynamicJsonEnabler` (and its `soundness` export) removed. Its replacement is
  `jacinta.Json is rudiments.Dynamical`. `jacinta.dynamicAccess.dynamicJson` is retained,
  retyped from `DynamicJsonEnabler` to `Json is Dynamical`. The erased parameter
  `(using erased dynamicJsonEnabler: DynamicJsonEnabler)` of `Json#update` and both
  `Json#updateDynamic` overloads, and the erased context parameter of the
  `jacinta.optics.jsonLens` given, are now `(? >: Json) is Dynamical`. `Json#selectDynamic` and
  `Json#applyDynamic` on an unverified `Json` now summon `(? >: Json) is Dynamical` in place of
  `DynamicJsonEnabler`. `dynamicJson` satisfies each of these, and so does a
  `rudiments.dynamically[Json]` or `dynamically` block. (#TBD)

## stratiform

- `stratiform.DynamicTelEnabler` (and its `soundness` export) removed. Its replacement is
  `stratiform.Tel is rudiments.Dynamical`. `stratiform.dynamicAccess.dynamicTel` is retained,
  retyped from `DynamicTelEnabler` to `Tel is Dynamical`. The erased parameter
  `(using erased dynamicTelEnabler: DynamicTelEnabler)` of `Tel#modify`, and the erased context
  parameter of the `stratiform.optics.telLens` given, are now `(? >: Tel) is Dynamical`.
  `Tel#selectDynamic` and `Tel#applyDynamic` on an unverified `Tel` now summon
  `(? >: Tel) is Dynamical` in place of `DynamicTelEnabler`. `dynamicTel` satisfies each of
  these, and so does a `rudiments.dynamically[Tel]` or `dynamically` block. (#TBD)

## xylophone

- `xylophone.DynamicXmlEnabler` (and its `soundness` export) removed. Its replacement is
  `xylophone.Xml is rudiments.Dynamical`. `xylophone.dynamicAccess.dynamicXml` is retained,
  retyped from `DynamicXmlEnabler` to `Xml is Dynamical`. The erased parameter
  `erased dynamicXmlEnabler: DynamicXmlEnabler` of `Xml#selectDynamic(name: String)` and
  `Xml#applyDynamic(name: String)`, and the erased context parameter of the
  `xylophone.xmlLens` given, are now `(? >: Xml) is Dynamical`. `dynamicXml` satisfies it,
  and so does a `rudiments.dynamically[Xml]` or `dynamically` block. (#TBD)

## ypsiloid

- `ypsiloid.DynamicYamlEnabler` (and its `soundness` export) removed. Its replacement is
  `ypsiloid.Yaml is rudiments.Dynamical`. `ypsiloid.dynamicAccess.dynamicYaml` is retained,
  retyped from `DynamicYamlEnabler` to `Yaml is Dynamical`. The erased parameter
  `(using erased dynamicYamlEnabler: DynamicYamlEnabler)` of `Yaml#selectDynamic`,
  `Yaml#applyDynamic`, `Yaml#update` and both `Yaml#updateDynamic` overloads, and the erased
  context parameter of the `Yaml.lens` given, are now `(? >: Yaml) is Dynamical`.
  `dynamicYaml` satisfies it, and so does a `rudiments.dynamically[Yaml]` or `dynamically`
  block. (#TBD)
