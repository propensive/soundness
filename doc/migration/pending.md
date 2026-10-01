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

## acyclicity

- `acyclicity.Dag.apply[node](edges: (node, node)*): Dag[node]` (and
  `acyclicity.Dag#add(key: node, value: node): Dag[node]`, which is built on it) now makes the
  target of every edge a key of the graph, with an empty dependency set unless it has edges of
  its own; previously only sources were keys. So `Dag(a -> b).keys` is `Set(a, b)` where it was
  `Set(a)`; `sources` includes such targets; `has(target)` is `true`; and `sorted`, `reachable`,
  `descendants`, `ancestors`, `lineage`, `closure` and `traversal` no longer raise
  `Dag.Error(Cyclic)` for a graph whose edges reach a node that is not itself a source. `edges`
  and `dot` are unchanged. (#2114)

## anthology

- `anthology.Link.Error.Reason.CompilationFailed(errors: Int)` is now
  `CompilationFailed(notices: List[anthology.Notice])`, carrying every diagnostic the compiler
  reported; the former count is `notices.filter(_.importance == anthology.Importance.Error).length`.
  The message, `compilation failed with N errors`, is unchanged. (#2114)
- Component `anthology.lira` (artifact `anthology-lira`) removed; it moved to the propensive/lira
  repository with reliquary (see the reliquary entry). Its types — `LiraBundle` — move from
  package `anthology` to package `lira`, unchanged in name and signature, and are no longer
  exported into `soundness`. Depend on `dev.propensive:lira-bundle` instead. (#2111)

## breviloquence

- `breviloquence.DynamicCborEnabler` (and its `soundness` export) removed. Its replacement is
  `breviloquence.Cbor is rudiments.Dynamical`. `breviloquence.dynamicAccess.dynamicCbor` is
  retained, retyped from `DynamicCborEnabler` to `Cbor is Dynamical`. The erased parameter
  `(using erased dynamicCborEnabler: DynamicCborEnabler)` of `Cbor#selectDynamic`,
  `Cbor#applyDynamic` and both `Cbor#updateDynamic` overloads, and the erased context parameter
  of the `Cbor.lens` given, are now `(? >: Cbor) is Dynamical`. `dynamicCbor` satisfies it, and
  so does a `rudiments.dynamically[Cbor]` or `dynamically` block. (#2102)
- New `breviloquence.postables.cborPostable: Cbor is telekinesis.Postable` and
  `breviloquence.servables.cborServable: Cbor is telekinesis.Servable` (media type
  `application/cbor`, the encoded bytes as the body), also exported as
  `soundness.postables.cborPostable` and `soundness.servables.cborServable`. The `breviloquence.http`
  module now depends on `telekinesis.core` and, like `jacinta.http`, is JVM-only, so
  `breviloquence.construables.cborConstruable` is no longer available on Scala.js. (#2104)

## caesura

- `caesura.DynamicDsvEnabler` (and its `soundness` export) removed. Its replacement is
  `caesura.Dsv is rudiments.Dynamical`. `caesura.dynamicAccess.dynamicDsv` is retained, retyped
  from `DynamicDsvEnabler` to `Dsv is Dynamical`. The erased parameter
  `(using erased dynamicDsvEnabler: DynamicDsvEnabler)` of `Dsv#selectDynamic`, and the erased
  context parameter of the `caesura.dsvCellLens` given, are now `(? >: Dsv) is Dynamical`.
  `dynamicDsv` satisfies it, and so does a `rudiments.dynamically[Dsv]` or `dynamically`
  block. (#2102)

## degustation

- Component `degustation.lira` (artifact `degustation-lira`) removed; it moved to the
  propensive/lira repository with reliquary (see the reliquary entry). Its types —
  `TastyDiscipline` — move from package `degustation` to package `lira`, unchanged in name and
  signature, and are no longer exported into `soundness`. Depend on `dev.propensive:lira-tasty`
  instead. (#2111)

## ethereal

- The message `ethereal.cli` prints when an application's JAR is run without an XEK launcher
  (`ethereal.name` unset) now reads `This application must be invoked through its XEK
  launcher.`, gives the command as `xek <jar> <name>` instead of `xeq build --jar <jar> --out
  <name>`, and points to `https://propensive.dev/xek` and `https://github.com/propensive/xek`
  instead of `https://github.com/propensive/xeq`. Exit status unchanged (1). (#2113)

## exoskeleton

- `exoskeleton.Enclave` (`exoskeleton-rig`) packages its staged JAR by running the builder named
  by `$XEQ` (default `dist/xeq`) as `<builder> [--build-id <id>] <jar> <target>`, the command
  line of `xek` 0.10 and later, instead of `<builder> build --jar <jar> --out <target>
  [--build-id <id>]`. `$XEQ`, or `dist/xeq`, must therefore be an `xek` from the `xek-0.10`
  release or later; the `xeq` script of 0.9 and earlier fails to package. (#2113)

## gesticulate

- The registry behind the `media"…"` interpolator's compile-time check (`gesticulate/data/media.types`)
  is refreshed from IANA's current media-type registry: 409 types are added, among them
  `application/protobuf`, `application/yaml`, `application/toml`, `image/jxl` and `audio/flac`,
  and none is removed. A literal of a newly-registered type, previously a compile error
  ("… is not a registered media type"), now compiles; no literal that compiled before is
  rejected. (#2105)
- New enum case `gesticulate.Media.Group.Haptics`, for IANA's `haptics` top-level type (RFC
  9695). `haptics/…` media types, previously rejected with `MediaType.Error.Reason.InvalidGroup`
  both when parsed and as `media"…"` literals, are now accepted. A `match` over `Media.Group`
  that was exhaustive needs a `Haptics` case. (#2107)

## jacinta

- `jacinta.DynamicJsonEnabler` (and its `soundness` export) removed. Its replacement is
  `jacinta.Json is rudiments.Dynamical`. `jacinta.dynamicAccess.dynamicJson` is retained,
  retyped from `DynamicJsonEnabler` to `Json is Dynamical`. The erased parameter
  `(using erased dynamicJsonEnabler: DynamicJsonEnabler)` of `Json#update` and both
  `Json#updateDynamic` overloads, and the erased context parameter of the
  `jacinta.optics.jsonLens` given, are now `(? >: Json) is Dynamical`. `Json#selectDynamic` and
  `Json#applyDynamic` on an unverified `Json` now summon `(? >: Json) is Dynamical` in place of
  `DynamicJsonEnabler`. `dynamicJson` satisfies each of these, and so does a
  `rudiments.dynamically[Json]` or `dynamically` block. (#2102)
- New `jacinta.Json.emit(json: Json)(using Json.Formatting, parasite.Monitor, parasite.Probate): Iterator[Text]`,
  which serializes on a fiber and hands out the text as it is produced, and a new given
  `jacinta.Json.streamable: (Json.Formatting, Monitor, Probate) => Json is turbulence.Streamable by Text over Credit`
  built on it. `show` is unchanged and renders identically. `jacinta.core` now depends on
  `parasite.core`. (#2109)
- New push form `jacinta.Json.emit[medium: Json.Emitter](json: Json, deliver: medium => Unit)(using Json.Formatting, zephyrine.Buffering): Unit`,
  which serializes on the caller's thread with no fiber, handing each block to `deliver` as it
  fills: `emit[Text]` delivers text blocks through a `Producer[Text]`, and `emit[Data]` delivers
  UTF-8 bytes written directly by a byte-level serializer that escapes and encodes each string in
  one pass. `trait Json.Emitter[medium]` with givens `Json.Emitter.text` and `Json.Emitter.data`
  selects between them. The type argument is required when `deliver` is an untyped lambda. (#2109)
- New `jacinta.Json.lend(json: Json)(lending: zephyrine.Producer.Lending[Data])(using Json.Formatting, zephyrine.Buffering): Unit`,
  the borrowing form of the push `emit`: each filled block of UTF-8 is lent as a
  `zephyrine.Region[Data]` with its branded `Interval in region.type`, valid only for the
  duration of the call (the discipline of `Stream.lend`), so nothing is copied; `emit[Data]` is
  now defined over it and materializes each block. Numbers parsed as BCD are rendered by the
  byte-level writer directly from their nibbles, with no `String` round trip; the text of every
  form is unchanged. (#2109)

## locomotion

- New `locomotion.postables.protobufPostable: Protobuf is telekinesis.Postable` and
  `locomotion.servables.protobufServable: Protobuf is telekinesis.Servable` (media type
  `application/protobuf`, the wire-format bytes as the body), also exported as
  `soundness.postables.protobufPostable` and `soundness.servables.protobufServable`. The
  `locomotion.http` module now depends on `telekinesis.core` and, like `jacinta.http`, is
  JVM-only, so `locomotion.construables.protobufConstruable` is no longer available on Scala.js. (#2104)

## mandible

- Component `mandible.lira` (artifact `mandible-lira`) removed; it moved to the propensive/lira
  repository with reliquary (see the reliquary entry). Its types — `ClassfileAtomizer`,
  `ClassfileDiscipline`, `CtSym`, `HostArchive`, `HostContracts`, `HostRelease`, `HostTree`,
  `JsigDiscipline`, `JvmProfile` and `UsedSets` — move from package `mandible` to package `lira`,
  unchanged in name and signature, and are no longer exported into `soundness`. Depend on
  `dev.propensive:lira-classfile` instead. (#2111)

## reliquary

- Library `reliquary` removed, with its components `reliquary.core` (artifact `reliquary-core`)
  and `reliquary.derive` (`reliquary-derive`); it now lives in the propensive/lira repository.
  Its types move from package `reliquary` to package `lira`, unchanged in name and signature
  (`reliquary.Lira` → `lira.Lira`, `reliquary.Verification` → `lira.Verification`, and so on for
  every type the component exported), and they are no longer exported into `soundness`: a file
  that reached them through `import soundness.*` also needs `import lira.*` (or is itself in
  package `lira`). Replace the dependency `reliquary-core` with `dev.propensive:lira-format` and
  `reliquary-derive` with `dev.propensive:lira-derive`, published as GitHub release assets of
  propensive/lira and versioned with lira, not with Soundness. The `soundness-tool` bundle no
  longer contains them. (#2111)

## sibylline

- `sibylline.Anthropic` now sends a structured-output schema (`elicit`, `elicitAll`) as the JSON
  Schema subset the Messages API accepts: `type`, `properties`, `required`,
  `additionalProperties`, `items`, `enum`, `const`, `description`, `oneOf`/`allOf`/`anyOf`/`not`,
  `$ref`, and a string's `pattern` and `format`. jacinta's `optional` marker (rejected by the
  API as an unknown keyword) and the numeric and length bounds are no longer sent; a field's
  optionality is already expressed through `required`. Previously any `elicit` on Anthropic
  failed with `Invalid` (`property 'optional' is not supported`). (#2105)

## stratiform

- `stratiform.DynamicTelEnabler` (and its `soundness` export) removed. Its replacement is
  `stratiform.Tel is rudiments.Dynamical`. `stratiform.dynamicAccess.dynamicTel` is retained,
  retyped from `DynamicTelEnabler` to `Tel is Dynamical`. The erased parameter
  `(using erased dynamicTelEnabler: DynamicTelEnabler)` of `Tel#modify`, and the erased context
  parameter of the `stratiform.optics.telLens` given, are now `(? >: Tel) is Dynamical`.
  `Tel#selectDynamic` and `Tel#applyDynamic` on an unverified `Tel` now summon
  `(? >: Tel) is Dynamical` in place of `DynamicTelEnabler`. `dynamicTel` satisfies each of
  these, and so does a `rudiments.dynamically[Tel]` or `dynamically` block. (#2102)
- New `telp` string interpolator, `extension (inline context: StringContext) transparent inline def telp: contextual.Interpolation`
  (also exported as `soundness.telp`), producing a `stratiform.Telp` checked as the code
  compiles: a literal that `Telp.parse` would reject is a compile error positioned at the
  offending component, and a substitution is rejected. Backed by the new
  `stratiform.Telp.interpolable: Telp is contextual.Interpolable` given. `Telp.parse(text)` is
  unchanged for runtime paths. (#2104)
- New `stratiform.Tel.emit(tel: Tel)(using parasite.Monitor, parasite.Probate): Iterator[Text]`,
  which serializes on a fiber and hands out the text line by line as it is produced (a `Tel`
  rooted at a Compound is wrapped in a Document first, as `show` wraps it), and a new given
  `stratiform.Tel.streamable: (Monitor, Probate) => Tel is turbulence.Streamable by Text over Credit`
  built on it. `show` is unchanged and renders identically. `stratiform.core` now depends on
  `parasite.core`. (#2109)
- New push form `stratiform.Tel.emit[medium: Tel.Emitter](tel: Tel, deliver: medium => Unit)(using zephyrine.Buffering): Unit`,
  which serializes on the caller's thread with no fiber, handing each block to `deliver` as it
  fills: `emit[Text]` delivers text blocks through a `Producer[Text]`, and `emit[Data]` delivers
  UTF-8 bytes written directly by a byte-level serializer, which copies a parsed atom's bytes
  without decoding them. `trait Tel.Emitter[medium]` with givens `Tel.Emitter.text` and
  `Tel.Emitter.data` selects between them. The type argument is required when `deliver` is an
  untyped lambda. (#2109)
- New `stratiform.Tel.lend(tel: Tel)(lending: zephyrine.Producer.Lending[Data])(using zephyrine.Buffering): Unit`,
  the borrowing form of the push `emit`: each filled block of UTF-8 is lent as a
  `zephyrine.Region[Data]` with its branded extent, valid only for the duration of the call;
  `emit[Data]` is defined over it and materializes each block. (#2109)

## xenophile

- Component `xenophile.lira` (artifact `xenophile-lira`) removed; it moved to the propensive/lira
  repository with reliquary (see the reliquary entry). Its types — `CHeaderAtomizer`,
  `CHeaderDiscipline`, `DtsAtomizer`, `DtsDiscipline`, `KotlinMetadataAtomizer`,
  `KotlinMetadataDiscipline`, `WebIdlAtomizer`, `WebIdlDiscipline`, `WitAtomizer` and
  `WitDiscipline` — move from package `xenophile` to package `lira`, unchanged in name and
  signature, and are no longer exported into `soundness`. Depend on `dev.propensive:lira-foreign`
  instead. (#2111)

## xylophone

- `xylophone.DynamicXmlEnabler` (and its `soundness` export) removed. Its replacement is
  `xylophone.Xml is rudiments.Dynamical`. `xylophone.dynamicAccess.dynamicXml` is retained,
  retyped from `DynamicXmlEnabler` to `Xml is Dynamical`. The erased parameter
  `erased dynamicXmlEnabler: DynamicXmlEnabler` of `Xml#selectDynamic(name: String)` and
  `Xml#applyDynamic(name: String)`, and the erased context parameter of the
  `xylophone.xmlLens` given, are now `(? >: Xml) is Dynamical`. `dynamicXml` satisfies it,
  and so does a `rudiments.dynamically[Xml]` or `dynamically` block. (#2102)
- New codec givens in `object Xml`: `optionalDecodable`/`optionalEncodable` for any
  `value >: Unset.type` with a `vacuous.Mandatable to inner` (i.e. `Optional[inner]`), and
  `mapDecodable`/`mapEncodable` for `Map[key, value]`. A derived case class may now have
  `Optional` and `Map` fields. An `Unset` field encodes to no child element (an `@Xml.attribute`
  field to no attribute) and a missing element or attribute decodes to `Unset`; a `Map` field
  encodes to one child element per entry holding `<key>` and `<value>` children, and gathers all
  same-named children back. Previously such a field failed to derive, or (for `Optional[Text]`,
  through the `Decodable in Text` bridge) raised `Xml.Error(Reason.Missing)` when absent.
  `Optional[List[element]]` remains unsupported; use `List[element]`. (#2104)
- New `xylophone.servables.xmlServable: (Codepage) => Xml is telekinesis.Servable` (media type
  `application/xml; charset=UTF-8`), also exported as `soundness.servables.xmlServable`,
  alongside the existing `xmlPostable`. (#2104)
- New push form `xylophone.Xml.emit[medium: Xml.Emitter](document: turbulence.Document[Xml], deliver: medium => Unit)(using Xml.Formatting, zephyrine.Buffering): Unit`,
  which serializes on the caller's thread with no fiber, handing each block to `deliver` as it
  fills: `emit[Text]` delivers text blocks through a `Producer[Text]`, and `emit[Data]` delivers
  UTF-8 bytes written directly by a byte-level serializer that escapes and encodes each string in
  one pass. `trait Xml.Emitter[medium]` with givens `Xml.Emitter.text` and `Xml.Emitter.data`
  selects between them. The type argument is required when `deliver` is an untyped lambda.
- New `xylophone.Xml.lend(document: turbulence.Document[Xml])(lending: zephyrine.Producer.Lending[Data])(using Xml.Formatting, zephyrine.Buffering): Unit`,
  the borrowing form of the push `emit`: each filled block of UTF-8 is lent as a
  `zephyrine.Region[Data]` with its branded `Interval in region.type`, valid only for the
  duration of the call, so nothing is copied; `emit[Data]` is defined over it and materializes
  each block. The text of `show` and of the fiber form of `emit` is unchanged.
- The annotations `xylophone.Xml.attribute`, `xylophone.Xml.xmlns` and `xylophone.Xml.unqualified`
  are now reachable under `import soundness.*` as `@attribute`, `@xmlns(…)` and `@unqualified`,
  as they already were under `import xylophone.*`. `xmlns` and `unqualified` are exported;
  `attribute` is the type alias `soundness.attribute = xylophone.Xml.attribute`, so only the
  annotation (type) resolves and the term `attribute` still names `galilei`'s and `tarantula`'s
  methods. `@Xml.attribute` continues to work. (#1953)

## ypsiloid

- `ypsiloid.DynamicYamlEnabler` (and its `soundness` export) removed. Its replacement is
  `ypsiloid.Yaml is rudiments.Dynamical`. `ypsiloid.dynamicAccess.dynamicYaml` is retained,
  retyped from `DynamicYamlEnabler` to `Yaml is Dynamical`. The erased parameter
  `(using erased dynamicYamlEnabler: DynamicYamlEnabler)` of `Yaml#selectDynamic`,
  `Yaml#applyDynamic`, `Yaml#update` and both `Yaml#updateDynamic` overloads, and the erased
  context parameter of the `Yaml.lens` given, are now `(? >: Yaml) is Dynamical`.
  `dynamicYaml` satisfies it, and so does a `rudiments.dynamically[Yaml]` or `dynamically`
  block. (#2102)
- New `ypsiloid.postables.yamlPostable: (Codepage, Yaml.Formatting) => Yaml is telekinesis.Postable`
  and `ypsiloid.servables.yamlServable: (Codepage, Yaml.Formatting) => Yaml is telekinesis.Servable`
  (media type `application/yaml; charset=UTF-8`), also exported as
  `soundness.postables.yamlPostable` and `soundness.servables.yamlServable`. The `ypsiloid.http`
  module now depends on `telekinesis.core` and, like `jacinta.http`, is JVM-only, so
  `ypsiloid.construables.yamlConstruable` is no longer available on Scala.js. (#2104)

## zephyrine

- `zephyrine.Producer.Channel[medium]` (the streaming producer returned by `Producer[medium](…)`)
  gained a second type parameter: it is now `Producer.Channel[medium, operand]`, extending the
  new `abstract class Producer.Staged[medium, operand](block: Int)(using medium is Addressable { type Operand = operand })`,
  which holds the block staging shared with the new `Producer.Sink`. `Producer[medium](…)` now
  returns `Producer.Channel[medium, addr.Operand]^` in place of
  `Producer.Channel[medium] { type Operand = addr.Operand }^`; code that named the class
  spells `Channel[Text, Char]` (or the refinement's equivalent). `put`, `push`, `finish` and
  `iterator` are unchanged. (#2109)
- New `zephyrine.Producer.sink[medium](deliver: medium => Unit, block: Optional[Int] = Unset)(using medium is Addressable, Buffering)(body: Producer[medium] { type Operand = … }^ => Unit): Unit`
  (the class `Producer.Sink[medium, operand]`): the synchronous streaming counterpart of
  `Producer.collect`, handing each filled block to `deliver` on the calling thread and flushing
  the partial last block after `body` returns. (#2109)
- New `zephyrine.Producer.utf8(deliver: Data => Unit, block: Optional[Int] = Unset)(using Buffering)(body: Producer[Text] { type Operand = Char }^ => Unit): Unit`
  (the class `Producer.Utf8Sink`): a `Producer[Text]` whose blocks are delivered as UTF-8
  `Data`, encoded as characters arrive; a lone surrogate encodes as U+FFFD. It is the owning
  form of the new `zephyrine.Producer.lendUtf8(lending: Producer.Lending[Data], block: Optional[Int] = Unset)(using Buffering)(body: …): Unit`,
  which lends each filled block as a `Region[Data]` with its branded extent instead of copying
  it, under the new alias `type Producer.Lending[medium] = (region: Region[medium]) => (Interval in region.type) => Unit`.
  Both lease their byte block and char scratch from the shared `zephyrine.Blockpool` and offer
  them back when the body returns, as `Json.lend`'s writer does, so a warm writer allocates
  nothing of its own. (#2109)
- New `trait zephyrine.Producer.Emission[medium]` with givens `Producer.Emission.text` and
  `Producer.Emission.data`, selecting `sink` or `utf8` for a text serializer's push form written
  against `Producer[Text]`. (#2109)
- New `final class zephyrine.Producer.Utf8Writer(lending: Producer.Lending[Data], block: Int)`,
  the byte-level writer behind `Xml.lend` and `Tel.lend`, shared so that a text format's push
  form supplies only its escapes: `ascii(text: String)`, `name(text: String)` (through a cache
  of encoded names matched by identity or equality), `text(text: String)`,
  `escaped(text: String, escapes: Utf8Writer.Escapes)`,
  `bytes(source: scala.Array[Byte], from: Int, end: Int)`, `byte(value: Int)`,
  `long(value: Long)` and `finish(): Unit`, all `update` methods. It encodes UTF-8 straight into
  a block leased from the `Blockpool`, with a lone surrogate as U+FFFD (as `?` in a `name`), and
  lends each full block to `lending`.
  `Producer.Utf8Writer.escapes(entities: (Char, String)*): Utf8Writer.Escapes` builds an escape
  table for ASCII characters, and
  `Producer.Utf8Writer.lend(lending: Producer.Lending[Data], block: Optional[Int] = Unset)(using Buffering)(body: Utf8Writer^ => Unit): Unit`
  writes with `body` and flushes.
