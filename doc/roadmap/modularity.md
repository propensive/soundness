# Module Boundaries

Soundness has 134 libraries, and every one is meant to have a single, nameable purpose. As
modules have grown, each has accreted code that was needed for it to compile but no longer
belongs in it, and the dependency graph records that accretion: the critical path through the
build is nineteen modules deep, the test framework sits in the closure of the units library,
and the same algorithms — CRC-32, canonical Huffman coding, varints, bit-level I/O — are
implemented in up to six places. This track (issue #1640) restores the boundaries: every
module states its purpose and contains only code serving it; repetition is removed; hard
dependencies are severed with anticipation-style typeclass interfaces or integration
submodules wherever a lightweight interface suffices; and the build gets shallower and wider.

The constraint throughout is that **semantics and behaviour do not change**. Code may be
rewritten, moved, split and joined, but every leg must leave observable behaviour identical,
independently pass `make attest`, and keep the benchmarks compiling (`make build`). This
document was produced by a full audit of the codebase (2026-08); file and line references are
snapshots from that audit and must be re-verified at implementation time.

## The measured baseline (2026-08)

- Critical path, 19 modules deep: exegesis → obligatory.json → jacinta.schema →
  telekinesis.jvm → telekinesis.core → legerdemain → honeycomb → urticose.url → escapade →
  escritoire → polysyllabic → gossamer → hypotenuse → cardinality → fulminate →
  anticipation.http → anticipation.codec → anticipation.text → symbolism.
- Highest fan-in: gossamer (31 dependents), hypotenuse (12), contingency (11), jacinta (10),
  fulminate (10), hellenism (10), xenophile.wit (10).
- Largest modules: stratiform 16.7k lines/14 deps, xenophile 11.8k, jacinta 11.1k,
  pneumatic 10.9k, hallucination 9.9k, xylophone 8.3k, aviation 7.6k, ypsiloid 7.5k/15 deps.

## Mechanics that apply to every leg

- A new component means: a `build.mill` object, a `lib/<name>/src/<sub>/` directory, a
  `soundness_<lib>_<sub>.scala` export file, and membership in **exactly one** bundle;
  `./mill groupCheck.validate` enforces this. A new library additionally needs an
  `allLibraries` entry. Since #1744 the bundle is the unit of publication and each bundle
  derives its POM dependencies from its closure, so a component in no bundle is not published
  at all, and a component in two is packaged twice; `groupCheck.excluded` no longer lists
  interface modules, holding only the three standalone compiler plugins. Prefer the bundle
  that already owns the component's siblings (all `anticipation.*` are in `base`), and check
  that a new cross-bundle edge is one the graph already has.
- When a definition moves between components, its line in the old `soundness_*.scala` export
  file must be deleted **in the same commit** — duplicate toplevel exports resolve silently
  by classpath order.
- `scalaJs`/`scalaNative` flags must match the new dependency closure. Splitting a
  JVM-only tail off a module may let the remainder *gain* platforms (e.g. facsimile.core can
  drop `scalaJs = false` once its filesystem layer splits out); such gains change the
  `soundness.js`/`soundness.native` aggregates and are made deliberately.
- The `.wasi` components (turbulence.wasi, capricious.wasi, …) supply alternative backends;
  any restructure of Stdio or entropy must retarget them and keep `soundness.wasi` green.
- `bench.model` depends on all six `.staged` modules, and benchmarks are invisible to
  `soundness.all`; run `make build` after every leg.
- A given moved out of a companion object leaves implicit scope, requiring a downstream
  import sweep; a given moved *into* the subject type's companion re-enters implicit scope
  (preferred for edge inversions — no sweep needed).
- Verification per leg: `make attest` and `make build`; legs touching staged derivation or
  format parsers also run `make bench` for the affected format — performance parity is part
  of behaviour preservation.

## Corrections established during the audit

1. "cordillera" is a package inside `telekinesis.http2`, not a library; dedup items citing
   it are telekinesis.http2 items.
2. murmuration is half-wired: listed in `groupCheck.disabledLibraries` and absent from
   `allLibraries`, yet `rudiments.core` already depends on `murmuration.core`. Any move into
   murmuration needs a registration decision first.
3. scintillate re-derives chunked-transfer detection twice with a latent discrepancy
   (`== t"chunked"` vs `.contains(t"chunked")`); header-parsing consolidation must preserve
   this bug-for-bug or fix it with explicit sign-off.
4. tarantula's `screenshot(): Raster in Png` is public API; the hallucination sever keeps
   the decoded method (in an integration submodule) and adds a raw-`Data` variant — the
   existing return type never changes.
5. gossamer genuinely uses kaleidoscope but only reaches it transitively; the dependency
   should be declared when the dead ones are dropped.

## mod-1: dead edges and dead code

Horizon: near

Build edges that exist but are unused, and interface types nothing consumes. One PR, no code
motion beyond deletions:

- fulminate.core: `anticipation.http` → `anticipation.text` (http is unused; the dep existed
  only to reach text transitively — and since fulminate's fan-in is effectively the whole
  repo, this removes anticipation.codec/generic from the global minimum closure).
- urticose.url: drop `anticipation.html`. xylophone.core: `urticose.core` moves to
  xylophone.test. gossamer.core: drop mercator (test-only) and contextual (the `t"…"`
  interpolator is a bare macro), declare kaleidoscope. turbulence.core: drop stenography.
  rudiments.core: drop `anticipation.http` (verify `anticipation.path` while there).
- Delete zero-consumer anticipation types: `Legible` (src/text), `HtmlContent` and `Sgml`
  (src/html), with their export lines.

Done when: each dropped edge is verified unused by grep, the deletions compile, and
`make attest` passes.

## mod-2: one-given integration submodules

Horizon: near

Dependencies that exist to carry a single given instance (or one small file) move into
integration submodules, following the established template of `jacinta.time`,
`eucalyptus.ansi` and `hallucination.ansi`. Legs are mutually independent except where
noted.

- **urticose.ansi**: the `Url is Teletypeable` given (urticose.Url.scala:61) and
  `UrlPalette` move to a new `urticose.ansi` (deps: urticose.url, escapade.core). Cuts
  escapade and iridescence out of the closure of every URL, JSON and YAML user. Trap: the
  generic fallback `given showable: [value: Showable] => value is Teletypeable`
  (escapade.Teletypeable.scala:68) means a call site that misses the new import still
  compiles and silently renders unstyled — export the given from the `soundness` toplevel,
  sweep internal `e"…"` interpolations of URLs, and add a styling-assertion test.
- **honeycomb → anticipation.url**: the specific `HttpUrl is Attributive to Whatwg.Url`
  given (honeycomb.Attributive.scala:67) is behaviourally identical to the generic given at
  :64 (which routes through `Url`'s `abstractable = _.show`). Add a rendering test proving
  identical output, delete :67, and swap honeycomb.core's dep from urticose.url to
  anticipation.url.
- **anticipation.check**: a new anticipation submodule (in the `base` bundle with its
  siblings) holding probably's `Checkable` typeclass; probably.core re-exports it, and
  quantitative.core swaps its probably.core dep for anticipation.check (the macro sites are
  quantitative.internal.scala:208 and protointernal.scala:790). This frees ~40 modules —
  abacist, aviation and everything above them — from the test framework's closure.
- **chiaroscuro split**: `chiaroscuro.core` keeps Contrastable/Decomposable/Similarity; a
  new `chiaroscuro.render` takes Juxtaposition.scala and with it the escapade, escritoire
  and dendrology deps. probably.core depends on both, so its behaviour is unchanged, but
  everything needing only structural contrast escapes the rendering stack.
- **escapade decomposition**: a new `digression.ansi` takes the StackTrace/frame/method
  Teletypeable givens, the stack-trace palette and the `graphical` given
  (escapade.Teletypeable.scala:75–213), cutting escritoire, digression, anticipation.gfx
  and (verify) iridescence from escapade.core; the Stdio-writable givens
  (escapade.Teletype.scala:71–79, 134 — escapade's only turbulence use) move to
  `escapade.io`. escapade.core ends as csi + gossamer + anticipation.url: genuinely
  ANSI-shaped. The same silent-fallback trap as urticose.ansi applies.
- **fulminate.print**: `Message is Printable` (fulminate.Message.scala:46, which ignores
  its termcap) moves out of Message's companion into `fulminate.print`, cutting
  anticipation.print from fulminate.core. Sweep the Printable-over-Message summon sites and
  verify interaction with escapade's opt-in `displayableTypes.messagePrintable` competitor.
- **tarantula.image**: per correction 4; tarantula.core drops hallucination (and its
  gesticulate/pneumatic.flate/iridescence closure) from browser automation.
- **spectacular inversions** (after the escapade leg): `Uuid is Showable` moves into Uuid's
  companion in inimitable; `StackTrace is Showable` into StackTrace's companion in
  digression; each move flips the build edge in the same commit (spectacular loses the dep;
  the leaf gains spectacular.core — verified acyclic). Companion-to-companion moves
  preserve implicit scope, so no import sweep. The `typeRepr`/`meta` givens do **not**
  move: compiler-owned types have no companion to host them, and losing implicit scope
  would silently change `summonFrom` fallthrough in spectacular.Inspectable.scala:241.

Recorded as correct-as-is, so they are not re-litigated: fulminate → anticipation.log and
gigantism (Communicable extends Transcribable is the deliberate design, and
anticipation.log's closure is inside fulminate's once mod-1 lands); escapade →
anticipation.url/gfx; hellenism's deps; xylophone → serpentine (XPath genuinely reuses the
path algebra); stratiform's fourteen core deps (all used, all lightweight; only the
panopticon givens move, in mod-3).

Done when: each listed edge is cut, the new submodules are bundled, and no downstream module
regains the severed closure transitively.

## mod-3: module splits

Horizon: near–mid

- **legerdemain.query** — the biggest critical-path win: telekinesis uses legerdemain only
  for the `Query` type, and legerdemain's HTML-widget code is what needs honeycomb. `Query`
  splits into a honeycomb-free `legerdemain.query`; telekinesis.core retargets it, cutting
  the whole HTML/XML stack off the HTTP-client path. Check platform flags: query becomes
  fully cross-platform.
- **ethereal.dist** — **blocked as specified; needs a design decision before it is attempted
  again.** The intent was to split Runners, Assembler, Installer and Upgrade (the
  download/verify/package/self-update toolchain) from the daemon, taking telekinesis.jvm,
  urticose.url, gastronomy and zeppelin with them. On inspection the daemon and the toolchain
  are not separable along that line: `cli` (ethereal_core.scala:88) spans the rest of the file
  and *is* the daemon, and its `name` resolution recovers from a missing `ethereal.name`
  property by running the whole build-an-executable path, which calls `Runners.version`,
  `Runners.download`, `Assembler.PublicKeyLength` and `Assembler.assemble`. So core cannot shed
  Runners or Assembler, and those two need all four heavy dependencies between them; moving
  only Installer and Upgrade (which core does not reference) cuts nothing, since they need only
  zeppelin, which Assembler needs anyway. Putting `cli` in the new component instead would
  leave `core` holding little but the daemon's supporting types, and the two real external
  consumers — anthology.xeq (`Runners`) and ziggurat.packager (`Assembler`) — would take the
  new component regardless, so the split would buy nothing for them either. Doing this properly
  means extracting the build-an-executable branch behind an interface that `ethereal.dist`
  supplies, which changes how that path is reached and so crosses the no-semantic-change line.
  The `quantitative.units` observation does stand on its own: there is one such literal, not
  two — `val idleTimeout = Quantity[Hours[1]](6.0)` at ethereal_core.scala:266 — and
  `Long is Abstractable across Durations to Long` would replace it exactly.
- **ypsiloid de-jacinta**: `Bcd` (jacinta.internal.scala:284+, an opaque over
  `IArray[Double]`) moves to **hypotenuse**, severing ypsiloid → jacinta — a YAML library
  depending on a JSON library is the single most wrong edge in the graph. YamlPath (the sole
  importer of serpentine, urticose, beneficence and symbolism in ypsiloid) moves to
  `ypsiloid.pointer`.

  **Attempted and reverted; blocked on capture checking.** The mechanical part works: the
  351-line `Bcd` block (the opaque type and its companion) moves to `hypotenuse.Bcd`, jacinta
  re-exports it so every file in package `jacinta` still sees it unqualified, ypsiloid imports
  it from hypotenuse, and the ypsiloid → jacinta edge is gone. What does not survive is the
  premise that "the runtime representation is erased, so the move is bit-identical": that is
  true of the *runtime* representation and false of the *capture* behaviour, which is what the
  compiler enforces. Inside `object internal`, `Bcd`'s constructors were inferred to return a
  value carrying a fresh `any.rd` capability, and jacinta's parser launders it with
  `unsafeAssumePure`; across a module boundary that inference does not survive pickling, and
  the results arrive pure. Declaring the capture explicitly (`def apply(…): Bcd^`) fixes
  jacinta's laundering sites but then hands ypsiloid a fresh value where it expects a pure one,
  and laundering *there* in turn fails with `Found: Bcd^ / Required: Bcd^²` — a different root
  capability, exactly the read-only case the block's own comment warns `unsafeAssumePure`
  cannot launder. Getting this right needs someone fluent in the fork's capture rules to choose
  the capture signature `Bcd`'s constructors should present at a module boundary; it is not a
  matter of adjusting call sites. Note also that `Bcd.fromContent15` is `private[jacinta]` and
  its only caller is `jacinta.Json.Parser`, so any move makes that member public.

  The YamlPath half of this leg is independent of `Bcd`, and is **also blocked**, for the
  reason that blocked ethereal.dist: YamlPath is indeed the sole importer of serpentine,
  urticose, beneficence and symbolism in ypsiloid, but `ypsiloid.core` uses it pervasively —
  `Yaml.Focus` carries a `pointer: YamlPath`, there is a `Yaml is Positionable by YamlPath`
  given, and the `yp"…"` interpolator and its macro are in core — so it cannot move to a
  component above core, and moving it below core would leave core's dependency closure
  unchanged. Cutting those four dependencies means separating the Yaml AST from its pointer
  type, which is a much larger change than this leg described.
- **jacinta.optics and JsonPointer de-URL-ing** (after the ypsiloid leg; public API, so the
  largest care): the panopticon lens givens leave jacinta.Json.scala for `jacinta.optics`;
  JsonPointer's document-registry key changes from `HttpUrl` to Text or
  `Abstractable across Urls`, with the concrete HttpUrl conveniences in `jacinta.url` and
  jacinta.schema keeping URL-flavoured lookup through it. The RFC 6901 pointer core (also
  hand-rolled in ypsiloid.YamlPath and apoplexy.internal) is shared as part of this leg.
- **stratiform splits** (two PRs): (a) `stratiform.base256` is a general binary-to-text
  codec and moves into **monotonous** (extending `Serializable` to non-ASCII alphabets);
  stratiform.binary and revolution retarget; bitumen's hand-rolled `decodeOctal` adopts
  monotonous.Octal; the turbulence-facing givens move to `stratiform.io`.

  **(a) is mod-5 work, not a module split, and the parenthesis is the whole job.**
  `monotonous.Alphabet` is not itself ASCII-bound — it derives bits per character as
  `log2(chars.length)`, which gives 8 for a 256-character alphabet — but
  `Serializable.base` is: it precomputes `Array.tabulate(1 << bits)(alphabet(_).toByte)`, an
  ASCII *byte* lookup table, and its comment states the invariant plainly ("Every alphabet
  character is ASCII, so decoding the output as Latin-1 yields identical text"), which is what
  lets it build the result from Latin-1 bytes with no per-character boxing. `Base256`'s alphabet
  is emphatically not ASCII (`ḀḁЂЃĄą…`), so `.toByte` would truncate every character in it.
  Integrating the codec therefore means changing monotonous's documented fast path, which is
  performance-sensitive and belongs with mod-5's codec work, gated on equivalence tests against
  the present `Base256.encode`/`decode` and on the benchmarks. It also moves 13 consumer files
  in stratiform and reliquary from `Base256.encode(data)` to the `serialize`/`deserialize`
  vocabulary. A verbatim relocation of `Base256` into package `monotonous` would churn those
  same 13 files without achieving the unification, so it is the worst of both options and was
  not done. (b) the
  presentation-preserving editing layer (Mutation, Revision, TelHandle, ~1,607 lines, plus
  the telOpenable givens) becomes `stratiform.editing`, cutting aperture; the lens givens
  (Tel2.scala:107–137) become `stratiform.optics`, cutting panopticon; the schema layer
  (Tels.scala, Tels2.scala) becomes `stratiform.schema`, mirroring jacinta.schema's bundle
  placement.

  **Status:** `stratiform.optics` is **done** — the three lens/optic givens were members of
  `trait Tel2` (and so inherited into `Tel`'s implicit scope) and are now toplevel givens
  `telLens`, `telOrdinalOptical` and `telEachOptical`, cutting panopticon from stratiform.core.
  `Tel` is a plain class, not a `Product`, so panopticon's generic `deref` lens cannot silently
  substitute for a missing import.

  `stratiform.schema` is **blocked**: `Tel.scala` has 51 code references to `Tels` — `assign`,
  `resolveType`, `keywordMap` and friends — so the schema layer is woven into the core AST and
  cannot move above it. Unlike jacinta, whose core does not use its schema, stratiform's does.

  `stratiform.editing` is **partly analysed and not attempted**. A useful finding: `Tel.scala`'s
  `import aperture.*` is dead — the file references nothing from it — so the aperture edge rests
  entirely on `TelHandle.scala` plus two `Openable` givens. Those two are the obstacle: `Tel`'s
  `telOpenable` (Tel.scala:83) deliberately outranks `Tel2`'s `telViewOpenable` (Tel2.scala:100)
  because a given in the object beats one inherited from the trait, and that is what makes a
  writable source resolve to the write-back instance. Flattening both into toplevel givens in a
  new component destroys the ordering and makes them ambiguous for a source that is both
  `Readable` and `Writable`. Doing this leg means reconstructing the object-extends-trait
  layering inside `stratiform.editing`, deliberately, with a test that pins which instance a
  read-write source selects.
- **turbulence.stdio** — the concrete answer to "turbulence needs a clearer purpose":
  *streams* and *the standard streams* are different modules. Verified: Stdio, Io, In, Out
  and Err never touch the streaming algebra (they import only java.io, anticipation.print,
  rudiments and beneficence). They move to a module **below** turbulence
  (`Component(anticipation.print, rudiments.core, beneficence.core)`, ~290 lines including
  the `stdios` givens); turbulence.wasi retargets the new module, which is exactly what it
  exists to substitute. The `Stdio` and `stdios` toplevel exports move in the same commit.
  **Done**, with corrections. The predicted six stdio-only consumers were wrong: camouflage
  names no stdio type at all, dendrology and escritoire never had turbulence on their
  classpath, and burdock, probably and ultimatum reach the streaming algebra genuinely,
  through zeppelin, eucalyptus and profanity respectively — retargeting them changes nothing.
  Enumerating every component that *declares* turbulence.core and uses no streaming name gives
  exactly two: `pneumatic.core` and `dendrology.demo`. Retargeting pneumatic.core (which also
  had to declare the contingency and zephyrine it was getting through turbulence) takes its
  compile classpath from 42 modules to 23, dropping turbulence.core, parasite, capricious and
  hieroglyph.

  Two sub-items are **deferred, and need a decision**: `shred` cannot simply move to
  turbulence's test scope, because zephyrine's tests use it too and reach it transitively
  through probably; its natural home is `capricious` (it is a random-data generator over
  zephyrine's `Chain`), which would mean capricious depending on zephyrine. Until it moves,
  turbulence.core keeps its capricious dependency. `Document`/`Documentary` still want to move
  to a new `anticipation.document`; their consumers are wider than "format modules" (caduceus,
  exegesis and archimedes among them), so that is its own leg.
- **rudiments slimming** — the widest sweep in the programme; keep it purely mechanical.
  The confined-indexing cluster (Scribe, Grouping, Lattice, Surveyor, Deindex, Segmentable,
  Populated, ~730 lines, all sitting directly on denominative — the shape that ypsiloid,
  stratiform, facsimile and escapade converged on) becomes a new library (placeholder name
  **`concordance`**) between denominative and rudiments. Mutex, Counter
  and Loop (JVM concurrency) move to parasite; Exit to imperial or ambience;
  DecimalConverter to gossamer (rejoining Decimalizer, and cutting beneficence); Bijection
  to murmuration once correction 2 is decided.

  The `concordance` extraction is **done**, with one decision worth keeping: its sources keep
  `package rudiments`. Repackaging was tried first and abandoned — `Deindex`'s `apply`, `at`
  and `prim` extensions are the tree's indexing vocabulary, so a new package forces an import
  into nearly every file that indexes anything; the cascade reached sixty-odd components over
  six rounds and was still spreading. Keeping the package made the split invisible: no source
  file changed at all. `Bijection` went with the cluster rather than to murmuration, because
  its only consumer is `Deindex.bijection` and it needs denominative, which murmuration lacks.

  The **remaining re-homings are not worth doing as specified**, and the measurements say why:
  none of them reduces any dependency closure, because none of the code involved depends on
  anything that is not already universal. `Mutex`, `Counter` and `Loop` import only
  `java.util.concurrent`; `Exit` (48 lines) imports nothing at all; `DecimalConverter` imports
  only anticipation and beneficence. In particular the claim that moving `DecimalConverter`
  cuts beneficence from rudiments is wrong: beneficence is on the compile classpath of *every*
  one of rudiments' dependencies, `vacuous` and `denominative` included, so it stays regardless.
  Against that nil benefit, the moves cost import sweeps of the consumers, which number 4 for
  `DecimalConverter`, 9 for `Mutex`, 23 for `Exit` and 32 for `Loop`/`loop` (code uses, comments
  excluded) — `loop` being control-flow vocabulary in the same way `apply` is indexing
  vocabulary. They are worth doing only if the goal is rudiments' *description* rather than the
  build graph, and then only by moving the module while keeping the package, as `concordance`
  does. Note also that parasite.core already depends on rudiments.core, so anything moving into
  parasite must leave rudiments with no reference to it (true today of Loop, whose only mention
  in rudiments is the `loop` extension that defines it).
- **gossamer.lexicon**: Dictionary.scala (434 lines) moves to a submodule; check overlap
  with nomenclature.lexicon first.

Done when: each split module is bundled and its parent's dependency list contains only edges
its remaining code uses.

## mod-4: per-codec and per-format splits

Horizon: mid — ordered: pneumatic, then hallucination, then facsimile.

- **pneumatic per-codec**: `pneumatic.{brotli,lzma,lzw}` alongside the existing `flate`;
  BrotliDictionaryData (1,761 lines of pure tables) rides with brotli instead of taxing
  every consumer. telekinesis.http2, galilei, zeppelin, facsimile and hallucination then
  pick only flate (+lzw).
- **hallucination per-format**: five codecs share a ~890-line core; they become
  `hallucination.{jpeg,png,gif,webp,bmp}`, with `hallucination.aperture` for
  CanvasHandle/RasterOpenable; the gesticulate media-type descriptors follow their formats;
  pneumatic.flate moves to the png component; tarantula.image retargets png. Deduplication
  (Crc32, GifLzw) waits for mod-5 — split first, dedup second, one purpose per PR.

  The core-size estimate is right: the files that stay come to ~880 lines. The
  aperture component is **done**, though named `hallucination.canvas` — a component named
  `aperture` nested inside `object hallucination` shadows the `aperture` library it depends on,
  and mill's build file resolves the inner name first. `Canvas`'s `openable` given travelled
  with `CanvasHandle` and `RasterOpenable`, which emptied `Canvas`'s companion, so the companion
  is gone and `Canvas` is now just the form phantom.

  The **per-format split is blocked on a semantic question, not a mechanical one**. The
  pure-Scala backend (`core-native/RasterBackend.scala`) is a hard-coded dispatcher over all
  five codecs, and its format-agnostic `decode(data)` sniffs magic bytes by trying PNG, GIF,
  BMP, JPEG and WebP in turn. Splitting the codecs into separate components means that method
  can only recognise the formats actually linked in, so `decode(data)` becomes
  classpath-dependent where today it always recognises all five. That is arguably the point of
  the split, but it is an observable behaviour change and needs sign-off; the alternative is to
  invert the dispatcher into a registry each format component contributes to, which is a design
  change of its own. Worth noting while deciding: on the JVM the backend uses `javax.imageio`
  for PNG, JPEG, GIF and BMP and only the pure-Scala `WebpCodec`, so the four other pure codecs
  are compiled but unused at runtime there — they exist to be differentially tested against
  ImageIO, which any split has to keep possible.
- **facsimile splits**: Ascii85 moves to monotonous; Rc4 to enigmatic; Predictor (PNG/TIFF
  row filters, duplicating PngCodec logic) to the png component or pneumatic;
  `facsimile.crypto` takes Guard (cutting enigmatic and gastronomy from core);
  `facsimile.file` takes PdfFile (cutting galilei and ambience, and letting core drop
  `scalaJs = false`); `facsimile.fonts` takes the ~900-line phoenicia-facing font
  machinery. aviation stays with PdfInfo or rides with the file component.

  Mapping each heavy dependency to the files that import it gives: enigmatic → Guard, Pdf,
  PdfFile; gastronomy → FontEmbedder, Guard; galilei and ambience → PdfFile alone; phoenicia →
  FontEmbedder, PdfFont, facsimile_editing; aviation → PdfInfo.

  **`facsimile.file` is done**, and takes galilei and ambience with it: facsimile.core's compile
  classpath drops from 67 modules to 62. `PdfFile` appeared in `Pdf` only as three `Openable`
  givens, which moved with it as `pdfPathOpenable`, `pdfDataOpenable` and `pdfCreatable`. Their
  comment recorded that they were "anchored here — the form's companion — so `path.open[Pdf]()`
  … resolve with no import", so this knowingly trades that ergonomics away; `Openable` has no
  fallback, so a call site that forgets the import fails to compile.

  Core keeps `scalaJs = false`, contrary to the roadmap: the reason was two things, memory-mapped
  reads *and* JCE, and only the first left with `PdfFile`. `enigmatic` stays because `Pdf` itself
  uses it.

  **`facsimile.crypto` and `facsimile.fonts` are blocked**, both because `Pdf` and the page model
  depend on what they would take. Encryption is woven into `Pdf`: `guard` is a private var of
  type `Optional[Guard]`, `decryptStrings` takes a `Guard`, and `Guard.Method` drives
  `cryptMethod`. Fonts are woven into the page model: `Page.fonts` returns `Map[Text, PdfFont]`,
  `TextExtractor.extract` takes one, and `TextRun` has a `font: PdfFont` field. `FontEmbedder`
  alone may be separable, which would cut gastronomy only if `Guard` moved too — so the two are
  coupled. Ascii85, Rc4 and Predictor are mod-5 deduplication items rather than splits, and
  Ascii85 in particular cannot use `monotonous.Serializable.base`, whose bit-packing assumes a
  power-of-two alphabet; base-85 is not one.

Done when: a consumer of one format no longer compiles the others' code, and platform flags
reflect the new closures.

## mod-5: algorithm deduplication

Horizon: mid — after mod-4, so nothing moves twice.

- **A binary-primitives library** (placeholder name **`corpuscular`**; the fallback is a
  hypotenuse submodule): varints/LEB128 (seeded from stratiform.Varint), big-/little-endian
  accessors and a back-patchable byte builder (seeded from hallucination.Binary), bit
  readers/writers in LSB- and MSB-first variants, and a canonical-Huffman table builder
  (seeded from pneumatic.BrotliDecoder's, the cleanest of four). It lands with tests
  replicating every donor's edge cases *before* any consumer migrates.
- **Checksums into gastronomy**: a fast non-`Digestion` CRC-32/CRC-64/Adler-32 API;
  pneumatic.Flate, hallucination's png component, zeppelin and pneumatic's XzCheck migrate,
  gated by golden-vector tests. Four hand-rolled CRC-32s become one.
- **Consumer migration** onto the binary-primitives library, split by consumer:
  (a) pneumatic and hallucination codecs; (b) telekinesis.http2's Hpack and FrameReader;
  (c) stratiform, locomotion (which has two internal varint copies), mandible,
  breviloquence, phoenicia, anthology and zeppelin.
- **Remaining dedups**: the JSON string-escaping routine (jacinta.Json.scala:1247–1279,
  copied verbatim into ypsiloid) becomes a gossamer-level helper; a CSI tokenizer in
  escapade.csi replaces yossarian.Pty's and profanity.Keyboard's private escape-sequence
  state machines (yossarian's tests are the gate); a `SecureEntropy` capability in
  capricious replaces the direct `SecureRandom`/`UUID.randomUUID` uses in perihelion,
  enigmatic and inimitable (restoring wasi parity); telekinesis exports its byte-level
  header-block scanner for obligatory.ContentLength and scintillate (correction 3 applies).

Done when: each algorithm has exactly one implementation, verified by grep for the
signature patterns (CRC tables, Huffman builders, varint loops).

## mod-6: the staged-derivation kernel

Horizon: mid–far

The six staged-derivation engines (jacinta, breviloquence, locomotion, xylophone,
stratiform.staged, stratiform.binaryStaged — ~7,400 lines) share a verbatim-identical
expansion-time environment: macroClassloader, currentOutputDirectory, definedInCurrentRun,
innerClasspath, the TypeShape transport, `summonViaStaging` and the cache — ~250–300 lines
per engine, and prescience.internal.scala already holds the proto-version they all copied.

A new `prescience.staged` (bundled alongside its siblings) provides that kernel
once, parameterised by the format trait's `Class` (`TypeRepr.typeConstructorOf` instead of a
static `TypeRepr.of`). The format-specific generators, `fieldSeam` inline methods,
`Inlinable` traits and the six `XxxReader` classes **stay in their format packages**: the
generators quote `private[jacinta]`-style `rawParser`/`rawTactic` members, which a quote
written outside the package cannot name, and that capability seal is load-bearing. Expected
dedup: ~1,600–1,800 lines.

Migration is one format per PR — xylophone, locomotion, jacinta, breviloquence, then
stratiform (staged and binaryStaged together) — each verified with `make attest`,
`make build` and `make bench` for that format.

Done when: no `.staged` module contains its own copy of the expansion environment, and the
benchmark corpus shows performance parity per format.

## mod-7: stretch goals

Horizon: far — each requires explicit approval before starting, because each crosses the
no-semantic-change line or carries real regression risk.

- **Full staged-engine unification** (beyond the mod-6 kernel): requires unsealing
  `rawParser`/`rawTactic` (an API and capability-seal change) and abstracting
  `Expr[Reader]` over capability-classed types across module boundaries, where known
  cc-in-quotes pickling hazards live. Potential further ~4,000-line dedup.
- **A shared document-AST substrate for jacinta and ypsiloid** (placeholder name
  **`palimpsest`**): the `Array[Any]` node layout, PositionIndex, NumberMode, parser pool
  and descriptor layout that ypsiloid mirrors from jacinta. Audit blockers: ypsiloid's
  identity-sensitive `arrayPad` sentinel (compared with `eq`), deliberately divergent
  descriptor layouts, and hot-parser performance risk. If approved: extract from jacinta
  verbatim with jacinta delegating (bench-compared), then retarget ypsiloid separately.
- **Folding anticipation.html** (after mod-1 it holds only `GenericHtmlAttribute`, with two
  consumers).

## Rejected, with reasons — recorded so they are not re-litigated

- Inverting fulminate ↔ anticipation.log: the current direction is deliberate
  (`Communicable extends Transcribable` lets `Loggable.fanOut` log any error without
  anticipation naming `Message`), and the edge is cheap after mod-1.
- Moving spectacular's `typeRepr`/`meta` givens: no companion can host them, and the loss
  of implicit scope silently changes `summonFrom` fallthrough.
- Sharing the six `XxxReader` classes: only ~15 lines are common, and the
  `private[pkg]` seal is load-bearing.
- Merging zephyrine into turbulence, redrawing the gossamer/hieroglyph boundary, or merging
  proscenium/rudiments/vacuous: audited clean — the layering is real.

## Shape and expected outcomes

Five PRs, one per phase: mod-1 and mod-2 together, then mod-3, mod-4, mod-5 and mod-6 each
on their own. The legs within a phase remain individually verifiable — each must leave the
tree compiling and behaviour unchanged — but they are batched into one reviewable change
rather than one PR apiece. Within a PR the independent tracks may be done in any order: the
escapade chain (urticose.ansi → escapade decomposition → spectacular inversions);
anticipation.check + chiaroscuro; legerdemain.query; ethereal.dist; the ypsiloid → jacinta
chain; the stratiform splits; turbulence.stdio → rudiments; the pneumatic → hallucination →
facsimile → dedup chain; and the staged kernel. mod-7 stays out until separately approved.

Expected outcomes: the critical path shortens at three choke points (legerdemain.query,
urticose.ansi, the escapade slimming); the test framework leaves the closure of ~40
modules; ~2,500 lines of duplicated algorithms disappear in mod-5 and ~1,700 in mod-6;
and rudiments and turbulence — the two modules the issue singles out — get crisp,
one-sentence purposes: "extension-method utilities on core types" and "the pull-streaming
algebra", with the standard streams, concurrency primitives, process exit and confined
indexing each living where they belong.
