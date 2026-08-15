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

1. "cordillera" was a package inside `telekinesis.http2`, not a library; dedup items citing
   it are telekinesis.http2 items. The package has since been renamed to `telekinesis`, so
   the distinction no longer arises.
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

  **Done.** The 351-line `Bcd` block moves to `hypotenuse.Bcd`, and ypsiloid no longer has
  jacinta on its compile classpath at all.

  The earlier attempt failed because it tried to preserve the capture behaviour `Bcd`'s
  constructors had been *inferred* to have inside `object internal`, where every result carried
  a fresh `any.rd` capability that jacinta's parser laundered away with `unsafeAssumePure`.
  Declaring that explicitly fixed jacinta and broke ypsiloid, and vice versa. The resolution is
  that `Bcd` is simply a pure value — it is an opaque alias for an immutable `IArray[Double]` —
  so the constructors return it pure, every `unsafeAssumePure` around them disappears, and the
  one genuinely impure step, adopting a freshly-built mutable array, is discharged once by
  `Bcd.adopt` in the companion instead of at each call site.

  Two traps worth recording. `Bcd.fromContent15` was `private[jacinta]` and its only caller,
  `jacinta.Json.Parser`, is now in another library, so it is public — the one access widening
  the move requires. And re-exporting the type (`export hypotenuse.Bcd` in package `jacinta`)
  is *not* equivalent to importing it: the alias does not carry the companion's implicit scope,
  so `bcd.toLong` and `bcd.toDouble` stopped resolving until the files that use them imported
  `hypotenuse.Bcd` directly.

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
  *streams* and *the standard streams* are different modules. Verified: Stdio, In, Out
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

  The **remaining re-homings cannot be done as specified**, and the reason is structural rather
  than a matter of effort: each proposed home sits *above* a module that already uses the code.
  Approved for doing, attempted, and found blocked:

  - **`Loop` to parasite** — a cycle. `fulminate.internal` and `wisteria.internal` both use
    `loop`, and parasite depends on each of them transitively (parasite → digression →
    spectacular → wisteria; fulminate likewise).
  - **`Exit` to imperial or ambience** — a cycle. `contingency.Fatal` uses `Exit`, and imperial
    depends on contingency, not the reverse.
  - **`DecimalConverter` to gossamer** — a cycle. `spectacular.Showable` uses it, and gossamer
    depends on spectacular.
  - **`Mutex` to parasite** — possible, but harmful. `turbulence.Out` and `turbulence.Err` use
    it, and `turbulence.stdio` is the deliberately minimal component this track created, with 16
    modules on its classpath. parasite's closure is 37. Moving `Mutex` there would take the
    standard-streams module from 16 modules to at least 37, undoing most of what splitting it
    off achieved.
  - **`Counter` to parasite** — free, since nothing outside rudiments uses it, and pointless on
    its own.

  So rudiments holds these utilities precisely *because* they are used from below the topics
  they belong to. Re-homing them by subject means first moving their low-level consumers, which
  is a much larger change than this item describes. The earlier measurement stands too: none of
  these moves would shrink any closure.


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

  **Done, as a redesign of the backend rather than a move of files.** `Rasterizable` is now the
  codec interface, not just a descriptor: it carries `decode`, `encode` and `sniff` alongside
  `name`, `mediaType` and `alpha`, and each format supplies its own instance from its own
  component. The central `RasterBackend` is gone.

  Each format now chooses its own implementation per platform, using the `-jvm`/`-native`
  source-directory pattern: PNG, JPEG, GIF and BMP decode through `javax.imageio` on the JVM and
  through their pure codec on Scala.js and WASI, while WebP uses its pure codec everywhere,
  because no standard JRE ships a WebP reader. That dissolves what looked like a core policy —
  it was only a policy because core was making the choice. The pure codecs stay in each
  component's shared directory, so they are still compiled and still differentially tested
  against ImageIO on the JVM.

  Sniffing takes its candidates explicitly: `Raster(data)` and the `Aggregable` given now require
  a `RasterFormats`, which lists the formats the caller has linked. `hallucination.formats`
  depends on all five and supplies every format, so "decode anything" remains one import.
  Supplying an alternative codec, or adding a new format, is now an ordinary given in a new
  component; nothing in core changes.

  Resulting sizes: core 1,055 lines (including `Quantization`, which GIF's palette encoder shares
  with JPEG and which therefore stayed), webp 4,817, jpeg 2,767, gif 607, png 525, bmp 327.
  tarantula's screenshot decoder now sees `hallucination.core` and `hallucination.png` only.
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

- **A binary-primitives library** (**`corpuscular`**, now created): varints/LEB128 (seeded from
  stratiform.Varint), big-/little-endian accessors and a back-patchable byte builder (seeded
  from hallucination.Binary), bit readers/writers in LSB- and MSB-first variants, and a
  canonical-Huffman table builder (seeded from pneumatic.BrotliDecoder's, the cleanest of four).
  It lands with tests replicating every donor's edge cases *before* any consumer migrates.

  **The varints are not shareable as the roadmap assumes.** `stratiform.Varint.decode` and
  locomotion's `ProtobufParser` are not the same function with cosmetic differences:

  - stratiform aborts when `shift >= 64`; locomotion tolerates `shift` up to 70 and *discards*
    bits above 63 (`if shift < 64 then result |= …`), which is protobuf's rule for non-canonical
    encodings. Unifying them changes one or the other's behaviour on malformed input.
  - stratiform returns an allocated `Decoded(value, offset)`; locomotion mutates the parser's
    `pos` field and allocates nothing. A shared decoder either allocates in protobuf's hot loop
    or needs a cursor-style API that no donor currently has.
  - the error types differ (`VarintError` versus `ProtobufError` carrying a position), and the
    positions are part of protobuf's diagnostics.

  The *encoders* are closer, and `size`/`write` may still be worth sharing. What is genuinely
  duplicated is **inside locomotion**: `ProtobufParser` has the same decode loop twice,
  differing only in whether it bounds against `data.length` or `boundary`. That is a local
  refactor, parameterising the bound — worth doing, but it is a hot loop, so it wants
  `make bench` for protobuf either side of the change rather than being done on sight.
- **Checksums into ~~gastronomy~~ `corpuscular`**: a fast non-`Digestion` CRC-32/CRC-64/Adler-32
  API. **Partly done.** The shared implementations and their golden-vector tests have landed in
  `corpuscular`, and hallucination's PNG codec has migrated.

  Not gastronomy: `gastronomy.core` sits on 45 modules where `pneumatic.core` sits on 24, and
  neither pneumatic nor hallucination depends on gastronomy, so putting fast checksums there
  would nearly double their closures to save a table and a loop. `corpuscular` sits below them
  on `anticipation.codec` alone.

  The count of "four hand-rolled CRC-32s" needs correcting in both directions. There are five
  implementations of three algorithms — pneumatic's streaming `Crc32` and `Adler32` (JZlib
  ports), pneumatic's `Crc64` in the XZ layer, hallucination's one-shot `Crc32`, and zeppelin's
  `crc32` — but **two of them should not be deduplicated at all**: zeppelin's delegates to
  `java.util.zip.CRC32`, and gastronomy's `Digestion` CRC-32 likewise goes through
  `JavaStdlibHashing`. Both are JVM intrinsics; replacing them with a table-driven Scala loop
  would be a performance regression for no benefit. The same is true of pneumatic on the JVM,
  whose `FlateBackend` uses `JavaCrc32`; its pure `Crc32` is the JS/native path only.

  That asymmetry is itself worth resolving, and is recorded here as an open decision. pneumatic
  picks its checksum per platform — `flate-jvm` supplies `JavaCrc32`, `flate-native` the pure
  one — whereas zeppelin and gastronomy reach for `java.util.zip` unconditionally. For zeppelin
  that choice is part of why it is pinned to the JVM (`scalaJs = false`, "zip archives
  (`java.util.zip`, `galilei`)"): a ZIP library that cross-compiled would need the pure
  checksum, which `corpuscular` now provides. So the wiring is possible, and pneumatic's
  `<name>-jvm`/`<name>-native` source-directory split is the precedent for it. The decision is
  whether to adopt it — either make the platform choice consistent everywhere, or state plainly
  which libraries are JVM-only by design and stop paying for the abstraction elsewhere.

  **pneumatic's migration is done**, and the capture discipline it needed turned out to be
  three small things rather than a rewrite. `FlateChecksum` is now a `caps.Mutable` trait with
  `update def` methods, matching XzCheck's checkers and `corpuscular`'s. Making that compile
  required: declaring the fields that hold one as `FlateChecksum^` (a plain `val` hides the
  freshness, so the reference is read-only and no update method may be called on it), the same
  on the `crc32()` factories in both flate backends, and marking two reads in `Inflater`
  separate by hand — `window` and `adler` are both reached through `this`, which the separation
  checker rejects even though `update` takes its buffer read-only and keeps no reference to it.

  The build comment claiming pneumatic was "compiled with capture checking (not separation
  checking)" because "the faithfully-ported, aliasing-heavy zlib machinery does not (yet)
  satisfy the stricter ruleset" was stale: `settings.sep` applies both, and had been applied to
  this module all along. It is corrected.
- **Consumer migration** onto the binary-primitives library, split by consumer:
  (a) pneumatic and hallucination codecs; (b) telekinesis.http2's Hpack and FrameReader;
  (c) stratiform, locomotion (which has two internal varint copies), mandible,
  breviloquence, phoenicia, anthology and zeppelin.
- **Remaining dedups**: the JSON string-escaping routine (jacinta.Json.scala:1247–1279,
  copied verbatim into ypsiloid) becomes a gossamer-level helper; a CSI tokenizer in
  escapade.csi replaces yossarian.Pty's and profanity.Keyboard's private escape-sequence
  state machines (yossarian's tests are the gate);

  **Both of these premises fail on inspection.**

  The string escaper is *not* verbatim. jacinta names seven escapes (quote, backslash,
  backspace, formfeed, newline, return, tab); ypsiloid names five, omitting backspace and
  formfeed, which its `c < ' '` branch then emits as six-character unicode references instead --
  valid YAML, different bytes. ypsiloid also wraps the whole routine in `if plainSafe(string)`,
  because YAML may emit an unquoted plain scalar and JSON may not. What is genuinely shared is
  about twelve lines of sliding-window loop and an `escape` helper; the differences are precisely
  the parts that matter. Sharing it means parameterising over the escape table, in two hot
  serialisers, for twelve lines. (Whether ypsiloid's omission is deliberate is worth asking.)

  The CSI item is not a deduplication at all: `escapade.csi` is a *writer* -- `cuu`, `cud`, `sgr`
  and friends generate sequences -- and contains no parser, so this means writing a tokenizer,
  not moving one. The two consumers then differ in the ways that make a tokenizer hard to share:
  yossarian parses a complete buffer with a pure state machine and *raises* `Pty.EscapeError` on
  malformed input, as a terminal emulator must; profanity reads a live TTY where the parse is
  entangled with timing -- its `Lookahead` exists to decide whether a bare ESC is the Escape key
  or the start of a sequence split across a packet boundary -- and it degrades to
  `Keypress.Escape` rather than failing, because a user may type anything. A shared tokenizer
  would have to abstract over both the input model and the error policy. a `SecureEntropy` capability in
  capricious replaces the direct `SecureRandom`/`UUID.randomUUID` uses in perihelion,
  enigmatic and inimitable (restoring wasi parity) — **needs a decision; see below**; telekinesis exports its byte-level
  header-block scanner for obligatory.ContentLength and scintillate (correction 3 applies).

**SecureEntropy.** The list of sites needs correcting before this is attempted. enigmatic's
`SecureRandom` uses are all in `core-jvm`, its JVM backend, alongside a `core-native` twin and an
OpenSSL provider — that is the platform-backend pattern working as intended, not a parity gap, and
they should stay. perihelion is `scalaJs = false`, so its per-connection `SecureRandom` for
WebSocket masking keys is JVM-only by construction. The real site is inimitable's `Uuid.apply()`,
which calls `ju.UUID.randomUUID()` in a library that cross-compiles to every platform.

capricious is a viable home — `capricious.core` cross-compiles (the `scalaNative = false` is on
`capricious.wasi`), and capricious already has exactly the right precedent: a `wasi` component
supplying a `Randomization` backed by a `wasi:random/random` import.

What makes this a decision rather than a change is the shape. `Uuid()` has around 28 call sites
across the tree, so `def apply()(using SecureEntropy)` is a public API change requiring the
capability in scope everywhere. The alternative is `core-jvm`/`core-native` backends inside
inimitable, as galilei and pneumatic do, which fixes parity with no API change. Choosing between
them depends on whether `java.util.UUID.randomUUID` is actually deficient on JS, Native and WASI —
it compiles on all of them, so this is a question about entropy quality, and wants measuring on
those platforms rather than assuming.

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

# The second audit (2026-08-09)

With mod-1 through mod-5 landed (or blocked with recorded reasons), the graph was re-measured
at the tip of `modularity/decided-items` and swept again. Method: the component graph was
parsed out of `build.mill` and depth, fan-in and closure computed per *component* (the first
baseline counted libraries, so the two depth figures are not comparable); every declared
dependency whose package name never appears in the consumer's sources was flagged; and each
flag was then verified by hand against the dependency's `soundness_*.scala` export file (for
toplevel names usable without naming the package) and against implicit scope (givens in the
companions of mentioned types). File and line references are snapshots from this audit and
must be re-verified at implementation time. A grep verdict is evidence, not proof: the
definitive check for every removal below is compiling the component (and its dependents)
without the edge.

## The re-measured baseline

- Critical path, 27 components deep: exegesis.core → obligatory.json → hyperbole.core →
  harlequin.ansi → harlequin.core → hellenism.jvm → galilei.jvm → galilei.core →
  turbulence.core → parasite.core → nomenclature.core → gossamer.core → distillate.core →
  digression.core → iridescence.core → geodesy.angle → hypotenuse.core → contingency.core →
  rudiments.core → concordance.core → denominative.core → vacuous.core → fulminate.core →
  anticipation.log → anticipation.text → symbolism.core → prepositional.core.
- Highest fan-in: gossamer.core (44), turbulence.core (31), hypotenuse.core (23),
  hellenism.core and zephyrine.core (18), prepositional.core and contingency.core (16).
- Largest closures: exegesis.core (99 components), anthology.xeq (94), burdock.core (90),
  ziggurat.packager (89), ethereal.core (87).
- Simulating every leg below applied at once: critical path 27 → 23, exegesis.core's closure
  99 → 59, gossamer.core's closure → 23, pneumatic.core's → 21. The residual spine is
  anthology → hellenism.jvm → galilei → turbulence → parasite → nomenclature → gossamer →
  the foundations, whose one hard link is recorded at the end.

Both ends of the measured path turned out to be soft: the head (exegesis/obligatory down to
harlequin) is held together largely by dead edges, and the tail below gossamer hangs on a
handful of relocatable givens.

## mod-8: dead edges, round two — and the misdeclarations they hide

Horizon: near. One PR, deletions and dependency-list corrections only.

The scan surfaced two things the first audit's mod-1 did not: a second round of genuinely
dead edges, and — more urgent — components using dependencies they never declared, reaching
them through *someone else's* dead edge. The misdeclarations are correctness debt (one
classpath reorder from a build break) and go first, in the same PR:

- **hallucination.png** uses `corpuscular.Crc32.checksum`
  (hallucination.PngCodec.scala:84, 360) but declares only `core`, whose own corpuscular
  edge is otherwise dead. Move the `corpuscular.core` declaration from core to png.
- **obligatory.grpc** imports spectacular (obligatory.Grpc.scala:44,
  obligatory.GrpcChannel.scala:47) without declaring it, reaching it through
  locomotion.core's dead spectacular edge. Declare it before that edge is cut.
- **ziggurat.packager** uses `digest[Sha2[256]]` (ziggurat.Packager.scala:133) with
  gastronomy undeclared, currently supplied via telekinesis.jvm → coaxial.jvm. Declare it.
- **degustation.lira** imports rudiments (degustation.Tasty.scala:40) undeclared, reached
  through a chain of dead edges (reliquary → stratiform → contextual → rudiments). Declare
  it before cutting `contextual.core → rudiments.core`.

**Conduit edges** — name-dead, but the sole supplier of a module the consumer genuinely
imports. These are misdeclared dependency lists, not cuttable edges: declare what is really
used, and then decide per case whether the conduit drops:

- breviloquence.core → urticose.url (really needs gossamer.core; its sources import
  gossamer, spectacular, distillate and wisteria, none declared).
- revolution.core → serpentine.core (sole path to gossamer.core).
- serpentine.core → ambience.core (sole path to anticipation.path and anticipation.print;
  the `textPath` given in anticipation_serpentine_core.scala needs `Paths`).
- profanity.core → eucalyptus.core (sole path to turbulence.core and parasite.core) and
  → frontier.core (sole path to escapade.core; profanity's declared escapade.csi does not
  contain `Teletype`).
- honeycomb.core → xylophone.core (sole supplier of typonym.core, adversaria.core and
  wisteria.core, all imported by honeycomb).

**Confirmed dead** (no exported name used, no implicit-scope route, closure re-supplied by
other declared deps): hypotenuse.core → cardinality.core; iridescence.core →
contextual.core; zephyrine.core → hypotenuse.core (verify anticipation.opaque still
arrives); pneumatic.core → contingency.core; enigmatic.asn1 → gossamer.core;
facsimile.core → eucalyptus.core; probably.core → eucalyptus.core; profanity.core →
diuretic.core; tarantula.core → diuretic.core and → gastronomy.core; burdock.core →
gastronomy.core; apoplexy.core → legerdemain.core (it reaches legerdemain.query through
telekinesis.core, which is all it needs); denominative.core → fulminate.core;
contextual.core → rudiments.core; degustation.core → rudiments.core; polyvinyl.core →
rudiments.core; coaxial.core → frontier.core; punctuation.core → frontier.core;
austronesian.core → hellenism.core; locomotion.core → spectacular.core; typonym.core →
anticipation.text; vexillology.core → hypotenuse.core; yossarian.core → iridescence.core;
xenophile.js → gossamer.core; ziggurat.core → gastronomy.core; ziggurat.packager →
guillotine.core; telekinesis.http2 → hypotenuse.core; legerdemain.query → anamnesis.core;
stratiform.base256 → gossamer.core and → vacuous.core; embarcadero.containerd →
coaxial.core (redundant: re-supplied via telekinesis.http2 → coaxial.jvm);
surveillance.core → eucalyptus.core; prescience.core → fulminate.core and → gossamer.core.

Two scan traps, recorded so the next sweep does not repeat them: telekinesis.http2's
sources used to live in `package cordillera`, so a package-name grep called the edge dead
when it was genuinely used (embarcadero.containerd imports it) — that package is now
`telekinesis`, but a component whose package differs from its library will trip the same
wire; and components with
`override def sources` pull in sibling directories the per-component scan misses —
enigmatic.core → aperture.core looked dead but is used in `core-jvm`
(enigmatic.Keystore.scala).

Done when: each misdeclaration is fixed, each dead edge is deleted, and every affected
component and its dependents compile (`make attest`, `make build`).

## mod-9: the critical-path head

Horizon: near. The chain exegesis → obligatory.json → hyperbole → harlequin → anthology is
held together by dead edges at its top links; severing them detaches the LSP and JSON-RPC
stack from the compiler stack entirely.

- **obligatory.json** drops scintillate.server, hyperbole.core and revolution.core — all
  three dead. obligatory.json is a JSON-RPC peer plus SSE codec: it never serves HTTP
  (its `Servable` is telekinesis's, reached via jacinta.http; `httpBackends.virtualMachineHttp`
  comes from telekinesis.jvm via jacinta.schema), never introspects TASTy, and never touches
  a manifest. The hyperbole edge is what put the whole harlequin/anthology stack under
  exegesis; deleting it collapses the measured deepest path. (eucalyptus.core is genuinely
  used — obligatory.JsonRpc.scala:42.)
- **exegesis.core** → ethereal.core and → exoskeleton.completions move to **exegesis.demo**,
  their only real consumer (exegesis_demoserver.scala:38–53, exegesis_demoproxy.scala:39–59;
  nothing under src/core references either). ethereal was the stated reason for exegesis's
  `scalaJs = false` (build.mill), so core may gain a platform — a deliberate flag change per
  the mod-mechanics rules, gated on obligatory.json's own closure.
- **hyperbole.core** retargets harlequin.ansi → harlequin.core plus escapade.core. Nothing
  in hyperbole names `ScalaSyntaxPalette` or `syntaxHighlighting` (harlequin.ansi's entire
  content, and package-scoped, so it cannot be reached through implicit scope); what
  hyperbole uses is `SourceCode`/`Scala.highlight` (hyperbole.internal.scala:85, 93) and
  escapade directly (TastySymbol.scala:36, TastyTree.scala:39). escritoire and
  dendrology.tree are genuinely used and stay.
- **mandible.core** replaces its hyperbole.core dep with the four modules it actually
  imports — anthology.scala (mandible_core.scala:68), hellenism.jvm
  (mandible.Classfile.scala:43), escritoire.core (mandible.Bytecode.scala:48–57) and
  escapade.core. hyperbole was a conduit; nothing in mandible uses it.
- **perihelion.core** replaces scintillate.server with telekinesis.core (only `Servable`,
  `Http` and friends are used — perihelion.Websocket.scala:201–202; telekinesis.http2 and
  hellenism.core, which rode along, are unused) and drops eucalyptus.core outright (logging
  goes through anticipation's `Log`, perihelion.WebsocketEvent.scala:35).

**harlequin.scala (design item, approval needed before starting).** Scalac/LocalClasspath
usage in harlequin is confined to `Highlight`, `Diagnostic` (which needs only
anthology.core's `Importance`) and ~300 lines of `SourceCode`
(harlequin.SourceCode.scala:376–689: `resolveTypes`, `frontend`, the completion machinery).
A `harlequin.scala` component would free harlequin.core — and with it harlequin.md and every
Markdown-rendering consumer — from anthology.scala, hellenism.jvm and galilei.jvm. It would
*not* free them from the scala3-compiler jar itself: the tokenizer imports
`dotty.tools.dotc.parsing` directly (harlequin.SourceCode.scala:41–46). The blocker to
design around: `SourceCode.apply` takes `using Highlight` and pattern-matches on its
`scalac`/`classpath` fields (harlequin.SourceCode.scala:73–90), so `Highlight` must be
abstracted into a resolver interface that `harlequin.scala` supplies, with the tokenized
path as core's default. Also cheap and independent: harlequin.core's explicit hellenism.jvm
dep is already implied by anthology.scala → anthology.core.

Done when: exegesis.core's closure contains no compiler-stack module, and the deepest chain
through obligatory.json ends at jacinta.

## mod-10: the spine givens

Horizon: near. gossamer.core has fan-in 44, so everything below it taxes nearly the whole
tree — and what sits below it hangs on single-digit numbers of givens. These are mod-2-style
integration-submodule moves, with the same traps mod-2 recorded: a given moved out of a
companion (or out of the package's toplevel) leaves implicit scope, so each move needs a
downstream import sweep, a `soundness`-toplevel export, and where a silent generic fallback
exists, a test pinning the resolved instance.

- **gossamer.core → distillate.core** rests on five givens in one file: `textDecodable`
  (gossamer_core.scala:124) and the four `enumIdentification` case-style `Identifiable`
  givens (gossamer_core.scala:614–625) — the latter already opt-in behind an explicit
  import, so relocation costs their users nothing. Move them to a `gossamer.codec`-style
  submodule. Cutting this single edge is worth three levels of critical path (it removes
  distillate → digression → iridescence → geodesy.angle from under the fan-in-44 node).
- **distillate.core → digression.core** is one two-line given (`fqcn` Decodable,
  distillate.Decodable.scala:62). Invert it: `Fqcn` is digression's type, so the given
  belongs in a digression-side component that depends on distillate.
- **distillate.core → inimitable.core** is the identical shape (`uuid` Decodable,
  distillate.Decodable.scala:64) and takes the identical inversion.
- **distillate.core → wisteria.core** uses only `VariantError` as the error type of the
  `enumeration` given (distillate.Decodable.scala:108–114). Relocate the error class (a
  plain fulminate `Error` in wisteria.VariantError.scala) to a component both can see;
  the `enumeration` given itself stays in companion scope. Note wisteria remains in the
  closure via spectacular.core regardless — this cut is for distillate's own dependents.
- **digression.core → iridescence.core** is `StackTrace.Palette`, `hex` and
  `defaultPalette` (digression.StackTrace.scala:135–163), ~30 lines of pure colour. They
  move to **digression.ansi**, which gains iridescence.core; both existing consumers
  (digression_ansi.scala, probably.AnsiRenderer.scala:137, 355) are already downstream of
  it. This severs iridescence → geodesy.angle from the spine in one move.
- **digression.core → spectacular.core** is one given (`showable`,
  digression.StackTrace.scala:111–133) plus one `.show` on an `Int` (line 128, trivially
  `.toString.tt`). Movable to a `digression.show`, but rank it below the others: the given
  sits in `StackTrace`'s companion, so moving it costs implicit-scope resolution for every
  downstream `stackTrace.show`, and spectacular's own closure (stenography, wisteria) is
  modules the spine keeps anyway.
- **parasite.core → mercator.core** is one given, `Monad[Task]`
  (parasite.Task.scala:90–99). A `parasite.monad` component severs it; the caveat is the
  usual one (for-comprehensions over `Task` currently resolve import-free from the
  companion).
- **turbulence.core → capricious.core** is confirmed to be exactly the deferred `shred`
  item (turbulence_core.scala:319–324) and nothing else; mod-3's open decision on `shred`'s
  home is unchanged, only now it is the whole of the edge.

**Acquitted in the second audit, so they are not re-litigated:** gossamer.core →
kaleidoscope.core (`Scanner` is inside the interpolator macro expansion,
gossamer.internal.scala:239–292, and kaleidoscope's own closure is contingency alone);
digression.core → contingency.core (`raises` is in `Fqcn.apply`'s signature);
parasite.core → anticipation.time (~17 signature sites, and the dep is depth-2 with no text
stack — free); iridescence.core → geodesy.angle (`Angle` is in `Hsl`/`Hsv`'s public fields,
and iridescence already depends on hypotenuse directly, so the edge adds one shallow node);
turbulence.core → anticipation.http (three sites, but the `import zephyrine.{stream as _, *}`
shadowing arrangement in turbulence.Streamable.scala:48–50 depends on `HttpStreams.Body.stream`
being a same-module sibling, and the dep is depth-3 and off the spine — not worth the
friction).

**The one hard edge: parasite.core → nomenclature.core → gossamer.core.** `Name[Async]` is
in the public signatures of `task` (parasite_core.scala:150), `Task.apply`, `Monitor` and
both platform supervisors (~20 sites), and `Async is Nominative under Rules`
(parasite.Async.scala:47) puts nomenclature in the implicit machinery. There is no
relocation that severs this: the choice is between validated names and `Text` in the
concurrency API, which is a design decision of mod-7 class. Until it is made, gossamer and
everything beneath it remain in the closure of parasite, turbulence, and most of the tree.
After mod-8/9/10, this is the residual spine's only soft point.

## Second-audit shape

Three PRs: mod-8 (misdeclarations first, then the dead edges), mod-9 (the critical-path
head, including the cheap retargets; harlequin.scala stays out until its design is
approved), mod-10 (the spine givens, one integration submodule per leg, each with its
import sweep and pinning tests). Expected outcomes, by simulation: critical path 27 → 23
components; exegesis.core's closure 99 → 59; gossamer.core's closure → 23; and the
remaining depth concentrated in the one genuinely open question, parasite's validated task
names.
