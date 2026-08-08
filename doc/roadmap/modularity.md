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
  `IArray[Double]` whose runtime representation is erased, so the move is bit-identical)
  moves to **hypotenuse**, severing ypsiloid → jacinta — a YAML library depending on a JSON
  library is the single most wrong edge in the graph. YamlPath (the sole importer of
  serpentine, urticose, beneficence and symbolism in ypsiloid) moves to `ypsiloid.pointer`.
- **jacinta.optics and JsonPointer de-URL-ing** (after the ypsiloid leg; public API, so the
  largest care): the panopticon lens givens leave jacinta.Json.scala for `jacinta.optics`;
  JsonPointer's document-registry key changes from `HttpUrl` to Text or
  `Abstractable across Urls`, with the concrete HttpUrl conveniences in `jacinta.url` and
  jacinta.schema keeping URL-flavoured lookup through it. The RFC 6901 pointer core (also
  hand-rolled in ypsiloid.YamlPath and apoplexy.internal) is shared as part of this leg.
- **stratiform splits** (two PRs): (a) `stratiform.base256` is a general binary-to-text
  codec and moves into **monotonous** (extending `Serializable` to non-ASCII alphabets);
  stratiform.binary and revolution retarget; bitumen's hand-rolled `decodeOctal` adopts
  monotonous.Octal; the turbulence-facing givens move to `stratiform.io`. (b) the
  presentation-preserving editing layer (Mutation, Revision, TelHandle, ~1,607 lines, plus
  the telOpenable givens) becomes `stratiform.editing`, cutting aperture; the lens givens
  (Tel2.scala:107–137) become `stratiform.optics`, cutting panopticon; the schema layer
  (Tels.scala, Tels2.scala) becomes `stratiform.schema`, mirroring jacinta.schema's bundle
  placement.
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
  **`concordance`** — rename at will) between denominative and rudiments. Mutex, Counter
  and Loop (JVM concurrency) move to parasite; Exit to imperial or ambience;
  DecimalConverter to gossamer (rejoining Decimalizer, and cutting beneficence); Bijection
  to murmuration once correction 2 is decided.
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
- **facsimile splits**: Ascii85 moves to monotonous; Rc4 to enigmatic; Predictor (PNG/TIFF
  row filters, duplicating PngCodec logic) to the png component or pneumatic;
  `facsimile.crypto` takes Guard (cutting enigmatic and gastronomy from core);
  `facsimile.file` takes PdfFile (cutting galilei and ambience, and letting core drop
  `scalaJs = false`); `facsimile.fonts` takes the ~900-line phoenicia-facing font
  machinery. aviation stays with PdfInfo or rides with the file component.

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
