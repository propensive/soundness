# Changes since 0.64.0 (pending release)

This file is read by an LLM agent to upgrade code that consumes Soundness libraries. Each
entry states precisely what changed; see `AGENTS.md` for the format. Entries are grouped
by module, most-recently-added last within a module.

Entries for changes merged between the 0.64.0 release and the introduction of this file have
not yet been recorded here.

## ambience

- `ambience.systems.javaSystem` renamed to `javaBaseSystem`. (#1939)
- `ambience.environments.javaEnvironment` renamed to `javaBaseEnvironment`. (#1939)
- `ambience.workingDirectories.javaWorkingDirectory` renamed to `javaBaseWorkingDirectory`. (#1939)
- `ambience.workingDirectories.defaultWorkingDirectory` removed; use `javaBaseWorkingDirectory`,
  which reads the `user.dir` property (the removed given read `java.nio.file.Paths.get("")`,
  which resolves to the same directory). (#1939)
- `ambience.temporaryDirectories.javaTemporaryDirectory` renamed to `javaBaseTemporaryDirectory`. (#1939)

## turbulence

- `turbulence.stdios.virtualMachineStdio` (wrapping `java.io.FileDescriptor.{in,out,err}`) renamed
  to `fileDescriptorStdio`. (#1939)
- `turbulence.stdios.systemStdio` (wrapping `java.lang.System.{in,out,err}`) renamed to
  `javaLangSystemStdio`. (#1939)
- `turbulence.lineSeparation.virtualMachineLineSeparation` renamed to `javaBaseLineSeparation`. (#1939)

## coaxial

- `coaxial.socketBackends.virtualMachineSockets` renamed to `javaBaseSockets`. (#1939)
- `coaxial.socketBackends.native` (Scala Native module) renamed to `scalaNativeSockets`, and now
  re-exported as `soundness.socketBackends.scalaNativeSockets`. (#1939)

## galilei

- `galilei.filesystemBackends.virtualMachineFilesystem` renamed to `javaBaseFilesystem`. (#1939)
- `galilei.filesystemBackends.native` (Scala Native module) renamed to `scalaNativeFilesystem`,
  and now re-exported as `soundness.filesystemBackends.scalaNativeFilesystem`. (#1939)
- The nested objects under `galilei.filesystemOptions` are flattened into givens of the same
  types: `dereferenceSymlinks.enabled` → `dereferenceSymlinks`, `dereferenceSymlinks.disabled` →
  `preserveSymlinks`, `moveAtomically.enabled` → `moveAtomically`, `moveAtomically.disabled` →
  `moveNonAtomically`, `copyAttributes.enabled` → `copyAttributes`, `copyAttributes.disabled` →
  `discardAttributes`, `deleteRecursively.enabled` → `deleteRecursively`,
  `deleteRecursively.disabled` → `deleteOnlyEmpty`, `overwritePreexisting.enabled` →
  `overwritePreexisting`, `overwritePreexisting.disabled` → `failOnPreexisting`,
  `createNonexistentParents.enabled` → `createNonexistentParents`,
  `createNonexistentParents.disabled` → `requireParents`. The objects no longer exist. (#1939)

## telekinesis

- `telekinesis.httpBackends.virtualMachineHttp` renamed to `javaNetHttp`. (#1939)
- `telekinesis.httpBackends.nativeHttp` renamed to `soundnessHttp`. (#1939)

## scintillate

- `scintillate.httpServers.stdlibHttpd` renamed to `jdkHttpserver`; `stdlibPublicHttpd` renamed
  to `jdkHttpserverPublic`. (#1939)
- `scintillate.httpServers.nativeHttpServer` renamed to `soundnessHttpd`; `nativePublicHttpServer`
  renamed to `soundnessHttpdPublic`; both are now also re-exported as
  `soundness.httpServers.{soundnessHttpd, soundnessHttpdPublic}`. (#1939)
- `scintillate.frontends.threadPerConnection` renamed to `threadPerConnectionFrontend`;
  `frontends.reactive` renamed to `reactiveFrontend`. (#1939)
- `scintillate.webserverErrorPages.standardErrorPage` renamed to `styledErrorPage`. (#1939)

## surveillance

- `surveillance.watchers.nativeWatcher` renamed to `javaBaseWatcher`; `surveillance.NativeWatcher`
  (object) renamed to `JavaBaseWatcher`. (#1939)

## gastronomy

- `gastronomy.Provider.JavaStdlib` renamed to `Provider.JavaBase`; `gastronomy.JavaStdlibHashing`
  renamed to `JavaBaseHashing`; `gastronomy.Hashing.javaStdlibHashing` renamed to
  `javaBaseHashing`; `gastronomy.providers.javaStdlibProvider` renamed to `javaBaseProvider`. (#1939)
- Choice package `gastronomy.crypto` renamed to `gastronomy.cryptoPermits` (members
  `permitUnauthenticatedCrypto`, `permitDeprecatedCrypto`, `permitLegacyCrypto`,
  `permitDisallowedCrypto`, `permitCryptoThrough2014`, `permitCryptoThrough2024`,
  `permitCryptoThrough2030`, `permitLegacyTls`, `permitUntrustedCertificates`,
  `permitUncheckedRevocation`, `permitNonCryptographicHashes` unchanged). (#1939)

## enigmatic

- `enigmatic.JavaStdlibCrypto` renamed to `JavaBaseCrypto`; `enigmatic.Crypto.javaStdlibCrypto`
  renamed to `javaBaseCrypto`. (#1939)
- `enigmatic.cloaks.{cloakHeap, cloakOffHeap, cloakVeiledHeap, cloakVeiledOffHeap}` renamed to
  `{heapCloak, offHeapCloak, veiledHeapCloak, veiledOffHeapCloak}`. (#1939)
- Choice packages `enigmatic.blockCipherMode` and `enigmatic.blockCipherPadding` renamed to
  `blockCipherModes` and `blockCipherPaddings` (members `cbc`, `ctr`, `cfb`, `ofb`, `pkcs7`,
  `iso10126` unchanged). (#1939)

## kaleidoscope

- `kaleidoscope.Jur` (type and companion) renamed to `JavaBaseRegex`;
  `kaleidoscope.regexBackends.jur` renamed to `javaBaseRegex`. (#1939)

## diuretic

- Top-level `diuretic.javaNioFilePath` renamed to `javaNioPathRepresentative` and top-level
  `diuretic.javaIoFile` renamed to `javaIoFileRepresentative` (the `Representative of Paths`
  markers; `anticipation.pathInterfaces.{javaNioPath, javaIoFile}` are unchanged). (#1939)

## harlequin

- `punctuation.formattables.{scala, java}` (the `CommonFormattable` givens in `harlequin.md`)
  renamed to `{scalaFormattable, javaFormattable}`. (#1939)

## aviation

- `aviation.chronometries.unix` renamed to `unixChronometry`; `chronometries.atomic` renamed to
  `taiChronometry`. (#1939)
- `aviation.leapModes.exact` renamed to `exactLeapMode`. (#1939)
- `aviation.gapPolicies.pushBackward` renamed to `pushBackwardGapPolicy`; `gapPolicies.rejectGap`
  renamed to `rejectGapPolicy`. (#1939)
- `aviation.timespanFormats.{englishRelative, frenchRelative, germanRelative, spanishRelative}`
  renamed to `{englishRelativeTimespan, frenchRelativeTimespan, germanRelativeTimespan,
  spanishRelativeTimespan}`. (#1939)
- Newly re-exported into `soundness`: `hourFormats.{twelveHourSecondsClock,
  twentyFourHourSecondsClock}`, new family `timeSpecificities.{minutesSpecificity,
  secondsSpecificity}` (library package `aviation.timeSpecificities`), and
  `instantInterfaces.aviationInstant` / `durationInterfaces.aviationDuration`. (#1939)

## probably

- `probably.harnesses.threadLocal` renamed to `threadLocalHarness`. (#1939)
- `probably.autopsies.none` renamed to `noAutopsy`; `autopsies.contrastExpectations` renamed to
  `contrastAutopsy`. (#1939)

## iridescence

- Every member of `iridescence.mixing` gains the suffix `Mixing`: `proportional` →
  `proportionalMixing`, `multiply` → `multiplyMixing`, `screen` → `screenMixing`, `darken` →
  `darkenMixing`, `lighten` → `lightenMixing`, `difference` → `differenceMixing`, `exclusion` →
  `exclusionMixing`, `linearDodge` → `linearDodgeMixing`, `linearBurn` → `linearBurnMixing`,
  `hardLight` → `hardLightMixing`, `overlay` → `overlayMixing`, `softLight` → `softLightMixing`,
  `colorDodge` → `colorDodgeMixing`, `colorBurn` → `colorBurnMixing`. (#1939)
- Every member of `iridescence.colorimetry` gains the suffix `Colorimetry`: `incandescentTungsten`,
  `oldDirectSunlightAtNoon`, `oldDaylight`, `iccProfilePcs`, `midMorningDaylight`, `daylight`,
  `srgb`, `adobeRgb`, `northSkyDaylight`, `equalEnergy`, `daylightFluorescentF1`,
  `coolFluorescent`, `whiteFluorescent`, `warmWhiteFluorescent`, `daylightFluorescentF5`,
  `liteWhiteFluorescent`, `daylightFluorescentF7`, `d65Simulator`, `sylvaniaF40`, `d50Simulator`,
  `coolWhiteFluorescent`, `philipsTl85`, `ultralume50`, `philipsTl84`, `ultralume40`,
  `philipsTl83`, `ultralume30` become `incandescentTungstenColorimetry`, …,
  `ultralume30Colorimetry`. (#1939)

## gossamer

- `gossamer.collations.unicode` renamed to `unicodeCollation`; `collations.codepoints` renamed to
  `codepointCollation`. (#1939)

## rudiments

- New `rudiments.Atomic`, exported as `soundness.Atomic`: opaque, zero-cost wrappers over
  `java.util.concurrent.atomic`, each named for the type it holds. `Atomic.Int` wraps
  `AtomicInteger`, `Atomic.Long` wraps `AtomicLong`, `Atomic.Bool` wraps `AtomicBoolean`,
  `Atomic.Ref[value]` wraps `AtomicReference[value]`, `Atomic.Ints` wraps `AtomicIntegerArray`,
  `Atomic.Longs` wraps `AtomicLongArray`, and `Atomic.Refs[value]` wraps
  `AtomicReferenceArray[vacuous.Optional[value]]`. `LongAdder`, `DoubleAdder`, `LongAccumulator`,
  `DoubleAccumulator`, `AtomicMarkableReference`, `AtomicStampedReference` and the three
  `*FieldUpdater` classes are not wrapped. `Atomic.Int` and `Atomic.Long` shadow `scala.Int` and
  `scala.Long` under `import Atomic.*`, which is not an intended usage. (#1957)
- A match type `Atomic[value]` reduces to `Atomic.Int` for `Int`, `Atomic.Long` for `Long`,
  `Atomic.Bool` for `Boolean`, and `Atomic.Ref[value]` otherwise. It does not reduce for an
  abstract type parameter, nor for an opaque type whose representation is not visible at the use
  site — `Atomic[Text]`, `Atomic[Optional[Text]]` and `Atomic[SomeClass]` reduce, but
  `Atomic[List[x]]` does not, because the prelude's `List` is opaque. In those cases the concrete
  type is named: `Atomic.Ref[List[x]]`. `Atomic(0)`, `Atomic(0L)`,
  `Atomic(false)` and `Atomic(reference)` construct the corresponding cell; an explicit type
  argument (`Atomic[Int](0)`) applies the generic arm and does not conform. Every operation is
  `inline` and compiles to the same bytecode as the `java.util.concurrent.atomic` call it
  replaces. (#1957)
- Reads are `atomic()` (`get`), stores are `atomic() = value` (`set`) and `atomic.publish(value)`
  (`lazySet`); `atomic.swap(value)` is `getAndSet` and `atomic.replace(expected, updated)` is
  `compareAndSet`. `Atomic.Ref#apply()` returns `value` rather than `value | Null`, so no call
  site needs `.nn`; a cell declared `Atomic.Ref[Optional[x]]` reads as `Unset` when absent, where
  `.nn` on the underlying `AtomicReference` would have thrown. The three array types are indexed
  by `denominative.Ordinal`; `Atomic.Refs#apply` returns `vacuous.Optional[value]` because a fresh
  reference array is null-filled, while `Atomic.Ints#apply` and `Atomic.Longs#apply` return the
  primitive, because a fresh primitive array holds zeros. (#1957)
- Transitions are `atomic.ere(transition)`, yielding the value the transition displaced, and
  `atomic.since(transition)`, yielding the value it installed — replacing `getAndUpdate` and
  `updateAndGet` respectively. They are defined on `Atomic.Int`, `Atomic.Long`, `Atomic.Bool` and
  `Atomic.Ref`, and not on the array types. The transition must be written as a lambda literal:
  its shape is read at compiletime and replaced by the corresponding JDK intrinsic where one
  exists (`_ + 1` becomes `getAndIncrement`/`incrementAndGet`, `_ + n` becomes
  `getAndAdd`/`addAndGet`, `_ - 1` becomes `getAndDecrement`/`decrementAndGet`, a constant becomes
  `getAndSet`, and the identity becomes `get`), and otherwise by a compare-and-set retry loop with
  the transition beta-reduced into it. No `java.util.function.UnaryOperator` and no closure is
  allocated in either case. A transition may be re-run under contention and so must be pure; a
  transition which applies a function value obtained from outside is rejected at compiletime.
  (#1957)
- `ere` is overloaded to take a value directly, so `bool.ere(true)` is `getAndSet(true)` without a
  lambda. There is no `since` counterpart, which would return its own argument. The value overload
  is unreachable on an `Atomic.Ref[value]` whose `value` is a function type, where `ere` resolves
  to the transition overload and fails to compile; use `ref() = supplied`. (#1957)
- `Atomic.Ref#revise(transition)` takes a function value rather than a literal, for a transition
  whose shape cannot be read, and yields the value it installed. It is `inline`, so no closure is
  allocated, but the purity obligation is unchecked: the transition may be re-run under
  contention. (#1957)
- `rudiments.test` now depends on `mandible.core`, which asserts the bytecode `Atomic`'s
  operations compile to. No effect on `rudiments` itself. (#1960)

## proscenium

- `proscenium.Array#copyFrom(source, sourceStart, targetStart, count)` renamed to `place`, with
  two further overloads: `place(source)` copies the whole source to the start, and
  `place(source, at)` copies it to `at`. Behaviour unchanged. The name matches
  `rudiments.place` and `concordance.Scribe#place`, which already meant copying a source into
  the receiver; `snapshot` remains its counterpart, copying out into a fresh array. Indices stay
  `Int` rather than `Ordinal`, as `Array`'s `apply`/`update`/`readUnchecked` do, because
  `denominative` sits above `proscenium`. (#1960)

## concordance

- `rudiments.Scribe` can now grow. `Array.collect[element](hint)(lambda)` lends an unsized
  scribe which extends as it is written and yields exactly what was written, where
  `Array.scribe[element](size)(lambda)` continues to lend a fixed-size scribe, to return an array
  of exactly `size`, and to clamp rather than grow. Existing `Array.scribe` behaviour is
  unchanged. (#1960)
- `rudiments.Scribe#append` gains two bulk overloads at the cursor: `append(source)` and
  `append(source, from, count)`, both taking `Array[element]^{}`. `place` still requires an
  `Ordinal in scribe.type` and so remains available only to a sized scribe. (#1960)

## frontier

- `frontier.context.explainMissingContext` and `soundness.explainMissingContext` no longer
  succeed as an implicit candidate when the search resolves without them; the candidate now
  always fails (its diagnostic is used only if the whole search fails), leaving the compiler to
  select the instance itself. Effect: an implicit search made while type parameters are still
  undetermined (e.g. `join`'s `element`/`textual`) is no longer resolved with those parameters
  instantiated to `Any`. Inferred types of code that already compiled are unchanged. (#1944)

## stenography

- `stenography.Imports` gains `Imports.exports(scope: Designator)(using
  dotty.tools.dotc.core.Contexts.Context): Set[Designator]`, the targets (type and companion) of
  the `export` aliases declared in `scope`, and `Imports.resolve(designators: Set[Designator],
  direct: Set[Designator])(using Context): Imports`, which is `Imports(designators, direct)` with
  `direct` extended by `exports` of each of `designators`. `delicious.Reifier` gains
  `imports(designators: Set[Designator], direct: Set[Designator]): Imports`, `Imports.resolve`
  under the reifier's own context. Code which builds an `Imports` by hand from a set of wildcard
  imports for `Syntax#text` or `Designator#text` (a REPL abbreviating types against its session's
  imports) should build it through `Reifier#imports` or `Imports.resolve` instead: only then does
  a type reached through an `export` in a wildcard-imported scope (`jacinta.Json` under `import
  soundness.*`) render by its leaf name (`Json`) rather than qualified (`jacinta.Json`). The
  macro path (`Syntax.name`, `Syntax.designator`) already did this and is unchanged. (#1959)

## stratiform

- BinTEL framing changed (TEL upstream 8380ef7): `Bintel.frame` and `Bintel.frameSelfContained`
  now write a document-length varint immediately after the magic number, counting every byte
  that follows it. Frames written before this change fail to decode with
  `Bintel.Error.Reason.BadSignatureLength` (B03). Re-encode stored BinTEL documents. (#1961)

- `Bintel.Framed(signature, body)` gained a third field, `continuation: Int`, the offset at
  which the bytes after the document begin; `Bintel.Document(signature, root)` likewise gained
  `continuation: Int`. Pattern matches on either must bind three fields. (#1961)

- `Bintel.decodeDocument` and `Bintel.decodeDocumentSelfContained` are now single-document
  readers: bytes after the document are neither decoded nor rejected, and the result's
  `continuation` reports where they begin. To reject trailing bytes as before (B08), use the
  new `Bintel.decodeWholeDocument(data, schema)` / `Bintel.decodeWholeDocumentSelfContained(data)`
  (each also with `(…, codecs: Tel.Codec.Bindings, checkCanonical: Boolean = false)`). Both
  single-document readers gained overloads taking an `offset: Int` (as the last parameter).
  `Bintel.unframe` likewise no longer rejects trailing bytes and gained `unframe(data, offset)`. (#1961)

- New: `Bintel.decodeStream(data, schema): List[Bintel.Document]` (and a codec-aware overload)
  decodes every document of a byte stream in order, dispatching self-contained documents to
  their embedded schema; `Bintel.documentExtent(data)` / `documentExtent(data, offset)` returns
  a document's continuation offset from its header alone, without a schema. (#1961)

- `Bintel.Error.Reason` gained `DeclaredLengthMismatch` (`SN-609.16`, B16: declared length
  disagrees with the structural extent) and `NestingLimitExceeded` (`SN-609.17`: more than 256
  nested Structs; a decoder resource limit, not a B-code). The `TrailingBytes` message now
  reads "a whole-document reader found bytes after the document". (#1961)

- BinTEL varint decoding (`Varint.decode`, `Bintel.decode`, `Bintel.parse`) now rejects
  non-minimal encodings such as `80 00` (new `Varint.Error.Reason.Overlong`, `SN-608.3`;
  reported as `Bintel.Error.Reason.VarintError`, B02). A varint with no bytes available is now
  `VarintError` (B02) rather than `UnexpectedEoi` (B09). (#1961)

- `Bintel.decode`/`Bintel.encode` now raise `Bintel.Error.Reason.ReferenceUnresolved` (B10) for
  a `Tels.SelectRef` whose name is not a `SelectDefinition` of the schema; previously such a
  member silently contributed no keyword slots. (#1961)

- `Tel.Type.assign` now orders a Node's children by member order (TEL §18.3): every element
  filling `members[0]`, then `members[1]`, …, atoms before compounds within a member, source
  order otherwise. Previously all atom-derived elements preceded all compound-derived elements,
  each group in document order. Code that relied on document order of `Tel.Element.Node.children`
  must sort by source position itself. (#1961)

- `Tel.Document` gained `margin: Int` (fourth positional field, before `children`) and
  `continuation: Optional[Int] = Unset` (last field: the 1-indexed line on which the content
  after the document separator begins, or `Unset` when the document ended at end of input or
  was constructed rather than parsed). Constructor calls and pattern matches must be updated. (#1961)

- Layer composition (`Tels.Layers.compose`) now raises `Tel.Error.Reason.LayerKeywordCollision`
  (E205) when a layer field's keyword matches a variant keyword contributed by an existing
  `SelectRef`, or when a layer `SelectRef` brings a variant keyword already present; previously
  the former surfaced later as E201 (`DuplicateKeywordInStruct`) and the latter was not
  detected. `Tels.Validation.validate` now raises `EmptySelectVariants` (E202) for a select
  declared in a layer with neither variants nor excludes. (#1961)

- Parser diagnostics reassigned: a line indented under a comments-only block is now E111
  (`OverIndentation`) rather than E112 (`ChildOfNonCompound`), which is reserved for a line
  indented below a tabulated row; a tabulated row whose keyword-and-pre-column-atom portion, or
  any column value, occupies a column separator position (`M_i − 2` or `M_i − 1`) is now E118
  (`ColumnValueTooWide`). A literal atom's closing delimiter line is now the opening line with
  trailing spaces removed. (#1961)

- `Tels.Validation.validate` now raises `DuplicateDefinition` (E210) when two definitions of a
  base schema, or of one layer, share a name across its records, scalars and selects;
  previously only a layer definition duplicating a base one was detected. (#1961)

- Pragma diagnostics: `BadPragmaPhrase` (E121) and `MisplacedPragmaPhrase` (E122) are each
  accrued at most once per pragma line, at the first offending phrase, rather than once per
  phrase; and a single-character phrase that is not the final phrase is now E121 rather than
  E122 (the sigil form is final by definition). (#1961)

- A comment line inside a tabulated block (E109, `CommentNotPreceded`) now joins that block's
  `comments` and the following rows remain rows of the same block; previously it began a new
  block and later rows were parsed as ordinary compounds. (#1961)

- `Telp.delimiters` gained `+` (now twenty-two characters, `!"#$%&*+,./:;=?@\^_`|~`), so
  `Telp.parse(t"+a+b")` succeeds where it previously raised `Telp.Error.Reason.Syntax`. (#1961)

- `Base256.decodeStrict` reports every offending character, not only the first, as
  `Base256.Error.Reason.NotInAlphabet(offenders: scala.collection.immutable.List[Base256.Offender])`
  where `Base256.Offender(position: Int, codepoint: Int)` gives a zero-based *code-point* index
  (previously `NotInAlphabet(position: Int, character: Char)` with a UTF-16 unit index).
  `Base256.decode` and `decodeStrict` now treat a supplementary character as one character
  yielding one byte (previously two).

## ulysses (#1961)

- `Palimpsest.resolve` now bounds its backtracking search to `Palimpsest.searchLimit` (100,000)
  candidate trials and returns `Unset` on exhaustion; a new overload `resolve(limit: Int)(using
  Bibliography)` takes an explicit bound. (#1961)

## ethereal

- The launcher-to-daemon protocol is now BinTEL documents of the `ethereal-launcher` TEL
  schema (`ethereal.Launcher.schemaText`), replacing the line-oriented `i`/`e`/`m`/`s`/`x`/`v`
  messages and single-byte replies. A daemon built from this version only accepts launchers
  built from the Rust runner source at this version: rebuild runner stubs (`make
  runners-build`) or use a published `runners-<version>` release at or after the one carrying
  this protocol. Applications packaged with older stubs must be repackaged.
- `ethereal.DaemonEvent` removed. The daemon dispatches on `ethereal.Launcher.Message`
  (`Init`, `Stderr`, `Control`, `Signal`, `Exit`, `Verify`, `SignalAck`, `Verdict`, `Mode`,
  `ExitStatus`); `Launcher.encode`, `Launcher.decode` and `Launcher.readDocument` are the
  codec. `ethereal.Tty#byte` removed: a terminal-mode change is a `Launcher.Message.Mode`
  document on the control channel.
- `ethereal.Runners.version` is `0.4` (the first runner release speaking the BinTEL protocol),
  with `etc/runners/0.4.tsv` recording the stub hashes; `-Dbuild.executable` packaging without
  a local `dist/runners` downloads from the `runners-0.4` GitHub release.
- `ethereal.DaemonLogEvent` gained `ProtocolMismatch` (a document of another schema's
  signature).
- `ethereal.core` now depends on `stratiform.binary` (and so on `stratiform.core`,
  `stratiform.base256` and `ulysses.core`).

## probably (fume takes over running and reporting)

- `probably.Suite#main(arguments: Array[Text]): Unit` removed. A `Suite` is no longer a main
  class: it is run only by a host through
  `probably.Streamer.stream(suite: Text, arguments: Text, output: java.io.OutputStream): Int`
  (module `probably.events`) or `probably.Suite#invoke(arguments: Text, sink: TestEvent -> Unit): Int`.
  Build scripts that ran `java -cp … <package>.Tests …` must run the `fume` tool over the built
  classpath instead (`fume run -c <jar>`, or `fume run` with a `.fume/config.tel` naming it). (#TBD)
- `probably.Suite#invoke(arguments: Array[Text]): Int`, `probably.Suite#invoke(arguments: Text): Int`
  and `probably.Suite#invoke(arguments: List[Text]): Int` removed; they rendered a report to the
  suite's stdio, which no longer exists. `invoke(arguments: Text, sink: TestEvent -> Unit): Int`
  remains as the only entry point. (#TBD)
- `probably.Suite#invoke(arguments: Text, sink: TestEvent -> Unit): Int` no longer returns 3
  (environment error during reporting); results are 0 (passed), 1 (failures) or 2 (the suite
  threw). (#TBD)
- `probably.Suite#suiteIo: turbulence.Stdio` removed. (#TBD)
- `probably.Report` constructor changed from `Report()(using ambience.Environment)(using probably.TestPalette)`
  to `Report()`; no givens are required. (#TBD)
- `probably.Report#complete(coverage: Option[probably.Coverage])(using turbulence.Stdio): Unit`
  changed to `complete(): Unit`. It renders nothing; it settles `passed` and emits
  `TestEvent.RunCompleted` to the installed sink (a no-op without one). (#TBD)
- `probably.Report.Status` (enum `Pass, Fail, Throws, CheckThrows, Mixed, Suite, Bench, Stress,
  Profile, AspirePass, AspireFail`, with `symbol(using TestPalette): Teletype` and
  `describe: Teletype`) removed. `TestEvent.Outcome#outcome: Text` carries the per-verdict
  vocabulary on the event stream. (#TBD)
- `probably.Reporter.report` given changed from
  `(turbulence.Stdio, ambience.Environment, probably.TestPalette) => Reporter[Report]` to
  `Reporter[Report]`. (#TBD)
- `probably.Reporter[report]#live(report: report): Boolean` removed. (#TBD)
- `probably.Runner[report]#redraw(size: Int): Unit` removed; `Runner` no longer writes progress
  to stdout under any condition. (#TBD)
- `probably.TestPalette` (trait extending `chiaroscuro.JuxtapositionPalette`; members `warning,
  critical, benchmark, mixed, informative, cold, warm, hot, accented, highlight, detail, pass,
  fail, aspirePass, aspireFail, subdued, unaccented, positive, negative: Color in Srgb`) removed,
  together with `soundness.TestPalette`. (#TBD)
- `probably.Ci` (object; members `apply(), githubActions, gitlabCi, circleCi, travisCi, jenkins,
  azurePipelines, teamCity, bitbucketPipelines, buildkite, appVeyor, drone, semaphore, buddy,
  claudeCode: Boolean`) removed, together with `soundness.Ci`. No replacement. (#TBD)
- `probably.GithubActions` (object; members `workspaceRelative(path: Text): Text`,
  `error/warning/notice(message: Text, file: Optional[Text], line: Optional[Int], title: Optional[Text])(using Stdio): Unit`,
  `debug(message: Text)(using Stdio): Unit`, `group(title: Text)(using Stdio): Unit`,
  `endGroup()(using Stdio): Unit`, `grouped[result](title: Text)(block: => result)(using Stdio): result`)
  removed, together with `soundness.GithubActions`. No replacement: a host renders annotations
  from the `TestEvent` stream. (#TBD)
- `probably.AnsiRenderer` and `probably.TerseRenderer` removed; `probably` no longer renders
  reports. `probably.Documenting`, `probably.Doc` and `probably.Format` (all `private[probably]`)
  removed. The `CLAUDECODE` and `COLUMNS` environment variables and the `-Dscalac.coverage`
  system property no longer affect a suite. (#TBD)
- Module `probably.coverage` (artifact `probably-coverage`) removed, with `probably.Coverage`
  (object and `case class Coverage(path: Text, spec: Array[Juncture], oldHits: Set[Int], hits: Set[Int])`),
  `probably.Juncture`, `probably.Surface` and the `soundness.{Coverage, Juncture, Surface}`
  exports. The `soundness-test` bundle no longer contains it. No replacement. (#TBD)
- `probably.core` no longer depends on `chiaroscuro.render`, `ambience.core`, `digression.ansi`
  or `escapade.io` (and so, transitively, on `escritoire.core`, `dendrology.tree`,
  `iridescence.core` or `turbulence.core`); it depends on `digression.core` directly. A
  downstream module that obtained any of those through `probably` must declare them. (#TBD)

## hellenism

- New: `hellenism.LocalClasspath.of(classloader: Classloader)(using ambience.System): LocalClasspath`
  — the classpath a `URLClassLoader` loads from, or else the `java.class.path` property. Code that
  launches a JVM to run its own classes (`sh"java -classpath …"`) should derive the classpath from
  this, with the classloader of one of its own classes (`Classloader[MyObject.type]`), rather than
  read the property: under a test-running host such as fume the property names the host's jars.
  `anthology.Bundler.applicationClasspath` and `superlunary.Rig#classpath` now go through it
  (same result as before). (#TBD)

## superlunary

- `superlunary.Jvm` and `superlunary.Isolation` now pass `Rig` the classloader that loaded them
  (`Classloader[Jvm.type]` / `Classloader[Isolation.type]`) instead of
  `hellenism.classloaders.systemClassloader`, and `superlunary.Rig#classpath` derives its entries
  from `Classloader[Rig]` rather than the thread-context classloader. Identical in a plain
  `java -cp` process; differs only when the rig is loaded by a non-system classloader. (#TBD)
