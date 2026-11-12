# API nesting proposal

Supersedes the C1, C3 and C3b sections of `api-reduction-candidates.md` (July 2026). That
inventory predates the modularity drive, which both moved names between components and
introduced the constraint that governs feasibility here: **a nested type must be declared
inside its outer type's companion, so both must live in the same build component.** This
proposal was regenerated from the current export surface (2026-08-11): 823 multi-word
type-level names exported into `package soundness` (July's C1 examined a 163-name subset,
of which `LspServer` no longer exists and `TerminalCanvas` is now `TerminalBoard`). The
`Pem.Error`/`Pem.Label` pilot is done; its gotchas are retained in the Mechanics section.

Multi-word *term* identifiers (665) are out of scope for this round: they are mostly
givens, whose placement was deliberately settled by the #1632 orphan-given work, and any
renaming there should follow the given-placement rule in CLAUDE.md rather than this one.

## Rules

The split point is **semantic, not lexical**: `PlayScene` becomes `Play.Scene` only when `Play`
is a meaningful concept on its own, and a compound that is itself an established concept
does not split at all — it becomes a namespace for its own satellites.

- **R1 SPLIT**: `Play` names a concept, normally an existing type/object in the same
  component. `GitError → Git.Error`.
- **R2 KEEP + NAMESPACE**: the compound is an established concept independent of its
  prefix. `JsonPointer` (RFC 6901) does not become `Json.Pointer`; instead
  `JsonPointerError → JsonPointer.Error`. Likewise `MediaType`, `JsonSchema` and
  `XmlSchema` (external standards), `Httpd`, `BlockCipher`, `CompileError`,
  `WebDriver`, `WorkingDirectory`, `DomainSocket` (since renamed `Uds`), `JsonRpc`, `OAuth`, `StackTrace`,
  `SymmetricKey` (parallel with `PrivateKey`/`PublicKey`; the
  `Symmetric` trait is a separate abstraction). This resolves July's internal conflict:
  it proposed both `MediaType → Media.Type` and `MediaTypeError → MediaType.Error`.

  **The test is whether the compound names a thing with its own specification**, not
  whether its prefix happens to be a type in the same library. `YamlPath` and `TelPath`
  stay whole for exactly the reason `JsonPointer` does: a YAML path and a TEL path are
  addressing languages defined in their own right, and a reader who knows the concept
  knows it by the compound name. `Yaml.Error` is different — an error *of* the YAML
  parser, meaningful only relative to `Yaml`. The distinction is subtle and cannot be
  read off the syntax: ask whether the compound would appear as a heading in a
  specification.
- **R3 KEEP, NO NAMESPACE**: established compounds and sentence-like names with no
  satellites: `LineSeparation`, the filesystemOptions markers
  (`CreateNonexistentParents`, `DeleteRecursively`, …), `InitializationVector`,
  `BloomFilter`, `LruCache`, `SshUrl`, `SourceCode`, `ProgrammingLanguage`, compass
  points, nomenclature's `Must*` constraint sentences.
- **R4 SEMANTIC JOINT**: the joint may sit below the first word, producing two-level
  nesting where each segment is a real type: `HttpRequestError → Http.Request.Error`
  (`Http.Request` already exists), `HttpResponseError → Http.Response.Error`,
  `YamlPathError → Yaml.Path.Error`.
- **R5 EXCLUDED BUCKETS**: proscenium's stdlib mirrors (`ClassTag`, `TreeMap`,
  `ArrowAssoc`, `*HasAsScala`, …); embarcadero.containerd's 38 gRPC protocol mirrors
  (`*Request`/`*Response`, `AnyMessage`, …); aviation's calendar/format vocabulary
  (`CopticCalendar`, `HebrewMonth`, `LeapMode`, …); diuretic's `Java*` interop names;
  the `Wasi*Api` interface types; the `*Palette` family (deliberately cross-component:
  the subject in core, the palette in the ansi/render component).
- **R6 COMPONENT-BLOCKED**: outer and member in different components (or the outer nested
  inside an `internal` object, or platform-split source dirs) → the compound name stays.
  Full table below.
- **R7 SINGLE-GIVEN BACKING TYPES — retired.** This proposed eliminating a named type that
  exists only to back one `given`, letting the given instantiate an anonymous class.
  Measured against the tree, essentially none of the ~90 such types can be, and the reasons
  are specific and instructive; see the R7 section below. Where such a type is same-component
  it may still be *nested*, which several passes have done.

- **R8 OVER-GENERIC SINGLE WORD**: a single-word top-level name whose meaning is one
  library's notion, not the general concept the bare word suggests, nests under that
  library's owner even though it has no prefix to strip. The test: would a reader seeing the
  name unqualified under `import soundness.*` expect something more general than this?
  ultimatum's layout tree is not *the* `Frame`; probably's test-axis coordinate is not *the*
  `Value`. The counterweight is convenience: a library's principal type, or a commonly-used
  concept that really is general (`Data`, `Path`, `Stream`, `Session`, `Table`, `Color`),
  stays top-level. A name exported by two libraries into the umbrella is an R8 case by
  definition, and so is an `@unexported` that exists only to dodge a clash. Candidates are in
  the "Ninth-pass review" section below.

## The rename table (R1/R2/R4) — all verified same-component

Errors, events and satellites nesting under an existing companion (the dominant shape):

| library.component | renames |
|---|---|
| acyclicity.core | `DagError→Dag.Error`, `DotId→Dot.Id`, `DotIdentifier→Dot.Identifier` (⚑ `Id`/`Identifier` near-duplicates — merge candidate) |
| ambience.core | `EnvironmentError→Environment.Error`, `PropertyError→Property.Error`, `WorkingDirectoryError→WorkingDirectory.Error` |
| anthology.core | `CompilerError→Compiler.Error` (new companion for `trait Compiler`) |
| anthology.java | `JavacOption→Javac.Option` |
| apoplexy.core | `ApiError→Api.Error`, `OpenApiError→OpenApi.Error` |
| aviation.core | `TimezoneError→Timezone.Error`, `TzdbError→Tzdb.Error` |
| bitumen.core | `TarError→Tar.Error`, `TarCompression→Tar.Compression`, `TarBody→Tar.Body`, `TarFlag→Tar.Flag`, `TarHandle→Tar.Handle`, `TarRef→Tar.Ref` |
| cacophony.core | `AudioError→Audio.Error`, `FeedError→Feed.Error`, `OutletError→Outlet.Error` |
| caduceus.core | `CourierError→Courier.Error` |
| caesura.core | `DsvError→Dsv.Error`, `DsvFormat→Dsv.Format`, `DsvRedesignation→Dsv.Redesignation` |
| capricious.core | `RandomSize→Random.Size` |
| cataclysm.core | `CssError→Css.Error`, `CssErrors→Css.Errors`, `CssConvertible→Css.Convertible`, `SyntaxMatcher→Syntax.Matcher` (⚑ `Syntax` exists but is unexported; export it or skip) |
| coaxial.core | `DomainSocketEndpoint→DomainSocket.Endpoint` (`ConnectionError` stays: the ⚑ resolved negatively — `Connection` is a case class in coaxial.**jvm**, so R6 blocks it) |
| coaxial.jvm | `TlsAcceptance→Tls.Acceptance` |
| telekinesis.http2 | `Http2Connection→Http2.Connection`, `Http2Error→Http2.Error`, `Http2Event→Http2.Event`, `Http2Stream→Http2.Stream`, `Http2ServerConnection→Http2.ServerConnection`, `HpackTable→Hpack.Table`, `HpackEntry→Hpack.Entry` |
| digression.core | `FqcnError→Fqcn.Error` |
| dissonance.core | `DiffError→Diff.Error`, `RedraftError→Redraft.Error` |
| embarcadero.oci | `ImageConfig→Image.Config`, `ImageHandle→Image.Handle`, `OciError→Oci.Error` |
| enigmatic.core | `CryptoError→Crypto.Error`, `CipherSession→Cipher.Session` (new companion), `BlockCipherMode→BlockCipher.Mode`, `BlockCipherPadding→BlockCipher.Padding` (new companion) |
| enigmatic.asn1 | `Asn1Error→Asn1.Error` |
| enigmatic.cose | `CoseAlgorithm→Cose.Algorithm`, `CoseAuthenticator→Cose.Authenticator`, `CoseContext→Cose.Context`, `CoseError→Cose.Error`, `CoseMaced→Cose.Maced`, `CoseRecipient→Cose.Recipient`, `CoseSigned→Cose.Signed`, `CoseStructure→Cose.Structure`, `CoseTag→Cose.Tag`, `CoseVerifier→Cose.Verifier` |
| escritoire.core | `ColumnAlignment→Column.Alignment` |
| ethereal.core | `UpgradeError→Upgrade.Error` |
| exegesis.core | `LspClient→Lsp.Client`, `LspConnection→Lsp.Connection`, `LspDispatch→Lsp.Dispatch`, `LspError→Lsp.Error`, `LspProxy→Lsp.Proxy`, `LspRegistry→Lsp.Registry` |
| facsimile.core | `PdfError→Pdf.Error`, `PdfFont→Pdf.Font`, `PdfInfo→Pdf.Info`, `PdfMatrix→Pdf.Matrix`, `PdfOperator→Pdf.Operator`, `PdfRect→Pdf.Rect` |
| geodesy.core | `GeolocationError→Geolocation.Error` |
| gesticulate.core | `MediaTypeError→MediaType.Error`, `MultipartError→Multipart.Error` |
| guillotine.core | `PidError→Pid.Error`, `ProcessInput→Process.Input`, `ProcessRef→Process.Ref` |
| hallucination.core | `RasterError→Raster.Error`, `RasterFormats→Raster.Formats` |
| hellenism.core | `ClasspathEntry→Classpath.Entry`, `ClasspathError→Classpath.Error`, `ClasspathEvent→Classpath.Event` |
| hieroglyph.core | `CharsetError→Charset.Error`, `CodepageError→Codepage.Error` (nesting under the typeclass that raises them) |
| inimitable.core | `UuidError→Uuid.Error` |
| jacinta.core | `JsonError→Json.Error`, `JsonPrimitive→Json.Primitive`, `JsonPointerError→JsonPointer.Error` |
| jacinta.records | `JsonBlueprintDoc→JsonBlueprint.Doc`, `JsonBlueprintError→JsonBlueprint.Error` |
| kaleidoscope.core | `RegexError→Regex.Error`, `GlobToken→Glob.Token` |
| larceny.plugin | `CompileErrorId→CompileError.Id` |
| legerdemain.query | `QueryError→Query.Error` |
| locomotion.core | `ProtobufError→Protobuf.Error` |
| mandible.core | `ClassfileError→Classfile.Error` |
| metamorphose.core | `PermutationError→Permutation.Error` |
| nomenclature.lexicon | `MonikerError→Moniker.Error` |
| obligatory.grpc | `GrpcChannel→Grpc.Channel`, `GrpcError→Grpc.Error`, `GrpcFraming→Grpc.Framing` |
| obligatory.json | `JsonRpcError→JsonRpc.Error`, `SseError→Sse.Error`, `SseSource→Sse.Source` |
| octogenarian.core | `GitBranch→Git.Branch`, `GitCommand→Git.Command`, `GitError→Git.Error`, `GitEvent→Git.Event`, `GitHash→Git.Hash`, `GitPathStatus→Git.PathStatus`, `GitProcess→Git.Process`, `GitRefs→Git.Refs`, `GitRepo→Git.Repo`, `GitStatus→Git.Status`, `GitTag→Git.Tag`, `GitRefError→Git.RefError` (⚑ no `GitRef` type exists; `Git.Ref.Error` would mean inventing one — decide at execution) |
| orthodoxy.core | `OAuthError→OAuth.Error` |
| parasite.core | `AsyncError→Async.Error` |
| perihelion.core | `WebsocketError→Websocket.Error`, `WebsocketEvent→Websocket.Event` (⚑ suggestion: also `WsConnection→Websocket.Connection`, `WsUrl→Websocket.Url` — the `Ws` prefix duplicates the `Websocket` concept) |
| plutocrat.core | `CurrencyStyle→Currency.Style` |
| polysyllabic.core | `HyphenationError→Hyphenation.Error` |
| probably.core | `TestId→Test.Id` |
| profanity.core | `TerminalError→Terminal.Error`, `TerminalEvent→Terminal.Event`, `TerminalFeature→Terminal.Feature`, `TerminalInfo→Terminal.Info` (`TerminalBoard`/`InlineBoard` stay: they are `Board` variants, not Terminal satellites) |
| revolution.core | `ManifestAttribute→Manifest.Attribute`, `ManifestEntry→Manifest.Entry`, `SemverError→Semver.Error` |
| savagery.core | `SvgError→Svg.Error`, `SvgParser→Svg.Parser` |
| scintillate.server | `HttpServerEvent→Httpd.Event` (new companion for `Httpd`) |
| scintillate.servlet | `JavaServletFn→JavaServlet.Fn` |
| sedentary.core | `BenchError→Bench.Error` |
| serpentine.core | `PathError→Path.Error` |
| stratiform.core | `MutationError→Mutation.Error`, `TelError→Tel.Error`, `TelFlag→Tel.Flag`, `TelHandle→Tel.Handle` |
| stratiform.base256 | `Base256Error→Base256.Error` |
| stratiform.binary | `BintelError→Bintel.Error`, `VarintError→Varint.Error` |
| synesthesia.core | `McpClient→Mcp.Client`, `McpError→Mcp.Error`, `McpServer→Mcp.Server`, `McpSession→Mcp.Session`, `McpSpecification→Mcp.Specification` |
| tarantula.core | `WebDriverError→WebDriver.Error`, `WebDriverSession→WebDriver.Session`, `WebElement→WebDriver.Element`, `ShadowRoot→WebDriver.ShadowRoot` |
| telekinesis.core | `HttpClient→Http.Client`, `HttpError→Http.Error`, `HttpEvent→Http.Event`, `HttpRedirection→Http.Redirection`, `HttpRequestError→Http.Request.Error`, `HttpResponseError→Http.Response.Error` |
| urticose.core | `HostnameError→Hostname.Error`, `EmailAddressError→EmailAddress.Error`, `MacAddressError→MacAddress.Error`, `NetworkInterfaceError→NetworkInterface.Error` |
| urticose.url | `UrlError→Url.Error`, `UrlFragment→Url.Fragment` |
| xenophile.typescript | `TypescriptDeclaration→Typescript.Declaration` (with `Declared`, its sealed supertype), `TypescriptError→Typescript.Error`, `TypescriptMember→Typescript.Member`, `TypescriptType→Typescript.Type`, `TypescriptParser→Typescript.Parser` |
| xenophile.webidl | `WebIdlDefinition→WebIdl.Definition`, `WebIdlArgument→WebIdl.Argument`, `WebIdlMember→WebIdl.Member`, `WebIdlField→WebIdl.Field`, `WebIdlError→WebIdl.Error`, `WebIdlParser→WebIdl.Parser` (`WebIdlDialect` stays: reflective) |
| xenophile.wit | `WitFunction→Wit.Function`, `WitItem→Wit.Item`, `WitInterface→Wit.Interface`, `WitWorldModel→Wit.WorldModel`, `WitDocument→Wit.Document`, `WitParseError→Wit.ParseError`, `WitParser→Wit.Parser` (⚑ this row read `WitDeclaration→Wit.Declaration` until the third pass; no type of that name has ever existed — `xenophile.WitDeclaration.scala` is a container named after a type it does not declare. `WitDialect` stays: reflective) |
| xylophone.core | `XmlError→Xml.Error`, `XPathError→XPath.Error` |
| yossarian.core | `PtyEscapeError→Pty.EscapeError`, `PtyState→Pty.State` |
| ypsiloid.core | `YamlError→Yaml.Error`, `YamlPrimitive→Yaml.Primitive`, `YamlPathError→YamlPath.Error` (`YamlPath` stays whole, per R2) |
| zeppelin.core | `ZipError→Zip.Error`, `ZipEvent→Zip.Event`, `ZipHandle→Zip.Handle` (deferred: `ZipOpenable`/`ZipDataOpenable` share `ZipHandle`'s file — hoist first) |

## Kept whole after verification — mostly overturned by later passes

This section listed thirty-odd names as having "no meaningful same-component outer type".
**Twenty-six of them have since moved.** The verification behind it was a search for a
top-level type of the same name, which missed opaque types, companions nested inside an
`internal` object, platform source directories mistaken for components, and — most often —
a perfectly good host under a *different* name than the error's own prefix.

What the later passes actually did with them:

- an object already existed, or a companion was added: `RetryError→Tenacity.Error`,
  `IsinError→Isin.Error`, `AuthError→Auth.Error`, `BindError→Bind.Error`,
  `SerializationError→Serialization.Error`, `DecimalError→Decimal.Error`,
  `RangeError→Range.Error`, `UnsetError→Optional.Error`, `DataError→Database.Error`,
  `RemoteError→Rig.Error`, `DegustationError→Inspection.Error`, `FontError→Font.Error`,
  `RruleError→Rrule.Error`, `RecurrenceError→Recurrence.Error`,
  `OfflineError→Internet.Error`, `CertificateError→Certificate.Error`,
  `LinkError→Link.Error`, `TableError→Table.Error`, `IpAddressError→IpAddress.Error`,
  `TimestampError→Timestamp.Error`, `RpcError→Rpc.Error`, `FrameError→Framing.Error`,
  `IoError`/`IoEvent`→`Io.Error`/`Io.Event`
- two names turned out to be one concept: `DivisionError` and `OverflowError` are
  `Arithmetic.Error` with distinct `Reason`s
- one was a duplicate of a reason the codebase already had, and was **deleted**:
  `BoundsError` restated `JsonBlueprint.Error.Reason.IntOutOfRange`

Still compound, and for reasons now recorded in `api-reduction-candidates.md`:
`ConnectError`, `UncheckedError`, `LinkEvent` and `CompileError` (excluded under R2).
`EscapeError`, `InstallError` and `StreamError` joined the moved list as
`TextEscapes.Error`, `Install.Error` and `Truncation.Error`.

**The lesson for the next pass**: "no outer type of that name exists" is not the same as
"no host exists". Resolve the outer name to a file and a component, and consider hosts whose
name differs from the error's prefix, before recording anything as blocked.

## Component-blocked (R6) — compound names stay

| name | member component | outer (component) |
|---|---|---|
| `JsonSchema` | jacinta.schema | `Json` (core) — also R2: external standard |
| `TarOpenable`, `TarBuilder` | bitumen.jvm | `Tar` (core) |
| `PdfFile` | facsimile.file | `Pdf` (core) — keeps its own `PdfFile.Origin` |
| `RasterOpenable`, `CanvasHandle` | hallucination.canvas | `Raster`/`Canvas` (core) |
| `PngBackend` etc. (×4) | png-jvm/png-native source dirs | `Png` (png) — dual platform definitions |
| `HmacCipher` | enigmatic.cose | `Hmac` (core) |
| `KeystoreError` | enigmatic core (shared) | `Keystore` (core-jvm sources only) |
| `MarkdownPalette`, `StackTracePalette` | ansi components | core types (also R5 palette family) |
| `ClasspathJvm` | hellenism.jvm | `Classpath` (core) |
| `BintelInlinable` | stratiform.binaryStaged | `Bintel` (binary) |
| `ForeignLibrary`, `ForeignBuffer` | xenophile native + nativeruntime | `Foreign` (core) — dual definitions |
| `WitCase`, `WitError`, `WitHandle`, `WitVariant` | xenophile.wasm | `Wit` (wit) |
| `*Atomizer`, `*Discipline` (Wit/WebIdl/Kotlin/CHeader/Classfile/Dts) | xenophile.lira / mandible.lira | respective dialect components |
| `ImageOpenable` | embarcadero.oci-jvm | `Image` (oci) |
| `ImageRecord` | embarcadero.containerd | `Image` (oci) |
| `WasiRandom` | capricious.wasi | `Random` (core) |
| `CompileFlag` | anthology.scala | anthology.core |
| `ToolchainError` | anthology.linker | `Toolchain` (core) |
| `CliEvent` | exoskeleton.completions | `Cli` (args) |
| `HttpSession` | telekinesis.jvm | `Http` (core) |
| `HttpConnection` | scintillate.server | `Http` (telekinesis — cross-library) |

## R7 — retired: the single-given backing types cannot be inlined

R7 proposed eliminating named types that exist only to be instantiated in one `given`,
letting the given instantiate an anonymous class. **Measured against the tree, essentially
none of them can be**, and the three families it named each fail for a different, specific
reason. The rule is retired rather than left as standing advice.

The families, as they stand: `*Openable` (25 declarations), `*Derivation` (33),
`*Tactic` (13), `*Creatable` (10), `*Sessional` (9).

- **`*Openable`, `*Sessional`, `*Creatable` — named on purpose, and the code says so.**
  Twenty files carry a comment to the effect that "an anonymous subclass would freshen the
  capability types in its inferred `Result` member", among them galilei's `FileOpenable`,
  hallucination's `RasterOpenable`, telekinesis's `Http2`, perihelion, zeppelin's `Jar` and
  `Zip`, embarcadero's `Workload` and tarantula's `WebDriver`. These are not incidental
  carriers; the name is what pins the capability type.
- **`*Derivation` — forbidden by the compiler.** Their `conjunction` method must be
  `inline`, and moving the object into the `inline given` that uses it nests one inline
  method inside another: *"Implementation restriction: nested inline methods are not
  supported"*. Verified by attempting it on wisteria's `AddableDerivation`.
- **`*Tactic` — mostly not single-use, and load-bearing where it is.** `ThrowTactic` has 26
  references, `HaltTactic` 15, `OptionalTactic` and `AsyncTactic` 10 each. `ThrowTactic`
  further extends `caps.Unscoped` with a comment explaining that the capture behaviour is
  the reason the class exists, and tactic names appear in user-facing error messages.

A general scan for types whose only external use is inside a `given` returns six names, and
none survives inspection: `Vp8Encoder` and `WebpEncoder` are utility objects called for
their methods, `JavaIdentifier` is a phantom type used in a `Nominative under …` bound,
`HeapCloak` is a deliberately stable singleton (its whole point is an empty capture set) and
is already `private[enigmatic]`, and `FatalTactic`/`UncheckedTactic` are tactics as above.

The residual value in the original R7 observation is the opposite of what it proposed: where
a type exists only to back a given, that is usually *evidence of a capture-checking
constraint*, and the name should be read as load-bearing until proven otherwise.

## Mechanics

- **Consequent rules**: L2 (one toplevel type per file, `<module>.<Type>.scala` — delete
  the donor file when its type nests); L3 (companion `object` precedes its
  `class`/`trait`/`enum`, including nested pairs); F1 licence header. Enforced by the
  published `dev.propensive:consequent` plugin.
- **Error-nesting gotcha** (from the pilot): a nested `case class Error` makes the bare
  `Error` in its own `extends` clause self-referential — qualify as
  `extends fulminate.Error(...)`. Move the donor file's imports with the body.
- **New companions needed** (L3-ordered before their types): `Cipher`, `BlockCipher`,
  `Compiler`, `Httpd`.
- **Mega-file mitigation**: `ypsiloid.Yaml.scala` and `stratiform.Tel.scala` are ~6,100
  lines. Nesting their satellites is in scope; keep the files manageable by hoisting
  existing companion bulk into mixin traits in their own files — the codebase already
  does this (`object Yaml extends Yaml2, Dynamic`, `object Tel extends Tel2`).
- **Self-import gotcha**: `octogenarian.Git.scala` opens with
  `import GitError.Reason.*`, which becomes a self-import when `GitError` nests.
- **Pre-existing L3 anomaly to fix en route**: `hallucination.RasterFormats.scala`
  declares the case class before its object.
- **Export files**: every rename deletes the old name from the component's
  `soundness_*.scala` in the same commit (duplicate toplevel exports resolve silently by
  classpath order); the nested member is reachable through the exported outer name.
- **Enum cases** resolve unqualified inside a nested companion exactly as at top level.
- Always `grep -a` when sweeping (eleven source files contain literal NUL bytes).

## Corrections from the first implementation pass (2026-08-11)

Seventy of these renames are done (octogenarian, and the single-namespace, Tar, Css,
Raster, Classpath, MediaType, Svg and Pty families). Five proposed entries did not
survive contact, and the reasons generalise:

- **`BaseLayout` is not a satellite.** `object Base extends BaseLayout(…)`, so the layout
  is Base's *supertype*; nesting makes the object extend its own member. A shared prefix is
  not by itself evidence of a satellite relationship — check the inheritance direction.
- **`TarHeader` fails capture inference.** Its block-parsing code infers `Array[Byte]^{}`
  where the new enclosing scope requires `^{any}`; `Array` is invariant, so those are
  unrelated types rather than merely imprecise. Moving capture-sensitive code between
  enclosing objects can change inferred captures, and fixing it by annotation is a
  semantic change.
- **A sealed trait pins its subtypes to its file.** Nesting `SvgDef` drags the exported
  `LinearGradient` in with it. Check for `sealed` before proposing a move.
- **`YamlPath` and `TelPath` stay whole on semantic grounds** (R2, above), not merely
  because nesting them would shadow serpentine's `Path` — though it would, and both types
  are declared in terms of it. Where a semantic reason and a mechanical obstacle point the
  same way, record the semantic one: the mechanical obstacle might be removed later, and
  the name should not then drift.
- **Opaque types and aliases in package files** (`SvgId`, `TarRef`, `GitHash`) are not in
  their own donor files. An alias moves easily (`TarRef` → `Tar.Ref`); an opaque type whose
  companion carries its operations is better left alone.
- **Hoisting a file-mate is not free.** Splitting `HpackEntry` out of
  `HpackTable.scala` broke capture inference by itself, before any nesting:
  `HpackTable.static` infers `Array[HpackEntry]^{}` while the use site demands
  `Array[HpackEntry^'s1]^{any}`, and `Array` is invariant. Capture inference is sensitive
  to the *file boundary*, not only to the enclosing object, so verify a hoist on its own
  before treating it as preparation.
- **A second top-level type sharing a donor's file** travels with it (`TarDataOpenable` in
  `TarHandle.scala`, `ZipOpenable`/`ZipDataOpenable` in `ZipHandle.scala`). Hoist it to its
  own file first — which also fixes the pre-existing L2 violation — then nest.

Three further mechanical hazards, beyond the pilot's:

- The self-referential-base problem has **three** syntactic forms: `extends Error(`,
  `extends` and `Error(` on separate lines, and a bare `extends Error` with no arguments.
- A brace-enclosed import selector cannot hold a dotted path, so
  `import parasite.{async, AsyncError}` must split — and splitting it to
  `import parasite.Async.Error` then shadows `fulminate.Error` in files that extend it.
  Import the outer name instead.
- Two anonymous givens whose types differed only in the names being nested
  (`Tactic[AsyncError]`, `Tactic[ProtobufError]`) now synthesise the same name and collide;
  they need naming explicitly.

The reference rename must run over the **whole repository**, not the library being changed:
`Path.Error` alone is named by galilei, hellenism and imperial.

## Corrections from the second implementation pass (2026-08-11)

A further sixteen names nested (`Http2.Connection`, `Http2.Stream`, `Mcp.Server`,
`Mcp.Session`, the three `Zip.*`, and the nine-name COSE taxonomy). Four more rules, none
of which the first pass met:

- **A name used as data cannot be renamed textually.** xenophile resolves a source
  language's `Dialect` reflectively — `Class.forName(dialectType.typeSymbol.fullName)`,
  relying on `fullName` already being the JVM binary name, which holds only for a
  top-level object. Nesting `TypescriptDialect` makes the binary name
  `Typescript$Dialect$` while `fullName` still reads `xenophile.Typescript.Dialect`, so
  grammar loading fails at macro-expansion time. Nothing in `soundness.all` compiles the
  call sites; only the full test suite catches it. **Leave `TypescriptDialect`,
  `WebIdlDialect` and `WitDialect` alone**, and check for reflective lookup before nesting
  anything whose name might be resolved at runtime.
- **Check the outer object for a member of the target name.** `object Mcp` already had a
  nested `Error` — the JSON-RPC error payload — so `McpError` cannot become `Mcp.Error`
  and stays top-level.
- **A wildcard import over a namespace object is what makes nesting fragile.** A consumer
  writing `import Http2.*` alongside `import soundness.*` gets an ambiguity for any member
  sharing a name with the umbrella; nesting `Http2Stream` collided with zephyrine's
  `Stream` that way. Prefer fixing the import to abandoning the nesting — but for names
  generic enough to collide widely, weigh whether the nesting is worth it. There are 45
  such wildcards in the tree; the constant-table uses (`Vp8Tables`, `FlateTables`,
  `PeriodicTable`) are legitimate, the type-holding ones (`Mathml`, `Binary`, `Control`)
  are the fragile ones.
- **`caps.Pure` fixes the cross-file capture failure, but not the nesting one.**
  Declaring a genuinely pure type pure (`case class HpackEntry(…) extends caps.Pure`) lets
  it be split into its own file, which is otherwise blocked because the element's capture
  variable cannot be solved through a signature against an invariant `Array`. Nesting the
  same type still fails, so the constraint is not only the file boundary. Useful for L2
  compliance in its own right.

Two shapes of donor file also emerged. A file holding a **taxonomy** — COSE's nine
phantom markers — moves into the namespace whole, rather than being split N ways to
satisfy one-type-per-file first. And a **reference rename can match more than types**: an
enum case (`VarintError`, a constructor), an existing alias to the old name (`object Tel`
carried `type Error`), and package-qualified references to our own nested types
(`telekinesis.Http2.Frame`, which a guard against rewriting foreign types like
`jnh.HttpClient` will collapse if it only tests for a lowercase qualifier).

## Corrections from the third implementation pass (2026-08-11)

The remaining ~55 rows of the rename table were worked through, plus five of the six
split-first candidates: **63 names left the umbrella's exported surface**, and the table is
now exhausted. Six new rules, and one revert overturned.

- **A donor file holding a taxonomy nests only if its members are not separately exported.**
  The second pass's rule — a taxonomy moves into the namespace whole — needs that proviso.
  `TerminalEvent` is a `sealed trait` over `Interrupt`, `Keypress`, `CtrlChar` and
  `WindowsSignal`; `BlockCipherMode` and `BlockCipherPadding` are containers for `Cbc`,
  `Ecb`, `Ctr`, `Cfb`, `Ofb`, `Pkcs7`, `Iso10126` and `NoPadding`. Every one of those is an
  exported single-word name, so nesting the container silently renames all of them. That is
  a wider API decision than nesting a satellite, and all three stay.
- **A sealed trait pins its subtype's file too, not just its own.** The first pass recorded
  that nesting `SvgDef` drags its subtypes along. The converse also bites: `Declared` could
  not be *split out* of `xenophile.TypescriptDeclaration.scala`, because it is sealed and the
  declaration enum is its only subtype. Sealing constrains the move in both directions; the
  two move together.
- **Shadowing a supertype is fixable by qualifying it, not a reason to revert.**
  `ZipOpenable`/`ZipDataOpenable` were reverted in the first pass for shadowing the
  `Openable` they extend. `extends aperture.Openable` fixes it, exactly as a nested `Error`
  qualifies `fulminate.Error`. Both are now nested, as are stratiform's two openables.
  Check for the qualified form before recording a shadowing revert.
- **Check the outer object's *siblings* for the target name, not only the outer object.**
  Two collisions came from one level deeper than the second pass's rule reaches:
  `Wit.Function` is ambiguous with `Wit.Item.Function`, an enum case of a sibling member,
  inside that enum's own body; and `Tel.Flag` is ambiguous with `Tels.Flag` — a type in a
  *different* toplevel object — at every `import Tels.*` site within `object Tel`. The first
  was worth qualifying; the second was not, and `TelFlag` stays whole.
- **A donor's extension-method dependencies do not travel with its body.** Moving `LspProxy`
  into `object Lsp` broke `upstream.session`, an extension the donor file imported via
  `aperture.*` and the receiving file did not. Wildcard-import differences show up as
  missing *extension methods* long before they show up as missing types, so diff the two
  import lists rather than adding only what the type errors demand.
- **Never re-sort a receiving file's imports.** `obligatory.JsonRpc.scala` ends with
  `import httpBackends.javaNetHttp`, a choice import that must follow `telekinesis.*`;
  alphabetising the block moved it above and it stopped resolving. Insert new imports into
  the alphabetical run and leave the trailing choice imports alone. Relatedly, scope a
  donor's `scala.collection.immutable.{::, Nil}` imports to the member that needs them — at
  file level they re-point the cons pattern for every other member of the namespace.

Two further notes:

- **`TarHeader` failed a third time, and the message is sharper than "invariance".** The
  compiler reports that `^{}` is a *read-only* capture set and `^{any}` an *exclusive* one on
  a stateful `Array[Byte]`, so the one cannot subsume the other. That is a read-only/exclusive
  distinction, not merely `Array`'s invariance, which is probably why `caps.Pure` — which
  speaks to purity, not exclusivity — did not rescue `Hpack` either.
- **The wildcard-import fear is worth measuring.** The second pass predicted that nesting
  `LspProxy` would collide at the fourteen `import Lsp.*` sites, since `Proxy` is a known
  homonym. None of them is ambiguous. A wildcard over a namespace object is a hazard, not a
  blocker; test it rather than abandoning the nesting.


## Corrections from the fourth implementation pass (2026-08-12)

Eight names that the first three passes had recorded as blocked turned out not to be, and
the corrections are more interesting than the renames: in most cases the blocker was
something the codebase should not have been doing anyway.

- **A sealed trait's subtypes are a reason to move them *together*, not a reason to move
  none of them.** `SvgDef` was abandoned in the first pass because nesting it drags the
  exported `LinearGradient` in with it. That is exactly what should happen: both are now
  `Svg.Def` and `Svg.LinearGradient`. The rule this replaces — "check for `sealed` before
  proposing a move" — should read: check for `sealed`, then decide whether the whole
  cluster belongs in the namespace. Usually it does.
- **An opaque type belongs inside the object whose API it serves.** `SvgId` lived in
  `object internal` and was re-exported to package level; it is now `Svg.Id` with no
  re-export. Two consequences to expect: inside the defining object the type is
  transparent, so its companion is *not* in the implicit scope of a value the compiler
  sees as the underlying type — `id.text` must be written `Id.text(id)` there. Outside,
  nothing changes.
- **A nested companion is not a blocker; an unexported one would be.** The third pass ruled
  `MacAddressError` out because the `MacAddress` companion sits inside `internal.Opaques`.
  But that companion is exported to package level, so a type nested in it is exactly as
  reachable as one nested in a toplevel object. `MacAddress.Error` works.
- **Search the whole library, not the component, before declaring a type absent.**
  `ConnectionError` was recorded as un-nestable because coaxial "has no `Connection` type,
  only abstract `type Connection` members". It has one: a `case class Connection` in the
  **jvm** component, exported to the umbrella. Adding a namespace `object Connection` to
  core therefore put a second toplevel `Connection` in package `coaxial`, and ethereal's
  `Promise[Connection]` stopped resolving — "expected a type, but found a term". The error
  is in core and the type is in jvm, so this is an ordinary R6 component block, in the
  direction that cannot be fixed. `ConnectionError` stays whole.
- **An `@unexported` annotation is a nesting candidate flagging itself.** cataclysm's
  `Syntax` carried one, with a comment explaining that it clashes with stenography's
  `Syntax` in the umbrella. `Css.Syntax` removes the clash at its source, and the
  annotation with it. Any `@unexported` justified by a name clash is worth re-reading as a
  missing namespace.
- **A blocking wildcard import is usually the thing to fix.** `Tel.Flag` was blocked by
  `import Tels.*` inside `object Tel.Type`, making it ambiguous with `Tels.Flag`. Replacing
  the wildcard with 65 qualified `Tels.X` references removes the ambiguity and reads better
  besides: `Struct`, `Scalar`, `Field` and `Member` are generic enough that the reader needs
  telling which vocabulary they belong to. This generalises the second pass's note that a
  wildcard over a namespace object makes nesting fragile — prefer fixing the import.
- **A blocking member may simply be dead.** `object Mcp` already had a nested `Error`, so
  the second pass kept `McpError`. That payload was referenced nowhere and duplicated
  `JsonRpc.Failure` field for field — which the same file already calls. Check whether the
  occupant earns its name before working around it.

One caution learned the hard way: **do not sweep a homonym by bare name.** Renaming
cataclysm's `Syntax` with a repo-wide substitution rewrote stenography's and iridescence's
unrelated `Syntax` types. Scope the sweep to the owning library and let cross-library use
sites fail the compile instead.


## Corrections from the fifth implementation pass (2026-08-12)

A sweep through twenty-four prefix families named as obvious candidates. Thirty-one names
nested; most of the rest were blocked by the *outer* type living in another library, which
the family tables cannot show. Three new rules:

- **A prefix family is not a component.** `Css*`, `Io*`, `Dag*`, `Oci*`, `Unix*`, `Time*`,
  `Wasm*`, `Http*` and `Dts*` all look nestable in the tables and are not: `CssClass` is
  nomenclature's while `Css` is cataclysm's, `DagDiagram` is dendrology's while `Dag` is
  acyclicity's, aviation's `Unix` is the *epoch* and quantitative's `Time` is a *dimension*.
  Resolve the outer name to a declaration before treating a shared prefix as evidence.
- **Two `Facade`s in one file is worse than one `KotlinFacade`.** Nesting is worth doing
  when the compound name repeats its context; it stops being worth doing when the nested
  name collides with a *different* type the same file uses pervasively. `object Kotlin`
  names xenophile's `Facade` about ten times, so `Kotlin.Facade` was abandoned — unlike a
  shadowed supertype, there is no single base to qualify. `Workload.Grant`,
  `Workload.Openable` and `Teletype.Builder` all *were* worth it: each shadows exactly one
  base, qualified as `aperture.Grant`, `aperture.Openable` and `gossamer.Builder`.
- **The read-only/exclusive capture failure has a signature: `Array` indexing.**
  `MathmlReader` joined `TarHeader` and `HpackTable` in failing to nest, and all three do
  the same thing — index an `Array[T]^{}` inside a method moved into a capture-impure
  enclosing object, which then demands `^{any}`. Treat `Array` indexing in a donor as a
  warning sign, and check it before doing the rest of the work.

Two practical notes. **Take the receiving file's new imports from the donors, never from
guesswork** — inventing a plausible-looking set added `enigmatic.*` to a component that
does not depend on it and pulled in a `Numeric` ambiguity that had nothing to do with the
rename. And **a donor's file-mates must be checked every time**: `LiraDelta.scala` and
`LiraTree.scala` each held a second exported toplevel type (`Replacement`, `TreeEntry`)
that would otherwise have been renamed silently.


## Corrections from the sixth implementation pass (2026-08-12)

Thirty-three more names, from the families left after the fifth pass's sweep. Four rules,
three of them about *finding* the outer type rather than moving anything.

- **Search for `case object` too.** galilei's `Directory` was recorded as having no outer
  type. It is a `case object Directory extends WindowsEntry, UnixEntry` — the companion of
  `trait Directory` in the entry-kind taxonomy — which a search for `object Directory`
  silently misses. It took `Handle` and `Openable` without moving.
- **A companion nested inside `internal` is fine if it is exported.** Applied for the
  fourth time, and it is now the single most common false blocker: `Name`, `Port` and `Date`
  join `MacAddress`. All four were recorded as having no outer type, and all four have one
  in an `internal`/`Opaques` object that is re-exported to package level.
- **Check that a proposed namespace name is free — across every library.** `Chemical`,
  `Exec` and `Variant` were; `Daemon`, `Dom`, `Viewport` and `Textual` were not, each
  already an exported type in parasite, honeycomb, graffiti and gossamer. Creating a second
  would repeat the `Connection` failure. They are also homonyms rather than satellites —
  gossamer's `Textual` is the text typeclass — so nesting into the existing owner would be
  wrong even where the components allowed it.
- **The read-only/exclusive capture failure is not only `Array` indexing.** `TelReader`
  fails with "cannot call update method entryOrdinal since its capture set {} is read-only"
  — a `+=` on a mutable buffer, not an index. Three of the four instances index an `Array`;
  this one shows the underlying constraint is broader, so the warning sign is *any* mutation
  through a value whose capture set the enclosing object widens.

A third instance of "a name used as data", and the first one no compiler could catch. The
sweep rewrote `scalar("AtomClass", "atom-class")` and `Reference(t"AtomClass")` inside
reliquary's hand-encoded LIRA schemas — *string literals* naming a scalar in the
specification, not references to the Scala type. Everything compiled; one test out of 11,065
failed, comparing the hand-encoded schema against the `*.tel` resource that still said
`AtomClass`. Only `make attest` found it.

The check that generalises: after a sweep, look for literals whose entire content is a bare
dotted name (`"Play.Scene"`), since those are identifiers used as data rather than prose. Test
names and error messages mentioning the new name are fine; a literal that *is* the name is
not.

Two further sweep hazards fired, both caught by the compiler rather than by review. It
rewrote `jnf.WatchEvent`, which is `java.nio.file`'s — the second time a bare-name sweep has
reached outside the library that owns the name (the first was `Syntax`). And it produced
`import wisteria.{Discriminable, Variant.Error}`: a braced selector cannot hold a dotted
path, and a rename landing *inside* a selector list is how that happens.

A new namespace object also collides the same way a nested one does. `ExecEvent` became
`Exec.Event`, and octogenarian's `Git.Event` has a case named `Exec` — so `Exec.Event` inside
`object Git` resolved to the enum case, not the new namespace. Qualified as
`guillotine.Exec.Event`. Creating a namespace is not safer than nesting into an existing one:
both put a new name into scope wherever the family is used.


## Corrections from the seventh and eighth passes (2026-08-13 to 2026-08-17)

The `PlayError` sweep, and the families around it. Forty-eight names, and six rules — five of
them about not breaking something while moving something else.

- **Import the outer, never the member.** A rename to `Play.Error` breaks two import forms
  that look harmless: `import lib.PlayError` becomes `import lib.Play.Error`, which imports the
  *member* and leaves `Play` out of scope, so every `Play.Error` in the file fails to resolve;
  and `import lib.{PlayError, x}` becomes a dotted path inside a brace selector, which does not
  parse. Both bit. The convention is `import lib.Play` and write `Play.Error`.
- **A capitalised name cannot share its spelling with a lowercase top-level class or object
  in the same package.** On a case-insensitive filesystem — the default on macOS — `Rpc.class`
  and `rpc.class` are one file, and the second written destroys the first. It surfaces far
  away, as `Not found: type rpc` inside a macro in a different component, and survives
  incremental rebuilds until `mill clean`. `obligatory`'s `@rpc` annotation hit this; moving
  it into `object annotations` and exporting it frees the name. Nested lowercase objects are
  safe — hypotenuse's `arithmeticOptions.checkedDivision` does not block `Division`.
- **An inner wildcard import shadows the outer ones.** Gathering files into one object means
  merging their imports, and a nested object that re-imports only its *distinctive* imports
  loses the rest: `object Dialect` with `import proscenium.compat.*` inside it stopped seeing
  the `map` that `rudiments`/`gossamer` supply at file level. Give a nested object its source
  file's complete block, or hoist nothing.
- **A nested `Error` captures a bare `Error` in the same object.** `Httpd.Event
  .ConnectionFailed(error: Error)` named `fulminate.Error` through a wildcard import; adding
  a sibling `Httpd.Error` silently rebinds it, because a member of the enclosing object
  outranks a wildcard import. It failed to compile here, but in a position accepting any
  `Error` subtype it would not have.
- **A rename to `.Error` can collide two previously-distinct anonymous givens.** `given
  Tactic[StreamError]` and `given Tactic[Http2.Error]` in one scope synthesise different
  names; after the rename both are `given_Tactic_Error`. Three separate occurrences.
- **Check the donor file's *other* declarations before moving it.** A file named for one type
  may hold four. `profanity.TerminalInfo.scala` held `Interrupt`, `WindowsSignal` and
  `CtrlChar` as well, and moving the file wholesale deleted three exported types and silently
  rebound a union. The filename is not evidence of the file's contents.

**Homonyms are the recurring hazard, and the compiler catches most but not all.** Six in these
two passes: `ParseError` (zephyrine's and `Wit`'s, plus an MCP wire constant `val ParseError =
-32700`), `ServerError` (scintillate's and `Http.Status.Category`'s 5xx), `Frame` (perihelion's
WebSocket frame and ultimatum's layout frame), `EscapeError` (fulminate's and `Pty`'s),
`ConnectError` (telekinesis' and `Http2.ErrorCode`'s RFC 9113 `CONNECT_ERROR`), and
`HttpServer` (scintillate's and the JDK's `com.sun.net.httpserver.HttpServer`, which appears
inside the very file being renamed). Scope every sweep to the libraries that use *this* type,
and read the diff.

**Three findings that were not renames at all.** An error that looks misnamed may be one the
codebase already has: `BoundsError` duplicated `JsonBlueprint.Error.Reason.IntOutOfRange`,
same failure and same three fields, and was deleted. A supertype with one subtype and no
callers can go: turbulence's `trait Io` freed the name for galilei. And a type declared in one
library but raised only in another is misplaced, not misnamed — `BoundsError` was gossamer's,
`InstallError` is exoskeleton's but raised by ethereal too.

**Naming, where no single raiser owns an error.** Name the act (`Framing`, `Install`) or the
state (`Truncation`), not one of several raisers. `Streamable`, `Writable` and `Sink` all
report a cut stream, so none of them owns it.


## Execution shape

One library per commit (the pilot shape). Each commit: move the types, delete donor files,
fix the export file, sweep references (`grep -a`), clean-compile the library and its test
module. `make attest` gates each PR.

Eight passes have run this shape to completion; the rename table above is exhausted, and the
`PlayError` family with it — 48 of the 53 compound error names have gone. What is left is
recorded under "Next actions" in `api-reduction-candidates.md`. `TarHeader` is named there
for a reason worth repeating: `readUnchecked` clears the capture-checking blocker, but two of
its sites rely on the bounds check it removes, so that one waits on tightening the guards.

## Resolved since: `Tel.Provider` and `Json.Provider`

`TelBlueprint` and `JsonBlueprint` were recorded above as component-blocked; both now nest as
`Tel.Provider` and `Json.Provider`, by moving them into `core` (which gained the `polyvinyl.core`
edge) and dropping the `records` components. `JsonBlueprint.{Doc, Property, Error}` became
`Json.Provider.{Doc, Property, Error}`.

## Ninth-pass review (2026-11-12): what api-1 still lacks, and R8

A whole-surface review: 2008 names are exported into `package soundness`, 1510 of them
single-word. Since the inventory was last regenerated, 49 listed names had gone and **49 new
compound names had arrived** — `Acp*`, `Ssh*`, `*Supervisor`, `Font*`, `ChaCha20` — so new
code keeps adding to the surface this track is draining, and nothing stops it. The inventory is
regenerated, and the new names that keep their compounds are in its reviewed section.

Every owner below was resolved to a declaration in the *same build component* unless marked
*new*; † marks a proposal whose owner still needs confirming. Nothing here has been
compiled, and every row is subject to the hazards the earlier passes recorded.

### Umbrella clashes — two libraries export the same name — all resolved

No name is now exported into `package soundness` by more than one library.

| name | resolution |
|---|---|
| `Namespace` | embarcadero's is `Containerd.Namespace`, with the other 41 containerd records |
| `Commandline`, `Native` | octogenarian's git backends are no longer exported; they are chosen through `gitBackends` |
| `Pane` | ultimatum's is `Tui.Pane`. exoskeleton's keeps the name: it is the supertype `Tmux` implements, so `Tmux.Pane` would be backwards |
| `Arithmetic` | probably's `Baseline.Mode` cases are no longer re-exported |
| `Bytes` | rudiments' byte count is `Memory`; the unit `Bytes[n]`, with `Byte` and `Information`, moved into quantitative |
| `Layout` | ultimatum's new namespace is `Tui`, since `Layout` is punctuation's Markdown enum |
| `Tag`, `Renderable` | **blocked**: xylophone's stay `@unexported`, because `object Xml extends Tag.Container` and `Xml.Tag` would make `Xml` extend its own member |

### Compound names still nestable (api-1)

| library.component | renames |
|---|---|
| anthology.core | `CompileEvent→Compiler.Event`, `CompileProcess→Compiler.Process`, `CompileResult→Compiler.Result`, `CompileProgress→Compiler.Progress`; fold `CompileEvents` into `Compiler.Event`'s companion (`CompileFlag` is R6) — **done** |
| escritoire.core | `TableCell`, `TableRow`, `TableSection`, `TableStyle`, `TableRelabelling` → `Table.*` — **done** |
| ultimatum.core | `InlineAnchoring`, `InlineGrowth`, `InlineShrink` → `InlineRoot.*` — **done**, and then `InlineRoot` itself became `Tui.Block` (with `ScreenRoot` as `Tui.Screen`), so these are `Tui.Block.Anchoring` and so on |
| dendrology.tree | `TreeTile→TreeDiagram.Tile`, `TreeStyle→TreeDiagram.Style`, `TextualTreeStyle→TreeDiagram.TextualStyle` — **done** |
| dendrology.dag | `DagTile`, `DagStyle`, `TextualDagStyle` → `DagDiagram.*`; `LaneDagStyle`, `TextualLaneDagStyle` → `LaneDagDiagram.*` (not `Dag`, which is acyclicity's) — **done** |
| espionage.core | `AcpAgent→Acp.Agent`, `AcpClient→Acp.Client`; the split traits nest beneath them (`Acp.Agent.Lifecycle`, `Acp.Client.Fs`, …). The agent-location enum that held `Acp.Agent` becomes `Acp.Endpoint` (ruling 3) — **done** |
| degustation.tasty | `ClassSource→Universe.Source`, `ClassInfo→Universe.ClassInfo` — **done** |
| exoskeleton.args | merge `InvalidFlagError` and `MissingFlagError` into `Flag.Error` with a `Reason` — **done** |
| kaleidoscope.core | `RegexBackend→Regex.Backend` (sealed: its `regexBackends` givens move too, or it unseals) — **done** |
| parasite.core | `SharedProbate→Probate.Shared` (an alias; needs a companion) — **done** |
| aviation.core | `RecurrenceLiteral→Recurrence.Literal` (an alias) — **done** |
| cataclysm.core | `SyntaxMatcher→Css.Syntax.Matcher` — **done** |
| fulminate.core | `UncheckedError→Error.Unchecked` — **done** |

Still blocked: `TlsAcceptance`, `SecurePort` (platform-split `Tls`); `ArOpenable` (R6, as
`TarOpenable`); `PooledSupervisor` (dual platform definitions); `TableFixture` (`Table` is
escritoire's); `Ssh*` in octogenarian (four components, and `Ssh` is susurration's);
`DaemonEnvironment`, `Viewport*` and `Unix*` (outer names owned elsewhere).
`BlockCipherMode`/`BlockCipherPadding` nest, members and all: `Cbc`, `Ecb`, `Ctr`, `Cfb`,
`Ofb`, `Pkcs7`, `Iso10126` and `NoPadding` become `BlockCipher.Cbc` and so on (ruling 2). **Done.**

### R8 candidates — high confidence

| library | names → proposed home |
|---|---|
| ultimatum.core | `Frame`, `Pane`, `Panes`, `Placement`, `Sizing`, `Occupancy`, `Limits`, `Arrangement`, `Tick` → *new* `Tui.*` — **done**; `Rect` → `Tui.Region` (ruling 4: no abbreviation) |
| ultimatum.gauges | `Magnitude`, `Meter`, `Reading`, `Reckoning`, `Step`, `Standing`, `Transfer`, `Facet`, `Fraction`, `Gradient`, `Inlay` → *new* `Gauge.*`, or the existing `Gauging` — **done**: the gauges component was merged into core, and these, with `Bar`, `Dial`, `Spinner`, `Sparkline`, `Checklist`, `Stopwatch`, `Captioned`, `CaptionLayout`, `Tick` and `Countdown`, nest in `Tui` |
| anticipation | `Level→Log.Level`; the colour channels `Key`, `Cyan`, … → `Channel.*` † — **done**: `Log.Level`, and all nine channels in `Channel`, which is now exported (perihelion's own `Channel`, `Reader` and `Message` nested in `Websocket`) |
| honeycomb | `Kind`, `Capture` (attribute value sets) → the companion of their element or attribute † — **deferred**: `Kind` and `Capture` are two of a dozen attribute-value enums (`Autocomplete`, `Crossorigin`, `HDir`, `HttpEquiv`, `Method`, `Preload`, `Rel`, `Rev`, …), which should move as a family or not at all |
| archimedes | `Display`, `Cell` → `Mathml.*` — **done** |
| cataclysm | `Compound`, `Simple` → `Selector.*`; `Outcome→Css.Outcome` — **done**, with the rest of the selector grammar (`Combinator`, `Prefix`, `AttributeTest`, `AttributeMatcher`, `PseudoArgument`) |
| savagery | `Group`, `Figure` → `Svg.*`; `Up`, `Down`, `Left`, `Right` → `Delta.*` — **directions done**. `Figure` is **deferred**: it is a sealed taxonomy, so it would take `Rectangle`, `Ellipse`, `Group`, `Polyline`, `Outline` and `Lettering` with it |
| facsimile | `Destination→Pdf.Destination` — **done** |
| tasseomancy | `Lines`, `Category`, `Estimate` → `Chart.*` — `Category` and `Estimate` **done**; `Lines` **kept**, as one of the chart kinds beside `Bars`, `Pie` and `Scatter` |
| embarcadero | `Image`, `Index`, `History`, `Descriptor`, `Layer` → `Oci.*` — **done**, with `RootFs`, `ContainerConfig`, `WasmConfig` and `WasmComponent` |
| cacophony | `Configuration`, `Device`, `Feed` → `Audio.*` — **done**, with `Outlet`, `Feed`'s output counterpart |
| dissonance | `Chunk`, `Ins`, `Del`, `Edit`, `Change` → `Diff.*` — **done**, with `Par` and `Sub` (the abbreviations `Ins`, `Del`, `Par` and `Sub` remain) |
| polyvinyl | `Member`, `Multiplicity` → `Specification.*` — **done** |
| stratiform | `Mutation`, `Revision` → `Tel.*` — **done**, mixed into `Tel` from files of their own |
| gesticulate | `Part→Multipart.Part` — **done** |
| perihelion | `Masking→Websocket.Masking` — **done** |
| orthodoxy | `Scope→OAuth.Scope` — **done** |
| octogenarian.core | `Remote`, `Patch` → `Git.*` — **done**, with `FileDiff`, `Hunk` and `ChangeKind` |
| anthology.core | `Notice→Compiler.Notice`; `Triple`, `Universe`, `Tool` → `Toolchain.*` — **done** |
| praxinoscope | `Program`, `Ranges` → `Motif.*` — **done** |
| harlequin | `Token→SourceCode.Token`, `Role→Token.Role` — **done** |
| probably | `Value→Axis.Value` — **done** |
| synesthesia | `Agent`, `Human` → `Discourse.*` (susurration has its own `Agent`) — **done** |

### R8 candidates — wanting a ruling

telekinesis `Auth`, `Context`; coaxial `Trust→Tls.Trust` (platform-split `Tls`); contingency
`Pointer→Validation.Pointer`; probably `Report`, `Run`, `Trial`, `Verdict`; sedentary
`Profile`, `Stress` → `Bench.*`; aperture `Read`, `Write`, `Mode` → `Openable.*`; profanity
`Signal`, `Question`; zephyrine `Pace`, `Substrate`, `Regulation`; phoenicia `Face`,
`Medium`; serpentine `Case`; anamnesis `Entity`; exoskeleton `Login`, `Execution`; ethereal
`Resident`, `Terminus`, `Upgrade`, `Client`, `Egress` (they would need a new namespace, and
`Daemon` is parasite's).

### Rulings (2026-11-12)

1. **The gate is a flare rule**, not an `etc/` ratchet: a new exported compound name, or a name
   exported into the umbrella by two components, is reported where it is declared. Flare's
   predicates (`defines`, `matches`, …) cannot express either today — `defines` sees every
   member, not the umbrella surface — so this needs a flare feature built on what
   `flare.Umbrella` already computes, then a release and a pin.
2. **`BlockCipher.Cbc` and its siblings**: yes. The taxonomy nests with its members.
3. **`AcpAgent` becomes `Acp.Agent`.** The enum occupying the name is not an agent but where to
   find one — a subprocess or a pair of streams — so it becomes `Acp.Endpoint`. Everything ACP
   nests inside `Acp`. exegesis has the same shape (`Lsp.Server` is the location enum), and
   should follow when `LspLifecycle` and its siblings nest.
4. **`Tui`** is the namespace for ultimatum's layout vocabulary — and, in time, for most of
   ultimatum's entities. `Layout` was the first choice but is punctuation's Markdown block enum
   (`Markdown of Layout`), which keeps its name. `Rect` is renamed as it moves, since
   abbreviations are avoided: `Tui.Region`, a region of the screen in cells. facsimile's `Pdf.Rect` is not affected: it mirrors the PDF `/Rect` key.

### Reviewed and kept top-level

Either general vocabulary or a library's principal type: `Data`, `Builder`, `Cache`, `Color`,
`Duration`, `Date`, `Html`, `Json`, `Error`, `Exit`, `Loop`, `Grid`, `Column`, `Command`,
`Job`, `Credential`, `Catalog`, `Message`, `Session`, `Stream`, `Task`, `Promise`, `Path`,
`Root`, `Query`, `Record`, `Url`, `Origin`, `Range`, `Span`, `Theme`, `Point`, `Segment`,
`Series`, `Scale`, `Table`, `Sheet`, `Watch`, `Volume`, `Variant`.
