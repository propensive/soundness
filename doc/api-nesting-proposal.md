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

The split point is **semantic, not lexical**: `FooBar` becomes `Foo.Bar` only when `Foo`
is a meaningful concept on its own, and a compound that is itself an established concept
does not split at all — it becomes a namespace for its own satellites.

- **R1 SPLIT**: `Foo` names a concept, normally an existing type/object in the same
  component. `GitError → Git.Error`.
- **R2 KEEP + NAMESPACE**: the compound is an established concept independent of its
  prefix. `JsonPointer` (RFC 6901) does not become `Json.Pointer`; instead
  `JsonPointerError → JsonPointer.Error`. Likewise `MediaType`, `JsonSchema` and
  `XmlSchema` (external standards), `HttpServer`, `BlockCipher`, `CompileError`,
  `WebDriver`, `WorkingDirectory`, `DomainSocket`, `JsonRpc`, `OAuth`, `StackTrace`,
  `JsonBlueprint`, `SymmetricKey` (parallel with `PrivateKey`/`PublicKey`; the
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
- **R7 SINGLE-GIVEN BACKING TYPES**: a named type that exists only to be instantiated in
  one `given` should generally be **eliminated** — the given instantiates an anonymous
  class — rather than kept or nested. Where the name is genuinely needed (several
  instantiation sites, public API, or to avoid the anonymous-class-duplication warning at
  `inline given` sites — see coaxial's `@nowarn` precedent in `coaxial_wasi.scala`),
  nesting is acceptable when same-component. This absorbs July's C3/C3b lists.

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
| telekinesis.http2 (package cordillera) | `Http2Connection→Http2.Connection`, `Http2Error→Http2.Error`, `Http2Event→Http2.Event`, `Http2Stream→Http2.Stream`, `Http2ServerConnection→Http2.ServerConnection`, `HpackTable→Hpack.Table`, `HpackEntry→Hpack.Entry` |
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
| hieroglyph.core | `CharDecodeError→CharDecoder.Error`, `CharEncodeError→CharEncoder.Error` (nesting under the typeclass that raises them) |
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
| scintillate.server | `HttpServerEvent→HttpServer.Event` (new companion for `HttpServer`) |
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

## Kept whole after verification — no outer concept exists as a type

`TableCell`/`TableError`/`TableRow`/`TableSection`/`TableStyle`/`TableRelabelling`
(escritoire has `Tabular`/`Tabulation`, no `Table`); `DaemonEvent`/`DaemonLogEvent`/
`DaemonService` (no `Daemon` type; inventing `object Daemon` is possible but not proposed);
`ExecError`/`ExecEvent` (no `Exec`); `IpAddressError` (only `Ipv4`/`Ipv6`);
`PortError`/`PortType` (no `Port`); `RetryError` (no `Retry`); `IsinError` (no `Isin`);
`NameError`/`NameExtractor` (nomenclature's `Name` is an opaque type inside
`object internal` — same blocker as aviation's `Timestamp`, whose `TimestampError` also
stays); `AuthError`, `ConnectError`, `BindError`, `InstallError`, `SerializationError`,
`DecimalError`, `DivisionError`, `OverflowError`, `BoundsError`, `RangeError`,
`EscapeError`, `UncheckedError`, `UnsetError`, `DataError`, `RemoteError`,
`DegustationError`, `FontError`, `RruleError`, `RecurrenceError`, `OfflineError`,
`CertificateError`, `LinkError`/`LinkEvent`, `IoError`/`IoEvent`, `StreamError`,
`FrameError`, `RpcError` — no meaningful same-component outer type; all keep their
compound names. (Some may gain namespaces later if the outer concepts are ever reified.)

## Component-blocked (R6) — compound names stay

| name | member component | outer (component) |
|---|---|---|
| `JsonSchema` | jacinta.schema | `Json` (core) — also R2: external standard |
| `JsonBlueprint` | jacinta.records | `Json` (core) — nests its own satellites instead |
| `TarOpenable`, `TarBuilder` | bitumen.jvm | `Tar` (core) |
| `PdfFile` | facsimile.file | `Pdf` (core) — keeps its own `PdfFile.Origin` |
| `RasterOpenable`, `CanvasHandle` | hallucination.canvas | `Raster`/`Canvas` (core) |
| `PngBackend` etc. (×4) | png-jvm/png-native source dirs | `Png` (png) — dual platform definitions |
| `HmacCipher` | enigmatic.cose | `Hmac` (core) |
| `KeystoreError` | enigmatic core (shared) | `Keystore` (core-jvm sources only) |
| `MarkdownPalette`, `StackTracePalette` | ansi components | core types (also R5 palette family) |
| `ClasspathJvm` | hellenism.jvm | `Classpath` (core) |
| `BintelInlinable` | stratiform.binaryStaged | `Bintel` (binary) |
| `TelBlueprint` | stratiform.records | `Tel` (core) |
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

## Inline candidates (R7) — named types backing a single given

Verify the use-count at execution; where a type is instantiated exactly once, in one
given, prefer eliminating the name (anonymous class in the given). Candidates, from the
July C3b list plus the carriers the modularity work created:

- The per-format `DecodableDerivation`/`EncodableDerivation` objects (austronesian,
  breviloquence, caesura, jacinta, legerdemain, locomotion, stratiform, xylophone,
  ypsiloid) and wisteria's `AddableDerivation`/`DivisibleDerivation`/
  `MultiplicableDerivation`/`SubtractableDerivation`, stratiform's `TelsDerivation` —
  generically summoned, need not be named.
- Carrier classes returned by single givens: `TarOpenable`, `TarBuilder.TarCreatable`,
  `ImageOpenable`, `RasterOpenable`, `TelOpenable`, `TelViewOpenable`, `ZipOpenable`,
  `ZipDataOpenable`, `TarDataOpenable`, `WatchOpenable`, `WatchAllOpenable`,
  `DirectoryOpenable`, `FileOpenable`, `ImageDataOpenable`, `WorkloadOpenable` — each
  either inlines into its given or, where the name is load-bearing (e.g. it carries
  members or appears in public signatures), nests if same-component.
- The `*Sessional` instances (`GrpcSessional`, `LspSessional`, `WsSessional`,
  `ScalacSessional`, `NetworkDeviceSessional`) — same treatment.
- The `*Tactic` strategy backings (contingency's `AttemptTactic`, `AmalgamateTactic`,
  `EitherTactic`, `HaltTactic`, `OptionalTactic`, `ThrowTactic`, `TrackTactic`;
  parasite's `AsyncTactic`) — these back the `strategies` choice givens; inline where
  single-use, else keep (they are user-visible in error messages, which may argue for
  keeping names).
- Caveat from the C3 triage (still valid): a `private` modifier can leak into the
  inferred type of a public given and break downstream derivation — prefer anonymity or
  `@unexported`-style export removal over `private[lib]`.

## Mechanics

- **Consequent rules**: L2 (one toplevel type per file, `<module>.<Type>.scala` — delete
  the donor file when its type nests); L3 (companion `object` precedes its
  `class`/`trait`/`enum`, including nested pairs); F1 licence header. Enforced by the
  published `dev.propensive:consequent` plugin.
- **Error-nesting gotcha** (from the pilot): a nested `case class Error` makes the bare
  `Error` in its own `extends` clause self-referential — qualify as
  `extends fulminate.Error(...)`. Move the donor file's imports with the body.
- **New companions needed** (L3-ordered before their types): `Cipher`, `BlockCipher`,
  `Compiler`, `HttpServer`.
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
(`cordillera.Http2.Frame`, which a guard against rewriting foreign types like
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
  `import httpBackends.virtualMachine`, a choice import that must follow `telekinesis.*`;
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


## Execution shape

One library per commit (the pilot shape). Each commit: move the types, delete donor files,
fix the export file, sweep references (`grep -a`), clean-compile the library and its test
module. `make attest` gates each PR.

Three passes have run this shape to completion; the rename table above is exhausted. What
is left is recorded under "Next actions" in `api-reduction-candidates.md`: the three
taxonomy containers, `TarHeader`, and the two independent pieces of work (`caps.Pure` for
L2, and the namespace-wildcard survey).
