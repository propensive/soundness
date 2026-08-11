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
| bitumen.core | `TarError→Tar.Error`, `TarHeader→Tar.Header`, `TarCompression→Tar.Compression`, `TarBody→Tar.Body`, `TarFlag→Tar.Flag`, `TarHandle→Tar.Handle`, `TarRef→Tar.Ref` |
| cacophony.core | `AudioError→Audio.Error`, `FeedError→Feed.Error`, `OutletError→Outlet.Error` |
| caduceus.core | `CourierError→Courier.Error` |
| caesura.core | `DsvError→Dsv.Error`, `DsvFormat→Dsv.Format`, `DsvRedesignation→Dsv.Redesignation` |
| capricious.core | `RandomSize→Random.Size` |
| cataclysm.core | `CssError→Css.Error`, `CssErrors→Css.Errors`, `CssConvertible→Css.Convertible`, `SyntaxMatcher→Syntax.Matcher` (⚑ `Syntax` exists but is unexported; export it or skip) |
| coaxial.core | `ConnectionError→Connection.Error` (⚑ verify `Connection`), `DomainSocketEndpoint→DomainSocket.Endpoint` |
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
| imperial.core | `BaseLayout→Base.Layout` |
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
| savagery.core | `SvgError→Svg.Error`, `SvgId→Svg.Id`, `SvgDef→Svg.Def`, `SvgParser→Svg.Parser` |
| scintillate.server | `HttpServerEvent→HttpServer.Event` (new companion for `HttpServer`) |
| scintillate.servlet | `JavaServletFn→JavaServlet.Fn` |
| sedentary.core | `BenchError→Bench.Error` |
| serpentine.core | `PathError→Path.Error` |
| stratiform.core | `MutationError→Mutation.Error`, `TelError→Tel.Error`, `TelPath→Tel.Path`, `TelFlag→Tel.Flag`, `TelHandle→Tel.Handle` |
| stratiform.base256 | `Base256Error→Base256.Error` |
| stratiform.binary | `BintelError→Bintel.Error`, `VarintError→Varint.Error` |
| synesthesia.core | `McpClient→Mcp.Client`, `McpError→Mcp.Error`, `McpServer→Mcp.Server`, `McpSession→Mcp.Session`, `McpSpecification→Mcp.Specification` |
| tarantula.core | `WebDriverError→WebDriver.Error` |
| telekinesis.core | `HttpClient→Http.Client`, `HttpError→Http.Error`, `HttpEvent→Http.Event`, `HttpRedirection→Http.Redirection`, `HttpRequestError→Http.Request.Error`, `HttpResponseError→Http.Response.Error` |
| urticose.core | `HostnameError→Hostname.Error`, `EmailAddressError→EmailAddress.Error`, `MacAddressError→MacAddress.Error`, `NetworkInterfaceError→NetworkInterface.Error` |
| urticose.url | `UrlError→Url.Error`, `UrlFragment→Url.Fragment` |
| xenophile.typescript | `TypescriptDeclaration→Typescript.Declaration`, `TypescriptDialect→Typescript.Dialect`, `TypescriptError→Typescript.Error`, `TypescriptMember→Typescript.Member`, `TypescriptType→Typescript.Type`, `TypescriptParser→Typescript.Parser` |
| xenophile.webidl | `WebIdlDefinition→WebIdl.Definition`, `WebIdlDialect→WebIdl.Dialect`, `WebIdlParser→WebIdl.Parser` |
| xenophile.wit | `WitDeclaration→Wit.Declaration`, `WitDialect→Wit.Dialect`, `WitParser→Wit.Parser` |
| xylophone.core | `XmlError→Xml.Error`, `XPathError→XPath.Error` |
| yossarian.core | `PtyEscapeError→Pty.EscapeError`, `PtyState→Pty.State` |
| ypsiloid.core | `YamlError→Yaml.Error`, `YamlPrimitive→Yaml.Primitive`, `YamlPath→Yaml.Path`, `YamlPathError→Yaml.Path.Error` |
| zeppelin.core | `ZipError→Zip.Error`, `ZipEvent→Zip.Event`, `ZipHandle→Zip.Handle` |

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

## Execution shape (when implementation is approved)

One library per commit (the pilot shape), starting with the best-value targets:
octogenarian (12 renames, empty `object Git`, zero blockers), then synesthesia, exegesis,
enigmatic.cose, telekinesis.core, and outward. Each commit: move the types, delete donor
files, fix the export file, sweep references (`grep -a`), clean-compile the library and
its test module. `make attest` gates each PR.
