# Public API surface — multi-word names remaining

Regenerated from the `soundness_*` re-export files on 2026-08-11, after the nesting pass
(97 renames across 34 libraries). This lists what is **still** exported into `package
soundness` under a multi-word name; it replaces the earlier candidate inventory, whose C1,
C3 and C3b sections are superseded by `api-nesting-proposal.md` and whose C2 and C4
sections are folded in below.

- multi-word type-level names exported: **698**, across 162 components (was 823)
- of those, 466 sit in a prefix family of two or more; 232 are singletons
- names inside `package <name>:` blocks (choice packages such as `alphabets`,
  `manifestAttributes`, `constants`) are excluded: they are already namespaced

Most of what remains is **deliberate**, and falls into the categories the proposal records.
Before treating any row below as a candidate, check it against those rules — in particular
R2 (an established concept keeps its compound name: `JsonPointer`, `YamlPath`, `MediaType`,
`SymmetricKey`), R5 (excluded buckets) and R6 (component-blocked).

## What is deliberately excluded, and why

| bucket | examples | reason |
|---|---|---|
| stdlib mirrors (proscenium) | `ClassTag`, `TreeMap`, `ArrowAssoc`, `*HasAsScala` | compatibility surface; the names are the stdlib's |
| protocol mirrors (embarcadero.containerd) | `CreateContainerRequest`, `ListImagesResponse`, … | mirror the containerd gRPC schema |
| calendar vocabulary (aviation) | `CopticCalendar`, `HebrewMonth`, `LeapMode` | established domain terms |
| foreign interop (diuretic) | `JavaIoFile`, `JavaNioPath`, `JavaUtilDate` | deliberately name the foreign type |
| platform interfaces | `Wasi*Api` | one per library, loaded as a unit |
| render palettes | `MarkdownPalette`, `StackTracePalette`, `TestPalette`, … | cross-component by design: subject in core, palette in the ansi/render component |
| specification concepts (R2) | `JsonPointer`, `YamlPath`, `TelPath`, `JsonSchema`, `XmlSchema`, `MediaType`, `SymmetricKey`, `HttpServer`, `BlockCipher`, `CompileError` | the compound names a thing with its own specification |
| reflectively-loaded (new) | `TypescriptDialect`, `WebIdlDialect`, `WitDialect` | xenophile loads these by fully-qualified name; a name used as data cannot be renamed |
| component-blocked (R6) | `TarOpenable`, `PdfFile`, `ImageRecord`, `HmacCipher`, `ClasspathJvm`, … | outer companion lives in another component |

## Blocked by shape, not by policy

These would nest, but their files resist it; each needs preparatory work first, and the
proposal's corrections section explains the shapes:

- **file-mates**: `HpackTable`/`HpackEntry`, `Http2Connection`/`Http2Stream`,
  `CoseStructure` (shares a file with eight types), `WitDeclaration`, `WebIdlDefinition`,
  `TelHandle`/`TelOpenable`, `ZipOpenable`/`ZipDataOpenable`, `McpServer`/`McpSession`,
  `LspProxy` — hoist the siblings into their own files first.
- **capture-sensitive**: `TarHeader` (infers `Array[Byte]^{}` where the new scope needs
  `^{any}`, and `Array` is invariant).
- **sealed**: `SvgDef` (subtypes pinned to its file).
- **name already taken**: `McpError` (`object Mcp` has its own nested `Error`).
- **supertype, not satellite**: `BaseLayout` (`object Base extends BaseLayout`).
- **opaque/alias in a package file**: `SvgId`, `GitHash`-style declarations.

## Remaining names by prefix family

| prefix | libraries | n | names |
|---|---|---|---|
| `Apk*` | anthology | 3 | `ApkConfiguration`, `ApkManifest`, `ApkSigner` |
| `Arc*` | geodesy | 2 | `ArcMinute`, `ArcSecond` |
| `Atom*` | reliquary | 2 | `AtomClass`, `AtomReference` |
| `Attribute*` | cataclysm | 2 | `AttributeMatcher`, `AttributeTest` |
| `Block*` | enigmatic, galilei | 4 | `BlockCipher`, `BlockCipherMode`, `BlockCipherPadding`, `BlockDevice` |
| `Box*` | escritoire | 2 | `BoxDrawing`, `BoxLine` |
| `Cbor*` | breviloquence | 2 | `CborError`, `CborReader` |
| `Char*` | escapade, galilei, hieroglyph | 6 | `CharDecodeError`, `CharDecoder`, `CharDevice`, `CharEncodeError`, `CharEncoder`, `CharSpan` |
| `Chemical*` | charisma | 3 | `ChemicalElement`, `ChemicalEquation`, `ChemicalFormula` |
| `Class*` | honeycomb, mandible, proscenium | 3 | `ClassList`, `ClassSurface`, `ClassTag` |
| `Classfile*` | mandible | 2 | `ClassfileAtomizer`, `ClassfileDiscipline` |
| `Compile*` | anthology, larceny | 7 | `CompileError`, `CompileEvent`, `CompileEvents`, `CompileFlag`, `CompileProcess`, `CompileProgress`, `CompileResult` |
| `Content*` | embarcadero, obligatory | 2 | `ContentDescriptor`, `ContentLength` |
| `Coptic*` | aviation | 2 | `CopticCalendar`, `CopticMonth` |
| `Cose*` | enigmatic | 5 | `CoseContext`, `CoseMaced`, `CoseSigned`, `CoseStructure`, `CoseTag` |
| `Create*` | embarcadero, galilei | 8 | `CreateContainerRequest`, `CreateContainerResponse`, `CreateFlag`, `CreateNamespaceRequest`, `CreateNamespaceResponse`, `CreateNonexistentParents`, `CreateTaskRequest`, `CreateTaskResponse` |
| `Css*` | nomenclature | 2 | `CssClass`, `CssIdentifier` |
| `Daemon*` | ethereal | 3 | `DaemonEvent`, `DaemonLogEvent`, `DaemonService` |
| `Dag*` | dendrology | 3 | `DagDiagram`, `DagStyle`, `DagTile` |
| `Date*` | aviation | 2 | `DateNumerics`, `DateSeparation` |
| `Decimal*` | hypotenuse, rudiments | 2 | `DecimalConverter`, `DecimalError` |
| `Delete*` | embarcadero, galilei | 6 | `DeleteContainerRequest`, `DeleteImageRequest`, `DeleteNamespaceRequest`, `DeleteRecursively`, `DeleteTaskRequest`, `DeleteTaskResponse` |
| `Deps*` | burdock | 2 | `DepsDev`, `DepsEvent` |
| `Directory*` | galilei | 2 | `DirectoryHandle`, `DirectoryOpenable` |
| `Division*` | hypotenuse | 2 | `DivisionByZero`, `DivisionError` |
| `Dom*` | nomenclature | 2 | `DomId`, `DomIdentifier` |
| `Domain*` | coaxial | 2 | `DomainSocket`, `DomainSocketEndpoint` |
| `Dts*` | xenophile | 2 | `DtsAtomizer`, `DtsDiscipline` |
| `Dynamic*` | breviloquence, caesura, jacinta, stratiform, xylophone, ypsiloid | 6 | `DynamicCborEnabler`, `DynamicDsvEnabler`, `DynamicJsonEnabler`, `DynamicTelEnabler`, `DynamicXmlEnabler`, `DynamicYamlEnabler` |
| `East*` | geodesy | 2 | `EastNortheast`, `EastSoutheast` |
| `Email*` | urticose | 2 | `EmailAddress`, `EmailAddressError` |
| `Ethiopian*` | aviation | 2 | `EthiopianCalendar`, `EthiopianMonth` |
| `Exec*` | guillotine | 2 | `ExecError`, `ExecEvent` |
| `File*` | galilei, octogenarian | 2 | `FileDiff`, `FileOpenable` |
| `Filesystem*` | galilei | 2 | `FilesystemAttribute`, `FilesystemBackend` |
| `Foreign*` | xenophile | 2 | `ForeignBuffer`, `ForeignLibrary` |
| `Frame*` | obligatory, telekinesis | 2 | `FrameError`, `FrameReader` |
| `French*` | aviation | 2 | `FrenchRepublicanCalendar`, `FrenchRepublicanMonth` |
| `Get*` | embarcadero | 6 | `GetContainerRequest`, `GetContainerResponse`, `GetImageRequest`, `GetImageResponse`, `GetTaskRequest`, `GetTaskResponse` |
| `Grpc*` | obligatory | 4 | `GrpcChannel`, `GrpcError`, `GrpcFraming`, `GrpcSessional` |
| `Hebrew*` | aviation | 2 | `HebrewCalendar`, `HebrewMonth` |
| `Host*` | mandible | 3 | `HostArchive`, `HostContracts`, `HostRelease` |
| `Hpack*` | telekinesis | 2 | `HpackEntry`, `HpackTable` |
| `Http*` | anticipation, honeycomb, scintillate, telekinesis, urticose | 10 | `HttpConnection`, `HttpEquiv`, `HttpRequestError`, `HttpRequests`, `HttpResponseError`, `HttpServer`, `HttpServerEvent`, `HttpSession`, `HttpStreams`, `HttpUrl` |
| `Http2*` | telekinesis | 3 | `Http2Connection`, `Http2ServerConnection`, `Http2Stream` |
| `Image*` | embarcadero | 5 | `ImageConfig`, `ImageDataOpenable`, `ImageHandle`, `ImageOpenable`, `ImageRecord` |
| `Indian*` | aviation | 2 | `IndianCalendar`, `IndianMonth` |
| `Inline*` | profanity, ultimatum | 5 | `InlineAnchoring`, `InlineBoard`, `InlineGrowth`, `InlineRoot`, `InlineShrink` |
| `Io*` | galilei | 2 | `IoError`, `IoEvent` |
| `Islamic*` | aviation | 2 | `IslamicCalendar`, `IslamicMonth` |
| `Java*` | anthology, diuretic, enigmatic, gastronomy, scintillate | 12 | `JavaIoFile`, `JavaLongDuration`, `JavaLongInstant`, `JavaNetUrl`, `JavaNioPath`, `JavaServlet`, `JavaServletFn`, `JavaStdlibCrypto`, `JavaStdlibHashing`, `JavaTimeInstant`, `JavaUtilDate`, `JavaVersion` |
| `Json*` | jacinta, obligatory | 11 | `JsonBlueprint`, `JsonBlueprintDoc`, `JsonBlueprintError`, `JsonError`, `JsonPointer`, `JsonPointerError`, `JsonPrimitive`, `JsonReader`, `JsonRpc`, `JsonRpcError`, `JsonSchema` |
| `Kotlin*` | xenophile | 6 | `KotlinDialect`, `KotlinFacade`, `KotlinInvoke`, `KotlinMetadataAtomizer`, `KotlinMetadataDiscipline`, `KotlinRuntime` |
| `Lane*` | dendrology | 2 | `LaneDagDiagram`, `LaneDagStyle` |
| `Larceny*` | larceny | 2 | `LarcenyPlugin`, `LarcenyTransformer` |
| `Leap*` | aviation | 2 | `LeapMode`, `LeapSeconds` |
| `Line*` | escritoire, profanity, turbulence | 3 | `LineCharset`, `LineEditor`, `LineSeparation` |
| `Linear*` | denominative, savagery | 3 | `LinearAccessComplexity`, `LinearGradient`, `LinearSizeComplexity` |
| `Link*` | anthology | 2 | `LinkError`, `LinkEvent` |
| `Lira*` | anthology, reliquary | 12 | `LiraAdvisory`, `LiraAssembler`, `LiraBundle`, `LiraDelta`, `LiraError`, `LiraHash`, `LiraManifest`, `LiraPayload`, `LiraRealm`, `LiraSchemas`, `LiraTree`, `LiraValidators` |
| `List*` | embarcadero, proscenium | 9 | `ListContainersRequest`, `ListContainersResponse`, `ListHasAsScala`, `ListImagesRequest`, `ListImagesResponse`, `ListNamespacesRequest`, `ListNamespacesResponse`, `ListTasksRequest`, `ListTasksResponse` |
| `Local*` | hellenism, urticose | 2 | `LocalClasspath`, `LocalPart` |
| `Log*` | anticipation, eucalyptus | 2 | `LogPalette`, `LogSink` |
| `Lsp*` | exegesis | 2 | `LspProxy`, `LspSessional` |
| `Mac*` | galilei, urticose | 3 | `MacAddress`, `MacAddressError`, `MacOs` |
| `Manifest*` | reliquary, revolution | 3 | `ManifestAttribute`, `ManifestEntry`, `ManifestSigning` |
| `Map*` | proscenium | 2 | `MapHasAsJava`, `MapHasAsScala` |
| `Mathml*` | archimedes | 3 | `MathmlError`, `MathmlParser`, `MathmlReader` |
| `Mcp*` | synesthesia | 3 | `McpError`, `McpServer`, `McpSession` |
| `Metric*` | quantitative | 2 | `MetricPrefix`, `MetricUnit` |
| `Must*` | nomenclature | 9 | `MustContain`, `MustEnd`, `MustMatch`, `MustNotContain`, `MustNotEnd`, `MustNotEqual`, `MustNotMatch`, `MustNotStart`, `MustStart` |
| `Name*` | nomenclature | 2 | `NameError`, `NameExtractor` |
| `Native*` | surveillance, xenophile | 2 | `NativeInvoke`, `NativeWatcher` |
| `Nautical*` | quantitative | 2 | `NauticalMile`, `NauticalMiles` |
| `Network*` | sedentary, urticose | 4 | `NetworkDevice`, `NetworkDeviceSessional`, `NetworkInterface`, `NetworkInterfaceError` |
| `No*` | enigmatic, quantitative, scintillate | 3 | `NoCache`, `NoPadding`, `NoPrefix` |
| `North*` | geodesy | 2 | `NorthNortheast`, `NorthNorthwest` |
| `Number*` | distillate, jacinta | 2 | `NumberError`, `NumberMode` |
| `Oci*` | anthology, embarcadero | 3 | `OciConfiguration`, `OciError`, `OciImage` |
| `Open*` | apoplexy, galilei | 2 | `OpenApi`, `OpenFlag` |
| `Pdf*` | facsimile | 7 | `PdfError`, `PdfFile`, `PdfFont`, `PdfInfo`, `PdfMatrix`, `PdfOperator`, `PdfRect` |
| `Pem*` | enigmatic | 2 | `PemError`, `PemLabel` |
| `Persian*` | aviation | 2 | `PersianCalendar`, `PersianMonth` |
| `Port*` | urticose | 2 | `PortError`, `PortType` |
| `Process*` | embarcadero, guillotine | 3 | `ProcessInput`, `ProcessRef`, `ProcessStatus` |
| `Product*` | wisteria | 3 | `ProductDerivable`, `ProductDerivation`, `ProductReflection` |
| `Pseudo*` | cataclysm, telekinesis | 2 | `PseudoArgument`, `PseudoHeaders` |
| `Recurrence*` | aviation | 3 | `RecurrenceError`, `RecurrenceLiteral`, `RecurrenceSet` |
| `Scala*` | degustation, harlequin | 3 | `ScalaAtom`, `ScalaReference`, `ScalaSyntaxPalette` |
| `Scalac*` | anthology | 2 | `ScalacSession`, `ScalacSessional` |
| `Signature*` | enigmatic | 2 | `SignatureAlgorithm`, `SignatureDigest` |
| `Socket*` | coaxial, scintillate | 5 | `SocketBackend`, `SocketEvent`, `SocketOption`, `SocketServer`, `SocketService` |
| `South*` | geodesy | 2 | `SouthSoutheast`, `SouthSouthwest` |
| `Sse*` | obligatory | 2 | `SseError`, `SseSource` |
| `Stack*` | digression, hyperbole | 3 | `StackResolver`, `StackTrace`, `StackTracePalette` |
| `Start*` | embarcadero | 2 | `StartRequest`, `StartResponse` |
| `Stream*` | turbulence | 2 | `StreamError`, `StreamOutputStream` |
| `Sum*` | wisteria | 2 | `SumDerivation`, `SumReflection` |
| `Table*` | escritoire, phoenicia | 7 | `TableCell`, `TableError`, `TableRelabelling`, `TableRow`, `TableSection`, `TableStyle`, `TableTag` |
| `Tar*` | bitumen | 4 | `TarBuilder`, `TarDataOpenable`, `TarHeader`, `TarOpenable` |
| `Tasty*` | hyperbole | 5 | `TastyDefinition`, `TastyFile`, `TastyPalette`, `TastySymbol`, `TastyTree` |
| `Tel*` | stratiform | 7 | `TelBlueprint`, `TelFlag`, `TelHandle`, `TelOpenable`, `TelPath`, `TelReader`, `TelViewOpenable` |
| `Teletype*` | escapade, punctuation | 2 | `TeletypeBuilder`, `TeletypeFormattable` |
| `Terminal*` | escapade, ethereal, profanity | 7 | `TerminalBoard`, `TerminalError`, `TerminalEscapes`, `TerminalEvent`, `TerminalFeature`, `TerminalInfo`, `TerminalMode` |
| `Text*` | escapade, escritoire, facsimile, fulminate, gossamer, hieroglyph, honeycomb | 7 | `TextAlignment`, `TextBuilder`, `TextEscapes`, `TextNode`, `TextRun`, `TextSanitizer`, `TextStyle` |
| `Textual*` | dendrology | 3 | `TextualDagStyle`, `TextualLaneDagStyle`, `TextualTreeStyle` |
| `Time*` | abacist, aviation | 8 | `TimeError`, `TimeEvent`, `TimeFormat`, `TimeMinutes`, `TimeNumerics`, `TimeSeconds`, `TimeSeparation`, `TimeSpecificity` |
| `Track*` | contingency | 2 | `TrackFoci`, `TrackTactic` |
| `Tree*` | dendrology, proscenium, reliquary | 7 | `TreeDiagram`, `TreeEntry`, `TreeMap`, `TreePath`, `TreeSet`, `TreeStyle`, `TreeTile` |
| `Type*` | bitumen, typonym | 5 | `TypeElement`, `TypeFlag`, `TypeList`, `TypeMap`, `TypeSet` |
| `Typescript*` | xenophile | 6 | `TypescriptDeclaration`, `TypescriptDialect`, `TypescriptError`, `TypescriptMember`, `TypescriptParser`, `TypescriptType` |
| `Udp*` | coaxial, urticose | 2 | `UdpPort`, `UdpResponse` |
| `Unix*` | bitumen, galilei, profanity | 5 | `UnixEntry`, `UnixGroup`, `UnixMode`, `UnixSignal`, `UnixUser` |
| `Url*` | urticose | 3 | `UrlError`, `UrlFragment`, `UrlPalette` |
| `Variant*` | wisteria | 2 | `VariantError`, `VariantIndex` |
| `Viewport*` | cataclysm | 4 | `ViewportHeights`, `ViewportMaxes`, `ViewportMins`, `ViewportWidths` |
| `Wait*` | embarcadero | 2 | `WaitRequest`, `WaitResponse` |
| `Wasi*` | ambience, anthology, aviation, capricious, coaxial, galilei, telekinesis, turbulence | 9 | `WasiCliApi`, `WasiClockApi`, `WasiEnvironmentApi`, `WasiFilesystemApi`, `WasiHttpApi`, `WasiRandom`, `WasiRandomApi`, `WasiSocketsApi`, `WasiToolchain` |
| `Wasm*` | anthology, embarcadero, xenophile | 4 | `WasmComponent`, `WasmConfig`, `WasmInvoke`, `WasmObject` |
| `Watch*` | surveillance | 5 | `WatchAllOpenable`, `WatchError`, `WatchEvent`, `WatchHandle`, `WatchOpenable` |
| `Web*` | iridescence, tarantula, xenophile | 8 | `WebColors`, `WebDriver`, `WebDriverError`, `WebIdl`, `WebIdlAtomizer`, `WebIdlDialect`, `WebIdlDiscipline`, `WebSocket` |
| `Websocket*` | perihelion | 2 | `WebsocketError`, `WebsocketEvent` |
| `West*` | geodesy | 2 | `WestNorthwest`, `WestSouthwest` |
| `Windows*` | galilei, profanity | 2 | `WindowsEntry`, `WindowsSignal` |
| `Wit*` | anthology, xenophile | 8 | `WitAtomizer`, `WitCase`, `WitDialect`, `WitDiscipline`, `WitError`, `WitHandle`, `WitVariant`, `WitWorld` |
| `Working*` | ambience, aviation | 2 | `WorkingDays`, `WorkingDirectory` |
| `Workload*` | embarcadero | 3 | `WorkloadGrant`, `WorkloadHandle`, `WorkloadOpenable` |
| `Ws*` | perihelion | 3 | `WsConnection`, `WsSessional`, `WsUrl` |
| `Xml*` | xylophone | 3 | `XmlError`, `XmlReader`, `XmlSchema` |
| `Zip*` | zeppelin | 6 | `ZipBuilder`, `ZipDataOpenable`, `ZipError`, `ZipEvent`, `ZipHandle`, `ZipOpenable` |

### Singletons (232)

`AdaptiveSupervisor`, `AddOp`, `AlexandrianCalendar`, `AmalgamateTactic`, `AmountOfSubstance`, `AnyMessage`, `ArrowAssoc`, `AsciiBuilder`, `AsyncTactic`, `AtomsBlob`, `AttemptTactic`, `AuthError`, `BaseLayout`, `BenchmarkDevice`, `BeneficencePlugin`, `BindError`, `BlobStream`, `BloomFilter`, `BorderStyle`, `BoundsError`, `BytecodePalette`, `CanonicalCbor`, `CanvasHandle`, `CapabilityDiscipline`, `CardinalWind`, `CarriageReturn`, `CaseSensitivity`, `CellRef`, `CertificateError`, `ChangeKind`, `ChannelLayout`, `CheckOverflow`, `CipherSession`, `ClasspathIndex`, `CliEvent`, `CollectionConverters`, `ColorDepth`, `CommonFormattable`, `CompilerError`, `ConnectError`, `ConnectionError`, `ContainerConfig`, `CopyAttributes`, `CrLf`, `CtSym`, `CtrlChar`, `DataError`, `DecodableManifest`, `DegustationError`, `DereferenceSymlinks`, `DisciplineError`, `DismissError`, `DivOp`, `DnsLabel`, `DockerEvent`, `DummyImplicit`, `EcosystemProfile`, `EditorField`, `EitherTactic`, `EncodableManifest`, `EntryPoint`, `EnumerationHasAsScala`, `ErgoError`, `EscapeError`, `EucalyptusGcp`, `ExpectationError`, `FastForward`, `FieldIndex`, `FlowExtent`, `FluidOunce`, `FoldableRectoPanel`, `FontError`, `GapPolicy`, `GarbageCollection`, `GenericHtmlAttribute`, `GithubActions`, `GivensPhase`, `GraphemeBreak`, `HalfWind`, `HaltTactic`, `HmacCipher`, `HostnameError`, `Html4Transitional`, `InitializationVector`, `InstallError`, `IntercardinalWind`, `InterfaceAddress`, `IpAddressError`, `Ipv4Subnet`, `Ipv6Subnet`, `IsinError`, `IteratorHasAsScala`, `JarBuilder`, `JavacOption`, `JsInvoke`, `JsigDiscipline`, `JuxtapositionPalette`, `JvmProfile`, `KeyStore`, `KeystoreError`, `KillRequest`, `LanguageFeature`, `LayeredDagDiagram`, `LazyEnvironment`, `LengthPrefix`, `LocalhostDevice`, `LongNameFormat`, `LruCache`, `MarkdownPalette`, `MathML`, `MediaType`, `MenuField`, `MlDsa`, `MonotonicClock`, `MoveAtomically`, `MulOp`, `NirPlugin`, `NonFatal`, `NotFound`, `NoteRef`, `NumericRange`, `OfflineError`, `OffsetCalendar`, `OnlineClasspath`, `OpaqueDiscipline`, `OpensslCrypto`, `OperationSize`, `OptionalTactic`, `OrdinalCalendar`, `OtfTag`, `OverflowError`, `OverwritePreexisting`, `PanamaInvoke`, `ParseError`, `PartiallyOrdered`, `PcmFlag`, `PeriodicTable`, `PhysicalState`, `PidError`, `PixelOpaque`, `PlaceholderKind`, `PlatformSupervisor`, `PojoError`, `PolarGaussian`, `PollingWatcher`, `PositionTracking`, `PosixCommands`, `PrivateKey`, `ProcessingPermit`, `ProgrammingLanguage`, `ProgressBar`, `PropertyDef`, `PublicKey`, `RadioGroup`, `RamFlag`, `RangeError`, `RasterOpenable`, `RectoPanel`, `ReferenceError`, `ReflogEntry`, `RemoteError`, `RequestServable`, `ResetMode`, `RetryError`, `Rgb12Opaque`, `Rgb32Opaque`, `RomanCalendar`, `RootFs`, `RpcError`, `RruleError`, `SchemaSignature`, `ScreenRoot`, `SecureEndpoint`, `SelectMenu`, `SelectorList`, `SemanticMessage`, `SemverError`, `SeqHasAsJava`, `SerializationError`, `ServerError`, `ShaderPlugin`, `SiderealDays`, `SignalResponse`, `SimpleTExtractor`, `SolarDay`, `SoundnessHashing`, `SourceCode`, `SparseSegment`, `SshUrl`, `StandardMetadata`, `StaticAnnotation`, `SubOp`, `SvgDef`, `SymmetricKey`, `SyntaxMatcher`, `TcpPort`, `TemperatureScale`, `TemporaryDirectory`, `TestPalette`, `ThemeColor`, `ThrowTactic`, `TimestampError`, `TlsAcceptance`, `ToolchainError`, `TopMenu`, `TransferEncoding`, `TraversalOrder`, `TrieMap`, `TripleDes`, `TtfTag`, `UnboundedSizeComplexity`, `UncheckedError`, `UniformDistribution`, `UnitsNames`, `UnsetError`, `UnusedFeature`, `UsedSets`, `UsesBlob`, `ValueToken`, `VersionResponse`, `VersoPanel`, `VerticalAlignment`, `VirtualSupervisor`, `WarningFlag`, `WebserverErrorPage`, `WeekDate`, `WeekdayOrdinal`, `WideCharacterWidth`, `WireType`, `WritingBuilder`, `XeqConfiguration`, `YamlPathError`

## Retained from the earlier inventory

**C2 — non-established abbreviations** (still current): `AddOp` (symbolism), `CellRef`
(caesura), `Err` (turbulence), `ProcessRef` (guillotine — now `Process.Ref`).
`GitRefError` is resolved: it is `Git.RefError`.

**C4-homonym — one name, two meanings** (still current, and worth watching as nesting
proceeds, since nesting can resolve a homonym by qualifying one of the pair):
`Attributive` (honeycomb, xylophone), `Completion` (exoskeleton, harlequin), `Diagnostic`
(frontier, harlequin), `Executor` (apoplexy, superlunary), `Extensions` (decorum,
gesticulate), `Frame` (perihelion, ultimatum), `Imports` (decorum, stenography),
`Manifest` (embarcadero, revolution), `Proxy` (austronesian, vicarious), `Renderable`
(honeycomb, xylophone), `Syntax` (cataclysm, stenography), `Tag` (honeycomb, xylophone),
`Timestamp` (aviation, embarcadero).

**C4-synonym — competing terms for one role**: the `*Parser` family remains the largest
(`CborParser`, `CssParser`, `JsonParser`, `ProtobufParser`, `TelParser`, `YamlParser`, …).
Most are `private[lib]` engines rather than exported names; `Svg.Parser` shows the shape
the exported ones take once nested.
