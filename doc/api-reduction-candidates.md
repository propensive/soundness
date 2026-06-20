# Public API surface — reduction candidate inventory

Generated read-only from the `soundness_*` re-export files and `module.TypeName.scala` declarations. Categories C1–C4 per the plan. **Identification only** — no fixes applied here.

- modules scanned: 126
- distinct public identifiers exported into `soundness`: 2789
- top-level type declarations (incl. tests): 1377


**Reading the tables (heuristic output — confirm before acting):**
- C1 `→ suggested` nests under the *longest* same-module type that prefixes the name; a few split points still need a human eye (e.g. `Git.RefError` could be `GitRef.Error`).
- C3b lists every `object … extends …Derivable/Derivation`. Those suffixed `*Derivation` are pure backing instances (demote/anonymise). Those named like a public typeclass (`Spannable`, `Tabulable`, `Digestible`, `JsonSchema`, …) *are* the typeclass companion self-deriving — keep the type, the note just flags where derivation lives.
- 'exported' = appears in a `soundness_*` re-export; 'internal' = compiles but not in the `soundness` namespace (already invisible to `import soundness.*`).


## C4-homonym — one name, two meanings (declared in ≥2 modules)

| name | modules | both/which exported |
|---|---|---|
| `Attributive` | honeycomb, xylophone | only honeycomb exported |
| `Benchmarks` | breviloquence, caesura, escapade, gossamer, honeycomb, jacinta, locomotion, panopticon, polysyllabic, punctuation, scintillate, serpentine, stratiform, xylophone, ypsiloid, zephyrine | neither exported |
| `Completion` | exoskeleton, harlequin | only exoskeleton exported |
| `Diagnostic` | frontier, harlequin | only harlequin exported |
| `Executor` | apoplexy, superlunary | only superlunary exported |
| `Extensions` | decorum, gesticulate | only gesticulate exported |
| `Frame` | perihelion, ultimatum | only ultimatum exported |
| `Imports` | decorum, stenography | only stenography exported |
| `Manifest` | embarcadero, revolution | only revolution exported |
| `Proxy` | austronesian, vicarious | only vicarious exported |
| `Renderable` | honeycomb, xylophone | only honeycomb exported |
| `Syntax` | cataclysm, stenography | only stenography exported |
| `Tag` | honeycomb, xylophone | only honeycomb exported |
| `Timestamp` | aviation, embarcadero | only aviation exported |
| `TimingMain` | breviloquence, locomotion | neither exported |

## C4-synonym — competing terms for one role (role-suffix families)

- **`*Builder`** (4): `AsciiBuilder`(gossamer), `BlockBuilder`(punctuation), `TeletypeBuilder`(escapade), `TextBuilder`(gossamer)
- **`*Decoder`** (1): `CharDecoder`(hieroglyph)
- **`*Derivation`** (2): `ProductDerivation`(wisteria), `SumDerivation`(wisteria)
- **`*Encoder`** (1): `CharEncoder`(hieroglyph)
- **`*Parser`** (11): `BlockParser`(punctuation), `CborParser`(breviloquence), `CssParser`(cataclysm), `InlineParser`(punctuation), `JsonParser`(jacinta), `ProtobufParser`(locomotion), `SelectorParser`(cataclysm), `SvgParser`(savagery), `SyntaxParser`(cataclysm), `TelParser`(stratiform), `YamlParser`(ypsiloid)
- **`*Printer`** (1): `ProtobufPrinter`(locomotion)
- **`*Reader`** (1): `FrameReader`(cordillera)
- **`*Tokenizer`** (1): `ValueTokenizer`(cataclysm)
- **`*Writer`** (1): `GivensWriter`(beneficence)

## C3 — typeclass-backing entities (candidates to demote/inline/de-export)

| name | module | kind | visibility |
|---|---|---|---|
| `FrameReader` | cordillera | class | exported |
| `Tokenizer` | decorum | object | exported |
| `TeletypeBuilder` | escapade | class | exported |
| `AsciiBuilder` | gossamer | class | exported |
| `Builder` | gossamer | class | exported |
| `TextBuilder` | gossamer | class | exported |
| `CharDecoder` | hieroglyph | object | exported |
| `CharEncoder` | hieroglyph | object | exported |
| `Parser` | punctuation | object | exported |
| `Renderer` | punctuation | object | exported |
| `SvgParser` | savagery | object | exported |
| `Derivation` | wisteria | trait | exported |
| `ProductDerivation` | wisteria | object | exported |
| `SumDerivation` | wisteria | object | exported |
| `GivensWriter` | beneficence | class | internal |
| `CborParser` | breviloquence | ? | internal |
| `CssParser` | cataclysm | ? | internal |
| `SelectorParser` | cataclysm | ? | internal |
| `SyntaxParser` | cataclysm | ? | internal |
| `ValueTokenizer` | cataclysm | ? | internal |
| `JsonParser` | jacinta | type | internal |
| `ProtobufParser` | locomotion | ? | internal |
| `ProtobufPrinter` | locomotion | ? | internal |
| `BlockBuilder` | punctuation | trait | internal |
| `BlockParser` | punctuation | ? | internal |
| `InlineParser` | punctuation | ? | internal |
| `Serializer` | punctuation | class | internal |
| `TelParser` | stratiform | ? | internal |
| `YamlParser` | ypsiloid | ? | internal |

### C3 triage outcome (the role-suffix heuristic over-captures — verified by hand)

- **KEEP (false positives — public framework or user-facing, not backing engines):** `wisteria.{Derivation,ProductDerivation,SumDerivation}` (the derivation framework users `extends`), `gossamer.{Builder,TextBuilder,AsciiBuilder}` + `escapade.TeletypeBuilder` (user-instantiated builders), `punctuation.{Parser,Renderer}` and `savagery.SvgParser` (documented public entry points), `hieroglyph.{CharDecoder,CharEncoder}` (these *are* the summoned typeclass, named in `using` clauses across modules), `decorum.Tokenizer`, `cordillera.FrameReader` (plain utilities, no typeclass).
- **GENUINE but already `private[module]`** (not in the `soundness` surface — legitimately separate, often thread-local-pooled engines reached from several factory methods; folding them away buys nothing): `jacinta.JsonParser`, `ypsiloid.YamlParser`, `stratiform.TelParser`, `breviloquence.CborParser`, the cataclysm parsers/tokenizer, the punctuation internal cascade (`BlockParser`/`InlineParser`/`BlockBuilder`/`Serializer`), `beneficence.GivensWriter`.
- **ELIMINATED from the surface:** `locomotion.{ProtobufParser,ProtobufPrinter}` — pure codec engines used only inside the Protobuf typeclass givens, yet exported. Marked `@unexported` and dropped from the `soundness` export (kept public, *not* `private[locomotion]`: a private modifier leaks into the inferred type of the public Protobuf givens and breaks downstream `List` derivation). Net: two fewer top-level names in `soundness`.


## C3b — nested derivation objects (generically summoned; need not be named)

These `object …Derivation extends Derivable/…` instances back a typeclass and are reached only via derivation — candidates to make anonymous/private.

| object | module | file |
|---|---|---|
| `AddableDerivation` | wisteria | `lib/wisteria/src/core/wisteria_core.scala` |
| `Bufferable` | polaris | `lib/polaris/src/core/polaris.Bufferable.scala` |
| `Debufferable` | polaris | `lib/polaris/src/core/polaris.Debufferable.scala` |
| `DecodableDerivation` | austronesian | `lib/austronesian/src/core/austronesian.protointernal.scala` |
| `DecodableDerivation` | breviloquence | `lib/breviloquence/src/core/breviloquence.Cbor.scala` |
| `DecodableDerivation` | caesura | `lib/caesura/src/core/caesura.Dsv.scala` |
| `DecodableDerivation` | jacinta | `lib/jacinta/src/core/jacinta.Json.scala` |
| `DecodableDerivation` | legerdemain | `lib/legerdemain/src/core/legerdemain.Query.scala` |
| `DecodableDerivation` | locomotion | `lib/locomotion/src/core/locomotion.Protobuf.scala` |
| `DecodableDerivation` | stratiform | `lib/stratiform/src/core/stratiform.Tel2.scala` |
| `DecodableDerivation` | xylophone | `lib/xylophone/src/core/xylophone.Xml.scala` |
| `DecodableDerivation` | ypsiloid | `lib/ypsiloid/src/core/ypsiloid.Yaml.scala` |
| `Derivation` | chiaroscuro | `lib/chiaroscuro/src/core/chiaroscuro.Contrastable.scala` |
| `Derivation` | chiaroscuro | `lib/chiaroscuro/src/core/chiaroscuro.Decomposable.scala` |
| `Derivation` | spectacular | `lib/spectacular/src/core/spectacular.Inspectable.scala` |
| `Digestible` | gastronomy | `lib/gastronomy/src/core/gastronomy.Digestible.scala` |
| `DivisibleDerivation` | wisteria | `lib/wisteria/src/core/wisteria_core.scala` |
| `EncodableDerivation` | austronesian | `lib/austronesian/src/core/austronesian.protointernal.scala` |
| `EncodableDerivation` | breviloquence | `lib/breviloquence/src/core/breviloquence.Cbor.scala` |
| `EncodableDerivation` | caesura | `lib/caesura/src/core/caesura.Dsv.scala` |
| `EncodableDerivation` | jacinta | `lib/jacinta/src/core/jacinta.Json.scala` |
| `EncodableDerivation` | legerdemain | `lib/legerdemain/src/core/legerdemain.Query.scala` |
| `EncodableDerivation` | locomotion | `lib/locomotion/src/core/locomotion.Protobuf.scala` |
| `EncodableDerivation` | stratiform | `lib/stratiform/src/core/stratiform.Tel2.scala` |
| `EncodableDerivation` | xylophone | `lib/xylophone/src/core/xylophone.Xml.scala` |
| `EncodableDerivation` | ypsiloid | `lib/ypsiloid/src/core/ypsiloid.Yaml.scala` |
| `Formulaic` | legerdemain | `lib/legerdemain/src/core/legerdemain.Formulaic.scala` |
| `JsonSchema` | jacinta | `lib/jacinta/src/schema/jacinta.JsonSchema.scala` |
| `MultiplicableDerivation` | wisteria | `lib/wisteria/src/core/wisteria_core.scala` |
| `Randomizable` | capricious | `lib/capricious/src/core/capricious.Randomizable.scala` |
| `Restorable` | austronesian | `lib/austronesian/src/core/austronesian.Restorable.scala` |
| `Spannable` | caesura | `lib/caesura/src/core/caesura.Spannable.scala` |
| `SubtractableDerivation` | wisteria | `lib/wisteria/src/core/wisteria_core.scala` |
| `Tabulable` | escritoire | `lib/escritoire/src/core/escritoire.Tabulable.scala` |
| `TelsDerivation` | stratiform | `lib/stratiform/src/core/stratiform.Tels2.scala` |

## C2 — non-established abbreviations

| name | module | kind | visibility |
|---|---|---|---|
| `AddOp` | symbolism | trait | exported |
| `CellRef` | caesura | class | exported |
| `Err` | turbulence | object | exported |
| `GitRefError` | octogenarian | object | exported |
| `ProcessRef` | guillotine | trait | exported |

## C1 — multi-word names → `Foo.Bar` (strong: prefix already names a same-module type)

| name | module | → suggested | kind | visibility | prefix is local namespace |
|---|---|---|---|---|---|
| `DagError` | acyclicity | `Dag.Error` | object | exported | yes |
| `DotId` | acyclicity | `Dot.Id` | object | exported | yes |
| `DotIdentifier` | acyclicity | `Dot.Identifier` | object | exported | yes |
| `EnvironmentError` | ambience | `Environment.Error` | class | exported | yes |
| `PropertyError` | ambience | `Property.Error` | class | exported | yes |
| `WorkingDirectoryError` | ambience | `WorkingDirectory.Error` | class | exported | yes |
| `CompilerError` | anthology | `Compiler.Error` | class | exported | yes |
| `JavacOption` | anthology | `Javac.Option` | class | exported | yes |
| `UnusedFeature` | anthology | `Unused.Feature` | enum | exported | yes |
| `ApiError` | apoplexy | `Api.Error` | object | exported | yes |
| `OpenApiError` | apoplexy | `OpenApi.Error` | object | exported | yes |
| `TimestampError` | aviation | `Timestamp.Error` | object | exported | yes |
| `TimezoneError` | aviation | `Timezone.Error` | class | exported | yes |
| `TzdbError` | aviation | `Tzdb.Error` | object | exported | yes |
| `TarCompression` | bitumen | `Tar.Compression` | object | exported | yes |
| `TarError` | bitumen | `Tar.Error` | object | exported | yes |
| `TarHeader` | bitumen | `Tar.Header` | object | exported | yes |
| `AudioError` | cacophony | `Audio.Error` | class | exported | yes |
| `FeedError` | cacophony | `Feed.Error` | object | exported | yes |
| `OutletError` | cacophony | `Outlet.Error` | object | exported | yes |
| `CourierError` | caduceus | `Courier.Error` | class | exported | yes |
| `DsvError` | caesura | `Dsv.Error` | object | exported | yes |
| `DsvFormat` | caesura | `Dsv.Format` | class | exported | yes |
| `DsvRedesignation` | caesura | `Dsv.Redesignation` | trait | exported | yes |
| `RandomSize` | capricious | `Random.Size` | trait | exported | yes |
| `CssConvertible` | cataclysm | `Css.Convertible` | object | exported | yes |
| `CssError` | cataclysm | `Css.Error` | object | exported | yes |
| `CssErrors` | cataclysm | `Css.Errors` | class | exported | yes |
| `SyntaxMatcher` | cataclysm | `Syntax.Matcher` | object | exported | yes |
| `ConnectionError` | coaxial | `Connection.Error` | object | exported | yes |
| `DomainSocketEndpoint` | coaxial | `DomainSocket.Endpoint` | class | exported | yes |
| `AttemptTactic` | contingency | `Attempt.Tactic` | class | exported | yes |
| `HpackTable` | cordillera | `Hpack.Table` | class | exported | yes |
| `Http2Connection` | cordillera | `Http2.Connection` | object | exported | yes |
| `Http2Error` | cordillera | `Http2.Error` | object | exported | yes |
| `Http2Event` | cordillera | `Http2.Event` | object | exported | yes |
| `FqcnError` | digression | `Fqcn.Error` | object | exported | yes |
| `DiffError` | dissonance | `Diff.Error` | class | exported | yes |
| `RedraftError` | dissonance | `Redraft.Error` | object | exported | yes |
| `ImageRecord` | embarcadero | `Image.Record` | class | exported | yes |
| `BlockCipherMode` | enigmatic | `BlockCipher.Mode` | object | exported | yes |
| `BlockCipherPadding` | enigmatic | `BlockCipher.Padding` | object | exported | yes |
| `CipherSession` | enigmatic | `Cipher.Session` | trait | exported | yes |
| `CoseAlgorithm` | enigmatic | `Cose.Algorithm` | object | exported | yes |
| `CoseAuthenticator` | enigmatic | `Cose.Authenticator` | object | exported | yes |
| `CoseError` | enigmatic | `Cose.Error` | object | exported | yes |
| `CoseRecipient` | enigmatic | `Cose.Recipient` | class | exported | yes |
| `CoseStructure` | enigmatic | `Cose.Structure` | trait | exported | yes |
| `CoseVerifier` | enigmatic | `Cose.Verifier` | object | exported | yes |
| `CryptoError` | enigmatic | `Crypto.Error` | object | exported | yes |
| `HmacCipher` | enigmatic | `Hmac.Cipher` | object | exported | yes |
| `PemError` | enigmatic | `Pem.Error` | object | exported | yes |
| `PemLabel` | enigmatic | `Pem.Label` | object | exported | yes |
| `SymmetricKey` | enigmatic | `Symmetric.Key` | object | exported | yes |
| `ColumnAlignment` | escritoire | `Column.Alignment` | object | exported | yes |
| `UpgradeError` | ethereal | `Upgrade.Error` | object | exported | yes |
| `LspClient` | exegesis | `Lsp.Client` | trait | exported | yes |
| `LspError` | exegesis | `Lsp.Error` | object | exported | yes |
| `LspServer` | exegesis | `Lsp.Server` | trait | exported | yes |
| `CreateNonexistentParents` | galilei | `CreateNonexistent.Parents` | trait | exported | yes |
| `GeolocationError` | geodesy | `Geolocation.Error` | object | exported | yes |
| `MediaType` | gesticulate | `Media.Type` | object | exported | yes |
| `MediaTypeError` | gesticulate | `MediaType.Error` | object | exported | yes |
| `MultipartError` | gesticulate | `Multipart.Error` | object | exported | yes |
| `ReferenceError` | gnossienne | `Reference.Error` | object | exported | yes |
| `PidError` | guillotine | `Pid.Error` | class | exported | yes |
| `ProcessRef` | guillotine | `Process.Ref` | trait | exported | yes |
| `RasterError` | hallucination | `Raster.Error` | class | exported | yes |
| `ClasspathEntry` | hellenism | `Classpath.Entry` | object | exported | yes |
| `ClasspathError` | hellenism | `Classpath.Error` | class | exported | yes |
| `ClasspathEvent` | hellenism | `Classpath.Event` | object | exported | yes |
| `BaseLayout` | imperial | `Base.Layout` | object | exported | yes |
| `UuidError` | inimitable | `Uuid.Error` | class | exported | yes |
| `JsonBlueprint` | jacinta | `Json.Blueprint` | object | exported | yes |
| `JsonBlueprintDoc` | jacinta | `JsonBlueprint.Doc` | class | exported | yes |
| `JsonBlueprintError` | jacinta | `JsonBlueprint.Error` | object | exported | yes |
| `JsonError` | jacinta | `Json.Error` | object | exported | yes |
| `JsonPointer` | jacinta | `Json.Pointer` | object | exported | yes |
| `JsonPointerError` | jacinta | `JsonPointer.Error` | object | exported | yes |
| `JsonPrimitive` | jacinta | `Json.Primitive` | object | exported | yes |
| `JsonSchema` | jacinta | `Json.Schema` | object | exported | yes |
| `GlobToken` | kaleidoscope | `Glob.Token` | object | exported | yes |
| `RegexError` | kaleidoscope | `Regex.Error` | object | exported | yes |
| `CompileErrorId` | larceny | `CompileError.Id` | object | exported | yes |
| `QueryError` | legerdemain | `Query.Error` | object | exported | yes |
| `ProtobufError` | locomotion | `Protobuf.Error` | object | exported | yes |
| `PermutationError` | metamorphose | `Permutation.Error` | object | exported | yes |
| `SerializationError` | monotonous | `Serialization.Error` | class | exported | yes |
| `MonikerError` | nomenclature | `Moniker.Error` | object | exported | yes |
| `GrpcChannel` | obligatory | `Grpc.Channel` | object | exported | yes |
| `GrpcError` | obligatory | `Grpc.Error` | class | exported | yes |
| `GrpcFraming` | obligatory | `Grpc.Framing` | object | exported | yes |
| `JsonRpcError` | obligatory | `JsonRpc.Error` | object | exported | yes |
| `SseError` | obligatory | `Sse.Error` | object | exported | yes |
| `SseSource` | obligatory | `Sse.Source` | class | exported | yes |
| `GitCommand` | octogenarian | `Git.Command` | object | exported | yes |
| `GitError` | octogenarian | `Git.Error` | object | exported | yes |
| `GitEvent` | octogenarian | `Git.Event` | object | exported | yes |
| `GitPathStatus` | octogenarian | `Git.PathStatus` | class | exported | yes |
| `GitProcess` | octogenarian | `Git.Process` | class | exported | yes |
| `GitRefError` | octogenarian | `Git.RefError` | object | exported | yes |
| `GitRefs` | octogenarian | `Git.Refs` | object | exported | yes |
| `GitRepo` | octogenarian | `Git.Repo` | object | exported | yes |
| `GitStatus` | octogenarian | `Git.Status` | enum | exported | yes |
| `OAuthError` | orthodoxy | `OAuth.Error` | object | exported | yes |
| `CurrencyStyle` | plutocrat | `Currency.Style` | trait | exported | yes |
| `HyphenationError` | polysyllabic | `Hyphenation.Error` | class | exported | yes |
| `TestId` | probably | `Test.Id` | object | exported | yes |
| `TerminalCanvas` | profanity | `Terminal.Canvas` | object | exported | yes |
| `TerminalError` | profanity | `Terminal.Error` | class | exported | yes |
| `TerminalEvent` | profanity | `Terminal.Event` | trait | exported | yes |
| `TerminalFeature` | profanity | `Terminal.Feature` | class | exported | yes |
| `MarkdownPalette` | punctuation | `Markdown.Palette` | object | exported | yes |
| `ManifestAttribute` | revolution | `Manifest.Attribute` | class | exported | yes |
| `ManifestEntry` | revolution | `Manifest.Entry` | class | exported | yes |
| `SemverError` | revolution | `Semver.Error` | object | exported | yes |
| `SvgDef` | savagery | `Svg.Def` | trait | exported | yes |
| `SvgError` | savagery | `Svg.Error` | object | exported | yes |
| `HttpServerEvent` | scintillate | `HttpServer.Event` | object | exported | yes |
| `JavaServletFn` | scintillate | `JavaServlet.Fn` | class | exported | yes |
| `BenchError` | sedentary | `Bench.Error` | class | exported | yes |
| `PathError` | serpentine | `Path.Error` | object | exported | yes |
| `Base256Error` | stratiform | `Base256.Error` | object | exported | yes |
| `BintelError` | stratiform | `Bintel.Error` | object | exported | yes |
| `MutationError` | stratiform | `Mutation.Error` | object | exported | yes |
| `TelError` | stratiform | `Tel.Error` | object | exported | yes |
| `TelPath` | stratiform | `Tel.Path` | object | exported | yes |
| `VarintError` | stratiform | `Varint.Error` | object | exported | yes |
| `WatchError` | surveillance | `Watch.Error` | object | exported | yes |
| `WatchEvent` | surveillance | `Watch.Event` | enum | exported | yes |
| `McpClient` | synesthesia | `Mcp.Client` | trait | exported | yes |
| `McpError` | synesthesia | `Mcp.Error` | object | exported | yes |
| `McpServer` | synesthesia | `Mcp.Server` | trait | exported | yes |
| `McpSpecification` | synesthesia | `Mcp.Specification` | object | exported | yes |
| `WebDriverError` | tarantula | `WebDriver.Error` | object | exported | yes |
| `AuthError` | telekinesis | `Auth.Error` | class | exported | yes |
| `HttpClient` | telekinesis | `Http.Client` | object | exported | yes |
| `HttpError` | telekinesis | `Http.Error` | class | exported | yes |
| `HttpEvent` | telekinesis | `Http.Event` | object | exported | yes |
| `HttpRedirection` | telekinesis | `Http.Redirection` | object | exported | yes |
| `HttpRequestError` | telekinesis | `Http.RequestError` | object | exported | yes |
| `HttpResponseError` | telekinesis | `Http.ResponseError` | object | exported | yes |
| `LineSeparation` | turbulence | `Line.Separation` | object | exported | yes |
| `ShaderPlugin` | umbrageous | `Shader.Plugin` | class | exported | yes |
| `EmailAddressError` | urticose | `EmailAddress.Error` | object | exported | yes |
| `HostnameError` | urticose | `Hostname.Error` | object | exported | yes |
| `UrlError` | urticose | `Url.Error` | object | exported | yes |
| `UrlFragment` | urticose | `Url.Fragment` | object | exported | yes |
| `ForeignLibrary` | xenophile | `Foreign.Library` | object | exported | yes |
| `TypescriptDialect` | xenophile | `Typescript.Dialect` | object | exported | yes |
| `WebIdlDialect` | xenophile | `WebIdl.Dialect` | object | exported | yes |
| `WitDialect` | xenophile | `Wit.Dialect` | object | exported | yes |
| `XPathError` | xylophone | `XPath.Error` | object | exported | yes |
| `XmlError` | xylophone | `Xml.Error` | class | exported | yes |
| `XmlSchema` | xylophone | `Xml.Schema` | object | exported | yes |
| `PtyEscapeError` | yossarian | `Pty.EscapeError` | object | exported | yes |
| `PtyState` | yossarian | `Pty.State` | class | exported | yes |
| `YamlError` | ypsiloid | `Yaml.Error` | object | exported | yes |
| `YamlPath` | ypsiloid | `Yaml.Path` | object | exported | yes |
| `YamlPathError` | ypsiloid | `YamlPath.Error` | object | exported | yes |
| `YamlPrimitive` | ypsiloid | `Yaml.Primitive` | object | exported | yes |
| `ZipError` | zeppelin | `Zip.Error` | object | exported | yes |
| `ZipEvent` | zeppelin | `Zip.Event` | object | exported | yes |

_(163 strong; 490 total exported multi-word)_


## Pilot (enigmatic) — done, and gotchas for the fix step

Executed as the reference transformation: `PemError → Pem.Error`, `PemLabel → Pem.Label` (folded into the existing `object Pem`, the two `enigmatic.Pem{Error,Label}.scala` files deleted, both names dropped from the `soundness` export). `enigmatic.{core,cose,openssl,test}` compile clean; all 51 tests pass. Net: two fewer top-level names in `soundness`.

Gotchas observed (apply to every C1 rename):
- **`XxxError → Xxx.Error` shadows the base.** A nested `case class Error` makes the bare `Error` in its own `extends` clause self-referential — qualify the base as `extends fulminate.Error(...)`. This bites *every* error-nesting (the largest C1 sub-cluster).
- **Move imports with the body.** Nesting `PemLabel` pulled `spectacular.*` (its `Showable`) into `Pem.scala`; check the donor file's imports when folding a type in.
- **SN-847**: deleting the old `module.Xxx.scala` file is required (the type is no longer top-level); the receiving `module.Foo.scala` already satisfies the rule. SN-398 ordering (companion `object` before its `class`/`enum`) must hold for the *nested* pair too.
- **Enum cases** (`Proprietary`, `fromOrdinal`) resolve unqualified inside the nested companion object exactly as they did at top level — no change needed there.
