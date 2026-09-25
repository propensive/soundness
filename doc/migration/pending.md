# Changes since 0.68.0

This file is read by an LLM agent to upgrade code that consumes Soundness libraries from
0.68.0 to the next release. Each entry states precisely what changed; see `AGENTS.md` for the
format. Entries are grouped by module, most-recently-added last within a module.

## anticipation

- `anticipation.Termcap` gained `def height: Int = Int.MaxValue` beside `width`, with the same
  meaning for rows that `width` has for columns: `Int.MaxValue` when the height is unknown or
  unbounded. An implementation that already declares a member named `height` must mark it
  `override`. (#2064)
- Module `anticipation.html` removed, with `anticipation.GenericHtmlAttribute[-value]` (members
  `type Self <: String & Singleton`, `def name: Text`, `def serialize(value: value): Text`) and its
  `soundness.GenericHtmlAttribute` export. Nothing consumed the typeclass; honeycomb's
  `Attribute` is the HTML attribute typeclass. (#PR-dead-edges)
- Behaviour change: the given `anticipation.Checkable.stream` (`Chain[left] is Checkable against
  Chain[right]`) now requires the two chains to have the same length; previously it compared
  element-wise over the shorter chain, so `Chain(1, 2) === Chain(1)` was `true` and is now
  `false`. `anticipation.check` no longer depends on `rudiments.core`. (#PR-dead-edges)
- `tarantula.Focusable` moved to `anticipation.Focusable` (new module `anticipation.focus`, in the
  `base` bundle; `soundness.Focusable` now comes from anticipation). Members are unchanged
  (`def strategy: Text`, `def focus(value: Self): Text`, the `Focusable(strategy, focus)` factory),
  and it is now a `Typeclass.Pure`. Its instances moved into the subject types' companions:
  `Text` (`Focusable.text`), `honeycomb.Tag.focusable: [tag <: Tag] => tag is Focusable`,
  `honeycomb.ClassList.focusable: [classes <: ClassList] => classes is Focusable`,
  `nomenclature.DomId.focusable: Name[DomId] is Focusable`, `cataclysm.SelectorList.focusable`
  and `xylophone.XPath.focusable`; `tarantula.Focusable.{text, tag, domId, cssClass, selector,
  xpath}` no longer exist. Strategies and renderings are unchanged. (#PR-dead-edges)

## caduceus

- The given `caduceus.Sendable.htmlDoc: (dom: Dom, monitor: Monitor, probate: Probate) =>
  (Document[Html] is Sendable)^{monitor}` replaced by `Sendable.document: [document: Media] =>
  (streamable: (document is Streamable by Text over Credit)^) => (document is Sendable)^{streamable}`,
  which sends a `text/html` document as `Email.Body.HtmlOnly` and any other as `Email.Body(text)`.
  `Document[Html]` still resolves through honeycomb's `Html.media` and `Html.streamable` (the
  latter needing a `Monitor` and `Probate` at the use site, as before); a `Dom` is no longer
  required. `caduceus.core` now depends on `gesticulate.core` and `turbulence.core` instead of
  `honeycomb.core`, so a consumer that reached honeycomb (or parasite) only through caduceus must
  declare it. (#PR-dead-edges)

## cartouche

- `cartouche.core` no longer declares a dependency on `hypotenuse.core` (it still arrives
  transitively through `capricious.core`). (#PR-dead-edges)

## cataclysm

- The givens `cataclysm.Css.Convertible.srgb: Srgb is Css.Convertible of "color"` and
  `Css.Convertible.chroma: Chroma is Css.Convertible of "color"` replaced by one
  `Css.Convertible.chromatic: [color] => ((? >: color) is Chromatic) => color is Css.Convertible of
  "color"`, which renders any colour with a `Chromatic` instance for it or a supertype (every iridescence colour form, through
  `Color.chromatic`, and `Chroma` itself) as a `#rrggbb` triplet. `Srgb` values render identically.
  `cataclysm.core` depends on `anticipation.color` and no longer on `iridescence.core`; a consumer
  that reached iridescence only through cataclysm must declare it. (#PR-dead-edges)
- `cataclysm.core` no longer ships or reads the classpath resources `/cataclysm/properties.json`
  and `/cataclysm/syntaxes.json`: the MDN property and syntax tables are compiled into string
  tables (`cataclysm.CssData`, private) at build time from `lib/cataclysm/data`. Behaviour of
  `PropertyDef` and `SyntaxMatcher` is unchanged. `cataclysm.core` no longer depends on
  `jacinta.core` or `hellenism.core` and is no longer marked JVM/JS-only; `cataclysm.html` (which
  reads stylesheets with `cp"…"`) declares `hellenism.core` itself. A consumer that reached jacinta
  or hellenism only through `cataclysm.core` must declare it. (#PR-dead-edges)
- New module `cataclysm.fonts` (in the `web` bundle; dependencies `cataclysm.core`, `phoenicia.core`,
  `monotonous.core`, `anticipation.url`) now holds the font half of cataclysm: `cataclysm.Web` (with
  its generic-family `Typesettable` givens), `cataclysm.FontFace`, the `style` extensions on
  `phoenicia.Face` and `Font`, and `Css.fontFace(font)`/`Css.fontFaces(fonts*)`, which are now
  extensions on `Css.type` defined in that module rather than methods of `object Css` (call sites
  are unchanged given `import cataclysm.*` or `import soundness.*`). Package and names are
  unchanged; the `soundness` exports of `Web`, `FontFace` and `style` moved to
  `soundness_cataclysm_fonts`. `cataclysm.core` no longer depends on `phoenicia.core`,
  `monotonous.core` or `anticipation.url`; a consumer using any of these, or the fonts API, through
  `cataclysm.core` must declare `cataclysm.fonts` (savagery and tasseomancy now do). (#PR-dead-edges)

## coaxial

- `coaxial.Connection` gained a third constructor parameter, `peer: Optional[Text] = Unset`:
  the connecting user's principal name as the kernel reports it (`SO_PEERCRED`), or `Unset`
  where the platform or transport offers none. `Connection(in, out)` still compiles; pattern
  matches on `Connection(in, out)` must bind the third field. (#2064)
- The JVM `DomainSocket#listenConnections` extension gained a second parameter in its handler
  list, `ownerOnly: Boolean = false`: `listenConnections(handler, ownerOnly = true)` sets the
  socket file's mode to `0600` immediately after binding (on a filesystem with POSIX
  permissions), where previously the bound socket always took the process umask. Existing
  calls `listenConnections(handler)` are unchanged. (#2064)

- `coaxial.Trust` gained a fifth constructor parameter, `pinned: Optional[Data] = Unset`: the
  SHA-256 digest of the one peer certificate a connection accepts (see
  `TlsAcceptance#pinning`). `Trust(expired, selfSigned, hostname, anchors)` still compiles;
  pattern matches on `Trust(expired, selfSigned, hostname, anchors)` must bind the fifth
  field. `coaxial.Socket.Error.Reason` gained a variant `Handshake` (number 4); exhaustive
  matches over `Reason` must handle it. (#TBD)

## corpuscular

- `corpuscular.Crc64.table: Array[Long]^{}` changed shape: it now holds eight slicing tables
  flattened end to end (2048 entries, indexed `k*256 + n` for slice `k` in `0 until 8`),
  previously the single 256-entry bytewise table. The first 256 entries are the same bytewise
  table as before, so code that indexes it by a byte value is unaffected; code that relied on
  `table.length == 256` or iterated the whole table must use `table.readable.take(256)`.
  `Crc64.Accumulator`'s results are unchanged.

## delicious

- The extension `delicious.semantic` on `anthology.Notice` (`def semantic:
  Optional[SemanticMessage]`, exported as `soundness.semantic`) removed; write
  `notice.markup.let(SemanticMessage.parse(_))`. `delicious.scala` no longer depends on
  `anthology.core`. (#PR-dead-edges)

## dendrology

- `dendrology.dag` no longer depends on `tessellate.core`; a consumer of `dendrology.dag` that used
  tessellate names without depending on `tessellate.core` or `dendrology.tree` must declare it. (#PR-dead-edges)

## digression

- Behaviour change: `digression.teletypeables.stackTraceTeletype` (and `exceptionTeletype`,
  which delegates to it) now renders each frame's class, separator and method as one contiguous
  word, and its file, colon and line as another (`pkg.Tests.run()  Tests.scala:42`), where
  previously each was a separate table column with a blank cell between them
  (`pkg.Tests . run()  Tests.scala : 42`). Rows remain aligned on the separator and on the
  colon: the class and file are right-padded to the widest in the trace, and the method and
  line are left-aligned. A frame with no line number renders no colon. Code that matched on the
  rendered text (for instance, `Tests.scala : 42`) must expect the contiguous form. (#2052)

## enigmatic

- Additive, but changes which calls compile: the given `enigmatic.SignatureAlgorithm.mlDsa:
  [level <: 44 | 65 | 87: ValueOf] => MlDsa[level] is SignatureAlgorithm` makes
  `Certificate.selfSigned` accept a `PrivateKey[MlDsa[level]]` (previously a missing-given error).
  It writes `id-ml-dsa-44/65/87` (`2.16.840.1.101.3.4.3.17/18/19`, no parameters) and ignores
  the `Signature.Digest` in scope. Also new: `enigmatic.Certificate.issued[holder <: Cipher,
  signer <: Cipher](subject: Distinguished, key: PublicKey[holder], issuer: Distinguished,
  issuerKey: PrivateKey[signer], validity: Period[Instant over Unix], serial: BigInt, authority:
  Boolean = false, alternatives: List[Text] = Nil)(using signer & Signing, signer is
  SignatureAlgorithm, Signature.Digest, Hash in Sha2[256], erased Permit[Weakness[signer]])(using
  Tactic[Certificate.Error], Tactic[Asn1.Error], Diagnostics): Certificate` (adds an
  `AuthorityKeyIdentifier` extension; `selfSigned` output is unchanged) and
  `enigmatic.Certificate#verify[cipher <: Cipher](issuer: PublicKey[cipher])(using cipher &
  Signing, erased ProcessingPermit[Weakness[cipher]]): Boolean`. (#2073)
- Behaviour change: `enigmatic.PublicKey#verify` through the JDK provider (`JavaBaseCrypto`:
  RSA, ECDSA, DSA and ML-DSA) now returns `false` for a signature the JDK cannot decode (wrong
  length, malformed DER, out-of-range ML-DSA hints), where previously a
  `java.security.SignatureException` escaped. (#2073)

## distillate

- `distillate.Decodable.enumeration` (the `enumeration is Decodable in Text` given for
  `reflect.Enum` subtypes) now raises `distillate.Enumerable.Error(inputLabel: Text, sum: Text,
  validVariants: List[Text])` (`SN-900`) where it previously raised `wisteria.Variant.Error` with the
  same three fields; its `Tactic` requirement changed accordingly. `distillate.core` now depends on
  `contingency.core` instead of `wisteria.core`, so a consumer that reached wisteria (or vicarious)
  only through distillate must declare it. (#PR-dead-edges)

## enigmatic

- `enigmatic.Pem` (with `Pem.Label`, `Pem.Error`, `SN-389`, and its `Decodable`/`Aggregable`
  givens) moved from module `enigmatic.core` to `enigmatic.asn1`; the package and names are
  unchanged, and `enigmatic.core` still re-exports it transitively through its dependency on
  `enigmatic.asn1`. The `soundness` export of `Pem` moved from `soundness_enigmatic_core` to
  `soundness_enigmatic_asn1` (same name, `soundness.Pem`). (#PR-dead-edges)
- `enigmatic.Signing` (`def sign(data: Data, privateKey: Data): Data`, `def verify(data: Data,
  signature: Data, publicKey: Data): Boolean`) moved to `gastronomy.Signing` (module
  `gastronomy.core`), and its `soundness` export moved with it (still `soundness.Signing`).
  `Rsa`, `Dsa`, `Ecdsa`, `MlDsa` and `HmacCipher` still extend it. (#PR-dead-edges)

## escapade

- `escapade.core` no longer depends on `mercator.core` or `zephyrine.core`; a consumer that reached
  either only through escapade must declare it. (#PR-dead-edges)

## ethereal

- `ethereal.Stdin` renamed to `ethereal.Terminus`, with its cases `Terminal` and `Pipe` and its
  `Terminus is Decodable in Text` / `Terminus is Encodable in Text` givens unchanged. The
  `soundness` umbrella exports `Terminus` in place of `Stdin`. (#2046)
- `ethereal.DaemonService`'s `cliInput` parameter is now typed `ethereal.Terminus` instead of
  `ethereal.Stdin`, and two parameters were inserted after it, so the constructor is now
  `DaemonService[bus <: Matchable](pid: Pid, shutdown: () => Unit, cliInput: Terminus,
  cliOutput: Terminus, cliError: Terminus, executable: Path on Local, deliver: bus => Unit,
  bus: Chain[bus], script: Text, startTime: Long, helpThunk: () => Optional[Help],
  setMode: Tty => Unit)`. `cliInput` still reports whether the client's stdin is a terminal;
  `cliOutput` and `cliError` report the same for its stdout and stderr, which a daemon cannot
  determine for itself. `DaemonService#cooked` still keys off `cliInput` alone. (#2046)
- `ethereal.Launcher.Message.Init`'s `tty: Boolean` parameter was replaced by `stdinTty: Boolean`,
  `stdoutTty: Boolean` and `stderrTty: Boolean` in the same position, so the case is now
  `Init(pid: Int, uid: Int, username: Text, script: Text, pwd: Text, stdinTty: Boolean,
  stdoutTty: Boolean, stderrTty: Boolean, arguments: List[Text], environment: List[Text])`.
  (#2046)
- The `ethereal-launcher` wire schema changed correspondingly: `record Init`'s `tty` field became
  `stdin-tty`, `stdout-tty` and `stderr-tty`, and its `argument` and `environment` fields moved
  from indices 6 and 7 to 8 and 9. `ethereal.Launcher.signature` is therefore now
  `eeced165c15f73119cf7710812671924aa558722927d29f37538e7b3953296c2ce`, previously
  `4701ec19cd0fd3ecfc0e1b8a6525b4edc3a3b1deda370f681986db9aa39c1da692`. Launcher and daemon
  compare signatures on every document and refuse a mismatch, so a daemon built against this
  release will not communicate at all with a launcher from XEQ 0.6 or earlier: every executable
  packaged with an older `xeq` must be repackaged with `xeq` 0.7 or later. (#2046)
- `ethereal.DaemonLogEvent.Failure`, previously a nullary case, is now
  `case Failure(error: Text)`, carrying the `toString` of the throwable which failed the
  invocation. Its `Communicable` rendering changed from "the connection handler failed" to
  "the invocation failed: <error>". Pattern matches on `DaemonLogEvent.Failure` must bind or
  ignore the parameter. Behaviour change: an invocation which throws a `java.lang.Throwable`
  that is not an `Exception` now settles the client's exit status through the `Backstop`
  (`Exit(2)` for every provided backstop), where it previously left the client waiting
  indefinitely. (#2033)

- The `ethereal-launcher` wire schema is that of XEQ 0.8: `record Init` gains `invoked-as`,
  `umask`, `columns`, `rows`, `input-codepage` and `output-codepage` (indices 10–15; existing
  fields keep theirs); `record Signal` gains `columns`, `rows` and `deadline` (indices 2–4);
  `select Message` gains the variants `closed` (10) and `shutdown` (11); and `uid` is the
  platform's user identifier, a SID on Windows. `ethereal.Launcher.signature` is therefore now
  `e50b7e82c11b06783dafa8a2ecc4e35f7ba31044ecd38fc5d9fe9e47a7c11e59e5`, previously
  `eeced165c15f73119cf7710812671924aa558722927d29f37538e7b3953296c2ce`, so a daemon built
  against this release communicates only with a launcher from XEQ 0.8 or later: every
  executable packaged with an older `xeq` must be repackaged. The `xeq` pin in `etc/xeq.tsv` is
  0.8. (#2064)
- `ethereal.Launcher.Message.Init` is now `Init(pid: Int, uid: Text, username: Text, script:
  Text, pwd: Text, stdinTty: Boolean, stdoutTty: Boolean, stderrTty: Boolean, arguments:
  List[Text], environment: List[Text], invokedAs: Optional[Text] = Unset, umask: Optional[Text]
  = Unset, columns: Optional[Int] = Unset, rows: Optional[Int] = Unset, inputCodepage:
  Optional[Int] = Unset, outputCodepage: Optional[Int] = Unset)`: `uid` was `Int`, and six
  optional parameters were appended. `Message.Signal` is now `Signal(pid: Int, name: Text,
  columns: Optional[Int] = Unset, rows: Optional[Int] = Unset, deadline: Optional[Long] =
  Unset)`, previously `Signal(pid: Int, name: Text)`. Pattern matches on either case must bind
  or ignore the new fields. New cases `Message.Closed(pid: Int, stream: Text)` and
  `Message.Shutdown` were added; an exhaustive match on `Message` must handle them. (#2064)
- `ethereal.DaemonService` gained three trailing constructor parameters, so it is now
  `DaemonService[bus <: Matchable](pid: Pid, shutdown: () => Unit, cliInput: Terminus,
  cliOutput: Terminus, cliError: Terminus, executable: Path on Local, deliver: bus => Unit,
  bus: Chain[bus], script: Text, startTime: Long, helpThunk: () => Optional[Help], setMode: Tty
  => Unit, invokedAs: Optional[Text], sizeThunk: () => Optional[(Int, Int)], umask:
  Optional[Umask])`. It now also extends `galilei.Umask.Provider`, so a `DaemonService` in
  scope makes the invocation's `Umask` summonable, and overrides `Entrypoint#retire()`. Its
  `shutdown` thunk now *drains* the daemon — no further invocation is served, those in flight
  finish, then the process exits — where it previously exited as soon as the calling invocation
  ended, cutting short any other in flight. (#2064)
- The daemon now sets its socket to mode `0600` and refuses a connection whose peer user, as
  the kernel reports it, is not the socket's owner, or whose `init` document claims a `uid`
  other than the daemon's own `ethereal.user.id` (read as text, previously as `Int`); a
  refused invocation exits with status 2. A daemon answering `verify` with a stale verdict now
  drains as for `shutdown` instead of exiting immediately, for at most 30 seconds. (#2064)
- Behaviour change: after the launcher reports that the client's stdout or stderr has lost its
  reader (`closed`), the invocation's next write to that stream raises the new
  `ethereal.Outlet.Error(stream: Text)`, and an invocation that lets it escape ends with exit
  status 141 without consulting the `Backstop`; previously such writes were silently
  discarded and the invocation ran on. (#2064)
- `ethereal.DaemonLogEvent` gained the cases `PeerRefused(user: Text)`, `Draining`,
  `Refused(pid: Pid)` and `Closed(pid: Pid, stream: Text)`; an exhaustive match must handle
  them. (#2064)
- The `soundness` umbrella additionally exports `ethereal.Outlet` and `ethereal.Transcoder`. (#2064)
- `ethereal.core` no longer depends on `telekinesis.jvm` or `urticose.url`; a consumer that reached
  telekinesis, urticose or `legerdemain.query` only through ethereal must declare it. (#PR-dead-edges)

## exoskeleton

- `exoskeleton.Enclave.Launcher#sandbox` requires one more piece of evidence, so its context
  parameter list is now `(using Tactic[Enclave.Error], Tactic[Exec.Error], Tactic[Number.Error],
  Tactic[Path.Error])`. The new `exoskeleton.Enclave.Error(tool: Path on Linux)` is raised when
  the sandboxed tool's daemon answers the `{admin} pid` request with no output; previously that
  case surfaced as a `distillate.Number.Error` from decoding the empty string. (#2046)
- `exoskeleton.Enclave.Launcher#sandbox` now kills the daemon and deletes the installed
  completion scripts on every exit from the block, including an abort or a thrown exception;
  previously the teardown ran only when the block returned normally, leaving a daemon process
  alive after a failure. (#2046)
- `exoskeleton.Manpages.install(page: Roff, force: Boolean = false)` gained the required
  givens `ambience.Environment` and `ambience.System`, in the second `using` clause:
  `(using erased effectful: Effectful)(using Environment, System, Diagnostics)(using (Io.Event
  is Loggable)^)(using Tactic[Install.Error])`. It no longer imports
  `environments.javaBaseEnvironment` or `systems.javaBaseSystem` itself. The XDG directories
  are now resolved from the given `Environment`, not the JVM's. An `exoskeleton.Cli` in scope
  supplies the `Environment` (through `Environment.Provider`); `systems.javaBaseSystem` supplies
  the `System`. (#2034)
- `exoskeleton.Completions.ensure(force: Boolean = false)` gained the required givens
  `ambience.Environment` and `ambience.System`: `(using Entrypoint^, Environment, System,
  WorkingDirectory, Diagnostics)(using (CliEvent is Loggable)^)`, previously
  `(using Entrypoint^, WorkingDirectory, Diagnostics)(using (CliEvent is Loggable)^)`. Same
  resolution and semantics as for `Manpages.install`. (#2034)
- `exoskeleton.Completions.install(force: Boolean = false)` gained the required givens
  `ambience.Environment` and `ambience.System`: `(using entrypoint: Entrypoint^)(using erased
  effectful: Effectful)(using Environment, System, WorkingDirectory, Diagnostics)(using (CliEvent
  is Loggable)^)(using Tactic[Install.Error])`, previously without `Environment, System`. The
  overload `install(shell: Shell, command: Text, scriptName: Name[Linux], dirs: List[Path on
  Linux])` is unchanged. (#2034)
- `exoskeleton.Cli.arguments(textArguments: List[Text], focus: Optional[Int] = Unset, position:
  Optional[Int] = Unset, tab: Optional[Ordinal] = Unset)` changed semantics: the `Argument` at
  index `focus` now has `cursor = position.or(text.length)` where it previously had
  `cursor = position`, so a focused argument always carries a cursor (at the end of the word
  when `position` is `Unset`). Arguments at other indices, and every argument when `focus` is
  `Unset`, are unchanged. (#1964)
- `exoskeleton.Interpreter#focus` for `interpreters.posixInterpreter` and
  `interpreters.posixClusteringInterpreter` changed semantics: the focus is now derived from the
  argument carrying the cursor — that argument's own piece when it is a flag, otherwise the
  flag preceding it — and is `Unset` when no argument carries a cursor. Previously it was
  always derived from the last flag on the command line. Consequently a `Discoverable` for a
  flag's operand is now consulted wherever the flag stands, not only when it is last. (#1964)
- `exoskeleton.Completion#serialize` changed semantics: when the text being completed starts
  with `-`, cursor suggestions whose `core` does not start with `-` are dropped, and if none
  remain the flag list is offered, where previously any cursor suggestion suppressed the flag
  list; and suggestions are wrapped by the focused argument's format only when the focus is the
  argument being completed, not when it is the flag preceding it. An application no longer
  needs a "flag-first" match arm ahead of its `Subcommand` patterns for `--fl<TAB>` to offer
  flags. (#2035)
- `exoskeleton.Flag` gained `def present(using Cli, Interpreter, Topic is Interpretable, (? <:
  Topic) is Discoverable): Boolean` and `def value(using Cli, Interpreter, Topic is
  Interpretable, (? <: Topic) is Discoverable)(using erased Effectful): Optional[Topic]`, plain
  (non-inline) readers which register the flag as `apply()` does. Code that reads
  `flag().present` inside an `inline def` must use `flag.present` instead: there the transparent
  `apply()` is not expanded and `.present` resolves against `Prospective[Topic] |
  Optional[Topic]` through vacuous's `Optional` extension, answering `true` unconditionally.
  (#2032)
- `exoskeleton.Login.id` is now `Optional[UserId]`, previously `Optional[Int]`: the new opaque
  `exoskeleton.UserId` (in the `args` module, exported by `soundness`) wraps the platform's
  user identifier as text — a numeric uid on Unix, a SID on Windows — with `text`, `unix:
  Optional[Int]` and `sid: Optional[Text]`. Code reading `login.id` as an `Int` must use
  `login.id.let(_.unix)`; code constructing a `Login` with a numeric id must wrap it,
  `Login(name, UserId(t"501"))`. (#2064)
- `exoskeleton.Cli#trap` and the top-level `exoskeleton.trap` now take
  `PartialFunction[Signal, SignalResponse]`, previously `PartialFunction[UnixSignal |
  WindowsSignal, SignalResponse]`, and `Cli#dispatchSignal` takes a `profanity.Signal` rather
  than a `UnixSignal | WindowsSignal`. A handler `case Interrupt.Int => …` becomes `case
  Signal(Interrupt.Int, _, _, _) => …`, and `case _: UnixSignal => …` becomes `case Signal(_:
  UnixSignal, _, _, _) => …`. (#2064)
- `exoskeleton.Entrypoint` gained `def retire(): Unit = ()`, and the `{admin}` subcommand of the
  completions executive gained `shutdown`, which calls it. (#2064)
- `exoskeleton.core` depends on `galilei.core` instead of `galilei.jvm`; a consumer that reached
  `galilei.jvm` or `guillotine.core` only through exoskeleton must declare it. (#PR-dead-edges)

## galilei

- `galilei.FilesystemBackend`'s creation primitives take a mode: `createDirectory(path, mode:
  Optional[Int])`, `createFile(path, mode: Optional[Int])`, `createFifo(path, mode:
  Optional[Int])` and `open(path, flags, mode: Optional[Int])(lambda)`, where `mode` is the
  exact POSIX permission bits to create with, or `Unset` to leave the process umask to apply.
  Every `FilesystemBackend` implementation must add the parameter (and may ignore it where
  the platform has no permission bits, as the WASI backend does); every direct call must pass
  it. (#2064)
- `galilei.Creation.DirectoryCreatable`, `FileCreatable` and `FifoCreatable`, and
  `galilei.FileOpenable`, take one more context parameter, `umask: Umask`, as do the givens
  `Platform.directoryCreatable`, `fileCreatable`, `fifoCreatable` and `File.openable` that
  construct them. The new opaque `galilei.Umask` (exported by `soundness`) has a companion
  given `inherited: Umask` equal to `Umask.process`, so code that summons none is unchanged in
  behaviour; a `Umask` given, or a `Umask.Provider` in scope, is applied to every file and
  directory created through `create[File]`, `create[Directory]`, `create[Fifo]` and
  `open[File](…, OpenFlag.Create)`. (#2064)
- `galilei.Device.create[plane <: Posix: Filesystem](path: Path on plane, kind: Kind, major: Int,
  minor: Int)(using CreateNonexistentParents on plane, OverwritePreexisting on plane,
  WorkingDirectory, guillotine.Exec.Event is Loggable): Path on plane raises Io.Error` lost its
  last two `using` parameters: it now runs `mknod` through a raw `ProcessBuilder` in the JVM's
  working directory and no longer logs the command, and a failure to start the process raises
  `Io.Error(path, Operation.Create, Reason.PermissionDenied)` where it previously raised
  `Reason.Unsupported`. `galilei.jvm` no longer depends on `guillotine.core`; a consumer that
  reached guillotine only through `galilei.jvm` (or through `hellenism.jvm`, `zeppelin.core`,
  `exoskeleton.core`, `octogenarian.core`, `bitumen.jvm`, `facsimile.file`, `hyperbole.stacks`,
  `reliquary.derive`) must declare it. (#PR-dead-edges)
- The given `serpentine.Navigable.uuid: [plane, uuid <: Uuid] => uuid is Navigable on plane`
  moved to `galilei.Platform.uuidNavigable: [uuid <: Uuid, filesystem <: Platform] => uuid is
  Navigable on filesystem`: a `Uuid` is still a path segment on every OS filesystem plane
  (`Local`, `Linux`, `MacOs`, `Posix`, `Windows`), but no longer on other planes (URLs, JSON
  pointers, YAML paths, classpaths). `serpentine.core` no longer depends on `inimitable.core`,
  and `galilei.core` now does. (#PR-dead-edges)

## gesticulate

- The givens `gesticulate.MediaType.formenctype`, `MediaType.media`, `MediaType.enctype` and
  `MediaType.htype` (each `("…" is GenericHtmlAttribute[MediaType])`) removed with
  `anticipation.GenericHtmlAttribute`. `gesticulate.core` no longer depends on `anticipation.html`. (#PR-dead-edges)
- `gesticulate.core` no longer ships or reads the classpath resource `/gesticulate/media.types`:
  the IANA registry is compiled into a string table (`gesticulate.MediaTypeData`, private) at
  build time from `lib/gesticulate/data`. The `media"…"` macro's unregistered-type check is
  unchanged. (#PR-dead-edges)

## gossamer

- The `join` extension methods (`join`, `join(separator)`, `join(left, separator, right)`,
  `join(separator, penultimate)` and `join(left, separator, penultimate, right)`) no longer
  require the joined type to be `gossamer.Textual`, only `gossamer.Joinable`. The extension's
  type parameters changed from `[self, element, textual >: element]` with
  `(using source: self is Joinable.Source by element)(using joinable: textual is Joinable,
  textual0: textual is Textual)` and result type `textual`, to `[self, element, result]` with
  `(using source: self is Joinable.Source by element)(using assembly: self is
  Joinable.Assembly by element to result)` and result type `result`; every separator, prefix
  and suffix parameter is now typed `assembly.Part` instead of `textual`. For textual and
  `Message` elements the result and separator types are unchanged. Code that passed `join`'s
  type arguments explicitly must be rewritten. (#2057)
- New `gossamer.Joinable` givens `Joinable.list: [element] => List[element] is Joinable`,
  `Joinable.chain: [element] => Chain[element] is Joinable`, `Joinable.sequence: [element] =>
  Sequence[element] is Joinable` and `Joinable.set: [element] => Set[element] is Joinable`
  (the `proscenium` shapes), joining by concatenation (union for `Set`), so `join` on a
  collection of those collections, with an optional separator collection, now compiles:
  `List(List(1), List(2)).join(List(0))` is `List(1, 0, 2)`. (#2057)
- New `gossamer.Joinable.Assembly` typeclass (`self is Joinable.Assembly by element to result
  { type Part >: element }`) with givens `Assembly.joinable` (elements that are `Joinable`;
  `Part` and `Result` are that type) and, at lower priority, `Assembly.interleaving`
  (`self is Reshapable by element to result`; `Part = element`). The latter gives `join` a
  second meaning on collections whose elements are not `Joinable`: the separators are
  elements, interleaved into a rebuilt collection of the receiver's shape, so
  `List(1, 2, 3).join(0)` is `List(1, 0, 2, 0, 3)`, where previously it did not compile.
  (#2057)

## harlequin

- `harlequin.Fragment.infixBase` now rejects every Scala 3 keyword, hard and soft, as an infix
  receiver, using the new `prophesy.ScalaKeywords.all` in place of its own private list, which
  lacked `erased`, `macro`, `throws` and `tracked`. A fragment such as `erased x ma` (cursor at
  the end) therefore yields `(Unset, t"ma")` where it previously yielded `(t"x.", t"ma")`.
  Nothing else about its result changed.

## honeycomb

- `honeycomb.core` no longer ships or reads the classpath resources `/honeycomb/entities-html4.tsv`
  and `/honeycomb/entities-extra.tsv`: the named-character-reference tables are compiled into string
  tables (`honeycomb.EntityData`, private) at build time from `lib/honeycomb/data`.
  `Html4Transitional.entities` and `Whatwg.entities` are unchanged. `honeycomb.core` no longer
  depends on `hellenism.core` and is no longer marked JVM/JS-only; a consumer that reached
  hellenism only through honeycomb must declare it. (#PR-dead-edges)

## hypotenuse

- Module `anticipation.check` moved to `hypotenuse.check`, and its package from `anticipation` to
  `hypotenuse`: `anticipation.Checkable`, `anticipation.Tolerance`, and the extensions `===`, `!==`,
  `+/-` and `±` are now `hypotenuse.Checkable`, `hypotenuse.Tolerance`, `hypotenuse.===` etc.
  The `soundness` exports and probably's re-exports (`import probably.*`) are unchanged; code
  that imported them via `import anticipation.*` needs `import hypotenuse.*` instead. Members,
  signatures and behaviour are unchanged. (#PR-dead-edges)

## iridescence

- `iridescence.core` no longer depends on `contextual.core`; a consumer that reached contextual only
  through iridescence must declare it. (#PR-dead-edges)

## jacinta

- `jacinta.JsonPointer(url: Optional[HttpUrl] = Unset, path: Path on JsonPointer = JsonPointer)`
  became `JsonPointer(path: Path on JsonPointer = JsonPointer)`: the `url` field, which no code
  path ever set (the decoder rejects any reference not beginning with `#`), is gone. Pattern
  matches `JsonPointer(url, path)` become `JsonPointer(path)`; `pointer.url` no longer exists.
  `JsonPointer is Encodable in Text` therefore always renders `#` or `#/…`, as it always did in
  practice. (#PR-dead-edges)
- `jacinta.JsonPointer.Registry` (a `beneficence.Findable` with `update(url: HttpUrl, document:
  Json): Unit`, `apply(url: HttpUrl): Optional[Json]` and `protected def lookup(url: HttpUrl):
  Optional[Json]`), `JsonPointer#apply(using registry: JsonPointer.Registry^)(document: Json)(using
  Tactic[JsonPointer.Error]): Json` (which always returned `document`), and the givens
  `jacinta.jsonPointerRegistries.standaloneRegistry` and `jsonPointerRegistries.fetchingRegistry`
  (module `jacinta.schema`, also exported as `soundness.jsonPointerRegistries.*`) removed. No
  replacement: `JsonPointer` addresses the current document only. (#PR-dead-edges)
- `jacinta.JsonPointer.Error.Reason.UnknownDocument` (`SN-415.1`) removed; `ExpectedHash`,
  `ExpectedSlash` and `BadEscape` keep numbers 2, 3 and 4. (#PR-dead-edges)
- `jacinta.core` depends on `serpentine.core` directly and no longer on `urticose.url`; a consumer
  that reached urticose only through jacinta must declare it. (#PR-dead-edges)

## octogenarian

- `octogenarian.core` depends on `enigmatic.asn1` instead of `enigmatic.core`; a consumer that
  reached `enigmatic.core` (or `gastronomy.core`, `aperture.core`) only through octogenarian must
  declare it. (#PR-dead-edges)

## pneumatic

- New `pneumatic.Brotli.continuation(base: Data, next: Data, window: Int = Brotli.Window): Data`
  and `pneumatic.Brotli.prefix(base: Data, window: Int = Brotli.Window, block: Int =
  Brotli.Block): Data`, with the constants `Brotli.Window = 24` and `Brotli.Block = 1 << 24`.
  `continuation` encodes `next` against an LZ77 window preloaded with `base`, producing the
  meta-block(s) for `next` alone with no stream header, ending in ISLAST = 1; `prefix` builds the
  RFC 7932-fixed priming stream (WBITS header, `base` as uncompressed meta-blocks of at most
  `block` bytes, then the empty metadata meta-block `0x06`), so that
  `(prefix(base, w, b) ++ continuation(base, next, w)).decompress[Brotli]` is `base ++ next`.
  Code carrying its own port of the encoder for this purpose (`lira.Priming`) should call these
  instead. Existing `compress[Brotli]` output is byte-for-byte unchanged. (#2047)
- `pneumatic.core` no longer depends on `turbulence.stdio`; a consumer of any pneumatic module that
  used turbulence names without depending on turbulence must declare `turbulence.core` or
  `turbulence.stdio`. (#PR-dead-edges)

## praxinoscope

- `praxinoscope.core` no longer depends on `contextual.core`; a consumer that reached contextual
  only through praxinoscope must declare it. (#PR-dead-edges)

## profanity

- New `profanity.Signal(interrupt: UnixSignal | WindowsSignal, columns: Optional[Int] = Unset,
  rows: Optional[Int] = Unset, deadline: Optional[Quantity[Seconds[1]]] = Unset)`, exported by
  `soundness`, is what `Console#trap` handlers now receive: `Console#trap` takes
  `PartialFunction[Signal, SignalResponse]`, previously `PartialFunction[UnixSignal |
  WindowsSignal, SignalResponse]`. See the `exoskeleton` entry for the rewrite of a handler.
  (#2064)
- `profanity.WindowsSignal`'s `Close`, `Logoff` and `Shutdown` cases now have the short names
  `CTRL_CLOSE`, `CTRL_LOGOFF` and `CTRL_SHUTDOWN`, previously `CLOSE`, `LOGOFF` and `SHUTDOWN`,
  and its `Decodable in Text` given decodes by short name (`CTRL_C`, `CTRL_BREAK`, …), where
  it previously required the case name (`Ctrlc`) and so could decode none of them. (#2064)
- `profanity.Terminal` takes its initial size from the stdio's `Termcap` (`width`/`height`)
  when that carries one, before `LINES`/`COLUMNS`, and on a `WINCH` or `CONT` that carries a
  size it records the size and emits `Terminal.Info.WindowSize` directly, sending only the
  anchor query rather than the cursor-position size probe; the probe is still sent when the
  signal carries no size. (#2064)

## reliquary

- `reliquary.core` depends on `gastronomy.core`'s `Signing` (unchanged name, see enigmatic) and on
  `revolution.semver` instead of `enigmatic.core` and `revolution.core`; a consumer that reached
  `enigmatic.core`, `aperture.core`, `revolution.core`'s manifest types or `turbulence` only
  through reliquary must declare them. (#PR-dead-edges)

## revolution

- New module `revolution.semver` (in the `tool` bundle; dependencies `gossamer.core`,
  `distillate.core`, cross-platform) holds `revolution.Semver`, `revolution.Compatibility`, the
  `v"…"` interpolator and `revolution.internal.semver`; `revolution.core` depends on it and keeps
  `Manifest`, `EncodableManifest`, `DecodableManifest` and `manifestAttributes`. Package and
  names are unchanged. The `soundness` exports of `Semver`, `Compatibility` and `v` moved to
  `soundness_revolution_semver`. (#PR-dead-edges)

## savagery

- `savagery.core` depends on `geodesy.angle` instead of `geodesy.core`; a consumer that used
  `Geolocation`, `Compass` or the other `geodesy.core` types through savagery must declare
  `geodesy.core`. (#PR-dead-edges)

## stratiform

- `stratiform.Tel.Error.Reason.UnconstrainedScalar` (E224) removed: `Tels.Validation` no longer
  rejects a `scalar` declaring neither `validate` nor `pattern`, so such a schema now validates
  where it previously raised `Tel.Error(Reason.UnconstrainedScalar)`. Code matching on the case
  must drop that branch; the error number 224 stays reserved and is not reused. (#2048)
- `stratiform.SchemaResolver.resolve` now answers the built-in `acceptance` schema (BinTEL
  §8.4) at resolution step 1, as it does `tels`: a pragma whose reference is
  `specification.tel/acceptance` (with no selector, or with `:1.0.0`) or whose signature is
  `SchemaResolver.acceptanceSignature` resolves to `Tels.Axiom.acceptance` with
  `Resolved.step == Tels.Resolution.Step.Builtin` and `Resolved.document == Unset`, where it
  previously fell through to the stores, the library and the delegate; a store or delegate
  that served that coordinate or signature is no longer consulted for it. (#2051)
- Module `stratiform.binary` removed; everything it provided — `stratiform.Bintel`,
  `stratiform.Varint`, `stratiform.BintelParser`, `stratiform.BintelReader`,
  `stratiform.SchemaSignature`, `stratiform.SchemaResolver`, and the extension methods
  `bintel`, `bintelDocument` and `valueHash` on `Tel`, `Tel.Element` and `Tel.Encodable`
  values — is now in `stratiform.core` (artifact `stratiform-core`), unchanged in package and
  signature. Build definitions depending on `stratiform.binary` must depend on
  `stratiform.core` instead; `stratiform.binaryStaged` now depends on `stratiform.core`.
  `ulysses.core` and `stratiform.core` are unchanged for the JVM; `ulysses.core` additionally
  now publishes Scala.js and Scala Native artifacts, and `stratiform.core` now depends on
  `gastronomy.core` and `ulysses.core`. (#2051)
- `stratiform.Tels.tels[T](name)` derives a schema of a different shape. A field whose type is a
  case class was `Tels.Field(_, _, keyword, Tels.Struct(members, validators), _)`; it is now
  `Tels.Field(_, _, keyword, Tels.Reference(typeName), _)` with `Tels.RecordDefinition(typeName,
  members, Array.empty)` registered in `Tels#records` (`typeName` is the case class's simple
  name; a root type's own record is registered only when a member references it). A variant of a
  sealed type whose case is a product likewise carries `Tels.Reference(caseName)`. A sum-typed
  field nested inside a product now registers its `Tels.SelectDefinition` in `Tels#selects`
  (previously an unresolvable `Reference`). A required field's `required` polarity is now
  `Tels.Polarity.Implicit` where it was `Tels.Polarity.Tight`, and a `Map` field's `key`/`value`
  members are `Implicit` where they were `Tight`. The BinTEL wire form of a value is unchanged.
  `stratiform.TelBlueprint` records built over a derived schema therefore expose a case-class
  field as a nested `Record` (previously a `Tel`). (#2056)
- `stratiform.TelSchematic` gained members with defaults — `fieldType: Tels.Type`,
  `definitions(seen: scala.collection.immutable.Set[Text]): TelSchematic.Definitions`,
  `layers(seen: scala.collection.immutable.Set[Text]): List[Tels.Layer]`,
  `rootLayers: List[Tels.Layer]` — and its `polarity` default changed
  from `Tels.Polarity.Tight` to `Tels.Polarity.Implicit`; an instance overriding `polarity` is
  unaffected, one relying on the default now derives implicit polarity. (#2056)

## surveillance

- `surveillance.Watch.Event#path[directory: Instantiable across Paths from Text]: directory` is
  now `path[directory](using (directory is Instantiable across Paths from Text)^): directory`: the
  path evidence may capture (as `Path on Local`'s does, through its filesystem). Call sites are
  unchanged; an explicit `using` argument that was previously rejected for capturing is now
  accepted. (#2076)

- `surveillance.core` no longer declares a dependency on `gossamer.core` (it still arrives
  transitively through `turbulence.core`). (#PR-dead-edges)

## tarantula

- `tarantula.core` no longer depends on `cataclysm.core` or `xylophone.core` (see anticipation:
  `Focusable` and its instances moved out of tarantula); a consumer that used either (or jacinta,
  hellenism, phoenicia, quantitative through cataclysm) only through tarantula must declare it. (#PR-dead-edges)

## telekinesis

- The givens `telekinesis.Http.Method.formmethod` and `Http.Method.method` (each
  `("…" is GenericHtmlAttribute[Method])`) removed with `anticipation.GenericHtmlAttribute`. (#PR-dead-edges)

## turbulence

- `turbulence.shred(mean: Double, variance: Double)(using Random): Chain[Data]` (on `Chain[Data]`)
  became `shred(chunkSize: => Int): Chain[Data]`: the caller now supplies each chunk's size
  (evaluated afresh per chunk, floored at 1) instead of a gamma distribution's parameters. The
  old behaviour is `given Distribution = Gamma.approximate(mean, variance)` followed by
  `stream.shred(arbitrary[Double]().toInt)` under a `Random`. `turbulence.core` no longer depends
  on `capricious.core`, so a consumer that reached capricious (or wisteria, hypotenuse) only
  through turbulence must declare it. (#PR-dead-edges)

## vivisection

- `vivisection.Jdwp.Capabilities` gains two fields: `canGetBytecodes: Boolean` inserted as the
  third parameter (after `canWatchFieldAccess`) and `canGetConstantPool: Boolean` appended last,
  so the constructor is now `Capabilities(canWatchFieldModification, canWatchFieldAccess,
  canGetBytecodes, canGetSyntheticAttribute, canPopFrames, canGetSourceDebugExtension,
  canUseSourceNameFilters, canGetConstantPool)`. Positional construction and pattern matching
  must add both. (#2059)
- `vivisection.Debug#step(thread: ThreadId, depth: Jdwp.StepDepth)(handler: Debug.Handler): Unit`
  (the logical step) no longer runs `handler` for a landing in a bridge method, a synthetic
  method other than a lambda body (name containing `$anonfun$`), a lazy val's accessor, a
  trivial getter, setter or same-name forwarder, or any class whose name matches `java.*`,
  `javax.*`, `jdk.*`, `sun.*`, `com.sun.*` or `scala.runtime.*`; it re-steps instead
  (`Into` for a step into, `Out` otherwise) until a landing outside those, or the existing
  64-iteration cap. Its requests now also carry `ClassExclude` modifiers for those six
  patterns. Code relying on a logical step stopping inside JDK classes or generated methods
  must use the primitive `Debug#step(thread, depth, size): Int` and `Debug#events`, which are
  unchanged. (#2059)

## xylophone

- `xylophone.core` now depends on `anticipation.focus` (additive: `XPath.focusable`). (#PR-dead-edges)

## ypsiloid

- `ypsiloid.YamlPath(url: Optional[HttpUrl] = Unset, path: Path on YamlPath = YamlPath)` became
  `YamlPath(path: Path on YamlPath = YamlPath)`: the `url` field, which no code path ever set, is
  gone. Pattern matches `YamlPath(url, path)` become `YamlPath(path)`; `path.url` no longer exists.
  `YamlPath is Encodable in Text` always renders `#…`, as it always did in practice. (#PR-dead-edges)
- `ypsiloid.YamlPath.Registry` (a `beneficence.Findable` with `update`, `apply` and `protected def
  lookup`, all keyed by `HttpUrl`) and `YamlPath#apply(using registry: YamlPath.Registry)(document:
  Yaml): Yaml raises YamlPath.Error` (which always returned `document`) removed. No replacement. (#PR-dead-edges)
- `ypsiloid.YamlPath.Error.Reason.UnknownDocument` (`SN-546.1`) removed; `ExpectedHash`,
  `ExpectedSlash` and `BadEscape` keep numbers 2, 3 and 4. (#PR-dead-edges)
- `ypsiloid.core` no longer depends on `urticose.url`; a consumer that reached urticose only
  through ypsiloid must declare it. (#PR-dead-edges)

## zeppelin

- `zeppelin.Zip.Entry` gained nine trailing parameters after `alignment: Int = 1`:
  `flags: Optional[Int] = Unset`, `versionMadeBy: Optional[Int] = Unset`,
  `localVersion: Optional[Int] = Unset`, `centralVersion: Optional[Int] = Unset`,
  `internalAttributes: Optional[Int] = Unset`, `externalAttributes: Optional[Long] = Unset`,
  `localExtra: Optional[Data] = Unset`, `centralExtra: Optional[Data] = Unset` and
  `localSizes: Boolean = true`. Construction by name or with the first six positional
  parameters is unaffected; `Zip.Entry.unapply` now yields twenty fields instead of eleven, so
  a pattern `Zip.Entry(a, b, c, d, e, f, g, h, i, j, k)` no longer matches. (#2045)
- `zeppelin.Zipfile.read` and `zeppelin.Zipfile.parse` (and so `path.open[Zip]()`) now
  populate those nine fields from the archive's headers, and `zeppelin.Zipfile#serialize`
  writes a set field verbatim where it previously derived the value: the general-purpose
  flags were `0x800` for a non-ASCII name and `0` otherwise, both version fields `20` (`45`
  for ZIP64), both attribute fields `0` (external `0x10` for a directory), the extra fields
  empty. An entry read from an archive and written back therefore reproduces the original's
  header values and, when bit 3 of `flags` is set, a data descriptor after its payload, rather
  than being normalised to zeppelin's defaults. Code that relied on that normalisation must
  reset the fields (with `Zip.Entry#withHeaders`, to `Unset` and `localSizes = true`) before
  writing. `Zipfile.read` also now reads every entry's local header eagerly, where it
  previously deferred that read to the entry's content. (#2045)
