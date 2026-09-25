# Changes since 0.68.0

This file is read by an LLM agent to upgrade code that consumes Soundness libraries from
0.68.0 to the next release. Each entry states precisely what changed; see `AGENTS.md` for the
format. Entries are grouped by module, most-recently-added last within a module.

## corpuscular

- `corpuscular.Crc64.table: Array[Long]^{}` changed shape: it now holds eight slicing tables
  flattened end to end (2048 entries, indexed `k*256 + n` for slice `k` in `0 until 8`),
  previously the single 256-entry bytewise table. The first 256 entries are the same bytewise
  table as before, so code that indexes it by a byte value is unaffected; code that relied on
  `table.length == 256` or iterated the whole table must use `table.readable.take(256)`.
  `Crc64.Accumulator`'s results are unchanged.

## digression

- Behaviour change: `digression.teletypeables.stackTraceTeletype` (and `exceptionTeletype`,
  which delegates to it) now renders each frame's class, separator and method as one contiguous
  word, and its file, colon and line as another (`pkg.Tests.run()  Tests.scala:42`), where
  previously each was a separate table column with a blank cell between them
  (`pkg.Tests . run()  Tests.scala : 42`). Rows remain aligned on the separator and on the
  colon: the class and file are right-padded to the widest in the trace, and the method and
  line are left-aligned. A frame with no line number renders no colon. Code that matched on the
  rendered text (for instance, `Tests.scala : 42`) must expect the contiguous form. (#2052)

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
