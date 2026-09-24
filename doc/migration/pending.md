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

## stratiform

- `stratiform.Tel.Error.Reason.UnconstrainedScalar` (E224) removed: `Tels.Validation` no longer
  rejects a `scalar` declaring neither `validate` nor `pattern`, so such a schema now validates
  where it previously raised `Tel.Error(Reason.UnconstrainedScalar)`. Code matching on the case
  must drop that branch; the error number 224 stays reserved and is not reused. (#2048)
