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

## stratiform

- `stratiform.Tel.Error.Reason.UnconstrainedScalar` (E224) removed: `Tels.Validation` no longer
  rejects a `scalar` declaring neither `validate` nor `pattern`, so such a schema now validates
  where it previously raised `Tel.Error(Reason.UnconstrainedScalar)`. Code matching on the case
  must drop that branch; the error number 224 stays reserved and is not reused. (#2048)

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
