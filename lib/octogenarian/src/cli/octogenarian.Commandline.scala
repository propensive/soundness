                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package octogenarian

import java.nio.file as jnf

import scala.caps

import ambience.*
import anticipation.*
import aperture.*
import contingency.*
import distillate.*
import fulminate.*
import galilei.*
import gossamer.*
import guillotine.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

object Commandline:
  // The `git` executable to run, as found on the search path or given explicitly.
  case class Command(path: Text)

  object Event:
    given execEvent: Commandline.Event transcribes guillotine.Exec.Event = Commandline.Event.Exec(_)

    given communicable: Commandline.Event is Communicable =
      case Exec(reason) => m"the git operation did not execute: $reason"

  enum Event:
    case Exec(event: guillotine.Exec.Event) extends Event, Log.Process

  // What a git invocation produced.
  case class Outcome(exit: Exit, stdout: Text, stderr: Text):
    def ok: Boolean = exit == Exit.Ok

    def lines: List[Text] =
      if stdout.length == 0 then Nil else stdout.cut(t"\n").filter(_.length > 0)

  private[octogenarian] val zero: Text = t"0000000000000000000000000000000000000000"
  private[octogenarian] val empty: Text = t""

  // What git said on standard error, read as a reason. git's messages are stable enough to
  // match on, but anything unrecognised is `Unexpected`, with the message kept as the error's
  // detail, so nothing is discarded.
  def reason(stderr0: Text): Git.Error.Reason =
    import Git.Error.Reason as Reason

    // git's messages end with a newline, and some span lines; each pattern is matched against the
    // whole message in dot-all mode.
    val stderr = stderr0.trim

    stderr match

      case r"(?s).*fatal: not a git repository.*" =>
        Reason.NoRepository(t"")

      case r"(?s).*'$ref([^']*)': is at $actual([0-9a-f]+) but expected $expected([0-9a-f]+).*" =>
        Reason.Moved(ref, hashOf(expected), hashOf(actual))

      case r"(?s).*cannot lock ref '$ref([^']*)': reference already exists.*" =>
        Reason.RefExists(ref)

      case r"(?s).*cannot lock ref '$ref([^']*)': unable to resolve reference.*" =>
        Reason.NoSuchRef(ref)

      case r"(?s).*'$ref([^']*)': reference is missing but expected $expected([0-9a-f]+).*" =>
        Reason.Moved(ref, hashOf(expected), Unset)

      case r"(?s).*a branch named '$ref([^']*)' already exists.*" => Reason.RefExists(ref)
      case r"(?s).*tag '$ref([^']*)' already exists.*"            => Reason.RefExists(ref)

      case r"(?s).*[Tt]he branch '$branch([^']*)' is not fully merged.*" =>
        Reason.Unmerged(Git.Branch.unsafe(branch), Git.Hash.zero)

      case r"(?s).*ambiguous argument '$ref([^']*)': unknown revision.*" => Reason.NoSuchRef(ref)
      case r"(?s).*unknown revision or path not in the working tree.*"   => Reason.NoSuchRef(t"")
      case r"(?s).*[Nn]ot a valid object name.*"                         => Reason.NoSuchObject(t"")
      case r"(?s).*could not read object.*"                              => Reason.NoSuchObject(t"")
      case r"(?s).*nothing to commit.*"                                  => Reason.NothingToCommit
      case r"(?s).*[Yy]ou are not currently on a branch.*"               => Reason.Detached
      case r"(?s).*HEAD detached.*"                                      => Reason.Detached

      case r"(?s).*[Yy]our local changes to the following files would be overwritten.*" =>
        Reason.Dirty(indented(stderr))

      case r"(?s).*untracked working tree files would be overwritten.*" =>
        Reason.Dirty(indented(stderr))

      case r"(?s).*[Nn]ot possible to fast-forward.*"                   => Reason.NotFastForward
      case r"(?s).*\(non-fast-forward\).*"                              => Reason.NotFastForward
      case r"(?s).*\(fetch first\).*"                                   => Reason.NotFastForward

      case r"(?s).*\(stale info\).*" =>
        Reason.LeaseFailed(t"", Unset, Unset)

      case r"(?s).*'$path([^']*)' is outside repository.*" =>
        Reason.OutsideWorktree(path)

      case r"(?s).*pathspec '$path([^']*)' did not match any file.*"    => Reason.Untracked(path)

      case r"(?s).*[Nn]o rebase in progress.*" =>
        Reason.NoOperationInProgress

      case r"(?s).*no cherry-pick or revert in progress.*" =>
        Reason.NoOperationInProgress

      case r"(?s).*[Tt]here is no merge to abort.*" =>
        Reason.NoOperationInProgress

      case r"(?s).*[Nn]o merge in progress.*" =>
        Reason.NoOperationInProgress

      case r"(?s).*[Yy]ou have not concluded your merge.*" =>
        Reason.NoOperationInProgress

      case r"(?s).*already a rebase-merge directory.*" =>
        Reason.NoOperationInProgress

      case r"(?s).*cherry-pick is already in progress.*" =>
        Reason.NoOperationInProgress

      case r"(?s).*[Nn]o such remote.*"                                 => Reason.NoSuchRemote(t"")
      case r"(?s).*does not appear to be a git repository.*"            => Reason.NoSuchRemote(t"")
      case r"(?s).*remote $name(\S+) already exists.*"                  => Reason.RemoteExists(name)
      case r"(?s).*[Aa]uthentication failed.*"                          => Reason.Unauthorized(t"")
      case r"(?s).*Permission denied \(publickey.*"                     => Reason.Unauthorized(t"")
      case r"(?s).*could not read Username.*"                           => Reason.Unauthorized(t"")
      case r"(?s).*[Cc]ould not resolve host.*"                         => Reason.Unreachable(t"")
      case r"(?s).*Connection refused.*"                                => Reason.Unreachable(t"")
      case r"(?s).*Connection timed out.*"                              => Reason.Unreachable(t"")
      case r"(?s).*unable to access.*"                                  => Reason.Unreachable(t"")

      case r"(?s).*\[remote rejected\] $ref(\S+) -> $dest(\S+) \($why([^)]*)\).*" =>
        Reason.RemoteRejected(dest, why)

      case r"(?s).*key does not contain a section.*" => Reason.InvalidConfig(t"", t"")
      case r"(?s).*invalid key.*"                    => Reason.InvalidConfig(t"", t"")

      case r"(?s).*Cannot add notes. Found existing notes for object $hash([0-9a-f]{40}).*" =>
        Reason.NoteExists(Git.Hash.unsafe(hash))

      case r"(?s).*gpg: BAD signature.*"          => Reason.BadSignature(stderr)
      case r"(?s).*gpg failed to sign the data.*" => Reason.SigningFailed(stderr)
      case r"(?s).*is not a working tree.*"       => Reason.NoSuchWorktree(t"")
      case r"(?s).*CONFLICT \(.*"                 => Reason.Conflict(Nil)
      case _                                      => Reason.Unexpected

  private def hashOf(text: Text): Optional[Git.Hash] =
    if text == zero then Unset else Git.Hash.unsafe(text)

  private[octogenarian] val tab: Text = t"\t"

  // The tab-indented paths git lists after a "would be overwritten" message.
  private def indented(stderr: Text): List[Text] =
    stderr.cut(t"\n").sweep { case line if line.starts(tab) => line.skip(1) }

  // Reads one of git's progress lines — `Receiving objects:  45% (123/456), …` or
  // `remote: Counting objects: 100% (5/5), done.` — into the activity.
  private def progress(activity: Git.Activity^, line: Text): Unit = line match

    case r"(remote: )?$phase([A-Z][a-z]+ [a-z]+): +$pc([0-9]+)% \($done([0-9]+)/$total([0-9]+)\).*"
    =>
      val remote = line.starts(t"remote: ")
      val doneCount = safely(done.as[Long]).or(0L)
      val totalCount = safely(total.as[Long]).or(0L)

      Git.Activity.Phase.parse(phase) match
        case parsed: Git.Activity.Phase =>
          val report = activity.report().value
          if report.phase != parsed || report.remote != remote then activity.begin(parsed, remote)
          activity.progress(doneCount, totalCount)

        case _ => ()

    case _ =>
      ()

  private def collect[element, result](list: List[element])(lambda: element => Optional[result])
  :   List[result] =

    def recur(remaining: List[element], acc: List[result]): List[result] = remaining match
      case head :: tail =>
        lambda(head) match
          case result: result @unchecked if result != Unset => recur(tail, result :: acc)
          case _                                            => recur(tail, acc)

      case _ => acc.reverse

    recur(list, Nil)

// The backend that drives the `git` command. Each operation is one or a few invocations of
// git's plumbing; porcelain verbs run the porcelain command and read its outcome. Every child
// runs under `LC_ALL=C`, so that its messages are the English ones the reason table knows, and
// with `GIT_TERMINAL_PROMPT=0`, so that a credential prompt fails rather than hangs.
class Commandline(command: Commandline.Command)
  ( using working:     WorkingDirectory,
          environment: Environment,
          loggable:    (Commandline.Event is Loggable)^ )

extends Git.Backend:
  import Git.Error.Reason as Reason
  import Commandline.{Outcome, collect, tab, empty}

  private val prelude: List[Text] =
    List(t"env", t"LC_ALL=C", t"GIT_TERMINAL_PROMPT=0", t"GIT_EDITOR=true", command.path)

  private def gitPath(gitDir: Path on Linux): Text = t"--git-dir=${gitDir.encode}"
  private def treePath(worktree: Path on Linux): Text = t"--work-tree=${worktree.encode}"

  private def dirArgs(gitDir: Path on Linux, worktree: Optional[Path on Linux] = Unset)
  :   List[Text] =

    worktree.lay(List(gitPath(gitDir))): worktree => List(gitPath(gitDir), treePath(worktree))

  // Runs git, reading its standard output and error to the end, and its exit status. The output
  // is read before the error: git's commands write at most a few lines to standard error when
  // not reporting progress, so the pipe never fills.
  private def run(arguments: List[Text], input: Optional[Data] = Unset)(using Tactic[Git.Error])
  :   Outcome =

    val executable = Command((prelude + arguments)*)

    try throwErrors[Exec.Error]:
      val job = executable.fork[Exit]()
      input.lay(()): data => safely(job.stdin[Data](Stream(Chain(data))))
      val stdout = job.text()
      val stderr = job.errorText()
      Outcome(job.exitStatus(), stdout, stderr)

    catch case error: Exec.Error =>
      abort(Git.Error(Reason.CannotExecuteGit, Objects.text(error.stderr)))

  // Runs git, reading its standard output as bytes.
  private def runData(arguments: List[Text], input: Optional[Data] = Unset)(using Tactic[Git.Error])
  :   (Exit, Data, Text) =

    val executable = Command((prelude + arguments)*)

    try throwErrors[Exec.Error]:
      val job = executable.fork[Exit]()
      input.lay(()): data => safely(job.stdin[Data](Stream(Chain(data))))
      val stdout: Data = unsafely(job.stdout().memoize)
      val stderr = job.errorText()
      (job.exitStatus(), stdout, stderr)

    catch case error: Exec.Error =>
      abort(Git.Error(Reason.CannotExecuteGit, Objects.text(error.stderr)))

  // Runs a transfer, feeding its progress lines (which git writes to standard error, separated
  // by carriage returns) into the activity as they arrive, and keeping the rest as the error text.
  private def runProgress(arguments: List[Text], activity: Git.Activity^)(using Tactic[Git.Error])
  :   Outcome =

    import hieroglyph.charsets.utf8Charset, hieroglyph.textSanitizers.substituteSanitizer
    import turbulence.lineSeparation.adaptiveLinefeedLineSeparation

    val executable = Command((prelude + arguments)*)

    try throwErrors[Exec.Error]:
      val job = executable.fork[Exit]()
      val messages = scala.collection.mutable.ListBuffer[Text]()

      def consider(line: Text): Unit =
        Commandline.progress(activity, line)
        if !line.contains(t"%") && line.length > 0 then messages += line

      safely[Truncation.Error](job.stderr().delineate.records.each(consider(_)))
      val stdout = job.text()
      val exit = job.exitStatus()
      activity.finish()
      Outcome(exit, stdout, messages.to(List).join(t"\n"))

    catch case error: Exec.Error =>
      abort(Git.Error(Reason.CannotExecuteGit, Objects.text(error.stderr)))

  private def expect(outcome: Outcome)(using Tactic[Git.Error]): Outcome =
    if outcome.ok then outcome
    else abort(Git.Error(Commandline.reason(outcome.stderr), outcome.stderr.trim))

  private def expect(outcome: Outcome, reason: Reason)(using Tactic[Git.Error]): Outcome =
    if outcome.ok then outcome else abort(Git.Error(reason, outcome.stderr.trim))

  private def pathOf(text: Text)(using Tactic[Git.Error]): Path on Linux =
    safely(text.trim.as[Path on Linux]).or(abort(Git.Error(Reason.NoRepository(text))))

  private def hashes(outcome: Outcome): List[Git.Hash] =
    outcome.lines.sweep { case line if line.length == 40 => Git.Hash.unsafe(line) }

  private def exists(path: jnf.Path): Boolean = jnf.Files.exists(path)

  private def nio(path: Path on Linux, rest: Text*): jnf.Path =
    rest.foldLeft(jnf.Path.of(path.encode.s).nn): (path, name) => path.resolve(name.s).nn

  private def readFile(path: jnf.Path): Optional[Text] =
    if exists(path) then String(jnf.Files.readAllBytes(path).nn, "UTF-8").nn.tt.trim else Unset

  // ─────────────────────────────────────────── Repository ─────────────────────────────────────

  def locate(path: Path on Linux)(using Tactic[Git.Error]): Optional[Git.Backend.Location] =
    val arguments =
      List
        ( t"-C",
          path.encode,
          t"rev-parse",
          t"--path-format=absolute",
          t"--git-dir",
          t"--git-common-dir",
          t"--is-bare-repository",
          t"--is-inside-work-tree" )

    val outcome = run(arguments)

    if !outcome.ok then Unset else outcome.lines match

      case List(gitDir, commonDir, bare, inside) =>
        val gitDirPath = pathOf(gitDir)
        val commonDirPath = pathOf(commonDir)

        val worktree: Optional[Path on Linux] =
          if bare == t"true" then Unset
          else if inside == t"true" then
            val top = run(List(t"-C", path.encode, t"rev-parse", t"--show-toplevel"))
            if top.ok then pathOf(top.stdout) else Unset
          else
            // Inside the git directory itself: a `.git` directory's parent is its working tree; a
            // linked worktree's git directory records where its tree is.
            readFile(nio(gitDirPath, t"gitdir")) match
              case text: Text => parentOf(pathOf(text))
              case _          => if gitDirPath.name == t".git" then parentOf(gitDirPath) else Unset

        Git.Backend.Location(gitDirPath, commonDirPath, worktree)

      case _ => Unset

  private def parentOf(path: Path on Linux): Optional[Path on Linux] = path.parent

  def init(target: Path on Linux, initialBranch: Optional[Git.Branch], bare: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    val bareArg = if bare then List(t"--bare") else Nil
    val branchArg = initialBranch.lay(Nil): branch => List(t"--initial-branch=${branch.text}")
    expect(run(List(t"init", t"-q") + bareArg + branchArg + List(target.encode)))

  def clone
    ( source: Text, target: Path on Linux, branch: Optional[Git.Branch], depth: Optional[Int],
      bare: Boolean, recursive: Boolean, activity: Git.Activity^ )
    ( using Internet, Tactic[Git.Error] )
  :   Unit =

    val arguments =
      List(t"clone", t"--progress") +
        branch.lay(Nil) { branch => List(t"--branch=${branch.text}") } +
        depth.lay(Nil) { depth => List(t"--depth=$depth") } +
        (if bare then List(t"--bare") else Nil) +
        (if recursive then List(t"--recursive") else Nil) +
        List(source, target.encode)

    expect(runProgress(arguments, activity))

  // ────────────────────────────────────────────── Refs ────────────────────────────────────────

  def head(gitDir: Path on Linux)(using Tactic[Git.Error]): Git.Head =
    val symbolic = run(dirArgs(gitDir) + List(t"symbolic-ref", t"-q", t"HEAD"))

    if symbolic.ok then
      val name = symbolic.stdout.trim
      val branch = branchOf(name)
      val tip = run(dirArgs(gitDir) + List(t"rev-parse", t"-q", t"--verify", t"HEAD"))
      Git.Head.Attached(branch, if tip.ok then Git.Hash.unsafe(tip.stdout.trim) else Unset)
    else
      val outcome = run(dirArgs(gitDir) + List(t"rev-parse", t"-q", t"--verify", t"HEAD"))
      val tip = expect(outcome, Reason.NoSuchRef(t"HEAD"))
      Git.Head.Detached(Git.Hash.unsafe(tip.stdout.trim))

  def readRef(gitDir: Path on Linux, ref: Text)
    ( using Tactic[Git.Error] )
  :   Optional[Git.Backend.Target] =

    val symbolic = run(dirArgs(gitDir) + List(t"symbolic-ref", t"-q", ref))

    if symbolic.ok then Git.Backend.Target.Symbolic(symbolic.stdout.trim) else
      val direct = run(dirArgs(gitDir) + List(t"rev-parse", t"-q", t"--verify", ref))

      if direct.ok && direct.stdout.trim.length == 40
      then Git.Backend.Target.Direct(Git.Hash.unsafe(direct.stdout.trim))
      else Unset

  def listRefs(gitDir: Path on Linux, prefix: Optional[Text])
    ( using Tactic[Git.Error] )
  :   List[(Text, Git.Hash)] =

    val format = t"--format=%(refname) %(objectname)"
    val arguments = List(t"for-each-ref", format) + prefix.let(List(_)).or(Nil)
    val outcome = expect(run(dirArgs(gitDir) + arguments))
    collect(outcome.lines)(refLine(_))

  private def refLine(line: Text): Optional[(Text, Git.Hash)] = line.cut(t" ") match
    case List(name, hash) if hash.length == 40 => (name, Git.Hash.unsafe(hash))
    case _                                     => Unset

  def peel(gitDir: Path on Linux, hash: Git.Hash)(using Tactic[Git.Error]): Git.Hash =
    val outcome = run(dirArgs(gitDir) + List(t"rev-parse", t"-q", t"--verify", t"${hash.text}^{}"))
    if outcome.ok then Git.Hash.unsafe(outcome.stdout.trim) else hash

  def updateRef
    ( gitDir: Path on Linux, ref: Text, to: Optional[Git.Hash], expected: Git.Backend.Expectation,
      message: Optional[Text] )
    ( using Tactic[Git.Error] )
  :   Unit =

    val messageArgs = message.lay(Nil): message => List(t"-m", message)

    val old: List[Text] = expected match
      case Git.Backend.Expectation.Unconditional => Nil
      case Git.Backend.Expectation.Absent        => List(Commandline.zero)
      case Git.Backend.Expectation.Exactly(hash) => List(hash.text)

    val arguments = to.lay(List(t"update-ref") + messageArgs + List(t"-d", ref) + old): to =>
      List(t"update-ref") + messageArgs + List(ref, to.text) + old

    val outcome = run(dirArgs(gitDir) + arguments)

    if !outcome.ok then
      val reason = Commandline.reason(outcome.stderr) match

        case Reason.Unexpected | Reason.NoSuchRef(_)
        if expected != Git.Backend.Expectation.Unconditional =>
          // The ref was expected to hold a value and does not: it moved, to nothing.
          Reason.Moved(ref, expected.expected, Unset)

        case Reason.RefExists(_) if expected == Git.Backend.Expectation.Absent =>
          // The ref was expected not to exist and does: it moved, from nothing.
          Reason.Moved(ref, Unset, resolve(gitDir, ref))

        case other => other

      abort(Git.Error(reason, outcome.stderr.trim))

  def updateSymbolicRef(gitDir: Path on Linux, name: Text, target: Text)
    ( using Tactic[Git.Error] )
  :   Unit =

    expect(run(dirArgs(gitDir) + List(t"symbolic-ref", name, target)))

  def resolve(gitDir: Path on Linux, revision: Text)(using Tactic[Git.Error]): Optional[Git.Hash] =
    val arguments = List(t"rev-parse", t"-q", t"--verify", t"--end-of-options", revision)
    val outcome = run(dirArgs(gitDir) + arguments)

    if outcome.ok && outcome.stdout.trim.length == 40 then Git.Hash.unsafe(outcome.stdout.trim)
    else Unset

  def revList
    ( gitDir: Path on Linux,
      from:   Text,
      until:  Optional[Text],
      limit:  Optional[Int],
      paths:  List[Text] )
    ( using Tactic[Git.Error] )
  :   List[Git.Hash] =

    val limitArgs = limit.lay(Nil): limit => List(t"--max-count=$limit")
    val untilArgs = until.lay(Nil): until => List(t"^$until")
    val pathArgs = if paths == Nil then Nil else t"--" :: paths

    val arguments = List(t"rev-list") + limitArgs + List(from) + untilArgs + pathArgs
    val outcome = run(dirArgs(gitDir) + arguments)

    if outcome.ok then hashes(outcome)
    else abort(Git.Error(Reason.NoSuchRef(from), outcome.stderr.trim))

  // A repository with no commits has no reflog, which git reports as an error.
  def reflog(gitDir: Path on Linux, ref: Optional[Text])
    ( using Tactic[Git.Error] )
  :   List[ReflogEntry] =

    val arguments = List(t"reflog", t"show", t"--format=%H %gd %ct %gs") + ref.let(List(_)).or(Nil)
    val outcome = run(dirArgs(gitDir) + arguments)
    if outcome.ok then collect(outcome.lines)(reflogEntry(_)) else Nil

  private def reflogEntry(line: Text): Optional[ReflogEntry] = line match

    case r"$hash([a-f0-9]{40}) $selector(\S+) $time([0-9]+) $message(.*)" =>
      ReflogEntry(Git.Hash.unsafe(hash), selector, safely(time.as[Long]).or(0L), message)

    case _ =>
      Unset

  def mergeBase(gitDir: Path on Linux, left: Git.Hash, right: Git.Hash)
    ( using Tactic[Git.Error] )
  :   Optional[Git.Hash] =

    val outcome = run(dirArgs(gitDir) + List(t"merge-base", left.text, right.text))
    if outcome.ok then Git.Hash.unsafe(outcome.stdout.trim) else Unset

  def isAncestor(gitDir: Path on Linux, ancestor: Git.Hash, descendant: Git.Hash)
    ( using Tactic[Git.Error] )
  :   Boolean =

    run(dirArgs(gitDir) + List(t"merge-base", t"--is-ancestor", ancestor.text, descendant.text)).ok

  // ──────────────────────────────────────────── Objects ───────────────────────────────────────

  def readObject(gitDir: Path on Linux, hash: Git.Hash)
    ( using Tactic[Git.Error] )
  :   Optional[(Git.Object.Kind, Data)] =

    readObjects(gitDir, List(hash)).prim.let: (_, kind, data) => (kind, data)

  // `cat-file --batch` answers each hash with `<hash> <type> <size>\n<content>\n`, or
  // `<hash> missing\n`.
  def readObjects(gitDir: Path on Linux, hashes: List[Git.Hash])(using Tactic[Git.Error])
  :   List[(Git.Hash, Git.Object.Kind, Data)] =

    if hashes == Nil then Nil else
      val input = Objects.bytes(hashes.map(_.text).join(t"", t"\n", t"\n"))
      val (exit, stdout, stderr) = runData(dirArgs(gitDir) + List(t"cat-file", t"--batch"), input)
      if exit != Exit.Ok then abort(Git.Error(Commandline.reason(stderr), stderr.trim))

      val array = stdout.asInstanceOf[scala.Array[Byte]]
      val length = array.length

      def lineEnd(from: Int): Int =
        if from >= length || array.apply(from) == '\n'.toByte then from else lineEnd(from + 1)

      def recur(index: Int, acc: List[(Git.Hash, Git.Object.Kind, Data)])
      :   List[(Git.Hash, Git.Object.Kind, Data)] =

        if index >= length then acc.reverse else
          val end = lineEnd(index)
          val header = String(array, index, end - index, "UTF-8").nn.tt

          header.cut(t" ") match

            case List(hash, kindText, sizeText) =>
              val size = safely(sizeText.as[Int]).or(0)
              val start = end + 1
              val content = java.util.Arrays.copyOfRange(array, start, start + size).nn
              val data = content.asInstanceOf[Data]

              val next = Git.Object.Kind.parse(kindText).lay(acc): kind =>
                (Git.Hash.unsafe(hash), kind, data) :: acc

              recur(start + size + 1, next)

            case _ =>
              recur(end + 1, acc)

      recur(0, Nil)

  def writeObject(gitDir: Path on Linux, kind: Git.Object.Kind, data: Data)
    ( using Tactic[Git.Error] )
  :   Git.Hash =

    val arguments = List(t"hash-object", t"-w", t"-t", kind.text, t"--stdin")
    val outcome = expect(run(dirArgs(gitDir) + arguments, data))

    Git.Hash.unsafe(outcome.stdout.trim)

  def diffTree(gitDir: Path on Linux, left: Git.Hash, right: Git.Hash)
    ( using Tactic[Git.Error] )
  :   List[FileDiff] =

    val outcome = expect(run(dirArgs(gitDir) + List(t"diff", t"--no-color", left.text, right.text)))
    Patch.parse(outcome.lines)

  // ─────────────────────────────────────── Index and working files ────────────────────────────

  def status(gitDir: Path on Linux, worktree: Path on Linux, ignored: Boolean, untracked: Boolean)
    ( using Tactic[Git.Error] )
  :   List[Git.Backend.StatusLine] =

    val arguments =
      List(t"status", t"--porcelain=v1") +
        (if ignored then List(t"--ignored") else Nil) +
        (if untracked then List(t"--untracked-files=all") else List(t"--untracked-files=no"))

    val outcome = expect(run(dirArgs(gitDir, worktree) + arguments))
    collect(outcome.stdout.cut(t"\n").filter(_.length > 0))(statusLine(_))

  private def unquote(text: Text): Text =
    if !text.starts(t"\"") then text else
      val inner = text.skip(1).skip(1, Rtl).s
      val builder = StringBuilder()

      def recur(index: Int): Unit = if index < inner.length then
        inner.charAt(index) match

          case '\\' if index + 1 < inner.length =>
            inner.charAt(index + 1) match
              case 'n'   => builder.append('\n')
              case 't'   => builder.append('\t')
              case other => builder.append(other)

            recur(index + 2)

          case other =>
            builder.append(other)
            recur(index + 1)

      recur(0)
      builder.toString.tt

  private def statusLine(line: Text): Optional[Git.Backend.StatusLine] =
    if line.length < 4 then Unset else
      val index = Git.Status.parse(line.s.charAt(0))
      val worktree = Git.Status.parse(line.s.charAt(1))
      val rest = line.skip(3)
      val arrow = rest.s.indexOf(" -> ")

      if arrow < 0 then Git.Backend.StatusLine(index, worktree, unquote(rest), Unset)
      else Git.Backend.StatusLine
        ( index, worktree, unquote(rest.skip(arrow + 4)), unquote(rest.keep(arrow)) )

  def add(gitDir: Path on Linux, worktree: Path on Linux, paths: List[Text], all: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    val arguments = if all then List(t"add", t"-A") else List(t"add", t"--") + paths
    expect(run(dirArgs(gitDir, worktree) + arguments))

  def remove
    ( gitDir:   Path on Linux,
      worktree: Path on Linux,
      paths:    List[Text],
      cached:   Boolean,
      force:    Boolean )
    ( using Tactic[Git.Error] )
  :   Unit =

    val arguments =
      List(t"rm", t"-r", t"-q") +
        (if cached then List(t"--cached") else Nil) +
        (if force then List(t"-f") else Nil) +
        (t"--" :: paths)

    expect(run(dirArgs(gitDir, worktree) + arguments))

  def move(gitDir: Path on Linux, worktree: Path on Linux, from: Text, to: Text)
    ( using Tactic[Git.Error] )
  :   Unit =

    expect(run(dirArgs(gitDir, worktree) + List(t"mv", from, to)))

  def restore
    ( gitDir: Path on Linux, worktree: Path on Linux, paths: List[Text], source: Optional[Text],
      staged: Boolean, workingFiles: Boolean )
    ( using Tactic[Git.Error] )
  :   Unit =

    val arguments =
      List(t"restore") +
        (if staged then List(t"--staged") else Nil) +
        (if workingFiles then List(t"--worktree") else Nil) +
        source.lay(Nil) { source => List(t"--source=$source") } +
        (t"--" :: (if paths == Nil then List(t".") else paths))

    expect(run(dirArgs(gitDir, worktree) + arguments))

  def clean
    ( gitDir:      Path on Linux,
      worktree:    Path on Linux,
      paths:       List[Text],
      directories: Boolean,
      ignored:     Boolean )
    ( using Tactic[Git.Error] )
  :   List[Text] =

    val options =
      (if directories then List(t"-d") else Nil) + (if ignored then List(t"-x") else Nil)

    val pathArgs = if paths == Nil then Nil else t"--" :: paths

    val previewArgs = List(t"clean", t"-n") + options + pathArgs
    val preview = expect(run(dirArgs(gitDir, worktree) + previewArgs))

    val wouldRemove = t"Would remove "
    val removed = preview.lines.sweep { case line if line.starts(wouldRemove) => line.skip(13) }
    expect(run(dirArgs(gitDir, worktree) + List(t"clean", t"-f", t"-q") + options + pathArgs))
    removed.map(unslashed(_))

  private def unslashed(path: Text): Text = if path.ends(t"/") then path.skip(1, Rtl) else path

  def writeTree(gitDir: Path on Linux, worktree: Path on Linux)(using Tactic[Git.Error]): Git.Hash =
    Git.Hash.unsafe(expect(run(dirArgs(gitDir, worktree) + List(t"write-tree"))).stdout.trim)

  def readTree(gitDir: Path on Linux, worktree: Path on Linux, mode: ResetMode)
    ( using Tactic[Git.Error] )
  :   Unit =

    mode match
      case ResetMode.Soft => ()

      case other =>
        expect(run(dirArgs(gitDir, worktree) + List(t"reset", t"-q", other.flag, t"HEAD")))

  def diffWorktree
    ( gitDir:   Path on Linux,
      worktree: Path on Linux,
      staged:   Boolean,
      against:  Optional[Text] )
    ( using Tactic[Git.Error] )
  :   List[FileDiff] =

    val stagedArgs = if staged then List(t"--staged") else Nil
    val arguments = List(t"diff", t"--no-color") + stagedArgs + against.let(List(_)).or(Nil)

    Patch.parse(expect(run(dirArgs(gitDir, worktree) + arguments)).lines)

  def tracked(gitDir: Path on Linux, worktree: Path on Linux)(using Tactic[Git.Error]): List[Text] =
    expect(run(dirArgs(gitDir, worktree) + List(t"ls-files"))).lines

  def conflicts(gitDir: Path on Linux, worktree: Path on Linux)
    ( using Tactic[Git.Error] )
  :   List[Text] =

    run(dirArgs(gitDir, worktree) + List(t"diff", t"--name-only", t"--diff-filter=U")).lines

  // ─────────────────────────────────────────── Porcelain ──────────────────────────────────────

  def checkout(gitDir: Path on Linux, worktree: Path on Linux, revision: Text)
    ( using Tactic[Git.Error] )
  :   Unit =

    expect(run(dirArgs(gitDir, worktree) + List(t"checkout", t"-q", revision, t"--")))

  def switch(gitDir: Path on Linux, worktree: Path on Linux, branch: Git.Branch, create: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    val createArgs = if create then List(t"-c") else Nil
    val arguments = List(t"switch", t"-q") + createArgs + List(branch.text)

    expect(run(dirArgs(gitDir, worktree) + arguments))

  private def currentHead(gitDir: Path on Linux)(using Tactic[Git.Error]): Git.Hash =
    head(gitDir).commit.or(abort(Git.Error(Reason.NoSuchRef(t"HEAD"))))

  // A porcelain verb which may stop on conflicts: a failure with conflicts in the index is the
  // `Stopped` outcome; any other failure is an error.
  private def sequenced
    ( gitDir:   Path on Linux,
      worktree: Path on Linux,
      outcome:  Outcome )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    if outcome.ok then Git.Sequence.Outcome.Completed(currentHead(gitDir))
    else operation(gitDir, worktree) match
      case operation: Git.Operation => Git.Sequence.Outcome.Stopped(operation)

      case _ =>
        abort(Git.Error(Commandline.reason(outcome.stderr), outcome.stderr.trim))

  def merge
    ( gitDir: Path on Linux, worktree: Path on Linux, revision: Text, ff: FastForward,
      message: Optional[Text], sign: Boolean )
    ( using Tactic[Git.Error] )
  :   Git.Merge.Outcome =

    val ffArgs = ff match
      case FastForward.Auto  => Nil
      case FastForward.Only  => List(t"--ff-only")
      case FastForward.Never => List(t"--no-ff")

    val arguments =
      List(t"merge", t"--no-edit") +
        ffArgs +
        message.lay(Nil) { message => List(t"-m", message) } +
        (if sign then List(t"-S") else Nil) +
        List(revision)

    val before = head(gitDir).commit
    val outcome = run(dirArgs(gitDir, worktree) + arguments)

    if outcome.ok then
      if outcome.stdout.contains(t"Already up to date") then Git.Merge.Outcome.UpToDate
      else if outcome.stdout.contains(t"Fast-forward")
      then Git.Merge.Outcome.FastForwarded(currentHead(gitDir))
      else Git.Merge.Outcome.Merged(currentHead(gitDir))
    else
      operation(gitDir, worktree) match
      case operation: Git.Operation => Git.Merge.Outcome.Stopped(operation)

      case _ =>
        abort(Git.Error(Commandline.reason(outcome.stderr), outcome.stderr.trim))

  def cherryPick
    ( gitDir:   Path on Linux,
      worktree: Path on Linux,
      commits:  List[Git.Hash],
      noCommit: Boolean )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    val noCommitArgs = if noCommit then List(t"-n") else Nil
    val arguments = List(t"cherry-pick") + noCommitArgs + commits.map(_.text)

    sequenced(gitDir, worktree, run(dirArgs(gitDir, worktree) + arguments))

  def revert
    ( gitDir:   Path on Linux,
      worktree: Path on Linux,
      commits:  List[Git.Hash],
      noCommit: Boolean )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    val noCommitArgs = if noCommit then List(t"-n") else Nil
    val arguments = List(t"revert", t"--no-edit") + noCommitArgs + commits.map(_.text)
    sequenced(gitDir, worktree, run(dirArgs(gitDir, worktree) + arguments))

  def rebase
    ( gitDir: Path on Linux, worktree: Path on Linux, onto: Text, upstream: Optional[Text],
      options: List[Git.Rebase.Option] )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    val configArgs =
      if options.has(Git.Rebase.Option.NoRenames) then List(t"-c", t"merge.renames=false") else Nil

    val optionArgs = options.bind(rebaseOption(_))

    val targetArgs = upstream.lay(List(onto)): upstream => List(t"--onto", onto, upstream)
    val arguments = configArgs + List(t"rebase") + optionArgs + targetArgs
    sequenced(gitDir, worktree, run(dirArgs(gitDir, worktree) + arguments))

  private def rebaseOption(option: Git.Rebase.Option): List[Text] = option match
    case Git.Rebase.Option.Autosquash => List(t"--autosquash")
    case Git.Rebase.Option.KeepEmpty  => List(t"--keep-empty")
    case Git.Rebase.Option.Signoff    => List(t"--signoff")
    case Git.Rebase.Option.Autostash  => List(t"--autostash")
    case Git.Rebase.Option.NoRenames  => Nil

  def sequence
    ( gitDir:   Path on Linux,
      worktree: Path on Linux,
      verb:     Git.Sequence.Verb )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    val flag = verb match
      case Git.Sequence.Verb.Continue => t"--continue"
      case Git.Sequence.Verb.Skip     => t"--skip"
      case Git.Sequence.Verb.Abort    => t"--abort"
      case Git.Sequence.Verb.Quit     => t"--quit"

    def present(name: Text): Boolean = exists(nio(gitDir, name))

    val command: List[Text] =
      if present(t"rebase-merge") || present(t"rebase-apply") then
        List(t"rebase", flag)
      else if present(t"CHERRY_PICK_HEAD") || present(t"sequencer") then
        List(t"cherry-pick", flag)
      else if present(t"REVERT_HEAD") then
        List(t"revert", flag)
      else if present(t"MERGE_HEAD") then
        verb match
          case Git.Sequence.Verb.Continue => List(t"commit", t"--no-edit")
          case Git.Sequence.Verb.Skip     => abort(Git.Error(Reason.Unsupported(t"skipping")))
          case _                          => List(t"merge", t"--abort")
      else
        abort(Git.Error(Reason.NoOperationInProgress))

    sequenced(gitDir, worktree, run(dirArgs(gitDir, worktree) + command))

  def operation(gitDir: Path on Linux, worktree: Optional[Path on Linux])
    ( using Tactic[Git.Error] )
  :   Optional[Git.Operation] =

    val conflicting: List[Text] = worktree.lay(Nil)(conflicts(gitDir, _))
    def hash(name: Text): Optional[Git.Hash] = readFile(nio(gitDir, name)).let(Git.Hash.unsafe(_))

    def number(path: jnf.Path): Int = readFile(path) match
      case text: Text => safely(text.as[Int]).or(0)
      case _          => 0

    if exists(nio(gitDir, t"rebase-merge")) then
      val dir = nio(gitDir, t"rebase-merge")

      Git.Operation.Rebasing
        ( readFile(dir.resolve("onto").nn).let(Git.Hash.unsafe(_)).or(Git.Hash.zero),
          readFile(dir.resolve("orig-head").nn).let(Git.Hash.unsafe(_)).or(Git.Hash.zero),
          number(dir.resolve("msgnum").nn),
          number(dir.resolve("end").nn) - number(dir.resolve("msgnum").nn),
          conflicting )
    else if exists(nio(gitDir, t"rebase-apply")) then
      val dir = nio(gitDir, t"rebase-apply")

      Git.Operation.Rebasing
        ( readFile(dir.resolve("onto").nn).let(Git.Hash.unsafe(_)).or(Git.Hash.zero),
          readFile(dir.resolve("orig-head").nn).let(Git.Hash.unsafe(_)).or(Git.Hash.zero),
          number(dir.resolve("next").nn),
          number(dir.resolve("last").nn) - number(dir.resolve("next").nn),
          conflicting )
    else
      hash(t"CHERRY_PICK_HEAD").let(Git.Operation.CherryPicking(_, conflicting))
      . or(hash(t"REVERT_HEAD").let(Git.Operation.Reverting(_, conflicting)))
      . or(readFile(nio(gitDir, t"MERGE_HEAD")).let(merging(_, conflicting)))
      . or(if exists(nio(gitDir, t"BISECT_LOG")) then Git.Operation.Bisecting else Unset)

  private def merging(text: Text, conflicting: List[Text]): Git.Operation =
    Git.Operation.Merging
      ( text.cut(t"\n").filter(_.length == 40).map(Git.Hash.unsafe(_)), conflicting )

  def stash
    ( gitDir:           Path on Linux,
      worktree:         Path on Linux,
      message:          Optional[Text],
      includeUntracked: Boolean,
      keepIndex:        Boolean )
    ( using Tactic[Git.Error] )
  :   Optional[Git.Hash] =

    val arguments =
      List(t"stash", t"push", t"-q") +
        (if includeUntracked then List(t"-u") else Nil) +
        (if keepIndex then List(t"--keep-index") else Nil) +
        message.lay(Nil): message => List(t"-m", message)

    val before = resolve(gitDir, t"refs/stash")
    expect(run(dirArgs(gitDir, worktree) + arguments))
    val after = resolve(gitDir, t"refs/stash")
    if after == before then Unset else after

  def stashes(gitDir: Path on Linux)(using Tactic[Git.Error]): List[Git.Stash.Entry] =
    val outcome = run(dirArgs(gitDir) + List(t"reflog", t"show", t"--format=%H %gs", t"refs/stash"))

    if !outcome.ok then Nil else
      def recur
        ( lines: List[Text],
          index: Int,
          acc:   List[Git.Stash.Entry] )
      :   List[Git.Stash.Entry] =

        lines match
          case line :: rest =>
            val space = line.s.indexOf(' ')
            val hash = Git.Hash.unsafe(line.keep(space))
            val entry = Git.Stash.Entry(index, hash, line.skip(space + 1))
            recur(rest, index + 1, entry :: acc)

          case _ => acc.reverse

      recur(outcome.lines, 0, Nil)

  def applyStash
    ( gitDir:   Path on Linux,
      worktree: Path on Linux,
      entry:    Int,
      index:    Boolean,
      drop:     Boolean )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    val verb = if drop then t"pop" else t"apply"

    val indexArgs = if index then List(t"--index") else Nil
    val arguments = List(t"stash", verb, t"-q") + indexArgs + List(t"stash@{$entry}")

    sequenced(gitDir, worktree, run(dirArgs(gitDir, worktree) + arguments))

  def dropStash(gitDir: Path on Linux, entry: Int)(using Tactic[Git.Error]): Unit =
    expect
      ( run(dirArgs(gitDir) + List(t"stash", t"drop", t"-q", t"stash@{$entry}")),
        Reason.NoSuchStash )

  // ───────────────────────────────────────────── Notes ────────────────────────────────────────

  def readNote(gitDir: Path on Linux, ref: Text, target: Git.Hash)
    ( using Tactic[Git.Error] )
  :   Optional[Text] =

    val outcome = run(dirArgs(gitDir) + List(t"notes", t"--ref=$ref", t"show", target.text))

    if !outcome.ok then Unset
    else if outcome.stdout.ends(t"\n") then outcome.stdout.skip(1, Rtl)
    else outcome.stdout

  def listNotes(gitDir: Path on Linux, ref: Text)
    ( using Tactic[Git.Error] )
  :   List[(Git.Hash, Git.Hash)] =

    val outcome = run(dirArgs(gitDir) + List(t"notes", t"--ref=$ref", t"list"))

    if !outcome.ok then Nil else collect(outcome.lines)(noteLine(_))

  private def noteLine(line: Text): Optional[(Git.Hash, Git.Hash)] = line.cut(t" ") match

    case List(note, target) if note.length == 40 && target.length == 40 =>
      (Git.Hash.unsafe(note), Git.Hash.unsafe(target))

    case _ => Unset

  // The edit is made on a temporary ref seeded with `base`, by git's own notes porcelain; the
  // new notes commit is read back and the temporary ref deleted, leaving the real ref to the
  // handle's compare-and-swap.
  def editNotes(gitDir: Path on Linux, base: Optional[Git.Hash], edit: Git.Backend.NoteEdit)
    ( using Tactic[Git.Error] )
  :   Git.Hash =

    val scratch = t"refs/notes/octogenarian-${java.util.UUID.randomUUID.nn.toString.tt}"
    base.lay(()): base => expect(run(dirArgs(gitDir) + List(t"update-ref", scratch, base.text)))

    val arguments = edit match

      case Git.Backend.NoteEdit.Add(target, body, force) =>
        List(t"add") + (if force then List(t"-f") else Nil) + List(t"-m", body, target.text)

      case Git.Backend.NoteEdit.Append(target, body) =>
        List(t"append", t"-m", body, target.text)

      case Git.Backend.NoteEdit.Remove(target, ignoreMissing) =>
        val missingArgs = if ignoreMissing then List(t"--ignore-missing") else Nil
        List(t"remove") + missingArgs + List(target.text)

      case Git.Backend.NoteEdit.Copy(source, target, force) =>
        List(t"copy") + (if force then List(t"-f") else Nil) + List(source.text, target.text)

    try
      expect(run(dirArgs(gitDir) + List(t"notes", t"--ref=$scratch") + arguments))
      resolve(gitDir, scratch).or(abort(Git.Error(Reason.NoSuchRef(scratch))))
    finally run(dirArgs(gitDir) + List(t"update-ref", t"-d", scratch))

  // ───────────────────────────────────────────── Config ───────────────────────────────────────

  private def configArgs(source: Git.Config.Source): List[Text] = source match
    case Git.Config.Source.Effective(gitDir) => List(gitPath(gitDir), t"config")
    case Git.Config.Source.File(path)        => List(t"config", t"--file", path.encode)

    case Git.Config.Source.Repository(gitDir, scope) =>
      val scopeFlag = scope match
        case Git.Config.Scope.Local    => t"--local"
        case Git.Config.Scope.Worktree => t"--worktree"
        case Git.Config.Scope.Global   => t"--global"
        case Git.Config.Scope.System   => t"--system"

      List(gitPath(gitDir), t"config", scopeFlag)

  def configRead(source: Git.Config.Source)(using Tactic[Git.Error]): List[(Git.Config.Key, Text)] =
    val outcome = run(configArgs(source) + List(t"--list", t"--null"))
    // `--null` ends each entry with NUL and separates key from value with a newline.
    if !outcome.ok then Nil
    else collect(outcome.stdout.cut(t"\u0000").filter(_.length > 0))(configEntry(_))

  private def configEntry(entry: Text): Optional[(Git.Config.Key, Text)] =
    val newline = entry.s.indexOf('\n')

    val (key, value) =
      if newline < 0 then (entry, t"") else (entry.keep(newline), entry.skip(newline + 1))

    safely(Git.Config.Key.parse(key)).let((_, value))

  def configWrite(source: Git.Config.Source, key: Git.Config.Key, value: Text, add: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    val arguments = (if add then List(t"--add") else Nil) + List(key.text, value)
    expect(run(configArgs(source) + arguments))

  def configUnset(source: Git.Config.Source, key: Git.Config.Key, all: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    val flag = if all then t"--unset-all" else t"--unset"
    val outcome = run(configArgs(source) + List(flag, key.text))

    // Exit status 5 means the key was not set, which is the state asked for.
    if !outcome.ok && outcome.exit != Exit.Fail(5) then expect(outcome)

  // ────────────────────────────────────── Remotes and transport ───────────────────────────────

  def remoteRefs(gitDir: Path on Linux, remote: Text)
    ( using Internet, Tactic[Git.Error] )
  :   List[(Text, Git.Hash)] =

    val outcome = expect(run(dirArgs(gitDir) + List(t"ls-remote", remote)))
    collect(outcome.lines)(remoteRefLine(_))

  private def remoteRefLine(line: Text): Optional[(Text, Git.Hash)] = line.cut(tab) match
    case List(hash, name) if hash.length == 40 => (name, Git.Hash.unsafe(hash))
    case _                                     => Unset

  def fetch
    ( gitDir: Path on Linux, remote: Text, refspecs: List[Git.Refspec], depth: Optional[Int],
      prune: Boolean, tags: Boolean, activity: Git.Activity^ )
    ( using Internet, Tactic[Git.Error] )
  :   List[(Text, Git.Hash)] =

    val arguments =
      List(t"fetch", t"--progress") +
        depth.lay(Nil) { depth => List(t"--depth=$depth") } +
        (if prune then List(t"--prune") else Nil) +
        (if tags then List(t"--tags") else Nil) +
        List(remote) +
        refspecs.map(_.text)

    // The refs the fetch changed: those whose value differs from before it ran. (`--porcelain`
    // would report them directly, but needs git 2.41.)
    val before = listRefs(gitDir, Unset).to[Map]
    expect(runProgress(dirArgs(gitDir) + arguments, activity))
    val after = listRefs(gitDir, Unset)
    after.filter: (name, hash) => before.at(name) != Optional(hash)

  def push
    ( gitDir: Path on Linux, remote: Text, refspecs: List[Git.Refspec], force: Git.Push.Force,
      tags: Boolean, setUpstream: Boolean, activity: Git.Activity^ )
    ( using Internet, Tactic[Git.Error] )
  :   List[(Text, Git.Push.Outcome)] =

    val forceArgs = force match
      case Git.Push.Force.Never               => Nil
      case Git.Push.Force.Always              => List(t"--force")
      case Git.Push.Force.Lease(expectations) => expectations.to[List].map(lease(_, _))

    val arguments =
      List(t"push", t"--progress", t"--porcelain") +
        forceArgs +
        (if tags then List(t"--tags") else Nil) +
        (if setUpstream then List(t"--set-upstream") else Nil) +
        List(remote) +
        refspecs.map(_.text)

    val outcome = runProgress(dirArgs(gitDir) + arguments, activity)
    val results = collect(outcome.lines)(pushLine(_))

    val rejected = results.exists: (_, result) => result.isInstanceOf[Git.Push.Outcome.Rejected]

    if outcome.ok || rejected then results
    else abort(Git.Error(Commandline.reason(outcome.stderr), outcome.stderr.trim))

  private def lease(ref: Git.Ref, expected: Stale[Optional[Git.Hash]]): Text =
    t"--force-with-lease=${ref.encode}:${expected.value.lay(Commandline.zero)(_.text)}"

  // `<flag>\t<from>:<to>\t<summary> (<reason>)`: a space for a fast-forward, `+` forced, `-`
  // deleted, `*` new, `!` rejected, `=` up to date.
  private def pushLine(line: Text): Optional[(Text, Git.Push.Outcome)] =
    line.cut(tab) match

      case List(flag, refs, summary) =>
        val destination = refs.cut(t":") match
          case List(_, destination) => destination
          case _                    => refs

        def range: Optional[(Git.Hash, Git.Hash)] = summary.cut(t".") match

          case List(from, t"", to) if from.length >= 7 && to.length >= 7 =>
            (Git.Hash.unsafe(from), Git.Hash.unsafe(to))

          case _ => Unset

        val outcome: Optional[Git.Push.Outcome] = flag match
          case t" " => range.let: (from, to) => Git.Push.Outcome.Updated(from, to, false)
          case t"+" => range.let: (from, to) => Git.Push.Outcome.Updated(from, to, true)
          case t"-" => Git.Push.Outcome.Deleted(Git.Hash.zero)
          case t"*" => Git.Push.Outcome.Created(Git.Hash.zero)
          case t"=" => Git.Push.Outcome.UpToDate

          case t"!" =>
            val open = summary.s.indexOf('(')
            val reason = if open < 0 then summary else summary.skip(open + 1).skip(1, Rtl)
            Git.Push.Outcome.Rejected(reason)

          case _    => Unset

        outcome.let((destination, _))

      case _ => Unset

  // ────────────────────────────────────────── Linked worktrees ────────────────────────────────

  def worktrees(gitDir: Path on Linux)(using Tactic[Git.Error]): List[Git.Backend.WorktreeEntry] =
    val outcome = expect(run(dirArgs(gitDir) + List(t"worktree", t"list", t"--porcelain")))

    def blocks(lines: List[Text]): List[List[Text]] = lines match
      case Nil => Nil

      case _ =>
        val (block, rest) = lines.span(_ != empty)

        rest match
          case _ :: tail => block :: blocks(tail)
          case _         => List(block)

    val tactic = summon[Tactic[Git.Error]]
    collect(blocks(outcome.stdout.cut(t"\n")))(worktreeEntry(_)(using tactic))

  private val worktreePrefix: Text = t"worktree "
  private val heads: Text = t"refs/heads/"

  private def branchOf(ref: Text): Git.Branch =
    Git.Branch.unsafe(if ref.starts(heads) then ref.skip(11) else ref)

  private def worktreeField(block: List[Text], name: Text): Optional[Text] =
    val prefixed = t"$name "

    block.reap:
      case line if line == name || line.starts(prefixed) =>
        if line == name then empty else line.skip(name.length + 1)

  private def worktreeEntry(block: List[Text])
    ( using Tactic[Git.Error] )
  :   Optional[Git.Backend.WorktreeEntry] =

    block.reap { case line if line.starts(worktreePrefix) => line.skip(9) } match

      case path: Text =>
        Git.Backend.WorktreeEntry
          ( pathOf(path),
            worktreeField(block, t"HEAD").let(Git.Hash.unsafe(_)),
            worktreeField(block, t"branch").let(branchOf(_)),
            worktreeField(block, t"bare").present,
            worktreeField(block, t"locked"),
            worktreeField(block, t"prunable") )

      case _ => Unset

  def addWorktree
    ( gitDir:    Path on Linux,
      target:    Path on Linux,
      revision:  Text,
      detach:    Boolean,
      newBranch: Optional[Git.Branch] )
    ( using Tactic[Git.Error] )
  :   Unit =

    val arguments =
      List(t"worktree", t"add") +
        (if detach then List(t"--detach") else Nil) +
        newBranch.lay(Nil) { branch => List(t"-b", branch.text) } +
        List(target.encode, revision)

    expect(run(dirArgs(gitDir) + arguments))

  def removeWorktree(gitDir: Path on Linux, path: Path on Linux, force: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    val forceArgs = if force then List(t"--force") else Nil
    val arguments = List(t"worktree", t"remove") + forceArgs + List(path.encode)

    expect(run(dirArgs(gitDir) + arguments))

  def lockWorktree(gitDir: Path on Linux, path: Path on Linux, reason: Optional[Text])
    ( using Tactic[Git.Error] )
  :   Unit =

    val reasonArgs = reason.lay(Nil): reason => List(t"--reason=$reason")
    val arguments = List(t"worktree", t"lock") + reasonArgs + List(path.encode)
    expect(run(dirArgs(gitDir) + arguments))

  def unlockWorktree(gitDir: Path on Linux, path: Path on Linux)(using Tactic[Git.Error]): Unit =
    expect(run(dirArgs(gitDir) + List(t"worktree", t"unlock", path.encode)))

  def moveWorktree(gitDir: Path on Linux, path: Path on Linux, to: Path on Linux)
    ( using Tactic[Git.Error] )
  :   Unit =

    expect(run(dirArgs(gitDir) + List(t"worktree", t"move", path.encode, to.encode)))

  def pruneWorktrees(gitDir: Path on Linux)(using Tactic[Git.Error]): List[Path on Linux] =
    val prunable = collect(worktrees(gitDir))(prunablePath(_))
    expect(run(dirArgs(gitDir) + List(t"worktree", t"prune")))
    prunable

  private def prunablePath(entry: Git.Backend.WorktreeEntry): Optional[Path on Linux] =
    entry.prunable.let: _ => entry.path
