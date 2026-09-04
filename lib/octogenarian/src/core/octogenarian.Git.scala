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

import scala.caps

import scala.compiletime.*

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import galilei.*
import gossamer.*
import guillotine.*
import kaleidoscope.*
import nomenclature.*
import prepositional.*
import rudiments.*
import denominative.dysasymptotics.linearSize
import serpentine.*
import symbolism.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

import beneficence.*
import enigmatic.*
import filesystemBackends.javaBaseFilesystem
import spectacular.*

object Git:
  import Error.Reason.*

  // Drop consecutive equal values from a single-pass iterator — git repeats a
  // progress percentage across many carriage-return updates. Replaces the
  // Chain-only `deduplicate` combinator, matching its consecutive semantics.
  private def distinctConsecutive(iterator: Iterator[Progress]): Iterator[Progress] =
    var previous: Optional[Progress] = Unset

    // The filter closure privately owns `previous`; the deduplicated view is
    // observationally pure.
    caps.unsafe.unsafeAssumePure:
      iterator.filter: progress =>
        previous.lay(true)(_ != progress).also { previous = progress }

  def progress(process: Job[?, ?]): Iterator[Progress] =
    import hieroglyph.charDecoders.utf8Decoder, hieroglyph.textSanitizers.substituteSanitizer
    import turbulence.lineSeparation.adaptiveLinefeedLineSeparation

    // `delineate` splits on `\n`, `\r\n` and `\r`, so git's carriage-return
    // progress updates each become their own line — subsuming the old manual
    // `cut(r"[\n\r]")`. The stderr line iterator is laundered pure (exactly as
    // the old `chain` bridge did) so the progress iterator is a plain,
    // single-owner value the fetching `Job` carries alongside its result.
    val stages = safely[Truncation.Error]:
      val lines = caps.unsafe.unsafeAssumePure(process.stderr().delineate.records)

      lines.collect:
        case r"Receiving objects: *$pc(\d*)\%.*" => Progress.Receiving(pc.s.toInt/100.0)
        case r"Resolving deltas: *$pc(\d+)\%.*"  => Progress.Resolving(pc.s.toInt/100.0)
        case r"Unpacking objects: *$pc(\d+)\%.*" => Progress.Unpacking(pc.s.toInt/100.0)

        case r"remote: *Counting objects: *$pc(\d+)\%.*" =>
          Progress.RemoteCounting(pc.s.toInt/100.0)

        case r"remote: *Compressing objects: *$pc(\d+)\%.*" =>
          Progress.RemoteCompressing(pc.s.toInt/100.0)

    . or(Iterator.empty[Progress])

    distinctConsecutive(stages)


  def init
    [ path: Abstractable across Paths to Text ]
    ( targetPath: path, initialBranch: Optional[Git.Branch] = Unset )
    ( using WorkingDirectory,
            Tactic[Git.Error],
            ((Path on Linux) is Decodable in Text)^,
            Tactic[Exec.Error] )
    ( using command: Git.Command )
  ( using Tactic[Name.Error], (Git.Event is Loggable)^ )
  :   Worktree =

    try
      throwErrors[Path.Error | Io.Error]:
        val target: Path on Linux = targetPath.generic.as[Path on Linux]
        val branchOpt = initialBranch.lay(sh""): branch => sh"--initial-branch=$branch"
        sh"$command init $branchOpt $target".exec[Exit]()

        Worktree(Git.Repo(target/".git"), target)

    catch
      case error: Path.Error => abort(Git.Error(InvalidRepoPath))
      case error: Io.Error   => abort(Git.Error(InvalidRepoPath))


  def initBare
    [ path: Abstractable across Paths to Text ]
    ( targetPath: path, initialBranch: Optional[Git.Branch] = Unset )
    ( using WorkingDirectory,
            Tactic[Git.Error],
            ((Path on Linux) is Decodable in Text)^,
            Tactic[Exec.Error] )
    ( using command: Git.Command )
  ( using Tactic[Name.Error], (Git.Event is Loggable)^ )
  :   Git.Repo =

    try
      throwErrors[Path.Error | Io.Error]:
        val target: Path on Linux = targetPath.generic.as[Path on Linux]
        val branchOpt = initialBranch.lay(sh""): branch => sh"--initial-branch=$branch"
        sh"$command init --bare $branchOpt $target".exec[Exit]()

        Git.Repo(target)

    catch
      case error: Path.Error => abort(Git.Error(InvalidRepoPath))
      case error: Io.Error   => abort(Git.Error(InvalidRepoPath))


  inline def cloneCommit[source <: Matchable, path: Abstractable across Paths to Text]
    ( source: source, targetPath: path, commit: Git.Hash )
    ( using Internet,
            ((Path on Linux) is Decodable in Text)^,
            Git.Command,
            Tactic[Git.Error],
            Tactic[Exec.Error],
            WorkingDirectory )
  ( using Tactic[Name.Error], (Git.Event is Loggable)^ )
  :   Git.Process[Worktree] =

    val sourceText = inline source match
      case source: SshUrl => source.text

      case other =>
        summonFrom:
          case given (`source` is Abstractable across Urls to Text)  => source.generic
          case given (`source` is Abstractable across Paths to Text) => source.generic

    uncheckedCloneCommit(sourceText, targetPath, commit)


  inline def clone[source <: Matchable, path: Abstractable across Paths to Text]
    ( source:     source,
      targetPath: path,
      branch:     Optional[Git.Branch] = Unset,
      recursive:  Boolean             = false )
    ( using Internet,
            WorkingDirectory,
            ((Path on Linux) is Decodable in Text)^,
            Tactic[Exec.Error],
            Git.Command )
  ( using Tactic[Path.Error], Tactic[Name.Error], Tactic[Git.Error], (Git.Event is Loggable)^ )
  :   Git.Process[Worktree] =

    val sourceText = inline source match
      case source: SshUrl => source.text

      case other =>
        summonFrom:
          case given (`source` is Abstractable across Urls to Text)  => source.generic
          case given (`source` is Abstractable across Paths to Text) => source.generic

    uncheckedClone(sourceText, targetPath, branch, recursive)


  inline def cloneBare[source <: Matchable, path: Abstractable across Paths to Text]
    ( source:     source,
      targetPath: path,
      branch:     Optional[Git.Branch] = Unset )
    ( using Internet,
            WorkingDirectory,
            ((Path on Linux) is Decodable in Text)^,
            Tactic[Exec.Error],
            Git.Command )
  ( using Tactic[Path.Error], Tactic[Name.Error], Tactic[Git.Error], (Git.Event is Loggable)^ )
  :   Git.Process[Git.Repo] =

    val sourceText = inline source match
      case source: SshUrl => source.text

      case other =>
        summonFrom:
          case given (`source` is Abstractable across Urls to Text)  => source.generic
          case given (`source` is Abstractable across Paths to Text) => source.generic

    uncheckedCloneBare(sourceText, targetPath, branch)


  private def uncheckedCloneCommit[path: Abstractable across Paths to Text]
    ( source: Text, targetPath: path, commit: Git.Hash )
    ( using Internet, ((Path on Linux) is Decodable in Text)^, Git.Command )
    ( using gitError:         Tactic[Git.Error],
            exec:             Tactic[Exec.Error],
            workingDirectory: WorkingDirectory )
  ( using Tactic[Name.Error], (Git.Event is Loggable)^ )
  :   Git.Process[Worktree] =

    val worktree = init(targetPath)
    val fetch = worktree.repo.fetch(1, source, commit)

    Git.Process(fetch.progress):
      fetch.complete()
      worktree.checkout(commit)
      worktree


  private def uncheckedClone[path: Abstractable across Paths to Text]
    ( source:     Text,
      targetPath: path,
      branch:     Optional[Git.Branch],
      recursive:  Boolean )
    ( using Internet,
            WorkingDirectory,
            ((Path on Linux) is Decodable in Text)^,
            Tactic[Exec.Error],
            Tactic[Path.Error],
            Tactic[Name.Error],
            Git.Command )
    ( using gitError: Tactic[Git.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Git.Process[Worktree] =

    val target: Path on Linux =
      try targetPath.generic.as[Path on Linux]
      catch case error: Path.Error => abort(Git.Error(InvalidRepoPath))

    val branchOption = branch.lay(sh""): branch => sh"--branch=$branch"
    val recursiveOption = if recursive then sh"--recursive" else sh""

    val process =
      sh"$git clone --progress $branchOption $recursiveOption $source $target"
      . fork[Exit]()

    Git.Process[Worktree](progress(process)):
      process.await() match
        case Exit.Ok =>
          try throwErrors[Io.Error](Worktree(Git.Repo((target/".git")), target))
          catch case error: Io.Error => abort(Git.Error(CloneFailed))

        case _ =>
          abort(Git.Error(CloneFailed))


  private def uncheckedCloneBare[path: Abstractable across Paths to Text]
    ( source:     Text,
      targetPath: path,
      branch:     Optional[Git.Branch] )
    ( using Internet,
            WorkingDirectory,
            ((Path on Linux) is Decodable in Text)^,
            Tactic[Exec.Error],
            Tactic[Path.Error],
            Tactic[Name.Error],
            Git.Command )
    ( using gitError: Tactic[Git.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Git.Process[Git.Repo] =

    val target: Path on Linux =
      try targetPath.generic.as[Path on Linux]
      catch case error: Path.Error => abort(Git.Error(InvalidRepoPath))

    val branchOption = branch.lay(sh""): branch => sh"--branch=$branch"

    val process =
      sh"$git clone --bare --progress $branchOption $source $target"
      . fork[Exit]()

    Git.Process[Git.Repo](progress(process)):
      process.await() match
        case Exit.Ok =>
          try throwErrors[Io.Error](Git.Repo(target))
          catch case error: Io.Error => abort(Git.Error(CloneFailed))

        case _ =>
          abort(Git.Error(CloneFailed))

  // GitCommand → Git.Command
  object Command:
    given parameterizable: Git.Command is Parameterizable = _.path

  case class Command(path: Text) extends Findable

  // GitError → Git.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case CannotExecuteGit   extends Reason(1)
      case CloneFailed        extends Reason(2)
      case InvalidRepoPath    extends Reason(3)
      case RepoDoesNotExist   extends Reason(4)
      case BranchDoesNotExist extends Reason(5)
      case CommitDoesNotExist extends Reason(6)
      case CommitFailed       extends Reason(7)
      case CannotSwitchBranch extends Reason(8)
      case PullFailed         extends Reason(9)
      case BranchFailed       extends Reason(10)
      case TagFailed          extends Reason(11)
      case AddFailed          extends Reason(12)
      case NoWorkTree         extends Reason(13)
      case ResetFailed        extends Reason(14)
      case MvFailed           extends Reason(15)
      case ReflogFailed       extends Reason(16)
      case DiffFailed         extends Reason(17)
      case WorktreeFailed     extends Reason(18)
      case MergeFailed        extends Reason(19)
      case CherryPickFailed   extends Reason(20)
      case RevertFailed       extends Reason(21)
      case RemoteFailed       extends Reason(22)
      case CheckoutFailed     extends Reason(23)
      case PushFailed         extends Reason(24)
      case NotesFailed        extends Reason(25)
      case NoteNotFound       extends Reason(26)

    import Reason.*

    given communicable: Reason is Communicable =
      case CannotExecuteGit   => m"the `git` command could not be executed"
      case CloneFailed        => m"the repository could not be cloned"
      case InvalidRepoPath    => m"the repository path was not valid"
      case RepoDoesNotExist   => m"the repository does not exist"
      case BranchDoesNotExist => m"the branch does not exist"
      case CommitDoesNotExist => m"the commit does not exist"
      case CommitFailed       => m"the commit could not be created"
      case AddFailed          => m"the path could not be added"
      case PullFailed         => m"the pull operation did not complete"
      case BranchFailed       => m"the new branch could not be created"
      case TagFailed          => m"the new tag could not be created"
      case NoWorkTree         => m"this bare repository does not have a work tree"
      case CannotSwitchBranch => m"the branch could not be changed"
      case ResetFailed        => m"the reset operation failed"
      case MvFailed           => m"the move operation failed"
      case ReflogFailed       => m"the reflog could not be read"
      case DiffFailed         => m"the diff could not be computed"
      case WorktreeFailed     => m"the worktree operation failed"
      case MergeFailed        => m"the merge could not be completed"
      case CherryPickFailed   => m"the cherry-pick could not be completed"
      case RevertFailed       => m"the revert could not be completed"
      case RemoteFailed       => m"the remote operation failed"
      case CheckoutFailed     => m"the checkout could not be completed"
      case PushFailed         => m"the push operation failed"
      case NotesFailed        => m"the notes operation failed"
      case NoteNotFound       => m"no note was attached to the given object"

  case class Error(reason: Git.Error.Reason)(using Diagnostics)
  extends fulminate.Error(685, reason.number)
    ( m"the Git operation could not be completed because $reason" )

  // GitEvent → Git.Event
  object Event:
    given execEvent: Git.Event transcribes guillotine.Exec.Event = Git.Event.Exec(_)

    given communicable: Git.Event is Communicable =
      case Exec(reason) => m"the git operation did not execute: $reason"

  enum Event:
    case Exec(event: guillotine.Exec.Event) extends Git.Event, Log.Process

  // GitPathStatus → Git.PathStatus
  case class PathStatus
    ( status1: Optional[Git.Status], status2: Optional[Git.Status], path1: Text, path2: Optional[Text] )

  // GitProcess → Git.Process
  class Process[+result](val progress: Iterator[Progress])(closure: => result):
    lazy val result: result = closure
    def complete(): result = result

  // GitRefError → Git.RefError
  object RefError:
    enum Reason(val number: Int) extends Clarification:
      case LeadingOrTrailingDot extends Reason(1)
      case ReservedSuffix       extends Reason(2)
      case ReservedSequence     extends Reason(3)
      case DoubleDot            extends Reason(4)
      case EmptySegment         extends Reason(5)
      case InvalidCharacter     extends Reason(6)
      case BadHash              extends Reason(7)

    given communicable: Reason is Communicable =
      case Reason.LeadingOrTrailingDot => m"a path segment starts or ends with `.`"
      case Reason.ReservedSuffix       => m"a path segment ends with the reserved suffix `.lock`"
      case Reason.ReservedSequence     => m"the name contains the reserved sequence `@{`"
      case Reason.DoubleDot            => m"the name contains `..`"
      case Reason.EmptySegment         => m"the name contains an empty path segment"
      case Reason.InvalidCharacter     => m"the name contains a character forbidden by Git"
      case Reason.BadHash              => m"the value is not a 40-character hexadecimal hash"

  case class RefError(value: Text, reason: Git.RefError.Reason)(using Diagnostics)
  extends fulminate.Error(976, reason.number)
    ( m"$value is not a valid Git reference because $reason" )

  // GitRefs → Git.Refs
  // `Git.Refs` is the Serpentine path scheme for Git references. Every fully-
  // qualified reference (`refs/heads/main`, `refs/notes/commits`, …) is rooted
  // at `refs/` and is slash-separated, so it maps cleanly onto a Serpentine
  // path. Component-level validation reproduces the rules `git check-ref-format`
  // enforces, expressed as an `Admissible` typeclass.
  object Refs extends Root(t"refs/"):
    type Plane = Git.Refs

    // Construct a notes-namespace ref path of the form `refs/notes/<namespace>`.
    // The namespace is validated against `git check-ref-format`'s per-segment
    // rules; an invalid namespace raises `Git.RefError`.
    def notes(namespace: Text)(using Tactic[Git.RefError]): Path on Git.Refs =
      validateSegment(namespace)
      Git.Refs / t"notes" / namespace

    // Construct a branch ref path of the form `refs/heads/<branch>`.
    def heads(branch: Text)(using Tactic[Git.RefError]): Path on Git.Refs =
      validateSegment(branch)
      Git.Refs / t"heads" / branch

    // Construct a tag ref path of the form `refs/tags/<tag>`.
    def tags(tag: Text)(using Tactic[Git.RefError]): Path on Git.Refs =
      validateSegment(tag)
      Git.Refs / t"tags" / tag

    // The default notes namespace used by `git notes` when no `--ref` is given.
    val defaultNotes: Path on Git.Refs = unsafely(Git.Refs.notes(t"commits"))

    // Serpentine's `/` operator does not invoke an `Admissible`'s `check` at
    // construction time, so the per-segment rules live here and are invoked
    // explicitly from the typed constructors above.
    def validateSegment(segment: Text)(using Tactic[Git.RefError]): Unit =
      def fail(reason: Git.RefError.Reason) = abort(Git.RefError(segment, reason))
      if segment.length == 0     then fail(Git.RefError.Reason.EmptySegment)
      if segment.starts(t".")    then fail(Git.RefError.Reason.LeadingOrTrailingDot)
      if segment.ends(t".")      then fail(Git.RefError.Reason.LeadingOrTrailingDot)
      if segment.ends(t".lock")  then fail(Git.RefError.Reason.ReservedSuffix)
      if segment.contains(t"@{") then fail(Git.RefError.Reason.ReservedSequence)
      if segment.contains(t"..") then fail(Git.RefError.Reason.DoubleDot)

      List('*', '[', '\\', ' ', '^', '~', ':', '?', '/').each: ch =>
        if segment.contains(ch) then fail(Git.RefError.Reason.InvalidCharacter)

    // The default `text is Admissible on filesystem` from Serpentine already
    // satisfies the typeclass slot for any plane; the marker here is for the
    // benefit of `summonFrom` in Path's `/` operator, which treats the absence
    // of an `Admissible` as evidence that the result should be unplatformed.
    given admissible: [text <: Text] => text is Admissible on Git.Refs = _ => ()

    given filesystem: Git.Refs is Filesystem:
      val name: Text = t"Git.Refs"
      val separator: Text = t"/"
      val self: Text = t"@"
      val parent: Text = t".."

    // A validated `Path on Git.Refs` already satisfies every rule git enforces,
    // so it is safe to expose it as an opaque `Refspec` for any operation that
    // accepts one.
    given pathIsRefspec: Conversion[Path on Git.Refs, Refspec] =
      path => Refspec.unsafe(path.encode)

  trait Refs

  // GitRepo → Git.Repo
  object Repo:
    def at[abstractable: Abstractable across Paths to Text](path: abstractable)
      ( using Tactic[Path.Error], Tactic[Name.Error], Tactic[Git.Error], Tactic[Io.Error] )
    :   Git.Repo =

      unsafely(path.generic.as[Path on Linux]).pipe: path =>
        if !path.existent() then abort(Git.Error(RepoDoesNotExist))

        if (path / ".git").existent() then Git.Repo((path / ".git")) else Git.Repo(path)


  case class Repo(gitDir: Path on Linux):
    val repoOptions = sh"--git-dir=$gitDir"


    def pushTags()(using Internet, Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error])
    ( using (Git.Event is Loggable)^ )
    :   Unit =

      sh"$git $repoOptions push --tags".exec[Exit]() match
        case Exit.Ok => ()
        case failure => abort(Git.Error(PushFailed))


    def push()(using Internet, Tactic[Git.Error], Git.Command, WorkingDirectory, Tactic[Exec.Error])
    ( using (Git.Event is Loggable)^ )
    :   Unit =

      sh"$git $repoOptions push".exec[Exit]() match
        case Exit.Ok => ()
        case failure => abort(Git.Error(PushFailed))


    def fetch(depth: Optional[Int] = Unset, repo: Text, refspec: Refspec)
      ( using Git.Command, Internet, WorkingDirectory )
      ( using gitError: Tactic[Git.Error], exec: Tactic[Exec.Error] )
    ( using (Git.Event is Loggable)^ )
    :   Git.Process[Unit] =

      val depthOption = depth.lay(sh""): depth => sh"--depth=$depth"
      val command = sh"$git $repoOptions fetch $depthOption --progress $repo $refspec"
      val process = command.fork[Exit]()

      Git.Process[Unit](Git.progress(process)):
        process.await() match
          case Exit.Ok => ()
          case failure => abort(Git.Error(PullFailed))


    object config:
      def get[value: Decodable in Text](variable: Text)
        ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
      ( using (Git.Event is Loggable)^ )
      :   value =

        sh"$git $repoOptions config --get $variable".exec[Text]().as[value]

    def tags()(using Git.Command, WorkingDirectory, Tactic[Exec.Error])
      ( using (Git.Event is Loggable)^ )
    :   List[Git.Tag] =
      sh"$git $repoOptions tag".exec[Iterator[Text]]().to(List).map(Git.Tag.unsafe(_))


    def tag(name: Git.Tag)(using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error])
    ( using (Git.Event is Loggable)^ )
    :   Git.Tag =

      sh"$git $repoOptions tag $name".exec[Exit]() match
        case Exit.Ok => name
        case failure => abort(Git.Error(TagFailed))


    def deleteTag(name: Git.Tag)
      ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
    ( using (Git.Event is Loggable)^ )
    :   Unit =

      sh"$git $repoOptions tag -d $name".exec[Exit]() match
        case Exit.Ok => ()
        case failure => abort(Git.Error(TagFailed))


    def deleteBranch(branch: Git.Branch, force: Boolean = false)
      ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
    ( using (Git.Event is Loggable)^ )
    :   Unit =

      val flag = if force then sh"-D" else sh"-d"

      sh"$git $repoOptions branch $flag $branch".exec[Exit]() match
        case Exit.Ok => ()
        case failure => abort(Git.Error(BranchFailed))


    def renameBranch(from: Git.Branch, to: Git.Branch, force: Boolean = false)
      ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
    ( using (Git.Event is Loggable)^ )
    :   Unit =

      val flag = if force then sh"-M" else sh"-m"

      sh"$git $repoOptions branch $flag $from $to".exec[Exit]() match
        case Exit.Ok => ()
        case failure => abort(Git.Error(BranchFailed))


    def remotes()(using Git.Command, WorkingDirectory, Tactic[Exec.Error])
    ( using (Git.Event is Loggable)^ )
    :   List[Remote] =

      val lines = sh"$git $repoOptions remote -v".exec[Iterator[Text]]()

      val grouped = lines.collect:
        case r"$name(\S+)\t$url(\S+) \($kind(fetch|push)\)" => (name, url, kind)

      val remotes = grouped.to(List).group(_._1).to[List].map: (name, rows) =>
        val fetch = rows.reap { case (_, url, t"fetch") => url }.or(t"")
        val push  = rows.reap { case (_, url, t"push")  => url }
        Remote(name, fetch, push)

      remotes


    def addRemote(name: Text, url: Text)
      ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
    ( using (Git.Event is Loggable)^ )
    :   Remote =

      sh"$git $repoOptions remote add $name $url".exec[Exit]() match
        case Exit.Ok => Remote(name, url)
        case failure => abort(Git.Error(RemoteFailed))


    def removeRemote(name: Text)
      ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
    ( using (Git.Event is Loggable)^ )
    :   Unit =

      sh"$git $repoOptions remote remove $name".exec[Exit]() match
        case Exit.Ok => ()
        case failure => abort(Git.Error(RemoteFailed))


    private def parsePem(text: Text): Optional[Pem] = safely(text.read[Pem])

    def log()(using Git.Command, WorkingDirectory, Tactic[Exec.Error])
      ( using (Git.Event is Loggable)^ )
    :   List[Commit] =
      val lines =
        sh"$git $repoOptions log --format=raw --color=never".exec[Iterator[Text]]().buffered

      val commits = scala.collection.mutable.ListBuffer[Commit]()

      var hash:      Optional[Git.Hash] = Unset
      var tree:      Optional[Git.Hash] = Unset
      var parents:   List[Git.Hash] = Nil
      var author:    Optional[Text]    = Unset
      var committer: Optional[Text]    = Unset
      var signature: List[Text]        = Nil
      var body:      List[Text] = Nil

      // A commit is emitted only once all four mandatory headers have arrived; the
      // nested `let`s both check and name them, so the emission reads bare values.
      def flush(): Unit =
        hash.let: hash =>
          tree.let: tree =>
            author.let: author =>
              committer.let: committer =>
                unsafely:
                  commits +=
                    Commit
                      ( hash,
                        tree,
                        parents.reverse,
                        author,
                        committer,
                        parsePem(signature.join(t"\n")),
                        body.reverse )

      // A gpgsig block continues on the following one-space-indented lines.
      def indented(): List[Text] =
        val buffer = scala.collection.mutable.ListBuffer[Text]()

        while lines.hasNext && { lines.head match { case r" $line(.*)" => true; case _ => false } }
        do lines.next() match
          case r" $line(.*)" => buffer += line
          case _             => ()

        buffer.to(List)

      while lines.hasNext do lines.next() match
        case t""                 => ()

        case r"commit $h(.{40})" =>
          flush()
          hash = Git.Hash.unsafe(h); tree = Unset; parents = Nil
          author = Unset; committer = Unset; signature = Nil; body = Nil

        case r"tree $t(.{40})"                           => tree = Git.Hash.unsafe(t)
        case r"parent $p(.{40})"                         => parents = Git.Hash.unsafe(p) :: parents
        case r"author $a(.*) $ts([0-9]+) $tz(.....)"     => author = a
        case r"committer $c(.*) $ts([0-9]+) $tz(.....)"  => committer = c
        case r"gpgsig $start(.*)"                        => signature = start :: indented()
        case r"    $line(.*)"                            => body = line :: body
        case other                                       => ()

      flush()
      commits.to(List)


    def diff(refA: Refspec, refB: Refspec)
      ( using Git.Command, WorkingDirectory, Tactic[Exec.Error] )
    ( using (Git.Event is Loggable)^ )
    :   List[FileDiff] =

      Patch.parse(sh"$git $repoOptions diff --no-color $refA $refB".exec[Iterator[Text]]())


    def reflog(ref: Optional[Refspec] = Unset)
      ( using Git.Command, WorkingDirectory, Tactic[Exec.Error] )
    ( using (Git.Event is Loggable)^ )
    :   List[ReflogEntry] =

      val refArg = ref.lay(sh""): ref => sh"$ref"
      val format = t"--format=%H %gd %ct %gs"

      sh"$git $repoOptions reflog show $format $refArg".exec[Iterator[Text]]().collect:
        case r"$hash([a-f0-9]{40}) $selector(\S+) $time([0-9]+) $message(.*)" =>
          ReflogEntry(Git.Hash.unsafe(hash), selector, time.s.toLong, message)
      . to(List)


    def revParse(refspec: Refspec)(using Git.Command, WorkingDirectory, Tactic[Exec.Error])
    ( using (Git.Event is Loggable)^ )
    :   Git.Hash =

      Git.Hash.unsafe(sh"$git $repoOptions rev-parse $refspec".exec[Text]().trim)


    object notes:
      def show(target: Git.Hash, ref: Path on Git.Refs = Git.Refs.defaultNotes)
        ( using Git.Command, WorkingDirectory, Tactic[Exec.Error] )
      ( using (Git.Event is Loggable)^ )
      :   Optional[Text] =

        val refArg = sh"--ref=${ref.encode}"

        sh"$git $repoOptions notes $refArg show $target".exec[Exit]() match
          case Exit.Ok =>
            // `git notes show` appends a trailing newline to its output that is
            // not part of the stored note; strip it to round-trip cleanly with
            // bodies passed in to `add` / `append`.
            val raw = sh"$git $repoOptions notes $refArg show $target".exec[Text]()
            if raw.ends(t"\n") then raw.skip(1, Rtl) else raw

          case _ =>
            Unset


      def add
        ( target: Git.Hash, body: Text, force: Boolean = false,
          ref:    Path on Git.Refs = Git.Refs.defaultNotes )
        ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
      ( using (Git.Event is Loggable)^ )
      :   Unit =

        val refArg   = sh"--ref=${ref.encode}"
        val forceOpt = if force then sh"-f" else sh""

        sh"$git $repoOptions notes $refArg add $forceOpt -m $body $target".exec[Exit]() match
          case Exit.Ok => ()
          case _       => abort(Git.Error(NotesFailed))


      def append
        ( target: Git.Hash, body: Text, ref: Path on Git.Refs = Git.Refs.defaultNotes )
        ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
      ( using (Git.Event is Loggable)^ )
      :   Unit =

        val refArg = sh"--ref=${ref.encode}"

        sh"$git $repoOptions notes $refArg append -m $body $target".exec[Exit]() match
          case Exit.Ok => ()
          case _       => abort(Git.Error(NotesFailed))


      def remove
        ( target: Git.Hash, ignoreMissing: Boolean = false,
          ref:    Path on Git.Refs = Git.Refs.defaultNotes )
        ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
      ( using (Git.Event is Loggable)^ )
      :   Unit =

        val refArg     = sh"--ref=${ref.encode}"
        val missingOpt = if ignoreMissing then sh"--ignore-missing" else sh""

        sh"$git $repoOptions notes $refArg remove $missingOpt $target".exec[Exit]() match
          case Exit.Ok => ()
          case _       => abort(Git.Error(NotesFailed))


      def list(ref: Path on Git.Refs = Git.Refs.defaultNotes)
        ( using Git.Command, WorkingDirectory, Tactic[Exec.Error] )
      ( using (Git.Event is Loggable)^ )
      :   List[(Git.Hash, Git.Hash)] =

        val refArg = sh"--ref=${ref.encode}"

        sh"$git $repoOptions notes $refArg list".exec[Iterator[Text]]().collect:
          case r"$noteHash([a-f0-9]{40}) $target([a-f0-9]{40})" =>
            (Git.Hash.unsafe(noteHash), Git.Hash.unsafe(target))
        . to(List)


      def copy
        ( from: Git.Hash, to: Git.Hash, force: Boolean = false,
          ref:  Path on Git.Refs = Git.Refs.defaultNotes )
        ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
      ( using (Git.Event is Loggable)^ )
      :   Unit =

        val refArg   = sh"--ref=${ref.encode}"
        val forceOpt = if force then sh"-f" else sh""

        sh"$git $repoOptions notes $refArg copy $forceOpt $from $to".exec[Exit]() match
          case Exit.Ok => ()
          case _       => abort(Git.Error(NotesFailed))


    // Lists every non-bare worktree attached to this object database.
    def worktrees()(using Git.Command, WorkingDirectory, Tactic[Exec.Error])
    ( using Tactic[Git.Error], (Git.Event is Loggable)^ )
    :   List[Worktree] =

      val lines = sh"$git $repoOptions worktree list --porcelain".exec[List[Text]]()

      // Each worktree block is separated by an empty line. Split, then keep
      // only the non-bare entries (a `bare` line indicates a bare worktree
      // entry, which has no working tree).
      def blocks(remaining: List[Text]): List[List[Text]] = remaining match
        case Nil => Nil

        case _ =>
          val (block, rest) = remaining.span(_ != t"")
          block :: blocks(rest.skip(_ == t""))

      val worktrees = blocks(lines).bind: block =>
        val isBare = block.has(t"bare")

        block.sweep:
          case r"worktree $path(.*)" if !isBare =>
            val pathOnLinux = unsafely(path.as[Path on Linux])
            Worktree(this, pathOnLinux)

      worktrees


    def addWorktree
      [ path: Abstractable across Paths to Text ]
      ( target: path, ref: Refspec, detach: Boolean = false )
      ( using WorkingDirectory,
              Tactic[Git.Error],
              ((Path on Linux) is Decodable in Text)^,
              Tactic[Exec.Error],
              Git.Command )
    ( using Tactic[Name.Error], Tactic[Path.Error], (Git.Event is Loggable)^ )
    :   Worktree =

      val targetPath: Path on Linux =
        try target.generic.as[Path on Linux]
        catch case error: Path.Error => abort(Git.Error(WorktreeFailed))

      val detachOpt = if detach then sh"--detach" else sh""

      sh"$git $repoOptions worktree add $detachOpt $targetPath $ref".exec[Exit]() match
        case Exit.Ok => Worktree(this, targetPath)
        case failure => abort(Git.Error(WorktreeFailed))


    def removeWorktree(worktree: Worktree, force: Boolean = false)
      ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
    ( using (Git.Event is Loggable)^ )
    :   Unit =

      val forceOpt = if force then sh"--force" else sh""

      sh"$git $repoOptions worktree remove $forceOpt ${worktree.path}".exec[Exit]() match
        case Exit.Ok => ()
        case failure => abort(Git.Error(WorktreeFailed))


    def pruneWorktrees()
      ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
    ( using (Git.Event is Loggable)^ )
    :   Unit =

      sh"$git $repoOptions worktree prune".exec[Exit]() match
        case Exit.Ok => ()
        case failure => abort(Git.Error(WorktreeFailed))

  // GitStatus → Git.Status
  enum Status:
    case Updated, Added, Deleted, Renamed, Copied, Unmerged, Untracked, Ignored

  // GitTag/GitBranch/GitHash → Git.Tag/Branch/Hash
  object Tag:
    def unsafe(text: Text): Git.Tag = new Git.Tag(text)
    def parse(text: Text)(using Tactic[Git.RefError]): Git.Tag = new Git.Tag(octogenarian.internal.Refspec.parse(text))

    given decoder: (tactic: Tactic[Git.RefError])
    =>  ((Git.Tag is Decodable in Text)^{tactic}) = parse(_)
    given showable: Git.Tag is Showable = _.text

    // The name alone would be indistinguishable from a branch name, a tag and a raw refspec,
    // each of which is a different argument to git, so every ref names its own kind.
    given inspectable: [tag <: Git.Tag] => tag is Inspectable = tag => t"Tag(${tag.text})"

  case class Tag(text: Text) extends octogenarian.internal.Refspec

  object Branch:
    def unsafe(text: Text): Git.Branch = new Git.Branch(text)
    def parse(text: Text)(using Tactic[Git.RefError]): Git.Branch = new Git.Branch(octogenarian.internal.Refspec.parse(text))

    given decoder: (tactic: Tactic[Git.RefError])
    =>  ((Git.Branch is Decodable in Text)^{tactic}) = parse(_)
    given showable: Git.Branch is Showable = _.text

    given inspectable: [branch <: Git.Branch] => branch is Inspectable = branch =>
      t"Branch(${branch.text})"

  case class Branch(text: Text) extends octogenarian.internal.Refspec

  // `Git.Hash` extends Serpentine's `Root` (and therefore `Path`), so a hash
  // IS a notes-plane path root: `hash / t"foo" / t"bar"` invokes Serpentine's
  // own `Path.def /` directly, with no Conversion or entry-point extension
  // needed.  Equality is by hash (Drive-style override).
  object Hash:
    def apply(text: Text)(using Tactic[Git.RefError]): Git.Hash = text match
      case r"[a-f0-9]{40}" => new Git.Hash(text)
      case _               => abort(Git.RefError(text, Git.RefError.Reason.BadHash))

    def unsafe(text: Text): Git.Hash = new Git.Hash(text)

    given decoder: (tactic: Tactic[Git.RefError])
    =>  ((Git.Hash is Decodable in Text)^{tactic}) = apply(_)
    given showable: Git.Hash is Showable = _.text

    // The full forty hexadecimal digits: an abbreviated hash is ambiguous between objects, and
    // an inspection which abbreviated would hide exactly the difference being looked for.
    given inspectable: [hash <: Git.Hash] => hash is Inspectable = hash => t"Hash(${hash.text})"

  class Hash(val text: Text) extends Root(t"$text/"), octogenarian.internal.Refspec:
    type Plane = Notes
    type Limit = Git.Hash

    override def hashCode: Int = text.hashCode

    override def equals(any: Any): Boolean = any match
      case other: Git.Hash => text == other.text
      case _              => false
