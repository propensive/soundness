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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import serpentine.*
import symbolism.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

import GitError.Reason.*

object Git:
  // Drop consecutive equal values from a single-pass iterator — git repeats a
  // progress percentage across many carriage-return updates. Replaces the
  // Progression-only `deduplicate` combinator, matching its consecutive semantics.
  private def distinctConsecutive(iterator: Iterator[Progress]): Iterator[Progress] =
    var previous: Optional[Progress] = Unset

    iterator.filter: progress =>
      (previous.absent || previous.vouch != progress).also { previous = progress }

  def progress(process: Job[?, ?]): Iterator[Progress] =
    import hieroglyph.charDecoders.utf8Decoder, hieroglyph.textSanitizers.substituteSanitizer
    import turbulence.lineSeparation.adaptiveLinefeedLineSeparation

    // `delineate` splits on `\n`, `\r\n` and `\r`, so git's carriage-return
    // progress updates each become their own line — subsuming the old manual
    // `cut(r"[\n\r]")`. The stderr line iterator is laundered pure (exactly as
    // the old `toProgression` bridge did) so the progress iterator is a plain,
    // single-owner value the fetching `Job` carries alongside its result.
    val stages = safely[StreamError]:
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
    ( targetPath: path, initialBranch: Optional[GitBranch] = Unset )
    ( using WorkingDirectory,
            Tactic[GitError],
            ((Path on Linux) is Decodable in Text)^,
            Tactic[ExecError] )
    ( using command: GitCommand )
  :   Worktree raises NameError logs GitEvent =

    try
      throwErrors[PathError | IoError]:
        val target: Path on Linux = targetPath.generic.as[Path on Linux]
        val branchOpt = initialBranch.lay(sh""): branch => sh"--initial-branch=$branch"
        sh"$command init $branchOpt $target".exec[Exit]()

        Worktree(GitRepo(target/".git"), target)

    catch
      case error: PathError => abort(GitError(InvalidRepoPath))
      case error: IoError   => abort(GitError(InvalidRepoPath))


  def initBare
    [ path: Abstractable across Paths to Text ]
    ( targetPath: path, initialBranch: Optional[GitBranch] = Unset )
    ( using WorkingDirectory,
            Tactic[GitError],
            ((Path on Linux) is Decodable in Text)^,
            Tactic[ExecError] )
    ( using command: GitCommand )
  :   GitRepo raises NameError logs GitEvent =

    try
      throwErrors[PathError | IoError]:
        val target: Path on Linux = targetPath.generic.as[Path on Linux]
        val branchOpt = initialBranch.lay(sh""): branch => sh"--initial-branch=$branch"
        sh"$command init --bare $branchOpt $target".exec[Exit]()

        GitRepo(target)

    catch
      case error: PathError => abort(GitError(InvalidRepoPath))
      case error: IoError   => abort(GitError(InvalidRepoPath))


  inline def cloneCommit[source <: Matchable, path: Abstractable across Paths to Text]
    ( source: source, targetPath: path, commit: GitHash )
    ( using Internet,
            ((Path on Linux) is Decodable in Text)^,
            GitCommand,
            Tactic[GitError],
            Tactic[ExecError],
            WorkingDirectory )
  :   GitProcess[Worktree] raises NameError logs GitEvent =

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
      branch:     Optional[GitBranch] = Unset,
      recursive:  Boolean             = false )
    ( using Internet,
            WorkingDirectory,
            ((Path on Linux) is Decodable in Text)^,
            Tactic[ExecError],
            GitCommand )
  :   GitProcess[Worktree] raises PathError raises NameError raises GitError logs GitEvent =

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
      branch:     Optional[GitBranch] = Unset )
    ( using Internet,
            WorkingDirectory,
            ((Path on Linux) is Decodable in Text)^,
            Tactic[ExecError],
            GitCommand )
  :   GitProcess[GitRepo] raises PathError raises NameError raises GitError logs GitEvent =

    val sourceText = inline source match
      case source: SshUrl => source.text

      case other =>
        summonFrom:
          case given (`source` is Abstractable across Urls to Text)  => source.generic
          case given (`source` is Abstractable across Paths to Text) => source.generic

    uncheckedCloneBare(sourceText, targetPath, branch)


  private def uncheckedCloneCommit[path: Abstractable across Paths to Text]
    ( source: Text, targetPath: path, commit: GitHash )
    ( using Internet, ((Path on Linux) is Decodable in Text)^, GitCommand )
    ( using gitError:         Tactic[GitError],
            exec:             Tactic[ExecError],
            workingDirectory: WorkingDirectory )
  :   GitProcess[Worktree] raises NameError logs GitEvent =

    val worktree = init(targetPath)
    val fetch = worktree.repo.fetch(1, source, commit)

    GitProcess(fetch.progress):
      fetch.complete()
      worktree.checkout(commit)
      worktree


  private def uncheckedClone[path: Abstractable across Paths to Text]
    ( source:     Text,
      targetPath: path,
      branch:     Optional[GitBranch],
      recursive:  Boolean )
    ( using Internet,
            WorkingDirectory,
            ((Path on Linux) is Decodable in Text)^,
            Tactic[ExecError],
            Tactic[PathError],
            Tactic[NameError],
            GitCommand )
    ( using gitError: Tactic[GitError] )
  :   GitProcess[Worktree] logs GitEvent =

    val target: Path on Linux =
      try targetPath.generic.as[Path on Linux]
      catch case error: PathError => abort(GitError(InvalidRepoPath))

    val branchOption = branch.lay(sh""): branch => sh"--branch=$branch"
    val recursiveOption = if recursive then sh"--recursive" else sh""

    val process =
      sh"$git clone --progress $branchOption $recursiveOption $source $target"
      . fork[Exit]()

    GitProcess[Worktree](progress(process)):
      process.await() match
        case Exit.Ok =>
          try throwErrors[IoError](Worktree(GitRepo((target/".git")), target))
          catch case error: IoError => abort(GitError(CloneFailed))

        case _ =>
          abort(GitError(CloneFailed))


  private def uncheckedCloneBare[path: Abstractable across Paths to Text]
    ( source:     Text,
      targetPath: path,
      branch:     Optional[GitBranch] )
    ( using Internet,
            WorkingDirectory,
            ((Path on Linux) is Decodable in Text)^,
            Tactic[ExecError],
            Tactic[PathError],
            Tactic[NameError],
            GitCommand )
    ( using gitError: Tactic[GitError] )
  :   GitProcess[GitRepo] logs GitEvent =

    val target: Path on Linux =
      try targetPath.generic.as[Path on Linux]
      catch case error: PathError => abort(GitError(InvalidRepoPath))

    val branchOption = branch.lay(sh""): branch => sh"--branch=$branch"

    val process =
      sh"$git clone --bare --progress $branchOption $source $target"
      . fork[Exit]()

    GitProcess[GitRepo](progress(process)):
      process.await() match
        case Exit.Ok =>
          try throwErrors[IoError](GitRepo(target))
          catch case error: IoError => abort(GitError(CloneFailed))

        case _ =>
          abort(GitError(CloneFailed))
