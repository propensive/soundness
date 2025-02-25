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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import galilei.*
import gossamer.*
import guillotine.*
import kaleidoscope.*
import nettlesome.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*

import pathNavigation.posix

import GitError.Reason.*

object Git:
  def progress(process: Process[?, ?]): Stream[Progress] =
    safely[StreamError]:
      process.stderr().map(_.utf8).map(_.trim).flatMap(_.cut(r"[\n\r]")).collect:
        case r"Receiving objects: *$pc(\d*)\%.*" => Progress.Receiving(pc.s.toInt/100.0)
        case r"Resolving deltas: *$pc(\d+)\%.*"  => Progress.Resolving(pc.s.toInt/100.0)
        case r"Unpacking objects: *$pc(\d+)\%.*" => Progress.Unpacking(pc.s.toInt/100.0)

        case r"remote: *Counting objects: *$pc(\d+)\%.*" =>
          Progress.RemoteCounting(pc.s.toInt/100.0)

        case r"remote: *Compressing objects: *$pc(\d+)\%.*" =>
          Progress.RemoteCompressing(pc.s.toInt/100.0)

    . or(Stream())
    . deduplicate

  def init
     [PathType: Abstractable across Paths into Text]
     (targetPath: PathType, bare: Boolean = false)
     (using WorkingDirectory,
            Tactic[GitError],
            (Path on Posix) is Decodable in Text,
            Tactic[ExecError])
     (using command: GitCommand)
  :     GitRepo logs GitEvent raises NameError =
    try
      throwErrors[PathError | IoError]:
        val bareOpt = if bare then sh"--bare" else sh""
        val target: Path on Posix = targetPath.generic.decode[Path on Posix]
        sh"$command init $bareOpt $target".exec[Exit]()

        if bare then GitRepo(target, Unset) else GitRepo((target / n".git"), target)

    catch
      case error: PathError => abort(GitError(InvalidRepoPath))
      case error: IoError   => abort(GitError(InvalidRepoPath))

  inline def cloneCommit[SourceType <: Matchable, PathType: Abstractable across Paths into Text]
     (source: SourceType, targetPath: PathType, commit: CommitHash)
     (using Internet,
            (Path on Posix) is Decodable in Text,
            GitCommand,
            Tactic[GitError],
            Tactic[ExecError],
            WorkingDirectory)
  :     GitProcess[GitRepo] logs GitEvent raises NameError =

    val sourceText = inline source match
      case source: SshUrl => source.text
      case other          => summonFrom:
        case given (SourceType is Abstractable across Urls into Text)  => source.generic
        case given (SourceType is Abstractable across Paths into Text) => source.generic

    uncheckedCloneCommit(sourceText, targetPath, commit)

  inline def clone[SourceType <: Matchable, PathType: Abstractable across Paths into Text]
     (source:     SourceType,
      targetPath: PathType,
      bare:       Boolean             = false,
      branch:     Optional[GitBranch] = Unset,
      recursive:  Boolean             = false)
     (using Internet,
            WorkingDirectory,
            (Path on Posix) is Decodable in Text,
            Tactic[ExecError],
            GitCommand)
     (using gitError: Tactic[GitError])
  :     GitProcess[GitRepo] logs GitEvent raises PathError raises NameError =

    val sourceText = inline source match
      case source: SshUrl => source.text
      case other          => summonFrom:
        case given (SourceType is Abstractable across Urls into Text)  => source.generic
        case given (SourceType is Abstractable across Paths into Text) => source.generic

    uncheckedClone(sourceText, targetPath, bare, branch, recursive)

  private def uncheckedCloneCommit[PathType: Abstractable across Paths into Text]
     (source: Text, targetPath: PathType, commit: CommitHash)
     (using Internet, (Path on Posix) is Decodable in Text, GitCommand)
     (using gitError:         Tactic[GitError],
            exec:             Tactic[ExecError],
            workingDirectory: WorkingDirectory)
  :     GitProcess[GitRepo] logs GitEvent raises NameError =

    val gitRepo = init(targetPath)
    val fetch = gitRepo.fetch(1, source, commit)

    GitProcess(fetch.progress):
      fetch.complete()
      gitRepo.checkout(commit)
      gitRepo

  private def uncheckedClone[PathType: Abstractable across Paths into Text]
     (source:     Text,
      targetPath: PathType,
      bare:       Boolean             = false,
      branch:     Optional[GitBranch] = Unset,
      recursive:  Boolean             = false)
     (using Internet,
            WorkingDirectory,
            (Path on Posix) is Decodable in Text,
            Tactic[ExecError],
            GitCommand)
     (using gitError: Tactic[GitError])
  :     GitProcess[GitRepo] logs GitEvent raises PathError raises NameError =

    val target: Path on Posix =
      try targetPath.generic.decode[Path on Posix]
      catch case error: PathError => abort(GitError(InvalidRepoPath))

    val bareOption = if bare then sh"--bare" else sh""
    val branchOption = branch.lay(sh"") { branch => sh"--branch=$branch" }
    val recursiveOption = if recursive then sh"--recursive" else sh""

    val process =
      sh"$git clone --progress $bareOption $branchOption $recursiveOption $source $target"
      . fork[Exit]()

    GitProcess[GitRepo](progress(process)):
      process.await() match
        case Exit.Ok =>
          try throwErrors[IoError](GitRepo((target / n".git"), target))
          catch case error: IoError => abort(GitError(CloneFailed))

        case _ =>
          abort(GitError(CloneFailed))
