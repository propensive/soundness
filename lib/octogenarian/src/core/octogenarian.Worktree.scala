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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import galilei.*
import gossamer.*
import guillotine.*
import kaleidoscope.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import urticose.*
import vacuous.*

import GitError.Reason.*

object Worktree:
  def apply[abstractable: Abstractable across Paths to Text](path: abstractable)
  :   Worktree raises PathError raises NameError raises GitError raises IoError =

    unsafely(path.generic.decode[Path on Linux]).pipe: path =>
      if !path.exists() then abort(GitError(RepoDoesNotExist))

      if (path / ".git").exists() then Worktree(GitRepo((path / ".git")), path)
      else abort(GitError(NoWorkTree))


case class Worktree(repo: GitRepo, path: Path on Linux):
  val repoOptions = sh"--git-dir=${repo.gitDir} --work-tree=$path"


  @targetName("checkoutTag")
  def checkout(tag: GitTag)(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :   Unit logs GitEvent =

    sh"$git $repoOptions checkout $tag".exec[Exit]()


  @targetName("checkoutBranch")
  def checkout(branch: GitBranch)(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :   Unit logs GitEvent =

    sh"$git $repoOptions checkout $branch".exec[Exit]()


  @targetName("checkoutGitHash")
  def checkout(commit: GitHash)(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :   Unit logs GitEvent =

    sh"$git $repoOptions checkout $commit".exec[Exit]()


  def switch(branch: GitBranch)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    sh"$git $repoOptions switch $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(CannotSwitchBranch))


  def pull()(using GitCommand, Internet, WorkingDirectory)
    ( using gitError: Tactic[GitError], exec: Tactic[ExecError] )
  :   GitProcess[Unit] logs GitEvent =

    val process = sh"$git $repoOptions pull --progress".fork[Exit]()

    GitProcess[Unit](Git.progress(process)):
      process.await() match
        case Exit.Ok => ()
        case failure => abort(GitError(PullFailed))


  def commit(message: Text)(using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
  :   Unit logs GitEvent =

    sh"$git $repoOptions commit -m $message".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(CommitFailed))


  def branches()(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :   List[GitBranch] logs GitEvent =

    sh"$git $repoOptions branch"
    . exec[Stream[Text]]()
    . map(_.skip(2))
    . to(List)
    . map(GitBranch.unsafe(_))

  // FIXME: this uses an `Executor[String]` instead of an `Executor[Text]` because, for some
  // reason, the latter captures the `WorkingDirectory` parameter
  def branch()(using GitCommand, WorkingDirectory, Tactic[ExecError]): GitBranch logs GitEvent =
    GitBranch.unsafe(sh"$git $repoOptions branch --show-current".exec[String]().tt)


  def makeBranch(branch: GitBranch)(using GitCommand, WorkingDirectory)
  :   Unit logs GitEvent raises ExecError raises GitError =

    sh"$git $repoOptions checkout -b $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(BranchFailed))


  def add[path: Abstractable across Paths to Text](file: path)(using GitCommand, WorkingDirectory)
  :   Unit logs GitEvent raises PathError raises NameError raises ExecError raises GitError =

    val relativePath =
      safely(this.path.toward(file.generic.decode[Path on Linux])).or:
        abort(GitError(AddFailed))

    val command = sh"$git $repoOptions add $relativePath"

    command.exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(AddFailed))


  def reset(): Unit = ()
  def mv(): Unit = ()
  def diff(): Unit = ()


  def status(ignored: Boolean = false)(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :   List[GitPathStatus] logs GitEvent =

    val ignoredParam = if ignored then sh"--ignored" else sh""

    def unescape(text: Text): Text = if text.at(Prim) != '"' then text else Text.build:
      def recur(index: Int, escape: Boolean): Unit =
        if index < text.length then
          text.s.charAt(index) match
            case '\\' =>
              if escape then append('\\')
              recur(index + 1, !escape)

            case '"' =>
              if escape then
                append('"')
                recur(index + 1, false)

            case char =>
              append(char)
              recur(index + 1, false)

      recur(1, false)

    def key(character: Text): Optional[GitStatus] = character match
      case t" " => Unset
      case t"M" => GitStatus.Updated
      case t"A" => GitStatus.Added
      case t"D" => GitStatus.Deleted
      case t"R" => GitStatus.Renamed
      case t"C" => GitStatus.Copied
      case t"U" => GitStatus.Unmerged
      case t"?" => GitStatus.Untracked
      case t"!" => GitStatus.Ignored
      case _    => Unset

    sh"$git $repoOptions status --porcelain $ignoredParam".exec[List[Text]]().flatMap:
      case r"$key1([ ACDMRU?!])$key2([ ADMU?!]) $path(.*)$path2( -> (.*))?" =>
        val optionalPath = path2.let(_.skip(4)).let(unescape)
        List(GitPathStatus(key(key1), key(key2), unescape(path), optionalPath))

      case _ =>
        Nil
