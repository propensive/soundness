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

import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import galilei.{append as _, *}
import gossamer.*
import guillotine.*
import kaleidoscope.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import urticose.*
import vacuous.*

import GitError.Reason.*

import filesystemBackends.virtualMachine

object Worktree:
  def apply[abstractable: Abstractable across Paths to Text](path: abstractable)
    ( using Tactic[PathError], Tactic[NameError], Tactic[GitError], Tactic[IoError] )
  :   Worktree =

    unsafely(path.generic.decode[Path on Linux]).pipe: path =>
      if !path.exists() then abort(GitError(RepoDoesNotExist))

      if (path / ".git").exists() then Worktree(GitRepo((path / ".git")), path)
      else abort(GitError(NoWorkTree))


case class Worktree(repo: GitRepo, path: Path on Linux):
  val repoOptions = sh"--git-dir=${repo.gitDir} --work-tree=$path"


  @targetName("checkoutTag")
  def checkout(tag: GitTag)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    sh"$git $repoOptions checkout $tag".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(CheckoutFailed))


  @targetName("checkoutBranch")
  def checkout(branch: GitBranch)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    sh"$git $repoOptions checkout $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(CheckoutFailed))


  @targetName("checkoutGitHash")
  def checkout(commit: GitHash)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    sh"$git $repoOptions checkout $commit".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(CheckoutFailed))


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
    . exec[LazyList[Text]]()
    . map(_.skip(2))
    . to(List)
    . map(GitBranch.unsafe(_))

  // FIXME: this uses an `Executor[String]` instead of an `Executor[Text]` because, for some
  // reason, the latter captures the `WorkingDirectory` parameter
  def branch()(using GitCommand, WorkingDirectory, Tactic[ExecError]): GitBranch logs GitEvent =
    GitBranch.unsafe(sh"$git $repoOptions branch --show-current".exec[String]().tt.trim)


  def makeBranch(branch: GitBranch)
    ( using GitCommand, WorkingDirectory, Tactic[ExecError], Tactic[GitError] )
  :   Unit logs GitEvent =

    sh"$git $repoOptions checkout -b $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(BranchFailed))


  def add[path: Abstractable across Paths to Text](file: path)
    ( using GitCommand, WorkingDirectory, Tactic[PathError], Tactic[NameError], Tactic[ExecError],
            Tactic[GitError] )
  :   Unit logs GitEvent =

    val relativePath =
      safely(this.path.toward(file.generic.decode[Path on Linux])).or:
        abort(GitError(AddFailed))

    val command = sh"$git $repoOptions add $relativePath"

    command.exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(AddFailed))


  def reset(mode: ResetMode = ResetMode.Mixed, ref: Refspec = Refspec.head())
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    sh"$git $repoOptions reset $mode $ref".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(ResetFailed))


  def unstage[path: Abstractable across Paths to Text](file: path)
    ( using GitCommand, WorkingDirectory, Tactic[PathError], Tactic[NameError], Tactic[ExecError],
            Tactic[GitError] )
  :   Unit logs GitEvent =

    val relativePath =
      safely(this.path.toward(file.generic.decode[Path on Linux])).or:
        abort(GitError(ResetFailed))

    sh"$git $repoOptions reset HEAD -- $relativePath".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(ResetFailed))


  def mv
    [ fromPath: Abstractable across Paths to Text,
      toPath:   Abstractable across Paths to Text ]
    ( from: fromPath, to: toPath )
    ( using GitCommand, WorkingDirectory, Tactic[PathError], Tactic[NameError], Tactic[ExecError],
            Tactic[GitError] )
  :   Unit logs GitEvent =

    val fromRel = safely(this.path.toward(from.generic.decode[Path on Linux])).or:
      abort(GitError(MvFailed))

    val toRel = safely(this.path.toward(to.generic.decode[Path on Linux])).or:
      abort(GitError(MvFailed))

    sh"$git $repoOptions mv $fromRel $toRel".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(MvFailed))


  // diff(): worktree vs index. diff(staged = true): index vs HEAD.
  // diff(ref): full tree-vs-ref diff (working tree relative to ref).
  def diff(staged: Boolean = false)
    ( using GitCommand, WorkingDirectory, Tactic[ExecError] )
  :   LazyList[FileDiff] logs GitEvent =

    val stagedOpt = if staged then sh"--staged" else sh""
    Patch.parse(sh"$git $repoOptions diff --no-color $stagedOpt".exec[LazyList[Text]]())


  def diff(ref: Refspec)
    ( using GitCommand, WorkingDirectory, Tactic[ExecError] )
  :   LazyList[FileDiff] logs GitEvent =

    Patch.parse(sh"$git $repoOptions diff --no-color $ref".exec[LazyList[Text]]())


  def merge
    ( ref: Refspec, ff: FastForward = FastForward.Auto, message: Optional[Text] = Unset )
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    val ffOpt = ff match
      case FastForward.Auto  => sh""
      case FastForward.Only  => sh"--ff-only"
      case FastForward.Never => sh"--no-ff"

    val msgOpt = message.lay(sh""): m => sh"-m $m"

    sh"$git $repoOptions merge $ffOpt $msgOpt $ref".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(MergeFailed))


  def cherryPick(commit: GitHash)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    sh"$git $repoOptions cherry-pick $commit".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(CherryPickFailed))


  def revert(commit: GitHash, noCommit: Boolean = false)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    val noCommitOpt = if noCommit then sh"-n" else sh""

    sh"$git $repoOptions revert --no-edit $noCommitOpt $commit".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(RevertFailed))


  def lock(reason: Optional[Text] = Unset)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    val reasonOpt = reason.lay(sh""): reason => sh"--reason=$reason"

    sh"$git $repoOptions worktree lock $reasonOpt $path".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(WorktreeFailed))


  def unlock()(using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
  :   Unit logs GitEvent =

    sh"$git $repoOptions worktree unlock $path".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(WorktreeFailed))


  def move[path: Abstractable across Paths to Text](newPath: path)
    ( using GitCommand,
            WorkingDirectory,
            Tactic[GitError],
            Tactic[ExecError],
            ((Path on Linux) is Decodable in Text)^ )
  :   Worktree raises NameError raises PathError logs GitEvent =

    val target: Path on Linux =
      try newPath.generic.decode[Path on Linux]
      catch case error: PathError => abort(GitError(WorktreeFailed))

    sh"$git $repoOptions worktree move ${this.path} $target".exec[Exit]() match
      case Exit.Ok => Worktree(repo, target)
      case failure => abort(GitError(WorktreeFailed))


  def remove(force: Boolean = false)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    repo.removeWorktree(this, force)


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
