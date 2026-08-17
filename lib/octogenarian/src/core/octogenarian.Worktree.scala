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
import rudiments.*
import serpentine.*
import urticose.*
import vacuous.*

import Git.Error.Reason.*

import filesystemBackends.virtualMachineFilesystem

object Worktree:
  def apply[abstractable: Abstractable across Paths to Text](path: abstractable)
    ( using Tactic[Path.Error], Tactic[Name.Error], Tactic[Git.Error], Tactic[Io.Error] )
  :   Worktree =

    unsafely(path.generic.as[Path on Linux]).pipe: path =>
      if !path.existent() then abort(Git.Error(RepoDoesNotExist))

      if (path / ".git").existent() then Worktree(Git.Repo((path / ".git")), path)
      else abort(Git.Error(NoWorkTree))


case class Worktree(repo: Git.Repo, path: Path on Linux):
  val repoOptions = sh"--git-dir=${repo.gitDir} --work-tree=$path"


  @targetName("checkoutTag")
  def checkout(tag: Git.Tag)
    ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    sh"$git $repoOptions checkout $tag".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(CheckoutFailed))


  @targetName("checkoutBranch")
  def checkout(branch: Git.Branch)
    ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    sh"$git $repoOptions checkout $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(CheckoutFailed))


  @targetName("checkoutGitHash")
  def checkout(commit: Git.Hash)
    ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    sh"$git $repoOptions checkout $commit".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(CheckoutFailed))


  def switch(branch: Git.Branch)
    ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    sh"$git $repoOptions switch $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(CannotSwitchBranch))


  def pull()(using Git.Command, Internet, WorkingDirectory)
    ( using gitError: Tactic[Git.Error], exec: Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Git.Process[Unit] =

    val process = sh"$git $repoOptions pull --progress".fork[Exit]()

    Git.Process[Unit](Git.progress(process)):
      process.await() match
        case Exit.Ok => ()
        case failure => abort(Git.Error(PullFailed))


  def commit(message: Text)(using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error])
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    sh"$git $repoOptions commit -m $message".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(CommitFailed))


  def branches()(using Git.Command, WorkingDirectory, Tactic[Exec.Error])
  ( using (Git.Event is Loggable)^ )
  :   List[Git.Branch] =

    sh"$git $repoOptions branch"
    . exec[Iterator[Text]]()
    . map(_.skip(2))
    . to(List)
    . map(Git.Branch.unsafe(_))

  // FIXME: this uses an `Executor[String]` instead of an `Executor[Text]` because, for some
  // reason, the latter captures the `WorkingDirectory` parameter
  def branch()(using Git.Command, WorkingDirectory, Tactic[Exec.Error])
    ( using (Git.Event is Loggable)^ )
  :   Git.Branch =
    Git.Branch.unsafe(sh"$git $repoOptions branch --show-current".exec[String]().tt.trim)


  def makeBranch(branch: Git.Branch)
    ( using Git.Command, WorkingDirectory, Tactic[Exec.Error], Tactic[Git.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    sh"$git $repoOptions checkout -b $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(BranchFailed))


  def add[path: Abstractable across Paths to Text](file: path)
    ( using Git.Command, WorkingDirectory, Tactic[Path.Error], Tactic[Name.Error], Tactic[Exec.Error],
            Tactic[Git.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    val relativePath =
      safely(this.path.toward(file.generic.as[Path on Linux])).or:
        abort(Git.Error(AddFailed))

    val command = sh"$git $repoOptions add $relativePath"

    command.exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(AddFailed))


  def reset(mode: ResetMode = ResetMode.Mixed, ref: Refspec = Refspec.head())
    ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    sh"$git $repoOptions reset $mode $ref".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(ResetFailed))


  def unstage[path: Abstractable across Paths to Text](file: path)
    ( using Git.Command, WorkingDirectory, Tactic[Path.Error], Tactic[Name.Error], Tactic[Exec.Error],
            Tactic[Git.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    val relativePath =
      safely(this.path.toward(file.generic.as[Path on Linux])).or:
        abort(Git.Error(ResetFailed))

    sh"$git $repoOptions reset HEAD -- $relativePath".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(ResetFailed))


  def mv
    [ fromPath: Abstractable across Paths to Text,
      toPath:   Abstractable across Paths to Text ]
    ( from: fromPath, to: toPath )
    ( using Git.Command, WorkingDirectory, Tactic[Path.Error], Tactic[Name.Error], Tactic[Exec.Error],
            Tactic[Git.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    val fromRel = safely(this.path.toward(from.generic.as[Path on Linux])).or:
      abort(Git.Error(MvFailed))

    val toRel = safely(this.path.toward(to.generic.as[Path on Linux])).or:
      abort(Git.Error(MvFailed))

    sh"$git $repoOptions mv $fromRel $toRel".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(MvFailed))


  // diff(): worktree vs index. diff(staged = true): index vs HEAD.
  // diff(ref): full tree-vs-ref diff (working tree relative to ref).
  def diff(staged: Boolean = false)
    ( using Git.Command, WorkingDirectory, Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   List[FileDiff] =

    val stagedOpt = if staged then sh"--staged" else sh""
    Patch.parse(sh"$git $repoOptions diff --no-color $stagedOpt".exec[Iterator[Text]]())


  def diff(ref: Refspec)
    ( using Git.Command, WorkingDirectory, Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   List[FileDiff] =

    Patch.parse(sh"$git $repoOptions diff --no-color $ref".exec[Iterator[Text]]())


  def merge
    ( ref: Refspec, ff: FastForward = FastForward.Auto, message: Optional[Text] = Unset )
    ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    val ffOpt = ff match
      case FastForward.Auto  => sh""
      case FastForward.Only  => sh"--ff-only"
      case FastForward.Never => sh"--no-ff"

    val msgOpt = message.lay(sh""): m => sh"-m $m"

    sh"$git $repoOptions merge $ffOpt $msgOpt $ref".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(MergeFailed))


  def cherryPick(commit: Git.Hash)
    ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    sh"$git $repoOptions cherry-pick $commit".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(CherryPickFailed))


  def revert(commit: Git.Hash, noCommit: Boolean = false)
    ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    val noCommitOpt = if noCommit then sh"-n" else sh""

    sh"$git $repoOptions revert --no-edit $noCommitOpt $commit".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(RevertFailed))


  def lock(reason: Optional[Text] = Unset)
    ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    val reasonOpt = reason.lay(sh""): reason => sh"--reason=$reason"

    sh"$git $repoOptions worktree lock $reasonOpt $path".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(WorktreeFailed))


  def unlock()(using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error])
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    sh"$git $repoOptions worktree unlock $path".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(Git.Error(WorktreeFailed))


  def move[path: Abstractable across Paths to Text](newPath: path)
    ( using Git.Command,
            WorkingDirectory,
            Tactic[Git.Error],
            Tactic[Exec.Error],
            ((Path on Linux) is Decodable in Text)^ )
  ( using Tactic[Name.Error], Tactic[Path.Error], (Git.Event is Loggable)^ )
  :   Worktree =

    val target: Path on Linux =
      try newPath.generic.as[Path on Linux]
      catch case error: Path.Error => abort(Git.Error(WorktreeFailed))

    sh"$git $repoOptions worktree move ${this.path} $target".exec[Exit]() match
      case Exit.Ok => Worktree(repo, target)
      case failure => abort(Git.Error(WorktreeFailed))


  def remove(force: Boolean = false)
    ( using Git.Command, WorkingDirectory, Tactic[Git.Error], Tactic[Exec.Error] )
  ( using (Git.Event is Loggable)^ )
  :   Unit =

    repo.removeWorktree(this, force)


  def status(ignored: Boolean = false)(using Git.Command, WorkingDirectory, Tactic[Exec.Error])
  ( using (Git.Event is Loggable)^ )
  :   List[Git.PathStatus] =

    val ignoredParam = if ignored then sh"--ignored" else sh""

    def unescape(text: Text): Text = if text(Prim) != '"' then text else Text.build:
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

    def key(character: Text): Optional[Git.Status] = character match
      case t" " => Unset
      case t"M" => Git.Status.Updated
      case t"A" => Git.Status.Added
      case t"D" => Git.Status.Deleted
      case t"R" => Git.Status.Renamed
      case t"C" => Git.Status.Copied
      case t"U" => Git.Status.Unmerged
      case t"?" => Git.Status.Untracked
      case t"!" => Git.Status.Ignored
      case _    => Unset

    sh"$git $repoOptions status --porcelain $ignoredParam".exec[List[Text]]().bind:
      case r"$key1([ ACDMRU?!])$key2([ ADMU?!]) $path(.*)$path2( -> (.*))?" =>
        val optionalPath = path2.let(_.skip(4)).let(unescape)
        List(Git.PathStatus(key(key1), key(key2), unescape(path), optionalPath))

      case _ =>
        Nil
