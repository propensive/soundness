/*
    Octogenarian, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package octogenarian

import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import enigmatic.*
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
import spectacular.*
import vacuous.*

import pathNavigation.posix

import GitError.Reason.*

object GitRepo:
  def apply[PathType: Abstractable across Paths into Text](path: PathType)
     (using gitError: Tactic[GitError], io: Tactic[IoError])
  :     GitRepo raises PathError raises NameError =

    unsafely(path.generic.decode[Path on Posix]).pipe: path =>
      if !path.exists() then abort(GitError(RepoDoesNotExist))

      if (path / n".git").exists() then GitRepo((path / n".git"), path)
      else new GitRepo(path)

case class GitRepo(gitDir: Path on Posix, workTree: Optional[Path on Posix] = Unset):

  val repoOptions = workTree match
    case Unset          => sh"--git-dir=$gitDir"
    case workTree: Path => sh"--git-dir=$gitDir --work-tree=$workTree"

  @targetName("checkoutTag")
  def checkout(tag: Tag)(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :     Unit logs GitEvent =
    sh"$git $repoOptions checkout $tag".exec[Exit]()

  @targetName("checkoutBranch")
  def checkout(branch: Branch)(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :     Unit logs GitEvent =
    sh"$git $repoOptions checkout $branch".exec[Exit]()

  @targetName("checkoutCommitHash")
  def checkout(commit: CommitHash)(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :     Unit logs GitEvent =
    sh"$git $repoOptions checkout $commit".exec[Exit]()

  def pushTags()(using Internet, Tactic[GitError], GitCommand, WorkingDirectory, Tactic[ExecError])
  :     Unit logs GitEvent =

    sh"$git $repoOptions push --tags".exec[Exit]()

  def push()(using Internet, Tactic[GitError], GitCommand, WorkingDirectory, Tactic[ExecError])
  :     Unit logs GitEvent =

    sh"$git $repoOptions push".exec[Exit]()

  def switch(branch: Branch)
     (using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
  :     Unit logs GitEvent =

    sh"$git $repoOptions switch $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure       => abort(GitError(CannotSwitchBranch))

  def pull()(using GitCommand, Internet, WorkingDirectory)
     (using gitError: Tactic[GitError], exec: Tactic[ExecError])
  :     GitProcess[Unit] logs GitEvent =

    val process = sh"$git $repoOptions pull --progress".fork[Exit]()

    GitProcess[Unit](Git.progress(process)):
      process.await() match
        case Exit.Ok => ()
        case failure       => abort(GitError(PullFailed))

  def fetch(depth: Optional[Int] = Unset, repo: Text, refspec: Refspec)
     (using GitCommand, Internet, WorkingDirectory)
     (using gitError: Tactic[GitError], exec: Tactic[ExecError])
  :     GitProcess[Unit] logs GitEvent /*^{gitError, exec}*/ =

    val depthOption = depth.lay(sh"") { depth => sh"--depth=$depth" }
    val command = sh"$git $repoOptions fetch $depthOption --progress $repo $refspec"
    val process = command.fork[Exit]()

    GitProcess[Unit](Git.progress(process)):
      process.await() match
        case Exit.Ok => ()
        case failure       => abort(GitError(PullFailed))

  def commit(message: Text)(using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
  :     Unit logs GitEvent =

    sh"$git $repoOptions commit -m $message".exec[Exit]() match
      case Exit.Ok => ()
      case failure       => abort(GitError(CommitFailed))

  def branches()(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :     List[Branch] logs GitEvent =
    sh"$git $repoOptions branch"
    . exec[Stream[Text]]()
    . map(_.skip(2))
    . to(List)
    . map(Branch.unsafe(_))

  // FIXME: this uses an `Executor[String]` instead of an `Executor[Text]` because, for some
  // reason, the latter captures the `WorkingDirectory` parameter
  def branch()(using GitCommand, WorkingDirectory, Tactic[ExecError]): Branch logs GitEvent =
    Branch.unsafe(sh"$git $repoOptions branch --show-current".exec[String]().tt)

  def makeBranch(branch: Branch)
     (using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
  :     Unit logs GitEvent =

    sh"$git $repoOptions checkout -b $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure       => abort(GitError(BranchFailed))

  def add[PathType: Abstractable across Paths into Text](path: PathType)
     (using GitCommand, WorkingDirectory, Tactic[ExecError], Tactic[GitError])
  :     Unit logs GitEvent raises PathError raises NameError =

    val relativePath: Relative =
      workTree.let: workTree =>
        safely(path.generic.decode[Path on Posix].relativeTo(workTree)).or:
          abort(GitError(AddFailed))

      . or(abort(GitError(NoWorkTree)))

    val command = sh"$git $repoOptions add $relativePath"

    command.exec[Exit]() match
      case Exit.Ok => ()
      case failure       => abort(GitError(AddFailed))

  def reset(): Unit = ()
  def mv(): Unit = ()

  object config:
    def get[ValueType: Decoder](variable: Text)
       (using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
    :     ValueType logs GitEvent =
      sh"$git $repoOptions config --get $variable".exec[Text]().decode[ValueType]

  def tags()(using GitCommand, WorkingDirectory, Tactic[ExecError]): List[Tag] logs GitEvent =
    sh"$git $repoOptions tag".exec[Stream[Text]]().to(List).map(Tag.unsafe(_))

  def tag(name: Tag)(using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
  :     Tag logs GitEvent =
    sh"$git $repoOptions tag $name".exec[Exit]() match
      case Exit.Ok => name
      case failure       => abort(GitError(TagFailed))

  private def parsePem(text: Text): Optional[Pem] = safely(Pem.parse(text))

  def log()(using GitCommand, WorkingDirectory, Tactic[ExecError]): Stream[Commit] logs GitEvent =
    def recur
       (stream:    Stream[Text],
        hash:     Optional[CommitHash] = Unset,
        tree:     Optional[CommitHash] = Unset,
        parents:   List[CommitHash]     = Nil,
        author:    Optional[Text]       = Unset,
        committer: Optional[Text]       = Unset,
        signature: List[Text]           = Nil,
        lines:    List[Text]           = Nil)
    :     Stream[Commit] =

      def commit(): Stream[Commit] =
        if hash.absent || tree.absent || author.absent || committer.absent then Stream()
        else
          given Unsafe = Unsafe
          val pem = parsePem(signature.join(t"\n"))

          Stream:
            Commit
             (hash.vouch,
              tree.vouch,
              parents.reverse,
              author.vouch,
              committer.vouch,
              pem,
              lines.reverse)

      def read(stream: Stream[Text], lines: List[Text]): (List[Text], Stream[Text]) =
        stream match
          case r" $line(.*)" #:: tail => read(tail, line :: lines)
          case _                      => (lines.reverse, stream)

      stream match
        case head #:: tail => head match
          case t"" =>
            recur(tail, hash, tree, parents, author, committer, signature, lines)

          case r"commit $hash(.{40})" =>
            commit() #::: recur(tail, CommitHash.unsafe(hash), Unset, Nil, Unset, Unset, Nil, Nil)

          case r"tree $tree(.{40})" =>
            recur(tail, hash, CommitHash.unsafe(tree), parents, author, committer, signature, lines)

          case r"parent $parent(.{40})" =>
            val parents2 = CommitHash.unsafe(parent) :: parents
            recur(tail, hash, tree, parents2, author, committer, signature, lines)

          case r"author $author(.*) $timestamp([0-9]+) $time(.....)" =>
            recur(tail, hash, tree, parents, author, committer, signature, lines)

          case r"committer $committer(.*) $timestamp([0-9]+) $time(.....)" =>
            recur(tail, hash, tree, parents, author, committer, signature, lines)

          case r"gpgsig $start(.*)" =>
            val (signature, rest) = read(tail, Nil)
            recur(rest, hash, tree, parents, author, committer, start :: signature, lines)

          case r"    $line(.*)" =>
            recur(tail, hash, tree, parents, author, committer, signature, line :: lines)

          case other =>
            println("ignoring "+other)
            recur(tail, hash, tree, parents, author, committer, signature, lines)

        case _ =>
          commit()

    recur(sh"$git $repoOptions log --format=raw --color=never".exec[Stream[Text]]())

  def reflog(): Unit = ()

  def revParse(refspec: Refspec)(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :     CommitHash logs GitEvent =
    CommitHash.unsafe(sh"$git $repoOptions rev-parse $refspec".exec[Text]())

  def status(ignored: Boolean = false)(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :     List[GitPathStatus] logs GitEvent =
    val ignoredParam = if ignored then sh"--ignored" else sh""

    def unescape(text: Text): Text = if text.at(Prim) != '"' then text else Text.construct:
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

  def diff(): Unit = ()
