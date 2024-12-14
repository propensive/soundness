/*
    Octogenarian, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import anticipation.*, filesystemApi.serpentinePath
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
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

import pathNavigation.posix

import language.experimental.pureFunctions

given Realm = realm"octogenarian"

import GitError.Detail.*

object GitError:
  enum Detail:
    case CannotExecuteGit, CloneFailed, InvalidRepoPath, RepoDoesNotExist, BranchDoesNotExist,
        CommitDoesNotExist, CommitFailed, CannotSwitchBranch, PullFailed, BranchFailed, TagFailed,
        AddFailed, NoWorkTree

  given Detail is Communicable =
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

case class GitError(detail: GitError.Detail)(using Diagnostics)
extends Error(m"the Git operation could not be completed because $detail")

case class GitRefError(value: Text)(using Diagnostics)
extends Error(m"$value is not a valid Git reference")

class GitProcess[+ResultType](val progress: LazyList[Progress])(closure: => ResultType):
  lazy val result: ResultType/*^{closure*}*/ = closure
  def complete(): ResultType/*^{closure*}*/ = result

object GitRepo:
  def apply[PathType: GenericPath](path: PathType)(using gitError: Tactic[GitError], io: Tactic[IoError])
          : GitRepo raises PathError raises NameError =

    unsafely(path.pathText.decode[Path on Posix]).pipe: path =>
      if !path.exists() then abort(GitError(RepoDoesNotExist))

      if (path / n".git").exists() then GitRepo((path / n".git"), path)
      else new GitRepo(path)

case class GitRepo(gitDir: Path on Posix, workTree: Optional[Path on Posix] = Unset):

  val repoOptions = workTree match
    case Unset          => sh"--git-dir=$gitDir"
    case workTree: Path => sh"--git-dir=$gitDir --work-tree=$workTree"

  @targetName("checkoutTag")
  def checkout(tag: Tag)(using GitCommand, WorkingDirectory, Tactic[ExecError]): Unit logs GitEvent =
    sh"$git $repoOptions checkout $tag".exec[Exit]()

  @targetName("checkoutBranch")
  def checkout(branch: Branch)(using GitCommand, WorkingDirectory, Tactic[ExecError]): Unit logs GitEvent =
    sh"$git $repoOptions checkout $branch".exec[Exit]()

  @targetName("checkoutCommitHash")
  def checkout(commit: CommitHash)(using GitCommand, WorkingDirectory, Tactic[ExecError]): Unit logs GitEvent =
    sh"$git $repoOptions checkout $commit".exec[Exit]()

  def pushTags()(using Internet, Tactic[GitError], GitCommand, WorkingDirectory, Tactic[ExecError])
          : Unit logs GitEvent =

    sh"$git $repoOptions push --tags".exec[Exit]()

  def push()(using Internet, Tactic[GitError], GitCommand, WorkingDirectory, Tactic[ExecError])
          : Unit logs GitEvent =

    sh"$git $repoOptions push".exec[Exit]()

  def switch(branch: Branch)(using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
          : Unit logs GitEvent =

    sh"$git $repoOptions switch $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure       => abort(GitError(CannotSwitchBranch))

  def pull()(using GitCommand, Internet, WorkingDirectory)
     (using gitError: Tactic[GitError], exec: Tactic[ExecError])
          : GitProcess[Unit] logs GitEvent =

    val process = sh"$git $repoOptions pull --progress".fork[Exit]()

    GitProcess[Unit](Git.progress(process)):
      process.await() match
        case Exit.Ok => ()
        case failure       => abort(GitError(PullFailed))

  def fetch(depth: Optional[Int] = Unset, repo: Text, refspec: Refspec)
     (using GitCommand, Internet, WorkingDirectory)
     (using gitError: Tactic[GitError], exec: Tactic[ExecError])
          : GitProcess[Unit] logs GitEvent /*^{gitError, exec}*/ =

    val depthOption = depth.lay(sh"") { depth => sh"--depth=$depth" }
    val command = sh"$git $repoOptions fetch $depthOption --progress $repo $refspec"
    val process = command.fork[Exit]()

    GitProcess[Unit](Git.progress(process)):
      process.await() match
        case Exit.Ok => ()
        case failure       => abort(GitError(PullFailed))

  def commit(message: Text)(using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
          : Unit logs GitEvent =

    sh"$git $repoOptions commit -m $message".exec[Exit]() match
      case Exit.Ok => ()
      case failure       => abort(GitError(CommitFailed))

  def branches()(using GitCommand, WorkingDirectory, Tactic[ExecError]): List[Branch] logs GitEvent =
    sh"$git $repoOptions branch".exec[LazyList[Text]]().map(_.skip(2)).to(List).map(Branch.unsafe(_))

  // FIXME: this uses an `Executor[String]` instead of an `Executor[Text]` because, for some
  // reason, the latter captures the `WorkingDirectory` parameter
  def branch()(using GitCommand, WorkingDirectory, Tactic[ExecError]): Branch logs GitEvent =
    Branch.unsafe(sh"$git $repoOptions branch --show-current".exec[String]().tt)

  def makeBranch(branch: Branch)
     (using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
          : Unit logs GitEvent =

    sh"$git $repoOptions checkout -b $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure       => abort(GitError(BranchFailed))

  def add[PathType: GenericPath](path: PathType)
     (using GitCommand, WorkingDirectory, Tactic[ExecError], Tactic[GitError])
          : Unit logs GitEvent raises PathError raises NameError =

    val relativePath: Relative =
      workTree.let: workTree =>
        safely(path.pathText.decode[Path on Posix].relativeTo(workTree)).or:
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
            : ValueType logs GitEvent =
      sh"$git $repoOptions config --get $variable".exec[Text]().decode[ValueType]

  def tags()(using GitCommand, WorkingDirectory, Tactic[ExecError]): List[Tag] logs GitEvent =
    sh"$git $repoOptions tag".exec[LazyList[Text]]().to(List).map(Tag.unsafe(_))

  def tag(name: Tag)(using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError]): Tag logs GitEvent =
    sh"$git $repoOptions tag $name".exec[Exit]() match
      case Exit.Ok => name
      case failure       => abort(GitError(TagFailed))

  private def parsePem(text: Text): Optional[Pem] = safely(Pem.parse(text))

  def log()(using GitCommand, WorkingDirectory, Tactic[ExecError]): LazyList[Commit] logs GitEvent =
    def recur
       (stream:    LazyList[Text],
        hash:      Optional[CommitHash] = Unset,
        tree:      Optional[CommitHash] = Unset,
        parents:   List[CommitHash]     = Nil,
        author:    Optional[Text]       = Unset,
        committer: Optional[Text]       = Unset,
        signature: List[Text]           = Nil,
        lines:     List[Text]           = Nil)
            : LazyList[Commit] =

      def commit(): LazyList[Commit] =
        if hash.absent || tree.absent || author.absent || committer.absent then LazyList()
        else
          given Unsafe = Unsafe
          val pem = parsePem(signature.join(t"\n"))

          LazyList:
            Commit(hash.vouch, tree.vouch, parents.reverse, author.vouch, committer.vouch, pem, lines.reverse)

      def read(stream: LazyList[Text], lines: List[Text]): (List[Text], LazyList[Text]) = stream match
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
            recur(tail, hash, tree, CommitHash.unsafe(parent) :: parents, author, committer, signature, lines)

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

    recur(sh"$git $repoOptions log --format=raw --color=never".exec[LazyList[Text]]())

  def reflog(): Unit = ()

  def revParse(refspec: Refspec)(using GitCommand, WorkingDirectory, Tactic[ExecError]): CommitHash logs GitEvent =
    CommitHash.unsafe(sh"$git $repoOptions rev-parse $refspec".exec[Text]())

  def status(ignored: Boolean = false)(using GitCommand, WorkingDirectory, Tactic[ExecError]): List[GitPathStatus] logs GitEvent =
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

enum GitStatus:
  case Updated, Added, Deleted, Renamed, Copied, Unmerged, Untracked, Ignored

case class GitPathStatus(status1: Optional[GitStatus], status2: Optional[GitStatus], path1: Text, path2: Optional[Text])

enum Progress:
  case Receiving(complete: Double)
  case Resolving(complete: Double)
  case Unpacking(complete: Double)
  case RemoteCounting(complete: Double)
  case RemoteCompressing(complete: Double)

private[octogenarian] inline def git(using command: GitCommand): GitCommand = command

object Git:
  def progress(process: Process[?, ?]): LazyList[Progress] =
    safely[StreamError]:
      process.stderr().map(_.utf8).map(_.trim).flatMap(_.cut(r"[\n\r]")).collect:
        case r"Receiving objects: *${pc}([0-9]*)\%.*"            => Progress.Receiving(pc.s.toInt/100.0)
        case r"Resolving deltas: *${pc}([0-9]+)\%.*"             => Progress.Resolving(pc.s.toInt/100.0)
        case r"Unpacking objects: *${pc}([0-9]+)\%.*"            => Progress.Unpacking(pc.s.toInt/100.0)
        case r"remote: *Compressing objects: *${pc}([0-9]+)\%.*" => Progress.RemoteCompressing(pc.s.toInt/100.0)
        case r"remote: *Counting objects: *${pc}([0-9]+)\%.*"    => Progress.RemoteCounting(pc.s.toInt/100.0)

    . or(LazyList()).deduplicate

  def init
     [PathType: GenericPath]
     (targetPath: PathType, bare: Boolean = false)
     (using WorkingDirectory, Tactic[GitError], Decoder[Path on Posix], Tactic[ExecError])
     (using command: GitCommand)
          : GitRepo logs GitEvent raises NameError =
    try
      throwErrors[PathError | IoError]:
        val bareOpt = if bare then sh"--bare" else sh""
        val target: Path on Posix = targetPath.pathText.decode[Path on Posix]
        sh"$command init $bareOpt $target".exec[Exit]()

        if bare then GitRepo(target, Unset) else GitRepo((target / n".git"), target)

    catch
      case error: PathError => abort(GitError(InvalidRepoPath))
      case error: IoError   => abort(GitError(InvalidRepoPath))

  inline def cloneCommit[SourceType <: Matchable, PathType: GenericPath]
     (source: SourceType, targetPath: PathType, commit: CommitHash)
     (using Internet,
            Decoder[Path on Posix],
            GitCommand,
            Tactic[GitError],
            Tactic[ExecError],
            WorkingDirectory)
          : GitProcess[GitRepo] logs GitEvent raises NameError =

    val sourceText = inline source match
      case source: SshUrl => source.text
      case other          => summonFrom:
        case genericUrl: (SourceType is GenericUrl) => genericUrl.text(source)
        case given (SourceType is GenericPath)      => source.pathText

    uncheckedCloneCommit(sourceText, targetPath, commit)

  inline def clone[SourceType <: Matchable, PathType: GenericPath]
     (source:     SourceType,
      targetPath: PathType,
      bare:       Boolean          = false,
      branch:     Optional[Branch] = Unset,
      recursive:  Boolean          = false)
     (using Internet, WorkingDirectory, Decoder[Path on Posix], Tactic[ExecError], GitCommand)
     (using gitError: Tactic[GitError])
          : GitProcess[GitRepo] logs GitEvent raises PathError raises NameError =

    val sourceText = inline source match
      case source: SshUrl => source.text
      case other          => summonFrom:
        case genericUrl: (SourceType is GenericUrl) => genericUrl.text(source)
        case given (SourceType is GenericPath)      => source.pathText

    uncheckedClone(sourceText, targetPath, bare, branch, recursive)

  private def uncheckedCloneCommit[PathType: GenericPath]
     (source: Text, targetPath: PathType, commit: CommitHash)
     (using Internet, Decoder[Path on Posix], GitCommand)
     (using gitError:         Tactic[GitError],
            exec:             Tactic[ExecError],
            workingDirectory: WorkingDirectory)
          : GitProcess[GitRepo] logs GitEvent raises NameError =

    val gitRepo = init(targetPath)
    val fetch = gitRepo.fetch(1, source, commit)

    GitProcess(fetch.progress):
      fetch.complete()
      gitRepo.checkout(commit)
      gitRepo

  private def uncheckedClone[PathType: GenericPath]
     (source:     Text,
      targetPath: PathType,
      bare:       Boolean          = false,
      branch:     Optional[Branch] = Unset,
      recursive:  Boolean          = false)
     (using Internet, WorkingDirectory, Decoder[Path on Posix], Tactic[ExecError], GitCommand)
     (using gitError: Tactic[GitError])
          : GitProcess[GitRepo] logs GitEvent raises PathError raises NameError =

    val target: Path on Posix =
      try targetPath.pathText.decode[Path on Posix] catch case error: PathError => abort(GitError(InvalidRepoPath))

    val bareOption = if bare then sh"--bare" else sh""
    val branchOption = branch.lay(sh"") { branch => sh"--branch=$branch" }
    val recursiveOption = if recursive then sh"--recursive" else sh""

    val process =
      sh"$git clone --progress $bareOption $branchOption $recursiveOption $source $target".fork[Exit]()

    GitProcess[GitRepo](progress(process)):
      process.await() match
        case Exit.Ok =>
          try throwErrors[IoError](GitRepo((target / n".git"), target))
          catch case error: IoError => abort(GitError(CloneFailed))

        case _ =>
          abort(GitError(CloneFailed))


object Octogenarian:
  opaque type Refspec = Text
  opaque type Tag <: Refspec = Text
  opaque type Branch <: Refspec = Text
  opaque type CommitHash <: Refspec = Text

  object Refspec:
    def head(n: Int = 0): Refspec = t"HEAD~$n"

    def parse(text: Text)(using Tactic[GitRefError]): Text =
      text.cut(t"/").each: part =>
        if part.starts(t".") || part.ends(t".") then raise(GitRefError(text), text)
        if part.ends(t".lock") then raise(GitRefError(text), text)
        if part.contains(t"@{") then raise(GitRefError(text), text)
        if part.contains(t"..") then raise(GitRefError(text), text)
        if part.length == 0 then raise(GitRefError(text), text)

        for char <- List('*', '[', '\\', ' ', '^', '~', ':', '?')
        do if part.contains(char) then raise(GitRefError(text), text)

      text

    given Refspec is Encodable in Text as encodable = identity(_)
    given Refspec is Showable = identity(_)

  object Tag:
    def unsafe(text: Text): Tag = text
    def apply(text: Text)(using Tactic[GitRefError]): Tag = Refspec.parse(text)
    given decoder(using Tactic[GitRefError]): Decoder[Tag] = apply(_)
    given Tag is Showable = identity(_)

  object Branch:
    def unsafe(text: Text): Branch = text
    def apply(text: Text)(using Tactic[GitRefError]): Branch = Refspec.parse(text)
    given decoder(using Tactic[GitRefError]): Decoder[Branch] = apply(_)
    given Branch is Showable = identity(_)

  object CommitHash:
    def apply(text: Text)(using Tactic[GitRefError]): CommitHash = text match
      case r"[a-f0-9]{40}" => text
      case _               => raise(GitRefError(text), text)

    def unsafe(text: Text): CommitHash = text

    given decoder(using Tactic[GitRefError]): Decoder[CommitHash] = apply(_)
    given CommitHash is Showable = identity(_)

export Octogenarian.{Tag, Branch, CommitHash, Refspec}

object GitCommand:
  given GitCommand is Parameterizable = _.path.text

case class GitCommand(path: Path)

case class Commit
   (commit: CommitHash,
    tree: CommitHash,
    parent: List[CommitHash],
    author: Text,
    committer: Text,
    signature: Optional[Pem],
    message: List[Text])

package gitCommands:
  given environmentDefault
     (using WorkingDirectory, Tactic[NameError], Tactic[PathError], Tactic[IoError], Tactic[ExecError], GitEvent is Loggable)
          : GitCommand =

    val path: Path on Posix = sh"which git"()
    GitCommand(path)

case class SshUrl(user: Optional[Text], hostname: Hostname, path: Text):
  def text: Text =
    def userText = user.lay(t"") { user => t"$user:" }
    t"$userText@$hostname$path"

object GitEvent:
  given GitEvent transcribes ExecEvent = GitEvent.Exec(_)

  given GitEvent is Communicable =
    case Exec(reason) => m"The Git operation did not execute successfully: $reason"

enum GitEvent:
  case Exec(event: ExecEvent)
