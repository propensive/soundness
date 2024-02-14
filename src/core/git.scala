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

import anticipation.*, fileApi.galileiApi
import eucalyptus.*
import fulminate.*
import contingency.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import gastronomy.*
import gossamer.*
import guillotine.*
import nettlesome.*
import kaleidoscope.*
import rudiments.*
import vacuous.*
import serpentine.*, hierarchies.unix
import spectacular.*
import turbulence.*

//import language.experimental.captureChecking

import GitError.Detail.*

object GitError:
  enum Detail:
    case CannotExecuteGit, CloneFailed, InvalidRepoPath, RepoDoesNotExist, BranchDoesNotExist,
        CommitDoesNotExist, CommitFailed, CannotSwitchBranch, PullFailed, BranchFailed, TagFailed
  
  given Communicable[Detail] =
    case CannotExecuteGit   => msg"the `git` command could not be executed"
    case CloneFailed        => msg"the repository could not be cloned"
    case InvalidRepoPath    => msg"the repository path was not valid"
    case RepoDoesNotExist   => msg"the repository does not exist"
    case BranchDoesNotExist => msg"the branch does not exist"
    case CommitDoesNotExist => msg"the commit does not exist"
    case CommitFailed       => msg"the commit could not be created"
    case PullFailed         => msg"the pull operation did not complete"
    case BranchFailed       => msg"the new branch could not be created"
    case TagFailed          => msg"the new tag could not be created"
    case CannotSwitchBranch => msg"the branch could not be changed"


case class GitError(detail: GitError.Detail)
extends Error(msg"the Git operation could not be completed because $detail")

case class GitRefError(value: Text) extends Error(msg"$value is not a valid Git reference")

class GitProcess[+ResultType](val progress: LazyList[Progress])(closure: => ResultType):
  lazy val result: ResultType/*^{closure*}*/ = closure
  def complete(): ResultType/*^{closure*}*/ = result

object GitRepo:
  def apply
      [PathType: GenericPath]
      (path: PathType)
      (using gitError: Raises[GitError], io: Raises[IoError])
      : GitRepo =
    
    val path2 = SpecificPath(path.pathText)
    if !path2.exists() then abort(GitError(RepoDoesNotExist))
    
    if (path2 / p".git").exists() then GitRepo((path2 / p".git").as[Directory], path2.as[Directory])
    else GitRepo(path2.as[Directory])

case class GitRepo(gitDir: Directory, workTree: Optional[Directory] = Unset):

  val repoOptions = workTree match
    case Unset               => sh"--git-dir=${gitDir.path}"
    case workTree: Directory => sh"--git-dir=${gitDir.path} --work-tree=${workTree.path}"

  @targetName("checkoutTag")
  def checkout(tag: Tag)(using Log[Text], GitCommand, WorkingDirectory, Raises[ExecError]): Unit =
      sh"$git $repoOptions checkout $tag".exec[ExitStatus]()
  
  @targetName("checkoutBranch")
  def checkout(branch: Branch)(using Log[Text], GitCommand, WorkingDirectory, Raises[ExecError]): Unit =
      sh"$git $repoOptions checkout $branch".exec[ExitStatus]()
  
  @targetName("checkoutCommitHash")
  def checkout(commit: CommitHash)(using Log[Text], GitCommand, WorkingDirectory, Raises[ExecError]): Unit =
      sh"$git $repoOptions checkout $commit".exec[ExitStatus]()
  
  def pushTags()(using Log[Text], Internet, Raises[GitError], GitCommand, WorkingDirectory, Raises[ExecError]): Unit =
    sh"$git $repoOptions push --tags".exec[ExitStatus]()

  def switch(branch: Branch)(using GitCommand, Log[Text], WorkingDirectory, Raises[GitError], Raises[ExecError]): Unit =
    sh"$git $repoOptions switch $branch".exec[ExitStatus]() match
      case ExitStatus.Ok => ()
      case failure       => abort(GitError(CannotSwitchBranch))
  
  def pull
      ()(using GitCommand, Log[Text], Internet, WorkingDirectory)(using gitError: Raises[GitError], exec: Raises[ExecError])
      : GitProcess[Unit]/*^{gitError, exec}*/ =
    
    val process = sh"$git $repoOptions pull --progress".fork[ExitStatus]()

    GitProcess[Unit](Git.progress(process)):
      process.await() match
        case ExitStatus.Ok => ()
        case failure       => abort(GitError(PullFailed))
  
  def fetch
      (depth: Optional[Int] = Unset, repo: Text, refspec: Refspec)
      (using GitCommand, Log[Text], Internet, WorkingDirectory)(using gitError: Raises[GitError], exec: Raises[ExecError])
      : GitProcess[Unit]/*^{gitError, exec}*/ =
    
    val depthOption = depth.lay(sh"") { depth => sh"--depth=$depth" }
    val command = sh"$git $repoOptions fetch $depthOption --progress $repo $refspec"
    val process = command.fork[ExitStatus]()

    GitProcess[Unit](Git.progress(process)):
      process.await() match
        case ExitStatus.Ok => ()
        case failure       => abort(GitError(PullFailed))
  
  def commit(message: Text)(using GitCommand, Log[Text], WorkingDirectory, Raises[GitError], Raises[ExecError]): Unit =
    sh"$git $repoOptions commit -m $message".exec[ExitStatus]() match
      case ExitStatus.Ok => ()
      case failure       => abort(GitError(CommitFailed))
  
  def branches()(using GitCommand, WorkingDirectory, Log[Text], Raises[ExecError]): List[Branch] =
    sh"$git $repoOptions branch".exec[LazyList[Text]]().map(_.drop(2)).to(List).map(Branch.unsafe(_))
  
  // FIXME: this uses an `Executor[String]` instead of an `Executor[Text]` because, for some
  // reason, the latter captures the `WorkingDirectory` parameter
  def branch()(using GitCommand, WorkingDirectory, Log[Text], Raises[ExecError]): Branch =
    Branch.unsafe(sh"$git $repoOptions branch --show-current".exec[String]().tt)
  
  def makeBranch
      (branch: Branch)
      (using GitCommand, WorkingDirectory, Log[Text], Raises[GitError], Raises[ExecError])
      : Unit =

    sh"$git $repoOptions checkout -b $branch".exec[ExitStatus]() match
      case ExitStatus.Ok => ()
      case failure       => abort(GitError(BranchFailed))
  
  def add(): Unit = ()
  def reset(): Unit = ()
  def mv(): Unit = ()
  
  def tags()(using GitCommand, WorkingDirectory, Log[Text], Raises[ExecError]): List[Tag] =
    sh"$git $repoOptions tag".exec[LazyList[Text]]().to(List).map(Tag.unsafe(_))

  def tag(name: Tag)(using GitCommand, WorkingDirectory, Log[Text], Raises[GitError], Raises[ExecError]): Tag =
    sh"$git $repoOptions tag $name".exec[ExitStatus]() match
      case ExitStatus.Ok => name
      case failure       => abort(GitError(TagFailed))
  
  private def parsePem(text: Text): Optional[Pem] = safely(Pem.parse(text))

  def log()(using GitCommand, WorkingDirectory, Log[Text], Raises[ExecError]): LazyList[Commit] =
    def recur
        (stream: LazyList[Text], hash: Optional[CommitHash] = Unset, tree: Optional[CommitHash] = Unset,
            parents: List[CommitHash] = Nil, author: Optional[Text] = Unset,
            committer: Optional[Text] = Unset, signature: List[Text] = Nil, lines: List[Text] = Nil)
        : LazyList[Commit] =
      
      def commit(): LazyList[Commit] =
        if hash.absent || tree.absent || author.absent || committer.absent then LazyList()
        else
          given Unsafe = Unsafe
          val pem = parsePem(signature.join(t"\n"))
          
          LazyList(Commit(hash.vouch, tree.vouch, parents.reverse, author.vouch, committer.vouch, pem,
            lines.reverse))
      
      def read(stream: LazyList[Text], lines: List[Text]): (List[Text], LazyList[Text]) =
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
  def status(): Unit = ()
    
  def diff(): Unit = ()

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
      process.stderr().map(_.uString).map(_.trim).flatMap(_.cut(r"[\n\r]")).collect:
        case r"Receiving objects: *${pc}([0-9]*)\%.*"            => Progress.Receiving(pc.s.toInt/100.0)
        case r"Resolving deltas: *${pc}([0-9]+)\%.*"             => Progress.Resolving(pc.s.toInt/100.0)
        case r"Unpacking objects: *${pc}([0-9]+)\%.*"            => Progress.Unpacking(pc.s.toInt/100.0)
        case r"remote: *Compressing objects: *${pc}([0-9]+)\%.*"  => Progress.RemoteCompressing(pc.s.toInt/100.0)
        case r"remote: *Counting objects: *${pc}([0-9]+)\%.*"     => Progress.RemoteCounting(pc.s.toInt/100.0)
    .or(LazyList()).deduplicate

  def init
      [PathType: GenericPath]
      (targetPath: PathType, bare: Boolean = false)
      (using Log[Text], WorkingDirectory, Raises[GitError], Decoder[Path], Raises[ExecError])(using command: GitCommand)
      : GitRepo =
    try
      throwErrors[PathError | IoError]:
        val bareOpt = if bare then sh"--bare" else sh""
        val target: Path = targetPath.pathText.decodeAs[Path]
        sh"$command init $bareOpt $target".exec[ExitStatus]()
        
        if bare then GitRepo(target.as[Directory], Unset)
        else GitRepo((target / p".git").as[Directory], target.as[Directory])

    catch
      case error: PathError => abort(GitError(InvalidRepoPath))
      case error: IoError   => abort(GitError(InvalidRepoPath))

  inline def cloneCommit
      [SourceType, PathType: GenericPath]
      (source: SourceType, targetPath: PathType, commit: CommitHash)
      (using Internet, Decoder[Path], GitCommand, Raises[GitError], Raises[ExecError], Log[Text],
          WorkingDirectory)
      : GitProcess[GitRepo] =
    val sourceText = inline source match
      case source: SshUrl => source.text
      case other          => summonFrom:
        case genericUrl: GenericUrl[SourceType] => genericUrl.text(source)
        case given GenericPath[SourceType]      => source.pathText
    
    uncheckedCloneCommit(sourceText, targetPath, commit)

  inline def clone
      [SourceType, PathType: GenericPath]
      (source: SourceType, targetPath: PathType, bare: Boolean = false, branch: Optional[Branch] = Unset,
          recursive: Boolean = false)
      (using Internet, WorkingDirectory, Log[Text], Decoder[Path], Raises[ExecError], GitCommand)(using gitError: Raises[GitError])
      : GitProcess[GitRepo] =
    val sourceText = inline source match
      case source: SshUrl => source.text
      case other          => summonFrom:
        case genericUrl: GenericUrl[SourceType] => genericUrl.text(source)
        case given GenericPath[SourceType]      => source.pathText
    
    uncheckedClone(sourceText, targetPath, bare, branch, recursive)
    
  private def uncheckedCloneCommit
      [PathType: GenericPath]
      (source: Text, targetPath: PathType, commit: CommitHash)
      (using Internet, Decoder[Path], GitCommand)(using gitError: Raises[GitError], exec: Raises[ExecError], log: Log[Text], workingDirectory: WorkingDirectory)
      : GitProcess[GitRepo]/*^{gitError, log, workingDirectory, exec}*/ =
    
    val gitRepo = init(targetPath)
    
    val fetch = gitRepo.fetch(1, source, commit)
    
    GitProcess(fetch.progress):
      fetch.complete()
      gitRepo.checkout(commit)
      gitRepo

  private def uncheckedClone
      [PathType: GenericPath]
      (source: Text, targetPath: PathType, bare: Boolean = false, branch: Optional[Branch] = Unset,
          recursive: Boolean = false)
      (using Internet, WorkingDirectory, Log[Text], Decoder[Path], Raises[ExecError], GitCommand)(using gitError: Raises[GitError])
      : GitProcess[GitRepo]/*^{gitError}*/ =
    
    val target: Path = try targetPath.pathText.decodeAs[Path] catch case error: PathError => abort(GitError(InvalidRepoPath))
    
    val bareOption = if bare then sh"--bare" else sh""
    val branchOption = branch.lay(sh"") { branch => sh"--branch=$branch" }
    val recursiveOption = if recursive then sh"--recursive" else sh""
    
    val process = sh"$git clone --progress $bareOption $branchOption $recursiveOption $source $target".fork[ExitStatus]()
    
    GitProcess[GitRepo](progress(process)):
      process.await() match
        case ExitStatus.Ok =>
          try throwErrors[IoError](GitRepo((target / p".git").as[Directory], target.as[Directory]))
          catch case error: IoError => abort(GitError(CloneFailed))
        
        case _ =>
          abort(GitError(CloneFailed))


object Octogenarian:
  opaque type Refspec = Text
  opaque type Tag <: Refspec = Text
  opaque type Branch <: Refspec = Text
  opaque type CommitHash <: Refspec = Text

  object Refspec:
    def parse(text: Text)(using Raises[GitRefError]): Text =
      text.cut(t"/").each: part =>
        if part.starts(t".") || part.ends(t".") then raise(GitRefError(text))(text)
        if part.ends(t".lock") then raise(GitRefError(text))(text)
        if part.contains(t"@{") then raise(GitRefError(text))(text)
        if part.contains(t"..") then raise(GitRefError(text))(text)
        if part.length == 0 then raise(GitRefError(text))(text)

        for ch <- List('*', '[', '\\', ' ', '^', '~', ':', '?')
        do if part.contains(ch) then raise(GitRefError(text))(text)

      text
    
    given Encoder[Refspec] = identity(_)
    given Show[Refspec] = identity(_)

  object Tag:
    def unsafe(text: Text): Tag = text
    def apply(text: Text)(using Raises[GitRefError]): Tag = Refspec.parse(text)
    given encoder: Encoder[Tag] = identity(_)
    given decoder(using Raises[GitRefError]): Decoder[Tag] = apply(_)
    given show: Show[Tag] = identity(_)

  object Branch:
    def unsafe(text: Text): Branch = text
    def apply(text: Text)(using Raises[GitRefError]): Branch = Refspec.parse(text)
    given encoder: Encoder[Branch] = identity(_)
    given decoder(using Raises[GitRefError]): Decoder[Branch] = apply(_)
    given show: Show[Branch] = identity(_)

  object CommitHash:
    def apply(text: Text)(using Raises[GitRefError]): CommitHash = text match
      case r"[a-f0-9]{40}" => text
      case _               => raise(GitRefError(text))(text)
    
    def unsafe(text: Text): CommitHash = text
    
    given encoder: Encoder[CommitHash] = identity(_)
    given decoder(using Raises[GitRefError]): Decoder[CommitHash] = apply(_)
    given show: Show[CommitHash] = identity(_)

export Octogenarian.{Tag, Branch, CommitHash, Refspec}

object GitCommand:
  given Parameterizable[GitCommand] = _.file.path.fullname

case class GitCommand(file: File)

case class Commit
    (commit: CommitHash, tree: CommitHash, parent: List[CommitHash], author: Text, committer: Text,
        signature: Optional[Pem], message: List[Text])

package gitCommands:
  given environmentDefault(using WorkingDirectory, Raises[PathError], Log[Text], Raises[IoError], Raises[ExecError]): GitCommand =
    val path: Path = sh"which git"()

    GitCommand(path.as[File])

case class SshUrl(user: Optional[Text], hostname: Hostname, path: Text):
  def userText = user.lay(t"") { user => t"$user:" }
  t"$userText@$hostname$path"