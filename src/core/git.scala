/*
    Nonagenarian, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package nonagenarian

import anticipation.*, fileApi.galileiApi
import digression.*
import eucalyptus.*
import fulminate.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import gastronomy.*
import gossamer.*
import guillotine.*
import kaleidoscope.*
import rudiments.*
import serpentine.*, hierarchies.unix
import spectacular.*
import turbulence.*

import language.experimental.captureChecking

import GitError.Detail.*

object GitError:
  enum Detail:
    case CannotExecuteGit, CloneFailed, InvalidRepoPath, RepoDoesNotExist, BranchDoesNotExist,
        CommitDoesNotExist, CommitFailed, CannotSwitchBranch, PullFailed, BranchFailed, TagFailed
  
  given AsMessage[Detail] =
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

object GitProcess:
  def apply
      [ResultType]
      (progress: LazyList[Progress])
      (fn: CanThrow[GitError] ?-> ResultType)
      : GitProcess[ResultType] =
    
    GitProcess(() => fn, progress)

case class GitProcess[+ResultType](complete: () -> (CanThrow[GitError]) ?-> ResultType, progress: LazyList[Progress]):
  def map[ResultType2](fn: (CanThrow[GitError]) ?-> ResultType => ResultType2): GitProcess[ResultType2] =
    GitProcess[ResultType2](progress)(fn(complete()))

object GitRepo:
  def apply(path: Path): GitRepo throws IoError | GitError =
    if !path.exists() then throw GitError(RepoDoesNotExist)
    if (path / p".git").exists() then GitRepo((path / p".git").as[Directory], path.as[Directory])
    else GitRepo(path.as[Directory])

case class GitRepo(gitDir: Directory, workTree: Maybe[Directory] = Unset):

  val repoOptions = workTree match
    case Unset               => sh"--git-dir=${gitDir.path}"
    case workTree: Directory => sh"--git-dir=${gitDir.path} --work-tree=${workTree.path}"

  def checkout(refspec: RefSpec)(using Log, GitCommand, WorkingDirectory): Unit =
    sh"$git $repoOptions checkout $refspec".exec[ExitStatus]()
  
  def pushTags()(using Log, Internet, CanThrow[GitError], GitCommand, WorkingDirectory): Unit =
    sh"$git $repoOptions push --tags".exec[ExitStatus]()

  def switch(branch: Branch)(using GitCommand, Log, WorkingDirectory, CanThrow[GitError]): Unit =
    sh"$git $repoOptions switch $branch".exec[ExitStatus]() match
      case ExitStatus.Ok => ()
      case failure       => throw GitError(CannotSwitchBranch)
  
  def pull()(using git: GitCommand, log: Log, internet: Internet, working: WorkingDirectory, gitError: CanThrow[GitError]): GitProcess[Unit] =
    val process = sh"$git $repoOptions pull --progress".fork[ExitStatus]()

    GitProcess[Unit](Git.progress(process)):
      process.await() match
        case ExitStatus.Ok => ()
        case failure       => throw GitError(PullFailed)
  
  def fetch
      (depth: Maybe[Int] = Unset, repo: Text, refspec: RefSpec)
      (using GitCommand, Log, Internet, WorkingDirectory, CanThrow[GitError])
      : GitProcess[Unit] =
    
    val depthOption = depth.fm(sh"") { depth => sh"--depth=$depth" }
    val command = sh"$git $repoOptions fetch $depthOption --progress $repo $refspec"
    val process = command.fork[ExitStatus]()

    GitProcess[Unit](Git.progress(process)):
      process.await() match
        case ExitStatus.Ok => ()
        case failure       => throw GitError(PullFailed)
  
  def commit(message: Text)(using GitCommand, Log, WorkingDirectory, CanThrow[GitError]): Unit =
    sh"$git $repoOptions commit -m $message".exec[ExitStatus]() match
      case ExitStatus.Ok => ()
      case failure       => throw GitError(CommitFailed)
  
  def branches()(using GitCommand, WorkingDirectory, Log): List[Branch] =
    sh"$git $repoOptions branch".exec[LazyList[Text]]().map(_.drop(2)).to(List).map(Branch(_))
  
  // FIXME: this uses an `Executor[String]` instead of an `Executor[Text]` because, for some
  // reason, the latter captures the `WorkingDirectory` parameter
  def branch()(using GitCommand, WorkingDirectory, Log): Branch =
    Branch(sh"$git $repoOptions branch --show-current".exec[String]().tt)
  
  def makeBranch
      (branch: Branch)
      (using GitCommand, WorkingDirectory, Log, CanThrow[GitError])
      : Unit =

    sh"$git $repoOptions checkout -b $branch".exec[ExitStatus]() match
      case ExitStatus.Ok => ()
      case failure       => throw GitError(BranchFailed)
  
  def add(): Unit = ()
  def reset(): Unit = ()
  def mv(): Unit = ()
  
  def tags()(using GitCommand, WorkingDirectory, Log): List[Tag] =
    sh"$git $repoOptions tag".exec[LazyList[Text]]().to(List).map(Tag(_))

  def tag(name: Tag)(using GitCommand, WorkingDirectory, Log, CanThrow[GitError]): Tag =
    sh"$git $repoOptions tag $name".exec[ExitStatus]() match
      case ExitStatus.Ok => name
      case failure       => throw GitError(TagFailed)
  
  private def parsePem(text: Text): Maybe[Pem] = safely(Pem.parse(text))

  def log()(using GitCommand, WorkingDirectory, Log): LazyList[Commit] =
    def recur
        (stream: LazyList[Text], hash: Maybe[CommitHash] = Unset, tree: Maybe[CommitHash] = Unset,
            parents: List[CommitHash] = Nil, author: Maybe[Text] = Unset,
            committer: Maybe[Text] = Unset, signature: List[Text] = Nil, lines: List[Text] = Nil)
        : LazyList[Commit] =
      
      def commit(): LazyList[Commit] =
        if hash.unset || tree.unset || author.unset || committer.unset then LazyList()
        else
          given Unsafe.type = Unsafe
          val pem = parsePem(signature.join(t"\n"))
          
          LazyList(Commit(hash.avow, tree.avow, parents.reverse, author.avow, committer.avow, pem,
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
            commit() #::: recur(tail, CommitHash(hash), Unset, Nil, Unset, Unset, Nil, Nil)
          
          case r"tree $tree(.{40})" =>
            recur(tail, hash, CommitHash(tree), parents, author, committer, signature, lines)
          
          case r"parent $parent(.{40})" =>
            recur(tail, hash, tree, CommitHash(parent) :: parents, author, committer, signature, lines)

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

private[nonagenarian] inline def git(using command: GitCommand): GitCommand = command

object Git:
  def progress(process: Process[?, ?]): LazyList[Progress] =
    try process.stderr().map(_.uString).map(_.trim).collect:
      case r"Receiving objects: *${As[Int](pc)}([0-9]*)\%.*" => Progress.Receiving(pc/100.0)
      case r"Resolving deltas: *${As[Int](pc)}([0-9]+)\%.*"  => Progress.Resolving(pc/100.0)
    catch case error: StreamCutError => LazyList()

  def init
      [PathType: GenericPathReader]
      (targetPath: PathType, bare: Boolean = false)
      (using Log, WorkingDirectory, CanThrow[GitError])
      (using command: GitCommand, decoder: Decoder[Path])
      : GitRepo =
    try
      val bareOpt = if bare then sh"--bare" else sh""
      val target: Path = targetPath.fullPath.decodeAs[Path]
      sh"$command init $bareOpt $target".exec[ExitStatus]()
      
      if bare then GitRepo(target.as[Directory], Unset)
      else GitRepo((target / p".git").as[Directory], target.as[Directory])
    catch
      case error: PathError => throw GitError(InvalidRepoPath)
      case error: IoError   => throw GitError(InvalidRepoPath)
  
  def cloneCommit
      [PathType: GenericPathReader]
      (repo: Text, targetPath: PathType, commit: CommitHash)
      (using Internet, WorkingDirectory, Log, Decoder[Path], CanThrow[GitError], GitCommand)
      : GitProcess[GitRepo] =
    
    val gitRepo = init(targetPath)
    
    gitRepo.fetch(1, repo, commit).map: _ =>
      gitRepo.checkout(commit)
      gitRepo

  def clone
      [PathType: GenericPathReader]
      (source: Text, targetPath: PathType, bare: Boolean = false, branch: Maybe[Branch] = Unset,
          recursive: Boolean = false)
      (using Internet, WorkingDirectory, Log, Decoder[Path], CanThrow[GitError], GitCommand)
      : GitProcess[GitRepo] =
    
    try
      val target: Path = targetPath.fullPath.decodeAs[Path]
      val bareOption = if bare then sh"--bare" else sh""
      val branchOption = branch.fm(sh"") { branch => sh"--branch=$branch" }
      val recursiveOption = if recursive then sh"--recursive" else sh""
      
      val process = sh"$git clone --progress $bareOption $branchOption $recursiveOption $source $target".fork[ExitStatus]()
      
      def complete()(using CanThrow[GitError]): GitRepo = process.await() match
        case ExitStatus.Ok =>
          try GitRepo((target / p".git").as[Directory], target.as[Directory])
          catch case error: IoError => throw GitError(CloneFailed)
        
        case _ =>
          throw GitError(CloneFailed)
  
      GitProcess[GitRepo](() => complete(), progress(process))

    catch
      case error: PathError => throw GitError(InvalidRepoPath)
      case error: IoError   => throw GitError(InvalidRepoPath)

object RefSpec:
  given Encoder[RefSpec] = _.name

trait RefSpec:
  def name: Text

case class Tag(name: Text) extends RefSpec
case class Branch(name: Text) extends RefSpec
case class CommitHash(name: Text) extends RefSpec

object GitCommand:
  given AsParams[GitCommand] = _.file.path.fullname

case class GitCommand(file: File)

case class Commit
    (commit: CommitHash, tree: CommitHash, parent: List[CommitHash], author: Text, committer: Text,
        signature: Maybe[Pem], message: List[Text])

package gitCommands:
  given environmentDefault(using WorkingDirectory, CanThrow[PathError], Log, CanThrow[IoError]): GitCommand =
    val path: Path = sh"which git"()

    GitCommand(path.as[File])
