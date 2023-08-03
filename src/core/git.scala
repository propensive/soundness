package nonagenarian

import anticipation.*, fileApi.galileiApi
import eucalyptus.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import gossamer.*
import guillotine.*
import kaleidoscope.*
import rudiments.*
import serpentine.*, hierarchies.unix
import spectacular.*
import turbulence.*

import language.experimental.captureChecking

object GitError:
  enum Detail:
    case CannotExecuteGit, CloneFailed, InvalidRepoPath, RepoDoesNotExist, BranchDoesNotExist,
        CommitDoesNotExist, CommitFailed, CannotSwitchBranch, PullFailed
  
  given AsMessage[Detail] =
    case Detail.CannotExecuteGit   => msg"the `git` command could not be executed"
    case Detail.CloneFailed        => msg"the repository could not be cloned"
    case Detail.InvalidRepoPath    => msg"the repository path was not valid"
    case Detail.RepoDoesNotExist   => msg"the repository does not exist"
    case Detail.BranchDoesNotExist => msg"the branch does not exist"
    case Detail.CommitDoesNotExist => msg"the commit does not exist"
    case Detail.CommitFailed       => msg"the commit could not be created"
    case Detail.PullFailed         => msg"the pull operation did not complete"
    case Detail.CannotSwitchBranch => msg"the branch could not be changed"

case class GitError(detail: GitError.Detail)
extends Error(msg"the Git operation could not be completed because $detail")

case class CloneProcess(complete: () => GitRepo, progress: LazyList[Progress])

object GitRepo:
  def apply(path: Path): GitRepo throws IoError | GitError =
    if !path.exists() then throw GitError(GitError.Detail.RepoDoesNotExist)
    if (path / p".git").exists() then GitRepo((path / p".git").as[Directory], path.as[Directory])
    else GitRepo(path.as[Directory])

case class GitRepo(gitDir: Directory, workTree: Maybe[Directory] = Unset):

  val repo = workTree match
    case Unset               => sh"--git-dir=${gitDir.path}"
    case workTree: Directory => sh"--git-dir=${gitDir.path} --work-tree=${workTree.path}"

  def checkout(ref: Text)(using Log, GitCommand, WorkingDirectory): Unit =
    sh"$git $repo checkout $ref".exec[ExitStatus]()
  
  def push()(using Log, Internet, CanThrow[GitError], GitCommand, WorkingDirectory): Unit =
    sh"$git $repo push".exec[ExitStatus]()

  def switch(branch: Branch)(using GitCommand, Log, WorkingDirectory, CanThrow[GitError]): Unit =
    sh"$git $repo switch $branch".exec[ExitStatus]() match
      case ExitStatus.Ok => ()
      case failure       => throw GitError(GitError.Detail.CannotSwitchBranch)
  
  def pull()(using GitCommand, Log, Internet, WorkingDirectory, CanThrow[GitError]): Unit =
    sh"$git $repo pull".exec[ExitStatus]() match
      case ExitStatus.Ok => ()
      case failure       => throw GitError(GitError.Detail.PullFailed)
  
  def commit(message: Text)(using GitCommand, Log, WorkingDirectory, CanThrow[GitError]): Unit =
    sh"$git $repo commit -m $message".exec[ExitStatus]() match
      case ExitStatus.Ok => ()
      case failure       => throw GitError(GitError.Detail.CommitFailed)
  
  def branches()(using GitCommand, WorkingDirectory, Log): List[Branch] =
    sh"$git $repo branch".exec[LazyList[Text]]().map(_.drop(2)).to(List).map(Branch(_))
  
  def add(): Unit = ()
  def reset(): Unit = ()
  def fetch(): Unit = ()
  def mv(): Unit = ()
  def tags()(using GitCommand, WorkingDirectory, Log): List[Tag] =
    sh"$git $repo tag".exec[LazyList[Text]]().to(List).map(Tag(_))

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
          LazyList(Commit(hash.avow, tree.avow, parents.reverse, author.avow, committer.avow,
            signature, lines.reverse))
      
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
    
    recur(sh"$git $repo log --format=raw --color=never".exec[LazyList[Text]]())

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
      case error: PathError           => throw GitError(GitError.Detail.InvalidRepoPath)
      case error: IoError             => throw GitError(GitError.Detail.InvalidRepoPath)
  
  def clone
      [PathType: GenericPathReader]
      (source: Text, targetPath: PathType)
      (using Internet, WorkingDirectory, Log, Decoder[Path], CanThrow[GitError], GitCommand)
      : CloneProcess =
    
    try
      val target: Path = targetPath.fullPath.decodeAs[Path]
      val process = sh"$git clone --progress $source $target".fork[ExitStatus]()
      
      def complete(): GitRepo = process.await() match
        case ExitStatus.Ok =>
          try GitRepo((target / p".git").as[Directory], target.as[Directory])
          catch case error: IoError => throw GitError(GitError.Detail.CloneFailed)
        
        case _ =>
          throw GitError(GitError.Detail.CloneFailed)
  
      CloneProcess(() => complete(), progress(process))

    catch
      case error: PathError           => throw GitError(GitError.Detail.InvalidRepoPath)
      case error: IoError             => throw GitError(GitError.Detail.InvalidRepoPath)

object GitRef:
  given Encoder[GitRef] = _.name

trait GitRef:
  def name: Text

case class Tag(name: Text) extends GitRef
case class Branch(name: Text) extends GitRef
case class CommitHash(name: Text) extends GitRef

object GitCommand:
  given AsParams[GitCommand] = _.file.path.fullname

case class GitCommand(file: File)

case class Commit
    (commit: CommitHash, tree: CommitHash, parent: List[CommitHash], author: Text, committer: Text,
        signature: List[Text], message: List[Text])

package gitCommands:
  given environmentDefault(using WorkingDirectory, CanThrow[PathError], Log, CanThrow[IoError]): GitCommand =
    val path: Path = sh"which git"()

    GitCommand(path.as[File])