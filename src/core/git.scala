package nonagenarian

import anticipation.*, fileApi.galileiApi
import eucalyptus.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import gossamer.*
import guillotine.*
import kaleidoscope.*
import ambience.*
import rudiments.*, workingDirectory.jvm
import serpentine.*, hierarchies.unix
import spectacular.*
import turbulence.*

import language.experimental.captureChecking

object GitError:
  enum Detail:
    case CannotExecuteGit, CloneFailed, InvalidRepoPath, RepoDoesNotExist, BranchDoesNotExist,
        CommitDoesNotExist
  
  given AsMessage[Detail] =
    case Detail.CannotExecuteGit   => msg"the `git` command could not be executed"
    case Detail.CloneFailed        => msg"the repository could not be cloned"
    case Detail.InvalidRepoPath    => msg"the repository path was not valid"
    case Detail.RepoDoesNotExist   => msg"the repository does not exist"
    case Detail.BranchDoesNotExist => msg"the branch does not exist"
    case Detail.CommitDoesNotExist => msg"the commit does not exist"

case class GitError(detail: GitError.Detail)
extends Error(msg"the Git operation could not be completed because $detail")

case class CloneProcess(complete: () => GitRepo, progress: LazyList[Progress])

object GitRepo:
  def apply(path: Path): GitRepo throws IoError | GitError =
    if !path.exists() then throw GitError(GitError.Detail.RepoDoesNotExist)
    if (path / p".git").exists() then GitRepo((path / p".git").as[Directory], path.as[Directory])
    else GitRepo(path.as[Directory])

case class GitRepo(gitDir: Directory, workTree: Maybe[Directory] = Unset):

  val targetParams = workTree match
    case Unset               => sh"--git-dir=${gitDir.path}"
    case workTree: Directory => sh"--git-dir=${gitDir.path} --work-tree=${workTree.path}"

  def checkout(ref: Text)(using Log, GitCommand, WorkingDirectory): Unit =
    sh"$git $targetParams checkout $ref".exec[ExitStatus]()
  
  def push()(using Log, WorkingDirectory, Internet, CanThrow[GitError], GitCommand): Unit =
    sh"$git $targetParams push"

  def switch(branch: Branch)(using GitCommand, Log, WorkingDirectory): Unit =
    sh"$git $targetParams switch $branch"
  
  def pull(): Unit = ()
  def commit(): Unit = ()
  def add(): Unit = ()
  def branch(): Unit = ()
  def reset(): Unit = ()
  def fetch(): Unit = ()
  def rm(): Unit = ()
  def mv(): Unit = ()
  def switch(): Unit = ()
  def tag(): Unit = ()
  def log(): Unit = ()
  def reflog(): Unit = ()
  def status(): Unit = ()
    
  def diff(): Unit = ()

enum Progress:
  case Receiving(complete: Double)
  case Resolving(complete: Double)

private[nonagenarian] inline def git(using command: GitCommand): GitCommand = command

object Git:
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
      
      val progress =
        try process.stderr().map(_.uString).map(_.trim).collect:
          case r"Receiving objects: *${As[Int](pc)}([0-9]*)\%.*" => Progress.Receiving(pc/100.0)
          case r"Resolving deltas: *${As[Int](pc)}([0-9]+)\%.*"  => Progress.Resolving(pc/100.0)
        catch case error: StreamCutError => LazyList()
  
      def complete(): GitRepo = process.await() match
        case ExitStatus.Ok =>
          try GitRepo((target / p".git").as[Directory], target.as[Directory])
          catch case error: IoError => throw GitError(GitError.Detail.CloneFailed)
        
        case _ =>
          throw GitError(GitError.Detail.CloneFailed)
  
      CloneProcess(() => complete(), progress)

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
        gpgsig: Text, message: List[Text])

package gitCommands:
  given environmentDefault(using working: WorkingDirectory, path: CanThrow[PathError], log: Log, io: CanThrow[IoError]): GitCommand^{working, path, log, io} =
    val path: Path = sh"which git"()

    GitCommand(path.as[File])