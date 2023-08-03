package nonagenarian

import ambience.*, systemProperties.jvm
import anticipation.*
import eucalyptus.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import gossamer.*
import guillotine.*
import kaleidoscope.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*

import language.experimental.captureChecking

object GitError:
  enum Detail:
    case GitNotOnPath
    case CloneFailed
    case InvalidRepoPath
  
  given AsMessage[Detail] =
    case Detail.GitNotOnPath    => msg"the `git` executable was not on the path"
    case Detail.CloneFailed     => msg"the `git` executable was not on the path"
    case Detail.InvalidRepoPath => msg"the repository path was not valid"

case class GitError(detail: GitError.Detail)
extends Error(msg"the Git operation could not be completed because $detail")

case class CloneProcess(complete: () => GitRepo, progress: LazyList[Progress])

case class GitRepo(directory: Directory, workTree: Maybe[Directory]):
  def checkout(ref: Text)(using Log): Unit throws GitError =
    try sh"git checkout $ref".exec[ExitStatus]()
    catch
      case error: SystemPropertyError => throw GitError(GitError.Detail.GitNotOnPath)
  
  def push()(using Log, Internet): Unit throws GitError =
    try sh"git --base-dir=${directory.path} push"
    catch
      case error: SystemPropertyError => throw GitError(GitError.Detail.GitNotOnPath)
      case error: SystemPropertyError => throw GitError(GitError.Detail.GitNotOnPath)

  
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

object Git:

  def init
      [PathType: GenericPathReader]
      (targetPath: PathType, bare: Boolean = false)
      (using log: Log, git: CanThrow[GitError], decoder: Decoder[Path])
      : GitRepo =
    try
      val bareOpt = if bare then sh"--bare" else sh""
      val target: Path = targetPath.fullPath.decodeAs[Path]
      sh"git init $bareOpt $target".exec[ExitStatus]()
      
      if bare then GitRepo(target.as[Directory], Unset)
      else GitRepo((target / p".git").as[Directory], target.as[Directory])
    catch
      case error: PathError           => throw GitError(GitError.Detail.InvalidRepoPath)
      case error: SystemPropertyError => throw GitError(GitError.Detail.GitNotOnPath)
      case error: IoError             => throw GitError(GitError.Detail.InvalidRepoPath)
  
  def clone
      [PathType: GenericPathReader]
      (source: Text, targetPath: PathType)
      (using Internet, Log, Decoder[Path], CanThrow[GitError])
      : CloneProcess =
    
    try
      val target: Path = targetPath.fullPath.decodeAs[Path]
      val process = sh"git clone --progress $source $target".fork[ExitStatus]()
      
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
      case error: SystemPropertyError => throw GitError(GitError.Detail.GitNotOnPath)
      case error: IoError             => throw GitError(GitError.Detail.InvalidRepoPath)

trait GitRef:
  def name: Text

case class GitTag(name: Text) extends GitRef
case class GitBranch(name: Text) extends GitRef

case class GitCommit(bytes: Bytes) extends GitRef:
  def name: Text = bytes.hex