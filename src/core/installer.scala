/*
    Ethereal, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package ethereal

import anticipation.*, filesystemApi.galileiPath
import galilei.*, filesystemOptions.{createNonexistent, dereferenceSymlinks, overwritePreexisting, deleteRecursively, createNonexistentParents}
import serpentine.*, pathHierarchies.unix
import rudiments.*
import vacuous.*
import guillotine.*
import profanity.*
import hypotenuse.*
import gossamer.*
import exoskeleton.*
import eucalyptus.*
import turbulence.*
import contingency.*
import spectacular.*
import ambience.*
import fulminate.*

enum DaemonLogEvent:
  case WriteExecutable(location: Text)
  case Shutdown
  case Termination
  case Failure
  case NewCli
  case UnrecognizedMessage
  case ReceivedSignal(signal: Signal)
  case ExitStatusRequest(pid: Pid)
  case CloseConnection(pid: Pid)
  case StderrRequest(pid: Pid)
  case Init(pid: Pid)

object DaemonLogEvent:
  given DaemonLogEvent is Communicable =
    case WriteExecutable(location) => m"Writing executable to $location"
    case Shutdown                  => m"Shutting down"
    case Termination               => m"Terminating client connection"
    case Failure                   => m"A failure occurred"
    case NewCli                    => m"Instantiating a new CLI"
    case UnrecognizedMessage       => m"Unrecognized message"
    case ReceivedSignal(signal)    => m"Received signal $signal"
    case ExitStatusRequest(pid)    => m"Exit status requested from $pid"
    case CloseConnection(pid)      => m"Connection closed from $pid"
    case StderrRequest(pid)        => m"STDERR requested from $pid"
    case Init(pid)                 => m"Initializing $pid"

object Installer:
  given Realm = realm"ethereal"

  object Result:
    given Result is Communicable =
      case AlreadyOnPath(script, path) => m"The $script command is already installed at $path."
      case Installed(script, path)     => m"The $script command was installed to $path."
      case PathNotWritable             => m"No directory on the PATH environment variable was writable"

  enum Result:
    case AlreadyOnPath(script: Text, path: Text)
    case Installed(script: Text, path: Text)
    case PathNotWritable

  def candidateTargets()(using service: DaemonService[?])
      (using Environment, HomeDirectory, SystemProperties)
          : List[Directory] logs DaemonLogEvent raises InstallError =
    tend:
      case PathError(_, _)     => InstallError(InstallError.Reason.Environment)
      case EnvironmentError(_) => InstallError(InstallError.Reason.Environment)
      case IoError(_)          => InstallError(InstallError.Reason.Io)
    .within:
      val paths: List[Unix.Path] = Environment.path

      val preferences: List[Unix.Path] = List
       (Xdg.bin[Unix.Path],
        % / p"usr" / p"local" / p"bin",
        % / p"usr" / p"bin",
        % / p"usr" / p"local" / p"sbin",
        % / p"opt" / p"bin",
        % / p"bin",
        % / p"bin")

      paths.filter(_.exists()).map(_.as[Directory]).filter(_.writable()).sortBy: directory =>
        preferences.indexOf(directory.path) match
          case -1    => Int.MaxValue
          case index => index


  def install(force: Boolean = false, target: Optional[Unix.Path] = Unset)
      (using service: DaemonService[?], environment: Environment, home: HomeDirectory)
      (using Effectful)
        : Result logs DaemonLogEvent raises InstallError =
    import workingDirectories.default
    import systemProperties.virtualMachine

    tend:
      case PathError(_, _)        => InstallError(InstallError.Reason.Environment)
      case SystemPropertyError(_) => InstallError(InstallError.Reason.Environment)
      case NumberError(_, _)      => InstallError(InstallError.Reason.Environment)
      case IoError(_)             => InstallError(InstallError.Reason.Io)
      case ExecError(_, _, _)     => InstallError(InstallError.Reason.Io)
      case StreamError(_)         => InstallError(InstallError.Reason.Io)
    .within:
      val command: Text = service.scriptName
      val scriptPath = mute[ExecEvent](sh"sh -c 'command -v $command'".exec[Text]())

      if safely(scriptPath.decodeAs[Unix.Path]) == service.script && !force
      then Result.AlreadyOnPath(command, service.script.show)
      else
        val payloadSize: ByteSize = ByteSize(Properties.ethereal.payloadSize[Int]())
        val jarSize: ByteSize = ByteSize(Properties.ethereal.jarSize[Int]())
        val scriptFile: File = service.script.as[File]
        val fileSize = scriptFile.size()
        val prefixSize = fileSize - payloadSize - jarSize
        val stream = scriptFile.stream[Bytes]
        val installDirectory = target.let(_.as[Directory]).or(candidateTargets().prim)

        val installFile = installDirectory.let: directory =>
          (directory / Name(command)).make[File]()

        installFile.let: file =>
          val filename: Text = file.inspect
          Log.info(DaemonLogEvent.WriteExecutable(filename))
          if prefixSize > 0.b then (stream.take(prefixSize) ++ stream.skip(fileSize - jarSize)).writeTo(file)
          else stream.writeTo(file)
          file.executable() = true
          Result.Installed(command, file.path.show)
        .or:
          Result.PathNotWritable
