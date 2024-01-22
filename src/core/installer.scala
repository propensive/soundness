/*
    Spectral, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package spectral

import anticipation.*, fileApi.galileiApi
import galilei.*, filesystemOptions.{createNonexistent, dereferenceSymlinks, overwritePreexisting, deleteRecursively, createNonexistentParents}
import serpentine.*, hierarchies.unix
import rudiments.*
import vacuous.*
import guillotine.*
import hypotenuse.*
import gossamer.*
import exoskeleton.*
import turbulence.*
import eucalyptus.*
import perforate.*
import spectacular.*
import ambience.*
import fulminate.*

object Installer:
  object Result:
    given Communicable[Result] =
      case AlreadyOnPath(script, path) => msg"The $script command is already installed at $path."
      case Installed(script, path)     => msg"The $script command was installed to $path."
      case PathNotWritable             => msg"No directory on the PATH environment variable was writable"

  enum Result:
    case AlreadyOnPath(script: Text, path: Text)
    case Installed(script: Text, path: Text)
    case PathNotWritable

  def candidateTargets
      ()
      (using service: DaemonService[?])
      (using Log[Text], Environment, HomeDirectory, SystemProperties)
      : List[Directory] raises InstallError =
    mitigate:
      case PathError(_, _)          => InstallError(InstallError.Reason.Environment)
      case EnvironmentError(_)      => InstallError(InstallError.Reason.Environment)
      case SystemPropertyError(_)   => InstallError(InstallError.Reason.Environment)
      case IoError(_)               => InstallError(InstallError.Reason.Io)
      case ExecError(command, _, _) => InstallError(InstallError.Reason.Io)
    .within:
      val paths: List[Path] = Environment.path

      val preferences: List[Path] = List(
        Xdg.bin[Path],
        % / p"usr" / p"local" / p"bin",
        % / p"usr" / p"bin",
        % / p"usr" / p"local" / p"sbin",
        % / p"opt" / p"bin",
        % / p"bin",
        % / p"bin"
      )

      paths.filter(_.exists()).map(_.as[Directory]).filter(_.writable()).sortBy: directory =>
        preferences.indexOf(directory.path) match
          case -1    => Int.MaxValue
          case index => index

  def install
      (force: Boolean = false, target: Optional[Path] = Unset)
      (using service: DaemonService[?], log: Log[Text], environment: Environment, home: HomeDirectory)
      (using Effectful)
      : Result raises InstallError =
    mitigate:
      case PathError(_, _)          => InstallError(InstallError.Reason.Environment)
      case ExecError(command, _, _) => InstallError(InstallError.Reason.Io)
      case StreamError(_)           => InstallError(InstallError.Reason.Io)
      case SystemPropertyError(_)   => InstallError(InstallError.Reason.Environment)
      case EnvironmentError(_)      => InstallError(InstallError.Reason.Environment)
      case IoError(_)               => InstallError(InstallError.Reason.Io)
      case NotFoundError(_)         => InstallError(InstallError.Reason.Io)
      case NumberError(_, _)        => InstallError(InstallError.Reason.Environment)
    .within:
      import workingDirectories.default
      import systemProperties.virtualMachine
      val command: Text = service.scriptName
      val scriptPath = sh"sh -c 'command -v $command'".exec[Text]()

      if safely(scriptPath.decodeAs[Path]) == service.script && !force
      then Result.AlreadyOnPath(command, service.script.show)
      else
        val payloadSize: ByteSize = ByteSize(Properties.spectral.payloadSize[Int]())
        val jarSize: ByteSize = ByteSize(Properties.spectral.jarSize[Int]())
        val scriptFile: File = service.script.as[File]
        val fileSize = scriptFile.size()
        val prefixSize = fileSize - payloadSize - jarSize
        val stream = scriptFile.stream[Bytes]
        val paths: List[Path] = Environment.path
        val installDirectory = target.let(_.as[Directory]).or(candidateTargets().headOption.optional)
        
        val installFile = installDirectory.let: directory =>
          (directory / PathName(command)).make[File]()

        installFile.let: file =>
          Log.info(t"Writing executable to ${file.debug}")
          if prefixSize > 0.b then (stream.take(prefixSize) ++ stream.drop(fileSize - jarSize)).writeTo(file)
          else stream.writeTo(file)
          file.executable() = true
          Result.Installed(command, file.path.show)
        .or:
          Result.PathNotWritable


