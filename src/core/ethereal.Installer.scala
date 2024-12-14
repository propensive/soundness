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

import ambience.*
import anticipation.*
import contingency.*
import eucalyptus.*
import exoskeleton.*
import fulminate.*
import galilei.*
import gossamer.*
import guillotine.*
import hypotenuse.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled

import pathNavigation.linux

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

  def candidateTargets()(using service: DaemonService[?], diagnostics: Diagnostics)
     (using Environment, HomeDirectory, SystemProperties)
          : List[Path on Linux] logs DaemonLogEvent raises InstallError =
    tend:
      case PathError(_, _)     => InstallError(InstallError.Reason.Environment)
      case EnvironmentError(_) => InstallError(InstallError.Reason.Environment)
      case IoError(_, _, _)    => InstallError(InstallError.Reason.Io)
      case NameError(_, _, _)  => InstallError(InstallError.Reason.Io)

    . within:
        val paths: List[Path on Linux] = Environment.path

        val preferences: List[Path on Linux] = List
         (Xdg.bin[Path on Linux],
          % / n"usr" / n"local" / n"bin",
          % / n"usr" / n"bin",
          % / n"usr" / n"local" / n"sbin",
          % / n"opt" / n"bin",
          % / n"bin",
          % / n"bin")

        paths.filter(_.exists()).filter(_.writable()).sortBy: directory =>
          preferences.indexOf(directory) match
            case -1    => Int.MaxValue
            case index => index

  def install(force: Boolean = false, target: Optional[Path on Linux] = Unset)
     (using service: DaemonService[?], environment: Environment, home: HomeDirectory)
     (using Effectful, Diagnostics)
        : Result logs DaemonLogEvent raises InstallError =
    import workingDirectories.default
    import systemProperties.virtualMachine

    tend:
      case PathError(_, _)        => InstallError(InstallError.Reason.Environment)
      case SystemPropertyError(_) => InstallError(InstallError.Reason.Environment)
      case NumberError(_, _)      => InstallError(InstallError.Reason.Environment)
      case IoError(_, _, _)       => InstallError(InstallError.Reason.Io)
      case NameError(_, _, _)     => InstallError(InstallError.Reason.Io)
      case ExecError(_, _, _)     => InstallError(InstallError.Reason.Io)
      case StreamError(_)         => InstallError(InstallError.Reason.Io)

    . within:
        val command: Text = service.scriptName
        val scriptPath = mute[ExecEvent](sh"sh -c 'command -v $command'".exec[Text]())

        if safely(scriptPath.decode[Path on Linux]) == service.script && !force
        then Result.AlreadyOnPath(command, service.script.text)
        else
          val payloadSize: Memory = Memory(Properties.ethereal.payloadSize[Int]())
          val jarSize: Memory = Memory(Properties.ethereal.jarSize[Int]())
          val scriptFile: Path on Linux = service.script
          val fileSize = scriptFile.size()
          val prefixSize = fileSize - payloadSize - jarSize
          val installDirectory: Path on Linux = target.or(candidateTargets().prim).or:
            abort(InstallError(InstallError.Reason.Environment))

          val installFile: Optional[Path on Linux] = installDirectory.let: directory =>
            (directory / Name(command)).make[File]().on[Linux]

          installFile.let: file =>
            val filename: Text = file.inspect
            Log.info(DaemonLogEvent.WriteExecutable(filename))

            scriptFile.open: file =>
              val stream = file.stream[Bytes]

              if prefixSize > 0.b
              then (stream.take(prefixSize) ++ stream.discard(fileSize - jarSize)).writeTo(file)
              else stream.writeTo(file)

            file.executable() = true
            Result.Installed(command, file.text)

          . or:
              Result.PathNotWritable
