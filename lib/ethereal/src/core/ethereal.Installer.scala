                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package ethereal

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
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
import symbolism.*
import turbulence.*
import vacuous.*

import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled

object Installer:
  private given realm: Realm = realm"et"

  object Result:
    given communicable: Result is Communicable =
      case AlreadyOnPath(script, path) => m"The $script command is already installed at $path."
      case Installed(script, path)     => m"The $script command was installed to $path."

      case PathNotWritable =>
        m"No directory on the PATH environment variable was writable"

  enum Result:
    case AlreadyOnPath(script: Text, path: Text)
    case Installed(script: Text, path: Text)
    case PathNotWritable


  def candidateTargets()(using service: DaemonService[?], diagnostics: Diagnostics)
    ( using Environment, System )
  :   List[Path on Linux] logs DaemonLogEvent raises InstallError =

    mitigate:
      case PathError(_, _)     => InstallError(InstallError.Reason.Environment)
      case EnvironmentError(_) => InstallError(InstallError.Reason.Environment)
      case IoError(_, _, _, _) => InstallError(InstallError.Reason.Io)
      case NameError(_, _, _)  => InstallError(InstallError.Reason.Io)

    . within:
        val paths: List[Path on Linux] = Environment.path

        val preferences: List[Path on Linux] =
          List
            ( Xdg.bin[Path on Linux],
              % / "usr" / "local" / "bin",
              % / "usr" / "bin",
              % / "usr" / "local" / "sbin",
              % / "opt" / "bin",
              % / "bin",
              % / "bin" )

        paths.filter(_.exists()).filter(_.writable()).sortBy: directory =>
          preferences.indexOf(directory) match
            case -1    => Int.MaxValue
            case index => index


  def install(force: Boolean = false, target: Optional[Path on Linux] = Unset)
    ( using service: DaemonService[?], environment: Environment )
    ( using erased Effectful )
    ( using Diagnostics )
  :   Result logs DaemonLogEvent raises InstallError =

    import workingDirectories.java
    import systems.java

    mitigate:
      case PathError(_, _)     => InstallError(InstallError.Reason.Environment)
      case PropertyError(_)    => InstallError(InstallError.Reason.Environment)
      case NumberError(_, _, _) => InstallError(InstallError.Reason.Environment)
      case IoError(_, _, _, _) => InstallError(InstallError.Reason.Io)
      case NameError(_, _, _)  => InstallError(InstallError.Reason.Io)
      case ExecError(_, _, _)  => InstallError(InstallError.Reason.Io)
      case StreamError(_)      => InstallError(InstallError.Reason.Io)

    . within:
        val command: Text = service.script
        val scriptPath = mute[ExecEvent](sh"sh -c 'command -v $command'".exec[Text]())

        if safely(scriptPath.decode[Path on Linux]) == service.executable && !force
        then Result.AlreadyOnPath(command, service.executable.encode)
        else
          val payloadSize: Bytes = Bytes(System.properties.ethereal.payloadSize[Int]())
          val jarSize: Bytes = Bytes(System.properties.ethereal.jarSize[Int]())
          val fileSize = service.executable.size()
          val prefixSize = fileSize - payloadSize - jarSize

          val installDirectory: Path on Linux = target.or(candidateTargets().prim).or:
            abort(InstallError(InstallError.Reason.Environment))

          val file: Path on Linux = installDirectory/command
          val installFile: Optional[Path on Linux] = file.create[File]()

          installFile.let: file =>
            val filename: Text = file.inspect
            Log.info(DaemonLogEvent.WriteExecutable(filename))

            service.executable.open: file =>
              val stream = file.stream[Data]

              if prefixSize > 0.b
              then (stream.take(prefixSize) ++ stream.discard(fileSize - jarSize)).writeTo(file)
              else stream.writeTo(file)

            file.executable() = true
            Result.Installed(command, file.encode)

          . or(Result.PathNotWritable)
