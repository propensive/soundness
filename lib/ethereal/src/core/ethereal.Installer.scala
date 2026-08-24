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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import aperture.*
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
import zeppelin.*

import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.overwritePreexisting.enabled

import filesystemBackends.virtualMachineFilesystem

object Installer:
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
    ( using Tactic[Install.Error], (DaemonLogEvent is Loggable)^ )
  :   List[Path on Linux] =

    mitigate:
      case Path.Error(_, _)     => Install.Error(Install.Error.Reason.Environment)
      case Environment.Error(_) => Install.Error(Install.Error.Reason.Environment)
      case Io.Error(_, _, _, _) => Install.Error(Install.Error.Reason.Io)
      case Name.Error(_, _, _)  => Install.Error(Install.Error.Reason.Io)

    . protect:
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

        paths.filter(_.existent()).filter(_.writable()).sort: directory =>
          preferences.stdlib.indexOf(directory) match
            case -1    => Int.MaxValue
            case index => index


  def install(force: Boolean = false, target: Optional[Path on Linux] = Unset)
    ( using service: DaemonService[?], environment: Environment )
    ( using erased effectful: Effectful )
    ( using Diagnostics )
    ( using Tactic[Install.Error], (DaemonLogEvent is Loggable)^ )
  :   Result =

    import workingDirectories.javaWorkingDirectory
    import systems.javaSystem

    mitigate:
      case Path.Error(_, _)      => Install.Error(Install.Error.Reason.Environment)
      case Property.Error(_)     => Install.Error(Install.Error.Reason.Environment)
      case Number.Error(_, _, _) => Install.Error(Install.Error.Reason.Environment)
      case Io.Error(_, _, _, _)  => Install.Error(Install.Error.Reason.Io)
      case Name.Error(_, _, _)   => Install.Error(Install.Error.Reason.Io)
      case guillotine.Exec.Error(_, _, _)   => Install.Error(Install.Error.Reason.Io)
      case Truncation.Error(_)       => Install.Error(Install.Error.Reason.Io)
      case Zip.Error(_)          => Install.Error(Install.Error.Reason.Io)

    . protect:
        val command: Text = service.script
        val scriptPath = mute[guillotine.Exec.Event](sh"sh -c 'command -v $command'".exec[Text]())

        if safely(scriptPath.as[Path on Linux]) == service.executable && !force
        then Result.AlreadyOnPath(command, service.executable.encode)
        else
          val payloadSize: Bytes = Bytes(System.properties.ethereal.payloadSize[Int]())
          val jarSize: Bytes = Bytes(System.properties.ethereal.jarSize[Int]())
          val fileSize = service.executable.filesize()
          val prefixSize = fileSize - payloadSize - jarSize

          val installDirectory: Path on Linux = target.or(candidateTargets().prim).or:
            abort(Install.Error(Install.Error.Reason.Environment))

          val file: Path on Linux = installDirectory/command

          // Write to a hidden sibling and rename it over the target. The rename gives
          // the target a fresh inode, which matters on macOS: its code-signing cache
          // is per-vnode, and overwriting a previously-executed binary's content in
          // place poisons the inode so that every subsequent exec is SIGKILLed. It
          // also means a half-written launcher is never visible at the target path.
          val tempFile: Path on Linux = installDirectory/t".$command.tmp-${Process().pid.value}"
          val installFile: Optional[Path on Linux] = tempFile.create[File](CreateFlag.Replace)

          installFile.let: _ =>
            Log.info(DaemonLogEvent.WriteExecutable(file.inspect))

            // Two sequential opens rather than one nested inside the other's loan (the
            // inner open's evidence would mint fresh roots that cannot unify with the
            // outer handle's; see `Assembler.assemble`). The read is strict, so nothing
            // reads the closed handle.
            val chunks = service.executable.open[File]():
              source ?=> source.reader().stdlib.to(List)

            tempFile.open[File](Write, OpenFlag.Create): target ?=>
              val stream = chunks.stdlib.to(Chain)

              if prefixSize > 0.b
              then target.write(stream.take(prefixSize) #::: stream.drop(fileSize - jarSize))
              else target.write(stream)

            // Excising the embedded payload from between the stub and the JAR moves the JAR
            // earlier by exactly the payload's size, which invalidates the JAR's ZIP64 locator —
            // the one physical offset the format records. See `Zipfile.rebase` and #1680.
            if prefixSize > 0.b && payloadSize > 0.b
            then Zipfile.rebase(tempFile, -payloadSize.long)

            tempFile.executable() = true

            import filesystemOptions.moveAtomically.enabled
            tempFile.moveTo(file)
            Result.Installed(command, file.encode)

          . or(Result.PathNotWritable)
