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
package galilei

import language.experimental.pureFunctions

import java.nio.channels as jnc
import java.nio.file as jnf

import ambience.*
import anticipation.*
import beneficence.*
import contingency.*
import fulminate.*
import guillotine.*
import prepositional.*
import rudiments.*
import serpentine.*

object Creatable:
  given [plane: Filesystem]
  =>  ( createNonexistentParents: CreateNonexistentParents on plane,
        overwritePreexisting:     OverwritePreexisting on plane,
        tactic:                   Tactic[IoError] )
  =>  ( (Directory is Creatable on plane to (Path on plane))^ ) =

    new Creatable:
      type Self = Directory
      type Result = Path on Plane
      type Plane = plane

      def create(path: Path on Plane): Path on Plane =
        createNonexistentParents(path):
          overwritePreexisting(path):
            jnf.Files.createDirectory(jnf.Path.of(path.encode.s).nn)
            path


  given socket: [plane <: Posix: Filesystem]
  =>  ( createNonexistentParents: CreateNonexistentParents on plane,
        overwritePreexisting:     OverwritePreexisting on plane,
        tactic:                   Tactic[IoError] )
  =>  ( (Socket is Creatable to Socket)^ ) =

    new Creatable:
      type Plane = plane
      type Self = Socket
      type Result = Socket

      def create(path: Path on Plane): Result =
        createNonexistentParents(path):
          overwritePreexisting(path):
            val address = java.net.UnixDomainSocketAddress.of(path.javaPath).nn
            val channel = jnc.ServerSocketChannel.open(java.net.StandardProtocolFamily.UNIX).nn
            channel.bind(address)
            Socket(channel)


  given file: [plane: Filesystem]
  =>  ( createNonexistentParents: CreateNonexistentParents on plane,
        overwritePreexisting:     OverwritePreexisting on plane,
        tactic:                   Tactic[IoError] )
  =>  ( (File is Creatable on plane to (Path on plane))^ ) =

    new Creatable:
      type Plane = plane
      type Self = File
      type Result = Path on Plane

      def create(path: Path on Plane): Path on Plane = path.also:
        createNonexistentParents(path):
          overwritePreexisting(path):
            jnf.Files.createFile(path.javaPath)


  given fifo: [plane: Filesystem]
  =>  ( createNonexistentParents: CreateNonexistentParents on plane,
        overwritePreexisting:     OverwritePreexisting on plane,
        working:                  WorkingDirectory,
        tactic:                   Tactic[IoError],
        loggable:                 ExecEvent is Loggable )
  =>  ( (Fifo is Creatable to (Path on plane))^ ) =

    new Creatable:
      type Self = Fifo
      type Result = Path on Plane
      type Plane = plane

      def create(path: Path on Plane): Path on Plane = path.also:
        createNonexistentParents(path):
          overwritePreexisting(path):
            mitigate:
              case ExecError(_, _, _) =>
                import errorDiagnostics.stackTracesDiagnostics
                IoError(path, IoError.Operation.Create, IoError.Reason.Unsupported)

            . protect:
                sh"mkfifo $path"() match
                  case Exit.Ok => ()

                  case _ =>
                    raise
                      ( IoError(path, IoError.Operation.Create, IoError.Reason.PermissionDenied) )

trait Creatable extends Findable, Resultant, Planar:
  type Self
  def create(path: Path on Plane): Result
