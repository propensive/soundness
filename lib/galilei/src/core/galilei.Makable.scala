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

import ambience.*
import anticipation.*
import contingency.*
import fulminate.*
import guillotine.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*

import java.nio.file as jnf
import java.nio.channels as jnc

import language.experimental.pureFunctions

object Makable:
  given [plane: Filesystem]
  =>  ( createNonexistentParents: CreateNonexistentParents on plane,
        overwritePreexisting:     OverwritePreexisting on plane,
        tactic:                   Tactic[IoError] )
  =>  Directory is Makable on plane to (Path on plane) =

      new Makable:
        type Self = Directory
        type Result = Path on Plane
        type Plane = plane

        def make(path: Path on Plane): Path on Plane =
          createNonexistentParents(path):
            overwritePreexisting(path):
              jnf.Files.createDirectory(jnf.Path.of(path.encode.s).nn)
              path

  given socket: [plane <: Posix: Filesystem]
  =>  ( createNonexistentParents: CreateNonexistentParents on plane,
        overwritePreexisting:     OverwritePreexisting on plane,
        tactic:                   Tactic[IoError] )
  =>  Socket is Makable to Socket =

      new Makable:
        type Plane = plane
        type Self = Socket
        type Result = Socket

        def make(path: Path on Plane): Result =
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
  =>  File is Makable on plane to (Path on plane) =

      new Makable:
        type Plane = plane
        type Self = File
        type Result = Path on Plane

        def make(path: Path on Plane): Path on Plane = path.also:
          createNonexistentParents(path):
            overwritePreexisting(path):
              jnf.Files.createFile(path.javaPath)


  given fifo: [plane: Filesystem]
  =>  ( createNonexistentParents: CreateNonexistentParents on plane,
        overwritePreexisting:     OverwritePreexisting on plane,
        working:                  WorkingDirectory,
        tactic:                   Tactic[IoError],
        loggable:                 ExecEvent is Loggable )
  =>  Fifo is Makable to (Path on plane) =

      new Makable:
        type Self = Fifo
        type Result = Path on Plane
        type Plane = plane

        def make(path: Path on Plane): Path on Plane = path.also:
          createNonexistentParents(path):
            overwritePreexisting(path):
              mitigate:
                case ExecError(_, _, _) =>
                  import errorDiagnostics.stackTraces
                  IoError(path, IoError.Operation.Create, IoError.Reason.Unsupported)

              . within:
                  sh"mkfifo $path"() match
                    case Exit.Ok => ()
                    case _             =>
                      raise
                        ( IoError(path, IoError.Operation.Create, IoError.Reason.PermissionDenied) )

trait Makable extends Typeclass, Resultant, Planar:
  def make(path: Path on Plane): Result
