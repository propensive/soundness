/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import anticipation.*
import contextual.*
import contingency.*
import prepositional.*
import fulminate.*
import guillotine.*
import serpentine.*
import rudiments.*

import scala.jdk.StreamConverters.*

import java.nio as jn
import java.nio.file as jnf
import java.nio.channels as jnc

import language.experimental.pureFunctions

object Creatable:
  given (using createNonexistentParents: CreateNonexistentParents,
               overwritePreexisting:     OverwritePreexisting,
               tactic:                   Tactic[IoError]) => Directory is Creatable into Path =
    _.tap: path =>
      createNonexistentParents(path):
        overwritePreexisting(path):
          jnf.Files.createDirectory(path.javaPath)

  given (using createNonexistentParents: CreateNonexistentParents,
               overwritePreexisting:     OverwritePreexisting,
               tactic:                   Tactic[IoError])
            => Socket is Creatable into Socket as socket =

    path =>
      createNonexistentParents(path):
        overwritePreexisting(path):
          val address = java.net.UnixDomainSocketAddress.of(path.javaPath).nn
          val channel = jnc.ServerSocketChannel.open(java.net.StandardProtocolFamily.UNIX).nn
          channel.bind(address)
          Socket(channel)

  given (using createNonexistentParents: CreateNonexistentParents,
               overwritePreexisting:     OverwritePreexisting,
               tactic:                   Tactic[IoError]) => File is Creatable into Path as file =
    _.tap: path =>
      createNonexistentParents(path):
        overwritePreexisting(path):
          jnf.Files.createFile(path.javaPath)

  given (using createNonexistentParents: CreateNonexistentParents,
               overwritePreexisting:     OverwritePreexisting,
               working:                  WorkingDirectory,
               tactic:                   Tactic[IoError],
               loggable:                 ExecEvent is Loggable)
      => Fifo is Creatable into Path as fifo =
    _.tap: path =>
      createNonexistentParents(path):
        overwritePreexisting(path):
          tend:
            case ExecError(_, _, _) =>
              import exceptionDiagnostics.stackTraces
              IoError(path, IoError.Operation.Create, IoError.Reason.Unsupported)
          .within:
            sh"mkfifo $path"() match
              case ExitStatus.Ok => ()
              case _             =>
                raise(IoError(path, IoError.Operation.Create, IoError.Reason.PermissionDenied))

trait Creatable:
  type Self
  type Result
  def create(path: Path): Result
