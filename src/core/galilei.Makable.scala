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

object Makable:
  given [PlatformType <: Filesystem]
     (using createNonexistentParents: CreateNonexistentParents on PlatformType,
            overwritePreexisting:     OverwritePreexisting on PlatformType,
            tactic:                   Tactic[IoError])
          => Directory is Makable on PlatformType into (Path on PlatformType) =
    new Makable:
      type Self = Directory
      type Result = Path on Platform
      type Platform = PlatformType

      def make(path: Path on Platform): Path on Platform =
        createNonexistentParents(path):
          overwritePreexisting(path):
            jnf.Files.createDirectory(path.javaPath)
            path

  given [PlatformType <: Posix]
     (using createNonexistentParents: CreateNonexistentParents on PlatformType,
            overwritePreexisting:     OverwritePreexisting on PlatformType,
            tactic:                   Tactic[IoError])
      => Socket is Makable into Socket as socket =
    new Makable:
      type Platform = PlatformType
      type Self = Socket
      type Result = Socket

      def make(path: Path on Platform): Result =
        createNonexistentParents(path):
          overwritePreexisting(path):
            val address = java.net.UnixDomainSocketAddress.of(path.javaPath).nn
            val channel = jnc.ServerSocketChannel.open(java.net.StandardProtocolFamily.UNIX).nn
            channel.bind(address)
            Socket(channel)

  given [PlatformType <: Filesystem]
     (using createNonexistentParents: CreateNonexistentParents on PlatformType,
            overwritePreexisting:     OverwritePreexisting on PlatformType,
            tactic:                   Tactic[IoError])
            => File is Makable on PlatformType into (Path on PlatformType) as file =
    new Makable:
      type Platform = PlatformType
      type Self = File
      type Result = Path on Platform

      def make(path: Path on Platform): Path on Platform = path.also:
        createNonexistentParents(path):
          overwritePreexisting(path):
            jnf.Files.createFile(path.javaPath)

  given [PlatformType <: Filesystem]
     (using createNonexistentParents: CreateNonexistentParents on PlatformType,
            overwritePreexisting:     OverwritePreexisting on PlatformType,
            working:                  WorkingDirectory,
            tactic:                   Tactic[IoError],
            loggable:                 ExecEvent is Loggable)
      => Fifo is Makable into (Path on PlatformType) as fifo =
    new Makable:
      type Self = Fifo
      type Result = Path on Platform
      type Platform = PlatformType

      def make(path: Path on Platform): Path on Platform = path.also:
        createNonexistentParents(path):
          overwritePreexisting(path):
            tend:
              case ExecError(_, _, _) =>
                import errorDiagnostics.stackTraces
                IoError(path, IoError.Operation.Create, IoError.Reason.Unsupported)

            . within:
                sh"mkfifo $path"() match
                  case Exit.Ok => ()
                  case _             =>
                    raise(IoError(path, IoError.Operation.Create, IoError.Reason.PermissionDenied))

trait Makable:
  type Self
  type Result
  type Platform <: Filesystem
  def make(path: Path on Platform): Result
