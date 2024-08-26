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
import guillotine.*
import rudiments.*

import scala.jdk.StreamConverters.*

import java.nio as jn
import java.nio.file as jnf
import java.nio.channels as jnc

import language.experimental.pureFunctions

object EntryMaker:
  given directory
      (using createNonexistentParents: CreateNonexistentParents, overwritePreexisting: OverwritePreexisting)
      (using io: Tactic[IoError])
          : EntryMaker[Directory, Path] = path =>
    createNonexistentParents(path):
      overwritePreexisting(path):
        jnf.Files.createDirectory(path.stdlib)

    Directory(path)

  given socket
      (using createNonexistentParents: CreateNonexistentParents, overwritePreexisting: OverwritePreexisting)
      (using io: Tactic[IoError])
          : EntryMaker[Socket, Unix.Path] = path =>
    createNonexistentParents(path):
      overwritePreexisting(path):
        val address = java.net.UnixDomainSocketAddress.of(path.stdlib).nn
        val channel = jnc.ServerSocketChannel.open(java.net.StandardProtocolFamily.UNIX).nn
        channel.bind(address)
        Socket(path, channel)

  given file
      (using createNonexistentParents: CreateNonexistentParents, overwritePreexisting: OverwritePreexisting)
          : EntryMaker[File, Path] =
    path => createNonexistentParents(path):
      overwritePreexisting(path):
        jnf.Files.createFile(path.stdlib)

    File(path)

  given fifo
      (using createNonexistentParents: CreateNonexistentParents,
             overwritePreexisting:     OverwritePreexisting,
             working:                  WorkingDirectory,
             io:                       Tactic[IoError],
             exec:                     Tactic[ExecError])
          : (EntryMaker[Fifo, Unix.Path] logs IoEvent) =

    path => createNonexistentParents(path):
      overwritePreexisting(path):
        sh"mkfifo $path"() match
          case ExitStatus.Ok => ()
          case _             => raise(IoError(path))

    Fifo(path)

@capability
trait EntryMaker[+EntryType <: Entry, -PathType <: Path]:
  def apply(value: PathType): EntryType
