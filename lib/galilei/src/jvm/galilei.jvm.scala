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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import java.io as ji
import java.net as jn
import java.nio.channels as jnc
import java.nio.file as jnf

import aperture.*
import contingency.*
import prepositional.*
import serpentine.*

// The `java.nio` representations of a path, for interoperating with Java APIs directly.
extension [plane: Filesystem](path: Path on plane)
  def javaPath: jnf.Path = jnf.Path.of(Path.encodable.encode(path).s).nn
  def javaFile: ji.File = javaPath.toFile.nn

object SocketCreation:
  // Binding a Unix-domain socket is inherently scoped under the new model: the block form
  // provides the live, bound `Socket` and closes its channel when the scope ends (the socket
  // file remains, as before; an exception escaping the scope removes it). The no-block form
  // binds and immediately closes, leaving just the socket file.
  class SocketCreatable[filesystem <: Posix: Filesystem, path <: Path on filesystem]
    ( using backend: FilesystemBackend on filesystem, tactic: Tactic[IoError] )
  extends Creatable:

    type Self = path
    type Form = Socket
    type Operand = CreateFlag
    type Grants = Grant.Read & Grant.Write
    type Result = Socket

    def create[result]
      ( value: path, flags: List[CreateFlag] )
      ( block: (Socket & Granting[Grant.Read & Grant.Write]) ?=> result )
    :   result =

      Creation.ensure(value, flags)
      Creation.replace(value, flags)

      val address = jn.UnixDomainSocketAddress.of(value.javaPath).nn
      val channel = jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn
      channel.bind(address)

      try
        try block(using new Socket(channel) with Granting[Grant.Read & Grant.Write] {})
        catch case throwable: Throwable =>
          try backend.deleteIfExists(value) catch case _: Exception => ()
          throw throwable
      finally channel.close()

  given socket: [filesystem <: Posix: Filesystem, path <: Path on filesystem]
  =>  ( FilesystemBackend on filesystem, Tactic[IoError] )
  =>  SocketCreatable[filesystem, path] =
    SocketCreatable[filesystem, path]

export SocketCreation.socket as socketCreatable
