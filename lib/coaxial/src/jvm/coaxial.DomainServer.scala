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
package coaxial

import java.net as jn
import java.nio.channels as jnc
import java.nio.file as jnf

import anticipation.*
import beneficence.*
import contingency.*
import parasite.*
import prepositional.*
import rudiments.*
import vacuous.*

// A JVM-only Unix-domain server that hands each accepted connection to `handler` as a raw
// `Connection` — the blocking `InputStream`/`OutputStream` pair. This is the shape a consumer
// needs when it must build a subprocess `Stdio` (an `ji.PrintStream` over the socket, streams
// handed to a child process) rather than the platform-neutral `Duplex` that `Bindable.listen`
// exposes; `ethereal`'s daemon is the motivating case. The accept loop and per-connection
// supervision mirror `listen`: each connection is served by its own `async` task (so a handler
// failure or a dropped client only ends its own task, and the connection is always closed),
// while a failure to *accept* skips one loop iteration; `stop()` unwinds the parked `accept()`.
extension (domainSocket: DomainSocket)
  // A loan, like `Bindable.listen`: the running server is lent to `block` as a
  // `Socket.Service` capability and always stopped afterwards.
  // With `ownerOnly`, the socket file is made mode 0600 as soon as it is bound (a bound socket
  // otherwise takes the process umask, typically 0755), so that only this user can connect;
  // a filesystem without POSIX permissions (Windows) is left as it is. Each accepted
  // connection carries the peer's user, where the platform reports one, in `Connection.peer`.
  def listenConnections[result](using Monitor, Probate)
    ( handler: Connection => Unit, ownerOnly: Boolean = false )
    ( using (Socket.Event is Loggable)^ )
    ( block: Socket.Service ?=> result )
  :   result =

    val channel = jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn
    channel.configureBlocking(true)
    channel.bind(jn.UnixDomainSocketAddress.of(domainSocket.address.s))

    if ownerOnly && jnf.FileSystems.getDefault.nn.supportedFileAttributeViews.nn.contains("posix")
    then
      jnf.Files.setPosixFilePermissions
        ( jnf.Path.of(domainSocket.address.s),
          jnf.attribute.PosixFilePermissions.fromString("rw-------") )

    Log.info(Socket.Event.Listening(domainSocket.address))

    def peer(client: jnc.SocketChannel): Optional[Text] = safely:
      client.getOption(jdk.net.ExtendedSocketOptions.SO_PEERCRED).nn.user().nn.getName().nn.tt

    val bindLoop = loop:
      safely:
        val client = channel.accept().nn

        Connection
          ( jnc.Channels.newInputStream(client).nn,
            jnc.Channels.newOutputStream(client).nn,
            peer(client) )

      . let: connection =>
          // Fire-and-forget: the fresh task handle is discarded (a lambda result may not
          // carry it).
          async:
            safely(try handler(connection) finally connection.close())

          ()

    // The loop is created and awaited under the same monitor; no aliased writer.
    val task = scala.caps.unsafe.unsafeAssumeSeparate(async(bindLoop.run()))

    val service = Socket.Service: () =>
      bindLoop.stop()
      channel.close()
      scala.caps.unsafe.unsafeAssumeSeparate(safely(task.await()))
      Log.fine(Socket.Event.Closed(domainSocket.address))

    try block(using service) finally service.stop()
