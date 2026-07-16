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
package coaxial

import java.net as jn
import java.nio.channels as jnc

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
  // `SocketService` capability and always stopped afterwards.
  def listenConnections[result](using Monitor, Probate)(handler: Connection => Unit)
    ( using (SocketEvent is Loggable)^ )
    ( block: SocketService ?=> result )
  :   result =

    val channel = jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn
    channel.configureBlocking(true)
    channel.bind(jn.UnixDomainSocketAddress.of(domainSocket.address.s))
    Log.info(SocketEvent.Listening(domainSocket.address))

    val bindLoop = loop:
      safely:
        val client = channel.accept().nn
        Connection(jnc.Channels.newInputStream(client).nn, jnc.Channels.newOutputStream(client).nn)

      . let: connection =>
          async:
            safely(try handler(connection) finally connection.close())

    val task = async(bindLoop.run())

    val service = SocketService: () =>
      bindLoop.stop()
      channel.close()
      safely(task.await())
      Log.fine(SocketEvent.Closed(domainSocket.address))

    try block(using service) finally service.stop()
