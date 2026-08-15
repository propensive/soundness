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

import scala.caps

import anticipation.*
import contingency.*
import gigantism.*
import prepositional.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

object Serviceable:
  given domainSocket: (backend: Socket.Backend, tactic: Tactic[Truncation.Error])
  =>  (options: Every[Socket.Option.Domain])
  =>  ( (DomainSocket is Serviceable)^{tactic, caps.any} ) = new Serviceable:
    type Self = DomainSocket
    type Output = Data
    type Connection = backend.Exchange

    def connect(domainSocket: DomainSocket, interface: Optional[MacAddress]): Connection =
      backend.dialDomain(domainSocket, List.of(options.values))

    def transmit(connection: Connection, consume input: (Stream[Data] over Credit)^): Unit =
      backend.request(connection, input)

    def receive(connection: Connection): (Stream[Data] over Credit)^{this, caps.any} =
      backend.response(connection)

    def close(connection: Connection): Unit = backend.hangUp(connection)

  given tcpEndpoint: Online
  =>  (backend: Socket.Backend, tactic: Tactic[Truncation.Error])
  =>  (options: Every[Socket.Option.Tcp])
  =>  ( (Endpoint[Tcp.Port] is Serviceable)^{tactic} ) = new Serviceable:
    type Self = Endpoint[Tcp.Port]
    type Output = Data
    type Connection = backend.Exchange

    def connect(endpoint: Endpoint[Tcp.Port], interface: Optional[MacAddress]): Connection =
      backend.dialTcp(endpoint, interface, List.of(options.values))

    def transmit(connection: Connection, consume input: (Stream[Data] over Credit)^): Unit =
      backend.request(connection, input)

    def close(connection: Connection): Unit = backend.hangUp(connection)

    def receive(connection: Connection): (Stream[Data] over Credit)^{this, caps.any} =
      backend.response(connection)

  given tcpPort: (backend: Socket.Backend, tactic: Tactic[Truncation.Error])
  =>  (options: Every[Socket.Option.Tcp])
  =>  ( (Tcp.Port is Serviceable)^{tactic} ) = new Serviceable:
    type Self = Tcp.Port
    type Output = Data
    type Connection = backend.Exchange

    def connect(port: Tcp.Port, interface: Optional[MacAddress]): Connection =
      backend.dialTcpPort(port, interface, List.of(options.values))

    def close(connection: Connection): Unit = backend.hangUp(connection)

    def receive(connection: Connection): (Stream[Data] over Credit)^{this, caps.any} =
      backend.response(connection)

    def transmit(connection: Connection, consume input: (Stream[Data] over Credit)^): Unit =
      backend.request(connection, input)

trait Serviceable extends Routable:
  // A fresh pull endpoint over the connection's read side; single-use, like the
  // connection itself. The endpoint may retain this instance's capabilities (a
  // `Tactic[Truncation.Error]`), so the result is instance-relative rather than fresh.
  def receive(connection: Connection): (Stream[Data] over Credit)^{this, caps.any}
  def close(connection: Connection): Unit
