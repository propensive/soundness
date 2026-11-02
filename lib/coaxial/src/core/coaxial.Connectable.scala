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

import contingency.*
import gigantism.*
import prepositional.*
import urticose.*
import vacuous.*

// Opens a persistent, bidirectional `Duplex` connection to an endpoint. The
// counterpart to `Serviceable` for protocols that keep a connection open and read
// and write concurrently (e.g. HTTP/2), rather than the single request/response
// exchange `Serviceable` models. The backend opens channels in blocking mode so a
// read loop parks in `read` instead of busy-polling, and applies socket options
// (and, for TCP, a local interface bind) before connecting.
object Connectable:
  // An instance over a connection which cannot fail to open — an endpoint whose duplex is
  // already in hand, say — from a plain function: the trait's `connect` declares an error, so
  // a lambda does not convert to it.
  def apply[self](lambda: (self, Optional[MacAddress]) -> Duplex): self is Connectable =
    new Connectable:
      type Self = self

      def connect(endpoint: self, interface: Optional[MacAddress]): Duplex raises Socket.Error =
        lambda(endpoint, interface)

  given domainSocket: (backend: Socket.Backend, options: Every[Socket.Option.Domain])
  =>  DomainSocket is Connectable:
    // A Unix-domain socket has no network interface, so `interface` is not applicable here.
    def connect(domainSocket: DomainSocket, interface: Optional[MacAddress])
    :   Duplex raises Socket.Error =

      backend.duplexDomain(domainSocket, options.values.to(List))

  // Honestly tracked: the instance is resolvable only with `Online` permission, so it is a
  // capability carrying that evidence in its capture set.
  given tcpEndpoint: (online: Online)
  =>  ( backend: Socket.Backend, options: Every[Socket.Option.Tcp] )
  =>  ( (Endpoint[Tcp.Port] is Connectable)^{online, caps.any} ) =

    new Connectable:
      type Self = Endpoint[Tcp.Port]

      def connect(endpoint: Endpoint[Tcp.Port], interface: Optional[MacAddress])
      :   Duplex raises Socket.Error =

        backend.duplexTcp(endpoint, interface, options.values.to(List))

trait Connectable extends Typeclass:
  def connect(endpoint: Self, interface: Optional[MacAddress]): Duplex raises Socket.Error
