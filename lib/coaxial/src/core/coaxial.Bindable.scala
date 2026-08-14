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

import anticipation.*
import contingency.*
import gigantism.*
import prepositional.*
import urticose.*
import vacuous.*
import zephyrine.*

object Bindable:
  given domainSocket: (backend: Socket.Backend, options: Every[Socket.Option.Domain])
  =>  DomainSocket is Bindable:
    type Binding = backend.ServerSocket
    type Input = Duplex
    type Output = Data

    // A Unix-domain socket has no network interface, so `interface` is not applicable here.
    def bind(domainSocket: DomainSocket, interface: Optional[MacAddress]): Binding =
      backend.listenDomain(domainSocket, List.of(options.values))

    def connect(binding: Binding): Duplex raises Socket.Error = backend.accept(binding)

    def transmit(binding: Binding, input: Duplex, bytes: Data): Unit raises Socket.Error =
      input.send(Stream(bytes))

    def stop(binding: Binding): Unit = backend.shutdown(binding)
    def close(connection: Duplex): Unit raises Socket.Error = connection.close()

  given tcpPort: (backend: Socket.Backend, options: Every[Socket.Option.Tcp]) => Tcp.Port is Bindable:
    type Binding = backend.ServerSocket
    type Input = Duplex
    type Output = Data

    def bind(port: Tcp.Port, interface: Optional[MacAddress]): Binding =
      backend.listenTcp(port, interface, List.of(options.values))

    def connect(binding: Binding): Duplex raises Socket.Error = backend.accept(binding)

    def transmit(binding: Binding, input: Duplex, bytes: Data): Unit raises Socket.Error =
      input.send(Stream(bytes))

    def close(connection: Duplex): Unit raises Socket.Error = connection.close()
    def stop(binding: Binding): Unit = backend.shutdown(binding)

  given udpPort: (backend: Socket.Backend, options: Every[Socket.Option.Udp]) => Udp.Port is Bindable:
    type Binding = backend.DatagramSocket
    type Input = Packet
    type Output = UdpResponse

    def bind(port: Udp.Port, interface: Optional[MacAddress]): Binding =
      backend.listenUdp(port, interface, List.of(options.values))

    def connect(binding: Binding): Packet raises Socket.Error = backend.receive(binding)

    def transmit(binding: Binding, input: Packet, response: UdpResponse)
    :   Unit raises Socket.Error =

      response match
        case UdpResponse.Ignore      => ()
        case UdpResponse.Reply(data) => backend.reply(binding, input.sender, input.port, data)

    def stop(binding: Binding): Unit = backend.unbind(binding)
    def close(input: Packet): Unit raises Socket.Error = ()

trait Bindable extends Typeclass:
  type Binding
  type Input
  type Output

  def bind(socket: Self, interface: Optional[MacAddress]): Binding
  def connect(binding: Binding): Input raises Socket.Error
  def transmit(binding: Binding, input: Input, output: Output): Unit raises Socket.Error
  def close(connection: Input): Unit raises Socket.Error
  def stop(binding: Binding): Unit
