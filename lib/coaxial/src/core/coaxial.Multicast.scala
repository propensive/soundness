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
import gossamer.*
import murmuration.filter
import spectacular.*
import urticose.*
import vacuous.*

// A multicast group and port, which a program `subscribe`s to: it joins the group on each
// suitable interface, receives the group's datagrams, and may send to the group (an
// announcement) or back to one sender (a response it asked for by unicast). The group is one
// address, hence one address family: a protocol spoken over both IPv4 and IPv6 multicast, such
// as mDNS, subscribes to two groups.
object Multicast:
  given showable: Multicast is Showable = multicast =>
    val group = multicast.group.absolve match
      case ipv4: (Ipv4 @unchecked) => ipv4.show
      case ipv6: Ipv6              => t"[${ipv6.show}]"

    t"$group:${multicast.port.show}"

  // The handler's verdict on a received datagram: a response to the group (the multicast
  // norm, so that every member learns the answer), one to the sender alone, or nothing.
  enum Reply:
    case Ignore
    case Group(data: Data)
    case Unicast(data: Data)

  // The loaned handle on a joined group: `Socket.Service`'s `stop`, plus sends that the receive
  // loop did not prompt, from inside or outside the handler.
  abstract class Subscription(stopServer: () => Unit) extends Socket.Service(stopServer):
    def send(data: Data): Unit raises Socket.Error
    def send(data: Data, destination: Ipv4 | Ipv6, port: Udp.Port): Unit raises Socket.Error

  // The subscription `subscribe` lends: a named class rather than an anonymous one, so the
  // `transparent inline` loan does not duplicate it at each call site.
  private[coaxial] final class Joined(subscribable: Subscribable)
    ( binding: subscribable.Binding, stopServer: () => Unit )
  extends Subscription(stopServer):

    def send(data: Data): Unit raises Socket.Error = subscribable.sendGroup(binding, data)

    def send(data: Data, destination: Ipv4 | Ipv6, port: Udp.Port): Unit raises Socket.Error =
      subscribable.sendTo(binding, destination, port, data)

  given subscribable: (backend: Socket.Backend, options: Every[Socket.Option.Udp])
  =>  Multicast is Subscribable:
    type Binding = backend.MulticastSocket

    def join(multicast: Multicast, interface: Optional[MacAddress]): Binding =
      backend.joinMulticast(multicast, interfaces(interface), options.values.to(List))

    def receive(binding: Binding): Packet raises Socket.Error = backend.receiveMulticast(binding)

    def sendGroup(binding: Binding, data: Data): Unit raises Socket.Error =
      backend.sendGroup(binding, data)

    def sendTo(binding: Binding, destination: Ipv4 | Ipv6, port: Udp.Port, data: Data)
    :   Unit raises Socket.Error =

      backend.sendTo(binding, destination, port, data)

    def leave(binding: Binding): Unit = backend.leaveMulticast(binding)

    def transmit(binding: Binding, packet: Packet, reply: Reply): Unit raises Socket.Error =
      reply match
        case Reply.Ignore        => ()
        case Reply.Group(data)   => backend.sendGroup(binding, data)
        case Reply.Unicast(data) => backend.sendTo(binding, packet.sender, packet.port, data)

  // `listen`'s loan form for a group, for a subscriber that only responds; `subscribe` adds the
  // unprompted sends.
  given bindable: (subscribable: Multicast is Subscribable) => Multicast is Bindable:
    type Binding = subscribable.Binding
    type Input = Packet
    type Output = Reply

    def bind(multicast: Multicast, interface: Optional[MacAddress]): Binding =
      subscribable.join(multicast, interface)

    def connect(binding: Binding): Packet raises Socket.Error = subscribable.receive(binding)

    def transmit(binding: Binding, input: Packet, reply: Reply): Unit raises Socket.Error =
      subscribable.transmit(binding, input, reply)

    def stop(binding: Binding): Unit = subscribable.leave(binding)
    def close(input: Packet): Unit raises Socket.Error = ()

  // The interfaces to join on: the one with the given hardware address, or every interface that
  // is up and multicast-capable other than loopback (which cannot carry multicast on Linux; a
  // program's own datagrams come back through `Socket.Option.MulticastLoop` instead).
  def interfaces(interface: Optional[MacAddress]): List[NetworkInterface] =
    val all = safely(NetworkInterface.all()).or(Nil)

    val suitable = all.filter: nic => nic.up && nic.multicast && !nic.loopback
    interface.let { mac => all.filter(_.hardware == mac) }.or(suitable)

case class Multicast(group: Ipv4 | Ipv6, port: Udp.Port)
