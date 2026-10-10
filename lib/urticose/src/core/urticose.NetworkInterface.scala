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
package urticose

import java.net as jn
import java.util as ju

import scala.jdk.CollectionConverters.*

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

import NetworkInterface.Error.Reason.*

object NetworkInterface:
  given showable: NetworkInterface is Showable = _.name

  def all(): List[NetworkInterface] raises NetworkInterface.Error = enumerated:
    def recur(interfaces: ju.Enumeration[jn.NetworkInterface], acc: List[NetworkInterface])
    :   List[NetworkInterface] =

      if !interfaces.hasMoreElements then acc.reverse
      else recur(interfaces, read(interfaces.nextElement.nn) :: acc)

    Optional(jn.NetworkInterface.getNetworkInterfaces).lay(Nil)(recur(_, Nil))

  def byName(name: Text): Optional[NetworkInterface] raises NetworkInterface.Error = enumerated:
    Optional(jn.NetworkInterface.getByName(name.s)).let(read(_))

  def byIndex(index: Int): Optional[NetworkInterface] raises NetworkInterface.Error = enumerated:
    Optional(jn.NetworkInterface.getByIndex(index)).let(read(_))

  def byAddress(address: Ipv4 | Ipv6): Optional[NetworkInterface] raises NetworkInterface.Error =
    enumerated:
      val inet = jn.InetAddress.getByAddress(Array.unsafeJvm(address.bytes)).nn
      Optional(jn.NetworkInterface.getByInetAddress(inet)).let(read(_))

  // Inline, so the thunk never crosses a checked function boundary: a context-function
  // result would hide the caller's thunk, which the separation checker rejects.
  private inline def enumerated[result](inline block: result)
    ( using Tactic[NetworkInterface.Error]^ )
  :   result =

    try block catch case error: jn.SocketException =>
      abort(NetworkInterface.Error(Enumeration(message(error))))

  private def message(error: jn.SocketException): Text =
    Optional(error.getMessage).lay(t"of a socket error")(_.tt)

  private def read(nic: jn.NetworkInterface): NetworkInterface raises NetworkInterface.Error =
    val name = nic.getName.nn.tt

    try
      val hardware = Optional(nic.getHardwareAddress).let: bytes =>
        MacAddress(bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5))

      val addresses = nic.getInterfaceAddresses.nn.to[List].map: entry =>
        val broadcast = Optional(entry.getBroadcast).let(inet(_)).let(_.absolve match
          case ipv4: (Ipv4 @unchecked) => ipv4
          case _: Ipv6                 => Unset)

        InterfaceAddress(inet(entry.getAddress.nn), entry.getNetworkPrefixLength.toInt, broadcast)

      NetworkInterface
        ( name,
          nic.getDisplayName.nn.tt,
          nic.getIndex,
          hardware,
          addresses,
          nic.getMTU,
          nic.isUp,
          nic.isLoopback,
          nic.isPointToPoint,
          nic.supportsMulticast,
          nic.isVirtual )

    catch case error: jn.SocketException =>
      abort(NetworkInterface.Error(Inspection(name, message(error))))

  // Every `InetAddress` is an `Inet4Address` or an `Inet6Address`, so one of the two decoders
  // accepts its bytes; the `Ipv4.Localhost` fallback is unreachable.
  private def inet(address: jn.InetAddress): Ipv4 | Ipv6 =
    val data = Array.unsafeFrozen(address.getAddress.nn)
    safely(data.as[Ipv4]).or(safely(data.as[Ipv6])).or(Ipv4.Localhost)

  // NetworkInterfaceError → NetworkInterface.Error
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Enumeration(message) =>
          m"the network interfaces could not be enumerated because $message"

        case Inspection(name, message) =>
          m"the interface $name could not be inspected because $message"

    enum Reason(val number: Int) extends Clarification:
      case Enumeration(message: Text)             extends Reason(1)
      case Inspection(name: Text, message: Text)  extends Reason(2)

  case class Error(reason: NetworkInterface.Error.Reason)(using Diagnostics)
  extends fulminate.Error(418, reason.number)
    ( m"the network interface could not be read because $reason" )

case class NetworkInterface
  ( name:         Text,
    displayName:  Text,
    index:        Int,
    hardware:     Optional[MacAddress],
    addresses:    List[InterfaceAddress],
    mtu:          Int,
    up:           Boolean,
    loopback:     Boolean,
    pointToPoint: Boolean,
    multicast:    Boolean,
    virtual:      Boolean ):

  def ipv4: List[Ipv4] =
    addresses.map(_.address).sweep { case ip: (Ipv4 @unchecked) => ip }

  def ipv6: List[Ipv6] =
    addresses.map(_.address).sweep { case ip: Ipv6 => ip }
