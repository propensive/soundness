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
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

import NetworkInterfaceError.Reason.*

object NetworkInterface:
  given showable: NetworkInterface is Showable = _.name

  def all(): List[NetworkInterface] raises NetworkInterfaceError = enumerated:
    def recur(interfaces: ju.Enumeration[jn.NetworkInterface], acc: List[NetworkInterface])
    :   List[NetworkInterface] =

      if !interfaces.hasMoreElements then acc.reverse
      else recur(interfaces, read(interfaces.nextElement.nn) :: acc)

    Optional(jn.NetworkInterface.getNetworkInterfaces).lay(Nil)(recur(_, Nil))

  def byName(name: Text): Optional[NetworkInterface] raises NetworkInterfaceError = enumerated:
    Optional(jn.NetworkInterface.getByName(name.s)).let(read(_))

  def byIndex(index: Int): Optional[NetworkInterface] raises NetworkInterfaceError = enumerated:
    Optional(jn.NetworkInterface.getByIndex(index)).let(read(_))

  def byAddress(address: Ipv4 | Ipv6): Optional[NetworkInterface] raises NetworkInterfaceError =
    enumerated:
      val inet = jn.InetAddress.getByAddress(bytes(address)).nn
      Optional(jn.NetworkInterface.getByInetAddress(inet)).let(read(_))

  // Inline, so the thunk never crosses a checked function boundary: a context-function
  // result would hide the caller's thunk, which the separation checker rejects.
  private inline def enumerated[result](inline block: result)
    ( using Tactic[NetworkInterfaceError]^ )
  :   result =

    try block catch case error: jn.SocketException =>
      abort(NetworkInterfaceError(Enumeration(message(error))))

  private def message(error: jn.SocketException): Text =
    Optional(error.getMessage).lay(t"of a socket error")(_.tt)

  private def read(nic: jn.NetworkInterface): NetworkInterface raises NetworkInterfaceError =
    val name = nic.getName.nn.tt

    try
      val hardware = Optional(nic.getHardwareAddress).let: bytes =>
        MacAddress(bytes(0), bytes(1), bytes(2), bytes(3), bytes(4), bytes(5))

      val addresses = nic.getInterfaceAddresses.nn.to[List].map: entry =>
        val broadcast = Optional(entry.getBroadcast).let(ipv4(_))
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
      abort(NetworkInterfaceError(Inspection(name, message(error))))

  private def inet(address: jn.InetAddress): Ipv4 | Ipv6 =
    val bytes = address.getAddress.nn

    if bytes.length == 4 then ipv4(address)
    else Ipv6(longOf(bytes, 0), longOf(bytes, 8))

  private def ipv4(address: jn.InetAddress): Ipv4 =
    val bytes = address.getAddress.nn
    Ipv4(bytes(0).toInt, bytes(1).toInt, bytes(2).toInt, bytes(3).toInt)

  private def longOf(bytes: scala.Array[Byte], offset: Int): Long =
    (0 until 8).foldLeft(0L): (acc, index) =>
      (acc << 8) | (bytes(offset + index) & 0xff).toLong

  private def bytes(address: Ipv4 | Ipv6): scala.Array[Byte] = address match
    case ipv6: Ipv6 =>
      val array = new scala.Array[Byte](16)
      for index <- 0 until 8 do array(index) = (ipv6.highBits >>> (56 - index*8)).toByte
      for index <- 0 until 8 do array(index + 8) = (ipv6.lowBits >>> (56 - index*8)).toByte
      array

    case ipv4: (Ipv4 @unchecked) =>
      scala.Array(ipv4.byte0.toByte, ipv4.byte1.toByte, ipv4.byte2.toByte, ipv4.byte3.toByte)

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
    List.of(addresses.stdlib.map(_.address).collect { case ip: (Ipv4 @unchecked) => ip })
  def ipv6: List[Ipv6] =
    List.of(addresses.stdlib.map(_.address).collect { case ip: Ipv6 => ip })
