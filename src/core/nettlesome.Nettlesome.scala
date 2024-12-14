/*
    Nettlesome, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package nettlesome

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import hypotenuse.*
import prepositional.*
import rudiments.*
import spectacular.*

import scala.quoted.*

import IpAddressError.Reason, Reason.*

object Nettlesome:
  given Realm = realm"nettlesome"

  object Opaques:
    opaque type Ipv4 <: Matchable = Int
    opaque type MacAddress <: Matchable = Long
    opaque type DnsLabel = Text
    opaque type TcpPort <: Port = Int & Port
    opaque type UdpPort <: Port = Int & Port

    object DnsLabel:
      given DnsLabel is Showable = identity(_)

      def apply(text: Text): DnsLabel = text

    extension (label: DnsLabel)
      def text: Text = label

    object Ipv4:
      erased given Underlying[Ipv4, Int] as underlying = ###

      given Ipv4 is Showable = ip =>
        t"${ip.byte0.toString}.${ip.byte1.toString}.${ip.byte2.toString}.${ip.byte3.toString}"

      given Ipv4 is Encodable in Text as encodable = _.show
      given (using Tactic[IpAddressError]) => Decoder[Ipv4] as decoder = parse(_)

      lazy val Localhost: Ipv4 = apply(127, 0, 0, 1)

      def apply(int: Int): Ipv4 = int

      def apply(byte0: Int, byte1: Int, byte2: Int, byte3: Int): Ipv4 =
        ((byte0 & 255) << 24) + ((byte1 & 255) << 16) + ((byte2 & 255) << 8) + (byte3 & 255)

      def parse(text: Text): Ipv4 raises IpAddressError =
        val bytes = text.cut(t".")
        if bytes.length == 4 then
          tend:
            case error@NumberError(text, _) =>
              given Diagnostics = error.diagnostics
              IpAddressError(Ipv4ByteNotNumeric(text))

          . within:
              bytes.map(Decoder.int.decode(_)).pipe: bytes =>
                for byte <- bytes
                do if !(0 <= byte <= 255) then raise(IpAddressError(Ipv4ByteOutOfRange(byte)), 0.toByte)

                Ipv4(bytes(0).toByte, bytes(1).toByte, bytes(2).toByte, bytes(3).toByte)

        else raise(IpAddressError(Ipv4WrongNumberOfGroups(bytes.length)), 0)

    object MacAddress:
      import MacAddressError.Reason.*
      erased given Underlying[MacAddress, Long] as underlying = ###
      given MacAddress is Showable = _.text
      given MacAddress is Encodable in Text as encodable = _.text
      given (using Tactic[MacAddressError]) => Decoder[MacAddress] as decoder = parse(_)

      def apply(value: Long): MacAddress = value

      def parse(text: Text): MacAddress raises MacAddressError =
        val groups = text.cut(t"-").to(List)

        if groups.length != 6
        then raise(MacAddressError(WrongGroupCount(groups.length)))

        @tailrec
        def recur(todo: List[Text], index: Int = 0, acc: Long = 0L): Long = todo match
          case Nil => acc

          case head :: tail =>
            if head.length != 2
            then raise(MacAddressError(WrongGroupLength(index, head.length)))

            val value = try Integer.parseInt(head.s, 16) catch case error: NumberFormatException =>
              raise(MacAddressError(NotHex(index, head)), 0)

            recur(tail, index + 1, (acc << 8) + value)

        recur(groups)

      def apply(byte0: Byte, byte1: Byte, byte2: Byte, byte3: Byte, byte4: Byte, byte5: Byte)
              : MacAddress =

        def recur(todo: List[Byte], done: Long): Long = todo match
          case head :: tail => recur(tail, (done << 8) + head)
          case Nil          => done

        recur(List(byte0, byte1, byte2, byte3, byte4, byte5), 0L)

    object TcpPort:
      erased given Underlying[TcpPort, Int] as underlying = ###
      given TcpPort is Showable = port => TextConversion.int.text(port.number)
      given TcpPort is Encodable in Text as encodable = port => TextConversion.int.text(port.number)

      given (using Tactic[NumberError], Tactic[PortError]) => Decoder[TcpPort] as decoder =
        text => apply(Decoder.int.decode(text))

      def unsafe(value: Int): TcpPort = value.asInstanceOf[TcpPort]

      def apply(value: Int): TcpPort raises PortError =
        if 1 <= value <= 65535 then value.asInstanceOf[TcpPort] else raise(PortError(), unsafe(1))

    object UdpPort:
      erased given Underlying[UdpPort, Int] as underlying = ###
      given UdpPort is Showable = port => TextConversion.int.text(port.number)
      given UdpPort is Encodable in Text as encodable = port => TextConversion.int.text(port.number)

      given (using Tactic[NumberError], Tactic[PortError]) => Decoder[UdpPort] as decoder =
        text => apply(Decoder.int.decode(text))

      def unsafe(value: Int): UdpPort = value.asInstanceOf[UdpPort]

      def apply(value: Int): UdpPort raises PortError =
        if 1 <= value <= 65535 then value.asInstanceOf[UdpPort] else raise(PortError(), unsafe(1))

    extension (port: TcpPort | UdpPort)
      def number: Int = port
      def privileged: Boolean = port < 1024

    extension (macAddress: MacAddress)
      def byte0: Int = (macAddress >>> 40).toInt
      def byte1: Int = (macAddress >>> 32).toInt & 255
      def byte2: Int = (macAddress >>> 24).toInt & 255
      def byte3: Int = (macAddress >>> 16).toInt & 255
      def byte4: Int = (macAddress >>> 8).toInt & 255
      def byte5: Int = macAddress.toInt & 255

      def text: Text =
        List(byte0, byte1, byte2, byte3, byte4, byte5).map(_.hex.pad(2, Rtl, '0')).join(t"-")

      def long: Long = macAddress

    extension (ip: Ipv4)
      def byte0: Int = ip >>> 24
      def byte1: Int = (ip >>> 16) & 255
      def byte2: Int = (ip >>> 8) & 255
      def byte3: Int = ip & 255

      def subnet(size: Int): Ipv4Subnet = Ipv4Subnet(ip & (-1 << (32 - size)), size)

      def int: Int = ip

  object Ipv4Subnet:
    given Ipv4Subnet is Showable = subnet => t"${subnet.ipv4}/${subnet.size}"

  case class Ipv4Subnet(ipv4: Ipv4, size: Int)

  case class Ipv6(highBits: Long, lowBits: Long)

  def tcpPort(context: Expr[StringContext])(using Quotes): Expr[TcpPort] =
    val portNumber: Int = abandonment(context.valueOrAbort.parts.head.tt.decode[Int])

    if 1 <= portNumber <= 65535 then '{TcpPort.unsafe(${Expr(portNumber)})}
    else abandon(m"the TCP port number ${portNumber} is not in the range 1-65535")

  def udpPort(context: Expr[StringContext])(using Quotes): Expr[UdpPort] =
    val portNumber: Int = abandonment(context.valueOrAbort.parts.head.tt.decode[Int])

    if 1 <= portNumber <= 65535 then '{UdpPort.unsafe(${Expr(portNumber)})}
    else abandon(m"the UDP port number ${portNumber} is not in the range 1-65535")

  def ip(context: Expr[StringContext])(using Quotes): Expr[Ipv4 | Ipv6] =
    val text = Text(context.valueOrAbort.parts.head)

    abandonment:
      if text.contains(t".") then
        val ipv4 = Ipv4.parse(text)
        '{Ipv4(${Expr(ipv4.byte0)}, ${Expr(ipv4.byte1)}, ${Expr(ipv4.byte2)}, ${Expr(ipv4.byte3)})}

      else
        val ipv6 = Ipv6.parse(text)
        '{Ipv6(${Expr(ipv6.highBits)}, ${Expr(ipv6.lowBits)})}

  def mac(context: Expr[StringContext])(using Quotes): Expr[MacAddress] = abandonment:
    val macAddress = MacAddress.parse(context.valueOrAbort.parts.head.tt)
    '{MacAddress(${Expr(macAddress.long)})}

  object Ipv6:
    lazy val Localhost: Ipv6 = apply(0, 0, 0, 0, 0, 0, 0, 1)

    given ToExpr[Ipv6] as toExpr:
      def apply(ipv6: Ipv6)(using Quotes): Expr[Ipv6] = '{Ipv6(${Expr(ipv6.highBits)}, ${Expr(ipv6.lowBits)})}

    given Ipv6 is Showable = ip =>
      def unpack(long: Long, groups: List[Int] = Nil): List[Int] =
        if groups.length == 4 then groups else unpack(long >>> 16, (long & 65535).toInt :: groups)

      def hex(values: List[Int]): Text =
        values.map(_.hex).join(t":")

      val groups = unpack(ip.highBits) ++ unpack(ip.lowBits)
      val (middleIndex, middleLength) = groups.longestTrain(_ == 0)

      if middleLength < 2 then hex(groups)
      else t"${hex(groups.take(middleIndex))}::${hex(groups.drop(middleIndex + middleLength))}"

    def apply(g0: Int, g1: Int, g2: Int, g3: Int, g4: Int, g5: Int, g6: Int, g7: Int): Ipv6 =
      Ipv6(pack(List(g0, g1, g2, g3)), pack(List(g4, g5, g6, g7)))

    def parseGroup(text: Text): Int raises IpAddressError =
      if text.length > 4 then raise(IpAddressError(Ipv6GroupWrongLength(text)))

      text.lower.s.each: char =>
        if !('0' <= char <= '9' || 'a' <= char <= 'f')
        then raise(IpAddressError(Ipv6GroupNotHex(text)))

      Integer.parseInt(text.s, 16)

    def pack(groups: List[Int], accumulator: Long = 0L): Long = groups match
      case Nil          => accumulator
      case head :: tail => pack(tail, (accumulator << 16) + (head & 65535))

    private val zeroes: List[Text] = List.fill(8)(t"0")

    def parse(text: Text): Ipv6 raises IpAddressError =
      val groups: List[Text] = text.cut(t"::").to(List) match
        case List(left, right) =>
          val leftGroups = left.cut(t":").to(List).filter(_ != t"")
          val rightGroups = right.cut(t":").to(List).filter(_ != t"")

          if leftGroups.length + rightGroups.length > 7
          then raise(IpAddressError(Ipv6TooManyNonzeroGroups(leftGroups.length + rightGroups.length)))

          leftGroups ++ List.fill((8 - leftGroups.length - rightGroups.length))(t"0") ++ rightGroups

        case List(whole) =>
          val groups = whole.cut(t":")

          if groups.length != 8
          then raise(IpAddressError(Ipv6WrongNumberOfGroups(groups.length)), zeroes) else groups.to(List)

        case _ =>
          raise(IpAddressError(Ipv6MultipleDoubleColons), zeroes)

      Ipv6(pack(groups.take(4).map(parseGroup)), pack(groups.drop(4).map(parseGroup)))
