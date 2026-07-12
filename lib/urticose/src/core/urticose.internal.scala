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
package urticose

import java.io as ji

import scala.compiletime.asMatchable
import scala.quoted.*

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gigantism.*
import gossamer.*
import hieroglyph.*, textMetrics.uniformMetric
import hypotenuse.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

import IpAddressError.Reason, Reason.*

object internal:
  // Resolve `Ipv4` to the nested opaque type directly rather than via the
  // package-level re-export; exporting `Ipv4Subnet` (whose field is typed `Ipv4`)
  // would otherwise create a cross-file cycle through that re-export.
  import Opaques.Ipv4

  private lazy val serviceNames: Map[(Boolean, Text), Int] =
    val stream =
      Optional(getClass.getResourceAsStream("/urticose/service-names-port-numbers.csv")).or:
        safely:
          val uri =
            new java.net.URI
              ( "https://www.iana.org/assignments/service-names-port-numbers/" +
                "service-names-port-numbers.csv" )

          uri.toURL().nn.openStream().nn: ji.InputStream

      . or:
          panic(m"could not read /urticose/service-names-port-numbers.csv from classpath")

    val lines: Iterator[List[Text]] =
      scala.io.Source.fromInputStream(stream).getLines().map(_.tt).map(_.cut(t","))

    lines.flatMap: list =>
      safely:
        if list(2) == t"tcp" then List((true, list(0)) -> list(1).as[Int])
        else if list(2) == t"udp" then List((false, list(0)) -> list(1).as[Int])
        else Nil

      . or(Nil)

    . to(Map)

  object Opaques:
    opaque type Ipv4 <: Matchable = Int
    opaque type MacAddress <: Matchable = Long
    opaque type DnsLabel = anticipation.Text
    opaque type Port <: PortType = Int & PortType

    object DnsLabel:
      given showable: DnsLabel is Showable = identity(_)

      def apply(text: Text): DnsLabel = text


    extension (label: DnsLabel)
      def text: Text = label


    object Ipv4:
      inline given underlying: Underlying[Ipv4, Int] = !!

      given showable: Ipv4 is Showable = ip =>
        t"${ip.byte0.toString}.${ip.byte1.toString}.${ip.byte2.toString}.${ip.byte3.toString}"

      given encodable: Ipv4 is Encodable in Text = _.show
      given decodable: (tactic: Tactic[IpAddressError])
      =>  ((Ipv4 is Decodable in Text)^{tactic}) =
        parse(_)

      lazy val Localhost: Ipv4 = apply(127, 0, 0, 1)

      def apply(int: Int): Ipv4 = int

      def apply(byte0: Int, byte1: Int, byte2: Int, byte3: Int): Ipv4 =
        ((byte0 & 255) << 24) + ((byte1 & 255) << 16) + ((byte2 & 255) << 8) + (byte3 & 255)

      def parse(text: Text): Ipv4 raises IpAddressError =
        val bytes = text.cut(t".")

        if bytes.length == 4 then
          mitigate:
            case error@NumberError(text, _, _) =>
              given diagnostics: Diagnostics = error.diagnostics
              IpAddressError(Ipv4ByteNotNumeric(text))

          . protect:
              bytes.map(Decodable.int.decoded(_)).pipe: bytes =>
                for byte <- bytes do
                  if !(0 <= byte <= 255)
                  then abort(IpAddressError(Ipv4ByteOutOfRange(byte)))

                Ipv4(bytes(0).toByte, bytes(1).toByte, bytes(2).toByte, bytes(3).toByte)

        else
          abort(IpAddressError(Ipv4WrongNumberOfGroups(bytes.length)))

    object MacAddress:
      import MacAddressError.Reason.*

      inline given underlying: Underlying[MacAddress, Long] = !!
      given showable: MacAddress is Showable = _.text
      given encodable: MacAddress is Encodable in Text = _.text
      given decoder: (tactic: Tactic[MacAddressError])
      =>  ((MacAddress is Decodable in Text)^{tactic}) =
        parse(_)

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
              abort(MacAddressError(NotHex(index, head)))

            recur(tail, index + 1, (acc << 8) + value)

        recur(groups)


      def apply(byte0: Byte, byte1: Byte, byte2: Byte, byte3: Byte, byte4: Byte, byte5: Byte)
      :   MacAddress =

        def recur(todo: List[Byte], done: Long): Long = todo match
          case head :: tail => recur(tail, (done << 8) + head)
          case Nil          => done

        recur(List(byte0, byte1, byte2, byte3, byte4, byte5), 0L)

    object Port:
      inline given underlying: Underlying[Port, Int] = !!
      given showable: Port is Showable = _.number.show
      given encodable: Port is Encodable in Text = _.number.show

      // `Self` is invariant in a typeclass, so the bare-`Port` instances above do not cover a
      // transport-refined `Port over transport` (e.g. `Port over Tcp`); provide it explicitly.
      given showableOver: [transport] => (Port over transport) is Showable = _.number.show

      given decodable: (numberTactic: Tactic[NumberError], portTactic: Tactic[PortError])
      =>  ((Port is Decodable in Text)^{numberTactic, portTactic}) =
        text => apply(text.as[Int])

      def unsafe[transport](value: Int): Port over transport =
        value.asInstanceOf[Port over transport]

      // A numbered port. The transport is taken from an explicit type argument
      // (`Port[Udp](8237)`) or inferred from the expected type (`Port(8237)` where a
      // `Port over Udp` is wanted).
      def apply[transport](value: Int): (Port over transport) raises PortError =
        if 1 <= value <= 65535 then value.asInstanceOf[Port over transport]
        else abort(PortError())

      // An unused port supplied by the OS: bind an ephemeral probe socket of the
      // right kind for the transport, then release it so the caller can rebind.
      // Subject to a benign TOCTOU race.
      def apply[transport]()(using allocatable: transport is Allocatable): Port over transport =
        allocatable.unused().asInstanceOf[Port over transport]


    extension (port: Port)
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

  def subnetPrefix(text: Text, max: Int)(outOfRange: Int => Reason)
  :   Int raises IpAddressError =

    val prefix =
      mitigate:
        case error@NumberError(_, _, _) =>
          given diagnostics: Diagnostics = error.diagnostics
          IpAddressError(SubnetPrefixNotNumeric(text))

      . protect:
          Decodable.int.decoded(text)

    if !(0 <= prefix <= max) then abort(IpAddressError(outOfRange(prefix)))
    prefix

  object Ipv4Subnet:
    given showable: Ipv4Subnet is Showable = subnet => t"${subnet.ipv4}/${subnet.size}"
    given encodable: Ipv4Subnet is Encodable in Text = _.show
    given decodable: (tactic: Tactic[IpAddressError])
    =>  ((Ipv4Subnet is Decodable in Text)^{tactic}) =
      parse(_)

    def parse(text: Text): Ipv4Subnet raises IpAddressError =
      text.cut(t"/").to(List) match
        case List(address, prefixText) =>
          Ipv4.parse(address).subnet(subnetPrefix(prefixText, 32)(Ipv4SubnetPrefixOutOfRange(_)))

        case other =>
          abort(IpAddressError(SubnetWrongFormat(other.length)))

  case class Ipv4Subnet(ipv4: Ipv4, size: Int)

  object Ipv6:
    lazy val Localhost: Ipv6 = apply(0, 0, 0, 0, 0, 0, 0, 1)

    given hostShowable: urticose.Host is Showable =
      case host: Hostname          => host.show
      case ipv6: Ipv6              => t"[${ipv6.show}]"
      case ipv4: (Ipv4 @unchecked) => ipv4.show

    given hostDecodable: (tactic: Tactic[HostnameError])
    =>  ((urticose.Host is Decodable in Text)^{tactic}) = text =>
      safely(text.as[Ipv6]).or(safely(text.as[Ipv4])).or(text.as[Hostname])

    given decodable: (tactic: Tactic[IpAddressError])
    =>  ((Ipv6 is Decodable in Text)^{tactic}) =
      parse(_)

    given toExpr: ToExpr[Ipv6]:
      def apply(ipv6: Ipv6)(using Quotes): Expr[Ipv6] =
        '{Ipv6(${Expr(ipv6.highBits)}, ${Expr(ipv6.lowBits)})}

    given showable: Ipv6 is Showable = ip =>
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

      text.lower.chars.each: char =>
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
          then
            raise(IpAddressError(Ipv6TooManyNonzeroGroups(leftGroups.length + rightGroups.length)))

          leftGroups ++ List.fill((8 - leftGroups.length - rightGroups.length))(t"0") ++ rightGroups

        case List(whole) =>
          val groups = whole.cut(t":")

          if groups.length != 8
          then abort(IpAddressError(Ipv6WrongNumberOfGroups(groups.length)))
          else groups.to(List)

        case _ =>
          abort(IpAddressError(Ipv6MultipleDoubleColons))

      Ipv6(pack(groups.take(4).map(parseGroup)), pack(groups.drop(4).map(parseGroup)))

  case class Ipv6(highBits: Long, lowBits: Long)

  extension (ip: Ipv6)
    def subnet(size: Int): Ipv6Subnet =
      val high =
        if size <= 0 then 0L
        else if size >= 64 then ip.highBits
        else ip.highBits & (-1L << (64 - size))

      val low =
        if size <= 64 then 0L
        else if size >= 128 then ip.lowBits
        else ip.lowBits & (-1L << (128 - size))

      Ipv6Subnet(Ipv6(high, low), size)

  object Ipv6Subnet:
    // Reference `Ipv6.showable` by name rather than summoning `Ipv6 is Showable`
    // implicitly: this given is completed eagerly when `Ipv6Subnet` is re-exported,
    // and an implicit search would re-enter the (also-exported) `Ipv6` companion.
    given showable: Ipv6Subnet is Showable = subnet =>
      t"${Ipv6.showable.text(subnet.ipv6)}/${subnet.size}"

    given encodable: Ipv6Subnet is Encodable in Text = _.show
    given decodable: (tactic: Tactic[IpAddressError])
    =>  ((Ipv6Subnet is Decodable in Text)^{tactic}) =
      parse(_)

    def parse(text: Text): Ipv6Subnet raises IpAddressError =
      text.cut(t"/").to(List) match
        case List(address, prefixText) =>
          Ipv6.parse(address).subnet(subnetPrefix(prefixText, 128)(Ipv6SubnetPrefixOutOfRange(_)))

        case other =>
          abort(IpAddressError(SubnetWrongFormat(other.length)))

  case class Ipv6Subnet(ipv6: Ipv6, size: Int)


  def emailAddress(context: Expr[StringContext]): Macro[EmailAddress] = abortive:
    val text: Text = context.valueOrAbort.parts.head.tt
    val address = EmailAddress.parse(text)

    val localPart: Expr[LocalPart] = address.localPart match
      case LocalPart.Quoted(text)   => '{LocalPart.Quoted(${Expr(text)})}
      case LocalPart.Unquoted(text) => '{LocalPart.Unquoted(${Expr(text)})}

    address.domain.asMatchable.absolve match
      case ipv6: Ipv6         => '{EmailAddress(Unset, $localPart, ${Expr(ipv6)})}
      case hostname: Hostname => '{EmailAddress(Unset, $localPart, ${Expr(hostname)})}
      case ipv4: Int          => '{EmailAddress(Unset, $localPart, Ipv4(${Expr(ipv4)}))}

  def hostname(context: Expr[StringContext]): Macro[Hostname] = abortive:
    Expr(Hostname.parse(context.valueOrAbort.parts.head.tt))

  def portService(context: Expr[StringContext], tcp: Boolean)
  :   Macro[Port] =

    import quotes.reflect.*

    val id = context.valueOrAbort.parts.head.tt
    val portType = if tcp then t"TCP" else t"UDP"

    safely(id.as[Int]).let: portNumber =>
      if 1 <= portNumber <= 65535 then
        ConstantType(IntConstant(portNumber)).asType.absolve match
          case '[number] =>
            if tcp then '{Port.unsafe(${Expr(portNumber)}).asInstanceOf[Port over Tcp of number]}
            else '{Port.unsafe(${Expr(portNumber)}).asInstanceOf[Port over Udp of number]}

      else
        halt(340, m"the $portType port number ${portNumber} is not in the range 1-65535")

    . or:
        serviceNames.at((tcp, id)).lay(halt(915, m"$id is not a valid $portType port")):
          case port: Int =>
            ConstantType(IntConstant(port)).asType.absolve match
              case '[type number <: Int; number] =>
                if tcp then '{Port.unsafe(${Expr(port)}).asInstanceOf[Port over Tcp of number]}
                else '{Port.unsafe(${Expr(port)}).asInstanceOf[Port over Udp of number]}


  def ip(context: Expr[StringContext]): Macro[Ipv4 | Ipv6] =
    val text = Text(context.valueOrAbort.parts.head)

    abortive:
      if text.contains(t".") then
        val ipv4 = text.as[Ipv4]
        '{Ipv4(${Expr(ipv4.byte0)}, ${Expr(ipv4.byte1)}, ${Expr(ipv4.byte2)}, ${Expr(ipv4.byte3)})}

      else
        val ipv6 = text.as[Ipv6]
        '{Ipv6(${Expr(ipv6.highBits)}, ${Expr(ipv6.lowBits)})}

  def subnet(context: Expr[StringContext]): Macro[Ipv4Subnet | Ipv6Subnet] =
    val text = Text(context.valueOrAbort.parts.head)

    abortive:
      if text.cut(t"/").head.contains(t".") then
        val subnet = Ipv4Subnet.parse(text)
        '{Ipv4Subnet(Ipv4(${Expr(subnet.ipv4.int)}), ${Expr(subnet.size)})}

      else
        val subnet = Ipv6Subnet.parse(text)
        val ipv6 = '{Ipv6(${Expr(subnet.ipv6.highBits)}, ${Expr(subnet.ipv6.lowBits)})}
        '{Ipv6Subnet($ipv6, ${Expr(subnet.size)})}

  def mac(context: Expr[StringContext]): Macro[MacAddress] = abortive:
    val macAddress = context.valueOrAbort.parts.head.tt.as[MacAddress]
    '{MacAddress(${Expr(macAddress.long)})}
