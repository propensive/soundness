/*
    Nettlesome, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import spectacular.*
import gossamer.*
import perforate.*
import fulminate.*
import anticipation.*

import scala.quoted.*

object IpAddressError:
  enum Issue:
    case Ipv4ByteOutOfRange(byte: Int)
    case Ipv4WrongNumberOfBytes(count: Int)
    case Ipv6GroupWrongLength(group: Text)
    case Ipv6GroupNotHex(group: Text)
    case Ipv6TooManyNonzeroGroups(count: Int)
    case Ipv6WrongNumberOfGroups(count: Int)
    case Ipv6MultipleDoubleColons
  
  object Issue:
    given AsMessage[Issue] =
      case Ipv4ByteOutOfRange(byte)       => msg"the number $byte is not in the range 0-255"
      case Ipv4WrongNumberOfBytes(count)  => msg"the address contains $count numbers instead of 4"
      case Ipv6GroupNotHex(group)         => msg"the group '$group' is not a hexadecimal number"
      case Ipv6WrongNumberOfGroups(count) => msg"the address has $count groups, but should have 8"
      case Ipv6MultipleDoubleColons       => msg":: appears more than once"
      
      case Ipv6TooManyNonzeroGroups(count) =>
        msg"the address has $count non-zero groups, which is more than is permitted"
      
      case Ipv6GroupWrongLength(group) =>
        msg"the group is more than 4 hexadecimal characters long"

import IpAddressError.Issue, Issue.*

case class IpAddressError(issue: Issue)
extends Error(msg"the IP address is not valid because $issue")

object Nettlesome:
  object Opaques:
    opaque type Ipv4 = Int

    object Ipv4:
      def apply(byte0: Int, byte1: Int, byte2: Int, byte3: Int): Ipv4 =
        ((byte0 & 255) << 24) + ((byte1 & 255) << 16) + ((byte2 & 255) << 8) + (byte3 & 255)
      
      given debug: Debug[Ipv4] = _.show
      
      given show: Show[Ipv4] = ip =>
        t"${ip.byte0.toString}.${ip.byte1.toString}.${ip.byte2.toString}.${ip.byte3.toString}"
      
      def parse(text: Text)(using Raises[IpAddressError]): Ipv4 = text.cut(t".") match
        case List(As[Int](byte0), As[Int](byte1), As[Int](byte2), As[Int](byte3)) =>
          for byte <- List(byte0, byte1, byte2, byte3)
          do if !(0 <= byte <= 255) then raise(IpAddressError(Ipv4ByteOutOfRange(byte)))(0.toByte)

          Ipv4(byte0.toByte, byte1.toByte, byte2.toByte, byte3.toByte)
        
        case list =>
          raise(IpAddressError(Ipv4WrongNumberOfBytes(list.length)))(Ipv4(0, 0, 0, 0))
  
    extension (ip: Ipv4)
      def byte0: Int = ip >>> 24
      def byte1: Int = (ip >>> 16) & 255
      def byte2: Int = (ip >>> 8) & 255
      def byte3: Int = ip & 255
    
      @targetName("subnet")
      def /(size: Int): Ipv4Subnet = Ipv4Subnet(ip & (-1 << (32 - size)), size)

  object Ipv4Subnet:
    given Show[Ipv4Subnet] = subnet => t"${subnet.ipv4}/${subnet.size}"

  case class Ipv4Subnet(ipv4: Ipv4, size: Int)
  
  case class Ipv6(highBits: Long, lowBits: Long)

  def ip(context: Expr[StringContext])(using Quotes): Expr[Ipv4 | Ipv6] =
    val text = Text(context.valueOrAbort.parts.head)
    
    failCompilation:
      if text.contains(t".") then
        val ipv4 = Ipv4.parse(text)
        '{Ipv4(${Expr(ipv4.byte0)}, ${Expr(ipv4.byte1)}, ${Expr(ipv4.byte2)}, ${Expr(ipv4.byte3)})}
      else
        val ipv6 = Ipv6.parse(text)
        '{Ipv6(${Expr(ipv6.highBits)}, ${Expr(ipv6.lowBits)})}

  object Ipv6:
    given debug: Debug[Ipv6] = _.show
    
    given show: Show[Ipv6] = ip =>
      def unpack(long: Long, groups: List[Int] = Nil): List[Int] =
        if groups.length == 4 then groups else unpack(long >>> 16, (long & 65535).toInt :: groups)
      
      def hex(values: List[Int]): Text =
        values.map(Integer.toHexString(_).nn).map(Text(_)).join(t":")

      val groups = unpack(ip.highBits) ++ unpack(ip.lowBits)
      val (middleIndex, middleLength) = groups.longestTrain(_ == 0)

      if middleLength < 2 then hex(groups)
      else t"${hex(groups.take(middleIndex))}::${hex(groups.drop(middleIndex + middleLength))}"

    def apply
        (group0: Int, group1: Int, group2: Int, group3: Int, group4: Int, group5: Int, group6: Int,
            group7: Int): Ipv6 =
      Ipv6(pack(List(group0, group1, group2, group3)), pack(List(group4, group5, group6, group7)))
    
    def parseGroup(text: Text)(using Raises[IpAddressError]): Int =
      if text.length > 4 then raise(IpAddressError(Ipv6GroupWrongLength(text)))(())
      
      text.lower.s.foreach: char =>
        if !('0' <= char <= '9' || 'a' <= char <= 'f')
        then raise(IpAddressError(Ipv6GroupNotHex(text)))(())
      
      Integer.parseInt(text.s, 16)
    
    def pack(groups: List[Int], accumulator: Long = 0L): Long = groups match
      case Nil          => accumulator
      case head :: tail => pack(tail, (accumulator << 16) + (head & 65535))
    
    def parse(text: Text)(using Raises[IpAddressError]): Ipv6 =
      val groups: List[Text] = text.cut(t"::") match
        case List(left, right) =>
          val leftGroups = left.cut(t":").filter(_ != t"")
          val rightGroups = right.cut(t":").filter(_ != t"")
          
          if leftGroups.length + rightGroups.length > 7
          then raise(IpAddressError(Ipv6TooManyNonzeroGroups(leftGroups.length + rightGroups.length)))(())
          
          leftGroups ++ List.fill((8 - leftGroups.length - rightGroups.length))(t"0") ++ rightGroups

        case List(whole) =>
          val groups = whole.cut(t":")
          if groups.length != 8
          then raise(IpAddressError(Ipv6WrongNumberOfGroups(groups.length)))(List.fill(8)(t"0")) else groups
        
        case _ =>
          raise(IpAddressError(Ipv6MultipleDoubleColons))(List.fill(8)(t"0"))
      
      Ipv6(pack(groups.take(4).map(parseGroup)), pack(groups.drop(4).map(parseGroup)))

export Nettlesome.Ipv6
export Nettlesome.Opaques.Ipv4

extension (inline context: StringContext) transparent inline def ip(): Ipv4 | Ipv6 =
  ${Nettlesome.ip('context)}
