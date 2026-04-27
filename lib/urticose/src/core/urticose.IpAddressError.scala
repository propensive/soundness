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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import anticipation.*
import fulminate.*

object IpAddressError:
  enum Reason(val number: Int) extends Clarification:
    case Ipv4ByteOutOfRange(byte: Int)         extends Reason(1)
    case Ipv4ByteNotNumeric(byte: Text)        extends Reason(2)
    case Ipv4WrongNumberOfGroups(count: Int)   extends Reason(3)
    case Ipv6GroupWrongLength(group: Text)     extends Reason(4)
    case Ipv6GroupNotHex(group: Text)          extends Reason(5)
    case Ipv6TooManyNonzeroGroups(count: Int)  extends Reason(6)
    case Ipv6WrongNumberOfGroups(count: Int)   extends Reason(7)
    case Ipv6MultipleDoubleColons              extends Reason(8)

  object Reason:
    given communicable: Reason is Communicable =
      case Ipv4ByteOutOfRange(byte)       => m"the number $byte is not in the range 0-255"
      case Ipv4ByteNotNumeric(byte)       => m"the part $byte is not a number"
      case Ipv6GroupNotHex(group)         => m"the group '$group' is not a hexadecimal number"
      case Ipv6WrongNumberOfGroups(count) => m"the address has $count groups, but should have 8"
      case Ipv6MultipleDoubleColons       => m":: appears more than once"

      case Ipv4WrongNumberOfGroups(count) =>
        m"the address contains $count period-separated groups instead of 4"

      case Ipv6TooManyNonzeroGroups(count) =>
        m"the address has $count non-zero groups, which is more than is permitted"

      case Ipv6GroupWrongLength(group) =>
        m"the group is more than 4 hexadecimal characters long"

case class IpAddressError(reason: IpAddressError.Reason)(using Diagnostics)
extends Error(realm"ur", 3, reason.number)(m"the IP address is not valid because $reason")
