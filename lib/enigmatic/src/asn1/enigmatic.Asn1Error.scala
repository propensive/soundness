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
package enigmatic

import fulminate.*

object Asn1Error:
  object Reason:
    given communicable: Reason is Communicable =
      case Truncated(offset)         => m"the input was truncated at byte $offset"
      case IndefiniteLength(offset)  => m"an indefinite length was found at byte $offset"
      case NonMinimalLength(offset)  => m"an overlong length was found at byte $offset"
      case NonMinimalTag(offset)     => m"an overlong tag number was found at byte $offset"
      case NonMinimalInteger(offset) => m"an overlong integer was found at byte $offset"
      case EmptyInteger(offset)      => m"an integer with no content was found at byte $offset"
      case Overflow(offset)          => m"an unrepresentable length was found at byte $offset"
      case Trailing(offset)          => m"unexpected trailing bytes were found from byte $offset"
      case InvalidUtf8(offset)       => m"invalid UTF-8 was found at byte $offset"
      case ReservedTag(offset)       => m"the reserved tag number 0 was found at byte $offset"
      case BadOid(offset)            => m"a malformed object identifier was found at byte $offset"
      case OidArcOverflow(offset)    => m"an arc too large for Int was found at byte $offset"
      case UnsortedSet(offset)       => m"the set at byte $offset was not in ascending order"
      case BadTime(offset)           => m"a malformed time value was found at byte $offset"

      case BadBoolean(offset, byte) =>
        m"a boolean with the content byte ${byte.toString} was found at byte $offset"

      case BadLength(offset, tag, length) =>
        m"the tag ${tag.toString} had the invalid length ${length.toString} at byte $offset"

      case BadUnusedBits(offset, count) =>
        m"a bit string declaring ${count.toString} unused bits was found at byte $offset"

      case NotPrimitive(offset, tag) =>
        m"the tag ${tag.toString} was encoded in constructed form at byte $offset"

      case NotConstructed(offset, tag) =>
        m"the tag ${tag.toString} was encoded in primitive form at byte $offset"

  enum Reason(val number: Int) extends Clarification:
    case Truncated(offset: Long) extends Reason(1)
    case IndefiniteLength(offset: Long) extends Reason(2)
    case NonMinimalLength(offset: Long) extends Reason(3)
    case NonMinimalTag(offset: Long) extends Reason(4)
    case NonMinimalInteger(offset: Long) extends Reason(5)
    case EmptyInteger(offset: Long) extends Reason(6)
    case Overflow(offset: Long) extends Reason(7)
    case Trailing(offset: Long) extends Reason(8)
    case InvalidUtf8(offset: Long) extends Reason(9)
    case ReservedTag(offset: Long) extends Reason(10)
    case BadOid(offset: Long) extends Reason(11)
    case OidArcOverflow(offset: Long) extends Reason(12)
    case UnsortedSet(offset: Long) extends Reason(13)
    case BadTime(offset: Long) extends Reason(14)
    case BadBoolean(offset: Long, byte: Int) extends Reason(15)
    case BadLength(offset: Long, tag: Int, length: Int) extends Reason(16)
    case BadUnusedBits(offset: Long, count: Int) extends Reason(17)
    case NotPrimitive(offset: Long, tag: Int) extends Reason(18)
    case NotConstructed(offset: Long, tag: Int) extends Reason(19)

case class Asn1Error(reason: Asn1Error.Reason)(using Diagnostics)
extends Error(523, reason.number)(m"could not process the ASN.1 value because $reason")
