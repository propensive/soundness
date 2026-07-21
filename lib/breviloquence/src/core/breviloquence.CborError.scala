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
package breviloquence

import fulminate.*

object CborError:
  object Primitive:
    given communicable: Primitive is Communicable =
      case Integer    => m"integer"
      case Float      => m"float"
      case ByteString => m"byte string"
      case TextString => m"text string"
      case Array      => m"array"
      case Map        => m"map"
      case Tag        => m"tag"
      case Boolean    => m"boolean"
      case Null       => m"null"
      case Undefined  => m"undefined"

  enum Primitive:
    case Integer, Float, ByteString, TextString, Array, Map, Tag, Boolean, Null, Undefined

  object Reason:
    given communicable: Reason is Communicable =
      case Truncated(offset)        => m"the input was truncated at byte $offset"
      case InvalidUtf8(offset)      => m"invalid UTF-8 was found at byte $offset"
      case Overflow(offset)         => m"an integer too large for Long was found at byte $offset"
      case UnexpectedBreak(offset)  => m"an unexpected break stop code was found at byte $offset"
      case Trailing(offset)         => m"unexpected trailing bytes were found from byte $offset"
      case OutOfRange               => m"the array index was out of range"
      case NotType(found, expected) => m"the CBOR value had type $found instead of $expected"
      case NonStringKey             => m"the map key was not a string"
      case Absent                   => m"the CBOR value was not present"

      case Reserved(offset, byte) =>
        m"a reserved CBOR head byte ${byte.toString} was found at byte $offset"

      case BadSimpleValue(offset, value) =>
        m"an invalid simple value ${value.toString} was found at byte $offset"

  enum Reason(val number: Int) extends Clarification:
    case Truncated(offset: Long) extends Reason(1)
    case Reserved(offset: Long, byte: Int) extends Reason(2)
    case BadSimpleValue(offset: Long, value: Int) extends Reason(3)
    case InvalidUtf8(offset: Long) extends Reason(4)
    case Overflow(offset: Long) extends Reason(5)
    case UnexpectedBreak(offset: Long) extends Reason(6)
    case Trailing(offset: Long) extends Reason(7)
    case OutOfRange extends Reason(8)
    case NotType(found: Primitive, expected: Primitive) extends Reason(9)
    case NonStringKey extends Reason(10)
    case Absent extends Reason(11)

case class CborError(reason: CborError.Reason)(using Diagnostics)
extends Error(595, reason.number)(m"could not process the CBOR value because $reason")
