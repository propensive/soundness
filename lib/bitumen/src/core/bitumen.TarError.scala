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
package bitumen

import anticipation.*
import fulminate.*
import hypotenuse.*

object TarError:
  enum Reason(val number: Int) extends Clarification:
    case NameTooLong(field: Text, length: Int, maximum: Int) extends Reason(1)
    case BadMagic(actual: Data) extends Reason(2)
    case BadChecksum(expected: U32, actual: U32) extends Reason(3)
    case UnknownTypeFlag(byte: Byte) extends Reason(4)
    case TruncatedStream(needed: Int, got: Int) extends Reason(5)
    case BadOctal(field: Text, data: Data) extends Reason(6)
    case BadPaxRecord(data: Data) extends Reason(7)
    case BadName(text: Text) extends Reason(8)
    case BadSparseMap(text: Text) extends Reason(9)
    case DeviceCreationUnsupported(path: Text) extends Reason(10)

  given communicable: Reason is Communicable =
    case Reason.NameTooLong(field, length, maximum) =>
      m"the $field field is $length bytes, exceeding the USTAR limit of $maximum bytes"

    case Reason.BadMagic(actual) =>
      m"the USTAR magic bytes are not valid (got ${actual.length} bytes)"

    case Reason.BadChecksum(expected, actual) =>
      m"the header checksum did not match (header recorded $expected but the recomputed value is $actual)"

    case Reason.UnknownTypeFlag(byte) =>
      val code: Int = byte.toInt & 0xff
      m"the entry type flag $code is not recognised"

    case Reason.TruncatedStream(needed, got) =>
      m"the archive stream ended unexpectedly (needed $needed bytes, got $got)"

    case Reason.BadOctal(field, _) =>
      m"the $field field did not contain a valid octal value"

    case Reason.BadPaxRecord(_) =>
      m"a PAX extended-header record could not be parsed"

    case Reason.BadName(text) =>
      m"the entry name $text is not a valid POSIX relative path"

    case Reason.BadSparseMap(text) =>
      m"the GNU sparse map $text could not be parsed"

    case Reason.DeviceCreationUnsupported(path) =>
      m"the special device entry at $path could not be created on this filesystem"

case class TarError(reason: TarError.Reason)(using Diagnostics)
extends Error(284, reason.number)(m"the TAR archive could not be read or written because $reason")
