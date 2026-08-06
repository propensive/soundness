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
package facsimile

import proscenium.compat.*

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import rudiments.*
import vacuous.*

// ASCII85Decode (ISO 32000-2 §7.4.3): groups of five characters from `!`–`u` encode four
// bytes base-85; `z` abbreviates four zero bytes; a partial final group of n characters
// yields n−1 bytes; `~>` ends the data.
private[facsimile] object Ascii85:
  def decode(data: Data)(using Tactic[PdfError]): Data =
    val bytes = DataBuilder()
    val group = new scala.Array[Int](5)
    var members = 0
    var done = false

    def emit(count: Int): Unit =
      var value = 0L
      var j = 0

      while j < 5 do
        value = value*85 + (if j < count then group(j) else 84)
        j += 1

      var shift = 24

      while shift > 32 - count*8 do
        bytes += ((value >> shift) & 0xff).toByte
        shift -= 8

    data.survey: surveyor =>
      while !done && surveyor.more do
        surveyor.next(()): element =>
          val byte = element & 0xff

          if byte == '~' then
            done = true
          else if byte == 'z' && members == 0 then
            bytes += 0
            bytes += 0
            bytes += 0
            bytes += 0
          else if byte >= '!' && byte <= 'u' then
            group(members) = byte - '!'
            members += 1

            if members == 5 then
              emit(5)
              members = 0
          else if !CosLexer.whitespace(byte) then
            abort(PdfError(PdfError.Reason.CorruptStream(t"ASCII85Decode")))

    if members == 1 then abort(PdfError(PdfError.Reason.CorruptStream(t"ASCII85Decode")))
    if members > 1 then emit(members)

    bytes.result()
