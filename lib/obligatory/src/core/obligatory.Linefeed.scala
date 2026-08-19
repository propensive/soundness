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
package obligatory

import anticipation.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

object Linefeed:
  // See `CarriageReturn.framable`: explicit `new` (the Scala.js pipeline mis-infers the SAM
  // lambda's `this`) plus a relabel of the local cursor's reachability of `input`.
  given framable: (Text is Framable by Linefeed) = new Framable:
    type Self = Text
    type Operand = Linefeed

    def frames(input: Iterator[Text]^): Iterator[Text]^{input, this} =
      val cursor = Cursor(input)

      val framed =
        Framable.frames[Text]:
          cursor.hold:
            val start = cursor.mark

            if !cursor.finished && cursor.seek(Lf.toByte.asInstanceOf[cursor.addressable.Operand])
            then cursor.grab(start, cursor.mark).also(cursor.next())
            else if cursor.mark == start then Unset else cursor.grab(start, cursor.mark)

      framed.asInstanceOf[Iterator[Text]^{input, this}]

  // Byte-level counterpart of `framable`, for protocols framed as newline-delimited JSON over a
  // binary stream (such as the Agent Client Protocol). Splitting happens on the raw `Lf` byte
  // before any text decoding, so multi-byte UTF-8 content passes through unharmed; a final
  // unterminated fragment is yielded as-is. The explicit `new` and the relabelling cast follow
  // `CarriageReturn.framable`.
  //
  // The terminator is consumed with `advance()`, never `next()`: this framer reads *live*
  // streams (a subprocess's output), and `next()` refills eagerly, so a frame ending flush with
  // its chunk would block awaiting bytes beyond the terminator instead of being delivered. The
  // deferred refill is paid at the head of the following frame — the one place a block is
  // correct. (See the boundary-safety note on `Cursor.advance`.)
  given framableData: (Data is Framable by Linefeed) = new Framable:
    type Self = Data
    type Operand = Linefeed

    def frames(input: Iterator[Data]^): Iterator[Data]^{input, this} =
      val cursor = Cursor(input)

      val framed =
        Framable.frames[Data]:
          cursor.hold:
            val start = cursor.mark

            // As in `ContentLength`: the inline `grab` expansion re-infers a fresh `any.rd` on
            // the frozen chunk; the cast reasserts the frozen form, which `grab` already
            // guarantees.
            if !cursor.finished && cursor.seek(Lf.toByte.asInstanceOf[cursor.addressable.Operand])
            then cursor.grab(start, cursor.mark).asInstanceOf[Data].also(cursor.advance())
            else if cursor.mark == start then Unset
            else cursor.grab(start, cursor.mark).asInstanceOf[Data]

      framed.asInstanceOf[Iterator[Data]^{input, this}]

sealed trait Linefeed
