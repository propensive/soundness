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
package zephyrine

import scala.quoted.*

import gigantism.*
import rudiments.*

object internal:
  def consume(cursor: Expr[Cursor[?]], text0: Expr[String], otherwise: Expr[Unit]): Macro[Unit] =
    import quotes.reflect.*

    val text = text0.valueOrAbort

    def recur(index: Int, checks: Expr[Unit]): Expr[Unit] =
      if index >= text.length then checks else
        val char = text(index)

        val checks2 =
          ' {
              $checks
              $cursor.next()

              $cursor.lay($otherwise): datum =>
                if datum != ${Expr(char)} then $otherwise
            }

        recur(index + 1, checks2)

    recur(0, '{()})

  // Fine-grained backpressure demand: "I can accept `count` more operands". A
  // count of elements of the medium's `Operand` type (bytes, chars or
  // records), not a byte count, so its meaning changes across a `Duct` that
  // changes medium; `Duct.translate` performs that conversion. Unboxed on the
  // hot path.
  opaque type Credit = Long

  object Credit:
    inline def apply(count: Long): Credit = count

    extension (credit: Credit)
      inline def count: Long = credit

  opaque type Datum = Int

  object Datum:
    // Sentinel for an exhausted cursor. The only `Datum` not derivable from a
    // byte or char.
    val End: Datum = -1

    // Lift an unsigned byte (`0..255` after `& 0xff`) into a `Datum`.
    inline def apply(byte: Byte): Datum = byte & 0xff

    // Lift a char into a `Datum`.
    inline def apply(char: Char): Datum = char.toInt

    // Construct from a raw `Int`. Caller is responsible for the value being a
    // valid byte/char or `-1` — used by `Cursor.peek` to wrap an `Int` it has
    // already validated.
    private[zephyrine] inline def fromRaw(int: Int): Datum = int

    inline given datumByte:  CanEqual[Datum, Byte]  = !!
    inline given byteDatum:  CanEqual[Byte, Datum] = !!
    inline given datumChar:  CanEqual[Datum, Char]  = !!
    inline given charDatum:  CanEqual[Char, Datum] = !!
    inline given datumDatum: CanEqual[Datum, Datum] = !!


    extension (datum: Datum)
      // The underlying `Int` representation. Use sparingly — anywhere it
      // leaks the `Datum` distinction is lost.
      inline def asInt: Int = datum

      // `true` if the cursor that produced this `Datum` was exhausted.
      inline def isEnd: Boolean = datum == Datum.End

