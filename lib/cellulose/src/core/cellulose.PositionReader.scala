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
package cellulose

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*
import zephyrine.*

import Character.*

class PositionReader(private var in: Stream[Text]):
  private var lastLine: Int = 0
  private var lastCol: Int = 0
  private var startLine: Int = 0
  private var startCol: Int = 0
  private var requireCr: Optional[Boolean] = Unset
  private var finished: Boolean = false

  private val buffer: StringBuilder = StringBuilder()

  private var current: Ordinal = Prim - 1

  def charStream(): Stream[Char] = Stream.continually(read()).takeWhile(_ != -1).map(_.toChar)

  private var text: Text =
    if in.nil then t""
    else
      val result = in.head
      in = in.tail
      result

  @tailrec
  private def read(): Int =
    current += 1

    text.at(current).let(_.toInt).or:
      if in.nil then -1 else
        text = in.head
        in = in.tail
        current = Prim - 1
        read()

  private def advance(char: Character): Unit = char.char match
    case '\n' =>
      lastLine += 1
      lastCol = 0

    case _ =>
      lastCol += 1

  def next()(using Tactic[ParseError]): Character =
    if finished then throw IllegalStateException("Attempted to read past the end of the stream")
    read() match
      case -1 =>
        finished = true
        Character.End

      case '\r' =>
        requireCr match
          case Unset => requireCr = true

          case false =>
            raise
              ( ParseError
                  ( Codl,
                    Codl.Position(lastLine, lastCol, 1),
                    Codl.Issue.CarriageReturnMismatch(false) ) )

          case true =>
            ()

        if read() != '\n' then
          raise
            ( ParseError
                ( Codl,
                  Codl.Position(lastLine, lastCol, 1),
                  Codl.Issue.UnexpectedCarriageReturn ) )

        Character('\n', lastLine, lastCol).tap(advance)

      case '\n' =>
        requireCr match
          case true =>
            raise
              ( ParseError
                  ( Codl,
                    Codl.Position(lastLine, lastCol, 1),
                    Codl.Issue.CarriageReturnMismatch(true) ) )

          case Unset =>
            requireCr = false

          case false =>
            ()

        Character('\n', lastLine, lastCol).tap(advance)

      case ch =>
        Character(ch, lastLine, lastCol).tap(advance)

  def start(): (Int, Int) = (startLine, startCol)
  def get(): Text = buffer.toString.show.also(buffer.clear())

  def put(char: Character): Unit =
    if buffer.nil then
      startLine = char.line
      startCol = char.column

    buffer.append(char.char)
