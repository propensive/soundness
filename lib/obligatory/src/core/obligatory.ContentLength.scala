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
package obligatory

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*
import zephyrine.*

object ContentLength:
  given framable: Tactic[FrameError] => Text is Framable by ContentLength = input =>
    val cursor: Cursor2[Text] = Cursor2(input)

    def fail(): Nothing = abort(FrameError(FrameError.Reason.ShortRead))
    def skip(): Unit = while cursor.next() && cursor.datum(using Unsafe) == ' ' do ()

    def key(mark: Cursor2.Mark)(using Cursor2.Held): Optional[Text] = cursor.lay(fail()):
      case Cr =>
        if mark != cursor.mark then fail() else
        cursor.consume(fail())("\n")
        cursor.next()
        Unset

      case ':' =>
        cursor.grab(mark, cursor.mark).also(skip())

      case chr =>
        if cursor.next() then key(mark) else abort(FrameError(FrameError.Reason.ShortRead))

    def value(mark: Cursor2.Mark)(using Cursor2.Held): Text = cursor.lay(fail()):
      case '\r' =>
        cursor.grab(mark, cursor.mark).also:
          cursor.consume(fail())("\n")
          cursor.next()

      case char =>
        if cursor.next() then value(mark) else fail()

    def frame(length: Int): Optional[Text] =
      if cursor.finished then Unset else cursor.hold(key(cursor.mark).let(_.lower)) match
        case Unset           => cursor.take(fail())(length)
        case t"content-type" => cursor.hold(value(cursor.mark)) yet frame(length)

        case t"content-length" =>
          val raw = cursor.hold(value(cursor.mark))
          frame(safely(raw.decode[Int]).or(abort(FrameError(FrameError.Reason.MalformedLength))))

        case other =>
          abort(FrameError(FrameError.Reason.UnknownHeader))

    new Iterator[Text]:
      private var ready: Optional[Text] = Unset

      def hasNext: Boolean =
        if ready == Unset then ready = frame(0)
        ready != Unset

      def next(): Text = ready.asInstanceOf[Text].also:
        ready = Unset

sealed trait ContentLength
