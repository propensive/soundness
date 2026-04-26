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
import denominative.*
import distillate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import jacinta.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import telekinesis.*
import vacuous.*
import zephyrine.*

object Sse:
  given servable: LazyList[Sse] is Servable =
    import charEncoders.utf8
    Servable[LazyList[Sse]](_ => media"text/event-stream"): stream =>
      Http.Body.Streaming(stream.map(_.encode.data))

  given framable: Text is Framable by Sse = input =>
    val cursor = Cursor(input)

    def frame(start: Cursor.Mark)(using Cursor.Held): Optional[Text] = cursor.hold:
      if !cursor.finished && cursor.seek(Lf) then
        val end = cursor.mark
        cursor.next()
        cursor.lay(cursor.grab(start, end)): char =>
          if char == Lf then cursor.next() yet cursor.grab(start, end) else frame(start)
      else if cursor.mark == start then Unset else
        cursor.grab(start, cursor.mark)

    new Iterator[Text]:
      private var ready: Optional[Text] = Unset

      def hasNext: Boolean =
        if ready == Unset then ready = cursor.hold(frame(cursor.mark))
        ready != Unset

      def next(): Text = ready.asInstanceOf[Text].also:
        ready = Unset

  given jsonEncodable: Json is Encodable in Sse =
    import jsonPrinters.minimal
    json => Sse("message", List(json.show))

  given textEncodable: Text is Encodable in Sse =
    text => Sse("message", List(text))

  given decodable: Tactic[SseError] => Sse is Decodable in Text = text =>
    var event: Text = "message"
    var data: List[Text] = Nil
    var id: Optional[Text] = Unset
    var retry: Optional[Long] = Unset

    text.cut(Lf).each: line =>
      line.s.indexOf(':') match
        case -1 => raise(SseError())

        case n =>
          val value = line.skip(if line.at(n.z + 1) == ' ' then n + 2 else n + 1)

          line.keep(n) match
            case "event" => event = value
            case "data"  => data ::= value
            case "id"    => id = value
            case "retry" => retry = safely(value.decode[Long]).lest(SseError())
            case _       => raise(SseError())

    Sse(event, data.reverse, id, retry)

  given encodable: Sse is Encodable in Text =
    sse =>
      val buffer = StringBuilder()
      buffer.append("event: ")
      buffer.append(sse.event.s)
      buffer.append("\n")

      sse.data.each: line =>
        buffer.append("data: ")
        buffer.append(line)
        buffer.append("\n")

      sse.id.let: id =>
        buffer.append("id: ")
        buffer.append(id)
        buffer.append("\n")

      sse.retry.let: retry =>
        buffer.append("retry: ")
        buffer.append(retry)
        buffer.append("\n")

      buffer.append("\n")
      buffer.toString().tt

case class Sse
  ( event: Text           = "message",
    data:  List[Text]     = Nil,
    id:    Optional[Text] = Unset,
    retry: Optional[Long] = Unset )
