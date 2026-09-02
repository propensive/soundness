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

import scala.caps

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import jacinta.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import telekinesis.*
import turbulence.*
import vacuous.*
import zephyrine.*

object Sse:
  given servable: Chain[Sse] is Servable =
    import charEncoders.utf8Encoder

    Servable[Chain[Sse]](_ => media"text/event-stream"): stream =>
      Http.Body.Flowing(() => zephyrine.Stream(stream.map(_.encode.in[Data])))

  given framable: Text is Framable by Sse = input =>
    // The frame reader owns its cursor exclusively for the whole parse.
    scala.caps.unsafe.unsafeAssumeSeparate:
     val cursor = Cursor(input)

     def frame(start: Cursor.Mark)(using Cursor.Held): Optional[Text] = cursor.hold:
       if !cursor.finished && cursor.seek(Lf.toByte.asInstanceOf[cursor.addressable.Operand]) then
         val end = cursor.mark
         cursor.next()

         cursor.lay(cursor.grab(start, end)): char =>
           if char == Lf then cursor.next() yet cursor.grab(start, end) else frame(start)
       else if cursor.mark == start then
         Unset
       else
         cursor.grab(start, cursor.mark)

     Framable.frames[Text]:
       cursor.hold(frame(cursor.mark))

  given jsonEncodable: Json is Encodable in Sse =
    import formatting.compactJsonFormatting
    json => Sse("message", List(json.show))

  given textEncodable: Text is Encodable in Sse =
    text => Sse("message", List(text))

  given decodable: Tactic[Sse.Error] => Sse is Decodable in Text =
    // The decode lambda closes over the resolution-scoped tactic, which shares the
    // instance's given-resolution lifetime; laundered pure per the codec-thunk seal
    // pattern (see rep/DECISIONS.md).
    caps.unsafe.unsafeAssumePure: text =>
      var event: Text = "message"
      var data: List[Text] = Nil
      var id: Optional[Text] = Unset
      var retry: Optional[Long] = Unset

      text.cut(Lf).each: line =>
        line.offsetOf(t":").lay(raise(Sse.Error(Sse.Error.Reason.MalformedField))): ordinal =>
          val n = ordinal.n0
          val value = line.skip(if line(n.z + 1) == ' ' then n + 2 else n + 1)

          line.keep(n) match
            case "event" => event = value
            case "data"  => data ::= value
            case "id"    => id = value

            case "retry" =>
              // A bad retry value records its error and leaves `retry` unset, so the other
              // fields of the frame keep accruing their own errors.
              retry = safely(value.as[Long]).or:
                raise(Sse.Error(Sse.Error.Reason.BadRetryValue))
                Unset

            case _ => raise(Sse.Error(Sse.Error.Reason.UnknownField))

      // Guarded: a frame with any malformed field is never delivered to the consumer as a
      // half-populated but valid-looking event.
      guard(Sse(event, data.reverse, id, retry))

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

  // SseError → Sse.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case MalformedField   extends Reason(1)
      case UnknownField     extends Reason(2)
      case BadRetryValue    extends Reason(3)
      case CapacityExceeded extends Reason(4)

    given communicable: Reason is Communicable =
      case Reason.MalformedField   => m"a line did not contain the expected `field: value` separator"
      case Reason.BadRetryValue    => m"the `retry` field value could not be parsed as an integer"

      case Reason.UnknownField =>
        m"the field name was not one of `event`, `data`, `id`, or `retry`"

      case Reason.CapacityExceeded =>
        m"the requested replay range exceeded the source buffer capacity"

  case class Error(reason: Sse.Error.Reason)(using Diagnostics)
  extends fulminate.Error(350, reason.number)(m"the server-sent event was not valid because $reason")

  // SseSource → Sse.Source
  class Source(capacity: Int):
    private val mutex: Mutex = Mutex()
    @scala.caps.unsafe.untrackedCaptures
    private val buffer: scala.Array[Sse] = new scala.Array(capacity)

    @scala.caps.unsafe.untrackedCaptures
    private var current: Int = 0
    @scala.caps.unsafe.untrackedCaptures
    private var spool: Relay[Sse] = Relay()

    def put[entity: Encodable in Sse](value: entity): Unit = mutex:
      val sse = value.encode.copy(id = current.show)
      buffer.asInstanceOf[scala.Array[Sse]^](current%capacity) = sse
      spool.put(sse)
      current += 1

    def stream(start: Optional[Int] = Unset): Chain[Sse] raises Error = mutex:
      start.let: start =>
        spool.stop()
        spool = Relay()

        if current - start - 1 > capacity then abort(Error(Error.Reason.CapacityExceeded)) else
          ((start + 1) until current).map: index => spool.put(buffer(index%capacity))

      Chain.from(spool.stream.records)

case class Sse
  ( event: Text           = "message",
    data:  List[Text]     = Nil,
    id:    Optional[Text] = Unset,
    retry: Optional[Long] = Unset )
