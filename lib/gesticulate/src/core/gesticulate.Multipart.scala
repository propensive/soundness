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
package gesticulate

import scala.reflect.*

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import denominative.dysasymptotics.linearSize
import turbulence.*
import vacuous.*
import zephyrine.*


object Multipart:
  enum Disposition:
    case Inline, Attachment, FormData


  def parse[input: Streamable by Data over Credit](input: input, boundary0: Optional[Text] = Unset)
  :   Multipart raises Multipart.Error =

    val cursor = Cursor[Data](input.source[Data])

    inline def expected(char: Char): Diagnostics ?=> Multipart.Error =
      Multipart.Error(Multipart.Error.Reason.Expected(char))

    val boundary: Data = cursor.hold:
      val start = cursor.mark
      cursor.expect('-')(expected('-'))
      cursor.expect('-')(expected('-'))
      cursor.seek('\r'.toByte.asInstanceOf[cursor.addressable.Operand])
      cursor.grab(start, cursor.mark)

    cursor.next()
    cursor.expect('\n')(expected('\n'))

    def headers(list: List[(Text, Text)]): Map[Text, Text] =
      if cursor.peek == '\r' then
        cursor.next()
        cursor.expect('\n')(expected('\n'))
        list.to[Map]

      else
        val key: Text = cursor.hold:
          val start = cursor.mark
          cursor.seek(':'.toByte.asInstanceOf[cursor.addressable.Operand])
          Text.ascii(cursor.grab(start, cursor.mark))

        cursor.next()
        cursor.expect(' ')(expected(' '))

        val value: Text = cursor.hold:
          val start = cursor.mark
          cursor.seek('\r'.toByte.asInstanceOf[cursor.addressable.Operand])
          Text.ascii(cursor.grab(start, cursor.mark))

        cursor.next()
        cursor.expect('\n')(expected('\n'))
        // A tail-recursive re-entry over the same single-owner cursor; no aliased writer.
        scala.caps.unsafe.unsafeAssumeSeparate(headers((key, value) :: list))

    // What ends every body: a line break followed by the boundary line. Its skip table is
    // built once per message.
    val delimiter: Cursor.Delimiter =
      val bytes = new scala.Array[Byte](boundary.length + 2)
      bytes(0) = '\r'.toByte
      bytes(1) = '\n'.toByte
      System.arraycopy(Array.unsafeJvm(boundary), 0, bytes, 2, boundary.length)
      Cursor.Delimiter(Array.unsafeFrozen(bytes))

    // The body is emitted as it is scanned, one block per buffered window, so that no hold
    // spans more than a window and the cursor's buffer stays a few kilobytes however large
    // the body. Each window is searched in bulk for the delimiter. When a window has none,
    // everything but its last `delimiter.length - 1` bytes — a possible prefix of a
    // delimiter straddling the refill — is emitted, and only that tail is held while the
    // next window arrives. A body the stream ends before terminating is emitted whole;
    // `parts()` then reports the missing boundary.
    def body(): Chain[Data] =
      var blocks: List[Data] = Nil
      var scanning = true

      // Terminates by state: each pass either finds the delimiter, consumes at least one byte
      // from a refill, or exhausts the stream.
      while scanning do
        if cursor.finished then scanning = false
        else
          val found = cursor.hold:
            val start = cursor.mark
            val distance = cursor.distance(delimiter)

            val emitted =
              if distance >= 0 then distance
              else cursor.available - (delimiter.length - 1).min(cursor.available)

            if emitted > 0 then
              cursor.unsafeAdvanceBy(emitted)(using Unsafe)
              blocks = cursor.grab(start, cursor.mark) :: blocks

            distance >= 0

          if found then
            // The delimiter lies wholly within the buffer, so this stays within it.
            cursor.unsafeAdvanceBy(delimiter.length)(using Unsafe)
            scanning = false
          else
            // Hold the tail through the refill: step to the buffer's end, ask for more (which
            // compacts down to the held tail before pulling), then return to the tail's start.
            cursor.hold:
              val tail = cursor.mark
              cursor.unsafeAdvanceBy(cursor.available)(using Unsafe)

              if cursor.more then cursor.cue(tail)
              else
                if cursor.mark != tail then blocks = cursor.grab(tail, cursor.mark) :: blocks
                scanning = false

      // `blocks` holds the newest block first; prepending them in turn restores the order.
      def chain(rest: List[Data], result: Chain[Data]): Chain[Data] = rest match
        case block :: earlier => chain(earlier, block #:: result)
        case _                => result

      chain(blocks, Chain())

    def parsePart(headers: Map[Text, Text], stream: Chain[Data])
    :   Part =
      headers.at(t"Content-Disposition").let: disposition =>
        val parts = disposition.cut(t";").map(_.trim)

        val params: Map[Text, Text] =
          parts.skip(1).map: param =>
            param.cut(t"=", 2) match
              case List(key, value) =>
                // `pen` is present only when `value` has at least two characters, so a lone
                // `"` (which starts and ends with a quote) is left unstripped rather than
                // miscomputed.
                if value.starts(t"\"") && value.ends(t"\"")
                then key -> value.pen.lay(value)((pen: Ordinal) => value.segment(Sec thru pen))
                else key -> value

              case _ =>
                abort(Multipart.Error(Multipart.Error.Reason.BadDisposition))

          . to[Map]

        val dispositionValue = parts.prim match
          case t"inline"     => Multipart.Disposition.Inline
          case t"form-data"  => Multipart.Disposition.FormData
          case t"attachment" => Multipart.Disposition.Attachment

          case _ =>
            abort(Multipart.Error(Multipart.Error.Reason.BadDisposition))

        val filename = params.at(t"filename")
        val name = params.at(t"name")

        Part(dispositionValue, headers, name, filename, stream)

      . or(Part(Multipart.Disposition.FormData, Map(), Unset, Unset, stream))

    def parts(): Chain[Part] =
      val part = parsePart(headers(Nil), body())

      if cursor.finished then
        raise(expected('-'))
        Chain()
      else if cursor.peek == '\r' then
        cursor.next()
        cursor.expect('\n')(expected('\n'))

        // Lazy continuation over the same single-owner cursor; no aliased writer.
        scala.caps.unsafe.unsafeAssumeSeparate(part #:: { part.body.strict; parts() })

      else if cursor.peek == '-' then
        cursor.next()
        cursor.expect('-')(expected('-'))
        cursor.expect('\r')(expected('\r'))
        cursor.expect('\n')(expected('\n'))

        Chain(part)

      else
        raise(expected('-'))
        Chain()

    Multipart(parts())

  // MultipartError → Multipart.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Expected(char: Char) extends Reason(1)
      case StreamContinues      extends Reason(2)
      case BadBoundaryEnding    extends Reason(3)
      case MediaType            extends Reason(4)
      case BadDisposition       extends Reason(5)

    given communicable: Reason is Communicable =
      case Multipart.Error.Reason.Expected(char)    => m"the character '$char' was expected"
      case Multipart.Error.Reason.StreamContinues   => m"the stream continues beyond the last part"
      case Multipart.Error.Reason.BadBoundaryEnding => m"unexpected content followed the boundary"
      case Multipart.Error.Reason.MediaType         => m"the media type is invalid"
      case Multipart.Error.Reason.BadDisposition    => m"the `Content-Disposition` header has the wrong format"

  import Multipart.Error.Reason

  case class Error(reason: Multipart.Error.Reason)(using Diagnostics)
  extends fulminate.Error(937, reason.number)(m"multipart data could not be read because $reason")

case class Multipart(parts: Chain[Part]):
  def at(name: Text): Optional[Part] = parts.seek(_.name == name).or(Unset)
