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

import scala.caps
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

  // A multipart body is bulk by nature: let the cursor's fills grow from the staging block
  // towards 64 KiB once the input has proved long (see `Buffering#window`), so a large upload
  // is scanned and lent in large regions while a small form stays at the staging size. Lexical
  // to this object, so it applies to `parse`'s cursor and nothing else.
  private given multipartBuffering: Buffering = new Buffering:
    def capacity(substrate: Substrate): Int = Buffering.standard.capacity(substrate)
    def depth: Int = Buffering.standard.depth

    override def window(substrate: Substrate): Int = substrate match
      case Substrate.Bytes => 65536
      case _               => capacity(substrate)


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

    // Sealed as telekinesis seals a request's body: the cursor is single-owner and reachable
    // only through each part's spring, and the neutral carrier keeps the spring's result from
    // naming the non-local cursor.
    val cursorRef: AnyRef = cursor.asInstanceOf[AnyRef]

    // A part's body, lent from the cursor up to the delimiter (`streamOf`) for as long as
    // the part is the current one. Forcing the tail of the parts chain closes it: whatever
    // remains is skipped, and it reads as empty thereafter.
    final class Body extends Spring[Data], caps.Mutable:
      private var open: Boolean = true
      update def close(): Unit = open = false

      def apply(): (Stream[Data] over Credit)^ =
        if open then streamOf(cursorRef.asInstanceOf[Cursor[Data, {}]^], delimiter)
        else Stream(Chain[Data]())

    def parsePart(headers: Map[Text, Text], stream: Spring[Data])
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
      val body: Body^ = Body()
      val part = parsePart(headers(Nil), caps.unsafe.unsafeAssumePure(body))

      // Forced once the consumer has read the body, or chosen not to: skip what remains of
      // it, close it, consume the boundary and read what follows — the next part's headers
      // or the closing `--`.
      def rest(): Chain[Part] =
        body().drain(region => range => ())
        body.close()

        if cursor.finished then
          raise(expected('-'))
          Chain()
        else
          // The delimiter lies wholly within the buffer, so this stays within it.
          cursor.unsafeAdvanceBy(delimiter.length)(using Unsafe)

          if cursor.finished then
            raise(expected('-'))
            Chain()
          else if cursor.peek == '\r' then
            cursor.next()
            cursor.expect('\n')(expected('\n'))
            // A re-entry over the same single-owner cursor; no aliased writer.
            scala.caps.unsafe.unsafeAssumeSeparate(parts())
          else if cursor.peek == '-' then
            cursor.next()
            cursor.expect('-')(expected('-'))
            cursor.expect('\r')(expected('\r'))
            cursor.expect('\n')(expected('\n'))
            Chain()
          else
            raise(expected('-'))
            Chain()

      // Lazy continuation over the same single-owner cursor; no aliased writer.
      scala.caps.unsafe.unsafeAssumeSeparate(part #:: rest())

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
