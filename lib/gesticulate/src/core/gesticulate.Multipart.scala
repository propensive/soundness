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

    // The token as one `String` straight off the buffer, as telekinesis reads a request head:
    // not a `Data` copied out of it and then a `String` copied out of that.
    def ascii(start: Cursor.Mark, end: Cursor.Mark): Text =
      cursor.slice(start, end): (bytes, offset, length) =>
        Text
          ( java.lang.String
              ( bytes.asInstanceOf[scala.Array[Byte]], offset, length,
                java.nio.charset.StandardCharsets.US_ASCII ) )

    def headers(list: List[(Text, Text)]): Map[Text, Text] =
      if cursor.peek == '\r' then
        cursor.next()
        cursor.expect('\n')(expected('\n'))
        list.to[Map]

      else
        val key: Text = cursor.hold:
          val start = cursor.mark
          cursor.seek(':'.toByte.asInstanceOf[cursor.addressable.Operand])
          ascii(start, cursor.mark)

        cursor.next()
        cursor.expect(' ')(expected(' '))

        val value: Text = cursor.hold:
          val start = cursor.mark
          cursor.seek('\r'.toByte.asInstanceOf[cursor.addressable.Operand])
          ascii(start, cursor.mark)

        cursor.next()
        cursor.expect('\n')(expected('\n'))
        // A tail-recursive re-entry over the same single-owner cursor; no aliased writer.
        // [closure-capture] local def re-entry over single-owner cursor
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

      // The one stream over this body, which every `body()` continues — the consumer's
      // reads and then the parse's own drain when the next part is forced. A fresh stream
      // per call would not do: a finished stream has stepped over the delimiter, and a new
      // one would take the next part for the rest of this body. Cast-erased like the cursor
      // it lends; the calls are sequential.
      // [registry-lifetime] cast-erased AnyRef stream handle field
      @caps.unsafe.untrackedCaptures
      private val stream: AnyRef =
        streamOf(cursorRef.asInstanceOf[Cursor[Data, {}]^], delimiter).asInstanceOf[AnyRef]

      def apply(): (Stream[Data] over Credit)^ =
        if open then stream.asInstanceOf[(Stream[Data] over Credit)^] else Stream(Chain[Data]())

    def parsePart(headers: Map[Text, Text], stream: Spring[Data])
    :   Part =
      headers.at(t"Content-Disposition").let: disposition =>
        // `form-data; name="field"; filename="f.bin"`: the token, then `key=value`
        // parameters, read in place rather than cut, trimmed and mapped. A quoted value loses
        // its quotes only when it has both, so a lone `"` is left as it is.
        val text: String = disposition.s
        val first = text.indexOf(';')
        val token = Text((if first < 0 then text else text.substring(0, first).nn).trim.nn)

        def params(from: Int, list: List[(Text, Text)]): Map[Text, Text] =
          if from < 0 then list.to[Map] else
            val next = text.indexOf(';', from)
            val param =
              (if next < 0 then text.substring(from).nn else text.substring(from, next).nn).trim.nn

            val equals = param.indexOf('=')
            if equals < 0 then abort(Multipart.Error(Multipart.Error.Reason.BadDisposition))
            val value = param.substring(equals + 1).nn

            val unquoted =
              if value.length >= 2 && value.startsWith("\"") && value.endsWith("\"")
              then value.substring(1, value.length - 1).nn
              else value

            params
              ( if next < 0 then -1 else next + 1,
                (param.substring(0, equals).nn.tt, unquoted.tt) :: list )

        val dispositionValue = token match
          case t"inline"     => Multipart.Disposition.Inline
          case t"form-data"  => Multipart.Disposition.FormData
          case t"attachment" => Multipart.Disposition.Attachment

          case _ =>
            abort(Multipart.Error(Multipart.Error.Reason.BadDisposition))

        val parameters = params(if first < 0 then -1 else first + 1, Nil)
        val filename = parameters.at(t"filename")
        val name = parameters.at(t"name")

        Part(dispositionValue, headers, name, filename, stream)

      . or(Part(Multipart.Disposition.FormData, Map(), Unset, Unset, stream))

    def parts(): Chain[Part] =
      val body: Body^ = Body()
      // [construction-fresh] fresh Body capability laundered into part
      val part = parsePart(headers(Nil), caps.unsafe.unsafeAssumePure(body))

      // Forced once the consumer has read the body, or chosen not to: skip what remains of
      // it, close it, consume the boundary and read what follows — the next part's headers
      // or the closing `--`.
      def rest(): Chain[Part] =
        body().drain(region => range => ())
        body.close()

        // The body's stream leaves the cursor after the delimiter, or exhausted if the input
        // ended first.
        if cursor.finished then
          raise(expected('-'))
          Chain()
        else if cursor.peek == '\r' then
          cursor.next()
          cursor.expect('\n')(expected('\n'))
          // A re-entry over the same single-owner cursor; no aliased writer.
          // [closure-capture] local def parts() re-entry over single-owner cursor
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
      // [closure-capture] lazy local-def continuation over single-owner cursor
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
  def at(name: Text): Optional[Part] = parts.seek(_.name == name)
