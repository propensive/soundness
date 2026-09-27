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

    // The boundary as it appears in a `content-type` parameter: without the leading `--`
    val boundaryText: Text = Text.ascii(boundary).skip(2)

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

    inline def skipBytes(count: Int): Unit =
      var i = 0
      while i < count && cursor.next() do i += 1

    def body(): Chain[Data] = cursor.hold:
      val bodyStart = cursor.mark
      var bodyEnd: Optional[Cursor.Mark] = Unset
      var continue = true

      while continue do
        if cursor.finished then continue = false
        else if cursor.peek != '\r' then
          if !cursor.next() then continue = false
        else
          val matched = cursor.lookahead:
            var ok = cursor.next() && cursor.peek == '\n'
            var i = 0

            while ok && i < boundary.length do
              ok = cursor.next() && cursor.peek == boundary.readUnchecked(i)
              i += 1

            ok

          if matched then
            bodyEnd = cursor.mark
            continue = false
          else if !cursor.next() then
            continue = false

      bodyEnd.let: end =>
        val out = cursor.grab(bodyStart, end)
        // Position is at the body-ending '\r'. Skip past "\r\n<boundary>" which
        // is boundary.length + 2 bytes total.
        skipBytes(boundary.length + 2)
        Chain(out)

      . or(Chain(cursor.grab(bodyStart, cursor.mark)))

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

    Multipart(parts(), boundaryText)

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

  // A boundary for a new body: random, and of characters no part is likely to contain
  def boundary(): Text = t"soundness-${java.util.UUID.randomUUID.nn.toString.nn}"

  // The body of a multipart, written out: each part between boundary lines, with its
  // `Content-Disposition` (rebuilt from the part's disposition, name and filename) and other
  // headers, and the closing boundary (RFC 2046 §5.1.1)
  given streamable: Multipart is Streamable by Data over Credit = multipart =>
    def ascii(text: Text): Data = Array.unsafeFrozen(text.s.getBytes("US-ASCII").nn)

    def disposition(part: Part): Text =
      val kind = part.disposition.or(Multipart.Disposition.FormData) match
        case Multipart.Disposition.Inline     => t"inline"
        case Multipart.Disposition.Attachment => t"attachment"
        case Multipart.Disposition.FormData   => t"form-data"

      val name = part.name.lay(t""): name => t"""; name="$name""""
      val filename = part.filename.lay(t""): filename => t"""; filename="$filename""""
      t"Content-Disposition: $kind$name$filename\r\n"

    def headers(part: Part): Text =
      part.headers.to[List].filter(_(0).lower != t"content-disposition").map: (key, value) =>
        t"$key: $value\r\n"
      . join

    def encoded(part: Part): Chain[Data] =
      val opening = ascii(t"--${multipart.boundary}\r\n${disposition(part)}${headers(part)}\r\n")
      Chain.concat(Chain.concat(Chain(opening), part.body), Chain(ascii(t"\r\n")))

    val closing = Chain(ascii(t"--${multipart.boundary}--\r\n"))
    zephyrine.Stream(Chain.concat(multipart.parts.bind(encoded(_)), closing))

case class Multipart(parts: Chain[Part], boundary: Text = Multipart.boundary()):
  def at(name: Text): Optional[Part] = parts.seek(_.name == name).or(Unset)
