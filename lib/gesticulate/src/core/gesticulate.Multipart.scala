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
package gesticulate

import scala.reflect.*

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

import MultipartError.Reason

object Multipart:
  enum Disposition:
    case Inline, Attachment, FormData


  def parse[input: Streamable by Data](input: input, boundary0: Optional[Text] = Unset)
  :   Multipart raises MultipartError =

    val cursor = Cursor[Data](input.stream[Data].filter(_.nonEmpty).iterator)

    inline def datum: Byte =
      if cursor.finished then -1.toByte else cursor.datum(using Unsafe)

    val boundary: Data = cursor.hold:
      val start = cursor.mark
      if datum != '-'.toByte then raise(MultipartError(Reason.Expected('-')))
      cursor.next()
      if datum != '-'.toByte then raise(MultipartError(Reason.Expected('-')))
      cursor.next()
      cursor.seek('\r'.toByte)
      cursor.grab(start, cursor.mark)

    cursor.next()
    if datum != '\n'.toByte then raise(MultipartError(Reason.Expected('\n')))
    cursor.next()

    def headers(list: List[(Text, Text)]): Map[Text, Text] =
      if datum == '\r'.toByte then
        cursor.next()
        if datum != '\n'.toByte then raise(MultipartError(Reason.Expected('\n')))
        cursor.next()
        list.to(Map)

      else
        val key: Text = cursor.hold:
          val start = cursor.mark
          cursor.seek(':'.toByte)
          Text.ascii(cursor.grab(start, cursor.mark))
        cursor.next()
        if datum != ' '.toByte then raise(MultipartError(Reason.Expected(' ')))
        cursor.next()
        val value: Text = cursor.hold:
          val start = cursor.mark
          cursor.seek('\r'.toByte)
          Text.ascii(cursor.grab(start, cursor.mark))
        cursor.next()
        if datum != '\n'.toByte then raise(MultipartError(Reason.Expected('\n')))
        cursor.next()
        headers((key, value) :: list)

    inline def skipBytes(count: Int): Unit =
      var i = 0
      while i < count && cursor.next() do i += 1

    def body(): Stream[Data] = cursor.hold:
      val bodyStart = cursor.mark
      var bodyEnd: Optional[Cursor.Mark] = Unset
      var continue = true
      while continue do
        if cursor.finished then continue = false
        else if datum != '\r'.toByte then
          if !cursor.next() then continue = false
        else
          val matched = cursor.hold:
            val crMark = cursor.mark
            var ok = cursor.next() && datum == '\n'.toByte
            var i = 0
            while ok && i < boundary.length do
              ok = cursor.next() && datum == boundary(i)
              i += 1
            cursor.cue(crMark)
            ok
          if matched then
            bodyEnd = cursor.mark
            continue = false
          else if !cursor.next() then continue = false

      bodyEnd.let: end =>
        val out = cursor.grab(bodyStart, end)
        // Position is at the body-ending '\r'. Skip past "\r\n<boundary>" which
        // is boundary.length + 2 bytes total.
        skipBytes(boundary.length + 2)
        Stream(out)
      . or(Stream(cursor.grab(bodyStart, cursor.mark)))

    def parsePart(headers: Map[Text, Text], stream: Stream[Data]): Part =
      headers.at(t"Content-Disposition").let: disposition =>
        val parts = disposition.cut(t";").map(_.trim)

        val params: Map[Text, Text] =
          parts.drop(1).map: param =>
            param.cut(t"=", 2) match
              case List(key, value) =>
                if value.starts(t"\"") && value.ends(t"\"")
                                      then key -> value.segment(Sec thru value.pen.vouch)
                                      else key -> value

              case _ =>
                raise(MultipartError(Reason.BadDisposition)) yet (t"", t"")

          . to(Map)

        val dispositionValue = parts.prim match
          case t"inline"     => Multipart.Disposition.Inline
          case t"form-data"  => Multipart.Disposition.FormData
          case t"attachment" => Multipart.Disposition.Attachment

          case _ =>
            raise(MultipartError(Reason.BadDisposition)) yet Multipart.Disposition.FormData

        val filename = params.at(t"filename")
        val name = params.at(t"name")

        Part(dispositionValue, headers, name, filename, stream)

      . or(Part(Multipart.Disposition.FormData, Map(), Unset, Unset, stream))

    def parts(): Stream[Part] =
      val part = parsePart(headers(Nil), body())

      if cursor.finished then
        raise(MultipartError(Reason.Expected('-')))
        Stream()
      else datum match
        case '\r' =>
          if !cursor.next() || datum != '\n'.toByte
          then raise(MultipartError(Reason.Expected('\n')))

          part #:: { part.body.strict; cursor.next(); parts() }

        case '-' =>
          if !cursor.next() || datum != '-'.toByte
          then raise(MultipartError(Reason.Expected('-')))

          if !cursor.next() || datum != '\r'.toByte
          then raise(MultipartError(Reason.Expected('\r')))

          if !cursor.next() || datum != '\n'.toByte
          then raise(MultipartError(Reason.Expected('\n')))
          Stream(part)

        case other =>
          raise(MultipartError(Reason.Expected('-')))
          Stream()

    Multipart(parts())


case class Multipart(parts: Stream[Part]):
  def at(name: Text): Optional[Part] = parts.find(_.name == name).getOrElse(Unset)
