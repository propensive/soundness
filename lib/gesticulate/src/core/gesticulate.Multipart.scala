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
package gesticulate

import scala.reflect.*

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import prepositional.*
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

    inline def expected(char: Char): Diagnostics ?=> MultipartError =
      MultipartError(Reason.Expected(char))

    val boundary: Data = cursor.hold:
      val start = cursor.mark
      cursor.expect('-')(expected('-'))
      cursor.expect('-')(expected('-'))
      cursor.seek('\r'.toByte)
      cursor.grab(start, cursor.mark)

    cursor.next()
    cursor.expect('\n')(expected('\n'))

    def headers(list: List[(Text, Text)]): Map[Text, Text] =
      if cursor.peek == '\r' then
        cursor.next()
        cursor.expect('\n')(expected('\n'))
        list.to(Map)

      else
        val key: Text = cursor.hold:
          val start = cursor.mark
          cursor.seek(':'.toByte)
          Text.ascii(cursor.grab(start, cursor.mark))

        cursor.next()
        cursor.expect(' ')(expected(' '))

        val value: Text = cursor.hold:
          val start = cursor.mark
          cursor.seek('\r'.toByte)
          Text.ascii(cursor.grab(start, cursor.mark))

        cursor.next()
        cursor.expect('\n')(expected('\n'))
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
        else if cursor.peek != '\r' then
          if !cursor.next() then continue = false
        else
          val matched = cursor.lookahead:
            var ok = cursor.next() && cursor.peek == '\n'
            var i = 0

            while ok && i < boundary.length do
              ok = cursor.next() && cursor.peek == boundary(i)
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
                abort(MultipartError(Reason.BadDisposition))

          . to(Map)

        val dispositionValue = parts.prim match
          case t"inline"     => Multipart.Disposition.Inline
          case t"form-data"  => Multipart.Disposition.FormData
          case t"attachment" => Multipart.Disposition.Attachment

          case _ =>
            abort(MultipartError(Reason.BadDisposition))

        val filename = params.at(t"filename")
        val name = params.at(t"name")

        Part(dispositionValue, headers, name, filename, stream)

      . or(Part(Multipart.Disposition.FormData, Map(), Unset, Unset, stream))

    def parts(): Stream[Part] =
      val part = parsePart(headers(Nil), body())

      if cursor.finished then
        raise(expected('-'))
        Stream()
      else if cursor.peek == '\r' then
        cursor.next()
        cursor.expect('\n')(expected('\n'))

        part #:: { part.body.strict; parts() }

      else if cursor.peek == '-' then
        cursor.next()
        cursor.expect('-')(expected('-'))
        cursor.expect('\r')(expected('\r'))
        cursor.expect('\n')(expected('\n'))

        Stream(part)

      else
        raise(expected('-'))
        Stream()

    Multipart(parts())


case class Multipart(parts: Stream[Part]):
  def at(name: Text): Optional[Part] = parts.find(_.name == name).getOrElse(Unset)
