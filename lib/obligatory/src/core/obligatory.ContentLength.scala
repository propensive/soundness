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
package obligatory

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import vacuous.*
import zephyrine.*

object ContentLength:
  // The Language Server Protocol (and the HTTP-style base protocol it derives from) defines the
  // `Content-Length` header as a count of *bytes*. Framing therefore happens at the byte level:
  // the ASCII headers are parsed byte-by-byte, then exactly `length` bytes of body are taken and
  // yielded as `Data`, leaving any text decoding to the caller. Counting characters (as a `Text`
  // framer would) coincides with bytes only for ASCII content and corrupts the stream on any
  // multi-byte UTF-8 body. Unrecognised headers are skipped rather than rejected, since clients
  // routinely send headers beyond `Content-Length` and `Content-Type`.
  given framable: (tactic: Tactic[FrameError])
  =>  ((Data is Framable by ContentLength)^{tactic}) = input =>
    val cursor = Cursor(input)

    val cr: Byte = 13
    val lf: Byte = 10
    val colon: Byte = 58
    val space: Byte = 32

    def fail(): Nothing = abort(FrameError(FrameError.Reason.ShortRead))
    def lower(byte: Byte): Byte = if byte >= 65 && byte <= 90 then (byte + 32).toByte else byte

    val name: Text = t"content-length"

    def frame(): Optional[Data] =
      if cursor.finished then Unset else
        var length: Int = -1
        var blank = false

        while !blank do
          if cursor.finished then fail()

          if cursor.datum(using Unsafe) == cr then
            cursor.next()
            if cursor.finished || cursor.datum(using Unsafe) != lf then fail()
            cursor.next()
            blank = true
          else
            var matches = true
            var index = 0

            while !cursor.finished && cursor.datum(using Unsafe) != colon do
              val byte = lower(cursor.datum(using Unsafe))
              if index >= name.length || byte != name.s.charAt(index).toByte then matches = false
              index += 1
              cursor.next()

            if index != name.length then matches = false
            if cursor.finished then fail()
            cursor.next()

            while !cursor.finished && cursor.datum(using Unsafe) == space do cursor.next()

            var value = 0
            var digits = false

            while !cursor.finished && cursor.datum(using Unsafe) != cr do
              val byte = cursor.datum(using Unsafe)

              if matches && byte >= 48 && byte <= 57 then
                value = value*10 + (byte - 48)
                digits = true

              cursor.next()

            if cursor.finished then fail()
            cursor.next()
            if cursor.finished || cursor.datum(using Unsafe) != lf then fail()
            cursor.next()

            if matches && digits then length = value

        if length < 0 then abort(FrameError(FrameError.Reason.MalformedLength))
        cursor.take(fail())(length)

    Framable.frames[Data](frame())

sealed trait ContentLength
