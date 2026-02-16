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
package synesthesia

import soundness.*

import strategies.throwUnsafely
import autopsies.contrastExpectations
import servables.jsonIsServable
import charEncoders.utf8
import charDecoders.utf8
import textSanitizers.skip
import jsonPrinters.minimal

object Tests extends Suite(m"Obligatory Tests"):
  def run(): Unit =
    suite(m"Unframing tests"):
      test(m"Unframe by carriage-return lines"):
        Stream(t"one\rtwo\r", t"three").iterator.frames[CarriageReturn].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by carriage-return lines, without terminal line"):
        Stream(t"one\rtwo", t"\rthree\r").iterator.frames[CarriageReturn].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by linefeed lines"):
        Stream(t"one\ntwo\nth", t"ree").iterator.frames[Linefeed].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by linefeed lines, without terminal line"):
        Stream(t"one\ntwo\nthree\n").iterator.frames[Linefeed].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by cr/lf lines"):
        Stream(t"""one\r\ntwo\r\nthree""").iterator.frames[CrLf].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by cr/lf lines, without terminal line"):
        Stream(t"""one\r\ntwo\r\nthree\r\n""").iterator.frames[CrLf].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Length-prefixed chunks"):
        Stream(Data(0, 0, 0, 3, 50, 100, -100, 0, 0, 0, 1, -128, 0, 0, 0, 5, 5, 4, 3, 2, 1))
        . iterator
        . frames[LengthPrefix]
        . to(List)
        . map(_.to(List))
      . assert(_ == List(List(50, 100, -100), List(-128), List(5, 4, 3, 2, 1)))

      test(m"Content-Length-prefixed chunks"):
        val input =
          t"Content-Type: x\r\nContent-Length: 5\r\n\r\n12345Content-Length: 3\r\n\r\nabc"

        Iterator(input).frames[ContentLength].to(List)
      . assert(_ == List("12345", "abc"))

      test(m"Server-side events"):
        val input = t"data: foobar\ndata: baz\n\ndata: hello world\n\n"

        Iterator(input).frames[Sse].to(List)
      . assert(_ == List("data: foobar\ndata: baz", "data: hello world"))

      test(m"Server-side events without terminal newlines"):
        val input = t"data: foobar\ndata: baz\n\ndata: hello world"

        Iterator(input).frames[Sse].to(List)
      . assert(_ == List("data: foobar\ndata: baz", "data: hello world"))

      test(m"Typed server-side events"):
        val input = t"event: one\ndata: foobar\ndata: baz\n\ndata: hello world"

        Iterator(input).frames[Sse].map(_.decode[Sse]).to(List)
      . assert(_ == List(Sse("one", List("foobar", "baz")), Sse("message", List("hello world"))))

      test(m"Typed server-side events with more fields"):
        val input = t"event: one\nid: 123\ndata: foobar\ndata: baz\n\ndata: hello world\nretry: 54321"

        Iterator(input).frames[Sse].map(_.decode[Sse]).to(List)
      . assert(_ == List(Sse("one", List("foobar", "baz"), "123"), Sse("message", List("hello world"), Unset, 54321L)))
