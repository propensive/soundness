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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8
import probably.*
import proscenium.*
import rudiments.*
import symbolism.*
import turbulence.*
import vacuous.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces

object Tests extends Suite(m"Gesticulate tests"):
  def run(): Unit =
    suite(m"Multipart parsing"):
      // Fixtures use the standard HTTP multipart format: each boundary
      // delimiter line is "--<name>\r\n", with a final terminating line of
      // "--<name>--\r\n". Tests are parameterised over a variety of block
      // sizes including very small ones to exercise cross-block detection.

      def chunks(text: Text, size: Int): Stream[Data] =
        val data: Data = text.data
        def go(offset: Int): Stream[Data] =
          if offset >= data.length then Stream() else
            val end = math.min(offset + size, data.length)
            data.slice(offset, end) #:: go(end)
        go(0)

      def bodyText(part: Part): Text = part.body.read[Data].utf8

      val blockSizes = List(1, 2, 3, 7, 13, 32, 4096)

      val singlePart =
        t"--xyz\r\n" +
        t"Content-Disposition: form-data; name=\"field1\"\r\n" +
        t"\r\n" +
        t"value1\r\n" +
        t"--xyz--\r\n"

      val twoParts =
        t"--xyz\r\n" +
        t"Content-Disposition: form-data; name=\"field1\"\r\n" +
        t"\r\n" +
        t"value1\r\n" +
        t"--xyz\r\n" +
        t"Content-Disposition: form-data; name=\"field2\"\r\n" +
        t"\r\n" +
        t"value2\r\n" +
        t"--xyz--\r\n"

      val partsWithFilename =
        t"--xyz\r\n" +
        t"Content-Disposition: form-data; name=\"file\"; filename=\"hello.txt\"\r\n" +
        t"Content-Type: text/plain\r\n" +
        t"\r\n" +
        t"file content\r\n" +
        t"--xyz--\r\n"

      for blockSize <- blockSizes do
        test(m"Single part: count at block size $blockSize"):
          Multipart.parse(chunks(singlePart, blockSize)).parts.length

        . assert(_ == 1)

        test(m"Single part: name at block size $blockSize"):
          Multipart.parse(chunks(singlePart, blockSize)).parts.head.name.or(t"")

        . assert(_ == t"field1")

        test(m"Single part: body at block size $blockSize"):
          bodyText(Multipart.parse(chunks(singlePart, blockSize)).parts.head)

        . assert(_ == t"value1")

        test(m"Two parts: count at block size $blockSize"):
          Multipart.parse(chunks(twoParts, blockSize)).parts.length

        . assert(_ == 2)

        test(m"Two parts: names at block size $blockSize"):
          Multipart.parse(chunks(twoParts, blockSize)).parts.map(_.name.or(t""))

        . assert(_ == Stream(t"field1", t"field2"))

        test(m"Two parts: bodies at block size $blockSize"):
          Multipart.parse(chunks(twoParts, blockSize)).parts.map(bodyText)

        . assert(_ == Stream(t"value1", t"value2"))

      test(m"Filename extraction"):
        Multipart.parse(chunks(partsWithFilename, 4096)).parts.head.filename.or(t"")

      . assert(_ == t"hello.txt")

      test(m"Disposition is FormData"):
        Multipart.parse(chunks(singlePart, 4096)).parts.head.disposition

      . assert(_ == Multipart.Disposition.FormData)

      test(m"Headers map preserved"):
        Multipart.parse(chunks(partsWithFilename, 4096)).parts.head.headers
          .at(t"Content-Type").or(t"")

      . assert(_ == t"text/plain")

      test(m"Body containing CR but not boundary"):
        val body =
          t"--xyz\r\n" +
          t"Content-Disposition: form-data; name=\"field\"\r\n" +
          t"\r\n" +
          t"line1\rline2\r\n" +
          t"--xyz--\r\n"
        bodyText(Multipart.parse(chunks(body, 4096)).parts.head)

      . assert(_ == t"line1\rline2")

      test(m"Body containing CRLF but not boundary"):
        val body =
          t"--xyz\r\n" +
          t"Content-Disposition: form-data; name=\"field\"\r\n" +
          t"\r\n" +
          t"line1\r\nstill body\r\n" +
          t"--xyz--\r\n"
        bodyText(Multipart.parse(chunks(body, 4096)).parts.head)

      . assert(_ == t"line1\r\nstill body")

      test(m"Body containing partial boundary prefix"):
        val body =
          t"--xyz\r\n" +
          t"Content-Disposition: form-data; name=\"field\"\r\n" +
          t"\r\n" +
          t"--xy not the boundary\r\n" +
          t"--xyz--\r\n"
        bodyText(Multipart.parse(chunks(body, 4096)).parts.head)

      . assert(_ == t"--xy not the boundary")

      test(m"Empty input throws"):
        capture[MultipartError](Multipart.parse(Stream[Data]())).reason

      . assert:
          case MultipartError.Reason.Expected(_) => true
          case _                                 => false

      test(m"Non-dash leading byte throws Expected('-')"):
        val body = t"X--xyz\r\n\r\n\r\n--xyz--\r\n"
        capture[MultipartError](Multipart.parse(Stream(body.data))).reason

      . assert(_ == MultipartError.Reason.Expected('-'))

      test(m"Single-dash leading sequence throws Expected('-')"):
        val body =
          t"-xyz\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\nv\r\n-xyz--\r\n"
        capture[MultipartError](Multipart.parse(Stream(body.data))).reason

      . assert(_ == MultipartError.Reason.Expected('-'))

    test(m"parse media type's type"):
      t"application/json".decode[MediaType].group
    . assert(_ == Media.Group.Application)

    test(m"parse media type's subtype"):
      t"application/json".decode[MediaType].subtype
    . assert(_ == Media.Subtype.Standard(t"json"))

    test(m"parse media type suffix"):
      t"application/epub+zip".decode[MediaType].suffixes
    . assert(_ == List(Media.Suffix.Zip))

    test(m"parse full media type"):
      t"application/json".decode[MediaType]
    . assert(_ == MediaType(Media.Group.Application, Media.Subtype.Standard(t"json")))

    test(m"parse full media type with parameter"):
      t"application/json; charset=UTF-8".decode[MediaType]
    . assert(_ == MediaType(Media.Group.Application, Media.Subtype.Standard(t"json"),
        parameters = List((t"charset", t"UTF-8"))))

    test(m"invalid media type"):
      capture(t"applicationjson".decode[MediaType])
    . assert(_ == MediaTypeError(t"applicationjson",
        MediaTypeError.Reason.NotOneSlash))
