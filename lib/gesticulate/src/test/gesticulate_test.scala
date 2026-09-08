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

import scala.math

import soundness.*

import charEncoders.utf8Encoder
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

object Tests extends Suite(m"Gesticulate tests"):
  def run(): Unit =
    suite(m"Multipart parsing"):
      // Fixtures use the standard HTTP multipart format: each boundary
      // delimiter line is "--<name>\r\n", with a final terminating line of
      // "--<name>--\r\n". Tests are parameterised over a variety of block
      // sizes including very small ones to exercise cross-block detection.

      def chunks(text: Text, size: Int): Chain[Data] =
        val data: Data = text.in[Data]
        def go(offset: Int): Chain[Data] =
          if offset >= data.readable.length then Chain() else
            val end = math.min(offset + size, data.readable.length)
            Array.frozen(data.readable.slice(offset, end)) #:: go(end)
        go(0)

      def bodyText(part: Part): Text = part.body.read[Data].utf8

      val blockSizes = List(1, 2, 3, 7, 13, 32, 4096)

      val singlePart =
        "--xyz\r\n" +
        "Content-Disposition: form-data; name=\"field1\"\r\n" +
        "\r\n" +
        "value1\r\n" +
        "--xyz--\r\n"

      val twoParts =
        "--xyz\r\n" +
        "Content-Disposition: form-data; name=\"field1\"\r\n" +
        "\r\n" +
        "value1\r\n" +
        "--xyz\r\n" +
        "Content-Disposition: form-data; name=\"field2\"\r\n" +
        "\r\n" +
        "value2\r\n" +
        "--xyz--\r\n"

      val partsWithFilename =
        "--xyz\r\n" +
        "Content-Disposition: form-data; name=\"file\"; filename=\"hello.txt\"\r\n" +
        "Content-Type: text/plain\r\n" +
        "\r\n" +
        "file content\r\n" +
        "--xyz--\r\n"

      for blockSize <- blockSizes do
        test(m"Single part: count at block size $blockSize"):
          Multipart.parse(chunks(singlePart, blockSize)).parts.stdlib.length

        . assert(_ == 1)

        test(m"Single part: name at block size $blockSize"):
          Multipart.parse(chunks(singlePart, blockSize)).parts.stdlib.head.name.or(t"")

        . assert(_ == "field1")

        test(m"Single part: body at block size $blockSize"):
          bodyText(Multipart.parse(chunks(singlePart, blockSize)).parts.stdlib.head)

        . assert(_ == "value1")

        test(m"Two parts: count at block size $blockSize"):
          Multipart.parse(chunks(twoParts, blockSize)).parts.stdlib.length

        . assert(_ == 2)

        test(m"Two parts: names at block size $blockSize"):
          Multipart.parse(chunks(twoParts, blockSize)).parts.map(_.name.or(t""))

        . assert(_ == Chain("field1", "field2"))

        test(m"Two parts: bodies at block size $blockSize"):
          Multipart.parse(chunks(twoParts, blockSize)).parts.map(bodyText)

        . assert(_ == Chain("value1", "value2"))

      test(m"Filename extraction"):
        Multipart.parse(chunks(partsWithFilename, 4096)).parts.stdlib.head.filename.or(t"")

      . assert(_ == "hello.txt")

      test(m"Disposition is FormData"):
        Multipart.parse(chunks(singlePart, 4096)).parts.stdlib.head.disposition

      . assert(_ == Multipart.Disposition.FormData)

      test(m"Headers map preserved"):
        Multipart.parse(chunks(partsWithFilename, 4096)).parts.stdlib.head.headers.at("Content-Type").or(t"")

      . assert(_ == "text/plain")

      test(m"Body containing CR but not boundary"):
        val body =
          "--xyz\r\n" +
          "Content-Disposition: form-data; name=\"field\"\r\n" +
          "\r\n" +
          "line1\rline2\r\n" +
          "--xyz--\r\n"
        bodyText(Multipart.parse(chunks(body, 4096)).parts.stdlib.head)

      . assert(_ == "line1\rline2")

      test(m"Body containing CRLF but not boundary"):
        val body =
          "--xyz\r\n" +
          "Content-Disposition: form-data; name=\"field\"\r\n" +
          "\r\n" +
          "line1\r\nstill body\r\n" +
          "--xyz--\r\n"
        bodyText(Multipart.parse(chunks(body, 4096)).parts.stdlib.head)

      . assert(_ == "line1\r\nstill body")

      test(m"Body containing partial boundary prefix"):
        val body =
          "--xyz\r\n" +
          "Content-Disposition: form-data; name=\"field\"\r\n" +
          "\r\n" +
          "--xy not the boundary\r\n" +
          "--xyz--\r\n"
        bodyText(Multipart.parse(chunks(body, 4096)).parts.stdlib.head)

      . assert(_ == "--xy not the boundary")

      test(m"Empty input throws"):
        capture[Multipart.Error](Multipart.parse(Chain[Data]())).reason

      . assert:
          case Multipart.Error.Reason.Expected(_) => true
          case _                                 => false

      test(m"Non-dash leading byte throws Expected('-')"):
        val body = "X--xyz\r\n\r\n\r\n--xyz--\r\n"
        capture[Multipart.Error](Multipart.parse(Chain(body.in[Data]))).reason

      . assert(_ == Multipart.Error.Reason.Expected('-'))

      test(m"Single-dash leading sequence throws Expected('-')"):
        val body =
          "-xyz\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\nv\r\n-xyz--\r\n"
        capture[Multipart.Error](Multipart.parse(Chain(body.in[Data]))).reason

      . assert(_ == Multipart.Error.Reason.Expected('-'))

    test(m"parse media type's type"):
      "application/json".as[MediaType].group
    . assert(_ == Media.Group.Application)

    test(m"parse media type's subtype"):
      "application/json".as[MediaType].subtype
    . assert(_ == Media.Subtype.Standard("json"))

    test(m"parse media type suffix"):
      "application/epub+zip".as[MediaType].suffixes
    . assert(_ == List(Media.Suffix.Zip))

    test(m"parse full media type"):
      "application/json".as[MediaType]
    . assert(_ == MediaType(Media.Group.Application, Media.Subtype.Standard("json")))

    test(m"parse full media type with parameter"):
      "application/json; charset=UTF-8".as[MediaType]
    . assert(_ == MediaType(Media.Group.Application, Media.Subtype.Standard("json"),
        parameters = List((t"charset", t"UTF-8"))))

    test(m"invalid media type"):
      capture("applicationjson".as[MediaType])
    . assert(_ == MediaType.Error("applicationjson",
        MediaType.Error.Reason.NotOneSlash))
