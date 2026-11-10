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

import soundness.*

import codepages.utf8Codepage
import strategies.throwUnsafely

object DetectionTests extends Suite("mime-detection", m"Gesticulate detection tests"):
  given gif: ("image/gif" is Detectable under Likely over Probe to Data) =
    Detectable.signature(t"GIF8".in[Data])

  given json: ("application/json" is Detectable under Plausible over Probe to Data) =
    Detectable(_.next() == '{'.toByte)

  // A format which is identified by its last four bytes, as a ZIP file is by its index, but
  // which usually begins with a recognisable header.
  given tailedStream: ("application/zip" is Detectable under Plausible over Probe to Data) =
    Detectable.signature(t"HEAD".in[Data])

  given tailedExpanse: ("application/zip" is Detectable under Likely over Expanse to Data) =
    Detectable.positional: expanse =>
      expanse.size >= 4 && expanse.read(expanse.size - 4, 4).utf8 == t"TAIL"

  given throwing: ("image/png" is Detectable under Likely over Probe to Data) =
    Detectable: probe =>
      probe.take(4)
      throw java.lang.IllegalStateException("detector failed")

  // Reads the whole stream, as only a detector which claims certainty should.
  given whole: ("text/csv" is Detectable under Certain over Probe to Data) =
    Detectable: probe =>
      def recur(): Boolean = probe.next().lay(true) { byte => byte != 0 && recur() }
      recur()

  def split(text: Text, size: Int): Chain[Data] =
    val data: Data = text.in[Data]

    def recur(offset: Int): Chain[Data] =
      if offset >= data.length then Chain() else
        val end = (offset + size).min(data.length)
        data.segment(offset.z till end.z) #:: recur(end)

    recur(0)

  def rest(cursor: Cursor[Data, {}]^): Text =
    summon[Data is Aggregable by Data].accept(streamOf(cursor)).utf8

  def run(): Unit =
    suite(m"Sequential detection"):
      test(m"a stream with the signature is detected"):
        val cursor = Cursor(Iterator(t"GIF89a-and-more".in[Data]))

        cursor match
          case Media["image/gif"](gif) => t"gif"
          case _                       => t"unknown"

      . assert(_ == t"gif")

      test(m"a stream without the signature is not detected"):
        val cursor = Cursor(Iterator(t"JFIF-and-more".in[Data]))

        cursor match
          case Media["image/gif"](gif) => t"gif"
          case _                       => t"unknown"

      . assert(_ == t"unknown")

      test(m"a later case sees the stream from its start"):
        val cursor = Cursor(Iterator(t"{\"key\": 1}".in[Data]))

        cursor match
          case Media["image/gif"](gif)         => t"gif"
          case Media["application/json"](json) => t"json"
          case _                               => t"unknown"

      . assert(_ == t"json")

      test(m"the bound detection carries its certainty"):
        val cursor = Cursor(Iterator(t"GIF89a-and-more".in[Data]))

        cursor match
          case Media["image/gif"](gif) =>
            val typed: Detection of "image/gif" under Likely to Data = gif
            typed.mediaType

          case _ =>
            media"text/plain"

      . assert(_ == media"image/gif")

      test(m"a demanded certainty is met by a likely detector"):
        val cursor = Cursor(Iterator(t"GIF89a-and-more".in[Data]))

        cursor match
          case Media["image/gif", Likely](gif) => t"gif"
          case _                               => t"unknown"

      . assert(_ == t"gif")

      for size <- List(1, 2, 3, 5, 64) do
        test(m"a signature split into blocks of $size is detected"):
          val cursor = Cursor(split(t"GIF89a-and-more", size))

          cursor match
            case Media["image/gif"](gif) => t"gif"
            case _                       => t"unknown"

        . assert(_ == t"gif")

        test(m"all of a stream in blocks of $size is read after a match"):
          val cursor = Cursor(split(t"GIF89a-and-more", size))

          cursor match
            case Media["image/gif"](gif) => gif.read(cursor).utf8
            case _                       => t"unknown"

        . assert(_ == t"GIF89a-and-more")

        test(m"all of a stream in blocks of $size is read after no match"):
          val cursor = Cursor(split(t"JFIF-and-more", size))

          cursor match
            case Media["image/gif"](gif) => t"gif"
            case _                       => rest(cursor)

        . assert(_ == t"JFIF-and-more")

      test(m"a stream shorter than the signature is not detected"):
        val cursor = Cursor(Iterator(t"GI".in[Data]))

        cursor match
          case Media["image/gif"](gif) => t"gif"
          case _                       => rest(cursor)

      . assert(_ == t"GI")

      test(m"an empty stream is not detected"):
        val cursor = Cursor(Iterator[Data]())

        cursor match
          case Media["image/gif"](gif) => t"gif"
          case _                       => t"unknown"

      . assert(_ == t"unknown")

      test(m"a failing detector leaves the stream at its start"):
        val cursor = Cursor(split(t"PNG-and-more", 2))

        val failed =
          try
            cursor match
              case Media["image/png"](png) => false
              case _                       => false
          catch case error: java.lang.IllegalStateException => true

        (failed, cursor.position.n0, rest(cursor))

      . assert(_ == (true, 0, t"PNG-and-more"))

      test(m"a detector which reads everything leaves the stream at its start"):
        val cursor = Cursor(split(t"a,b,c\n1,2,3\n", 3))

        cursor match
          case Media["text/csv", Certain](csv) => (cursor.position.n0, csv.read(cursor).utf8)
          case _                               => (-1, t"unknown")

      . assert(_ == (0, t"a,b,c\n1,2,3\n"))

      test(m"a detection is accepted where its certainty is demanded"):
        def demand(detection: Detection of "image/gif" under (? <: Likely)): Text = t"accepted"
        val cursor = Cursor(Iterator(t"GIF89a-and-more".in[Data]))

        cursor match
          case Media["image/gif"](gif) => demand(gif)
          case _                       => t"unknown"

      . assert(_ == t"accepted")

    suite(m"Compile errors"):
      test(m"a media type with no detector in scope does not compile"):
        demilitarize:
          Cursor(Iterator(t"abc".in[Data])) match
            case Media["image/jpeg"](jpeg) => 1
            case _                         => 0

        . map(_.message)
      . assert(_.exists(_.contains("image/jpeg cannot be detected in a stream")))

      test(m"a demanded certainty no detector meets does not compile"):
        demilitarize:
          Cursor(Iterator(t"abc".in[Data])) match
            case Media["application/json", Likely](json) => 1
            case _                                       => 0

        . map(_.message)
      . assert(_.exists(_.contains("at least Likely")))

      test(m"an unregistered media type does not compile"):
        demilitarize:
          Cursor(Iterator(t"abc".in[Data])) match
            case Media["image/jpog"](jpeg) => 1
            case _                         => 0

        . map(_.message)
      . assert(_.exists(_.contains("did you mean")))

      test(m"a stream cannot meet a certainty which needs random access"):
        demilitarize:
          Cursor(Iterator(t"abc".in[Data])) match
            case Media["application/zip", Likely](zip) => 1
            case _                                     => 0

        . map(_.message)
      . assert(_.exists(_.contains("application/zip cannot be detected in a stream")))

    suite(m"Random-access detection"):
      test(m"a stream is only plausibly the tailed format"):
        val cursor = Cursor(Iterator(t"HEAD-body-TAIL".in[Data]))

        cursor match
          case Media["application/zip"](zip) =>
            val typed: Detection of "application/zip" under Plausible to Data = zip
            t"zip"

          case _ =>
            t"unknown"

      . assert(_ == t"zip")

      test(m"data in memory is likely the tailed format"):
        val data: Data = t"prefixed-HEAD-body-TAIL".in[Data]

        data match
          case Media["application/zip", Likely](zip) =>
            val typed: Detection of "application/zip" under Likely to Data = zip
            t"zip"

          case _ =>
            t"unknown"

      . assert(_ == t"zip")

      test(m"a sequential detector reads data in memory"):
        val data: Data = t"GIF89a-and-more".in[Data]

        data match
          case Media["image/gif"](gif) => t"gif"
          case _                       => t"unknown"

      . assert(_ == t"gif")
