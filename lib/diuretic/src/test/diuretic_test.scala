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
package diuretic

import soundness.*

import java.io as ji
import java.net as jn
import java.nio.file as jnf
import java.time as jt
import java.util as ju

object Tests extends Suite(m"Diuretic Tests"):
  def run(): Unit =
    suite(m"java.time.Instant tests"):
      test(m"Instantiate an Instant from epoch milliseconds"):
        JavaTimeInstant(1000000000000L)
      . assert(_ == jt.Instant.ofEpochMilli(1000000000000L))

      test(m"Abstract an Instant to epoch milliseconds"):
        JavaTimeInstant.genericize(jt.Instant.ofEpochMilli(1000000000000L).nn)
      . assert(_ == 1000000000000L)

      test(m"Instant conversion round-trips"):
        JavaTimeInstant.genericize(JavaTimeInstant(1234567890123L))
      . assert(_ == 1234567890123L)

      test(m"The epoch is zero milliseconds"):
        JavaTimeInstant.genericize(jt.Instant.EPOCH.nn)
      . assert(_ == 0L)

      test(m"An Instant before the epoch is negative"):
        JavaTimeInstant.genericize(JavaTimeInstant(-1000L))
      . assert(_ == -1000L)

    suite(m"java.util.Date tests"):
      test(m"Instantiate a Date from epoch milliseconds"):
        JavaUtilDate(1000000000000L)
      . assert(_ == ju.Date(1000000000000L))

      test(m"Abstract a Date to epoch milliseconds"):
        JavaUtilDate.genericize(ju.Date(1000000000000L))
      . assert(_ == 1000000000000L)

      test(m"Date conversion round-trips"):
        JavaUtilDate.genericize(JavaUtilDate(-42L))
      . assert(_ == -42L)

    suite(m"Long instant and duration tests"):
      test(m"A Long instant is itself"):
        JavaLongInstant(99L)
      . assert(_ == 99L)

      test(m"Abstracting a Long instant is the identity"):
        JavaLongInstant.genericize(99L)
      . assert(_ == 99L)

      test(m"A Long duration is itself"):
        JavaLongDuration(99L)
      . assert(_ == 99L)

      test(m"Abstracting a Long duration is the identity"):
        JavaLongDuration.genericize(99L)
      . assert(_ == 99L)

    suite(m"java.nio.file.Path tests"):
      test(m"Instantiate a Path from text"):
        JavaNioPath(t"/tmp/example")
      . assert(_ == jnf.Paths.get("/tmp/example").nn)

      test(m"Abstract an absolute Path to text"):
        JavaNioPath.genericize(jnf.Paths.get("/tmp/example").nn)
      . assert(_ == t"/tmp/example")

      test(m"A relative Path is abstracted as an absolute path"):
        JavaNioPath.genericize(jnf.Paths.get("example").nn).starts(t"/")
      . assert(_ == true)

      test(m"Path conversion round-trips for an absolute path"):
        JavaNioPath.genericize(JavaNioPath(t"/tmp/example"))
      . assert(_ == t"/tmp/example")

      test(m"Redundant separators are normalized away"):
        JavaNioPath.genericize(JavaNioPath(t"/tmp//example"))
      . assert(_ == t"/tmp/example")

    suite(m"java.io.File tests"):
      test(m"Instantiate a File from text"):
        JavaIoFile(t"/tmp/example")
      . assert(_ == ji.File("/tmp/example"))

      test(m"Abstract an absolute File to text"):
        JavaIoFile.genericize(ji.File("/tmp/example"))
      . assert(_ == t"/tmp/example")

      test(m"A relative File is abstracted as an absolute path"):
        JavaIoFile.genericize(ji.File("example")).starts(t"/")
      . assert(_ == true)

      test(m"File conversion round-trips for an absolute path"):
        JavaIoFile.genericize(JavaIoFile(t"/tmp/example"))
      . assert(_ == t"/tmp/example")

    suite(m"java.net.URL tests"):
      test(m"Instantiate a URL from text"):
        JavaNetUrl(t"https://soundness.dev/index.html")
      . assert(_.toString.nn == "https://soundness.dev/index.html")

      test(m"Abstract a URL to text"):
        JavaNetUrl.genericize(jn.URI("https://soundness.dev/").nn.toURL().nn)
      . assert(_ == t"https://soundness.dev/")

      test(m"URL conversion round-trips"):
        JavaNetUrl.genericize(JavaNetUrl(t"https://soundness.dev/index.html"))
      . assert(_ == t"https://soundness.dev/index.html")

      test(m"A URL with a query string round-trips"):
        JavaNetUrl.genericize(JavaNetUrl(t"https://soundness.dev/search?q=text"))
      . assert(_ == t"https://soundness.dev/search?q=text")

      test(m"A URL with an explicit port round-trips"):
        JavaNetUrl.genericize(JavaNetUrl(t"http://localhost:8080/api"))
      . assert(_ == t"http://localhost:8080/api")

    suite(m"Interface given tests"):
      test(m"A java.time.Instant interface is available"):
        import instantInterfaces.javaTimeInstant
        summon[JavaTimeInstant.type]
      . assert(_ == JavaTimeInstant)

      test(m"A java.nio.file.Path interface is available"):
        import pathInterfaces.javaNioPath
        summon[JavaNioPath.type]
      . assert(_ == JavaNioPath)

      test(m"A java.net.URL interface is available"):
        import urlInterfaces.javaNetUrl
        summon[JavaNetUrl.type]
      . assert(_ == JavaNetUrl)

      test(m"A java.nio.file.Path represents the Paths domain"):
        demilitarize:
          erased val representative = summon[jnf.Path is Representative of Paths]
      . assert(_ == Nil)

      test(m"A java.io.File represents the Paths domain"):
        demilitarize:
          erased val representative = summon[ji.File is Representative of Paths]
      . assert(_ == Nil)
