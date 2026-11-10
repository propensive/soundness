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
package zeppelin

import soundness.*

import codepages.utf8Codepage
import detectables.{zipDetectable, zipIndexDetectable}
import strategies.throwUnsafely

object DetectionTests extends Suite("zip-detection", m"Zeppelin detection tests"):
  def run(): Unit =
    val archive: Data =
      Zipfile(List(Zip.Entry(t"hello.txt".as[Path on Zip], t"Hello world".in[Data]))).read[Data]

    // An archive which follows something else, as a self-extracting archive follows a program.
    val prefixed: Data = Array.concat(t"#!/bin/sh\n".in[Data], archive)

    def stream(data: Data): Text = Cursor(data) match
      case Media["application/zip"](zip) =>
        val typed: Detection of "application/zip" under Plausible to Zipfile = zip
        t"plausible"

      case _ =>
        t"unknown"

    def memory(data: Data): Text = data match
      case Media["application/zip", Likely](zip) =>
        val typed: Detection of "application/zip" under Likely to Zipfile = zip
        t"likely"

      case _ =>
        t"unknown"

    test(m"a stream which opens with an entry is plausibly an archive"):
      stream(archive)
    . assert(_ == t"plausible")

    test(m"a stream which opens with something else is not recognised"):
      stream(prefixed)
    . assert(_ == t"unknown")

    test(m"an archive in memory is likely an archive"):
      memory(archive)
    . assert(_ == t"likely")

    test(m"an archive which follows something else is likely an archive"):
      memory(prefixed)
    . assert(_ == t"likely")

    test(m"data which only opens like an archive is not likely one"):
      memory(hex"504b030400000000")
    . assert(_ == t"unknown")

    test(m"empty data is not an archive"):
      memory(Data())
    . assert(_ == t"unknown")
