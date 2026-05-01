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
package merino

import ambience.*
import anticipation.*, interfaces.paths.pathOnLinux
import contingency.*, strategies.throwUnsafely
import eucalyptus.*, logging.silent
import fulminate.*
import galilei.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8
import nomenclature.*
import octogenarian.*, gitCommands.environmentDefault
import prepositional.*
import probably.*
import proscenium.*
import rudiments.*, workingDirectories.system
import serpentine.*
import turbulence.*
import urticose.*, internetAccess.enabled
import zephyrine.*
import errorDiagnostics.stackTraces

import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.overwritePreexisting.disabled

object Tests extends Suite(m"Merino tests"):
  def run(): Unit =
    val work: Path on Linux = workingDirectory
    val jsonSuite: Path on Linux = work/"tests"/"JSONTestSuite"

    if !(jsonSuite/"test_parsing").exists() then
      Git.clone(url"https://github.com/nst/JSONTestSuite", jsonSuite).complete()

    val tests: Path on Linux = jsonSuite/"test_parsing"

    suite(m"Positive tests"):
      tests.children.filter(_.name.starts(t"y_")).each: file =>
        test(Message(file.name.skip(5, Rtl))):
          file.open(_.read[JsonAst])
        .check()

    val deeplyNested: Set[Text] =
      Set(t"n_structure_100000_opening_arrays.json", t"n_structure_open_array_object.json")

    suite(m"Negative tests"):
      tests.children
        . filter(_.name.starts(t"n_"))
        . filter { file => !deeplyNested.contains(file.name) }
        . each: file =>
            test(Message(file.name.skip(5, Rtl))):
              capture[ParseError](file.open(_.read[JsonAst]))
            .matches:
              case ParseError(_, _, _) => true

    suite(m"Number tests"):
      test(m"Parse 0e+1"):
        t"0e+1".read[JsonAst]
      . assert(_ == JsonAst(0L))

      test(m"Parse 0e1"):
        t"0e1".read[JsonAst]
      . assert(_ == JsonAst(0L))

      test(m"Parse ' 4'"):
        t" 4".read[JsonAst]
      . assert(_ == JsonAst(4L))

      test(m"Parse small negative number"):
        t"-0.000000000000000000000000000000000000000000000000000000000000000000000000000001".read[JsonAst]
      . assert(_ == JsonAst(-1.0e-78))

      test(m"Parse 20e1"):
        t"20e1".read[JsonAst]
      . assert(_ == JsonAst(200L))

      test(m"Parse 123e65"):
        t"123e65".read[JsonAst]
      . assert(_ == JsonAst(1.23e67))

      test(m"Parse -0"):
        t"-0".read[JsonAst]
      . assert(_ == JsonAst(-0.0))

      test(m"Parse -123"):
        t"-123".read[JsonAst]
      . assert(_ == JsonAst(-123L))

      test(m"Parse -1"):
        t"-1".read[JsonAst]
      . assert(_ == JsonAst(-1L))

      test(m"Parse 1E22"):
        t"1E22".read[JsonAst]
      . assert(_ == JsonAst(1.0E22))

      test(m"Parse 1E-2"):
        t"1E-2".read[JsonAst]
      . assert(_ == JsonAst(1.0E-2))

      test(m"Parse 1E+2"):
        t"1E+2".read[JsonAst]
      . assert(_ == JsonAst(1.0E2))

      test(m"Parse 123e45"):
        t"123e45".read[JsonAst]
      . assert(_ == JsonAst(1.23E47))

      test(m"Parse 123.456e78"):
        t"123.456e78".read[JsonAst]
      . assert(_ == JsonAst(1.23456E80))

      test(m"Parse 1e-2"):
        t"1e-2".read[JsonAst]
      . assert(_ == JsonAst(1.0E-2))

      test(m"Parse 1e+2"):
        t"1e+2".read[JsonAst]
      . assert(_ == JsonAst(1.0E2))

      test(m"Parse 123"):
        t"123".read[JsonAst]
      . assert(_ == JsonAst(123L))

      test(m"Parse 123.456789"):
        t"123.456789".read[JsonAst]
      . assert(_ == JsonAst(123.456789))

      test(m"Parse \"Hello World\""):
        t"\"Hello World\"".read[JsonAst]
      . assert(_ == JsonAst("Hello World"))

      test(m"Parse \"\""):
        t"\"\"".read[JsonAst]
      . assert(_ == JsonAst(""))



private given realm: Realm = Realm(t"tests")
