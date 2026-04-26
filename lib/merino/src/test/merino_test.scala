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
import fulminate.*
import galilei.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8
import nomenclature.*
import prepositional.*
import probably.*
import proscenium.*
import rudiments.*, workingDirectories.system
import serpentine.*
import turbulence.*
import zephyrine.*
import errorDiagnostics.stackTraces

import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.disabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.createNonexistent.disabled

//import unsafeExceptions.canThrowAny

object Tests extends Suite(m"Merino tests"):
  def run(): Unit =
    val work: Path on Linux = workingDirectory
    val tests: Path on Linux = work/"tests"/"test_parsing"
    val tests2: Path on Linux = work/"tests"/"test_transform"

    suite(m"Positive tests"):
      (tests.children.filter(_.name.starts(t"y_")) ++ tests2.children).each: file =>
        test(Message(file.name.skip(5, Rtl))):
          file.open(_.read[JsonAst])
        .check()

    suite(m"Negative tests"):
      tests.children.filter(_.name.starts(t"n_")).each: file =>
        test(Message(file.name.skip(5, Rtl))):
          capture[ParseError](file.open(_.read[JsonAst]))
        .matches:
          case ParseError(_, _, _) => true

    val testDir: Path on Linux = work/"data"

    suite(m"Parse large files"):
      val file: Data = test(m"Read file"):
        (testDir/"huge.json").open(_.read[Data])
      .check()

      val file2: Data = test(m"Read file 2"):
        (testDir/"huge2.json").open(_.read[Data])
      .check()

      // test(m"Parse huge file with Jawn"):
      //   import org.typelevel.jawn.*, ast.*
      //   JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(file.mutable(using Unsafe)).nn)
      // .benchmark()

      // test(m"Parse huge file with Merino"):
      //   JsonAst.parse(file)
      // .benchmark()

      // test(m"Parse big file with Jawn"):
      //   import org.typelevel.jawn.*, ast.*
      //   JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(file2.mutable(using Unsafe)).nn)
      // .benchmark()

      // test(m"Parse big file with Merino"):
      //   file2.read[JsonAst]
      // .benchmark()

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
