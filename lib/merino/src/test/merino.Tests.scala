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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import ambience.*, systemProperties.virtualMachine
import anticipation.*, filesystemApi.javaIoFile
import contingency.*, strategies.throwUnsafely
import eucalyptus.*
import fulminate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8
import probably.*
import rudiments.*, workingDirectories.virtualMachine
import turbulence.*
import sedentary.*
import errorDiagnostics.stackTraces

import java.io as ji

//import unsafeExceptions.canThrowAny

object Tests extends Suite(m"Merino tests"):
  def run(): Unit =
    val tests = ji.File(ji.File(workingDirectory, "tests"), "test_parsing")
    val tests2 = ji.File(ji.File(workingDirectory, "tests"), "test_transform")

    suite(m"Positive tests"):
      (tests.listFiles.nn.map(_.nn).to(List).filter(_.getName.nn.startsWith("y_")) ++ tests2.listFiles.nn.map(_.nn).to(List)).each: file =>
        test(Message(file.getName.nn.dropRight(5).tt)):
          JsonAst.parse(ji.BufferedInputStream(ji.FileInputStream(file)))
        .check()

    suite(m"Negative tests"):
      tests.listFiles.nn.map(_.nn).filter(_.getName.nn.startsWith("n_")).each: file =>
        test(Message(file.getName.nn.dropRight(5).tt)):
          capture(JsonAst.parse(ji.BufferedInputStream(ji.FileInputStream(file))))
        .matches:
          case JsonParseError(_, _, _) => true
          case _                       => false

    val testDir = ji.File(workingDirectory, "data")

    suite(m"Parse large files"):
      val file: Bytes = test(m"Read file"):
        ji.BufferedInputStream(ji.FileInputStream(ji.File(testDir, "huge.json"))).read[Bytes]
      .check()

      val file2: Bytes = test(m"Read file 2"):
        ji.BufferedInputStream(ji.FileInputStream(ji.File(testDir, "huge2.json"))).read[Bytes]
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

      test(m"Parse big file with Merino"):
        JsonAst.parse(file2)
      .benchmark()

    suite(m"Number tests"):
      test(m"Parse 0e+1"):
        JsonAst.parse(t"0e+1")
      .assert(_ == JsonAst(0L))

      test(m"Parse 0e1"):
        JsonAst.parse(t"0e1")
      .assert(_ == JsonAst(0L))

      test(m"Parse ' 4'"):
        JsonAst.parse(t" 4")
      .assert(_ == JsonAst(4L))

      test(m"Parse small negative number"):
        JsonAst.parse(t"-0.000000000000000000000000000000000000000000000000000000000000000000000000000001")
      .assert(_ == JsonAst(-1.0e-78))

      test(m"Parse 20e1"):
        JsonAst.parse(t"20e1")
      .assert(_ == JsonAst(200L))

      test(m"Parse 123e65"):
        JsonAst.parse(t"123e65")
      .assert(_ == JsonAst(1.23e67))

      test(m"Parse -0"):
        JsonAst.parse(t"-0")
      .assert(_ == JsonAst(-0.0))

      test(m"Parse -123"):
        JsonAst.parse(t"-123")
      .assert(_ == JsonAst(-123L))

      test(m"Parse -1"):
        JsonAst.parse(t"-1")
      .assert(_ == JsonAst(-1L))

      test(m"Parse 1E22"):
        JsonAst.parse(t"1E22")
      .assert(_ == JsonAst(1.0E22))

      test(m"Parse 1E-2"):
        JsonAst.parse(t"1E-2")
      .assert(_ == JsonAst(1.0E-2))

      test(m"Parse 1E+2"):
        JsonAst.parse(t"1E+2")
      .assert(_ == JsonAst(1.0E2))

      test(m"Parse 123e45"):
        JsonAst.parse(t"123e45")
      .assert(_ == JsonAst(1.23E47))

      test(m"Parse 123.456e78"):
        JsonAst.parse(t"123.456e78")
      .assert(_ == JsonAst(1.23456E80))

      test(m"Parse 1e-2"):
        JsonAst.parse(t"1e-2")
      .assert(_ == JsonAst(1.0E-2))

      test(m"Parse 1e+2"):
        JsonAst.parse(t"1e+2")
      .assert(_ == JsonAst(1.0E2))

      test(m"Parse 123"):
        JsonAst.parse(t"123")
      .assert(_ == JsonAst(123L))

      test(m"Parse 123.456789"):
        JsonAst.parse(t"123.456789")
      .assert(_ == JsonAst(123.456789))

      test(m"Parse \"Hello World\""):
        JsonAst.parse(t"\"Hello World\"")
      .assert(_ == JsonAst("Hello World"))

      test(m"Parse \"\""):
        JsonAst.parse(t"\"\"")
      .assert(_ == JsonAst(""))



given realm: Realm = Realm(t"tests")
