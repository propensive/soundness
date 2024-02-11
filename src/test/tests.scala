/*
    Merino
jawn, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package merino

import probably.*
import gossamer.*
import anticipation.*, fileApi.javaIo
import eucalyptus.*
import rudiments.*, workingDirectories.virtualMachine
import contingency.*, errorHandlers.throwUnsafely
import hieroglyph.*, charEncoders.utf8
import turbulence.*
import ambience.*, systemProperties.virtualMachine

import java.io as ji

//import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Merino tests"):
  def run(): Unit =
    val tests = ji.File(ji.File(workingDirectory, "tests"), "test_parsing")
    val tests2 = ji.File(ji.File(workingDirectory, "tests"), "test_transform")
    
    suite(t"Positive tests"):
      (tests.listFiles.nn.map(_.nn).to(List).filter(_.getName.nn.startsWith("y_")) ++ tests2.listFiles.nn.map(_.nn).to(List)).each: file =>
        test(Text(file.getName.nn.dropRight(5))):
          JsonAst.parse(ji.BufferedInputStream(ji.FileInputStream(file)))
        .check()
    
    suite(t"Negative tests"):
      tests.listFiles.nn.map(_.nn).filter(_.getName.nn.startsWith("n_")).each: file =>
        test(Text(file.getName.nn.dropRight(5))):
          capture(JsonAst.parse(ji.BufferedInputStream(ji.FileInputStream(file))))
        .matches:
          case JsonParseError(_, _, _) => true
          case _                       => false

    suite(t"Parse large files"):
      val file: Bytes = test(t"Read file"):
        ji.BufferedInputStream(ji.FileInputStream(ji.File(workingDirectory, "huge.json"))).readAs[Bytes]
      .check()
      
      val file2: Bytes = test(t"Read file 2"):
        ji.BufferedInputStream(ji.FileInputStream(ji.File(workingDirectory, "huge2.json"))).readAs[Bytes]
      .check()
      
      // test(t"Parse huge file with Jawn"):
      //   import org.typelevel.jawn.*, ast.*
      //   JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(file.mutable(using Unsafe)).nn)
      // .benchmark()
      
      // test(t"Parse huge file with Merino"):
      //   JsonAst.parse(file)
      // .benchmark()
      
      // test(t"Parse big file with Jawn"):
      //   import org.typelevel.jawn.*, ast.*
      //   JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(file2.mutable(using Unsafe)).nn)
      // .benchmark()
    
      test(t"Parse big file with Merino"):
        JsonAst.parse(file2)
      .benchmark()

    suite(t"Number tests"):
      test(t"Parse 0e+1"):
        JsonAst.parse(t"0e+1")
      .assert(_ == JsonAst(0L))
      
      test(t"Parse 0e1"):
        JsonAst.parse(t"0e1")
      .assert(_ == JsonAst(0L))
      
      test(t"Parse ' 4'"):
        JsonAst.parse(t" 4")
      .assert(_ == JsonAst(4L))
      
      test(t"Parse small negative number"):
        JsonAst.parse(t"-0.000000000000000000000000000000000000000000000000000000000000000000000000000001")
      .assert(_ == JsonAst(-1.0e-78))
      
      test(t"Parse 20e1"):
        JsonAst.parse(t"20e1")
      .assert(_ == JsonAst(200L))
      
      test(t"Parse 123e65"):
        JsonAst.parse(t"123e65")
      .assert(_ == JsonAst(1.23e67))
      
      test(t"Parse -0"):
        JsonAst.parse(t"-0")
      .assert(_ == JsonAst(-0.0))
      
      test(t"Parse -123"):
        JsonAst.parse(t"-123")
      .assert(_ == JsonAst(-123L))
      
      test(t"Parse -1"):
        JsonAst.parse(t"-1")
      .assert(_ == JsonAst(-1L))
      
      test(t"Parse 1E22"):
        JsonAst.parse(t"1E22")
      .assert(_ == JsonAst(1.0E22))
      
      test(t"Parse 1E-2"):
        JsonAst.parse(t"1E-2")
      .assert(_ == JsonAst(1.0E-2))
      
      test(t"Parse 1E+2"):
        JsonAst.parse(t"1E+2")
      .assert(_ == JsonAst(1.0E2))
      
      test(t"Parse 123e45"):
        JsonAst.parse(t"123e45")
      .assert(_ == JsonAst(1.23E47))
      
      test(t"Parse 123.456e78"):
        JsonAst.parse(t"123.456e78")
      .assert(_ == JsonAst(1.23456E80))
      
      test(t"Parse 1e-2"):
        JsonAst.parse(t"1e-2")
      .assert(_ == JsonAst(1.0E-2))
      
      test(t"Parse 1e+2"):
        JsonAst.parse(t"1e+2")
      .assert(_ == JsonAst(1.0E2))
      
      test(t"Parse 123"):
        JsonAst.parse(t"123")
      .assert(_ == JsonAst(123L))
      
      test(t"Parse 123.456789"):
        JsonAst.parse(t"123.456789")
      .assert(_ == JsonAst(123.456789))
      
      test(t"Parse \"Hello World\""):
        JsonAst.parse(t"\"Hello World\"")
      .assert(_ == JsonAst("Hello World"))
      
      test(t"Parse \"\""):
        JsonAst.parse(t"\"\"")
      .assert(_ == JsonAst(""))



given realm: Realm = Realm(t"tests")
