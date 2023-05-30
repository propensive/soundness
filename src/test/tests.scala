/*
    Merino
jawn, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
//import galilei.*, filesystems.unix
import anticipation.* //, fileApi.galileiApi
import eucalyptus.*
import rudiments.*
import hieroglyph.*, characterEncodings.utf8, badEncodingHandlers.strict
import parasitism.*, monitors.global
import turbulence.*, basicIo.jvm
import ambience.*, environments.system

import unsafeExceptions.canThrowAny

import LogFormat.standardAnsi

val StdoutSink = Stdout.sink
given Log({ case _ => StdoutSink })

object Tests extends Suite(t"Merino tests"):
  def run(): Unit =
    val tests = (env.pwd / p"tests" / p"test_parsing").directory(Expect)
    val tests2 = (env.pwd / p"tests" / p"test_transform").directory(Expect)
    
    suite(t"Positive tests"):
      (tests.files.filter(_.name.starts(t"y_")) ++ tests2.files).foreach: file =>
        test(file.name.drop(5, Rtl)):
          JsonAst.parse(file)
        .check(_ => true)
    
    suite(t"Negative tests"):
      tests.files.filter(_.name.starts(t"n_")).foreach: file =>
        test(file.name.drop(5, Rtl)):
          capture(JsonAst.parse(file))
        .matches:
          case JsonParseError(_, _, _) => true
          case _                       => false

    suite(t"Parse large files"):
      val file: Bytes = test(t"Read file"):
        (env.pwd / p"huge.json").file(Expect).readAs[Bytes, Bytes]
      .check(_ => true)
      
      val file2: Bytes = test(t"Read file 2"):
        (env.pwd / p"huge2.json").file(Expect).readAs[Bytes, Bytes]
      .check(_ => true)
      
      for i <- 1 to 1 do
        test(t"Parse huge file with Jawn $i"):
          import org.typelevel.jawn.*, ast.*
          JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(file.mutable(using Unsafe)).nn)
        .assert(_ => true)
        
      for i <- 1 to 1 do
        test(t"Parse huge file with Merino $i"):
          JsonAst.parse(file)
        .assert(_ => true)
        
      for i <- 1 to 1 do
        test(t"Parse big file with Jawn $i"):
          import org.typelevel.jawn.*, ast.*
          JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(file2.mutable(using Unsafe)).nn)
        .assert(_ => true)
      
      for i <- 1 to 1 do
        test(t"Parse big file with Merino $i"):
          JsonAst.parse(file2)
        .assert(_ => true)

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
