/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import probably.*
import rudiments.*
import gossamer.*
import larceny.*

case class Person(name: Text, age: Int)

object Tests extends Suite(t"Rudiments Tests"):
  def run(): Unit =
    
    def remoteCall()(using Internet): Unit = ()

    test(t"Check remote call is callable with `Internet`"):
      internet:
        remoteCall()
    .assert()
    
    test(t"Check remote call is not callable without `Internet`"):
      demilitarize:
        remoteCall()
      .map(_.errorId)
    .assert(_ == List(ErrorId.MissingImplicitArgumentID))

    test(t"Display a PID"):
      Pid(2999).toString
    .assert(_ == "PID:2999")

    suite(t"Longest train tests"):
      test(t"Find longest train of zeros in middle"):
        List(1, 0, 0, 2, 3, 4, 0, 0, 0, 5, 6, 0, 7).longestTrain(_ == 0)
      .assert(_ == (6, 3))
      
      test(t"Find longest train of zeros at start"):
        List(0, 0, 0, 2, 3, 4, 0, 0, 1, 5, 6, 0, 7).longestTrain(_ == 0)
      .assert(_ == (0, 3))
      
      test(t"Find longest train of zeros at end"):
        List(0, 0, 1, 2, 3, 4, 0, 0, 1, 5, 6, 0, 0, 0, 0).longestTrain(_ == 0)
      .assert(_ == (11, 4))
    
    suite(t"PID & exit status tests"):
      test(t"Zero exit-status is OK"):
        ExitStatus(0)
      .assert(_ == ExitStatus.Ok)
      
      test(t"Positive exit-status is a failure"):
        ExitStatus(1)
      .assert(_ == ExitStatus.Fail(1))

      test(t"Ok has exit status 0"):
        ExitStatus.Ok
      .assert(_() == 0)
      
      test(t"Failure has non-zero exit status"):
        ExitStatus.Fail(3)
      .assert(_() == 3)
    
    suite(t"Bytes tests"):
      test(t"Construct a `Bytes` literal"):
        Bytes(1, 2, 3)
      .assert(_.length == 3)
      
      test(t"Construct a `Bytes` value from a Long"):
        Bytes(Long.MaxValue)
      .assert(_.length == 8)
      
      test(t"Construct an empty `Bytes`"):
        Bytes()
      .assert(_.length == 0)

    suite(t"Byte Size tests"):
      test(t"Construct a simple ByteSize"):
        4.b: ByteSize
      .assert(_ == ByteSize(4))
      
      test(t"Construct a simple ByteSize in kB"):
        4.kb: ByteSize
      .assert(_ == ByteSize(4096))
      
      test(t"Construct a simple ByteSize in MB"):
        4.mb: ByteSize
      .assert(_ == ByteSize(4096*1024L))
      
      test(t"Construct a simple ByteSize in GB"):
        4.gb: ByteSize
      .assert(_ == ByteSize(4096*1024L*1024L))

      test(t"Construct a simple ByteSize in TB"):
        4.tb: ByteSize
      .assert(_ == ByteSize(4096*1024L*1024L*1024L))

      test(t"Compare bytes with >"):
        4.gb > 4.mb
      .assert(_ == true)
      
      test(t"Compare bytes with >="):
        4.gb >= 4.mb*1024
      .assert(_ == true)

      test(t"Sort some byte sizes"):
        List(1.b, 1.mb, 1.kb).sorted
      .assert(_ == List(1.b, 1.kb, 1.mb))
    
    suite(t"Y-combinator test"):
      test(t"Check factorial implementation"):
        def factorial(n: Int): Int = fix[Int] { i => if i <= 0 then 1 else i*recur(i - 1) } (n)
        factorial(4)
      .assert(_ == 24)
   
    suite(t"UUID tests"):
      test(t"Construct a new UUID"):
        Uuid()
      .matches:
        case Uuid(a, b) =>
      
      test(t"Get bytes from UUID"):
        Uuid().bytes
      .assert(_.length == 16)

