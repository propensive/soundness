/*
    Turbulence, version 0.4.0. Copyright 2022-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package turbulence

import probably.*
import eucalyptus.*
import gossamer.*
import rudiments.*
import parasitism.*, threading.platform
import stdouts.stdout

import unsafeExceptions.canThrowAny

import logging.silent

object Tests extends Suite(t"Turbulence tests"):
  def run(using Runner): Unit =
    suite(t"Streaming Unicode tests"):
      given Encoding = enc"UTF-8"
      val ascii = IArray(t"", t"a", t"ab", t"abc", t"abcd")
      
      val strings = for
        asc0 <- Array(t"", t"a", t"ab", t"abc") // 4 combinations
        cp2  <- Array(t"", t"£")                // 8
        asc1 <- Array(t"", t"a", t"ab", t"abc") // 32
        cp3  <- Array(t"", t"€")                // 64
        asc2 <- Array(t"", t"a", t"ab", t"abc") // 256
        cp4  <- Array(t"")//, t"𐍈")                // 512
        asc3 <- Array(t"", t"a", t"ab", t"abc") // 2048
      yield asc0+cp2+asc1+cp3+asc2+cp4

      for
        string <- strings
        bs     <- 8 to 5 by -1
      do
        test(t"length tests"):
          val stream: DataStream = string.bytes.grouped(bs).to(LazyList).map(identity(_))
          val result = stream.read[LazyList[Text]]()
          result.join.bytes.length
        .assert(_ == string.bytes.length)

        test(t"roundtrip tests"):
          val stream: DataStream = string.bytes.grouped(bs).to(LazyList).map(identity(_))
          val result = stream.read[LazyList[Text]]()
          if result.join != string then t"${result.join(t"[", ",", "]")} bs=$bs exp=${string.bytes.toList.map(_.show).join(t"[", t",", t"]")} got=${result.map(_.s.toList.map(_.toInt.toString).mkString(",").show).join(t" :# ")}"
          else result.join
        .assert(_ == string)
