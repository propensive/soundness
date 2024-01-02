/*
    Jacinta, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package jacinta

import gossamer.*
import probably.*
import rudiments.*
import turbulence.*
import hieroglyph.*, charEncoders.utf8

import unsafeExceptions.canThrowAny

object Bench extends Suite(t"Jacinta benchmarks"):
  def run(): Unit =
    test(t"Parse a simple object with one string value"):
      Json.parse(t"""{"foo": "bar"}""")
    .benchmark(duration = 3000L, warmup = 3000L)
    
    test(t"Parse a simple object with one numerical value"):
      Json.parse(t"""{"foo": 3.1415926 }""")
    .benchmark(duration = 3000L, warmup = 3000L)
    
    test(t"Parse true value"):
      Json.parse(t"""{"foo": true }""")
    .benchmark(duration = 3000L, warmup = 3000L)
    
    test(t"Parse false value"):
      Json.parse(t"""{"foo": false }""")
    .benchmark(duration = 3000L, warmup = 3000L)
    
    test(t"Parse array of strings"):
      Json.parse(t"""["foo", "bar", "baz", "quux", "abcd", "defg", "hijk"]""")
    .benchmark(duration = 3000L, warmup = 3000L)
    
    test(t"Parse array of numbers"):
      Json.parse(t"""[12345.6789, 98765.4321, 142536.475869]""")
    .benchmark(duration = 3000L, warmup = 3000L)
