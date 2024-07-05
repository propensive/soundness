/*
    Contingency, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package contingency

import kaleidoscope.*
import rudiments.*
import anticipation.*
import fulminate.*

//import errorHandlers.throwUnsafely

def unsafe(): Unit raises UnsetError = ()


object FooError:
  given FooError is Fatal as fooError = error =>
    println(error)
    ExitStatus.Fail(1)

case class FooError(msg: String) extends Error(m"foo $msg")

object BarError:
  given BarError is Fatal as barError = error =>
    println(error)
    ExitStatus.Ok

case class BarError(msg: String) extends Error(m"bar $msg")
case class UnsetError(n: Int) extends Error(m"bar: $n")
case class ValueError(n: Int) extends Error(m"there was a value error with $n")

def run(): Unit =
 println:
  quash:
    case UnsetError(n) => "foo"+n
    case ValueError(_) => "bar"
  .within:
    raise(UnsetError(99))(1)
    "yes"
