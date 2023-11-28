/*
    Perforate, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package perforate

import fulminate.*

//import language.experimental.captureChecking

case class FooError() extends Error(msg"Foo")
case class BarError(num: Int, str: String) extends Error(msg"Bar: $num $str")
case class BazError() extends Error(msg"Baz")
case class QuuxError(barError: BarError) extends Error(msg"Quux")

def foo()(using fooError: Raises[FooError]): String =
  if math.random < 0.3 then abort(FooError()) else "foo"

def bar()(using barError: Raises[BarError]): String =
  if math.random < 0.3 then abort(BarError(12, "two")) else "bar"

def quux()(using quuxError: Raises[QuuxError]): String =
  if math.random < 0.3 then abort(QuuxError(BarError(4, ""))) else "quux"

@main
def run(): Unit =
  println("Hello")
  given Raises[BazError] = ???
  println("Hello 2")
  
  mitigate:
    case _: FooError               => BazError()
    case QuuxError(BarError(n, s)) => BazError()
    case _: BarError               => BazError()
  .within:
    println("Hello 3")
    println(foo())
    println("Hello 4")
  
  println("Hello 5")