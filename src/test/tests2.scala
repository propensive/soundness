/*
    Probably, version 0.4.0. Copyright 2017-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package probably

import gossamer.*
import rudiments.*
import eucalyptus.*, logging.stdout
import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Probably 2 Tests"):
  def run(): Unit =
    def sum(a: Int, b: Int): Int = a + b

    suite(t"main suite"):
      test(t"One plus one equals two"):
        val x = 12
        sum(-11, x).inspect
        2
      .assert(_ == 3)

      test(t"Two is equal to one plus one")(1 + 1).assert(2 == _)
      
      case class Foo(x: Int, y: Text)
      //val t = Foo(1, "two")
      
      for i <- 1 to 100 do
        test(t"i is less than ten"):
          i
        .assert(_ < 10)

      for i <- 1 to 5 do
        test(t"Compare 1"):
          throw Exception("broken")
        .assert(Foo(1, t"two") == _)
      
        test(t"Compare 3"):
          Thread.sleep(90)
          Foo(1, t"two")
        .assert(Foo(1, t"two") == _)

      val seq = IArray("zero", "one", "two", "three", "four", "five")

      seq.curse:
        println(":"+precursor+"/"+cursor+"/"+postcursor)

      case class Bar(foo: Foo, foo2: Foo, foo3: Foo)

      val debugString = summon[Debug[Text]]
      val debugInt = summon[Debug[Int]]
      val compInt = summon[Comparable[Int]]
      val compText = summon[Comparable[Text]]
      val compFoo = summon[Comparable[Foo]]
      val compBar = summon[Comparable[Bar]]

      test(t"Compare two Bars"):
        Bar(Foo(0, t"un"), Foo(1, t"two"), Foo(3, t"three"))
      .assert(_ == Bar(Foo(1, t"one"), Foo(2, t"two"), Foo(3, t"three")))

    suite(t"subsuite"):
      suite(t"Deeper subsuite"):
        test(t"Subtest"):
          1 + 1
        .assert(_ == ({ throw Exception(); 2 }))
