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


object Tests2:
  def main(args: Array[String]): Unit =

    given Runner = Runner()

    def sum(a: Int, b: Int): Int = a + b

    suite(t"main suite"):
      test(t"One plus one equals two"):
        val x = 12
        //sum(-11, x).inspect
        2
      .assert(_ == 2)

      test(t"Two is equal to one plus one")(1 + 1).assert(2 == _)
      
      case class Foo(x: Int, y: String)
      val t = Foo(1, "two")
      
      test(t"Compare 1"):
        t
      .assert(Foo(1, "two") == _)
      
      test(t"Compare 2"):
        Foo(1, "two")
      .assert(t == _)