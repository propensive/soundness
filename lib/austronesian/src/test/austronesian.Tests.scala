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
┃    Soundness, version 0.32.0.                                                                    ┃
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
package austronesian

import java.util as ju
import soundness.*

sealed trait Something
case class Person(name: Text, age: Int) extends Something
case class Group(persons: List[Person], size: Int) extends Something
case class Colors(colors: Trie[Color]) extends Something

enum Color:
  case Red, Green, Blue

object Tests extends Suite(m"Austronesian tests"):

  extension (left: Stdlib) infix def like (right: Array[Any]): Boolean =
    ju.Arrays.deepEquals(left.asInstanceOf[Array[Any]], right)

  def run(): Unit =
    test(m"Serialize a case class")(Person("John", 30).stdlib)
    . assert(_ like Array("John", 30))

    test(m"Serialize a list of longs")(List(1L, 99L, 203L).stdlib)
    . assert(_ like Array(1L, 99L, 203L))

    test(m"Serialize a list of case classes")(List(Person("John", 12), Person("Jane", 93)).stdlib)
    . assert(_ like Array(Array("John", 12), Array("Jane", 93)))

    test(m"Serialize a nested case class structure"):
      Group(List(Person("John", 30), Person("Jane", 25)), 2).stdlib
    . assert(_ like Array(Array(Array("John", 30), Array("Jane", 25)), 2))

    val group = Group(List(Person("John", 30), Person("Jane", 25)), 2)

    test(m"Roundtrip a nested case class"):
      unsafely(group.stdlib.decode[Group])
    . assert(_ == group)

    test(m"Encode an enum"):
      val color: Color = Color.Green
      color.stdlib
    . assert(_ like Array("Green", Array[Any]()))

    test(m"Roundtrip an enum"):
      val color: Color = Color.Green
      unsafely(color.stdlib.decode[Color])
    . assert(_ == Color.Green)

    val data = List
                (Person(t"Jim", 19),
                 Group(persons = List(Person(t"Jane", 25), Person(t"John", 30)), size = 2),
                 Colors(Trie(Color.Blue, Color.Green, Color.Red)))

    test(m"Roundtrip a complex datatype"):
      unsafely(data.stdlib.decode[List[Something]])
    . assert(_ == data)




    // suite(t"Proxy testing"):
    //   test(t"Invoke a Proxy method"):
    //     import classloaders.system
    //     o"austronesian.Example".run(t"hello")
    //   .assert()

    //   test(t"Invoke the macro"):
    //     summon[Person is Restorable]
