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
┃    Soundness, version 0.36.0.                                                                    ┃
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
  def run(): Unit =
    test(m"Serialize a case class")(Person("John", 30).pojo)
    . assert(_ === Pojo(Array("John", 30: java.lang.Integer)))

    test(m"Serialize a list of longs")(List(1L, 99L, 203L).pojo)
    . assert(_ === Pojo(Array[Object](1L: java.lang.Long, 99L: java.lang.Long, 203L: java.lang.Long)))

    test(m"Serialize a list of case classes")(List(Person("John", 12), Person("Jane", 93)).pojo)
    . assert(_ === Pojo(Array(Array("John", 12: java.lang.Integer), Array("Jane", 93: java.lang.Integer))))

    test(m"Serialize a nested case class structure"):
      Group(List(Person("John", 30), Person("Jane", 25)), 2).pojo
    . assert(_ === Pojo(Array(Array(Array("John", 30), Array("Jane", 25)), 2: java.lang.Integer)))

    val group = Group(List(Person("John", 30), Person("Jane", 25)), 2)

    test(m"Roundtrip a nested case class"):
      unsafely(group.pojo.decode[Group])
    . assert(_ == group)

    test(m"Encode an enum"):
      val color: Color = Color.Green
      color.pojo
    . assert(_ === Pojo(Array("Green", Array[Any]())))

    test(m"Roundtrip an enum"):
      val color: Color = Color.Green
      unsafely(color.pojo.decode[Color])
    . assert(_ == Color.Green)

    val data = List
                (Person(t"Jim", 19),
                 Group(persons = List(Person(t"Jane", 25), Person(t"John", 30)), size = 2),
                 Colors(Trie(Color.Red, Color.Green)))

    test(m"Roundtrip a complex datatype"):
      recover:
        case VariantError(_, _, _) => println("variant")
        case PojoError()           => println("pojo")

      . within:
          unsafely(data.pojo.decode[List[Something]])

    . assert()

    suite(m"Proxy testing"):
      test(m"Invoke the macro"):
        println(summon[Person is Restorable])
      . assert()
