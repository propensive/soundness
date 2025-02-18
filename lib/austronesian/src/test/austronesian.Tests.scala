package austronesian

import java.util as ju
import soundness.*

case class Person(name: Text, age: Int)
case class Group(persons: List[Person], size: Int)

enum Color:
  case Red, Green, Blue

object Tests extends Suite(t"Austronesian tests"):

  extension (left: Stdlib) infix def like (right: Array[Any]): Boolean =
    ju.Arrays.deepEquals(left.asInstanceOf[Array[Any]], right)

  def run(): Unit =
    test(t"Serialize a case class")(Person("John", 30).stdlib)
    . assert(_ like Array("John", 30))

    test(t"Serialize a list of longs")(List(1L, 99L, 203L).stdlib)
    . assert(_ like Array(1L, 99L, 203L))

    test(t"Serialize a list of case classes")(List(Person("John", 12), Person("Jane", 93)).stdlib)
    . assert(_ like Array(Array("John", 12), Array("Jane", 93)))

    test(t"Serialize a nested case class structure"):
      Group(List(Person("John", 30), Person("Jane", 25)), 2).stdlib
    . assert(_ like Array(Array(Array("John", 30), Array("Jane", 25)), 2))

    val group = Group(List(Person("John", 30), Person("Jane", 25)), 2)
    test(t"Roundtrip a nested case class"):
      unsafely(group.stdlib.decode[Group])
    . assert(_ == group)

    test(t"Encode an enum"):
      val color: Color = Color.Green
      color.stdlib
    . assert(_ like Array("Green", Array[Any]()))

    test(t"Roundtrip an enum"):
      val color: Color = Color.Green
      unsafely(color.stdlib.decode[Color])
    . assert(_ == Color.Green)
