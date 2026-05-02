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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package jacinta

import soundness.*

import charEncoders.utf8
import strategies.throwUnsafely
import jsonPrinters.minimal

import jsonDiscriminables.discriminatedUnionByKind
import autopsies.contrastExpectations
import errorDiagnostics.stackTraces

case class Foo(x: Int, y: Text) derives CanEqual

case class InvalidState(name: String) extends Exception("Not a valid state: "+name)

case class Empty()
case class Inner(n: Int)
case class Outer(inner: Inner)
case class NamedOuter(name: Text, inner: Inner)
case class OptFoo(x: Option[Int])
case class OptionalFoo(x: Optional[Int])
case class Bar(a: Int, b: Text)
case class BarOpt(a: Int, b: Optional[Text])
case class Marked(@memo(t"the count") n: Int)

enum Choice:
  case First(value: Int)
  case Second(value: Text)

enum Shape:
  case Circle(radius: Double)
  case Square(side: Double)

object Tests extends Suite(m"Jacinta Tests"):
  def run(): Unit =
    suite(m"Parsing tests"):
      test(m"Parse a number"):
        t"42".read[Json].as[Int]
      . assert(_ == 42)

      test(m"Parse a string"):
        val s = t"\"string\"".read[Json]
        s.as[Text]
      . assert(_ == t"string")

      test(m"Parse true"):
        t"true".read[Json].as[Boolean]
      . assert(identity)

      test(m"Parse false"):
        t"false".read[Json].as[Boolean]
      . assert(!_)

      test(m"Parse float"):
        t"3.1415".read[Json].as[Float]
      . assert(_ == 3.1415f)

      test(m"Parse double"):
        t"3.1415926".read[Json].as[Double]
      . assert(_ == 3.1415926)

      test(m"Parse a long"):
        t"1234567890123".read[Json].as[Long]
      . assert(_ == 1234567890123L)

      test(m"Parse a negative integer"):
        t"-99".read[Json].as[Int]
      . assert(_ == -99)

      test(m"Parse zero"):
        t"0".read[Json].as[Int]
      . assert(_ == 0)

      test(m"Parse null as Unit"):
        t"null".read[Json].as[Unit]
      . assert(_ == ())

      test(m"Parse an empty array"):
        t"[]".read[Json].as[List[Int]]
      . assert(_ == Nil)

      test(m"Parse an array of numbers"):
        t"[1, 2, 3]".read[Json].as[List[Int]]
      . assert(_ == List(1, 2, 3))

      test(m"Parse an empty object"):
        t"{}".read[Json].as[Empty]
      . assert(_ == Empty())

      test(m"Parse a string with newline escape"):
        t""""line1\\nline2"""".read[Json].as[Text]
      . assert(_ == t"line1\nline2")

      test(m"Parse a string with tab escape"):
        t""""a\\tb"""".read[Json].as[Text]
      . assert(_ == t"a\tb")

      test(m"Parse a string with escaped backslash"):
        t""""a\\\\b"""".read[Json].as[Text]
      . assert(_ == t"a\\b")

      test(m"Parse a nested object"):
        t"""{"inner":{"n":42}}""".read[Json].as[Outer]
      . assert(_ == Outer(Inner(42)))

    suite(m"Serialization"):
      test(m"Serialize string"):
        t"foo".json.show
      . assert(_ == t""""foo"""")

      test(m"Serialize double"):
        3.14159.json.show
      . assert(_ == t"3.14159")

      test(m"Serialize true"):
        true.json.show
      . assert(_ == t"true")

      test(m"Serialize false"):
        false.json.show
      . assert(_ == t"false")

      test(m"Serialize Int"):
        42.json.show
      . assert(_ == t"42")

      test(m"Serialize Long"):
        9876543210L.json.show
      . assert(_ == t"9876543210")

      test(m"Serialize Unit as null"):
        ().json.show
      . assert(_ == t"null")

      test(m"Serialize a list of integers"):
        List(1, 2, 3).json.show
      . assert(_ == t"[1,2,3]")

      test(m"Serialize an empty list"):
        List[Int]().json.show
      . assert(_ == t"[]")

      test(m"Serialize a list of strings"):
        List(t"a", t"b").json.show
      . assert(_ == t"""["a","b"]""")

      test(m"Serialize a map"):
        Map(t"a" -> 1, t"b" -> 2).json.show
      . assert: result =>
          result == t"""{"a":1,"b":2}""" || result == t"""{"b":2,"a":1}"""

      test(m"Serialize a string containing a newline escapes it"):
        t"a\nb".json.show
      . assert(_ == t""""a\\nb"""")

      test(m"Serialize a string containing a tab escapes it"):
        t"a\tb".json.show
      . assert(_ == t""""a\\tb"""")

      test(m"Serialize a string containing a backslash escapes it"):
        t"a\\b".json.show
      . assert(_ == t""""a\\\\b"""")

      test(m"Serialize case class with Option as None"):
        case class Foo(x: Int, y: Option[Int])
        Foo(1, None).json.show
      . assert(_ == t"""{"x":1}""")

      test(m"Serialize case class with Option as Some"):
        case class Foo(x: Int, y: Option[Int])
        Foo(1, Some(2)).json.show
      . assert(_ == t"""{"x":1,"y":2}""")

      test(m"Serialize case class with Optional as Unset"):
        case class Foo(x: Int, y: Optional[Int])
        Foo(1, Unset).json.show
      . assert(_ == t"""{"x":1}""")

      test(m"Serialize case class with present Optional"):
        case class Foo(x: Int, y: Optional[Int])
        Foo(1, 2).json.show
      . assert(_ == t"""{"x":1,"y":2}""")

      test(m"Serialize a nested case class"):
        NamedOuter(t"hello", Inner(7)).json.show
      . assert(_ == t"""{"name":"hello","inner":{"n":7}}""")

    suite(m"Misc tests"):
      test(m"Serialize to Json"):
        Foo(1, t"two").json
      . assert(_ == Json.make(x = 1.json, y = t"two".json))

      test(m"Parse from JSON"):
        t"""{"x": 1}""".read[Json]
      . assert(_ == Json.make(x = 1.json))

      test(m"Read case class"):
        t"""{"x": 1, "y": "two"}""".read[Json].as[Foo]
      . assert(_ == Foo(1, t"two"))

      test(m"Extract an absent Option"):
        case class OptFoo(x: Option[Int])
        t"""{"y": 1}""".read[Json].as[OptFoo].x
      . assert(_ == None)

      test(m"Extract an option"):
        case class OptFoo(x: Option[Int])
        t"""{"x": 1}""".read[Json].as[OptFoo].x
      . assert(_ == Some(1))

      test(m"Extract a present Optional"):
        case class OptionalFoo(x: Optional[Int])
        t"""{"x": 1}""".read[Json].as[OptionalFoo].x
      . assert(_ == 1)

      test(m"Extract an absent Optional"):
        case class OptionalFoo(x: Optional[Int])
        t"""{"y": 1}""".read[Json].as[OptionalFoo].x
      . assert(_ == Unset)

      test(m"Extract a None"):
        case class OptFoo(x: Option[Int])
        t"""{"y": 1}""".read[Json].as[OptFoo].x
      . assert(_ == None)

      test(m"Access an absent Optional dynamically"):
        import dynamicJsonAccess.enabled
        t"""{"y": 1}""".read[Json].missing.as[Optional[Int]]
      . assert(_ == Unset)

    suite(m"Generic derivation tests"):
      case class Person(name: Text, age: Int)
      case class Band(guitarists: List[Person], drummer: Person, bassist: Option[Person])

      val paul =
        test(m"Serialize a simple case class"):
          Person(t"Paul", 81).json.show
        .check(_ == t"""{"name":"Paul","age":81}""")

      val john = t"""{"name": "John", "age": 40}"""
      val george = t"""{"name": "George", "age": 58}"""
      val ringo = t"""{"name": "Ringo", "age": 82}"""

      val beatles = t"""{"guitarists": [$john, $george], "drummer": $ringo, "bassist": $paul}"""

      val paulObj = test(m"Extract a Person"):
        paul.read[Json].as[Person]
      .check(_ == Person(t"Paul", 81))

      val ringoObj = test(m"Extract a different person"):
        ringo.read[Json].as[Person]
      .check(_ == Person(t"Ringo", 82))

      test(m"Extract a band"):
        beatles.read[Json].as[Band]
      . assert(_ == Band(List(Person(t"John", 40), Person(t"George", 58)), ringoObj, Some(paulObj)))

      test(m"Extract a band directly"):
        beatles.read[Band over Json]
      . assert(_ == Band(List(Person(t"John", 40), Person(t"George", 58)), ringoObj, Some(paulObj)))

      enum Player:
        case Guitarist(person: Person)
        case Drummer(person: Person)
        case Bassist(person: Person)

      val paulCoproduct = test(m"Serialize a coproduct"):
        val paul: Player = Player.Bassist(paulObj)
        paul.json.show
      .check(_ == t"""{"person":{"name":"Paul","age":81},"kind":"Bassist"}""")

      test(m"Decode a coproduct"):
        paulCoproduct.read[Json].as[Player]
      . assert(_ == Player.Bassist(paulObj))

      test(m"Decode a coproduct as a precise subtype"):
        paulCoproduct.read[Json].as[Player.Bassist]
      . assert(_ == Player.Bassist(paulObj))

      case class NewBand(members: Set[Player])

      import Player.*
      val newBand = NewBand(Set(Bassist(paulObj), Drummer(ringoObj), Guitarist(Person(t"John", 40)),
          Guitarist(Person(t"George", 58))))

      val newBandText = test(m"Serialize NewBand"):
        newBand.json.show
      .check(_ == t"""{"members":[{"person":{"name":"Paul","age":81},"kind":"Bassist"},{"person":{"name":"Ringo","age":82},"kind":"Drummer"},{"person":{"name":"John","age":40},"kind":"Guitarist"},{"person":{"name":"George","age":58},"kind":"Guitarist"}]}""")

      test(m"Decode a NewBand"):
        newBandText.read[Json].as[NewBand]
      . assert(_ == newBand)

      test(m"Update a JSON object dynamically"):
        import dynamicJsonAccess.enabled
        val john = t"""{"name": "John", "age": 40}""".decode[Json]
        val john2 = john.age = 41
        john2.as[Person]
      . assert(_ == Person("John", 41))

      test(m"Update a JSON array dynamically"):
        import dynamicJsonAccess.enabled
        val array = t"""[1, 2, 3]""".decode[Json]
        val array2 = array(1) = 5
        array2.as[List[Int]]
      . assert(_ == List(1, 5, 3))

      case class Role(name: String)
      case class Entity(name: String, age: Int, roles: List[Role])
      case class Org(name: String, leader: Entity)

      val org = Org("The Beatles", Entity("John", 40, List(Role("Leader")))).json

      test(m"Lens update on JSON"):
        import dynamicJsonAccess.enabled
        val org2 = org.lens(_.leader.age = 41.json)
        org2.as[Org]
      . assert(_ == Org("The Beatles", Entity("John", 41, List(Role("Leader")))))

      test(m"Lens update with optic on JSON"):
        import dynamicJsonAccess.enabled
        val org2 = org.lens(_.leader.roles(Prim) = Role("-").json)
        org2.as[Org]
      . assert(_ == Org("The Beatles", Entity("John", 40, List(Role("-")))))

      test(m"Deeper lens update with optic on JSON"):
        import dynamicJsonAccess.enabled
        val org2 = org.lens(_.leader.roles(Prim).name = "-".json)
        org2.as[Org]
      . assert(_ == Org("The Beatles", Entity("John", 40, List(Role("-")))))

    suite(m"Json construction"):
      test(m"Json.make with one field"):
        Json.make(name = t"Anna".json).show
      . assert(_ == t"""{"name":"Anna"}""")

      test(m"Json.make with multiple fields"):
        Json.make(a = 1.json, b = t"two".json, c = true.json).show
      . assert(_ == t"""{"a":1,"b":"two","c":true}""")

      test(m"Nested Json.make"):
        Json.make(outer = Json.make(inner = 1.json)).show
      . assert(_ == t"""{"outer":{"inner":1}}""")

      test(m"Construct via Json.ast with a long"):
        Json.ast(JsonAst(7L)).show
      . assert(_ == t"7")

      test(m"Construct via Json.ast with a string"):
        Json.ast(JsonAst("hello")).show
      . assert(_ == t""""hello"""")

      test(m"Construct via Json.ast with a boolean"):
        Json.ast(JsonAst(true)).show
      . assert(_ == t"true")

      test(m"Construct via Json.ast with null"):
        Json.ast(JsonAst(null)).show
      . assert(_ == t"null")

    suite(m"Json field/index access"):
      val obj = t"""{"name": "Alice", "age": 30}""".read[Json]
      val arr = t"[10, 20, 30]".read[Json]

      test(m"Apply a field name as Text"):
        obj(t"name").as[Text]
      . assert(_ == t"Alice")

      test(m"Apply a numeric field"):
        obj(t"age").as[Int]
      . assert(_ == 30)

      test(m"Access an array element by index"):
        arr(1).as[Int]
      . assert(_ == 20)

      test(m"Access first array element"):
        arr(0).as[Int]
      . assert(_ == 10)

      test(m"unsafeApply on existing field"):
        obj.unsafeApply(t"name").as[Text]
      . assert(_ == t"Alice")

      test(m"unsafeApply on absent field returns absent Json"):
        obj.unsafeApply(t"missing").as[Optional[Int]]
      . assert(_ == Unset)

      test(m"Access nested fields via apply"):
        val nested = t"""{"a": {"b": {"c": 42}}}""".read[Json]
        nested(t"a")(t"b")(t"c").as[Int]
      . assert(_ == 42)

      test(m"Access object then array element"):
        val mixed = t"""{"items": [1, 2, 3]}""".read[Json]
        mixed(t"items")(2).as[Int]
      . assert(_ == 3)

      test(m"Dynamic field access"):
        import dynamicJsonAccess.enabled
        val person = t"""{"name": "Bob"}""".read[Json]
        person.name.as[Text]
      . assert(_ == t"Bob")

      test(m"Dynamic indexed access of array field"):
        import dynamicJsonAccess.enabled
        val data = t"""{"nums": [4, 5, 6]}""".read[Json]
        data.nums(1).as[Int]
      . assert(_ == 5)

    suite(m"Json updates"):
      test(m"Add a field to an object via updateDynamic"):
        import dynamicJsonAccess.enabled
        val base = t"""{"x": 1}""".read[Json]
        val updated = base.y = 2
        updated.show
      . assert(_ == t"""{"x":1,"y":2}""")

      test(m"Replace a field via updateDynamic"):
        import dynamicJsonAccess.enabled
        val base = t"""{"x": 1, "y": 2}""".read[Json]
        val updated = base.x = 9
        updated.show
      . assert(_ == t"""{"x":9,"y":2}""")

      test(m"Update an array element"):
        import dynamicJsonAccess.enabled
        val arr = t"""[1, 2, 3]""".read[Json]
        val updated = arr(1) = 9
        updated.show
      . assert(_ == t"[1,9,3]")

      test(m"Set a field to a string"):
        import dynamicJsonAccess.enabled
        val base = t"""{"x": 1}""".read[Json]
        val updated = base.greeting = t"hi"
        updated.show
      . assert(_ == t"""{"x":1,"greeting":"hi"}""")

      test(m"Delete a field by assigning Unset"):
        import dynamicJsonAccess.enabled
        val base = t"""{"x": 1, "y": 2}""".read[Json]
        val updated = base.x = Unset
        updated.show
      . assert(_ == t"""{"y":2}""")

      test(m"Delete a field whose value is a nested object"):
        import dynamicJsonAccess.enabled
        val base = t"""{"x": {"k":1}, "y": 2}""".read[Json]
        val updated = base.x = Unset
        updated.show
      . assert(_ == t"""{"y":2}""")

      test(m"Deleting a missing field is a no-op"):
        import dynamicJsonAccess.enabled
        val base = t"""{"x": 1}""".read[Json]
        val updated = base.missing = Unset
        updated.show
      . assert(_ == t"""{"x":1}""")

    suite(m"Json equality and hashing"):
      test(m"Two equal JSON objects compare equal"):
        t"""{"x": 1}""".read[Json] == t"""{"x": 1}""".read[Json]
      . assert(identity)

      test(m"Two unequal JSON objects compare unequal"):
        t"""{"x": 1}""".read[Json] == t"""{"x": 2}""".read[Json]
      . assert(!_)

      test(m"Object equality is independent of field order"):
        t"""{"x": 1, "y": 2}""".read[Json] == t"""{"y": 2, "x": 1}""".read[Json]
      . assert(identity)

      test(m"Cross-type number equality (Long vs Double)"):
        t"5".read[Json] == t"5.0".read[Json]
      . assert(identity)

      test(m"Equal primitive JSON values have the same hashCode"):
        val a = t"42".read[Json].hashCode
        val b = t"42".read[Json].hashCode
        a == b
      . assert(identity)

      test(m"String JSON equality"):
        t""""abc"""".read[Json] == t""""abc"""".read[Json]
      . assert(identity)

      test(m"Different strings are unequal"):
        t""""abc"""".read[Json] == t""""xyz"""".read[Json]
      . assert(!_)

      test(m"Boolean equality"):
        t"true".read[Json] == t"true".read[Json]
      . assert(identity)

      test(m"Boolean inequality"):
        t"true".read[Json] == t"false".read[Json]
      . assert(!_)

      test(m"Equal arrays compare equal"):
        t"[1, 2, 3]".read[Json] == t"[1, 2, 3]".read[Json]
      . assert(identity)

      test(m"Different-length arrays are unequal"):
        t"[1, 2]".read[Json] == t"[1, 2, 3]".read[Json]
      . assert(!_)

      test(m"Json equals returns false against a non-Json value"):
        val json = t"1".read[Json]
        json.equals("string")
      . assert(!_)

    suite(m"JsonAst type predicates"):
      test(m"isLong on a long literal"):
        t"42".read[Json].root.isLong
      . assert(identity)

      test(m"isDouble on a decimal literal"):
        t"3.14".read[Json].root.isDouble
      . assert(identity)

      test(m"isNumber on a long literal"):
        t"42".read[Json].root.isNumber
      . assert(identity)

      test(m"isNumber on a double literal"):
        t"3.14".read[Json].root.isNumber
      . assert(identity)

      test(m"isString on a string"):
        t""""x"""".read[Json].root.isString
      . assert(identity)

      test(m"isBoolean on true"):
        t"true".read[Json].root.isBoolean
      . assert(identity)

      test(m"isBoolean on false"):
        t"false".read[Json].root.isBoolean
      . assert(identity)

      test(m"isNull on null"):
        t"null".read[Json].root.isNull
      . assert(identity)

      test(m"isArray on an array"):
        t"[]".read[Json].root.isArray
      . assert(identity)

      test(m"isObject on an object"):
        t"{}".read[Json].root.isObject
      . assert(identity)

      test(m"isString is false for a number"):
        t"1".read[Json].root.isString
      . assert(!_)

      test(m"isLong is false for a string"):
        t""""x"""".read[Json].root.isLong
      . assert(!_)

      test(m"isAbsent on an absent value"):
        Json.ast(JsonAst(Unset)).root.isAbsent
      . assert(identity)

    suite(m"JsonAst conversions"):
      test(m"Decode a Long from a number"):
        t"123".read[Json].as[Long]
      . assert(_ == 123L)

      test(m"Decode an Int from a fractional number truncates"):
        t"3.7".read[Json].as[Int]
      . assert(_ == 3)

      test(m"Decode a Double from a number"):
        t"3.14".read[Json].as[Double]
      . assert(_ == 3.14)

      test(m"Decode a Double from an integer"):
        t"5".read[Json].as[Double]
      . assert(_ == 5.0)

      test(m"Decode a String"):
        t""""hello"""".read[Json].as[String]
      . assert(_ == "hello")

      test(m"Decode true as Boolean"):
        t"true".read[Json].as[Boolean]
      . assert(identity)

      test(m"Decode false as Boolean"):
        t"false".read[Json].as[Boolean]
      . assert(!_)

      test(m"Decode an array as a list"):
        t"[1, 2, 3]".read[Json].as[List[Int]].length
      . assert(_ == 3)

      test(m"Decode an object as a map"):
        t"""{"a": 1, "b": 2}""".read[Json].as[Map[Text, Int]].size
      . assert(_ == 2)

      test(m"Decode a map preserves keys"):
        t"""{"a": 1, "b": 2}""".read[Json].as[Map[Text, Int]].keySet
      . assert(_ == Set(t"a", t"b"))

      test(m"Decode a map preserves values"):
        t"""{"a": 1, "b": 2}""".read[Json].as[Map[Text, Int]].values.toSet
      . assert(_ == Set(1, 2))

      test(m"primitive of a string is String"):
        t""""x"""".read[Json].root.primitive
      . assert(_ == JsonPrimitive.String)

      test(m"primitive of a number is Number"):
        t"7".read[Json].root.primitive
      . assert(_ == JsonPrimitive.Number)

      test(m"primitive of a boolean is Boolean"):
        t"false".read[Json].root.primitive
      . assert(_ == JsonPrimitive.Boolean)

      test(m"primitive of an array is Array"):
        t"[]".read[Json].root.primitive
      . assert(_ == JsonPrimitive.Array)

      test(m"primitive of an object is Object"):
        t"{}".read[Json].root.primitive
      . assert(_ == JsonPrimitive.Object)

      test(m"primitive of null is Null"):
        t"null".read[Json].root.primitive
      . assert(_ == JsonPrimitive.Null)

    suite(m"Json error handling"):
      test(m"Decode wrong type raises JsonError NotType"):
        capture[JsonError](t""""abc"""".read[Json].as[Int])
      . assert(_.reason match
          case JsonError.Reason.NotType(_, _) => true
          case _                              => false)

      test(m"Decode missing required field raises JsonError"):
        capture[JsonError](t"""{}""".read[Json].as[Foo])
      . assert(_.reason match
          case JsonError.Reason.Absent => true
          case _                       => false)

      test(m"Asking for a string when JSON is a number raises NotType"):
        capture[JsonError](t"42".read[Json].as[Text])
      . assert(_.reason match
          case JsonError.Reason.NotType(_, JsonPrimitive.String) => true
          case _                                                  => false)

      test(m"Asking for a boolean when JSON is null raises Absent or NotType"):
        capture[JsonError](t"null".read[Json].as[Boolean])
      . assert(_.reason match
          case JsonError.Reason.NotType(JsonPrimitive.Null, _) => true
          case _                                                => false)

      test(m"NotType reason is communicable"):
        JsonError(JsonError.Reason.NotType(JsonPrimitive.String, JsonPrimitive.Number)).message
      . assert(_.text.s.contains("could not access"))

    suite(m"Json printing"):
      test(m"Minimal printer omits whitespace"):
        import jsonPrinters.minimal
        Json.make(a = 1.json, b = 2.json).show
      . assert(_ == t"""{"a":1,"b":2}""")

      test(m"Indented printer adds whitespace"):
        import jsonPrinters.indented
        val printed = Json.make(a = 1.json, b = 2.json).show
        printed.contains(t"\n")
      . assert(identity)

      test(m"Indented printer pretty-prints arrays"):
        import jsonPrinters.indented
        val printed = List(1, 2, 3).json.show
        printed.contains(t"\n")
      . assert(identity)

    suite(m"Discriminator strategies"):
      test(m"Discriminate by 'kind' (default in this file)"):
        val s: Shape = Shape.Circle(1.0)
        s.json.show
      . assert(_ == t"""{"radius":1.0,"kind":"Circle"}""")

      test(m"Decode disjunction by 'kind'"):
        val s: Shape = Shape.Square(2.0)
        s.json.as[Shape]
      . assert(_ == Shape.Square(2.0))

      locally:
        import jsonDiscriminables.discriminatedUnionByType
        enum Animal:
          case Dog(name: Text)
          case Cat(name: Text)

        test(m"Discriminate by 'type'"):
          val a: Animal = Animal.Dog(t"Rex")
          a.json.show
        . assert(_ == t"""{"name":"Rex","type":"Dog"}""")

        test(m"Decode disjunction by 'type'"):
          val a: Animal = Animal.Cat(t"Whiskers")
          a.json.as[Animal]
        . assert(_ == Animal.Cat(t"Whiskers"))

    suite(m"JsonPointer tests"):
      test(m"Empty pointer encodes with #"):
        JsonPointer().encode.contains(t"#")
      . assert(identity)

      test(m"Pointer with one segment includes the segment"):
        JsonPointer()(t"foo").encode.contains(t"foo")
      . assert(identity)

      test(m"Pointer with multiple segments includes all"):
        val p = JsonPointer()(t"a")(t"b")(t"c").encode
        p.contains(t"a") && p.contains(t"b") && p.contains(t"c")
      . assert(identity)

      test(m"Pointer escapes ~ as ~0 in segment"):
        val p = JsonPointer()(t"a~b").encode
        p.contains(t"~0")
      . assert(identity)

      test(m"Pointer escapes / as ~1 in segment"):
        val p = JsonPointer()(t"a/b").encode
        p.contains(t"~1")
      . assert(identity)

      test(m"Pointer with ordinal segment encodes the index"):
        val p = JsonPointer()(Prim).encode
        p.contains(t"0")
      . assert(identity)

      test(m"JsonPointerError reason describes itself"):
        val err = JsonPointerError(JsonPointerError.Reason.UnknownDocument)
        err.message.text.s.contains("registry")
      . assert(identity)

      test(m"Standalone registry returns Unset for unknown URLs"):
        import jsonPointerRegistries.standalone
        val registry = summon[JsonPointer.Registry]
        registry(url"http://example.com/")
      . assert(_ == Unset)

      test(m"Standalone registry returns updated values"):
        import jsonPointerRegistries.standalone
        val registry = summon[JsonPointer.Registry]
        val doc = t"""{"a": 1}""".read[Json]
        registry(url"http://example.com/doc") = doc
        registry(url"http://example.com/doc") == doc
      . assert(identity)

    suite(m"Time encodables/decodables"):
      import jsonEncodables.encodeInstantsAsUnixEpochMilliseconds
      import jsonDecodables.decodeInstantsAsUnixEpochMilliseconds
      import jsonDecodables.decodeDurationsAsMilliseconds
      import aviation.*
      import abstractables.instantIsAbstractable

      test(m"Encode an Instant as a Long"):
        Instant(1700000000000L).json.show
      . assert(_ == t"1700000000000")

      test(m"Decode an Instant from a Long"):
        t"1700000000000".read[Json].as[Instant].long
      . assert(_ == 1700000000000L)

      test(m"Round-trip an Instant"):
        Instant(1234567890L).json.as[Instant].long
      . assert(_ == 1234567890L)

      test(m"Decode a Duration from a Long"):
        t"5000".read[Json].as[Duration].value
      . assert(_ == 5.0)

    suite(m"JsonSchema tests"):
      test(m"Schematic for Int yields an Integer schema"):
        infer[Int is Schematic in JsonSchema].schema()
      . assert(_ == JsonSchema.Integer())

      test(m"Schematic for Long yields an Integer schema"):
        infer[Long is Schematic in JsonSchema].schema()
      . assert(_ == JsonSchema.Integer())

      test(m"Schematic for Text yields a String schema"):
        infer[Text is Schematic in JsonSchema].schema()
      . assert(_ == JsonSchema.String())

      test(m"Schematic for Double yields a Number schema"):
        infer[Double is Schematic in JsonSchema].schema()
      . assert(_ == JsonSchema.Number())

      test(m"Schematic for Boolean yields a Boolean schema"):
        infer[Boolean is Schematic in JsonSchema].schema()
      . assert(_ == JsonSchema.Boolean())

      test(m"Schematic for Optional[Int] is an optional Integer"):
        infer[Optional[Int] is Schematic in JsonSchema].schema()
      . assert:
          case s: JsonSchema.Integer => s.optional
          case _                     => false

      test(m"Schematic for List[Int] is an Array of Integers"):
        infer[List[Int] is Schematic in JsonSchema].schema()
      . assert:
          case JsonSchema.Array(_, JsonSchema.Integer(_, _, _, _, _, _), _, _, _, _, _) => true
          case _                                                                         => false

      test(m"Schematic for Set[Text] is an Array of Strings"):
        infer[Set[Text] is Schematic in JsonSchema].schema()
      . assert:
          case JsonSchema.Array(_, JsonSchema.String(_, _, _, _, _, _), _, _, _, _, _) => true
          case _                                                                         => false

      test(m"Schematic for Map[Text, Int] is an object with additionalProperties"):
        infer[Map[Text, Int] is Schematic in JsonSchema].schema()
      . assert:
          case s: JsonSchema.Object => s.additionalProperties
          case _                    => false

      test(m"Derived schema for a case class is an Object"):
        JsonSchema.derived[Bar].schema()
      . assert:
          case _: JsonSchema.Object => true
          case _                    => false

      test(m"Derived schema for a case class lists fields as properties"):
        JsonSchema.derived[Bar].schema() match
          case obj: JsonSchema.Object => obj.properties.keySet
          case _                      => Set.empty[Text]
      . assert(_ == Set(t"a", t"b"))

      test(m"Derived schema marks all fields required when none optional"):
        JsonSchema.derived[Bar].schema() match
          case obj: JsonSchema.Object => obj.required.let(_.toSet).or(Set())
          case _                      => Set()
      . assert(_ == Set(t"a", t"b"))

      test(m"Derived schema omits optional fields from required"):
        JsonSchema.derived[BarOpt].schema() match
          case obj: JsonSchema.Object => obj.required.let(_.toSet).or(Set())
          case _                      => Set()
      . assert(_ == Set(t"a"))

      test(m"Derived schema for sum type uses oneOf"):
        JsonSchema.derived[Choice].schema() match
          case obj: JsonSchema.Object => obj.oneOf.let(_.length).or(0)
          case _                      => 0
      . assert(_ == 2)

      test(m"description_= sets the description on a String schema"):
        val schema = JsonSchema.String().`description_=`(t"a name")
        schema.description
      . assert(_ == t"a name")

      test(m"description_= sets the description on an Integer schema"):
        val schema = JsonSchema.Integer().`description_=`(t"a count")
        schema.description
      . assert(_ == t"a count")

      test(m"JsonSchema serializes a String schema with type field"):
        val s: JsonSchema = JsonSchema.String()
        s.json.show.contains(t""""type":"string"""")
      . assert(identity)

      test(m"JsonSchema serializes an Integer schema with type field"):
        val s: JsonSchema = JsonSchema.Integer()
        s.json.show.contains(t""""type":"integer"""")
      . assert(identity)

      test(m"JsonSchema serializes a Boolean schema with type field"):
        val s: JsonSchema = JsonSchema.Boolean()
        s.json.show.contains(t""""type":"boolean"""")
      . assert(identity)

      test(m"JsonSchema serializes a Null schema with type field"):
        val s: JsonSchema = JsonSchema.Null()
        s.json.show.contains(t""""type":"null"""")
      . assert(identity)

      test(m"JsonSchema serializes an Object schema with type field"):
        val s: JsonSchema = JsonSchema.Object()
        s.json.show.contains(t""""type":"object"""")
      . assert(identity)

      test(m"JsonSchema Format encodes as kebab case"):
        JsonSchema.Format.DateTime.encode
      . assert(_ == t"date-time")

      test(m"JsonSchema Format decodes from kebab case"):
        t"date-time".decode[JsonSchema.Format]
      . assert(_ == JsonSchema.Format.DateTime)

      test(m"JsonSchema Format roundtrips Email"):
        JsonSchema.Format.Email.encode.decode[JsonSchema.Format]
      . assert(_ == JsonSchema.Format.Email)

    suite(m"Ndjson tests"):
      test(m"Ndjson stream of three values decodes to a List"):
        val stream = Stream(
          t"1".read[Json],
          t"2".read[Json],
          t"3".read[Json])
        Ndjson(stream).stream.map(_.as[Int]).to(List)
      . assert(_ == List(1, 2, 3))

      test(m"Ndjson can hold heterogeneous values"):
        val stream = Stream(
          t""""hi"""".read[Json],
          t"42".read[Json],
          t"true".read[Json])
        Ndjson(stream).stream.length
      . assert(_ == 3)
