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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.language.dynamics

import charEncoders.utf8Encoder
import strategies.throwUnsafely
import formatting.compactJsonFormatting

import discriminables.jsonByKindDiscriminable
import errorDiagnostics.stackTracesDiagnostics

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
case class WithDefault(name: Text, age: Int = 18) derives CanEqual
case class Marked(@memo(t"the count") n: Int)
case class Renamed(@name[Json](t"first_name") firstName: Text, @name(t"yob") year: Int)
derives CanEqual

enum Choice:
  case First(value: Int)
  case Second(value: Text)

enum Shape:
  case Circle(radius: Double)
  case Square(side: Double)

enum Status derives CanEqual:
  @name[Json](t"ok") case Active(since: Int)
  @name(t"gone")     case Removed(at: Int)
                     case Pending(at: Int)

case class Person(name: Text, age: Int)
case class Band(guitarists: List[Person], drummer: Person, bassist: Option[Person])

case class FooOption(x: Int, y: Option[Int])
case class FooOptional(x: Int, y: Optional[Int])

enum Player:
  case Guitarist(person: Person)
  case Drummer(person: Person)
  case Bassist(person: Person)

case class NewBand(members: Set[Player])

case class Role(name: String)
case class Entity(name: String, age: Int, roles: List[Role])
case class Org(name: String, leader: Entity)

enum Animal:
  case Dog(name: Text)
  case Cat(name: Text)

case class Worker(name: Text, age: Int) derives CanEqual
case class Firm(boss: Worker, deputy: Worker) derives CanEqual

case class Crew(lead: Worker, members: List[Worker]) derives CanEqual

// Recursion through a collection (#1429), direct recursion, recursion through a map, and a generic
// product used over a recursive type (which must stay structurally derived, not mis-read as a codec).
case class Tree(value: Text, children: List[Tree]) derives CanEqual
case class TreeOpt(value: Text, child: Optional[TreeOpt]) derives CanEqual
case class Forest(trees: Map[Text, Tree]) derives CanEqual
case class Boxed[value](value: value) derives CanEqual

// Read by a hand-written `Json.Parsable` in the direct-parsing tests.
case class DirectPoint(x: Int, y: Int) derives CanEqual


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
        t"foo".in[Json].show
      . assert(_ == t""""foo"""")

      test(m"Serialize double"):
        3.14159.in[Json].show
      . assert(_ == t"3.14159")

      test(m"Serialize true"):
        true.in[Json].show
      . assert(_ == t"true")

      test(m"Serialize false"):
        false.in[Json].show
      . assert(_ == t"false")

      test(m"Serialize Int"):
        42.in[Json].show
      . assert(_ == t"42")

      test(m"Serialize Long"):
        9876543210L.in[Json].show
      . assert(_ == t"9876543210")

      test(m"Serialize Unit as null"):
        ().in[Json].show
      . assert(_ == t"null")

      test(m"Serialize a list of integers"):
        List(1, 2, 3).in[Json].show
      . assert(_ == t"[1,2,3]")

      test(m"Serialize an empty list"):
        List[Int]().in[Json].show
      . assert(_ == t"[]")

      test(m"Serialize a list of strings"):
        List(t"a", t"b").in[Json].show
      . assert(_ == t"""["a","b"]""")

      test(m"Serialize a map"):
        Map(t"a" -> 1, t"b" -> 2).in[Json].show
      . assert: result =>
          result == t"""{"a":1,"b":2}""" || result == t"""{"b":2,"a":1}"""

      test(m"Serialize a string containing a newline escapes it"):
        t"a\nb".in[Json].show
      . assert(_ == t""""a\\nb"""")

      test(m"Serialize a string containing a tab escapes it"):
        t"a\tb".in[Json].show
      . assert(_ == t""""a\\tb"""")

      test(m"Serialize a string containing a backslash escapes it"):
        t"a\\b".in[Json].show
      . assert(_ == t""""a\\\\b"""")

      test(m"Serialize case class with Option as None"):
        FooOption(1, None).in[Json].show
      . assert(_ == t"""{"x":1}""")

      test(m"Serialize case class with Option as Some"):
        FooOption(1, Some(2)).in[Json].show
      . assert(_ == t"""{"x":1,"y":2}""")

      test(m"Serialize case class with Optional as Unset"):
        FooOptional(1, Unset).in[Json].show
      . assert(_ == t"""{"x":1}""")

      test(m"Serialize case class with present Optional"):
        FooOptional(1, 2).in[Json].show
      . assert(_ == t"""{"x":1,"y":2}""")

      test(m"Serialize a nested case class"):
        NamedOuter(t"hello", Inner(7)).in[Json].show
      . assert(_ == t"""{"name":"hello","inner":{"n":7}}""")

    suite(m"Misc tests"):
      test(m"Serialize to Json"):
        Foo(1, t"two").in[Json]
      . assert(_ == Json.make(x = 1.in[Json], y = t"two".in[Json]))

      test(m"Parse from JSON"):
        t"""{"x": 1}""".read[Json]
      . assert(_ == Json.make(x = 1.in[Json]))

      test(m"Read case class"):
        t"""{"x": 1, "y": "two"}""".read[Json].as[Foo]
      . assert(_ == Foo(1, t"two"))

      test(m"@name[Json] and bare @name rename keys when encoding"):
        Renamed(t"Ann", 1984).in[Json].show
      . assert(_ == t"""{"first_name":"Ann","yob":1984}""")

      test(m"@name renames round-trip"):
        Renamed(t"Ann", 1984).in[Json].as[Renamed]
      . assert(_ == Renamed(t"Ann", 1984))

      test(m"Extract an absent Option"):
        t"""{"y": 1}""".read[Json].as[OptFoo].x
      . assert(_ == None)

      test(m"Extract an option"):
        t"""{"x": 1}""".read[Json].as[OptFoo].x
      . assert(_ == Some(1))

      test(m"Extract a present Optional"):
        t"""{"x": 1}""".read[Json].as[OptionalFoo].x
      . assert(_ == 1)

      test(m"Extract an absent Optional"):
        t"""{"y": 1}""".read[Json].as[OptionalFoo].x
      . assert(_ == Unset)

      test(m"Extract an explicit null Optional as Unset"):
        t"""{"x": null}""".read[Json].as[OptionalFoo].x
      . assert(_ == Unset)

      test(m"Decode a top-level null to an Optional as Unset"):
        t"""null""".read[Json].as[Optional[Int]]
      . assert(_ == Unset)

      test(m"Extract a None"):
        t"""{"y": 1}""".read[Json].as[OptFoo].x
      . assert(_ == None)

      test(m"Access an absent Optional dynamically"):
        import dynamicJsonAccess.enabled
        t"""{"y": 1}""".read[Json].missing.as[Optional[Int]]
      . assert(_ == Unset)

      test(m"Apply a case-class default when the field is omitted"):
        t"""{"name": "Eve"}""".read[Json].as[WithDefault]
      . assert(_ == WithDefault(t"Eve", 18))

      test(m"An explicit value overrides a case-class default"):
        t"""{"name": "Eve", "age": 30}""".read[Json].as[WithDefault]
      . assert(_ == WithDefault(t"Eve", 30))

    suite(m"Generic derivation tests"):
      val paul =
        test(m"Serialize a simple case class"):
          Person(t"Paul", 81).in[Json].show
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
        beatles.read[Band in Json]
      . assert(_ == Band(List(Person(t"John", 40), Person(t"George", 58)), ringoObj, Some(paulObj)))

      val paulCoproduct = test(m"Serialize a coproduct"):
        val paul: Player = Player.Bassist(paulObj)
        paul.in[Json].show
      .check(_ == t"""{"person":{"name":"Paul","age":81},"kind":"Bassist"}""")

      test(m"Decode a coproduct"):
        paulCoproduct.read[Json].as[Player]
      . assert(_ == Player.Bassist(paulObj))

      test(m"Decode a coproduct as a precise subtype"):
        paulCoproduct.read[Json].as[Player.Bassist]
      . assert(_ == Player.Bassist(paulObj))

      import Player.*
      val newBand = NewBand(Set(Bassist(paulObj), Drummer(ringoObj), Guitarist(Person(t"John", 40)),
          Guitarist(Person(t"George", 58))))

      val newBandText = test(m"Serialize NewBand"):
        newBand.in[Json].show
      .check(_ == t"""{"members":[{"person":{"name":"Paul","age":81},"kind":"Bassist"},{"person":{"name":"Ringo","age":82},"kind":"Drummer"},{"person":{"name":"John","age":40},"kind":"Guitarist"},{"person":{"name":"George","age":58},"kind":"Guitarist"}]}""")

      test(m"Decode a NewBand"):
        newBandText.read[Json].as[NewBand]
      . assert(_ == newBand)

      test(m"Update a JSON object dynamically"):
        import dynamicJsonAccess.enabled
        val john = t"""{"name": "John", "age": 40}""".as[Json]
        val john2 = john.age = 41
        john2.as[Person]
      . assert(_ == Person("John", 41))

      test(m"Update a JSON array dynamically"):
        import dynamicJsonAccess.enabled
        val array = t"""[1, 2, 3]""".as[Json]
        val array2 = array(1) = 5
        array2.as[List[Int]]
      . assert(_ == List(1, 5, 3))

      val org = Org("The Beatles", Entity("John", 40, List(Role("Leader")))).in[Json]

      test(m"Lens update on JSON"):
        import dynamicJsonAccess.enabled
        val org2 = org.lens(_.leader.age = 41.in[Json])
        org2.as[Org]
      . assert(_ == Org("The Beatles", Entity("John", 41, List(Role("Leader")))))

      test(m"Lens update with optic on JSON"):
        import dynamicJsonAccess.enabled, jsonConversion.encodable
        val org2 = org.lens(_.leader.roles(Prim) = Role("-"))
        org2.as[Org]
      . assert(_ == Org("The Beatles", Entity("John", 40, List(Role("-")))))

      test(m"Deeper lens update with optic on JSON"):
        import dynamicJsonAccess.enabled, jsonConversion.encodable
        val org2 = org.lens(_.leader.roles(Prim).name = "-")
        org2.as[Org]
      . assert(_ == Org("The Beatles", Entity("John", 40, List(Role("-")))))

      val band = Org("Q", Entity("John", 40, List(Role("a"), Role("b"), Role("c")))).in[Json]

      test(m"Lens reads a field by name"):
        import dynamicJsonAccess.enabled
        summon["name" is Lens from Json onto Json](org).as[String]
      . assert(_ == "The Beatles")

      test(m"Lens.modify transforms a field through a function"):
        import dynamicJsonAccess.enabled
        val lens = summon["name" is Lens from Json onto Json]
        lens.modify(org)(json => (json.as[String]+"!").in[Json]).as[Org]
      . assert(_ == Org("The Beatles!", Entity("John", 40, List(Role("Leader")))))

      test(m"Each optic updates every array element"):
        import dynamicJsonAccess.enabled, jsonConversion.encodable
        band.lens(_.leader.roles(Each).name = "x").as[Org]
      . assert(_ == Org("Q", Entity("John", 40, List(Role("x"), Role("x"), Role("x")))))

      test(m"Filter optic updates only matching elements"):
        import dynamicJsonAccess.enabled, jsonConversion.encodable
        band.lens(_.leader.roles(Filter[Json](_.name.as[String] == "b")).name = "x").as[Org]
      . assert(_ == Org("Q", Entity("John", 40, List(Role("a"), Role("x"), Role("c")))))

      test(m"Setting an absent field inserts it"):
        import dynamicJsonAccess.enabled, jsonConversion.encodable
        org.lens(_.extra = 9).selectDynamic("extra").as[Int]
      . assert(_ == 9)

    suite(m"Recursive derivation tests"):
      val tree = Tree(t"root", List(Tree(t"a", Nil), Tree(t"b", List(Tree(t"c", Nil)))))

      val treeJson =
        t"""{"value":"root","children":[{"value":"a","children":[]},{"value":"b","children":[{"value":"c","children":[]}]}]}"""

      test(m"Derive and encode a tree recursive through a List"):
        tree.in[Json].show
      . assert(_ == treeJson)

      test(m"Derive and decode a tree recursive through a List"):
        treeJson.read[Json].as[Tree]
      . assert(_ == tree)

      test(m"Round-trip a directly-recursive type via Optional"):
        val value = TreeOpt(t"a", TreeOpt(t"b", Unset))
        value.in[Json].show.read[Json].as[TreeOpt]
      . assert(_ == TreeOpt(t"a", TreeOpt(t"b", Unset)))

      test(m"Round-trip recursion through a Map value"):
        val forest = Forest(Map(t"x" -> Tree(t"x", Nil), t"y" -> tree))
        forest.in[Json].show.read[Json].as[Forest]
      . assert(_ == Forest(Map(t"x" -> Tree(t"x", Nil), t"y" -> tree)))

      test(m"A generic product over a recursive type stays structurally derived"):
        Boxed(tree).in[Json].show.read[Json].as[Boxed[Tree]]
      . assert(_ == Boxed(tree))

      test(m"Summon a decoder for a collection of a recursive type at the top level"):
        summon[List[Tree] is Decodable in Json]
      . assert(_ != null)

    suite(m"Json construction"):
      test(m"Json.make with one field"):
        Json.make(name = t"Anna".in[Json]).show
      . assert(_ == t"""{"name":"Anna"}""")

      test(m"Json.make with multiple fields"):
        Json.make(a = 1.in[Json], b = t"two".in[Json], c = true.in[Json]).show
      . assert(_ == t"""{"a":1,"b":"two","c":true}""")

      test(m"Nested Json.make"):
        Json.make(outer = Json.make(inner = 1.in[Json])).show
      . assert(_ == t"""{"outer":{"inner":1}}""")

      test(m"Construct via Json.ast with a long"):
        Json.ast(Json.Ast(7L)).show
      . assert(_ == t"7")

      test(m"Construct via Json.ast with a string"):
        Json.ast(Json.Ast("hello")).show
      . assert(_ == t""""hello"""")

      test(m"Construct via Json.ast with a boolean"):
        Json.ast(Json.Ast(true)).show
      . assert(_ == t"true")

      test(m"Construct via Json.ast with null"):
        Json.ast(Json.Ast(Json.JsonNull)).show
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

      test(m"apply on existing field"):
        obj(t"name").as[Text]
      . assert(_ == t"Alice")

      test(m"apply on absent field returns absent Json"):
        obj(t"missing").as[Optional[Int]]
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

    suite(m"Json.Ast type predicates"):
      test(m"isLong on a long literal"):
        Json.unseal(t"42".read[Json]).isLong
      . assert(identity)

      test(m"isDouble on a decimal literal"):
        Json.unseal(t"3.14".read[Json]).isDouble
      . assert(identity)

      test(m"isNumber on a long literal"):
        Json.unseal(t"42".read[Json]).isNumber
      . assert(identity)

      test(m"isNumber on a double literal"):
        Json.unseal(t"3.14".read[Json]).isNumber
      . assert(identity)

      test(m"isString on a string"):
        Json.unseal(t""""x"""".read[Json]).isString
      . assert(identity)

      test(m"isBoolean on true"):
        Json.unseal(t"true".read[Json]).isBoolean
      . assert(identity)

      test(m"isBoolean on false"):
        Json.unseal(t"false".read[Json]).isBoolean
      . assert(identity)

      test(m"isNull on null"):
        Json.unseal(t"null".read[Json]).isNull
      . assert(identity)

      test(m"isArray on an array"):
        Json.unseal(t"[]".read[Json]).isArray
      . assert(identity)

      test(m"isObject on an object"):
        Json.unseal(t"{}".read[Json]).isObject
      . assert(identity)

      test(m"isString is false for a number"):
        Json.unseal(t"1".read[Json]).isString
      . assert(!_)

      test(m"isLong is false for a string"):
        Json.unseal(t""""x"""".read[Json]).isLong
      . assert(!_)

      test(m"isAbsent on an absent value"):
        Json.unseal(Json.ast(Json.Ast(Unset))).isAbsent
      . assert(identity)

    suite(m"Json.Ast conversions"):
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
        Json.unseal(t""""x"""".read[Json]).primitive
      . assert(_ == JsonPrimitive.String)

      test(m"primitive of a number is Number"):
        Json.unseal(t"7".read[Json]).primitive
      . assert(_ == JsonPrimitive.Number)

      test(m"primitive of a boolean is Boolean"):
        Json.unseal(t"false".read[Json]).primitive
      . assert(_ == JsonPrimitive.Boolean)

      test(m"primitive of an array is Array"):
        Json.unseal(t"[]".read[Json]).primitive
      . assert(_ == JsonPrimitive.Array)

      test(m"primitive of an object is Object"):
        Json.unseal(t"{}".read[Json]).primitive
      . assert(_ == JsonPrimitive.Object)

      test(m"primitive of null is Null"):
        Json.unseal(t"null".read[Json]).primitive
      . assert(_ == JsonPrimitive.Null)

    suite(m"Direct parsing tests"):
      test(m"Read an Int directly"):
        t"42".read[Int in Json]
      . assert(_ == 42)

      test(m"Read a Long directly"):
        t"1234567890123".read[Long in Json]
      . assert(_ == 1234567890123L)

      test(m"Read a Double directly"):
        t"3.1415926".read[Double in Json]
      . assert(_ == 3.1415926)

      test(m"Read a Boolean directly"):
        t"true".read[Boolean in Json]
      . assert(identity)

      test(m"Read a Text directly"):
        t"\"hello world\"".read[Text in Json]
      . assert(_ == t"hello world")

      test(m"Read a string with escapes directly"):
        t"\"a\\nb\\u0041\"".read[Text in Json]
      . assert(_ == t"a\nbA")

      test(m"Read with surrounding whitespace"):
        t"  42  ".read[Int in Json]
      . assert(_ == 42)

      test(m"Direct number coercion matches the AST path"):
        t"3.9".read[Long in Json]
      . assert(_ == 3L)

      test(m"Read a Json value through the direct path"):
        t"""{"a": 1}""".read[Json in Json].as[Json]
      . assert: json =>
          import dynamicJsonAccess.enabled
          unsafely(json.a.as[Int]) == 1

      test(m"A hand-written Parsable reads an object without an AST"):
        given DirectPoint is Json.Parsable = Json.Parsable(Morphology.Any): reader =>
          reader.openObject()
          var x = 0
          var y = 0

          while
            reader.key().lay(false): key =>
              if key == t"x" then x = reader.int()
              else if key == t"y" then y = reader.int()
              else reader.skipValue()
              true
          do ()

          DirectPoint(x, y)

        t"""{"y": 3, "unknown": [1, {"a": false}], "x": 2}""".read[DirectPoint in Json]
      . assert(_ == DirectPoint(2, 3))

      test(m"Trailing content after a direct read raises ParseError"):
        capture[ParseError](t"42 true".read[Int in Json])
      . assert(_.issue match
          case Json.Ast.Issue.SpuriousContent(_) => true
          case _                                 => false)

      test(m"Type mismatch on a direct read raises ParseError"):
        capture[ParseError](t"\"abc\"".read[Int in Json])
      . assert(_.issue match
          case Json.Ast.Issue.ExpectedNumber('"') => true
          case _                                  => false)

      test(m"Malformed JSON on a direct read raises ParseError"):
        capture[ParseError](t"tru".read[Boolean in Json])
      . assert(_.issue match
          case Json.Ast.Issue.PrematureEnd | Json.Ast.Issue.ExpectedTrue => true
          case _                                                         => false)

    suite(m"Derived direct parsing tests"):
      given Person is Json.Parsable = Json.Parsable.derived
      given Band is Json.Parsable = Json.Parsable.derived
      given WithDefault is Json.Parsable = Json.Parsable.derived
      given Renamed is Json.Parsable = Json.Parsable.derived
      given FooOption is Json.Parsable = Json.Parsable.derived
      given FooOptional is Json.Parsable = Json.Parsable.derived
      given Org is Json.Parsable = Json.Parsable.derived
      given Tree is Json.Parsable = Json.Parsable.derived
      given TreeOpt is Json.Parsable = Json.Parsable.derived
      given Forest is Json.Parsable = Json.Parsable.derived
      given Empty is Json.Parsable = Json.Parsable.derived
      given NewBand is Json.Parsable = Json.Parsable.derived
      given Status is Json.Parsable = Json.Parsable.derived
      given Boxed[Int] is Json.Parsable = Json.Parsable.derived

      // The acceptance criterion for derivation: the direct read must equal
      // the AST-path read, for the same input.
      inline def parity[value](json: Text)
        ( using value is Json.Parsable, value is Json.Decodable )
      :   Boolean =
        json.read[value in Json] == json.read[Json].as[value]

      test(m"Derive a direct product parser"):
        t"""{"name": "Amy", "age": 50}""".read[Person in Json]
      . assert(_ == Person(t"Amy", 50))

      test(m"Derived parser accepts reordered fields"):
        t"""{"age": 50, "name": "Amy"}""".read[Person in Json]
      . assert(_ == Person(t"Amy", 50))

      test(m"Derived parser skips unknown fields, including nested ones"):
        t"""{"name": "Amy", "extra": {"deep": [1, {"x": null}]}, "age": 50}"""
        . read[Person in Json]
      . assert(_ == Person(t"Amy", 50))

      test(m"Duplicate keys are last-wins, as on the AST path"):
        parity[Person](t"""{"name": "Amy", "name": "Bea", "age": 50}""")
      . assert(identity)

      test(m"A missing field takes the declared default"):
        t"""{"name": "Kid"}""".read[WithDefault in Json]
      . assert(_ == WithDefault(t"Kid", 18))

      test(m"A missing required field raises JsonError Absent"):
        capture[JsonError](t"""{"age": 50}""".read[Person in Json])
      . assert(_.reason match
          case JsonError.Reason.Absent => true
          case _                       => false)

      test(m"@name renames apply to direct parsing"):
        parity[Renamed](t"""{"first_name": "Jon", "yob": 1983}""")
      . assert(identity)

      test(m"An absent Option field is None"):
        t"""{"x": 1}""".read[FooOption in Json]
      . assert(_ == FooOption(1, None))

      test(m"An Optional field reads null and absent as Unset, a value as itself"):
        ( t"""{"x": 1}""".read[FooOptional in Json],
          t"""{"x": 1, "y": null}""".read[FooOptional in Json],
          t"""{"x": 1, "y": 2}""".read[FooOptional in Json] )
      . assert(_ == (FooOptional(1, Unset), FooOptional(1, Unset), FooOptional(1, 2)))

      test(m"Nested products and collections parse directly"):
        val json = t"""{"name": "Acme", "leader":
            {"name": "Bob", "age": 40, "roles": [{"name": "CEO"}, {"name": "CTO"}]}}"""

        json.read[Org in Json]
      . assert(_ == Org("Acme", Entity("Bob", 40, List(Role("CEO"), Role("CTO")))))

      test(m"A full record round-trips equally on both paths"):
        val json = t"""{"guitarists": [{"name": "John", "age": 40}, {"name": "George", "age":
            58}], "drummer": {"name": "Ringo", "age": 82}, "bassist": {"name": "Paul", "age":
            81}}"""

        parity[Band](json)
      . assert(identity)

      test(m"Recursive types parse directly"):
        parity[Tree](t"""{"value": "a", "children": [{"value": "b", "children": []}]}""")
      . assert(identity)

      test(m"Recursion through an Optional parses directly"):
        parity[TreeOpt](t"""{"value": "a", "child": {"value": "b"}}""")
      . assert(identity)

      test(m"Recursion through a Map parses directly"):
        parity[Forest](t"""{"trees": {"oak": {"value": "a", "children": []}}}""")
      . assert(identity)

      test(m"An empty case class parses from an object with spurious fields"):
        t"""{"whatever": 42}""".read[Empty in Json]
      . assert(_ == Empty())

      test(m"A generic product parses directly"):
        t"""{"value": 42}""".read[Boxed[Int] in Json]
      . assert(_ == Boxed(42))

      test(m"A sum dispatches by discriminator, anywhere in the object"):
        parity[Status](t"""{"since": 2020, "kind": "ok"}""")
      . assert(identity)

      test(m"A collection of a derived sum parses equally on both paths"):
        val json = t"""{"members": [{"person": {"name": "Paul", "age": 81}, "kind": "Bassist"},
            {"person": {"name": "Ringo", "age": 82}, "kind": "Drummer"}]}"""

        parity[NewBand](json)
      . assert(identity)

      test(m"A top-level collection of an opted-in element reads directly"):
        t"""[{"name": "Amy", "age": 50}, {"name": "Bea", "age": 60}]"""
        . read[List[Person] in Json]
      . assert(_ == List(Person(t"Amy", 50), Person(t"Bea", 60)))

      test(m"A mistyped directly-parsed field raises ParseError"):
        // On the AST path this is a `JsonError`; token-level readers report
        // type mismatches as parse errors, with source positions.
        capture[ParseError](t"""{"name": "Amy", "age": "x"}""".read[Person in Json])
      . assert(_.issue match
          case Json.Ast.Issue.ExpectedNumber('"') => true
          case _                                  => false)

      test(m"A whole Data block reads directly, without stream plumbing"):
        t"""{"name": "Amy", "age": 50}""".in[Data].read[Person in Json]
      . assert(_ == Person(t"Amy", 50))

      test(m"An 18-digit-plus integer falls back to the general number path"):
        parity[Person](t"""{"name": "Old", "age": 99999999999999999999}""")
      . assert(identity)

      test(m"A leading zero raises the same issue on both paths"):
        val direct = capture[ParseError](t"""{"name": "Amy", "age": 0123}""".read[Person in Json])
        val ast = capture[ParseError](t"""{"name": "Amy", "age": 0123}""".read[Json])
        (direct.issue, ast.issue)
      . assert: (directIssue, astIssue) =>
          directIssue == astIssue

      test(m"A negative integer reads on the fast path"):
        t"""{"name": "Amy", "age": -3}""".read[Person in Json]
      . assert(_ == Person(t"Amy", -3))

      test(m"Malformed content inside a skipped field raises ParseError"):
        capture[ParseError](t"""{"name": "Amy", "junk": {"x": }, "age": 50}""".read[Person in Json])
      . assert(_.issue match
          case Json.Ast.Issue.ExpectedSomeValue(_) => true
          case _                                   => false)

      test(m"Deeply nested skipped values are scanned correctly"):
        t"""{"junk": [[[{"a": [1, {"b": "x\\n"}, null, true]}]]], "name": "Amy", "age": 50}"""
        . read[Person in Json]
      . assert(_ == Person(t"Amy", 50))

      test(m"A custom Decodable beats derivation via fromDecodable"):
        // The documented escape hatch for a type whose hand-written decoder
        // must win over structural derivation.
        val decodable: Worker is Json.Decodable =
          Json.Decodable(Morphology.Str): json =>
            unsafely(Worker(json.root.string, 0))

        given Worker is Json.Parsable = Json.Parsable.fromDecodable(decodable)

        t"""\"Syd\"""".read[Worker in Json]
      . assert(_ == Worker(t"Syd", 0))

    suite(m"Sum encoding tests"):
      test(m"Wrapper encoding writes a single-key object"):
        given Shape is Discriminable in Json = Json.DiscriminantWrapper()
        (Shape.Circle(1.0): Shape).in[Json].show
      . assert(_ == t"""{"Circle":{"radius":1.0}}""")

      test(m"Wrapper encoding decodes via the AST"):
        given Shape is Discriminable in Json = Json.DiscriminantWrapper()
        t"""{"Circle": {"radius": 2.5}}""".read[Json].as[Shape]
      . assert(_ == Shape.Circle(2.5))

      test(m"Wrapper encoding parses directly"):
        given Shape is Discriminable in Json = Json.DiscriminantWrapper()
        given Shape is Json.Parsable = Json.Parsable.derived
        t"""{"Circle": {"radius": 2.5}}""".read[Shape in Json]
      . assert(_ == Shape.Circle(2.5))

      test(m"A multi-key wrapper raises JsonError Absent on both paths"):
        given Shape is Discriminable in Json = Json.DiscriminantWrapper()
        given Shape is Json.Parsable = Json.Parsable.derived
        val json = t"""{"Circle": {"radius": 1.0}, "junk": 1}"""

        ( capture[JsonError](json.read[Shape in Json]).reason,
          capture[JsonError](json.read[Json].as[Shape]).reason )
      . assert(_ == (JsonError.Reason.Absent, JsonError.Reason.Absent))

      test(m"Envelope encoding writes tag and value fields"):
        given Shape is Discriminable in Json = Json.DiscriminantEnvelope(t"type", t"value")
        (Shape.Square(2.0): Shape).in[Json].show
      . assert(_ == t"""{"type":"Square","value":{"side":2.0}}""")

      test(m"Envelope encoding decodes via the AST"):
        given Shape is Discriminable in Json = Json.DiscriminantEnvelope(t"type", t"value")
        t"""{"type": "Square", "value": {"side": 3.0}}""".read[Json].as[Shape]
      . assert(_ == Shape.Square(3.0))

      test(m"Envelope encoding parses directly, tag first"):
        given Shape is Discriminable in Json = Json.DiscriminantEnvelope(t"type", t"value")
        given Shape is Json.Parsable = Json.Parsable.derived
        t"""{"type": "Square", "value": {"side": 3.0}}""".read[Shape in Json]
      . assert(_ == Shape.Square(3.0))

      test(m"Envelope encoding parses directly, tag last"):
        given Shape is Discriminable in Json = Json.DiscriminantEnvelope(t"type", t"value")
        given Shape is Json.Parsable = Json.Parsable.derived
        t"""{"value": {"side": 3.0}, "type": "Square"}""".read[Shape in Json]
      . assert(_ == Shape.Square(3.0))

      test(m"Envelope encoding decodes via the AST, tag last"):
        given Shape is Discriminable in Json = Json.DiscriminantEnvelope(t"type", t"value")
        t"""{"value": {"side": 3.0}, "type": "Square"}""".read[Json].as[Shape]
      . assert(_ == Shape.Square(3.0))

      test(m"An internal-field sum parses directly with the tag anywhere"):
        given Status is Json.Parsable = Json.Parsable.derived
        t"""{"since": 2020, "kind": "ok"}""".read[Status in Json]
      . assert(_ == Status.Active(2020))

      test(m"An internal-field sum with a missing tag raises Absent directly"):
        given Status is Json.Parsable = Json.Parsable.derived
        capture[JsonError](t"""{"since": 2020}""".read[Status in Json]).reason
      . assert(_ == JsonError.Reason.Absent)

      test(m"A custom Discriminable falls back to the AST bridge"):
        given Shape is Discriminable in Json = new Discriminable:
          type Form = Json
          type Self = Shape
          def rewrite(kind: Text, json: Json): Json = Json.DiscriminantWrapper[Shape]().rewrite(kind, json)
          def discriminate(json: Json): Optional[Text] = Json.DiscriminantWrapper[Shape]().discriminate(json)
          def variant(json: Json): Json = Json.DiscriminantWrapper[Shape]().variant(json)

        given Shape is Json.Parsable = Json.Parsable.derived
        t"""{"Circle": {"radius": 4.5}}""".read[Shape in Json]
      . assert(_ == Shape.Circle(4.5))

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
        import formatting.compactJsonFormatting
        Json.make(a = 1.in[Json], b = 2.in[Json]).show
      . assert(_ == t"""{"a":1,"b":2}""")

      test(m"Indented printer adds whitespace"):
        import formatting.indentedJsonFormatting
        val printed = Json.make(a = 1.in[Json], b = 2.in[Json]).show
        printed.contains(t"\n")
      . assert(identity)

      test(m"Indented printer pretty-prints arrays"):
        import formatting.indentedJsonFormatting
        val printed = List(1, 2, 3).in[Json].show
        printed.contains(t"\n")
      . assert(identity)

    suite(m"Discriminator strategies"):
      test(m"Discriminate by 'kind' (default in this file)"):
        val s: Shape = Shape.Circle(1.0)
        s.in[Json].show
      . assert(_ == t"""{"radius":1.0,"kind":"Circle"}""")

      test(m"Decode disjunction by 'kind'"):
        val s: Shape = Shape.Square(2.0)
        s.in[Json].as[Shape]
      . assert(_ == Shape.Square(2.0))

      test(m"@name renames a variant's discriminator"):
        (Status.Active(5): Status).in[Json].show
      . assert(_ == t"""{"since":5,"kind":"ok"}""")

      test(m"@name variants round-trip"):
        List(Status.Active(5), Status.Removed(9), Status.Pending(1)).map(_.in[Json].as[Status])
      . assert(_ == List(Status.Active(5), Status.Removed(9), Status.Pending(1)))

      locally:
        import discriminables.jsonByTypeDiscriminable

        test(m"Discriminate by 'type'"):
          val a: Animal = Animal.Dog(t"Rex")
          a.in[Json].show
        . assert(_ == t"""{"name":"Rex","type":"Dog"}""")

        test(m"Decode disjunction by 'type'"):
          val a: Animal = Animal.Cat(t"Whiskers")
          a.in[Json].as[Animal]
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
        val err = JsonPointerError(JsonPointerError.Reason.UnknownDocument, 0)
        err.message.text.s.contains("registry")
      . assert(identity)

      test(m"Standalone registry returns Unset for unknown URLs"):
        import jsonPointerRegistries.standaloneRegistry
        val registry = summon[JsonPointer.Registry]
        registry(url"http://example.com/")
      . assert(_ == Unset)

      test(m"Standalone registry returns updated values"):
        import jsonPointerRegistries.standaloneRegistry
        val registry = summon[JsonPointer.Registry]
        val doc = t"""{"a": 1}""".read[Json]
        registry(url"http://example.com/doc") = doc
        registry(url"http://example.com/doc") == doc
      . assert(identity)

    suite(m"Time encodables/decodables"):
      import encodables.instantJsonEncodable
      import encodables.durationJsonEncodable
      import decodables.instantJsonDecodable
      import decodables.durationJsonDecodable
      import aviation.*
      import chronometries.unix
      import abstractables.instantAbstractable

      test(m"Encode an Instant as a Long"):
        Instant(1700000000000L).in[Json].show
      . assert(_ == t"1700000000000")

      test(m"Decode an Instant from a Long"):
        t"1700000000000".read[Json].as[Instant over Unix].long
      . assert(_ == 1700000000000L)

      test(m"Round-trip an Instant"):
        Instant(1234567890L).in[Json].as[Instant over Unix].long
      . assert(_ == 1234567890L)

      test(m"Encode a Duration as a Long of milliseconds"):
        Duration(5000L).in[Json].show
      . assert(_ == t"5000")

      test(m"Decode a Duration from a Long of milliseconds"):
        t"5000".read[Json].as[Duration].value
      . assert(_ == 5.0)

      test(m"Round-trip a Duration"):
        Duration(60_000L).in[Json].as[Duration].value
      . assert(_ == 60.0)

    suite(m"JsonSchema tests"):
      test(m"Schematic for Int yields an Integer schema"):
        infer[Int is Schematic over JsonSchema].schema()
      . assert(_ == JsonSchema.Integer())

      test(m"Schematic for Long yields an Integer schema"):
        infer[Long is Schematic over JsonSchema].schema()
      . assert(_ == JsonSchema.Integer())

      test(m"Schematic for Text yields a String schema"):
        infer[Text is Schematic over JsonSchema].schema()
      . assert(_ == JsonSchema.String())

      test(m"Schematic for Double yields a Number schema"):
        infer[Double is Schematic over JsonSchema].schema()
      . assert(_ == JsonSchema.Number())

      test(m"Schematic for Boolean yields a Boolean schema"):
        infer[Boolean is Schematic over JsonSchema].schema()
      . assert(_ == JsonSchema.Boolean())

      test(m"Schematic for Optional[Int] is an optional Integer"):
        infer[Optional[Int] is Schematic over JsonSchema].schema()
      . assert:
          case s: JsonSchema.Integer => s.optional
          case _                     => false

      test(m"Schematic for List[Int] is an Array of Integers"):
        infer[List[Int] is Schematic over JsonSchema].schema()
      . assert:
          case JsonSchema.Array(_, JsonSchema.Integer(_, _, _, _, _, _), _, _, _, _, _) => true
          case _                                                                         => false

      test(m"Schematic for Set[Text] is an Array of Strings"):
        infer[Set[Text] is Schematic over JsonSchema].schema()
      . assert:
          case JsonSchema.Array(_, JsonSchema.String(_, _, _, _, _, _), _, _, _, _, _) => true
          case _                                                                         => false

      test(m"Schematic for Map[Text, Int] is an object with additionalProperties"):
        infer[Map[Text, Int] is Schematic over JsonSchema].schema()
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

      test(m"@memo annotation sets a description on a field"):
        JsonSchema.derived[Marked].schema() match
          case obj: JsonSchema.Object => obj.properties.at(t"n").let(_.description).or(t"")
          case _                      => t""
      . assert(_ == t"the count")

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
        s.in[Json].show.contains(t""""type":"string"""")
      . assert(identity)

      test(m"JsonSchema serializes an Integer schema with type field"):
        val s: JsonSchema = JsonSchema.Integer()
        s.in[Json].show.contains(t""""type":"integer"""")
      . assert(identity)

      test(m"JsonSchema serializes a Boolean schema with type field"):
        val s: JsonSchema = JsonSchema.Boolean()
        s.in[Json].show.contains(t""""type":"boolean"""")
      . assert(identity)

      test(m"JsonSchema serializes a Null schema with type field"):
        val s: JsonSchema = JsonSchema.Null()
        s.in[Json].show.contains(t""""type":"null"""")
      . assert(identity)

      test(m"JsonSchema serializes an Object schema with type field"):
        val s: JsonSchema = JsonSchema.Object()
        s.in[Json].show.contains(t""""type":"object"""")
      . assert(identity)

      test(m"JsonSchema Format encodes as kebab case"):
        JsonSchema.Format.DateTime.encode
      . assert(_ == t"date-time")

      test(m"JsonSchema Format decodes from kebab case"):
        t"date-time".as[JsonSchema.Format]
      . assert(_ == JsonSchema.Format.DateTime)

      test(m"JsonSchema Format roundtrips Email"):
        JsonSchema.Format.Email.encode.as[JsonSchema.Format]
      . assert(_ == JsonSchema.Format.Email)

    suite(m"Ndjson tests"):
      test(m"Ndjson stream of three values decodes to a List"):
        val stream = LazyList(
          t"1".read[Json],
          t"2".read[Json],
          t"3".read[Json])
        Ndjson(stream).stream.map(_.as[Int]).to(List)
      . assert(_ == List(1, 2, 3))

      test(m"Ndjson can hold heterogeneous values"):
        val stream = LazyList(
          t""""hi"""".read[Json],
          t"42".read[Json],
          t"true".read[Json])
        Ndjson(stream).stream.length
      . assert(_ == 3)

    suite(m"j\"\" interpolator"):
      test(m"Construct a number"):
        j"42"
      . assert(_ == t"42".read[Json])

      test(m"Construct an empty object"):
        j"{}"
      . assert(_ == t"{}".read[Json])

      test(m"Construct an empty array"):
        j"[]"
      . assert(_ == t"[]".read[Json])

      test(m"Object with literal value"):
        j"""{"a": 1}"""
      . assert(_ == t"""{"a": 1}""".read[Json])

      test(m"Array with literals"):
        j"""[1, 2, 3]"""
      . assert(_ == t"""[1, 2, 3]""".read[Json])

      test(m"Object with single value-position hole"):
        val x = 42
        j"""{"a": $x}"""
      . assert(_ == t"""{"a": 42}""".read[Json])

      test(m"Array with single value-position hole"):
        val x = 42
        j"""[1, $x, 3]"""
      . assert(_ == t"""[1, 42, 3]""".read[Json])

      test(m"Top-level value hole"):
        val x: Json = t"""{"y": true}""".read[Json]
        j"$x"
      . assert(_ == t"""{"y": true}""".read[Json])

      test(m"String literal"):
        j""""hello""""
      . assert(_ == t""""hello"""".read[Json])

      test(m"String with interior hole"):
        val name = t"world"
        j""""hello, $name!""""
      . assert(_ == t""""hello, world!"""".read[Json])

      test(m"Array tail spread"):
        val xs: List[Int] = List(2, 3, 4)
        j"""[1, $xs*]"""
      . assert(_ == t"""[1, 2, 3, 4]""".read[Json])

      test(m"Object rest splice"):
        val rest: Map[Text, Json] = Map(
          t"b" -> t"2".read[Json],
          t"c" -> t"3".read[Json])

        j"""{"a": 1, $rest}"""
      . assert(_ == t"""{"a": 1, "b": 2, "c": 3}""".read[Json])

    suite(m"j\"\" extractor"):
      test(m"Extract single value"):
        val scrutinee: Json = t"""{"a": 42}""".read[Json]
        scrutinee match
          case j"""{"a": $a}""" => a.as[Int]
          case _                => -1
      . assert(_ == 42)

      test(m"Match exact array"):
        val scrutinee: Json = t"""[1, 2, 3]""".read[Json]
        scrutinee match
          case j"""[$a, $b, $c]""" => (a.as[Int], b.as[Int], c.as[Int])
          case _                   => (0, 0, 0)
      . assert(_ == (1, 2, 3))

      test(m"Mismatched array length is rejected"):
        val scrutinee: Json = t"""[1, 2]""".read[Json]
        scrutinee match
          case j"""[$a, $b, $c]""" => true
          case _                   => false
      . assert(!_)

      test(m"Head/tail destructure"):
        val scrutinee: Json = t"""[1, 2, 3, 4]""".read[Json]
        scrutinee match
          case j"""[$head, $tail*]""" => (head.as[Int], tail.as[List[Int]])
          case _                      => (0, Nil)
      . assert(_ == (1, List(2, 3, 4)))

      test(m"Capture top-level Json"):
        val scrutinee: Json = t"""{"k": 1}""".read[Json]

        (scrutinee: @unchecked) match
          case j"$x" => x
      . assert(_ == t"""{"k": 1}""".read[Json])

      test(m"Match number literal"):
        val scrutinee: Json = t"""{"a": 42}""".read[Json]
        scrutinee match
          case j"""{"a": 42}""" => true
          case _                => false
      . assert(identity)

    suite(m"Compile-time hole-position errors"):
      test(m"non-Iterable spread is highlighted at the splice"):
        val bad: Int = 42
        demilitarize:
          j"""[$bad*]"""
        . map(_.focus)
      . assert(_ == List("bad"))

      test(m"parse error focus is inside the literal, not the whole thing"):
        val errors = demilitarize:
          j""" {"a": 1, } """
        // The literal is 19 chars; a precise focus on the trailing `,` is shorter.
        errors.map(_.focus.length < 19)
      . assert(_ == List(true))

      test(m"non-Iterable spread honours the `*` source skip"):
        // The spread literal has a `*` prefix in the second part that the
        // parser doesn't see. The error should still focus on `bad`, not
        // the splice plus the `*`.
        val bad: Int = 42
        demilitarize:
          j"""[$bad*]"""
        . map(_.focus)
      . assert(_ == List("bad"))

    suite(m"JSON pointer interpolator"):
      test(m"a same-document pointer parses"):
        jp"#/foo/bar".encode
      . assert(_ == t"#/foo/bar")

      test(m"the whole-document pointer parses"):
        jp"#".encode
      . assert(_ == t"#")

      test(m"a pointer not beginning with '#' is rejected at the first character"):
        demilitarize:
          jp"/foo/bar"
        . map(_.focus)
      . assert(_ == List("/"))

      test(m"a fragment not beginning with '/' is rejected after the '#'"):
        demilitarize:
          jp"#foo"
        . map(_.focus)
      . assert(_ == List("f"))

      test(m"a malformed '~' escape is rejected at the '~'"):
        demilitarize:
          jp"#/foo~2bar"
        . map(_.focus)
      . assert(_ == List("~"))

    suite(m"Specific per-path overrides"):
      val firm = Firm(Worker(t"ann", 30), Worker(t"bob", 40))
      val shout: Text is Json.Encodable = Json.Encodable(() => Morphology.Str): text => Json(text.upper)

      test(m"Without a Specific, all fields use default encoders"):
        firm.in[Json].show
      . assert(_ == t"""{"boss":{"name":"ann","age":30},"deputy":{"name":"bob","age":40}}""")

      test(m"A Specific overrides one field path along its spine only"):
        given (Firm is Specific over Json.Encodable) =
          specifically:
            case root.deputy.name() => shout

        firm.in[Json].show
      . assert(_ == t"""{"boss":{"name":"ann","age":30},"deputy":{"name":"BOB","age":40}}""")

      test(m"a collection element is overridden via a local given + re-derive"):
        val nameOnly: Worker is Json.Encodable = Json.Encodable(() => Morphology.Str): w => Json(w.name)

        given (Crew is Specific over Json.Encodable) =
          specifically:
            case root.members() =>
              given Worker is Json.Encodable = nameOnly
              summon[List[Worker] is Json.Encodable]

        Crew(Worker(t"al", 30), List(Worker(t"bo", 40), Worker(t"cy", 50))).in[Json].show
      . assert(_ == t"""{"lead":{"name":"al","age":30},"members":["bo","cy"]}""")

    ValidationTests()
    PositionTests()
    VerifyTests()
