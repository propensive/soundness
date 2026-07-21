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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package breviloquence

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

case class Point(x: Int, y: Int) derives CanEqual
case class Person(name: Text, age: Int) derives CanEqual
case class Wrapper(values: List[Int], label: Text) derives CanEqual

// Recursion through a collection (#1429) and a generic product used over a recursive type.
case class Tree(value: Text, children: List[Tree]) derives CanEqual
case class Boxed[value](value: value) derives CanEqual
case class Team(lead: Person, size: Int) derives CanEqual
case class OptPerson(name: Text, age: Optional[Int]) derives CanEqual
case class Renamed
   (@name[Cbor](t"data_files")  dataFiles:  List[Long],
    @name[Cbor](t"index_files") indexFiles: List[Long])
derives CanEqual

enum Shape derives CanEqual:
  case Circle(radius: Double)
  case Square(side: Double)

// Model types for the direct-parsing (`Inlinable`) suite. Same-run types
// stay on the structural ladder: the staging summon refuses them, which is
// exactly the degradation the suite exercises.
case class Defaulted(x: Int, y: Int = 7) derives CanEqual
case class Wide(seventeenCharacter: Int, y: Int) derives CanEqual
case class Blob(data: IArray[Byte])
case class Nums(values: List[Int]) derives CanEqual
case class Mixed(a: Double, b: Boolean, c: Text) derives CanEqual

enum CStatus derives CanEqual:
  @name[Cbor](t"ok") case Active(since: Int)
  @name(t"gone")     case Removed(at: Int)
                     case Pending(at: Int)

given (CStatus is Discriminable in Cbor) = Cbor.discriminatedUnion(t"kind")

private def hex(s: String): IArray[Byte] =
  val clean = s.filter(c => !c.isWhitespace)
  val out = new Array[Byte](clean.length/2)
  var index = 0
  while index < out.length do
    out(index) = Integer.parseInt(clean.substring(index*2, index*2 + 2), 16).toByte
    index += 1
  out.asInstanceOf[IArray[Byte]]

private def hexOf(bytes: IArray[Byte]): String =
  val sb = new StringBuilder
  var index = 0
  while index < bytes.length do
    sb.append(f"${bytes(index) & 0xFF}%02x")
    index += 1
  sb.toString

object Tests extends Suite(m"Breviloquence Tests"):
  def run(): Unit =
    suite(m"Parsing primitives"):
      test(m"Parse 0"):
        Cbor.ast(Cbor.Ast.parse(hex("00"))).as[Int]
      . assert(_ == 0)

      test(m"Parse 1"):
        Cbor.ast(Cbor.Ast.parse(hex("01"))).as[Int]
      . assert(_ == 1)

      test(m"Parse 23 (single-byte head)"):
        Cbor.ast(Cbor.Ast.parse(hex("17"))).as[Int]
      . assert(_ == 23)

      test(m"Parse 24 (1-byte payload)"):
        Cbor.ast(Cbor.Ast.parse(hex("1818"))).as[Int]
      . assert(_ == 24)

      test(m"Parse 1000 (2-byte payload)"):
        Cbor.ast(Cbor.Ast.parse(hex("1903e8"))).as[Int]
      . assert(_ == 1000)

      test(m"Parse 1000000 (4-byte payload)"):
        Cbor.ast(Cbor.Ast.parse(hex("1a000f4240"))).as[Long]
      . assert(_ == 1000000L)

      test(m"Parse 1000000000000 (8-byte payload)"):
        Cbor.ast(Cbor.Ast.parse(hex("1b000000e8d4a51000"))).as[Long]
      . assert(_ == 1000000000000L)

      test(m"Parse -1"):
        Cbor.ast(Cbor.Ast.parse(hex("20"))).as[Int]
      . assert(_ == -1)

      test(m"Parse -100"):
        Cbor.ast(Cbor.Ast.parse(hex("3863"))).as[Int]
      . assert(_ == -100)

      test(m"Parse false"):
        Cbor.ast(Cbor.Ast.parse(hex("f4"))).as[Boolean]
      . assert(!_)

      test(m"Parse true"):
        Cbor.ast(Cbor.Ast.parse(hex("f5"))).as[Boolean]
      . assert(identity)

      test(m"Parse null as Unit"):
        Cbor.ast(Cbor.Ast.parse(hex("f6"))).as[Unit]
      . assert(_ == ())

      test(m"Parse double-precision float"):
        Cbor.ast(Cbor.Ast.parse(hex("fb3ff199999999999a"))).as[Double]
      . assert(_ == 1.1)

      test(m"Parse single-precision float (3.4028235e38)"):
        // 0xfa 7f7fffff = max positive float
        val v = Cbor.ast(Cbor.Ast.parse(hex("fa7f7fffff"))).as[Double]
        math.abs(v - 3.4028234663852886e38) < 1e30
      . assert(identity)

      test(m"Parse half-precision float 1.0"):
        Cbor.ast(Cbor.Ast.parse(hex("f93c00"))).as[Double]
      . assert(_ == 1.0)

    suite(m"Parsing strings"):
      test(m"Parse empty text string"):
        Cbor.ast(Cbor.Ast.parse(hex("60"))).as[Text]
      . assert(_ == t"")

      test(m"Parse text 'a'"):
        Cbor.ast(Cbor.Ast.parse(hex("6161"))).as[Text]
      . assert(_ == t"a")

      test(m"Parse text 'IETF'"):
        Cbor.ast(Cbor.Ast.parse(hex("6449455446"))).as[Text]
      . assert(_ == t"IETF")

      test(m"Parse byte string [01 02 03 04]"):
        val bytes = Cbor.ast(Cbor.Ast.parse(hex("4401020304"))).as[IArray[Byte]]
        bytes.toList
      . assert(_ == List[Byte](1, 2, 3, 4))

    suite(m"Parsing arrays"):
      test(m"Parse empty array"):
        Cbor.ast(Cbor.Ast.parse(hex("80"))).as[List[Int]]
      . assert(_ == Nil)

      test(m"Parse [1, 2, 3]"):
        Cbor.ast(Cbor.Ast.parse(hex("83010203"))).as[List[Int]]
      . assert(_ == List(1, 2, 3))

      test(m"Parse indefinite-length array [1, 2, 3]"):
        Cbor.ast(Cbor.Ast.parse(hex("9f010203ff"))).as[List[Int]]
      . assert(_ == List(1, 2, 3))

    suite(m"Parsing maps"):
      test(m"Parse empty map"):
        Cbor.ast(Cbor.Ast.parse(hex("a0"))).as[Map[Text, Int]]
      . assert(_ == Map())

      test(m"Parse {a: 1}"):
        Cbor.ast(Cbor.Ast.parse(hex("a1616101"))).as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1))

      test(m"Parse {a: 1, b: 2}"):
        Cbor.ast(Cbor.Ast.parse(hex("a26161016162 02"))).as[Map[Text, Int]]
      . assert(_ == Map(t"a" -> 1, t"b" -> 2))

    suite(m"Tags"):
      test(m"Tag 1 (epoch time) preserves tag and inner value"):
        val cbor = Cbor.ast(Cbor.Ast.parse(hex("c11a514b67b0")))
        val ast = Cbor.unseal(cbor)
        ast.isTag && ast.tag.tag == 1L
      . assert(identity)

    suite(m"Encoder"):
      test(m"Round-trip integer 42"):
        val original = hex("182a")
        val ast = Cbor.Ast.parse(original)
        hexOf(Cbor.Ast.encodable.encoded(ast))
      . assert(_ == "182a")

      test(m"Round-trip [1, 2, 3]"):
        val original = hex("83010203")
        val ast = Cbor.Ast.parse(original)
        hexOf(Cbor.Ast.encodable.encoded(ast))
      . assert(_ == "83010203")

      test(m"Round-trip {a: 1, b: 2}"):
        val original = hex("a26161016162 02")
        val ast = Cbor.Ast.parse(original)
        hexOf(Cbor.Ast.encodable.encoded(ast))
      . assert(_ == "a2616101616202")

    suite(m"Diagnostic notation"):
      test(m"Render 42 as '42'"):
        Cbor.Ast.parse(hex("182a")).show
      . assert(_ == t"42")

      test(m"Render [1, 2, 3]"):
        Cbor.Ast.parse(hex("83010203")).show
      . assert(_ == t"[1, 2, 3]")

      test(m"Render byte string as hex"):
        Cbor.Ast.parse(hex("4401020304")).show
      . assert(_ == t"h'01020304'")

    suite(m"Generic derivation"):
      test(m"Encode Point(1, 2)"):
        val cbor = Point(1, 2).in[Cbor]
        val ast = Cbor.unseal(cbor)
        ast.isMap && ast.entries == 2
      . assert(identity)

      test(m"Round-trip Point(3, 4)"):
        val cbor = Point(3, 4).in[Cbor]
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(cbor))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[Point]
      . assert(_ == Point(3, 4))

      test(m"Round-trip Person(\"Ada\", 36)"):
        val cbor = Person(t"Ada", 36).in[Cbor]
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(cbor))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[Person]
      . assert(_ == Person(t"Ada", 36))

      test(m"Round-trip Wrapper with list"):
        val original = Wrapper(List(1, 2, 3), t"hello")
        val cbor = original.in[Cbor]
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(cbor))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[Wrapper] == original
      . assert(identity)

      val tree = Tree(t"root", List(Tree(t"a", Nil), Tree(t"b", List(Tree(t"c", Nil)))))

      test(m"Round-trip a type recursive through a List"):
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(tree.in[Cbor]))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[Tree] == tree
      . assert(identity)

      test(m"A generic product over a recursive type stays structurally derived"):
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(Boxed(tree).in[Cbor]))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[Boxed[Tree]] == Boxed(tree)
      . assert(identity)

    suite(m"Aggregable"):
      test(m"Aggregate single-chunk LazyList[Data] to Cbor"):
        val original = Point(3, 4)
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(original.in[Cbor]))
        LazyList(bytes).read[Cbor].as[Point]
      . assert(_ == Point(3, 4))

      test(m"Aggregate split-chunk LazyList[Data] to Cbor"):
        val original = Person(t"Ada", 36)
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(original.in[Cbor]))
        val half = bytes.length/2
        LazyList(bytes.slice(0, half), bytes.slice(half, bytes.length)).read[Cbor].as[Person]
      . assert(_ == Person(t"Ada", 36))

      test(m"Aggregate single-chunk LazyList[Data] to Cbor.Ast"):
        val original = Wrapper(List(1, 2, 3), t"hi")
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(original.in[Cbor]))
        Cbor.ast(LazyList(bytes).read[Cbor.Ast]).as[Wrapper]
      . assert(_ == Wrapper(List(1, 2, 3), t"hi"))

    suite(m"`in Cbor` decoder shorthand"):
      test(m"`read[T in Cbor]` resolves a value directly from bytes"):
        val original = Point(3, 4)
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(original.in[Cbor]))
        LazyList(bytes).read[Point in Cbor]
      . assert(_ == Point(3, 4))

      test(m"`read[T in Cbor]` works for nested case classes"):
        val original = Wrapper(List(1, 2, 3), t"hi")
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(original.in[Cbor]))
        LazyList(bytes).read[Wrapper in Cbor]
      . assert(_ == Wrapper(List(1, 2, 3), t"hi"))

    suite(m"@name field renaming"):
      test(m"Encode renames fields to wire keys"):
        val cbor = Renamed(List(1L, 2L), List(3L)).in[Cbor]
        val ast = Cbor.unseal(cbor)
        val keys = (0 until ast.entries).map(ast.key(_).string).toSet
        keys == Set("data_files", "index_files")
      . assert(identity)

      test(m"Decode reads wire keys back into Scala fields"):
        val original = Renamed(List(10L, 20L, 30L), List(99L))
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(original.in[Cbor]))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[Renamed]
      . assert(_ == Renamed(List(10L, 20L, 30L), List(99L)))

      test(m"No relabelling uses original field names"):
        val original = Wrapper(List(1, 2, 3), t"x")
        val cbor = original.in[Cbor]
        val ast = Cbor.unseal(cbor)
        val keys = (0 until ast.entries).map(ast.key(_).string).toSet
        keys == Set("values", "label")
      . assert(identity)

      test(m"@name renames a variant's discriminator"):
        val ast = Cbor.unseal((CStatus.Active(5): CStatus).in[Cbor])
        (0 until ast.entries).collectFirst:
          case i if ast.key(i).string == "kind" => ast.value(i).string
        . getOrElse("none")
      . assert(_ == "ok")

      test(m"@name variants round-trip"):
        List(CStatus.Active(5), CStatus.Removed(9), CStatus.Pending(1)).map: status =>
          val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal((status: CStatus).in[Cbor]))
          Cbor.ast(Cbor.Ast.parse(bytes)).as[CStatus]
      . assert(_ == List(CStatus.Active(5), CStatus.Removed(9), CStatus.Pending(1)))

    suite(m"HTTP content-type integration"):
      test(m"serialises with the application/cbor media type"):
        Person(t"Alice", 30).in[Cbor].generic(0)
      . assert(_ == t"application/cbor")

      test(m"request/response body round-trips"):
        val body = Person(t"Alice", 30).in[Cbor]
        body.generic(1).read[Person in Cbor]
      . assert(_ == Person(t"Alice", 30))

    suite(m"Optics"):
      import dynamicCborAccess.enabled, cborConversion.encodable

      val team = Team(Person(t"John", 40), 3).in[Cbor]
      val list = Wrapper(List(1, 2, 3), t"hi").in[Cbor]

      test(m"lens reads a field by name"):
        summon["size" is Lens from Cbor onto Cbor](team).as[Int]
      . assert(_ == 3)

      test(m"lens sets a top-level field"):
        team.lens(_.size = 5.in[Cbor]).as[Team]
      . assert(_ == Team(Person(t"John", 40), 5))

      test(m"lens sets a nested field"):
        team.lens(_.lead.name = t"Bob").as[Team]
      . assert(_ == Team(Person(t"Bob", 40), 3))

      test(m"lens.modify transforms a field through a function"):
        val lens = summon["size" is Lens from Cbor onto Cbor]
        lens.modify(team)(cbor => (cbor.as[Int] + 1).in[Cbor]).as[Team]
      . assert(_ == Team(Person(t"John", 40), 4))

      test(m"ordinal optic updates an array element"):
        list.lens(_.values(Sec) = 9).as[Wrapper]
      . assert(_ == Wrapper(List(1, 9, 3), t"hi"))

      test(m"each optic updates every array element"):
        list.lens(_.values(Each) = 0).as[Wrapper]
      . assert(_ == Wrapper(List(0, 0, 0), t"hi"))

      test(m"filter optic updates only matching elements"):
        list.lens(_.values(Filter[Cbor](_.as[Int] > 1)) = 0).as[Wrapper]
      . assert(_ == Wrapper(List(1, 0, 0), t"hi"))

      test(m"setting an absent field inserts it"):
        list.lens(_.extra = 7).selectDynamic("extra").as[Int]
      . assert(_ == 7)

    suite(m"Error byte-offsets"):
      test(m"truncated input reports the offset of the missing byte"):
        capture[CborError](Cbor.Ast.parse(hex("18"))).reason match
          case CborError.Reason.Truncated(offset) => offset
          case _                                   => -1L
      . assert(_ == 1L)

      test(m"trailing bytes report the offset where they begin"):
        capture[CborError](Cbor.Ast.parse(hex("0000"))).reason match
          case CborError.Reason.Trailing(offset) => offset
          case _                                 => -1L
      . assert(_ == 1L)

      test(m"a reserved head byte reports its offset and byte value"):
        capture[CborError](Cbor.Ast.parse(hex("1c"))).reason match
          case CborError.Reason.Reserved(offset, byte) => (offset, byte)
          case _                                        => (-1L, -1)
      . assert(_ == (0L, 28))

    suite(m"Dynamic access"):
      import dynamicCborAccess.enabled

      test(m"selectDynamic reads a map field by name"):
        Person(t"Ada", 36).in[Cbor].selectDynamic("name").as[Text]
      . assert(_ == t"Ada")

      test(m"applyDynamic indexes into an array-valued field"):
        Wrapper(List(10, 20, 30), t"hi").in[Cbor].applyDynamic("values")(1).as[Int]
      . assert(_ == 20)

      test(m"updateDynamic replaces a field's value"):
        Person(t"Ada", 36).in[Cbor].updateDynamic("age")(40).as[Person]
      . assert(_ == Person(t"Ada", 40))

      test(m"updateDynamic with Unset deletes a field"):
        Person(t"Ada", 36).in[Cbor].updateDynamic("age")(Unset).as[OptPerson]
      . assert(_ == OptPerson(t"Ada", Unset))

    suite(m"Optional fields"):
      test(m"an Optional field round-trips when present"):
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(OptPerson(t"Ada", 36).in[Cbor]))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[OptPerson]
      . assert(_ == OptPerson(t"Ada", 36))

      test(m"an Optional field round-trips when unset"):
        val bytes = Cbor.Ast.encodable.encoded(Cbor.unseal(OptPerson(t"Eve", Unset).in[Cbor]))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[OptPerson]
      . assert(_ == OptPerson(t"Eve", Unset))

    suite(m"Direct parsing (Inlinable)"):
      given (Point is Cbor.Parsable) = Inlinable.parsable[Point]
      given (Person is Cbor.Parsable) = Inlinable.parsable[Person]
      given (Wrapper is Cbor.Parsable) = Inlinable.parsable[Wrapper]
      given (Team is Cbor.Parsable) = Inlinable.parsable[Team]
      given (OptPerson is Cbor.Parsable) = Inlinable.parsable[OptPerson]
      given (Defaulted is Cbor.Parsable) = Inlinable.parsable[Defaulted]
      given (Wide is Cbor.Parsable) = Inlinable.parsable[Wide]
      given (Blob is Cbor.Parsable) = Inlinable.parsable[Blob]
      given (Nums is Cbor.Parsable) = Inlinable.parsable[Nums]
      given (Mixed is Cbor.Parsable) = Inlinable.parsable[Mixed]

      def encoded[value: Encodable in Cbor](value: value): IArray[Byte] =
        Cbor.Ast.encodable.encoded(Cbor.unseal(value.in[Cbor]))

      test(m"a flat product reads directly from bytes"):
        encoded(Point(3, 4)).read[Point in Cbor]
      . assert(_ == Point(3, 4))

      test(m"text and numeric scalars agree with the AST path"):
        encoded(Mixed(2.5, true, t"hello")).read[Mixed in Cbor]
      . assert(_ == Mixed(2.5, true, t"hello"))

      test(m"a nested product inlines through its own generated parser"):
        encoded(Team(Person(t"Ada", 36), 5)).read[Team in Cbor]
      . assert(_ == Team(Person(t"Ada", 36), 5))

      test(m"a collection field loops over a definite-length array"):
        encoded(Wrapper(List(1, 2, 3), t"hi")).read[Wrapper in Cbor]
      . assert(_ == Wrapper(List(1, 2, 3), t"hi"))

      test(m"a byte-string field reads in place"):
        encoded(Blob(hex("01020304"))).read[Blob in Cbor].data.to(List)
      . assert(_ == List[Byte](1, 2, 3, 4))

      test(m"an indefinite-length map parses to the same record"):
        hex("bf 6178 03 6179 04 ff").read[Point in Cbor]
      . assert(_ == Point(3, 4))

      test(m"an indefinite-length array parses to the same collection"):
        hex("a1 6676616c756573 9f 010203 ff").read[Nums in Cbor]
      . assert(_ == Nums(List(1, 2, 3)))

      test(m"an unknown key is skipped whole"):
        hex("a3 617a 05 6178 03 6179 04").read[Point in Cbor]
      . assert(_ == Point(3, 4))

      test(m"the first occurrence of a repeated key wins"):
        hex("a3 6178 03 6178 09 6179 04").read[Point in Cbor]
      . assert(_ == Point(3, 4))

      test(m"a non-text-keyed entry is ignored"):
        hex("a3 05 05 6178 03 6179 04").read[Point in Cbor]
      . assert(_ == Point(3, 4))

      test(m"a float coerces into an integer field as the AST accessor"):
        hex("a2 6178 fb3ff8000000000000 6179 02").read[Point in Cbor]
      . assert(_ == Point(1, 2))

      test(m"a key too long to pack resolves through the general step"):
        encoded(Wide(9, 2)).read[Wide in Cbor]
      . assert(_ == Wide(9, 2))

      test(m"a missing required field aborts with Absent"):
        capture[CborError](hex("a1 6178 03").read[Point in Cbor]).reason
      . assert(_ == CborError.Reason.Absent)

      test(m"a missing field with a declared default takes it"):
        hex("a1 6178 03").read[Defaulted in Cbor]
      . assert(_ == Defaulted(3, 7))

      test(m"an Optional field bridges to Unset when absent"):
        hex("a1 646e616d65 63457665").read[OptPerson in Cbor]
      . assert(_ == OptPerson(t"Eve", Unset))

      test(m"an Optional field bridges to Unset from a wire undefined"):
        hex("a2 646e616d65 63457665 63616765 f7").read[OptPerson in Cbor]
      . assert(_ == OptPerson(t"Eve", Unset))

      test(m"an Optional field reads its value when present"):
        encoded(OptPerson(t"Ada", 36)).read[OptPerson in Cbor]
      . assert(_ == OptPerson(t"Ada", 36))

      test(m"a non-map item reads as an empty record"):
        capture[CborError](hex("07").read[Point in Cbor]).reason
      . assert(_ == CborError.Reason.Absent)

      test(m"trailing bytes are rejected as on the AST path"):
        val bytes = encoded(Point(3, 4))
        val padded = IArray.from(bytes.to(List) :+ 0.toByte)
        capture[CborError](padded.read[Point in Cbor]).reason match
          case CborError.Reason.Trailing(offset) => offset
          case _                                 => -1L
      . assert(_ == 7L)

      test(m"the aggregable trigger routes a stream through the direct parser"):
        val bytes = encoded(Person(t"Ada", 36))
        LazyList(bytes).read[Person in Cbor]
      . assert(_ == Person(t"Ada", 36))

      test(m"a recursive type degrades its recursive field to the seam"):
        given (Tree is Cbor.Parsable) = Inlinable.parsable[Tree]
        val tree = Tree(t"root", List(Tree(t"a", Nil), Tree(t"b", List(Tree(t"c", Nil)))))
        encoded(tree).read[Tree in Cbor]
      . assert(_ == Tree(t"root", List(Tree(t"a", Nil), Tree(t"b", List(Tree(t"c", Nil))))))
