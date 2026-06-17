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
package breviloquence

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

case class Point(x: Int, y: Int) derives CanEqual
case class Person(name: Text, age: Int) derives CanEqual
case class Wrapper(values: List[Int], label: Text) derives CanEqual
case class Team(lead: Person, size: Int) derives CanEqual
case class OptPerson(name: Text, age: Optional[Int]) derives CanEqual
case class Renamed
   (@name[Cbor](t"data_files")  dataFiles:  List[Long],
    @name[Cbor](t"index_files") indexFiles: List[Long])
derives CanEqual

enum Shape derives CanEqual:
  case Circle(radius: Double)
  case Square(side: Double)

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
        hexOf(CborPrinter.encode(ast))
      . assert(_ == "182a")

      test(m"Round-trip [1, 2, 3]"):
        val original = hex("83010203")
        val ast = Cbor.Ast.parse(original)
        hexOf(CborPrinter.encode(ast))
      . assert(_ == "83010203")

      test(m"Round-trip {a: 1, b: 2}"):
        val original = hex("a26161016162 02")
        val ast = Cbor.Ast.parse(original)
        hexOf(CborPrinter.encode(ast))
      . assert(_ == "a2616101616202")

    suite(m"Diagnostic notation"):
      test(m"Render 42 as '42'"):
        CborPrinter.diagnostic(Cbor.Ast.parse(hex("182a")))
      . assert(_ == "42")

      test(m"Render [1, 2, 3]"):
        CborPrinter.diagnostic(Cbor.Ast.parse(hex("83010203")))
      . assert(_ == "[1, 2, 3]")

      test(m"Render byte string as hex"):
        CborPrinter.diagnostic(Cbor.Ast.parse(hex("4401020304")))
      . assert(_ == "h'01020304'")

    suite(m"Generic derivation"):
      test(m"Encode Point(1, 2)"):
        val cbor = Point(1, 2).cbor
        val ast = Cbor.unseal(cbor)
        ast.isMap && ast.entries == 2
      . assert(identity)

      test(m"Round-trip Point(3, 4)"):
        val cbor = Point(3, 4).cbor
        val bytes = CborPrinter.encode(Cbor.unseal(cbor))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[Point]
      . assert(_ == Point(3, 4))

      test(m"Round-trip Person(\"Ada\", 36)"):
        val cbor = Person(t"Ada", 36).cbor
        val bytes = CborPrinter.encode(Cbor.unseal(cbor))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[Person]
      . assert(_ == Person(t"Ada", 36))

      test(m"Round-trip Wrapper with list"):
        val original = Wrapper(List(1, 2, 3), t"hello")
        val cbor = original.cbor
        val bytes = CborPrinter.encode(Cbor.unseal(cbor))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[Wrapper] == original
      . assert(identity)

    suite(m"Aggregable"):
      test(m"Aggregate single-chunk Stream[Data] to Cbor"):
        val original = Point(3, 4)
        val bytes = CborPrinter.encode(Cbor.unseal(original.cbor))
        Stream(bytes).read[Cbor].as[Point]
      . assert(_ == Point(3, 4))

      test(m"Aggregate split-chunk Stream[Data] to Cbor"):
        val original = Person(t"Ada", 36)
        val bytes = CborPrinter.encode(Cbor.unseal(original.cbor))
        val half = bytes.length/2
        Stream(bytes.slice(0, half), bytes.slice(half, bytes.length)).read[Cbor].as[Person]
      . assert(_ == Person(t"Ada", 36))

      test(m"Aggregate single-chunk Stream[Data] to Cbor.Ast"):
        val original = Wrapper(List(1, 2, 3), t"hi")
        val bytes = CborPrinter.encode(Cbor.unseal(original.cbor))
        Cbor.ast(Stream(bytes).read[Cbor.Ast]).as[Wrapper]
      . assert(_ == Wrapper(List(1, 2, 3), t"hi"))

    suite(m"`in Cbor` decoder shorthand"):
      test(m"`read[T in Cbor]` resolves a value directly from bytes"):
        val original = Point(3, 4)
        val bytes = CborPrinter.encode(Cbor.unseal(original.cbor))
        Stream(bytes).read[Point in Cbor]
      . assert(_ == Point(3, 4))

      test(m"`read[T in Cbor]` works for nested case classes"):
        val original = Wrapper(List(1, 2, 3), t"hi")
        val bytes = CborPrinter.encode(Cbor.unseal(original.cbor))
        Stream(bytes).read[Wrapper in Cbor]
      . assert(_ == Wrapper(List(1, 2, 3), t"hi"))

    suite(m"@name field renaming"):
      test(m"Encode renames fields to wire keys"):
        val cbor = Renamed(List(1L, 2L), List(3L)).cbor
        val ast = Cbor.unseal(cbor)
        val keys = (0 until ast.entries).map(ast.key(_).string).toSet
        keys == Set("data_files", "index_files")
      . assert(identity)

      test(m"Decode reads wire keys back into Scala fields"):
        val original = Renamed(List(10L, 20L, 30L), List(99L))
        val bytes = CborPrinter.encode(Cbor.unseal(original.cbor))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[Renamed]
      . assert(_ == Renamed(List(10L, 20L, 30L), List(99L)))

      test(m"No relabelling uses original field names"):
        val original = Wrapper(List(1, 2, 3), t"x")
        val cbor = original.cbor
        val ast = Cbor.unseal(cbor)
        val keys = (0 until ast.entries).map(ast.key(_).string).toSet
        keys == Set("values", "label")
      . assert(identity)

      test(m"@name renames a variant's discriminator"):
        val ast = Cbor.unseal((CStatus.Active(5): CStatus).cbor)
        (0 until ast.entries).collectFirst:
          case i if ast.key(i).string == "kind" => ast.value(i).string
        . getOrElse("none")
      . assert(_ == "ok")

      test(m"@name variants round-trip"):
        List(CStatus.Active(5), CStatus.Removed(9), CStatus.Pending(1)).map: status =>
          val bytes = CborPrinter.encode(Cbor.unseal((status: CStatus).cbor))
          Cbor.ast(Cbor.Ast.parse(bytes)).as[CStatus]
      . assert(_ == List(CStatus.Active(5), CStatus.Removed(9), CStatus.Pending(1)))

    suite(m"HTTP content-type integration"):
      test(m"serialises with the application/cbor media type"):
        Person(t"Alice", 30).cbor.generic(0)
      . assert(_ == t"application/cbor")

      test(m"request/response body round-trips"):
        val body = Person(t"Alice", 30).cbor
        body.generic(1).read[Person in Cbor]
      . assert(_ == Person(t"Alice", 30))

    suite(m"Optics"):
      import dynamicCborAccess.enabled

      val team = Team(Person(t"John", 40), 3).cbor
      val list = Wrapper(List(1, 2, 3), t"hi").cbor

      test(m"lens reads a field by name"):
        summon["size" is Lens from Cbor onto Cbor](team).as[Int]
      . assert(_ == 3)

      test(m"lens sets a top-level field"):
        team.lens(_.size = 5.cbor).as[Team]
      . assert(_ == Team(Person(t"John", 40), 5))

      test(m"lens sets a nested field"):
        team.lens(_.lead.name = t"Bob".cbor).as[Team]
      . assert(_ == Team(Person(t"Bob", 40), 3))

      test(m"lens.modify transforms a field through a function"):
        val lens = summon["size" is Lens from Cbor onto Cbor]
        lens.modify(team)(cbor => (cbor.as[Int] + 1).cbor).as[Team]
      . assert(_ == Team(Person(t"John", 40), 4))

      test(m"ordinal optic updates an array element"):
        list.lens(_.values(Sec) = 9.cbor).as[Wrapper]
      . assert(_ == Wrapper(List(1, 9, 3), t"hi"))

      test(m"each optic updates every array element"):
        list.lens(_.values(Each) = 0.cbor).as[Wrapper]
      . assert(_ == Wrapper(List(0, 0, 0), t"hi"))

      test(m"filter optic updates only matching elements"):
        list.lens(_.values(Filter[Cbor](_.as[Int] > 1)) = 0.cbor).as[Wrapper]
      . assert(_ == Wrapper(List(1, 0, 0), t"hi"))

      test(m"setting an absent field inserts it"):
        list.lens(_.extra = 7.cbor).selectDynamic("extra").as[Int]
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
        Person(t"Ada", 36).cbor.selectDynamic("name").as[Text]
      . assert(_ == t"Ada")

      test(m"applyDynamic indexes into an array-valued field"):
        Wrapper(List(10, 20, 30), t"hi").cbor.applyDynamic("values")(1).as[Int]
      . assert(_ == 20)

      test(m"updateDynamic replaces a field's value"):
        Person(t"Ada", 36).cbor.updateDynamic("age")(40).as[Person]
      . assert(_ == Person(t"Ada", 40))

      test(m"updateDynamic with Unset deletes a field"):
        Person(t"Ada", 36).cbor.updateDynamic("age")(Unset).as[OptPerson]
      . assert(_ == OptPerson(t"Ada", Unset))

    suite(m"Optional fields"):
      test(m"an Optional field round-trips when present"):
        val bytes = CborPrinter.encode(Cbor.unseal(OptPerson(t"Ada", 36).cbor))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[OptPerson]
      . assert(_ == OptPerson(t"Ada", 36))

      test(m"an Optional field round-trips when unset"):
        val bytes = CborPrinter.encode(Cbor.unseal(OptPerson(t"Eve", Unset).cbor))
        Cbor.ast(Cbor.Ast.parse(bytes)).as[OptPerson]
      . assert(_ == OptPerson(t"Eve", Unset))
